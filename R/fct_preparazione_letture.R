# Preparazione delle letture reali per l'app.
#
# Queste funzioni trasformano le tabelle estratte dai sistemi aziendali nel
# CSV che l'app carica. Non sono usate dall'app: le richiamano gli script in
# `data-raw/`, nell'ordine in cui compaiono qui.

#' Tiene le letture dei mezzi collaudati, successive al collaudo
#'
#' Un mezzo conta solo se il test delle antenne ha esito "POSITIVO", e solo
#' per le letture fatte nei giorni dopo la data del test.
#'
#' @param df_letture Letture, con `giorno_lettura`, `giorno`,
#'   `targa_veicolo`, `RFID`, `comune_lettura`, `cantiere`, `NumeroLetture`,
#'   `latitudine` e `longitudine`. Il comune di lettura arriva dal sistema di
#'   localizzazione dei mezzi, il cantiere dalla tabella dei comuni: vedi il
#'   passo 2.
#' @param df_test_mezzi Test dei mezzi, con `targa_veicolo`,
#'   `matricola_veicolo`, `esito` e `data_test`.
#' @return Le letture valide, con la matricola del mezzo.
#' @noRd
filtra_letture_post_test <- function(df_letture, df_test_mezzi) {
  test_positivi <- dplyr::filter(df_test_mezzi, .data$esito == "POSITIVO")

  df_letture |>
    # inner_join scarta in automatico i mezzi mai testati positivamente
    dplyr::inner_join(test_positivi, by = "targa_veicolo") |>
    # solo le letture effettuate rigorosamente dopo la data del test
    dplyr::filter(.data$giorno > .data$data_test) |>
    # rimuove le colonne di appoggio aggiunte dal join
    dplyr::select(dplyr::all_of(c(
      "giorno_lettura",
      "giorno",
      "targa_veicolo",
      "matricola_veicolo",
      "RFID",
      "comune_lettura",
      "cantiere",
      "NumeroLetture",
      "latitudine",
      "longitudine"
    )))
}

#' Porta i codici RFID a un formato unico
#'
#' Rimuove gli zeri iniziali, poi riempie con zeri a sinistra: fino a 10
#' cifre i codici che ne hanno al più 10, fino a 24 gli altri. Prima corregge
#' i codici in cui un solo carattere dei primi 14, che dovrebbero essere tutti
#' zeri, è stato letto male.
#'
#' @param df Tabella con la colonna `RFID`.
#' @return La tabella con `RFID` in maiuscolo e a lunghezza 10 o 24.
#' @noRd
formatta_rfid <- function(df) {
  dplyr::mutate(
    df,
    RFID = as.character(toupper(.data$RFID)),

    # Correzione del bit flip sui primi 14 caratteri: 13 zeri su 14
    RFID = dplyr::if_else(
      stringr::str_length(.data$RFID) >= 14 &
        stringr::str_count(stringr::str_sub(.data$RFID, 1, 14), "0") == 13,
      paste0("00000000000000", stringr::str_sub(.data$RFID, 15)),
      .data$RFID
    ),

    # Rimuove gli zeri iniziali
    RFID = stringr::str_remove(.data$RFID, "^0+"),

    # Riempimento in base alla lunghezza rimasta
    RFID = dplyr::if_else(
      stringr::str_length(.data$RFID) <= 10,
      stringr::str_pad(.data$RFID, width = 10, side = "left", pad = "0"),
      stringr::str_pad(.data$RFID, width = 24, side = "left", pad = "0")
    )
  )
}

#' Associa a ogni lettura il contenitore e la fornitura del database
#'
#' Cerca il contenitore con lo stesso codice transponder, attivo nel giorno
#' della lettura, e ne prende i dati e quelli della sua fornitura. Se non lo
#' trova, cerca il codice tra i sacchetti e ne prende la fornitura. La lettura
#' è "Presente" a database se alla fine ha una fornitura. Un'utenza può avere
#' più forniture, ma una lettura ha al più una fornitura. Un sacchetto ha
#' solo la fornitura: l'utenza resta vuota.
#'
#' @param df_letture Letture con `RFID` e `giorno`.
#' @param df_associazione_contenitori Associazioni dei contenitori, con
#'   `codice_transponder`, il periodo di validità
#'   (`data_attivazione_contenitore` e `data_cessazione_contenitore`), i dati
#'   del contenitore (`servizio_transponder`, `volume`, `frequenza`,
#'   `numero_raccolte_annue`, `codice_categoria`) e i dati della fornitura
#'   (`id_fornitura`, `id_utenza` e `comune_fornitura`).
#' @param df_associazione_sacchetti Associazioni dei sacchetti, con
#'   `codice_uhf` e `id_fornitura`.
#' @return Le letture con i dati del contenitore e della fornitura, e
#'   `presente_a_database`. Per un non censito mancano.
#' @noRd
associa_servizio <- function(
  df_letture,
  df_associazione_contenitori,
  df_associazione_sacchetti
) {
  df_letture |>
    # join condizionale: stesso codice e lettura dentro il periodo di validità
    dplyr::left_join(
      df_associazione_contenitori,
      by = dplyr::join_by(
        "RFID" == "codice_transponder",
        "giorno" >= "data_attivazione_contenitore",
        "giorno" <= "data_cessazione_contenitore"
      )
    ) |>
    dplyr::left_join(
      dplyr::select(
        df_associazione_sacchetti,
        "codice_uhf",
        fornitura_sacchetto = "id_fornitura"
      ),
      by = c("RFID" = "codice_uhf")
    ) |>
    dplyr::mutate(
      # Le due tabelle possono scrivere il codice con tipi diversi: numero e
      # testo.
      id_fornitura = dplyr::coalesce(
        as.character(.data$id_fornitura),
        as.character(.data$fornitura_sacchetto)
      ),
      presente_a_database = dplyr::if_else(
        !is.na(.data$id_fornitura),
        "Presente",
        "Non Presente"
      )
    ) |>
    dplyr::select(-dplyr::all_of("fornitura_sacchetto"))
}

#' Associa a ogni lettura il giro del calendario e il servizio atteso
#'
#' Per ogni lettura considera i turni svolti dallo stesso mezzo nello stesso
#' giorno e sceglie quello in cui la lettura cade. Se non cade in nessun
#' turno, sceglie quello più vicino nel tempo; a parità di distanza, il primo
#' del calendario. Un turno che finisce dopo la mezzanotte vale per due
#' giorni: quello in cui inizia e il successivo. Senza turni il giro e il
#' servizio atteso restano vuoti.
#'
#' Dal turno vengono:
#' - `id_giro` ed `eseguito`, come li scrive il calendario;
#' - `servizio_giro`: il giro così come lo scrive il calendario, senza
#'   conversioni. `SECCO PAP` resta `SECCO PAP`, perché il secco porta a
#'   porta va distinto dagli altri giri del secco;
#' - `comune_giro`: il comune in cui il calendario mandava il mezzo.
#'
#' `servizio_atteso` è il servizio che il giro lascia aspettare, con il nome
#' del database, vedi `servizio_atteso_dal_giro()`: di norma il giro stesso,
#' `SECCO PAP` diventa `SECCO`; nei giri in cui non è chiaro quale rifiuto si
#' raccolga, il servizio dei censiti letti nel giro. Vale per ogni lettura, di
#' un censito o no.
#'
#' `coerenza_servizio` dice se il servizio del contenitore a database e il
#' servizio atteso sono dello stesso rifiuto, vedi `coerenza_servizio()`:
#' `SI`, `NO`, oppure mancante per un non censito o una lettura senza giro.
#'
#' Il comune di lettura e il cantiere arrivano già con le letture: vedi
#' `filtra_letture_post_test()`. `coerenza_comune` confronta il comune di
#' lettura con quello dichiarato, vedi `coerenza_comune()`: per un censito il
#' comune della fornitura, e se differisce la lettura è `Fuori comune`; per un
#' non censito il comune del giro, e se differisce la lettura è `Fuori giro`.
#' Se i due comuni coincidono la lettura è `Coerente`; se manca uno dei due,
#' `Coerenza ignota`.
#'
#' @param df_letture Letture prodotte da `associa_servizio()`.
#' @param df_calendario Calendario dei giri, con `giorno`, `ora_inizio`,
#'   `ora_fine`, `matricola_mezzo`, `id_giro`, `eseguito`,
#'   `descrizione_servizio` e `comune_giro`. Gli orari sono testi del tipo
#'   `"08:00:00"` o `"08:00"`.
#' @param servizi_applica_moda Giri del calendario, con il loro nome, per cui
#'   il servizio atteso viene dai censiti letti nel giro: vedi
#'   `giri_servizio_dai_censiti()`.
#' @return Una riga per lettura, con le 28 colonne del CSV dell'app.
#' @noRd
associa_servizio_atteso_da_calendario <- function(
  df_letture,
  df_calendario,
  servizi_applica_moda = giri_servizio_dai_censiti()
) {
  # 1. Turni del calendario, con inizio e fine come data e ora. Un turno a
  # cavallo della mezzanotte (inizio 22:00, fine 04:00) finisce il giorno
  # dopo, e viene ripetuto anche sotto quel giorno: altrimenti le letture
  # dopo mezzanotte non lo troverebbero.
  turni <- df_calendario |>
    dplyr::mutate(
      inizio_turno = as.POSIXct(paste(.data$giorno, .data$ora_inizio)),
      fine_turno = as.POSIXct(paste(.data$giorno, .data$ora_fine)),
      oltre_mezzanotte = .data$fine_turno < .data$inizio_turno,
      fine_turno = dplyr::if_else(
        .data$oltre_mezzanotte,
        .data$fine_turno + 86400,
        .data$fine_turno
      )
    )
  turni <- dplyr::bind_rows(
    turni,
    turni |>
      dplyr::filter(.data$oltre_mezzanotte) |>
      dplyr::mutate(giorno = .data$giorno + 1)
  )

  # 2. Tutti i turni del mezzo in quel giorno, e per ogni lettura quello più
  # vicino. Ordinare e tenere la prima riga di ogni lettura dà lo stesso
  # risultato di una scelta fatta lettura per lettura, che con milioni di
  # letture costa minuti.
  letture <- df_letture |>
    dplyr::mutate(id_lettura = dplyr::row_number()) |>
    dplyr::left_join(
      turni,
      by = c("matricola_veicolo" = "matricola_mezzo", "giorno"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      # Secondi tra la lettura e il turno: 0 dentro il turno o senza turni.
      # Un turno con orari illeggibili viene dopo tutti gli altri.
      distanza = dplyr::case_when(
        is.na(.data$id_giro) ~ 0,
        is.na(.data$inizio_turno) | is.na(.data$fine_turno) ~ NA_real_,
        .data$giorno_lettura < .data$inizio_turno ~ as.numeric(difftime(
          .data$inizio_turno,
          .data$giorno_lettura,
          units = "secs"
        )),
        .data$giorno_lettura > .data$fine_turno ~ as.numeric(difftime(
          .data$giorno_lettura,
          .data$fine_turno,
          units = "secs"
        )),
        TRUE ~ 0
      )
    ) |>
    dplyr::arrange(.data$id_lettura, .data$distanza) |>
    dplyr::filter(!duplicated(.data$id_lettura)) |>
    dplyr::rename(servizio_giro = "descrizione_servizio")

  # 3. Servizio atteso e coerenza con il servizio del contenitore.
  letture$servizio_atteso <- servizio_atteso_dal_giro(
    letture,
    servizi_applica_moda
  )
  letture$coerenza_servizio <- coerenza_servizio(letture)

  # 4. Comune di lettura a confronto con il comune dichiarato.
  letture$coerenza_comune <- coerenza_comune(letture)

  # Solo le colonne del CSV dell'app
  dplyr::select(
    letture,
    dplyr::all_of(c(
      "giorno_lettura",
      "giorno",
      "targa_veicolo",
      "matricola_veicolo",
      "id_giro",
      "eseguito",
      "RFID",
      "presente_a_database",
      "servizio_transponder",
      "servizio_atteso",
      "coerenza_servizio",
      "servizio_giro",
      "volume",
      "frequenza",
      "numero_raccolte_annue",
      "codice_categoria",
      "comune_fornitura",
      "comune_giro",
      "comune_lettura",
      "coerenza_comune",
      "cantiere",
      "data_attivazione_contenitore",
      "data_cessazione_contenitore",
      "id_fornitura",
      "stato_fornitura",
      "id_utenza",
      "latitudine",
      "longitudine"
    ))
  )
}

#' Colonne della fornitura aggiunte da `associa_nucleo_e_manovra()`
#'
#' Seguono, nel CSV dell'app, le 28 colonne di
#' `associa_servizio_atteso_da_calendario()`. L'app non le usa: servono
#' all'analisi dei conferimenti del secco, vedi `analisi_conferimenti()`.
#' @noRd
colonne_nucleo_manovra <- function() {
  c(
    "nucleo_residenti",
    "nucleo_non_residenti",
    "nucleo_totale",
    "tipo_categoria",
    "categoria",
    "sottocategoria",
    "manovra_sociale"
  )
}

#' Associa a ogni lettura il nucleo familiare e la manovra sociale
#'
#' Per una lettura legata a una fornitura cerca, nel giorno e all'ora della
#' lettura:
#' - la composizione del nucleo e la categoria della fornitura in vigore: la
#'   riga di `df_componenti` della stessa fornitura con l'inizio della
#'   decorrenza più recente non successivo alla lettura, purché la decorrenza
#'   non sia già finita. Una fine mancante vuol dire ancora in vigore;
#' - la manovra sociale in vigore nel giorno della lettura. Se ce n'è più di
#'   una, vince una manovra che dà il potenziamento del secco, vedi
#'   `manovre_potenziamento()`, e tra quelle la più recente.
#'
#' Una lettura senza fornitura, o senza righe in vigore, resta con i campi
#' vuoti. Una fornitura senza manovra in vigore ha `manovra_sociale` vuota.
#'
#' @param df_letture Letture con `id_fornitura`, `giorno_lettura` e
#'   `giorno`, come le restituisce `associa_servizio_atteso_da_calendario()`.
#' @param df_componenti Composizione dei nuclei, con `id_fornitura`,
#'   `data_inizio_decorrenza`, `data_fine_decorrenza`, `nucleo_residenti`,
#'   `nucleo_non_residenti`, `nucleo_totale`, `tipo_categoria`, `categoria`
#'   e `sottocategoria`.
#' @param df_manovra Manovre sociali, con `id_fornitura`,
#'   `data_inizio_dichiarazione`, `data_fine_dichiarazione` e una colonna con
#'   la tipologia della manovra.
#' @param colonna_manovra Nome della colonna di `df_manovra` con la tipologia,
#'   ad esempio `"MANOVRA SOCIALE ANZIANI"`. Se manca, è la prima colonna di
#'   testo con un valore che inizia per `MANOVRA SOCIALE`.
#' @return Le letture, con in coda le colonne di `colonne_nucleo_manovra()`.
#' @noRd
associa_nucleo_e_manovra <- function(
  df_letture,
  df_componenti,
  df_manovra,
  colonna_manovra = NULL
) {
  fornitura <- as.character(df_letture$id_fornitura)
  istante <- df_letture$giorno_lettura
  giorno <- as.Date(df_letture$giorno)

  # 1. Nucleo e categoria: la decorrenza iniziata più di recente, se non è
  # già finita. Il join lavora sulle coppie distinte di fornitura e istante.
  componenti <- df_componenti |>
    dplyr::mutate(
      id_fornitura = as.character(.data$id_fornitura),
      dplyr::across(
        dplyr::any_of(c("tipo_categoria", "categoria", "sottocategoria")),
        as.character
      ),
      dplyr::across(
        dplyr::any_of(c("nucleo_residenti", "nucleo_non_residenti", "nucleo_totale")),
        as.integer
      )
    )
  chiavi <- unique(data.frame(
    id_fornitura = fornitura,
    istante = istante,
    stringsAsFactors = FALSE
  )[!is.na(fornitura), , drop = FALSE])
  in_vigore <- dplyr::left_join(
    chiavi,
    componenti,
    by = dplyr::join_by("id_fornitura", "istante" >= "data_inizio_decorrenza"),
    relationship = "many-to-many"
  )
  # Per ogni coppia la decorrenza iniziata per ultima.
  in_vigore <- in_vigore[order(
    in_vigore$id_fornitura,
    as.numeric(in_vigore$istante),
    -as.numeric(in_vigore$data_inizio_decorrenza),
    method = "radix"
  ), , drop = FALSE]
  in_vigore <- in_vigore[!duplicated(in_vigore[c("id_fornitura", "istante")]), , drop = FALSE]
  finita <- !is.na(in_vigore$data_fine_decorrenza) &
    in_vigore$istante > in_vigore$data_fine_decorrenza
  campi_nucleo <- colonne_nucleo_manovra()[1:6]
  for (campo in campi_nucleo) {
    if (!campo %in% names(in_vigore)) {
      in_vigore[[campo]] <- NA
    }
    in_vigore[[campo]][finita] <- NA
  }
  riga <- match(
    paste(fornitura, as.numeric(istante), sep = "\r"),
    paste(in_vigore$id_fornitura, as.numeric(in_vigore$istante), sep = "\r")
  )
  riga[is.na(fornitura)] <- NA_integer_
  for (campo in campi_nucleo) {
    df_letture[[campo]] <- in_vigore[[campo]][riga]
  }

  # 2. Manovra sociale in vigore nel giorno della lettura.
  colonna_manovra <- colonna_manovra %||% trova_colonna_manovra(df_manovra)
  manovre <- data.frame(
    id_fornitura = as.character(df_manovra$id_fornitura),
    inizio = as.Date(df_manovra$data_inizio_dichiarazione),
    fine = as.Date(df_manovra$data_fine_dichiarazione),
    manovra = stringr::str_squish(toupper(as.character(df_manovra[[colonna_manovra]]))),
    stringsAsFactors = FALSE
  )
  manovre$fine[is.na(manovre$fine)] <- as.Date("2099-12-31")
  giorni <- unique(data.frame(
    id_fornitura = fornitura,
    giorno = giorno,
    stringsAsFactors = FALSE
  )[!is.na(fornitura), , drop = FALSE])
  trovate <- dplyr::inner_join(
    giorni,
    manovre,
    by = dplyr::join_by("id_fornitura", "giorno" >= "inizio", "giorno" <= "fine")
  )
  trovate <- trovate[!is.na(trovate$manovra) & nzchar(trovate$manovra), , drop = FALSE]
  # Prima le manovre del potenziamento, poi le più recenti.
  trovate <- trovate[order(
    trovate$id_fornitura,
    trovate$giorno,
    !trovate$manovra %in% manovre_potenziamento(),
    -as.numeric(trovate$inizio),
    method = "radix"
  ), , drop = FALSE]
  trovate <- trovate[!duplicated(trovate[c("id_fornitura", "giorno")]), , drop = FALSE]
  riga <- match(
    paste(fornitura, as.numeric(giorno), sep = "\r"),
    paste(trovate$id_fornitura, as.numeric(trovate$giorno), sep = "\r")
  )
  riga[is.na(fornitura)] <- NA_integer_
  df_letture$manovra_sociale <- trovate$manovra[riga]
  df_letture
}

#' Colonna con la tipologia della manovra sociale
#'
#' La prima colonna di testo, oltre al codice della fornitura, con almeno un
#' valore che inizia per `MANOVRA SOCIALE`.
#' @noRd
trova_colonna_manovra <- function(df_manovra) {
  for (campo in setdiff(names(df_manovra), "id_fornitura")) {
    valori <- df_manovra[[campo]]
    if (is.character(valori) || is.factor(valori)) {
      valori <- toupper(stringr::str_squish(as.character(valori)))
      if (any(startsWith(valori, "MANOVRA SOCIALE"), na.rm = TRUE)) {
        return(campo)
      }
    }
  }
  stop(
    "Nella tabella delle manovre sociali nessuna colonna ha valori che ",
    "iniziano per MANOVRA SOCIALE: indicala con l'argomento colonna_manovra.",
    call. = FALSE
  )
}

#' Giri in cui non è chiaro quale rifiuto si raccolga
#'
#' In questi giri il servizio atteso viene dai censiti letti nel giro, vedi
#' `servizio_atteso_dal_giro()`. L'elenco è destinato a crescere: un nome nuovo
#' va aggiunto qui e, per l'icona, alla tipologia «Servizi» di
#' `tipologie_servizio()`.
#'
#' @return I nomi dei giri, come li scrive il calendario.
#' @noRd
giri_servizio_dai_censiti <- function() {
  c("SERVIZI MERCATI", "SERV.MANIFESTAZIONE")
}

#' Servizio atteso di ogni lettura, dal suo giro
#'
#' Di norma è il giro, con il nome del database: `SECCO PAP` diventa `SECCO`,
#' vedi `servizio_da_giro()`. Nei giri di `servizi_applica_moda` non è chiaro
#' quale rifiuto si raccolga; un mezzo che non è uno scarrabile ne raccoglie
#' però uno solo alla volta. Il servizio atteso è allora il servizio a
#' database più frequente tra le letture di quel giro in quel giorno, cioè tra
#' quelle dei censiti. Se nessun censito ha un servizio, o due servizi sono
#' ugualmente frequenti, resta il giro.
#'
#' La usano `associa_servizio_atteso_da_calendario()` e lo script del dataset
#' di esempio.
#'
#' @param letture Letture con `giorno`, `id_giro`, `servizio_giro` e
#'   `servizio_transponder`.
#' @param servizi_applica_moda Giri, con il nome del calendario, in cui il
#'   servizio atteso viene dai censiti.
#' @return Il servizio atteso, uno per lettura, con il nome del database.
#'   Manca se manca il giro.
#' @noRd
servizio_atteso_dal_giro <- function(
  letture,
  servizi_applica_moda = giri_servizio_dai_censiti()
) {
  atteso <- as.character(letture$servizio_giro)
  speciali <- which(
    !is.na(letture$id_giro) & atteso %in% servizi_applica_moda
  )
  if (length(speciali) > 0) {
    moda <- moda_unica_per_gruppo(
      as.character(letture$servizio_transponder[speciali]),
      paste(letture$giorno[speciali], letture$id_giro[speciali], sep = "\r")
    )
    atteso[speciali] <- dplyr::coalesce(moda, atteso[speciali])
  }
  servizio_da_giro(atteso)
}

#' Valore più frequente di ogni gruppo, se è uno solo
#'
#' I mancanti e i testi vuoti non contano. Se due valori sono ugualmente
#' frequenti, o il gruppo non ha valori, il risultato manca.
#'
#' @param valori Vettore di testo.
#' @param gruppo Gruppo di ogni valore.
#' @return Un valore per elemento di `valori`: quello del suo gruppo.
#' @noRd
moda_unica_per_gruppo <- function(valori, gruppo) {
  validi <- !is.na(valori) & nzchar(valori)
  conteggi <- dplyr::count(
    dplyr::tibble(gruppo = gruppo[validi], valore = valori[validi]),
    .data$gruppo,
    .data$valore,
    name = "n"
  )
  conteggi <- conteggi[order(conteggi$gruppo, -conteggi$n), ]
  primo <- !duplicated(conteggi$gruppo)
  # Il primo valore di un gruppo vale se il secondo è meno frequente.
  secondo <- c(primo[-1], TRUE) |
    c(
      conteggi$n[-1] < conteggi$n[-nrow(conteggi)],
      TRUE
    )
  unici <- conteggi[primo & secondo, ]
  unici$valore[match(gruppo, unici$gruppo)]
}

#' Servizio atteso di un giro del calendario
#'
#' Il calendario chiama i giri con nomi suoi: `CARTA/CARTONE PAP` e
#' `CARTA CONT.STRADALI` diventano `CARTA`, come nel database, secondo la
#' tabella di `conversione_servizi()`. Un nome che la tabella non contiene
#' resta com'è e viene segnalato con un avviso: va aggiunto alla tabella.
#'
#' Il giro non viene toccato: la conversione serve solo al servizio atteso.
#'
#' @param servizio_giro Nomi dei giri, come li scrive il calendario.
#' @return I nomi dei servizi del database, uno per elemento.
#' @noRd
servizio_da_giro <- function(servizio_giro) {
  nomi <- unique(stats::na.omit(as.character(servizio_giro)))
  ignoti <- nomi[
    !stringr::str_to_upper(stringr::str_trim(nomi)) %in%
      conversione_servizi()$servizio_calendario
  ]
  if (length(ignoti) > 0) {
    warning(
      "Servizi del calendario assenti dalla tabella di conversione, ",
      "lasciati come sono: ",
      paste(sort(ignoti), collapse = ", "),
      call. = FALSE
    )
  }
  uniforma_servizio(servizio_giro)
}

# --- Lettura dei CSV degli script -------------------------------------------

#' Rileva il separatore del CSV (virgola o punto e virgola)
#'
#' @param path Percorso del file.
#' @return `","` oppure `";"`.
#' @noRd
rileva_separatore <- function(path) {
  intestazione <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(intestazione) == 0 || !nzchar(stringr::str_trim(intestazione))) {
    errore_validazione("Il file \u00e8 vuoto.")
  }
  n_virgole <- stringr::str_count(intestazione, stringr::fixed(","))
  n_punti_virgola <- stringr::str_count(intestazione, stringr::fixed(";"))
  if (n_punti_virgola > n_virgole) ";" else ","
}

#' Legge un CSV rilevando automaticamente il separatore
#'
#' Tutte le colonne vengono lette come testo, così codici come RFID e utenze
#' non perdono eventuali zeri iniziali. L'app legge le letture con DuckDB,
#' vedi `carica_archivio()`: questa funzione serve agli script della
#' preparazione dei dati, come il passo 4 per le letture storiche.
#'
#' @param path Percorso del file CSV.
#' @return Un data frame di sole colonne `character`.
#' @noRd
read_csv_auto <- function(path) {
  separatore <- rileva_separatore(path)
  tryCatch(
    data.table::fread(
      path,
      sep = separatore,
      header = TRUE,
      colClasses = "character",
      na.strings = c("NA", ""),
      encoding = "UTF-8",
      strip.white = TRUE,
      showProgress = FALSE,
      data.table = FALSE
    ),
    error = function(e) {
      errore_validazione(paste0(
        "Impossibile leggere il file come CSV: ",
        conditionMessage(e)
      ))
    }
  )
}
