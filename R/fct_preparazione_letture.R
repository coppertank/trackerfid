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
#'   `importa_letture()`.
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

#' Riporta il comune di lettura ai comuni della tabella
#'
#' Il sistema di localizzazione dei mezzi scrive a volte il nome di una
#' frazione, o un nome che la tabella dei comuni non contiene. Per quelle
#' letture il comune viene dalla posizione, con i confini della Regione del
#' Veneto, vedi `comune_del_punto()`; una lettura fuori dai comuni serviti
#' tiene il nome del file, e l'app la tratta come fuori dai comuni serviti. I
#' nomi che la tabella contiene, scritti in un'altra grafia, passano alla
#' grafia della tabella. Il cantiere segue il comune. Un messaggio elenca i
#' nomi corretti.
#'
#' Le letture vicine al confine non si toccano: il comune del file resta, e la
#' tolleranza dal confine la applica `coerenza_comune()`.
#'
#' @param letture Letture con `comune_lettura`, `cantiere`, `latitudine` e
#'   `longitudine`.
#' @return Le letture, con `comune_lettura` e `cantiere` corretti.
#' @noRd
correggi_comune_lettura <- function(letture) {
  tabella <- comuni_cantieri()
  riga <- indice_comune(letture$comune_lettura)
  nota <- !is.na(riga)
  letture$comune_lettura[nota] <- tabella$comune[riga[nota]]
  ignota <- which(!nota & !is.na(letture$latitudine) & !is.na(letture$longitudine))
  if (length(ignota) > 0) {
    trovato <- comune_del_punto(letture$latitudine[ignota], letture$longitudine[ignota])
    corrette <- ignota[!is.na(trovato)]
    if (length(corrette) > 0) {
      prima <- letture$comune_lettura[corrette]
      letture$comune_lettura[corrette] <- trovato[!is.na(trovato)]
      conteggio <- sort(table(paste0(
        dplyr::coalesce(prima, "(vuoto)"),
        " \u2192 ",
        letture$comune_lettura[corrette]
      )), decreasing = TRUE)
      message(sprintf(
        "Comune di lettura ricavato dalla posizione per %d letture con un nome che la tabella dei comuni non contiene: %s%s",
        length(corrette),
        paste(sprintf("%s (%d)", names(conteggio), conteggio)[seq_len(min(10, length(conteggio)))], collapse = ", "),
        if (length(conteggio) > 10) ", \u2026" else ""
      ))
    }
  }
  letture$cantiere <- dplyr::coalesce(
    cantiere_di_comune(letture$comune_lettura),
    letture$cantiere
  )
  letture
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
#'   `numero_raccolte_annue`) e i dati della fornitura
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

  # 4. Comune di lettura a confronto con il comune dichiarato, con la
  # tolleranza dal confine.
  letture$distanza_comune_dichiarato <- distanza_comune_dichiarato(letture)
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
      "comune_fornitura",
      "comune_giro",
      "comune_lettura",
      "coerenza_comune",
      "distanza_comune_dichiarato",
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

#' Colonne del nucleo e della categoria della fornitura
#'
#' Le aggiunge `associa_componenti()`.
#' @noRd
colonne_componenti <- function() {
  c(
    "nucleo_residenti",
    "nucleo_non_residenti",
    "nucleo_totale",
    "tipo_categoria",
    "categoria",
    "sottocategoria"
  )
}

#' Colonne della fornitura in coda al CSV dell'app
#'
#' Seguono le 28 colonne di `associa_servizio_atteso_da_calendario()`: quelle
#' di `colonne_componenti()` e `potenziamento_secco`, di
#' `associa_potenziamento()`. L'app non le usa: servono all'analisi dei
#' conferimenti del secco, vedi `analisi_conferimenti()`.
#' @noRd
colonne_nucleo_manovra <- function() {
  c(colonne_componenti(), "potenziamento_secco")
}

#' Associa a ogni lettura il nucleo familiare e la categoria della fornitura
#'
#' Per una lettura legata a una fornitura prende, all'istante della lettura,
#' la riga di `df_componenti` della stessa fornitura con l'inizio della
#' decorrenza più recente non successivo alla lettura, purché la decorrenza
#' non sia già finita. Una fine mancante vuol dire ancora in vigore. Una
#' lettura senza fornitura, o senza una decorrenza in vigore, resta con i campi
#' vuoti.
#'
#' Il confronto si fa sulle coppie distinte di fornitura e istante, molte meno
#' delle letture.
#'
#' @param df_letture Letture con `id_fornitura` e `giorno_lettura`.
#' @param df_componenti Composizione dei nuclei, con `id_fornitura`,
#'   `data_inizio_decorrenza`, `data_fine_decorrenza` e le colonne di
#'   `colonne_componenti()`, come le restituisce `importa_componenti()`.
#' @return Le letture, con in coda le colonne di `colonne_componenti()`.
#' @noRd
associa_componenti <- function(df_letture, df_componenti) {
  fornitura <- as.character(df_letture$id_fornitura)
  istante <- df_letture$giorno_lettura
  df_componenti$id_fornitura <- as.character(df_componenti$id_fornitura)

  coppie <- unique(data.frame(id_fornitura = fornitura, istante = istante)[!is.na(fornitura), , drop = FALSE])
  in_vigore <- dplyr::inner_join(
    coppie,
    df_componenti,
    by = dplyr::join_by("id_fornitura", "istante" >= "data_inizio_decorrenza"),
    relationship = "many-to-many"
  )
  # Per ogni coppia la decorrenza iniziata per ultima, se non è già finita.
  in_vigore <- in_vigore[order(
    in_vigore$id_fornitura,
    as.numeric(in_vigore$istante),
    -as.numeric(in_vigore$data_inizio_decorrenza),
    method = "radix"
  ), , drop = FALSE]
  in_vigore <- in_vigore[!duplicated(in_vigore[c("id_fornitura", "istante")]), , drop = FALSE]
  finita <- !is.na(in_vigore$data_fine_decorrenza) &
    in_vigore$istante > in_vigore$data_fine_decorrenza
  in_vigore <- in_vigore[!finita, , drop = FALSE]

  riga <- match(
    paste(fornitura, as.numeric(istante), sep = "\r"),
    paste(in_vigore$id_fornitura, as.numeric(in_vigore$istante), sep = "\r")
  )
  for (campo in colonne_componenti()) {
    valori <- in_vigore[[campo]] %||% rep(NA, nrow(in_vigore))
    df_letture[[campo]] <- valori[riga]
  }
  df_letture
}

#' Dice se la fornitura di ogni lettura ha il potenziamento del secco
#'
#' La tabella delle manovre sociali arriva già filtrata: ha solo le
#' dichiarazioni che danno il potenziamento del secco, una riga per periodo,
#' dall'inizio alla fine della dichiarazione. Una lettura ha il potenziamento
#' se nel giorno della lettura la sua fornitura ha almeno una dichiarazione in
#' vigore, estremi compresi; una fine mancante vuol dire ancora in vigore.
#' Una lettura senza fornitura, un non censito, resta senza valore.
#'
#' Una dichiarazione che finisce prima di cominciare è di solito una
#' dichiarazione annullata, scritta così per abitudine: non conta, e viene
#' segnalata con un avviso.
#'
#' @param df_letture Letture con `id_fornitura` e `giorno`.
#' @param df_manovra Dichiarazioni, con `id_fornitura`,
#'   `data_inizio_dichiarazione` e `data_fine_dichiarazione`, come le
#'   restituisce `importa_manovre()`.
#' @return Le letture, con in coda `potenziamento_secco`: `TRUE`, `FALSE`, o
#'   mancante senza fornitura.
#' @noRd
associa_potenziamento <- function(df_letture, df_manovra) {
  fornitura <- as.character(df_letture$id_fornitura)
  giorno <- as.Date(df_letture$giorno)
  dichiarazioni <- data.frame(
    id_fornitura = as.character(df_manovra$id_fornitura),
    inizio = as.Date(df_manovra$data_inizio_dichiarazione),
    fine = as.Date(df_manovra$data_fine_dichiarazione)
  )
  dichiarazioni$fine[is.na(dichiarazioni$fine)] <- as.Date("2099-12-31")

  rovesciate <- dichiarazioni$fine < dichiarazioni$inizio
  if (any(rovesciate, na.rm = TRUE)) {
    warning(
      sum(rovesciate, na.rm = TRUE),
      " dichiarazioni di manovra sociale finiscono prima di cominciare, ",
      "di solito perch\u00e9 annullate: non vengono considerate. Forniture: ",
      paste(utils::head(unique(dichiarazioni$id_fornitura[rovesciate %in% TRUE]), 10), collapse = ", "),
      if (length(unique(dichiarazioni$id_fornitura[rovesciate %in% TRUE])) > 10) ", ...",
      call. = FALSE
    )
    dichiarazioni <- dichiarazioni[!rovesciate %in% TRUE, , drop = FALSE]
  }

  # Coppie distinte di fornitura e giorno con almeno una dichiarazione in
  # vigore.
  coppie <- unique(data.frame(id_fornitura = fornitura, giorno = giorno)[!is.na(fornitura), , drop = FALSE])
  attive <- dplyr::semi_join(
    coppie,
    dichiarazioni,
    by = dplyr::join_by("id_fornitura", "giorno" >= "inizio", "giorno" <= "fine")
  )
  potenziamento <- paste(fornitura, as.numeric(giorno), sep = "\r") %in%
    paste(attive$id_fornitura, as.numeric(attive$giorno), sep = "\r")
  potenziamento[is.na(fornitura)] <- NA
  df_letture$potenziamento_secco <- potenziamento
  df_letture
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

#' Distanza di ogni lettura dal suo comune dichiarato, in metri
#'
#' Solo per le letture fatte in un comune diverso da quello dichiarato: il
#' comune del database per un censito, quello del giro per un non censito,
#' vedi `coerenza_comune()`. Zero se il punto cade dentro il comune
#' dichiarato, secondo i confini della Regione: il sistema di localizzazione
#' dei mezzi ne usa di un po' diversi. Mancante per le altre letture, e se il
#' comune dichiarato non è un comune servito.
#'
#' @param letture Letture con `presente_a_database`, `comune_lettura`,
#'   `comune_fornitura`, `comune_giro`, `latitudine` e `longitudine`.
#' @return Un numero intero di metri per lettura.
#' @noRd
distanza_comune_dichiarato <- function(letture) {
  n <- nrow(letture)
  colonna <- function(nome) as.character(letture[[nome]] %||% rep(NA_character_, n))
  censito <- letture[["presente_a_database"]] %in% "Presente"
  dichiarato <- ifelse(censito, colonna("comune_fornitura"), colonna("comune_giro"))
  chiave <- function(x) {
    per_valori_distinti(as.character(x), function(nomi) {
      chiavi <- chiave_comune(nomi)
      chiavi[!is.na(chiavi) & !nzchar(chiavi)] <- NA_character_
      chiavi
    })
  }
  letto <- chiave(colonna("comune_lettura"))
  atteso <- chiave(dichiarato)
  diverso <- which(!is.na(letto) & !is.na(atteso) & letto != atteso)
  esito <- rep(NA_real_, n)
  esito[diverso] <- round(distanza_dal_comune(
    letture$latitudine[diverso],
    letture$longitudine[diverso],
    dichiarato[diverso]
  ))
  esito
}

#' Giri del calendario da controllare
#'
#' Due controlli sul campo `eseguito` del calendario, nel periodo delle
#' letture:
#'
#' - un giro ancora `NO` più di `giorni_conferma` giorni prima dell'ultima
#'   lettura: probabilmente una conferma dimenticata;
#' - un giro confermato, `SI`, di un mezzo con l'antenna collaudata, dopo il
#'   collaudo, senza nessuna lettura: l'antenna potrebbe essere spenta o
#'   guasta. Un giro che non raccoglie contenitori, come la pulizia del
#'   territorio, può non avere letture.
#'
#' Un avviso dice quanti sono; l'elenco lo scrive `scrivi_letture_app()`.
#'
#' @param letture Letture con `giorno`, `matricola_veicolo` e `id_giro`, come
#'   le restituisce `associa_servizio_atteso_da_calendario()`.
#' @param calendario Calendario dei giri, come lo restituisce
#'   `importa_calendario()`.
#' @param test_mezzi Test dei mezzi, come li restituisce
#'   `importa_test_mezzi()`.
#' @param giorni_conferma Giorni dopo i quali un giro ancora `NO` va
#'   segnalato.
#' @return Data frame con una riga per giro da controllare: `giorno`,
#'   `matricola_veicolo`, `id_giro`, `servizio_giro`, `comune_giro`,
#'   `eseguito` e `problema`.
#' @noRd
controllo_giri <- function(letture, calendario, test_mezzi, giorni_conferma = 7) {
  vuoto <- data.frame(
    giorno = as.Date(character(0)),
    matricola_veicolo = character(0),
    id_giro = character(0),
    servizio_giro = character(0),
    comune_giro = character(0),
    eseguito = character(0),
    problema = character(0),
    stringsAsFactors = FALSE
  )
  if (nrow(letture) == 0 || nrow(calendario) == 0) {
    return(vuoto)
  }
  periodo <- range(letture$giorno, na.rm = TRUE)
  giri <- calendario[
    !is.na(calendario$giorno) &
      calendario$giorno >= periodo[1] & calendario$giorno <= periodo[2],
    c("giorno", "matricola_mezzo", "id_giro", "descrizione_servizio", "comune_giro", "eseguito")
  ]
  giri <- unique(giri)
  names(giri) <- c("giorno", "matricola_veicolo", "id_giro", "servizio_giro", "comune_giro", "eseguito")
  giri$id_giro <- as.character(giri$id_giro)
  esito <- toupper(stringr::str_squish(as.character(giri$eseguito)))

  non_confermato <- esito %in% "NO" & giri$giorno <= periodo[2] - giorni_conferma
  collaudati <- test_mezzi[test_mezzi$esito %in% "POSITIVO", , drop = FALSE]
  i <- match(giri$matricola_veicolo, collaudati$matricola_veicolo)
  con_antenna <- !is.na(i) & giri$giorno > collaudati$data_test[i]
  letti <- unique(paste(letture$matricola_veicolo, letture$giorno, letture$id_giro))
  senza_letture <- esito %in% "SI" & con_antenna &
    !paste(giri$matricola_veicolo, giri$giorno, giri$id_giro) %in% letti

  problemi <- rbind(
    cbind(giri[non_confermato, , drop = FALSE], problema = rep(
      sprintf("non confermato dopo %d giorni", giorni_conferma),
      sum(non_confermato)
    )),
    cbind(giri[senza_letture, , drop = FALSE], problema = rep(
      "confermato, senza letture dell'antenna",
      sum(senza_letture)
    ))
  )
  if (sum(non_confermato) > 0) {
    warning(sprintf(
      "%d giri sono ancora non confermati dopo pi\u00f9 di %d giorni: l'elenco \u00e8 in controllo_giri.csv.",
      sum(non_confermato),
      giorni_conferma
    ), call. = FALSE)
  }
  if (sum(senza_letture) > 0) {
    warning(sprintf(
      "%d giri confermati di mezzi con l'antenna non hanno nessuna lettura: l'antenna potrebbe essere spenta o guasta. L'elenco \u00e8 in controllo_giri.csv.",
      sum(senza_letture)
    ), call. = FALSE)
  }
  if (nrow(problemi) == 0) {
    return(vuoto)
  }
  problemi$problema <- as.character(problemi$problema)
  problemi <- problemi[order(problemi$giorno, problemi$matricola_veicolo, problemi$id_giro, method = "radix"), , drop = FALSE]
  rownames(problemi) <- NULL
  problemi
}

# --- La pipeline di preparazione --------------------------------------------------

#' Toglie le letture che non vanno nel CSV dell'app
#'
#' I sacchetti, codici che iniziano per `00BD`, e i codici non validi, che
#' iniziano con dieci zeri. Togliere i sacchetti è una scelta del proprietario
#' del progetto: finché resta, il CSV dell'app non ne contiene.
#'
#' @param letture Letture con `RFID`.
#' @noRd
filtra_codici_app <- function(letture) {
  scarta <- startsWith(letture$RFID, "00BD") | startsWith(letture$RFID, "0000000000")
  letture[!scarta, , drop = FALSE]
}

#' Letture pronte per l'app, dai file estratti dal passo 1
#'
#' Importa le tabelle con le funzioni di `R/fct_importa_tabelle.R`, poi
#' applica le regole nell'ordine: `filtra_letture_post_test()`,
#' `correggi_comune_lettura()`, `formatta_rfid()`, `associa_servizio()`,
#' `associa_servizio_atteso_da_calendario()`, `associa_componenti()`,
#' `associa_potenziamento()` e `filtra_codici_app()`. Controlla i giri del
#' calendario con `controllo_giri()`. Le tabelle intermedie non vengono
#' salvate: la pipeline di {targets} tiene solo i file finali.
#'
#' @param file Lista dei percorsi, con i nomi di `file_origine()`.
#' @return Le letture, con le 35 colonne del CSV dell'app, e l'attributo
#'   `controllo_giri`, i giri da controllare.
#' @noRd
prepara_letture_app <- function(file = file_origine()) {
  letture <- importa_letture(file$letture)
  test_mezzi <- importa_test_mezzi(file$test_mezzi)
  valide <- formatta_rfid(correggi_comune_lettura(
    filtra_letture_post_test(letture, test_mezzi)
  ))
  rm(letture)
  con_servizio <- associa_servizio(
    valide,
    importa_contenitori(file$contenitori),
    importa_sacchetti(file$gruppi_sacchetti, file$servizi_sacchetti)
  )
  rm(valide)
  calendario <- importa_calendario(file$calendario)
  con_giro <- associa_servizio_atteso_da_calendario(con_servizio, calendario)
  rm(con_servizio)
  da_controllare <- controllo_giri(con_giro, calendario, test_mezzi)
  rm(calendario)
  con_nucleo <- associa_componenti(con_giro, importa_componenti(file$componenti))
  rm(con_giro)
  con_nucleo <- associa_potenziamento(con_nucleo, importa_manovre(file$manovre))
  structure(filtra_codici_app(con_nucleo), controllo_giri = da_controllare)
}

#' Scrive il CSV dell'app e il suo archivio `.duckdb`
#'
#' Il CSV ha tutte le colonne della pipeline. L'archivio è lo stesso CSV
#' letto e validato come lo legge l'app, con le sole colonne dell'app e gli
#' avvisi della validazione: vedi `scrivi_archivio()`. `controllo_giri.csv`
#' elenca i giri da controllare, vedi `controllo_giri()`: c'è sempre, anche
#' senza righe.
#'
#' @param letture Risultato di `prepara_letture_app()`.
#' @param cartella Cartella in cui scrivere.
#' @return I percorsi dei tre file: il CSV, l'archivio e il controllo dei
#'   giri.
#' @noRd
scrivi_letture_app <- function(letture, cartella = "data-raw/output") {
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(cartella, "letture_app.csv")
  archivio <- file.path(cartella, "letture_app.duckdb")
  giri <- file.path(cartella, "controllo_giri.csv")
  da_controllare <- attr(letture, "controllo_giri") %||%
    controllo_giri(letture[0, ], data.frame(), data.frame())
  attr(letture, "controllo_giri") <- NULL
  utils::write.csv(letture, csv, row.names = FALSE)
  scrivi_archivio(csv, archivio)
  utils::write.csv(da_controllare, giri, row.names = FALSE, na = "")
  c(csv, archivio, giri)
}

#' Scrive il CSV delle letture storiche, per i report
#'
#' @param file Lista dei percorsi, con i nomi di `file_origine()`.
#' @param cartella Cartella in cui scrivere.
#' @param ... Nomi delle colonne, vedi `importa_letture_storiche()`.
#' @return Il percorso del CSV.
#' @noRd
scrivi_letture_storiche <- function(file = file_origine(), cartella = "data-raw/output", ...) {
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(cartella, "letture_storiche.csv")
  utils::write.csv(importa_letture_storiche(file$storiche, ...), csv, row.names = FALSE)
  csv
}

# --- Lettura dei CSV estratti ---------------------------------------------------

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
