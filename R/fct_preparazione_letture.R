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
#' @param df_letture Letture, con `targa_veicolo` e `giorno`.
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
      "latitudine",
      "longitudine",
      "IndirizzoLocalizzazione",
      "NumeroLetture"
    )))
}

#' Estrae i codici RFID dal testo degli eventi
#'
#' Tiene gli eventi "Lettura Tag" e ne ricava i codici. Un evento con più
#' codici separati da punto e virgola diventa una riga per codice.
#'
#' @param df Eventi, con la colonna `Evento`.
#' @return Gli eventi di lettura, con la colonna `RFID`.
#' @noRd
estrai_rfid <- function(df) {
  df |>
    dplyr::filter(stringr::str_detect(.data$Evento, "Lettura Tag")) |>
    # tutto ciò che segue "Lettura Tag " (spazio compreso) fino alla virgola
    dplyr::mutate(
      RFID = stringr::str_extract(.data$Evento, "(?<=Lettura Tag )[^,]+")
    ) |>
    # con un ';' divide i codici e crea una riga per ciascuno
    tidyr::separate_longer_delim(cols = "RFID", delim = ";") |>
    dplyr::mutate(RFID = stringr::str_trim(.data$RFID))
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

#' Associa a ogni lettura il servizio e l'utenza del database
#'
#' Cerca il contenitore con lo stesso codice transponder, attivo nel giorno
#' della lettura. Se non lo trova, cerca il codice tra i sacchetti e ne usa il
#' servizio come utenza. La lettura è "Presente" a database se alla fine ha
#' un'utenza. Converte anche le coordinate in numeri.
#'
#' @param df_letture Letture con `RFID`, `giorno`, `latitudine`, `longitudine`.
#' @param df_associazione_contenitori Associazioni dei contenitori, con
#'   `codice_transponder`, il periodo di validità, i dati del servizio e
#'   `comune_servizio`.
#' @param df_tag Anagrafica dei tag, con `codice_transponder`.
#' @param df_associazione_sacchetti Associazioni dei sacchetti, con
#'   `codice_uhf` e `id_servizio`.
#' @return Le letture con i dati del contenitore, `comune_da_database`, cioè
#'   il comune di servizio dell'anagrafica, `presente_a_database` e
#'   `presente_in_tag_contenitori`.
#' @noRd
associa_servizio <- function(
  df_letture,
  df_associazione_contenitori,
  df_tag,
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
      df_associazione_sacchetti,
      by = c("RFID" = "codice_uhf")
    ) |>
    dplyr::mutate(
      id_utenza = dplyr::coalesce(.data$id_utenza, .data$id_servizio),
      # Sostituisce eventuali virgole con il punto decimale
      latitudine = as.numeric(stringr::str_replace_all(
        .data$latitudine,
        ",",
        "."
      )),
      longitudine = as.numeric(stringr::str_replace_all(
        .data$longitudine,
        ",",
        "."
      )),

      # Presente a database se una delle due ricerche ha trovato un'utenza
      presente_a_database = dplyr::if_else(
        !is.na(.data$id_utenza),
        "Presente",
        "Non Presente"
      ),
      presente_in_tag_contenitori = dplyr::if_else(
        .data$RFID %in% df_tag$codice_transponder,
        "Presente",
        "Non Presente"
      )
    ) |>
    dplyr::select(-dplyr::all_of("id_servizio")) |>
    dplyr::rename(comune_da_database = "comune_servizio")
}

#' Associa a ogni lettura il giro del calendario dei mezzi
#'
#' Per ogni lettura considera i turni svolti dallo stesso mezzo nello stesso
#' giorno e sceglie quello in cui la lettura cade. Se non cade in nessun
#' turno, sceglie quello più vicino nel tempo. Senza turni il giro e il
#' servizio atteso restano vuoti. Vale per ogni lettura, di un censito o no.
#'
#' Un turno che finisce dopo la mezzanotte vale per due giorni: quello in cui
#' inizia e il successivo. Così una lettura fatta dopo mezzanotte ritrova il
#' turno iniziato la sera prima.
#'
#' Dal turno vengono due colonne. `servizio_giro` è il giro così come lo
#' scrive il calendario, senza conversioni: `SECCO PAP` resta `SECCO PAP`,
#' perché il secco porta a porta va distinto dagli altri giri del secco.
#' `servizio_atteso` è lo stesso servizio con il nome del database, vedi
#' `servizio_da_giro()`: ha così gli stessi valori di `servizio_transponder`.
#'
#' Restituisce le colonne con i nomi che l'app si aspetta, comprese quelle
#' geografiche:
#' - `comune_da_database` è quello già scritto da `associa_servizio()`;
#' - `comune_giro` viene dalla colonna `comune` del calendario, cioè il
#'   comune in cui il calendario mandava il mezzo;
#' - `comune_lettura` viene dalla posizione della lettura: è il comune in cui
#'   cade il punto, oppure `fuori_servizio()` fuori dai comuni serviti. Il
#'   calendario non conta: un mezzo può sostituirne un altro;
#' - `cantiere` dalla tabella dei comuni, per il comune in cui cade il punto;
#'   fuori dai comuni serviti, per il comune del database.
#'
#' Se un giro compare nel calendario con più comuni, alla lettura va il primo.
#'
#' @param df_letture Letture prodotte da `associa_servizio()`.
#' @param df_calendario Calendario dei giri, con `giorno`, `ora_inizio`,
#'   `ora_fine`, `matricola_mezzo`, `descrizione_servizio` e `comune`.
#' @return Una riga per lettura, con le colonne del CSV dell'app.
#' @noRd
associa_servizio_atteso_da_calendario <- function(
  df_letture,
  df_calendario
) {
  # 1. Preparazione delle letture
  df_letture_clean <- dplyr::mutate(
    df_letture,
    id_lettura = dplyr::row_number()
  )

  # 2. Preparazione del calendario
  df_calendario_clean <- df_calendario |>
    dplyr::select(
      "giorno",
      "ora_inizio",
      "ora_fine",
      "matricola_mezzo",
      "descrizione_servizio",
      "comune"
    ) |>
    dplyr::distinct() |>
    dplyr::mutate(
      # Data e ora di inizio e fine turno
      # (ora_inizio e ora_fine sono testi del tipo "08:00:00" o "08:00")
      datetime_inizio = as.POSIXct(paste(.data$giorno, .data$ora_inizio)),
      datetime_fine = as.POSIXct(paste(.data$giorno, .data$ora_fine)),

      # Turni a cavallo della mezzanotte (inizio 22:00, fine 04:00): se la
      # fine precede l'inizio si aggiunge un giorno (86400 secondi)
      oltre_mezzanotte = .data$datetime_fine < .data$datetime_inizio,
      datetime_fine = dplyr::if_else(
        .data$oltre_mezzanotte,
        .data$datetime_fine + 86400,
        .data$datetime_fine
      )
    )

  # Letture e turni si abbinano per giorno. Un turno a cavallo della
  # mezzanotte viene quindi ripetuto anche sotto il giorno successivo, con gli
  # stessi orari: altrimenti le letture dopo mezzanotte non lo troverebbero.
  turni_del_giorno_prima <- df_calendario_clean |>
    dplyr::filter(.data$oltre_mezzanotte) |>
    dplyr::mutate(giorno = .data$giorno + 1)
  df_calendario_clean <- dplyr::bind_rows(
    df_calendario_clean,
    turni_del_giorno_prima
  )

  # 3. Join "totale": tutti i turni del mezzo in quel giorno
  df_join <- dplyr::left_join(
    df_letture_clean,
    df_calendario_clean,
    by = c("giorno", "matricola_veicolo" = "matricola_mezzo"),
    relationship = "many-to-many"
  )

  # 4. Calcolo della vicinanza e scelta del turno
  df_join |>
    dplyr::mutate(
      # La lettura è avvenuta dentro il turno?
      dentro_orario = .data$giorno_lettura >= .data$datetime_inizio &
        .data$giorno_lettura <= .data$datetime_fine,

      # Distanza in minuti dall'inizio e dalla fine del turno
      distanza_inizio = abs(as.numeric(
        difftime(.data$giorno_lettura, .data$datetime_inizio, units = "mins")
      )),
      distanza_fine = abs(as.numeric(
        difftime(.data$giorno_lettura, .data$datetime_fine, units = "mins")
      )),

      # 0 se è dentro l'orario o se non c'è nessun turno, altrimenti la
      # distanza più piccola tra inizio e fine
      distanza_minima = dplyr::case_when(
        is.na(.data$datetime_inizio) ~ 0,
        .data$dentro_orario ~ 0,
        TRUE ~ pmin(.data$distanza_inizio, .data$distanza_fine, na.rm = TRUE)
      )
    ) |>
    # Per ogni lettura resta il turno con la distanza più bassa: a parità di
    # distanza, il primo. Ordinare e tenere la prima riga di ogni lettura dà
    # lo stesso risultato di una scelta fatta lettura per lettura, che con
    # milioni di letture costa minuti.
    dplyr::arrange(.data$id_lettura, .data$distanza_minima) |>
    dplyr::filter(!duplicated(.data$id_lettura)) |>
    dplyr::rename(
      servizio_transponder = "descrizione_rifiuto",
      servizio_giro = "descrizione_servizio",
      volume_previsto = "volume",
      numero_raccolte_annue_previste = "numero_raccolte_annue",
      comune_giro = "comune"
    ) |>
    # Il giro resta com'è nel calendario; il servizio atteso ne è la versione
    # con i nomi del database.
    dplyr::mutate(servizio_atteso = servizio_da_giro(.data$servizio_giro)) |>
    completa_geografia() |>
    # Solo le colonne del CSV dell'app
    dplyr::select(
      dplyr::all_of(c(
        "giorno_lettura",
        "targa_veicolo",
        "matricola_veicolo",
        "RFID",
        "presente_a_database",
        "servizio_transponder",
        "servizio_atteso",
        "servizio_giro",
        "volume_previsto",
        "numero_raccolte_annue_previste",
        "id_utenza",
        "latitudine",
        "longitudine",
        "comune_da_database",
        "comune_giro",
        "comune_lettura",
        "cantiere"
      ))
    )
}

#' Stima il servizio atteso dei non censiti dai censiti letti dallo stesso mezzo
#'
#' Il calendario dice che cosa un mezzo doveva fare, non che cosa ha fatto: un
#' mezzo può sostituirne un altro e raccogliere un rifiuto diverso da quello
#' del suo giro. Un giro raccoglie però un rifiuto solo, quindi i contenitori
#' censiti letti dallo stesso mezzo, poco prima e poco dopo, dicono quale.
#'
#' Per ogni lettura di un non censito votano i censiti letti dallo stesso
#' mezzo nei `minuti` precedenti e successivi. Ogni censito vota una volta al
#' giorno, con la sua prima lettura, e solo se a database ha un servizio. Se i
#' voti sono almeno `minimo_censiti` e almeno la quota `soglia` è per lo
#' stesso servizio, quello è il servizio atteso della lettura. Altrimenti
#' resta il servizio del calendario, con il nome del database: i censiti sono
#' pochi, oppure sono misti, come vicino al momento in cui il mezzo passa da un
#' giro all'altro.
#'
#' Gli orari del calendario non servono a delimitare il giro: nei casi in cui
#' il mezzo sostituisce un altro sono proprio quelli a essere sbagliati.
#'
#' Il servizio atteso delle letture dei censiti resta quello del calendario.
#'
#' `servizio_giro` non cambia mai: è il giro del calendario, con il suo nome.
#' La colonna `coerenza` dice se i servizi di ogni lettura concordano, vedi
#' `coerenza_servizio()`: per un censito il servizio del contenitore e il
#' giro, per un non censito il servizio atteso e il giro.
#'
#' @param df_letture Letture prodotte da
#'   `associa_servizio_atteso_da_calendario()`.
#' @param minuti Minuti prima e dopo la lettura entro cui un censito vota.
#' @param minimo_censiti Numero minimo di censiti perché il voto valga.
#' @param soglia Quota minima dei voti per lo stesso servizio.
#' @return Le letture, con `servizio_atteso` aggiornato per i non censiti e
#'   la colonna `coerenza`: `"Coerente"`, `"Non coerente"` o mancante. Se le
#'   letture non hanno `servizio_giro`, la colonna viene creata con il servizio
#'   atteso ricevuto. Applicata due volte dà lo stesso risultato.
#' @noRd
associa_servizio_atteso_dai_censiti <- function(
  df_letture,
  minuti = 30,
  minimo_censiti = 5,
  soglia = 0.8
) {
  n <- nrow(df_letture)
  # Il giro del calendario resta in una colonna sua, con il suo nome. Il
  # ripiego per il servizio atteso è lo stesso giro, con il nome del database.
  if (is.null(df_letture[["servizio_giro"]])) {
    df_letture$servizio_giro <- df_letture$servizio_atteso
  }
  calendario <- uniforma_servizio(df_letture$servizio_giro)
  istante <- as.numeric(df_letture$giorno_lettura)
  con_mezzo <- !is.na(df_letture$matricola_veicolo) & !is.na(istante)
  censita <- df_letture$presente_a_database %in% "Presente"
  mezzo <- indice_gruppi(df_letture$matricola_veicolo)
  stimato <- rep(NA_character_, n)

  votanti <- which(
    con_mezzo & censita & !is.na(df_letture$servizio_transponder)
  )
  # Un voto al giorno per contenitore e per mezzo: la prima lettura.
  votanti <- votanti[order(istante[votanti], method = "radix")]
  votanti <- votanti[
    !duplicated(paste(
      mezzo[votanti],
      istante[votanti] %/% 86400,
      df_letture$RFID[votanti],
      sep = "\r"
    ))
  ]
  da_stimare <- which(con_mezzo & !censita)
  if (length(votanti) > 0 && length(da_stimare) > 0) {
    # Mezzo e istante in un numero solo: i voti di un mezzo stanno in un
    # intervallo che non tocca quelli degli altri.
    posizione <- function(righe) mezzo[righe] * 1e10 + istante[righe]
    finestra <- 60 * minuti
    centro <- posizione(da_stimare)
    servizi <- unique(df_letture$servizio_transponder[votanti])
    # Voti per ogni servizio nella finestra di ogni lettura da stimare.
    voti <- vapply(
      servizi,
      function(servizio) {
        del_servizio <- sort(posizione(
          votanti[df_letture$servizio_transponder[votanti] == servizio]
        ))
        findInterval(centro + finestra, del_servizio) -
          findInterval(centro - finestra, del_servizio, left.open = TRUE)
      },
      numeric(length(da_stimare))
    )
    dim(voti) <- c(length(da_stimare), length(servizi))
    totale <- rowSums(voti)
    primo <- max.col(voti, ties.method = "first")
    del_primo <- voti[cbind(seq_along(primo), primo)]
    valido <- totale >= minimo_censiti & del_primo >= soglia * totale
    stimato[da_stimare[valido]] <- servizi[primo[valido]]
  }

  df_letture$servizio_atteso <- dplyr::coalesce(stimato, calendario)
  df_letture$coerenza <- coerenza_servizio(df_letture)
  dplyr::relocate(
    df_letture,
    dplyr::all_of(c("servizio_giro", "coerenza")),
    .after = dplyr::all_of("servizio_atteso")
  )
}

#' Aggiunge alle letture il comune in cui sono state fatte e il cantiere
#'
#' Applica `aggiungi_geografia()`, la stessa regola dell'app: il comune di
#' lettura viene dalla posizione. Le coordinate possono essere ancora testo,
#' con la virgola decimale: per il conto vengono lette come numeri, nel
#' risultato restano come sono.
#'
#' @param df Letture con `latitudine`, `longitudine`, `comune_da_database` e
#'   `comune_giro`.
#' @return Le letture con `comune_lettura` e `cantiere`.
#' @noRd
completa_geografia <- function(df) {
  posizioni <- df
  posizioni$latitudine <- converti_numero(df$latitudine)
  posizioni$longitudine <- converti_numero(df$longitudine)
  geografia <- aggiungi_geografia(posizioni)
  df$comune_lettura <- geografia$comune_lettura
  df$cantiere <- geografia$cantiere
  df
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
