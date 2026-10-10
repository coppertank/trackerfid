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
#'   `codice_transponder`, il periodo di validità e i dati del servizio.
#' @param df_tag Anagrafica dei tag, con `codice_transponder`.
#' @param df_associazione_sacchetti Associazioni dei sacchetti, con
#'   `codice_uhf` e `id_servizio`.
#' @return Le letture con i dati del contenitore, `presente_a_database` e
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
    dplyr::select(-dplyr::all_of("id_servizio"))
}

#' Associa a ogni lettura il servizio atteso dal calendario dei mezzi
#'
#' Per ogni lettura considera i turni svolti dallo stesso mezzo nello stesso
#' giorno e sceglie quello in cui la lettura cade. Se non cade in nessun
#' turno, sceglie quello più vicino nel tempo. Senza turni il servizio atteso
#' resta vuoto.
#'
#' Un turno che finisce dopo la mezzanotte vale per due giorni: quello in cui
#' inizia e il successivo. Così una lettura fatta dopo mezzanotte ritrova il
#' turno iniziato la sera prima.
#'
#' Restituisce le colonne con i nomi che l'app si aspetta, comprese quelle
#' geografiche quando i dati le permettono:
#' - `comune_da_database` dalla colonna `comune_servizio` dell'anagrafica dei
#'   contenitori;
#' - `comune_lettura` dalla colonna del calendario indicata con
#'   `colonna_comune`, cioè il comune in cui lavora il giro scelto;
#' - `cantiere` dalla tabella dei comuni, per il comune del database se c'è,
#'   altrimenti per quello di lettura.
#'
#' Se un giro compare nel calendario con più comuni, alla lettura va il primo.
#'
#' @param df_letture Letture prodotte da `associa_servizio()`.
#' @param df_calendario Calendario dei giri, con `giorno`, `ora_inizio`,
#'   `ora_fine`, `matricola_mezzo` e `descrizione_servizio`.
#' @param colonna_comune Nome della colonna del calendario con il comune del
#'   giro. Se la colonna non c'è, il risultato non ha `comune_lettura`.
#' @return Una riga per lettura, con le colonne del CSV dell'app.
#' @noRd
associa_servizio_atteso_da_calendario <- function(
  df_letture,
  df_calendario,
  colonna_comune = "comune"
) {
  # 1. Preparazione delle letture
  df_letture_clean <- dplyr::mutate(
    df_letture,
    id_lettura = dplyr::row_number()
  )

  # 2. Preparazione del calendario
  df_calendario_clean <- df_calendario |>
    dplyr::select(
      dplyr::all_of(c(
        "giorno",
        "ora_inizio",
        "ora_fine",
        "matricola_mezzo",
        "descrizione_servizio"
      )),
      dplyr::any_of(c(comune_lettura = colonna_comune))
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
      servizio_atteso = "descrizione_servizio",
      volume_previsto = "volume",
      numero_raccolte_annue_previste = "numero_raccolte_annue"
    ) |>
    dplyr::rename(dplyr::any_of(c(comune_da_database = "comune_servizio"))) |>
    completa_cantiere() |>
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
        "volume_previsto",
        "numero_raccolte_annue_previste",
        "id_utenza",
        "latitudine",
        "longitudine"
      )),
      dplyr::any_of(colonne_geografiche())
    )
}

#' Aggiunge il cantiere alle letture che hanno almeno un comune
#'
#' Senza colonne con il comune le letture restano come sono.
#' @noRd
completa_cantiere <- function(df) {
  if (!any(c("comune_da_database", "comune_lettura") %in% names(df))) {
    return(df)
  }
  aggiungi_geografia(df)
}
