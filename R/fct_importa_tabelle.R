# Importazione dei file estratti dal database aziendale (passo 1), uno per
# funzione: nomi delle colonne del progetto, tipi convertiti, comuni
# corretti. Le chiama `prepara_letture_app()`; l'app non le usa.

#' Percorsi dei file estratti, con i nomi fissi di ogni estrazione
#'
#' I test dei mezzi, scaricati da AppSheet, vanno salvati come
#' `AppSheet.ViewData.csv`.
#' @noRd
file_origine <- function(cartella = "data-raw/origine") {
  nomi <- c(
    test_mezzi = "AppSheet.ViewData.csv",
    letture = "letture_antenne.csv",
    contenitori = "dv_cnt_associazione_contenitori.csv",
    gruppi_sacchetti = "dv_cnt_gruppi_sacchetti.csv",
    servizi_sacchetti = "dv_cnt_gruppi_sacchetti_servizi.csv",
    calendario = "iv_estrazione_giri_servizi_rifiuti_filtered.csv",
    non_pianificati = "iv_estrazione_giri_non_pianificati_filtered.csv",
    componenti = "dv_associazione_componenti.csv",
    manovre = "dv_potenziamento_secco.csv",
    storiche = "dv_cnt_letture.csv"
  )
  as.list(stats::setNames(file.path(cartella, nomi), names(nomi)))
}

#' Legge un CSV estratto: tutto testo, separatore del file, spazi ai bordi tolti
#' @param na Testi da leggere come mancanti.
#' @noRd
leggi_tabella_origine <- function(
  percorso,
  na = c("", "NA", "NaN", "null", "NULL")
) {
  if (!file.exists(percorso)) {
    stop("File non trovato: ", percorso, call. = FALSE)
  }
  # data.table serve alla sola pipeline: sta tra i Suggests.
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Per leggere i file estratti serve il pacchetto data.table: install.packages(\"data.table\").",
      call. = FALSE
    )
  }
  data.table::fread(
    percorso,
    sep = rileva_separatore(percorso),
    header = TRUE,
    colClasses = "character",
    na.strings = na,
    encoding = "UTF-8",
    strip.white = TRUE,
    check.names = FALSE,
    showProgress = FALSE,
    data.table = FALSE
  )
}

#' Numero da un testo con la virgola o il punto decimale
#' @noRd
numero_testo <- function(x) {
  suppressWarnings(as.numeric(stringr::str_replace(
    x,
    stringr::fixed(","),
    "."
  )))
}

#' Data di fine: una data mancante vuol dire periodo aperto, 2099-12-31
#' @noRd
data_o_aperta <- function(x) {
  dplyr::coalesce(lubridate::as_date(x), as.Date("2099-12-31"))
}

#' Test delle antenne dei mezzi, da AppSheet
#'
#' Il trattino vale come mancante; l'ora del verbale ha i punti al posto dei
#' due punti.
#' @return `targa_veicolo`, `matricola_veicolo`, `esito`, `data_test`.
#' @noRd
importa_test_mezzi <- function(percorso) {
  leggi_tabella_origine(percorso, na = c("", "NA", "-")) |>
    dplyr::transmute(
      targa_veicolo = .data$TARGA_VEICOLO,
      matricola_veicolo = .data$MATRICOLA_VEICOLO,
      esito = .data$`ESITO TEST SEZIONE B`,
      data_test = .data$`ULTIMA DATA VERBALE SEZIONE B` |>
        stringr::str_replace_all(stringr::fixed("."), ":") |>
        lubridate::ymd_hm(quiet = TRUE) |>
        lubridate::as_date()
    )
}

#' Letture delle antenne
#'
#' Il comune di lettura è il pezzo dopo la prima virgola di
#' `IndirizzoLocalizzazione` («indirizzo, comune»), in maiuscolo; il cantiere
#' viene dalla tabella dei comuni, per nome esatto. Fuori le letture con
#' latitudine zero o senza comune. L'istante è nel fuso della sessione, come
#' i turni del calendario.
#' @noRd
importa_letture <- function(percorso, comuni = comuni_cantieri()) {
  leggi_tabella_origine(percorso, na = c("", "NA")) |>
    dplyr::transmute(
      data_ora_lettura = as.POSIXct(
        .data$DataeOraPrimaLettura,
        format = "%d/%m/%Y %H.%M.%S"
      ),
      data_lettura = lubridate::as_date(lubridate::dmy_hms(
        .data$DataeOraPrimaLettura,
        quiet = TRUE
      )),
      targa_veicolo = .data$Targa,
      RFID = .data$`RFID(EPC)`,
      comune_lettura = stringr::str_match(
        .data$IndirizzoLocalizzazione,
        "^[^,]*,\\s*([^,]*)"
      )[, 2] |>
        stringr::str_trim() |>
        toupper(),
      NumeroLetture = .data$NumeroLetture,
      latitudine = numero_testo(.data$Latitudine),
      longitudine = numero_testo(.data$Longitudine)
    ) |>
    # prima via i dati sporchi, poi la prima lettura valida di un transponder
    # nello stesso giorno, anche se letto più volte
    dplyr::filter(.data$latitudine != 0, .data$comune_lettura != "") |>
    dplyr::arrange(.data$data_ora_lettura) |>
    dplyr::distinct(.data$data_lettura, .data$RFID, .keep_all = TRUE) |>
    dplyr::left_join(
      dplyr::select(comuni, "comune", "cantiere"),
      by = c("comune_lettura" = "comune")
    )
}

#' Anagrafica dei contenitori con un codice transponder, e loro forniture
#'
#' Una cessazione mancante vuol dire ancora attivo. Corregge `LUSINA CONCO` e
#' `VALSTAGNA`, comune soppresso nel 2019 e oggi parte di `VALBRENTA`.
#' @noRd
importa_contenitori <- function(percorso) {
  leggi_tabella_origine(percorso) |>
    dplyr::filter(!is.na(.data$codice_transponder)) |>
    dplyr::transmute(
      codice_transponder = .data$codice_transponder,
      servizio_transponder = .data$descrizione_rifiuto,
      volume = numero_testo(.data$volume),
      frequenza = .data$frequenza,
      numero_raccolte_annue = numero_testo(.data$numero_raccolte_annue),
      data_attivazione_contenitore = lubridate::as_date(
        .data$data_attivazione_contenitore
      ),
      data_cessazione_contenitore = data_o_aperta(
        .data$data_cessazione_contenitore
      ),
      id_fornitura = .data$id_servizio,
      stato_fornitura = .data$stato_servizio,
      id_utenza = .data$id_utenza,
      comune_fornitura = dplyr::case_when(
        .data$comune_servizio == "LUSINA CONCO" ~ "LUSIANA CONCO",
        .data$comune_servizio == "VALSTAGNA" ~ "VALBRENTA",
        .default = .data$comune_servizio
      )
    )
}

#' Sacchetti e fornitura del loro gruppo
#'
#' Di un gruppo vale l'associazione con l'ultima variazione più recente; a
#' pari data la prima della tabella.
#' @return `codice_uhf`, `id_fornitura`.
#' @noRd
importa_sacchetti <- function(percorso_gruppi, percorso_servizi) {
  fornitura_del_gruppo <- leggi_tabella_origine(percorso_servizi) |>
    dplyr::mutate(variazione = as.POSIXct(.data$ultima_variazione)) |>
    dplyr::arrange(
      .data$codice_gruppo_sacchetti,
      dplyr::desc(.data$variazione)
    ) |>
    dplyr::distinct(.data$codice_gruppo_sacchetti, .keep_all = TRUE) |>
    dplyr::select("codice_gruppo_sacchetti", id_fornitura = "id_servizio")

  leggi_tabella_origine(percorso_gruppi) |>
    dplyr::left_join(fornitura_del_gruppo, by = "codice_gruppo_sacchetti") |>
    dplyr::select("codice_uhf", "id_fornitura")
}

#' Calendario dei giri pianificati
#'
#' Il comune del giro sta in `comune_giro`.
#' Gli orari restano testo `HH:MM`. Vedi `pulisci_giri()`.
#' @noRd
importa_calendario <- function(percorso) {
  calendario <- leggi_tabella_origine(percorso)
  pulisci_giri(calendario)
}

#' Giri non pianificati: data, mezzo, servizio e comune, senza orari
#' @noRd
importa_giri_non_pianificati <- function(percorso) {
  giri <- leggi_tabella_origine(percorso) |>
    dplyr::rename(dplyr::any_of(c(data_giro = "giorno")))
  mancanti <- setdiff(
    c("data_giro", "matricola_mezzo", "descrizione_servizio", "comune_giro"),
    names(giri)
  )
  if (length(mancanti) > 0) {
    stop(
      "Nel file dei giri non pianificati mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      call. = FALSE
    )
  }
  pulisci_giri(giri)
}

#' Regole comuni ai due calendari
#'
#' Fuori i giri senza mezzo; la matricola perde la lettera iniziale; la data
#' del giro (`data_giro`, in un file vecchio `giorno`) diventa una data;
#' `ROSA'` diventa `ROSÀ`.
#' @noRd
pulisci_giri <- function(giri) {
  giri |>
    dplyr::rename(dplyr::any_of(c(data_giro = "giorno"))) |>
    dplyr::filter(!is.na(.data$matricola_mezzo)) |>
    dplyr::mutate(
      matricola_mezzo = stringr::str_remove(.data$matricola_mezzo, "^[A-Za-z]"),
      data_giro = lubridate::as_date(.data$data_giro),
      comune_giro = dplyr::if_else(
        .data$comune_giro %in% "ROSA'",
        "ROS\u00c0",
        .data$comune_giro
      )
    )
}

#' Nucleo e categoria delle forniture, una riga per decorrenza
#'
#' Colonne in maiuscolo, date `AAAA-MM-GG HH:MM:SS`; fine mancante: ancora in
#' vigore.
#' @noRd
importa_componenti <- function(percorso) {
  data_ora <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")
  intero <- function(x) suppressWarnings(as.integer(numero_testo(x)))

  leggi_tabella_origine(percorso) |>
    dplyr::transmute(
      id_fornitura = .data$ID_SERVIZIO,
      data_inizio_decorrenza = data_ora(.data$DATA_INIZIO_DECORRENZA),
      data_fine_decorrenza = data_ora(.data$DATA_FINE_DECORRENZA),
      nucleo_residenti = intero(.data$NUCLEO_RESIDENTI),
      nucleo_non_residenti = intero(.data$NUCLEO_NON_RESIDENTI),
      nucleo_totale = intero(.data$NUCLEO_TOTALE),
      tipo_categoria = .data$TIPO_CATEGORIA,
      categoria = .data$CATEGORIA,
      sottocategoria = .data$SOTTOCATEGORIA
    )
}

#' Manovre sociali del potenziamento del secco, già filtrate dall'ETL aziendale
#'
#' Una riga per dichiarazione; senza fine vale ancora.
#' @noRd
importa_manovre <- function(percorso) {
  leggi_tabella_origine(percorso) |>
    dplyr::transmute(
      id_fornitura = .data$id_servizio,
      data_inizio_dichiarazione = lubridate::as_date(
        .data$data_inizio_dichiarazione
      ),
      data_fine_dichiarazione = data_o_aperta(.data$data_fine_dichiarazione)
    )
}

#' Data e ora da un testo `AAAA-MM-GG` o `GG/MM/AAAA`, con o senza ora, nel fuso della sessione
#' @noRd
data_ora_da_testo <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY"),
    tz = "",
    quiet = TRUE
  )
}

#' Letture storiche (saponette), prima delle antenne
#'
#' Data e ora, trasmissione e codice, nel formato della pipeline, senza righe
#' ripetute. `id_trasmissione` raggruppa i codici letti nello stesso giro in
#' un giorno, ma non è un `id_giro`. Le righe con data e ora illeggibile si
#' scartano, con un avviso.
#' @param colonna_rfid,colonna_data_ora,colonna_trasmissione Nomi delle colonne
#'   nel file.
#' @return `data_ora_lettura`, `data_lettura`, `id_trasmissione`, `RFID`.
#' @noRd
importa_letture_storiche <- function(
  percorso,
  colonna_rfid = "codice_transponder",
  colonna_data_ora = "data_lettura",
  colonna_trasmissione = "id_trasmissione"
) {
  grezzo <- leggi_tabella_origine(percorso, na = c("", "NA"))
  mancanti <- setdiff(
    c(colonna_rfid, colonna_data_ora, colonna_trasmissione),
    names(grezzo)
  )
  if (length(mancanti) > 0) {
    stop(
      "Nel file delle letture storiche mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      call. = FALSE
    )
  }

  storiche <- grezzo |>
    dplyr::transmute(
      testo_data_ora = .data[[colonna_data_ora]],
      data_ora_lettura = data_ora_da_testo(.data$testo_data_ora),
      data_lettura = lubridate::as_date(.data$data_ora_lettura),
      id_trasmissione = .data[[colonna_trasmissione]],
      RFID = .data[[colonna_rfid]]
    )

  illeggibili <- dplyr::filter(
    storiche,
    !is.na(.data$testo_data_ora),
    is.na(.data$data_ora_lettura)
  )
  if (nrow(illeggibili) > 0) {
    warning(
      nrow(illeggibili),
      " letture storiche hanno un istante illeggibile e vengono scartate, ad esempio: ",
      paste(utils::head(unique(illeggibili$testo_data_ora), 3), collapse = ", "),
      call. = FALSE
    )
  }

  storiche |>
    dplyr::select(-"testo_data_ora") |>
    dplyr::filter(!is.na(.data$RFID), !is.na(.data$data_ora_lettura)) |>
    formatta_rfid() |>
    dplyr::distinct()
}
