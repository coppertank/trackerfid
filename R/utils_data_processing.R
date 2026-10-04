# Funzioni di utilità per lettura, validazione ed elaborazione delle letture RFID.

#' Campi obbligatori del CSV
#' @noRd
colonne_obbligatorie <- function() {
  c(
    "giorno_lettura", "targa_veicolo", "matricola_veicolo", "RFID",
    "presente_a_database", "servizio_transponder", "servizio_atteso",
    "id_utenza", "latitudine", "longitudine"
  )
}

#' Valori ammessi per `presente_a_database`
#' @noRd
stati_database <- function() c("Presente", "Non Presente")

#' Percorso del dataset di esempio incluso nel pacchetto
#' @noRd
percorso_dataset_esempio <- function() {
  app_sys("extdata", "sample_rfid_dataset.csv")
}

#' Segnala un errore di validazione con uno o più messaggi espliciti
#' @noRd
errore_validazione <- function(messaggi) {
  rlang::abort(
    paste(messaggi, collapse = "\n"),
    class = "errore_validazione",
    messaggi = messaggi
  )
}

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
#' Tutte le colonne vengono lette come testo: la conversione dei tipi è
#' demandata a `validate_dataset()`, così codici come RFID e utenze non
#' perdono eventuali zeri iniziali.
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
        "Impossibile leggere il file come CSV: ", conditionMessage(e)
      ))
    }
  )
}

#' Converte un vettore di testo in data e ora (POSIXct, UTC)
#'
#' Il formato atteso è `YYYY-MM-DD HH:MM:SS`; sono accettate anche le varianti
#' senza secondi, con data italiana (`DD/MM/YYYY`) o con la sola data.
#' @noRd
converti_data_ora <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "Ymd", "dmY"),
    tz = "UTC",
    quiet = TRUE
  )
}

#' Converte un vettore di testo in numerico (accetta la virgola decimale)
#' @noRd
converti_numero <- function(x) {
  suppressWarnings(as.numeric(stringr::str_replace(x, stringr::fixed(","), ".")))
}

#' Elenca le prime righe del file che presentano un problema
#' @noRd
elenco_righe <- function(indici, massimo = 5) {
  # +1: la prima riga del file è l'intestazione
  righe <- utils::head(indici, massimo) + 1
  testo <- paste(righe, collapse = ", ")
  if (length(indici) > massimo) paste0(testo, ", \u2026") else testo
}

#' Valida il dataset e converte i campi nei tipi corretti
#'
#' Controlla la presenza dei 10 campi obbligatori, converte `giorno_lettura` in
#' POSIXct e le coordinate in numerico, normalizza i valori testuali.
#' I valori non interpretabili generano un errore esplicito; le righe con campi
#' essenziali vuoti vengono scartate e segnalate tra gli avvisi.
#'
#' @param data Data frame letto da `read_csv_auto()`.
#' @return Lista con `dati` (tibble validato) e `avvisi` (vettore di testo).
#' @noRd
validate_dataset <- function(data) {
  attese <- colonne_obbligatorie()
  nomi <- stringr::str_trim(names(data))
  posizione <- match(tolower(attese), tolower(nomi))
  mancanti <- attese[is.na(posizione)]
  if (length(mancanti) > 0) {
    errore_validazione(sprintf(
      "Colonne mancanti nel file: %s. Il CSV deve contenere tutti i 10 campi obbligatori.",
      paste(mancanti, collapse = ", ")
    ))
  }

  data <- dplyr::as_tibble(data[, posizione, drop = FALSE])
  names(data) <- attese
  if (nrow(data) == 0) {
    errore_validazione("Il file non contiene righe di dati.")
  }

  pulisci <- function(x) dplyr::na_if(stringr::str_squish(as.character(x)), "")
  data <- dplyr::mutate(data, dplyr::across(dplyr::everything(), pulisci))

  errori <- character(0)
  avvisi <- character(0)

  # --- Conversione dei tipi -------------------------------------------------
  giorno <- converti_data_ora(data$giorno_lettura)
  non_validi <- which(!is.na(data$giorno_lettura) & is.na(giorno))
  if (length(non_validi) > 0) {
    errori <- c(errori, sprintf(
      "giorno_lettura: %d valori non riconosciuti come data e ora (formato atteso YYYY-MM-DD HH:MM:SS). Righe del file: %s.",
      length(non_validi), elenco_righe(non_validi)
    ))
  }

  coordinate <- list(latitudine = c(-90, 90), longitudine = c(-180, 180))
  numeri <- purrr::imap(coordinate, function(limiti, campo) {
    valori <- converti_numero(data[[campo]])
    non_numerici <- which(!is.na(data[[campo]]) & is.na(valori))
    fuori_scala <- which(!is.na(valori) & (valori < limiti[1] | valori > limiti[2]))
    list(valori = valori, non_numerici = non_numerici, fuori_scala = fuori_scala)
  })
  for (campo in names(numeri)) {
    esito <- numeri[[campo]]
    if (length(esito$non_numerici) > 0) {
      errori <- c(errori, sprintf(
        "%s: %d valori non numerici. Righe del file: %s.",
        campo, length(esito$non_numerici), elenco_righe(esito$non_numerici)
      ))
    }
    if (length(esito$fuori_scala) > 0) {
      errori <- c(errori, sprintf(
        "%s: %d valori fuori dall'intervallo ammesso (%s .. %s). Righe del file: %s.",
        campo, length(esito$fuori_scala),
        coordinate[[campo]][1], coordinate[[campo]][2], elenco_righe(esito$fuori_scala)
      ))
    }
  }

  stato <- stati_database()[match(tolower(data$presente_a_database), tolower(stati_database()))]
  non_validi <- which(!is.na(data$presente_a_database) & is.na(stato))
  if (length(non_validi) > 0) {
    errori <- c(errori, sprintf(
      "presente_a_database: valori non ammessi (%s). Sono accettati solo \"Presente\" e \"Non Presente\". Righe del file: %s.",
      paste(utils::head(unique(data$presente_a_database[non_validi]), 3), collapse = ", "),
      elenco_righe(non_validi)
    ))
  }

  if (length(errori) > 0) {
    errore_validazione(errori)
  }

  data <- dplyr::mutate(
    data,
    giorno_lettura = giorno,
    presente_a_database = stato,
    servizio_transponder = stringr::str_to_upper(.data$servizio_transponder),
    servizio_atteso = stringr::str_to_upper(.data$servizio_atteso),
    latitudine = numeri$latitudine$valori,
    longitudine = numeri$longitudine$valori
  )

  # --- Righe con campi essenziali vuoti --------------------------------------
  essenziali <- c("giorno_lettura", "RFID", "presente_a_database", "latitudine", "longitudine")
  n_prima <- nrow(data)
  data <- tidyr::drop_na(data, dplyr::all_of(essenziali))
  n_scartate <- n_prima - nrow(data)
  if (nrow(data) == 0) {
    errore_validazione(sprintf(
      "Nessuna riga utilizzabile: tutte le righe hanno almeno un campo essenziale vuoto (%s).",
      paste(essenziali, collapse = ", ")
    ))
  }
  if (n_scartate > 0) {
    avvisi <- c(avvisi, sprintf(
      "%d righe scartate perch\u00e9 prive di un campo essenziale (%s).",
      n_scartate, paste(essenziali, collapse = ", ")
    ))
  }

  # --- Controlli di coerenza (non bloccanti) ----------------------------------
  incoerenti <- sum(
    data$presente_a_database == "Non Presente" &
      (!is.na(data$servizio_transponder) | !is.na(data$id_utenza))
  )
  if (incoerenti > 0) {
    avvisi <- c(avvisi, sprintf(
      "%d letture \"Non Presente\" riportano comunque servizio_transponder o id_utenza.",
      incoerenti
    ))
  }
  coppie <- dplyr::distinct(data, .data$targa_veicolo, .data$matricola_veicolo)
  if (anyDuplicated(coppie$targa_veicolo) > 0 || anyDuplicated(coppie$matricola_veicolo) > 0) {
    avvisi <- c(avvisi, "La relazione tra targa_veicolo e matricola_veicolo non \u00e8 univoca (1:1).")
  }

  list(dati = data, avvisi = avvisi)
}

#' Legge e valida un file CSV di letture RFID
#'
#' @param path Percorso del file CSV.
#' @return Lista con `dati` e `avvisi`, vedi `validate_dataset()`.
#' @noRd
carica_dataset <- function(path) {
  validate_dataset(read_csv_auto(path))
}

#' Deduplica RFID mantenendo solo l'ultima lettura
#'
#' @param data Dataframe con letture
#' @return Dataframe deduplicato (un RFID = una riga)
#' @noRd
deduplica_ultimo_rfid <- function(data) {
  data |>
    dplyr::arrange(dplyr::desc(.data$giorno_lettura)) |>
    dplyr::distinct(.data$RFID, .keep_all = TRUE)
}

#' Calcola percentuale di RFID "Presente"
#'
#' @param data Dataframe
#' @return Numeric tra 0 e 1
#' @noRd
calcola_pct_presente <- function(data) {
  n_presente <- sum(data$presente_a_database == "Presente", na.rm = TRUE)
  n_totale <- nrow(data)
  if (n_totale == 0) 0 else n_presente / n_totale
}

#' Ordina i servizi: prima quelli noti (ordine canonico), poi gli altri
#' @noRd
ordina_servizi <- function(servizi) {
  servizi <- as.character(unique(servizi[!is.na(servizi)]))
  noti <- servizi_config()$servizio
  as.character(c(intersect(noti, servizi), sort(setdiff(servizi, noti))))
}

#' Filtra le letture per periodo (estremi inclusi, a giornata intera)
#'
#' @param data Dataframe con letture.
#' @param periodo Vettore di due date (inizio, fine) oppure `NULL`.
#' @noRd
filtra_periodo <- function(data, periodo = NULL) {
  if (is.null(periodo)) {
    return(data)
  }
  periodo <- lubridate::as_date(periodo)
  giorno <- lubridate::as_date(data$giorno_lettura)
  data[giorno >= periodo[1] & giorno <= periodo[2], , drop = FALSE]
}

#' Applica i filtri laterali alle letture
#'
#' I servizi si filtrano per esclusione: una lettura passa se il suo servizio
#' non è tra quelli deselezionati. Le letture senza `servizio_transponder`
#' (non censite) passano solo con `includi_non_censiti`. Il filtro sul servizio
#' atteso riguarda le sole letture "Non Presente", le uniche per cui il campo è
#' valorizzato.
#'
#' @param data Dataframe con letture.
#' @param periodo Vettore di due date oppure `NULL` (nessun filtro).
#' @param stato_db Valori di `presente_a_database` da mantenere.
#' @param transponder_esclusi Servizi transponder deselezionati.
#' @param includi_non_censiti Mantiene le letture senza servizio transponder.
#' @param atteso_esclusi Servizi attesi deselezionati.
#' @param includi_senza_stima Mantiene le letture non censite senza servizio atteso.
#' @return Dataframe filtrato (tutte le letture che superano i filtri).
#' @noRd
filtra_letture <- function(data,
                           periodo = NULL,
                           stato_db = stati_database(),
                           transponder_esclusi = character(0),
                           includi_non_censiti = TRUE,
                           atteso_esclusi = character(0),
                           includi_senza_stima = TRUE) {
  data <- filtra_periodo(data, periodo)
  non_censita <- data$presente_a_database == "Non Presente"

  transponder_ok <- ifelse(
    is.na(data$servizio_transponder),
    includi_non_censiti,
    !(data$servizio_transponder %in% transponder_esclusi)
  )
  atteso_ok <- ifelse(
    is.na(data$servizio_atteso),
    !non_censita | includi_senza_stima,
    !(data$servizio_atteso %in% atteso_esclusi)
  )

  data[data$presente_a_database %in% stato_db & transponder_ok & atteso_ok, , drop = FALSE]
}

#' Conta le occorrenze di un servizio, in ordine canonico
#' @noRd
conta_servizi <- function(valori) {
  valori <- valori[!is.na(valori)]
  ordine <- ordina_servizi(valori)
  conteggi <- table(factor(valori, levels = ordine))
  stats::setNames(as.integer(conteggi), ordine)
}

#' Statistiche del dataset filtrato (una riga per RFID)
#'
#' @param data Dataframe deduplicato per RFID.
#' @return Lista con totale, conteggi per stato database e per servizio.
#' @noRd
calcola_statistiche <- function(data) {
  non_censiti <- data$presente_a_database == "Non Presente"
  list(
    totale = nrow(data),
    presente = sum(data$presente_a_database == "Presente"),
    non_presente = sum(non_censiti),
    pct_presente = calcola_pct_presente(data),
    transponder = conta_servizi(data$servizio_transponder),
    atteso = conta_servizi(data$servizio_atteso),
    senza_stima = sum(non_censiti & is.na(data$servizio_atteso))
  )
}

#' Estrae i codici da un testo di ricerca
#'
#' @param testo Testo con codici separati da virgola, punto e virgola o a capo.
#' @return Vettore di codici univoci, senza spazi superflui.
#' @noRd
analizza_input_ricerca <- function(testo) {
  if (is.null(testo) || length(testo) == 0 || is.na(testo)) {
    return(character(0))
  }
  codici <- stringr::str_trim(stringr::str_split(testo, "[,;\\n\\r]+")[[1]])
  unique(codici[codici != ""])
}

#' Confronto tra codici che ignora maiuscole e minuscole
#' @noRd
codice_in <- function(valori, codici) {
  !is.na(valori) & stringr::str_to_upper(valori) %in% stringr::str_to_upper(codici)
}

#' Tutte le letture degli RFID cercati
#'
#' @param data Dataframe con letture.
#' @param rfid_list Vettore di RFID cercati.
#' @return Tutte le letture trovate, ordinate per RFID e data decrescente, con
#'   la colonna `is_ultimo` che marca la lettura più recente di ogni RFID.
#' @noRd
cerca_per_rfid <- function(data, rfid_list) {
  data |>
    dplyr::filter(codice_in(.data$RFID, rfid_list)) |>
    dplyr::arrange(.data$RFID, dplyr::desc(.data$giorno_lettura)) |>
    dplyr::group_by(.data$RFID) |>
    dplyr::mutate(is_ultimo = dplyr::row_number() == 1L) |>
    dplyr::ungroup()
}

#' Filtra RFID dove l'ultima lettura ha una delle utenze cercate
#'
#' Un contenitore passato da un'utenza a un'altra compare solo cercando
#' l'utenza attuale.
#'
#' @param data Dataframe
#' @param utenze Vettore di id utenza ricercati
#' @return Dataframe filtrato (ultima lettura di ogni RFID)
#' @noRd
filtra_ultimo_per_utenza <- function(data, utenze) {
  data |>
    deduplica_ultimo_rfid() |>
    dplyr::filter(codice_in(.data$id_utenza, utenze)) |>
    dplyr::arrange(.data$id_utenza, .data$RFID)
}

#' Calcola bounding box di tutte le letture di un RFID
#'
#' @param data Dataframe con letture di un solo RFID
#' @return List con lat_min, lat_max, lng_min, lng_max
#' @noRd
calcola_bbox <- function(data) {
  list(
    lat_min = min(data$latitudine, na.rm = TRUE),
    lat_max = max(data$latitudine, na.rm = TRUE),
    lng_min = min(data$longitudine, na.rm = TRUE),
    lng_max = max(data$longitudine, na.rm = TRUE)
  )
}

#' Cronologia delle transizioni di un campo per un singolo RFID
#'
#' Letture consecutive con lo stesso valore vengono accorpate: resta una riga
#' per ogni cambio, con la data della prima lettura in cui il valore compare.
#'
#' @param letture Letture di un solo RFID.
#' @param campo Nome del campo (`"servizio_transponder"` o `"id_utenza"`).
#' @return Tibble con `giorno_lettura`, `valore`, `attuale`.
#' @noRd
cronologia_transizioni <- function(letture, campo) {
  x <- letture |>
    dplyr::filter(!is.na(.data[[campo]])) |>
    dplyr::arrange(.data$giorno_lettura)
  valori <- x[[campo]]
  cambio <- c(TRUE, valori[-1] != valori[-length(valori)])[seq_along(valori)]
  out <- dplyr::tibble(giorno_lettura = x$giorno_lettura[cambio], valore = valori[cambio])
  dplyr::mutate(out, attuale = dplyr::row_number() == dplyr::n())
}

#' Stima del servizio di un contenitore non censito
#'
#' @param letture Letture di un solo RFID.
#' @return Tibble con `servizio`, `n`, `pct` (0-100), in ordine decrescente.
#' @noRd
stima_servizio_atteso <- function(letture) {
  letture |>
    dplyr::filter(!is.na(.data$servizio_atteso)) |>
    dplyr::count(servizio = .data$servizio_atteso, name = "n") |>
    dplyr::mutate(pct = 100 * .data$n / sum(.data$n)) |>
    dplyr::arrange(dplyr::desc(.data$n), .data$servizio)
}

#' Analizza la storia di un RFID per il pannello di dettaglio
#'
#' Lo stato (censito o no) è quello dell'ultima lettura. I casi possibili sono:
#' `"censito_coerente"`, `"cambio_servizio"`, `"non_censito_con_stima"`,
#' `"non_censito_senza_stima"`.
#'
#' @param letture Tutte le letture di un solo RFID.
#' @return Lista con il caso rilevato e i dati per descriverlo.
#' @noRd
analizza_rfid <- function(letture) {
  letture <- dplyr::arrange(letture, .data$giorno_lettura)
  ultima <- letture[nrow(letture), ]
  censito <- identical(ultima$presente_a_database, "Presente")

  servizi <- cronologia_transizioni(letture, "servizio_transponder")
  utenze <- cronologia_transizioni(letture, "id_utenza")
  stima <- stima_servizio_atteso(letture)

  caso <- if (censito) {
    if (nrow(servizi) > 1) "cambio_servizio" else "censito_coerente"
  } else {
    if (nrow(stima) > 0) "non_censito_con_stima" else "non_censito_senza_stima"
  }

  list(
    rfid = ultima$RFID,
    caso = caso,
    censito = censito,
    n_letture = nrow(letture),
    prima_lettura = min(letture$giorno_lettura),
    ultima_lettura = ultima$giorno_lettura,
    servizio_attuale = if (nrow(servizi) > 0) servizi$valore[nrow(servizi)] else NA_character_,
    cronologia_servizio = servizi,
    stima = stima,
    cambio_utenza = nrow(utenze) > 1,
    cronologia_utenza = utenze
  )
}
