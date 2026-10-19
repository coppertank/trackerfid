# Dataset di esempio validato, letto una sola volta per tutti i test.
dati_esempio <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- carica_dataset(percorso_dataset_esempio())$dati
    }
    cache
  }
})

# Archivio DuckDB del dataset di esempio, aperto una sola volta.
archivio_esempio <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- carica_archivio(percorso_dataset_esempio())$archivio
    }
    cache
  }
})

# Selezione delle letture di un data frame, come la usa l'app: le letture
# vanno in un archivio DuckDB, vedi `archivio_da_letture()`.
selezione_di <- function(letture, periodo = NULL) {
  seleziona_periodo(archivio_da_letture(letture), periodo)
}

# Ultima lettura di ogni RFID tra quelle che superano i filtri, come la
# calcola l'app, vedi `ultime_letture()`. Senza filtri: l'ultima di ogni RFID.
ultime_di <- function(letture, ..., periodo = NULL) {
  ultime_letture(selezione_di(letture, periodo), ...)
}

# Letture del dataset di esempio che superano i filtri, una per RFID.
ultime_esempio <- function(..., periodo = NULL) {
  ultime_letture(seleziona_periodo(archivio_esempio(), periodo), ...)
}

# Quali letture superano i filtri: un valore per riga di `letture`, come
# la condizione SQL dell'app, vedi `condizione_filtri()`.
maschera_di <- function(letture, ...) {
  selezione <- selezione_di(letture)
  con <- selezione$archivio$con
  esito <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT riga, coalesce(%s, FALSE) AS passa FROM %s",
      condizione_filtri(con, ...),
      sql_con_icona(selezione)
    )
  )
  esito$passa[match(seq_len(nrow(letture)), esito$riga)]
}

# Scrive un CSV temporaneo a partire dalle righe indicate.
scrivi_csv <- function(righe, env = parent.frame()) {
  percorso <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(righe, percorso, useBytes = TRUE)
  percorso
}

# Le nove colonne obbligatorie, nell'ordine usato dalle righe dei test.
intestazione_csv <- paste(
  "giorno_lettura",
  "targa_veicolo",
  "matricola_veicolo",
  "RFID",
  "presente_a_database",
  "servizio_atteso",
  "id_utenza",
  "latitudine",
  "longitudine",
  sep = ","
)

# Le stesse, con il servizio del contenitore a database prima del servizio
# atteso, come nel file della pipeline.
intestazione_transponder <- sub(
  "servizio_atteso",
  "servizio_transponder,servizio_atteso",
  intestazione_csv
)

# Piccolo insieme di letture costruito a mano.
letture_test <- function() {
  dplyr::tibble(
    giorno_lettura = as.POSIXct(
      c(
        "2025-09-01 08:00:00",
        "2025-09-10 08:00:00",
        "2025-09-20 08:00:00",
        "2025-09-05 09:00:00",
        "2025-09-06 09:00:00",
        "2025-09-07 09:00:00"
      ),
      tz = "UTC"
    ),
    targa_veicolo = "AB123CD",
    matricola_veicolo = "VEH001",
    RFID = c("A", "A", "A", "B", "C", "C"),
    presente_a_database = c(rep("Presente", 4), rep("Non Presente", 2)),
    servizio_transponder = c("CARTA", "SECCO", "CARTA", "VETRO", NA, NA),
    servizio_atteso = c(NA, NA, NA, NA, "SECCO", NA),
    id_utenza = c("U1", "U1", "U2", "U1", NA, NA),
    latitudine = c(41.90, 41.91, 41.92, 41.80, 41.70, 41.70),
    longitudine = c(12.40, 12.41, 12.42, 12.30, 12.20, 12.20)
  )
}

# Analisi dei cluster del 2025 sul dataset di esempio, calcolata una sola volta.
analisi_esempio <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      cache <<- calcola_analisi_cluster(
        dati_esempio(),
        "2025-01-01",
        "2025-12-31"
      )
    }
    cache
  }
})

# Gradi di latitudine corrispondenti a una distanza in metri.
gradi_lat <- function(metri) metri / (raggio_terra_m() * pi / 180)

# Letture sintetiche di un RFID: una per data, tutte nello stesso punto. Un
# censito ha il servizio del database, `servizio`; `atteso` è il servizio
# atteso dal giro, per censiti e non censiti.
letture_rfid <- function(
  rfid,
  date,
  lat = 41.9,
  lon = 12.5,
  presente = "Presente",
  servizio = "CARTA",
  atteso = NA_character_,
  utenza = "U1",
  volume = 240,
  raccolte = 26
) {
  n <- length(date)
  censito <- presente == "Presente"
  dplyr::tibble(
    giorno_lettura = as.POSIXct(paste(date, "08:00:00"), tz = "UTC"),
    targa_veicolo = "AB123CD",
    matricola_veicolo = "VEH001",
    RFID = rfid,
    presente_a_database = presente,
    servizio_transponder = if (censito) rep_len(servizio, n) else NA_character_,
    servizio_atteso = rep_len(atteso, n),
    id_utenza = if (censito) utenza else NA_character_,
    latitudine = rep_len(lat, n),
    longitudine = rep_len(lon, n),
    volume_previsto = if (censito) volume else NA_real_,
    numero_raccolte_annue_previste = if (censito) raccolte else NA_real_
  )
}

# Date equidistanti dentro il 2025.
date_2025 <- function(n) {
  format(seq(as.Date("2025-01-10"), as.Date("2025-12-20"), length.out = n))
}

# --- Archivio da un data frame -------------------------------------------------
#
# L'app legge sempre un file; i test costruiscono le letture in un data frame e
# le portano in un archivio DuckDB con queste funzioni.

# Tipo SQL di ogni colonna della tabella delle letture
tipi_archivio <- function() {
  tipi <- stats::setNames(rep("VARCHAR", length(colonne_archivio())), colonne_archivio())
  tipi[["giorno_lettura"]] <- "TIMESTAMP"
  tipi[c("latitudine", "longitudine", colonne_quantita())] <- "DOUBLE"
  tipi[["riga"]] <- "BIGINT"
  tipi
}

# Converte un vettore in data e ora (POSIXct, UTC)
#
# Il formato atteso è `YYYY-MM-DD HH:MM:SS`; sono accettate anche le varianti
# senza secondi, con data italiana (`DD/MM/YYYY`) o con la sola data. Una
# colonna già di tipo data conserva l'ora che mostra, senza conversioni di
# fuso.
converti_data_ora <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(lubridate::force_tz(x, "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(format(x), tz = "UTC"))
  }
  lubridate::parse_date_time(
    x,
    orders = c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "Ymd", "dmY"),
    tz = "UTC",
    quiet = TRUE
  )
}

# Porta in un archivio letture già validate
#
# Le colonne di `colonne_dataset()` che mancano vengono create vuote, con il
# tipo giusto; le altre vengono ignorate. Come nella validazione, senza
# `servizio_giro` il giro è il servizio atteso. Le colonne derivate non
# vengono ricalcolate: le letture vanno in archivio così come sono.
#
# @param letture Data frame di letture validate.
# @param origine Nome da mostrare come origine delle letture.
# @return Archivio, vedi `nuovo_archivio()`.
archivio_da_letture <- function(letture, origine = "letture") {
  if (is.null(letture[[colonne_calendario()]])) {
    letture[[colonne_calendario()]] <- letture[["servizio_atteso"]]
  }
  tipi <- tipi_archivio()
  n <- nrow(letture)
  tabella <- lapply(stats::setNames(names(tipi), names(tipi)), function(campo) {
    valori <- letture[[campo]]
    switch(
      tipi[[campo]],
      TIMESTAMP = if (is.null(valori)) {
        as.POSIXct(rep(NA_real_, n), tz = "UTC")
      } else {
        converti_data_ora(valori)
      },
      DOUBLE = if (is.null(valori)) rep(NA_real_, n) else as.numeric(valori),
      BIGINT = seq_len(n),
      if (is.null(valori)) rep(NA_character_, n) else as.character(valori)
    )
  })
  con <- connessione_duckdb()
  DBI::dbWriteTable(con, "letture", as.data.frame(tabella, stringsAsFactors = FALSE))
  nuovo_archivio(con, "letture", origine = origine)
}

# Dati e attributi di una tabella reactable, come li riceve il browser.
dati_reactable <- function(widget) {
  as.data.frame(
    jsonlite::fromJSON(as.character(widget$x$tag$attribs$data)),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}
attributi_reactable <- function(widget) {
  widget$x$tag$attribs
}

# File estratti sintetici, come li scrive il passo 1, in una cartella: tre
# letture di un contenitore censito, una di un non censito, una di un
# sacchetto, più righe che la preparazione deve scartare. Restituisce i
# percorsi, con i nomi di `file_origine()`.
scrivi_origine_prova <- function(cartella) {
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  file <- file_origine(cartella)
  scrivi <- function(nome, righe) writeLines(righe, file[[nome]], useBytes = TRUE)
  scrivi("test_mezzi", c(
    "TARGA_VEICOLO;MATRICOLA_VEICOLO;ESITO TEST SEZIONE B;ULTIMA DATA VERBALE SEZIONE B",
    "AB123CD;VEH001;POSITIVO;2026-07-31 10.30",
    "ZZ999ZZ;VEH009;NEGATIVO;2026-07-31 10.30",
    "XY000XY;VEH002;-;-"
  ))
  scrivi("letture", c(
    "IndirizzoLocalizzazione;DataeOraPrimaLettura;RFID(EPC);Targa;Latitudine;Longitudine;NumeroLetture",
    "VIA ROMA 1, LIMENA;03/08/2026 08.15.00;123;AB123CD;45,4700;11,8400;2",
    "VIA ROMA 1, LIMENA;17/08/2026 08.20.00;123;AB123CD;45,4701;11,8401;1",
    "VIA ROMA 1, LIMENA;31/08/2026 08.10.00;123;AB123CD;45,4700;11,8400;1",
    "VIA PO 3, Limena, PD;03/08/2026 09.00.00;999;AB123CD;45,4710;11,8410;1",
    "VIA PO 5, LIMENA;03/08/2026 09.05.00;00BD00000000000000000001;AB123CD;45,4711;11,8411;1",
    # scartate: posizione zero, senza comune, mezzo con test negativo, lettura
    # prima del test
    "VIA PO 7, LIMENA;03/08/2026 09.10.00;124;AB123CD;0;0;1",
    "SENZA COMUNE;03/08/2026 09.15.00;125;AB123CD;45,4712;11,8412;1",
    "VIA PO 9, LIMENA;03/08/2026 09.20.00;126;ZZ999ZZ;45,4713;11,8413;1",
    "VIA PO 9, LIMENA;30/07/2026 09.20.00;127;AB123CD;45,4713;11,8413;1"
  ))
  scrivi("contenitori", c(
    "codice_transponder,descrizione_rifiuto,volume,frequenza,numero_raccolte_annue,data_attivazione_contenitore,data_cessazione_contenitore,id_servizio,stato_servizio,id_utenza,comune_servizio,codice_categoria",
    "0000000123,SECCO,120,QUINDICINALE,26,2020-01-01,,F1,APERTO,U1,LIMENA,UD",
    ",SECCO,120,QUINDICINALE,26,2020-01-01,,F9,APERTO,U9,VALSTAGNA,UD"
  ))
  scrivi("gruppi_sacchetti", c(
    "codice_uhf,codice_gruppo_sacchetti",
    "00BD00000000000000000001,G1"
  ))
  scrivi("servizi_sacchetti", c(
    "codice_gruppo_sacchetti,id_servizio,ultima_variazione",
    "G1,S_VECCHIA,2024-01-01 10:00:00",
    "G1,S1,2025-01-01 10:00:00"
  ))
  scrivi("calendario", c(
    "giorno,ora_inizio,ora_fine,matricola_mezzo,id_giro,eseguito,descrizione_servizio,comune",
    "2026-08-03,06:00:00,12:00:00,XVEH001,10,SI,SECCO PAP,LIMENA",
    "2026-08-17,06:00:00,12:00:00,XVEH001,10,SI,SECCO PAP,LIMENA",
    "2026-08-31,06:00:00,12:00:00,XVEH001,10,SI,SECCO PAP,ROSA'",
    "2026-08-31,06:00:00,12:00:00,,11,SI,SECCO PAP,LIMENA"
  ))
  scrivi("componenti", c(
    "id_servizio,data_inizio_decorrenza,data_fine_decorrenza,nucleo_residenti,nucleo_non_residenti,nucleo_totale,tipo_categoria,categoria,sottocategoria",
    "F1,2020/01/01 00.00.00,2026/08/15 00.00.00,2,0,2,DOMESTICA,ABITAZIONE,RESIDENTE",
    "F1,2026/08/15 00.00.00,,3,1,4,DOMESTICA,ABITAZIONE,RESIDENTE"
  ))
  scrivi("manovre", c(
    "id_servizio,data_inizio_dichiarazione,data_fine_dichiarazione",
    "F1,2026-08-20,"
  ))
  scrivi("storiche", c(
    "codice_transponder,data_lettura",
    "123,2025-03-01 07:00:00",
    "123,2025-03-01 07:00:00",
    "00BD00000000000000000001,2025-03-02 07:00:00",
    ",2025-03-03 07:00:00"
  ))
  file
}
