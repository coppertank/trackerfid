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

# Scrive un CSV temporaneo a partire dalle righe indicate.
scrivi_csv <- function(righe, env = parent.frame()) {
  percorso <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(righe, percorso, useBytes = TRUE)
  percorso
}

intestazione_csv <- paste(
  "giorno_lettura", "targa_veicolo", "matricola_veicolo", "RFID", "presente_a_database",
  "servizio_transponder", "servizio_atteso", "id_utenza", "latitudine", "longitudine",
  sep = ","
)

# Piccolo insieme di letture costruito a mano.
letture_test <- function() {
  dplyr::tibble(
    giorno_lettura = as.POSIXct(
      c(
        "2025-09-01 08:00:00", "2025-09-10 08:00:00", "2025-09-20 08:00:00",
        "2025-09-05 09:00:00", "2025-09-06 09:00:00", "2025-09-07 09:00:00"
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
      cache <<- calcola_analisi_cluster(dati_esempio(), "2025-01-01", "2025-12-31")
    }
    cache
  }
})

# Gradi di latitudine corrispondenti a una distanza in metri.
gradi_lat <- function(metri) metri / (raggio_terra_m() * pi / 180)

# Letture sintetiche di un RFID: una per data, tutte nello stesso punto.
letture_rfid <- function(rfid, date, lat = 41.9, lon = 12.5, presente = "Presente",
                         servizio = "CARTA", atteso = NA_character_, utenza = "U1",
                         volume = 240, raccolte = 26) {
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
