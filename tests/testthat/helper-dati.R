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
