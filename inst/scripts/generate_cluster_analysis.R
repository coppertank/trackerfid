#!/usr/bin/env Rscript
# Analisi dei cluster spaziali delle letture RFID, senza avviare l'app.
#
# Da riga di comando:
#   Rscript generate_cluster_analysis.R <letture.csv> [data_inizio] [data_fine] [cartella_output]
#
#   Le date sono nel formato YYYY-MM-DD. Senza date si analizza l'anno solare
#   piu' recente del file. Il risultato viene scritto nella cartella indicata
#   (quella corrente se manca) con il nome cluster_analysis_[dal]_[al].csv.
#
# Da una sessione R:
#   source("generate_cluster_analysis.R")
#   risultato <- genera_analisi_cluster(
#     csv_path = "data/letture_rfid_2025.csv",
#     data_inizio = as.Date("2025-01-01"),
#     data_fine = as.Date("2025-12-31")
#   )
#   write.csv(risultato, "output/cluster_analysis_2025.csv", row.names = FALSE)
#
# Parametri di DBSCAN: genera_analisi_cluster(..., eps_m = 100, min_pts = 1).
#
# Lo script usa le funzioni del pacchetto trackerfid: se si trova o viene
# eseguito dentro la cartella del progetto carica i sorgenti, altrimenti usa
# il pacchetto installato.

# Cartella in cui si trova questo script, sia con Rscript sia con source().
cartella_script <- function() {
  argomento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  # Rscript scrive gli spazi del percorso come "~+~".
  percorsi <- gsub("~+~", " ", sub("^--file=", "", argomento), fixed = TRUE)
  for (ambiente in sys.frames()) {
    if (!is.null(ambiente$ofile)) percorsi <- c(percorsi, ambiente$ofile)
  }
  unique(dirname(normalizePath(percorsi, mustWork = FALSE)))
}

# Risale dalle cartelle di partenza fino alla radice del progetto trackerfid.
radice_progetto <- function(partenze) {
  for (cartella in partenze) {
    repeat {
      descrizione <- file.path(cartella, "DESCRIPTION")
      # Solo i sorgenti hanno i file .R: un pacchetto installato non va ricaricato.
      if (file.exists(descrizione) &&
        any(grepl("^Package:\\s*trackerfid\\s*$", readLines(descrizione, warn = FALSE))) &&
        length(list.files(file.path(cartella, "R"), pattern = "\\.R$")) > 0) {
        return(cartella)
      }
      superiore <- dirname(cartella)
      if (identical(superiore, cartella)) break
      cartella <- superiore
    }
  }
  NULL
}

carica_trackerfid <- function() {
  radice <- radice_progetto(c(normalizePath(getwd(), mustWork = FALSE), cartella_script()))
  if (!is.null(radice) && requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(radice, quiet = TRUE)
    return(invisible("sorgenti"))
  }
  if (!requireNamespace("trackerfid", quietly = TRUE)) {
    stop(
      "Il pacchetto trackerfid non e' disponibile: installalo con devtools::install() ",
      "oppure esegui lo script dalla cartella del progetto.",
      call. = FALSE
    )
  }
  invisible("installato")
}

carica_trackerfid()
genera_analisi_cluster <- trackerfid::genera_analisi_cluster
scrivi_analisi_cluster <- trackerfid::scrivi_analisi_cluster

principale <- function(argomenti = commandArgs(trailingOnly = TRUE)) {
  if (length(argomenti) == 0 || argomenti[1] %in% c("-h", "--help")) {
    cat(
      "Uso: Rscript generate_cluster_analysis.R <letture.csv> [data_inizio] [data_fine] [cartella_output]\n",
      "     Date nel formato YYYY-MM-DD; senza date si analizza l'anno piu' recente del file.\n",
      sep = ""
    )
    return(invisible(NULL))
  }
  data_o_nulla <- function(testo) {
    if (is.na(testo) || !nzchar(testo)) {
      return(NULL)
    }
    data <- as.Date(testo, format = "%Y-%m-%d")
    if (is.na(data)) stop("Data non valida: ", testo, " (formato atteso YYYY-MM-DD).", call. = FALSE)
    data
  }
  risultato <- genera_analisi_cluster(
    csv_path = argomenti[1],
    data_inizio = data_o_nulla(argomenti[2]),
    data_fine = data_o_nulla(argomenti[3])
  )
  cartella <- if (is.na(argomenti[4])) "." else argomenti[4]
  file <- scrivi_analisi_cluster(risultato, cartella)
  cat(sprintf(
    "Scritti %d cluster di %d RFID in %s\n",
    nrow(risultato), length(unique(risultato$RFID)), file
  ))
  print(table(risultato$indicatore_cluster))
  invisible(file)
}

# Esecuzione diretta con Rscript (non quando lo script viene caricato con source).
if (sys.nframe() == 0L) {
  principale()
}
