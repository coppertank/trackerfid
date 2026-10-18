#' Modulo caricamento dati: interfaccia
#'
#' Pannello per il caricamento del file con le letture RFID: CSV, CSV
#' compresso con gzip, Parquet o archivio `.duckdb`, vedi `scrivi_archivio()`.
#' Si chiude con un click sul titolo, una volta caricati i dati: chiuso, il
#' titolo riporta il nome del file caricato.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_caricamento_ui <- function(id) {
  ns <- NS(id)
  sezione_sidebar(
    "Caricamento Dati",
    "upload",
    collassabile = TRUE,
    riassunto = textOutput(ns("riassunto"), inline = TRUE),
    fileInput(
      ns("file_upload"),
      label = "File delle letture RFID (CSV, CSV.GZ, Parquet o DuckDB)",
      accept = estensioni_accettate(),
      buttonLabel = "Sfoglia\u2026",
      placeholder = "Nessun file selezionato",
      width = "100%"
    ),
    actionLink(
      ns("carica_esempio"),
      "Usa i dati di esempio",
      icon = icon("table")
    ),
    uiOutput(ns("esito"))
  )
}

#' Estensioni e tipi di file accettati dal campo di caricamento
#' @noRd
estensioni_accettate <- function() {
  c(paste0(".", names(formati_file())), "text/csv", "text/plain")
}
