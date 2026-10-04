#' Modulo caricamento dati: interfaccia
#'
#' Pannello per il caricamento del file CSV con le letture RFID.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_caricamento_ui <- function(id) {
  ns <- NS(id)
  sezione_sidebar(
    "Caricamento Dati", "upload",
    fileInput(
      ns("file_upload"),
      label = "File CSV delle letture RFID",
      accept = c(".csv", "text/csv", "text/plain"),
      buttonLabel = "Sfoglia\u2026",
      placeholder = "Nessun file selezionato",
      width = "100%"
    ),
    actionLink(ns("carica_esempio"), "Usa il dataset di esempio", icon = icon("table")),
    uiOutput(ns("esito"))
  )
}
