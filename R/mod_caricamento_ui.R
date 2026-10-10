#' Modulo caricamento dati: interfaccia
#'
#' Pannello per il caricamento del file con le letture RFID e, in aggiunta,
#' di quello con le letture storiche precedenti alle antenne. Tutti e due
#' possono essere CSV, CSV compressi con gzip o Parquet.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_caricamento_ui <- function(id) {
  ns <- NS(id)
  sezione_sidebar(
    "Caricamento Dati",
    "upload",
    fileInput(
      ns("file_upload"),
      label = "File delle letture RFID (CSV, CSV.GZ o Parquet)",
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
    uiOutput(ns("esito")),
    div(
      class = "caricamento-storico",
      fileInput(
        ns("file_storico"),
        label = "Letture storiche, prima delle antenne (facoltativo)",
        accept = estensioni_accettate(),
        buttonLabel = "Sfoglia\u2026",
        placeholder = "Nessun file selezionato",
        width = "100%"
      ),
      uiOutput(ns("esito_storico"))
    )
  )
}

#' Estensioni e tipi di file accettati dai campi di caricamento
#' @noRd
estensioni_accettate <- function() {
  c(paste0(".", names(formati_file())), "text/csv", "text/plain")
}
