#' Modulo mappa Leaflet: interfaccia
#'
#' @param id Identificativo del modulo.
#' @param modalita `"cluster"` (mappa principale), `"rfid"` o `"utenza"`.
#' @noRd
mod_mappa_ui <- function(id, modalita = c("cluster", "rfid", "utenza")) {
  modalita <- match.arg(modalita)
  ns <- NS(id)
  etichetta <- switch(modalita,
    cluster = "Clustering",
    rfid = "Ricerca RFID",
    utenza = "Ricerca Utenza"
  )
  div(
    class = "pannello-mappa",
    div(
      class = "mappa-intestazione",
      span(
        class = paste0("badge-stato badge-", modalita),
        paste("Visualizzazione:", etichetta)
      ),
      textOutput(ns("riepilogo"), inline = TRUE)
    ),
    div(
      class = "mappa-contenitore",
      uiOutput(ns("messaggio")),
      leaflet::leafletOutput(ns("mappa"), height = "100%")
    )
  )
}
