#' Modulo mappa WebGL: interfaccia
#'
#' La legenda sta sopra la mappa, in un riquadro che si chiude con un click
#' sul titolo.
#'
#' @param id Identificativo del modulo.
#' @param modalita `"cluster"` (mappa principale), `"rfid"`, `"utenza"` o
#'   `"giro"`.
#' @noRd
mod_mappa_ui <- function(
  id,
  modalita = c("cluster", "rfid", "utenza", "giro")
) {
  modalita <- match.arg(modalita)
  ns <- NS(id)
  etichetta <- switch(
    modalita,
    cluster = "Clustering",
    rfid = "Ricerca RFID",
    utenza = "Ricerca Utenza",
    giro = "Ricerca Giro"
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
      mapgl::maplibreOutput(ns("mappa"), height = "100%"),
      div(class = "legenda-mappa", HTML(html_legenda(modalita)))
    )
  )
}
