#' Mappa WebGL con legenda collassabile e un riquadro facoltativo sopra
#' @param modalita `"cluster"`, `"rfid"`, `"utenza"` o `"giro"`.
#' @param sovrapposto Contenuto in basso a sinistra, ad esempio una tabella.
#' @noRd
mod_mappa_ui <- function(
  id,
  modalita = c("cluster", "rfid", "utenza", "giro"),
  sovrapposto = NULL
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
      sovrapposto,
      div(class = "legenda-mappa", HTML(html_legenda(modalita)))
    )
  )
}
