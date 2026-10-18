#' Modulo mappa WebGL: interfaccia
#'
#' La legenda sta sopra la mappa, in basso a destra, in un riquadro che si
#' apre e si chiude con un click sul titolo. In basso a sinistra può stare un
#' altro riquadro, ad esempio la tabella di una ricerca: vedi
#' `riquadro_tabella_mappa()`.
#'
#' @param id Identificativo del modulo.
#' @param modalita `"cluster"` (mappa principale), `"rfid"`, `"utenza"` o
#'   `"giro"`.
#' @param sovrapposto Contenuto facoltativo da mettere sopra la mappa.
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
