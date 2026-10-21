#' Periodo di analisi: due date nella barra in alto, valide per tutta l'app
#'
#' Restituisce un `li` per `shinydashboard::dashboardHeader()`.
#' @noRd
mod_periodo_ui <- function(id) {
  ns <- NS(id)
  tags$li(
    class = "dropdown periodo-intestazione",
    conditionalPanel(
      "output.caricato",
      ns = ns,
      div(
        class = "periodo-controlli",
        # Solo un avviso: intervallo non valido o periodo senza letture.
        uiOutput(ns("riepilogo"), inline = TRUE),
        icon("calendar-days"),
        tags$label(
          `for` = ns("intervallo"),
          class = "periodo-etichetta",
          "Periodo di analisi"
        ),
        dateRangeInput(
          ns("intervallo"),
          label = NULL,
          format = "dd/mm/yyyy",
          language = "it",
          weekstart = 1,
          separator = "al",
          width = "250px"
        )
      )
    )
  )
}
