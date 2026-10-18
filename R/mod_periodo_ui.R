#' Modulo periodo di analisi: interfaccia
#'
#' Un intervallo di date, dal / al. Il periodo vale per tutta l'app: mappe,
#' ricerche e analisi dei cluster. Per questo sta nella barra verde in alto,
#' all'estremità destra, e non nella barra laterale: va passato a
#' `shinydashboard::dashboardHeader()`. Compare quando un file è caricato, con
#' l'anno più recente del file.
#'
#' @param id Identificativo del modulo.
#' @return Un elemento `li` per l'intestazione.
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
        # Solo un avviso: intervallo non valido o periodo senza letture. Il
        # numero di letture sta nell'intestazione della mappa principale.
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
