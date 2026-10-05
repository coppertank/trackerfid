#' Modulo periodo di analisi: interfaccia
#'
#' Selezione dell'anno, con la possibilità di passare a un intervallo di date
#' personalizzato. Il periodo vale per tutta l'app: mappe, ricerche e analisi
#' dei cluster.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_periodo_ui <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    "output.caricato",
    ns = ns,
    sezione_sidebar(
      "Periodo di Analisi", "calendar-days",
      conditionalPanel(
        "!input.personalizzato",
        ns = ns,
        # Gli anni disponibili vengono impostati al caricamento del dataset.
        selectInput(
          ns("anno_filtro"), "Seleziona Anno di Analisi",
          choices = character(0), selectize = FALSE, width = "100%"
        )
      ),
      checkboxInput(ns("personalizzato"), "Periodo personalizzato", value = FALSE),
      conditionalPanel(
        "input.personalizzato",
        ns = ns,
        dateRangeInput(
          ns("intervallo"), "Dal / al",
          format = "dd/mm/yyyy", language = "it", weekstart = 1,
          separator = "al", width = "100%"
        )
      ),
      uiOutput(ns("riepilogo"))
    )
  )
}
