#' Modulo filtri laterali: interfaccia
#'
#' Filtri per periodo, stato a database e servizio, pulsante di reset e
#' riquadro con le statistiche del dataset filtrato.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_filtri_ui <- function(id) {
  ns <- NS(id)
  oggi <- Sys.Date()
  tagList(
    sezione_sidebar(
      "Filtri", "filter",
      conditionalPanel(
        "!output.caricato",
        ns = ns,
        p(class = "suggerimento", "Carica un file CSV per attivare i filtri.")
      ),
      conditionalPanel(
        "output.caricato",
        ns = ns,
        # L'intervallo reale viene impostato al caricamento del dataset.
        sliderInput(
          ns("date_range"), "Filtro Periodo",
          min = oggi - 30, max = oggi, value = c(oggi - 30, oggi),
          timeFormat = "%d/%m/%Y", ticks = FALSE, width = "100%"
        ),
        checkboxGroupInput(
          ns("presente_filter"), "Filtro Stato Database",
          choices = stati_database(), selected = stati_database(), inline = FALSE
        ),
        checkboxGroupInput(
          ns("servizio_transponder_check"), "Filtro Servizio Transponder",
          choices = character(0), inline = FALSE
        ),
        checkboxInput(ns("includi_non_censiti"), "Includi Non Censiti", value = TRUE),
        checkboxGroupInput(
          ns("servizio_atteso_check"), "Filtro Servizio Atteso",
          choices = character(0), inline = FALSE
        ),
        checkboxInput(ns("includi_senza_stima"), "Includi Non Censiti senza stima", value = TRUE),
        actionButton(ns("reset"), "\U0001F504 Reset Filtri", class = "btn-block")
      )
    ),
    conditionalPanel(
      "output.caricato",
      ns = ns,
      sezione_sidebar("Statistiche", "chart-simple", uiOutput(ns("stat_box")))
    )
  )
}
