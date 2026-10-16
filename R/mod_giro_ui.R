#' Ricerca dei giri: controlli nella barra laterale
#'
#' Si sceglie un mezzo, o tutti, e un giorno o un intervallo di giorni. Le
#' letture si possono restringere a quelle fuori giro o fuori comune.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_giro_sidebar_ui <- function(id) {
  ns <- NS(id)
  sezione_sidebar(
    "Ricerca per Giro",
    "truck",
    selectInput(
      ns("mezzo"),
      "Mezzo",
      choices = c("Tutti i mezzi" = ""),
      selectize = FALSE,
      width = "100%"
    ),
    dateRangeInput(
      ns("giorni"),
      "Giorno, o dal / al",
      format = "dd/mm/yyyy",
      language = "it",
      weekstart = 1,
      separator = "al",
      width = "100%"
    ),
    checkboxGroupInput(
      ns("solo"),
      "Mostra solo le letture",
      choiceNames = c("Fuori giro", "Fuori comune"),
      choiceValues = c("fuori_giro", "fuori_comune"),
      inline = FALSE
    ),
    p(
      class = "nota-filtro",
      "Fuori giro: non censito letto in un comune diverso da quello del giro.",
      "Fuori comune: censito letto in un comune diverso da quello del database."
    ),
    uiOutput(ns("esito"))
  )
}

#' Ricerca dei giri: tabella dei giri trovati
#'
#' Sta sopra la mappa della scheda. Un click su una riga mostra sulla mappa
#' le sole letture di quel giro.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_giro_ui <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    "output.con_giri",
    ns = ns,
    div(
      class = "tabella-giri",
      div(
        class = "titolo-giri",
        "Giri trovati: che cosa doveva fare il mezzo e che cosa ha letto.",
        "Un click su una riga mostra sulla mappa le sole letture di quel giro."
      ),
      DT::DTOutput(ns("tabella"))
    )
  )
}
