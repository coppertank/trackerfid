#' The application User-Interface
#'
#' Layout a dashboard: barra laterale (collassabile) con caricamento, periodo
#' di analisi, dettaglio del bidone e controlli della scheda attiva; corpo con
#' le tre mappe e l'analisi dei cluster spaziali.
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shinydashboard::dashboardPage(
      skin = "green",
      title = "Dashboard RFID Bidoni Rifiuti",
      header = shinydashboard::dashboardHeader(
        # Il titolo riporta il periodo di analisi, ad esempio "ANNO 2025".
        title = textOutput("titolo", inline = TRUE),
        titleWidth = 360
      ),
      sidebar = shinydashboard::dashboardSidebar(
        width = 360,
        mod_caricamento_ui("caricamento"),
        mod_periodo_ui("periodo"),
        div(id = "pannello_info", uiOutput("info_panel")),
        # I controlli seguono la scheda attiva: filtri oppure ricerche.
        conditionalPanel("input.scheda_attiva == 'mappa'", mod_filtri_ui("filtri")),
        conditionalPanel("input.scheda_attiva == 'rfid'", mod_ricerca_ui("ricerca_rfid", "rfid")),
        conditionalPanel("input.scheda_attiva == 'utenza'", mod_ricerca_ui("ricerca_utenza", "utenza")),
        conditionalPanel("input.scheda_attiva == 'cluster'", mod_cluster_analysis_sidebar_ui("cluster"))
      ),
      body = shinydashboard::dashboardBody(
        fluidRow(
          shinydashboard::tabBox(
            id = "scheda_attiva",
            width = 12,
            tabPanel(
              title = tagList(icon("map-location-dot"), "Mappa Principale"),
              value = "mappa",
              mod_mappa_ui("mappa_principale", "cluster")
            ),
            tabPanel(
              title = tagList(icon("barcode"), "Ricerca RFID"),
              value = "rfid",
              mod_mappa_ui("mappa_rfid", "rfid")
            ),
            tabPanel(
              title = tagList(icon("user"), "Ricerca Utenza"),
              value = "utenza",
              mod_mappa_ui("mappa_utenza", "utenza")
            ),
            tabPanel(
              title = tagList(icon("circle-nodes"), "Analisi Cluster Spaziale"),
              value = "cluster",
              mod_cluster_analysis_ui("cluster")
            )
          )
        )
      )
    )
  )
}

#' Sezione della barra laterale con titolo e icona
#'
#' @param titolo Titolo della sezione.
#' @param icona Nome dell'icona Font Awesome.
#' @param ... Contenuto della sezione.
#' @param azione Elemento opzionale mostrato a destra del titolo.
#' @noRd
sezione_sidebar <- function(titolo, icona, ..., azione = NULL) {
  div(
    class = "sezione-sidebar",
    div(class = "titolo-sezione", span(icon(icona), titolo), azione),
    ...
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Dashboard RFID Bidoni Rifiuti"
    )
  )
}
