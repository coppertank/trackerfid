#' Modulo analisi cluster spaziale: contenuto della scheda
#'
#' Tre viste dello stesso risultato: tabella interattiva, grafici e mappa.
#' I filtri della tabella valgono anche per grafici e mappa.
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_cluster_analysis_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "pannello-cluster",
    div(
      class = "mappa-intestazione",
      span(class = "badge-stato badge-cluster", "Visualizzazione: Analisi Cluster"),
      textOutput(ns("riepilogo"), inline = TRUE)
    ),
    conditionalPanel(
      "!output.pronta",
      ns = ns,
      div(class = "cluster-invito", textOutput(ns("invito")))
    ),
    conditionalPanel(
      "output.pronta",
      ns = ns,
      tabsetPanel(
        id = ns("vista"),
        type = "pills",
        tabPanel(
          "Tabella",
          value = "tabella", icon = icon("table"),
          div(class = "vista-cluster", DT::DTOutput(ns("tabella")))
        ),
        tabPanel(
          "Grafici",
          value = "grafici", icon = icon("chart-simple"),
          div(
            class = "vista-cluster",
            plotly::plotlyOutput(ns("grafico_indicatori"), height = "250px"),
            plotly::plotlyOutput(ns("grafico_dispersione"), height = "500px"),
            p(
              class = "nota-grafico",
              "Assi logaritmici. Le linee della griglia corrispondono alle soglie della",
              "classificazione; in grigio i cluster degli altri indicatori."
            )
          )
        ),
        tabPanel(
          "Mappa",
          value = "mappa", icon = icon("map-location-dot"),
          div(
            class = "vista-cluster mappa-contenitore mappa-cluster",
            leaflet::leafletOutput(ns("mappa"), height = "100%")
          )
        )
      )
    )
  )
}

#' Modulo analisi cluster spaziale: controlli nella barra laterale
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_cluster_analysis_sidebar_ui <- function(id) {
  ns <- NS(id)
  sezione_sidebar(
    "Analisi Cluster Spaziale", "circle-nodes",
    uiOutput(ns("periodo")),
    tags$details(
      class = "parametri-dbscan",
      tags$summary("Parametri DBSCAN"),
      numericInput(ns("eps_m"), "Raggio di ricerca (metri)", value = 100, min = 1, step = 10, width = "100%"),
      numericInput(ns("min_pts"), "Letture minime per cluster", value = 1, min = 1, step = 1, width = "100%")
    ),
    div(
      class = "pulsanti-ricerca",
      actionButton(ns("genera"), "Genera Analisi", icon = icon("play"), class = "btn-primary"),
      conditionalPanel(
        "output.pronta",
        ns = ns,
        downloadButton(ns("esporta"), "Esporta CSV")
      )
    ),
    uiOutput(ns("esito"))
  )
}
