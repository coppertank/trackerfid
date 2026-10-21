#' Interfaccia: barra in alto con titolo e periodo, barra laterale, cinque schede
#' @param request Parametro interno di shiny: non togliere.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    shinydashboard::dashboardPage(
      skin = "green",
      title = "Dashboard RFID Bidoni Rifiuti",
      header = shinydashboard::dashboardHeader(
        # Il titolo riporta il periodo di analisi, ad esempio "ANNO 2025".
        title = textOutput("titolo", inline = TRUE),
        titleWidth = 360,
        mod_periodo_ui("periodo")
      ),
      sidebar = shinydashboard::dashboardSidebar(
        width = 360,
        mod_caricamento_ui("caricamento"),
        div(id = "pannello_info", uiOutput("info_panel")),
        # I controlli seguono la scheda attiva: statistiche della mappa
        # principale, i cui filtri stanno sopra la mappa, oppure ricerche.
        conditionalPanel(
          "input.scheda_attiva == 'mappa'",
          mod_filtri_sidebar_ui("filtri")
        ),
        conditionalPanel(
          "input.scheda_attiva == 'rfid'",
          mod_ricerca_ui("ricerca_rfid", "rfid")
        ),
        conditionalPanel(
          "input.scheda_attiva == 'giro'",
          mod_giro_sidebar_ui("giro")
        ),
        conditionalPanel(
          "input.scheda_attiva == 'utenza'",
          mod_ricerca_ui("ricerca_utenza", "utenza")
        ),
        conditionalPanel(
          "input.scheda_attiva == 'cluster'",
          mod_cluster_analysis_sidebar_ui("cluster")
        )
      ),
      body = shinydashboard::dashboardBody(
        fluidRow(
          shinydashboard::tabBox(
            id = "scheda_attiva",
            width = 12,
            tabPanel(
              title = tagList(icon("map-location-dot"), "Mappa Principale"),
              value = "mappa",
              div(
                class = "scheda-mappa",
                mod_filtri_ui("filtri"),
                mod_mappa_ui("mappa_principale", "cluster")
              )
            ),
            tabPanel(
              title = tagList(icon("barcode"), "Ricerca RFID"),
              value = "rfid",
              mod_mappa_ui(
                "mappa_rfid",
                "rfid",
                sovrapposto = mod_ricerca_tabella_ui("ricerca_rfid")
              )
            ),
            tabPanel(
              title = tagList(icon("truck"), "Ricerca Giro"),
              value = "giro",
              mod_mappa_ui(
                "mappa_giro",
                "giro",
                sovrapposto = mod_giro_ui("giro")
              )
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
#' Collassabile: un `details` aperto all'avvio, che chiuso mostra
#' `riassunto` accanto al titolo.
#' @param azione Elemento a destra del titolo.
#' @noRd
sezione_sidebar <- function(
  titolo,
  icona,
  ...,
  azione = NULL,
  collassabile = FALSE,
  riassunto = NULL
) {
  if (!collassabile) {
    return(div(
      class = "sezione-sidebar",
      div(class = "titolo-sezione", span(icon(icona), titolo), azione),
      ...
    ))
  }
  tags$details(
    open = NA,
    class = "sezione-sidebar sezione-collassabile",
    tags$summary(
      class = "titolo-sezione",
      title = "Mostra o nascondi la sezione",
      span(
        class = "titolo-sezione-testo",
        icon(icona),
        titolo,
        if (!is.null(riassunto)) span(class = "riassunto-sezione", riassunto)
      ),
      azione
    ),
    div(class = "corpo-sezione", ...)
  )
}

#' Risorse esterne: CSS, JavaScript, favicon e cartella dei confini dei comuni
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  # Il file dei confini, che le mappe scaricano da sé: `indirizzo_confini()`.
  add_resource_path(
    risorsa_confini(),
    dirname(percorso_confini_comuni())
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Dashboard RFID Bidoni Rifiuti"
    )
  )
}
