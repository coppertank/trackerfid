#' Modulo caricamento dati: logica
#'
#' Legge il CSV caricato, lo valida e mostra l'esito: statistiche di
#' caricamento oppure messaggi di errore espliciti.
#'
#' @param id Identificativo del modulo.
#' @return Reactive con il dataset validato, `NULL` se non c'è un dataset valido.
#' @noRd
mod_caricamento_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(dati = NULL, errori = NULL, avvisi = NULL, origine = NULL)

    carica <- function(percorso, origine) {
      esito <- tryCatch(
        carica_dataset(percorso),
        errore_validazione = function(e) list(errori = e$messaggi),
        error = function(e) list(errori = paste("Impossibile leggere il file:", conditionMessage(e)))
      )
      stato$dati <- esito$dati
      stato$avvisi <- esito$avvisi
      stato$errori <- esito$errori
      stato$origine <- origine
    }

    observeEvent(input$file_upload, {
      carica(input$file_upload$datapath, input$file_upload$name)
    })

    observeEvent(input$carica_esempio, {
      carica(percorso_dataset_esempio(), "sample_rfid_dataset.csv (esempio)")
    })

    # `run_app(demo = TRUE)` apre l'app con il dataset di esempio già caricato.
    if (isTRUE(golem::get_golem_options("demo"))) {
      carica(percorso_dataset_esempio(), "sample_rfid_dataset.csv (esempio)")
    }

    output$esito <- renderUI({
      if (!is.null(stato$errori)) {
        return(div(
          class = "esito-caricamento esito-errore",
          tags$b("Caricamento non riuscito"),
          tags$div(class = "esito-file", stato$origine),
          tags$ul(purrr::map(stato$errori, tags$li))
        ))
      }
      dati <- stato$dati
      req(dati)
      periodo <- formatta_data_ora(range(dati$giorno_lettura), "%d/%m/%Y")
      div(
        class = "esito-caricamento esito-ok",
        tags$div(class = "esito-file", stato$origine),
        tags$div(tags$b(formatta_numero(nrow(dati))), " righe caricate"),
        tags$div(tags$b(formatta_numero(dplyr::n_distinct(dati$RFID))), " RFID univoci"),
        tags$div("Periodo: ", tags$b(periodo[1]), " a ", tags$b(periodo[2])),
        if (length(stato$avvisi) > 0) {
          tags$ul(class = "esito-avvisi", purrr::map(stato$avvisi, tags$li))
        }
      )
    })

    reactive(stato$dati)
  })
}
