#' Modulo caricamento dati: logica
#'
#' Legge i CSV caricati, li valida e mostra l'esito: statistiche di
#' caricamento oppure messaggi di errore espliciti. I file sono due: le
#' letture con le antenne, da cui dipende tutta l'app, e le letture storiche,
#' facoltative, che servono all'istogramma per anno del dettaglio del bidone.
#'
#' @param id Identificativo del modulo.
#' @return Lista di due reactive, `letture` e `storico`: ciascuno contiene il
#'   dataset validato, oppure `NULL` se manca un dataset valido.
#' @noRd
mod_caricamento_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      dati = NULL,
      errori = NULL,
      avvisi = NULL,
      origine = NULL
    )
    stato_storico <- reactiveValues(
      dati = NULL,
      errori = NULL,
      avvisi = NULL,
      origine = NULL
    )
    storico_esempio <- "sample_letture_storiche.csv (esempio)"

    # Legge un file con `lettore` e ne scrive l'esito in `destinazione`.
    carica <- function(destinazione, lettore, percorso, origine) {
      esito <- tryCatch(
        lettore(percorso),
        errore_validazione = function(e) list(errori = e$messaggi),
        error = function(e) {
          list(
            errori = paste("Impossibile leggere il file:", conditionMessage(e))
          )
        }
      )
      destinazione$dati <- esito$dati
      destinazione$avvisi <- esito$avvisi
      destinazione$errori <- esito$errori
      destinazione$origine <- origine
    }
    carica_esempio <- function() {
      carica(
        stato,
        carica_dataset,
        percorso_dataset_esempio(),
        "sample_rfid_dataset.csv (esempio)"
      )
      carica(
        stato_storico,
        carica_storico,
        percorso_storico_esempio(),
        storico_esempio
      )
    }

    observeEvent(input$file_upload, {
      carica(
        stato,
        carica_dataset,
        input$file_upload$datapath,
        input$file_upload$name
      )
      # Le letture storiche di esempio non riguardano un file caricato a mano.
      if (identical(stato_storico$origine, storico_esempio)) {
        for (campo in names(stato_storico)) {
          stato_storico[[campo]] <- NULL
        }
      }
    })

    observeEvent(input$file_storico, {
      carica(
        stato_storico,
        carica_storico,
        input$file_storico$datapath,
        input$file_storico$name
      )
    })

    observeEvent(input$carica_esempio, carica_esempio())

    # `run_app(demo = TRUE)` apre l'app con i dati di esempio già caricati.
    if (isTRUE(golem::get_golem_options("demo"))) {
      carica_esempio()
    }

    # Riquadro con l'esito di un caricamento: errori, oppure righe, RFID e periodo.
    riquadro_esito <- function(esito, etichetta_righe) {
      if (!is.null(esito$errori)) {
        return(div(
          class = "esito-caricamento esito-errore",
          tags$b("Caricamento non riuscito"),
          tags$div(class = "esito-file", esito$origine),
          tags$ul(purrr::map(esito$errori, tags$li))
        ))
      }
      dati <- esito$dati
      req(dati)
      periodo <- formatta_data_ora(range(dati$giorno_lettura), "%d/%m/%Y")
      div(
        class = "esito-caricamento esito-ok",
        tags$div(class = "esito-file", esito$origine),
        tags$div(tags$b(formatta_numero(nrow(dati))), etichetta_righe),
        tags$div(
          tags$b(formatta_numero(dplyr::n_distinct(dati$RFID))),
          " RFID univoci"
        ),
        tags$div("Periodo: ", tags$b(periodo[1]), " a ", tags$b(periodo[2])),
        if (length(esito$avvisi) > 0) {
          tags$ul(class = "esito-avvisi", purrr::map(esito$avvisi, tags$li))
        }
      )
    }

    output$esito <- renderUI(riquadro_esito(stato, " righe caricate"))
    output$esito_storico <- renderUI(riquadro_esito(
      stato_storico,
      " letture storiche"
    ))

    list(
      letture = reactive(stato$dati),
      storico = reactive(stato_storico$dati)
    )
  })
}
