#' Modulo caricamento dati: logica
#'
#' Legge i file caricati, li valida e mostra l'esito: statistiche di
#' caricamento oppure messaggi di errore espliciti. I file sono due: le
#' letture con le antenne, da cui dipende tutta l'app, e le letture storiche,
#' facoltative, che servono all'istogramma per anno del dettaglio del bidone.
#'
#' Un file molto grande non va per forza caricato dal browser: con
#' `run_app(letture = "percorso", storico = "percorso")` l'app lo legge dal
#' disco all'avvio, una volta sola per tutte le sessioni.
#'
#' @param id Identificativo del modulo.
#' @param demo `TRUE` per aprire l'app con i dati di esempio già caricati.
#'   Il valore predefinito viene da `run_app(demo = )`.
#' @param percorsi Lista con `letture` e `storico`: i file da leggere dal
#'   disco all'avvio, oppure `NULL`. Il valore predefinito viene da
#'   `run_app(letture = , storico = )`.
#' @return Lista di due reactive, `letture` e `storico`: ciascuno contiene il
#'   dataset validato, oppure `NULL` se manca un dataset valido.
#' @noRd
mod_caricamento_server <- function(
  id,
  demo = isTRUE(golem::get_golem_options("demo")),
  percorsi = list(
    letture = golem::get_golem_options("letture"),
    storico = golem::get_golem_options("storico")
  )
) {
  force(demo)
  force(percorsi)
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
    # Un file caricato dal browser ha un percorso temporaneo: l'estensione,
    # che dice il formato, sta nel nome originale. Con milioni di righe la
    # lettura dura qualche secondo: un avviso dice che è in corso.
    carica_file <- function(destinazione, lettore, file) {
      withProgress(
        message = "Lettura e controllo del file\u2026",
        value = 0.5,
        carica(
          destinazione,
          function(percorso) lettore(percorso, file$name),
          file$datapath,
          file$name
        )
      )
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
      carica_file(stato, carica_dataset, input$file_upload)
      # Le letture storiche di esempio non riguardano un file caricato a mano.
      if (identical(stato_storico$origine, storico_esempio)) {
        for (campo in names(stato_storico)) {
          stato_storico[[campo]] <- NULL
        }
      }
    })

    observeEvent(input$file_storico, {
      carica_file(stato_storico, carica_storico, input$file_storico)
    })

    observeEvent(input$carica_esempio, carica_esempio())

    # `run_app(demo = TRUE)` apre l'app con i dati di esempio già caricati.
    # `run_app(letture = , storico = )` la apre con i file indicati, letti dal
    # disco una volta sola per tutte le sessioni.
    if (isTRUE(demo)) {
      carica_esempio()
    }
    if (!is.null(percorsi$letture)) {
      carica(
        stato,
        function(percorso) leggi_una_volta(carica_dataset, percorso),
        percorsi$letture,
        basename(percorsi$letture)
      )
    }
    if (!is.null(percorsi$storico)) {
      carica(
        stato_storico,
        function(percorso) leggi_una_volta(carica_storico, percorso),
        percorsi$storico,
        basename(percorsi$storico)
      )
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
