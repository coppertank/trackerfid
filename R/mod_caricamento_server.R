#' Caricamento delle letture nell'archivio DuckDB, con esito o errori
#'
#' L'archivio di un file caricato dal browser è della sessione e si chiude
#' quando viene sostituito o la sessione finisce; quelli dell'esempio e dei
#' file letti dal disco sono condivisi e restano aperti.
#' @param demo,percorso Da `run_app(demo = , letture = )`.
#' @return Reactive con l'archivio, `NULL` senza un dataset valido.
#' @noRd
mod_caricamento_server <- function(
  id,
  demo = isTRUE(golem::get_golem_options("demo")),
  percorso = golem::get_golem_options("letture")
) {
  force(demo)
  force(percorso)
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      dati = NULL,
      errori = NULL,
      avvisi = NULL,
      origine = NULL
    )

    # Legge con `lettore`; chiude l'archivio sostituito, se della sessione.
    carica <- function(lettore, percorso, origine) {
      esito <- tryCatch(
        lettore(percorso),
        errore_validazione = function(e) list(errori = e$messaggi),
        error = function(e) {
          list(
            errori = paste("Impossibile leggere il file:", conditionMessage(e))
          )
        }
      )
      precedente <- isolate(stato$dati)
      if (inherits(precedente, "archivio_letture")) {
        chiudi_archivio(precedente)
      }
      stato$dati <- esito$archivio
      stato$avvisi <- esito$avvisi
      stato$errori <- esito$errori
      stato$origine <- origine
    }
    # Letto dal disco una volta per processo, condiviso, mai chiuso.
    archivio_condiviso <- function(percorso) {
      leggi_una_volta(
        function(p) {
          esito <- carica_archivio(p)
          esito$archivio$condiviso <- TRUE
          esito
        },
        percorso
      )
    }
    session$onSessionEnded(function() {
      archivio <- isolate(stato$dati)
      if (inherits(archivio, "archivio_letture")) chiudi_archivio(archivio)
    })
    # Il percorso è temporaneo: il formato lo dice il nome originale.
    carica_file <- function(file) {
      withProgress(
        message = "Lettura e controllo del file\u2026",
        value = 0.5,
        carica(
          function(percorso) carica_archivio(percorso, file$name),
          file$datapath,
          file$name
        )
      )
    }
    carica_esempio <- function() {
      carica(
        archivio_condiviso,
        percorso_dataset_esempio(),
        "sample_rfid_dataset.csv (esempio)"
      )
    }

    observeEvent(input$file_upload, carica_file(input$file_upload))
    observeEvent(input$carica_esempio, carica_esempio())

    if (isTRUE(demo)) {
      carica_esempio()
    }
    if (!is.null(percorso)) {
      carica(archivio_condiviso, percorso, basename(percorso))
    }

    # Esito: errori, oppure righe, RFID e periodo.
    output$riassunto <- renderText({
      if (is.null(stato$dati)) "" else stato$origine
    })

    output$esito <- renderUI({
      if (!is.null(stato$errori)) {
        return(div(
          class = "esito-caricamento esito-errore",
          tags$b("Caricamento non riuscito"),
          tags$div(class = "esito-file", stato$origine),
          tags$ul(purrr::map(stato$errori, tags$li))
        ))
      }
      archivio <- stato$dati
      req(archivio)
      periodo <- formatta_data_ora(
        c(archivio$primo, archivio$ultimo),
        "%d/%m/%Y"
      )
      div(
        class = "esito-caricamento esito-ok",
        tags$div(class = "esito-file", stato$origine),
        tags$div(tags$b(formatta_numero(archivio$n)), " righe caricate"),
        tags$div(tags$b(formatta_numero(archivio$n_rfid)), " RFID univoci"),
        tags$div("Periodo: ", tags$b(periodo[1]), " a ", tags$b(periodo[2])),
        if (length(stato$avvisi) > 0) {
          tags$ul(class = "esito-avvisi", purrr::map(stato$avvisi, tags$li))
        }
      )
    })

    reactive(stato$dati)
  })
}
