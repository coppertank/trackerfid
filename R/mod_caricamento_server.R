#' Modulo caricamento dati: logica
#'
#' Legge il file delle letture caricato, lo valida e mostra l'esito:
#' statistiche di caricamento oppure messaggi di errore espliciti.
#'
#' Un file molto grande non va per forza caricato dal browser: con
#' `run_app(letture = "percorso")` l'app lo legge dal disco all'avvio, una
#' volta sola per tutte le sessioni.
#'
#' Le letture finiscono in un archivio DuckDB, vedi
#' `carica_archivio()`. Quello di un file caricato dal browser appartiene alla
#' sessione: si chiude quando un altro file lo sostituisce o quando la
#' sessione finisce. Quelli del dataset di esempio e dei file letti dal disco
#' sono condivisi tra le sessioni e restano aperti.
#'
#' @param id Identificativo del modulo.
#' @param demo `TRUE` per aprire l'app con i dati di esempio già caricati.
#'   Il valore predefinito viene da `run_app(demo = )`.
#' @param percorso File delle letture da leggere dal disco all'avvio, oppure
#'   `NULL`. Il valore predefinito viene da `run_app(letture = )`.
#' @return Reactive con l'archivio delle letture, oppure `NULL` se manca un
#'   dataset valido.
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

    # Legge un file con `lettore` e ne scrive l'esito nello stato. L'archivio
    # sostituito si chiude, se apparteneva alla sessione.
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
    # Un archivio letto dal disco una volta per processo: lo usano tutte le
    # sessioni, e nessuna lo chiude.
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
    # Un file caricato dal browser ha un percorso temporaneo: l'estensione,
    # che dice il formato, sta nel nome originale. Con milioni di righe la
    # lettura dura qualche secondo: un avviso dice che è in corso.
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

    # `run_app(demo = TRUE)` apre l'app con i dati di esempio già caricati.
    # `run_app(letture = )` la apre con il file indicato, letto dal disco una
    # volta sola per tutte le sessioni.
    if (isTRUE(demo)) {
      carica_esempio()
    }
    if (!is.null(percorso)) {
      carica(archivio_condiviso, percorso, basename(percorso))
    }

    # Esito del caricamento: errori, oppure righe, RFID e periodo, dal
    # riepilogo dell'archivio.
    # Nome del file caricato, accanto al titolo della sezione chiusa.
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
      periodo <- formatta_data_ora(c(archivio$primo, archivio$ultimo), "%d/%m/%Y")
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
