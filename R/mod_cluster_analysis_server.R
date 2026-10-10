#' Modulo analisi cluster spaziale: logica
#'
#' L'analisi parte dal pulsante «Genera Analisi» e riguarda tutte le letture
#' del periodo scelto nella barra laterale. Se cambiano dataset o periodo il
#' risultato viene scartato, così non resta a schermo un'analisi superata.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con il dataset validato completo (o `NULL`).
#' @param periodo Reactive con le due date del periodo di analisi (o `NULL`).
#' @param etichetta Reactive con la descrizione breve del periodo.
#' @return Reactive con l'ultimo cluster scelto in tabella o sulla mappa:
#'   lista con `rfid` e `quando`.
#' @noRd
mod_cluster_analysis_server <- function(
  id,
  dati,
  periodo,
  etichetta = function() NULL
) {
  moduleServer(id, function(input, output, session) {
    risultato <- reactiveVal(NULL)
    secondi <- reactiveVal(NULL)

    observeEvent(list(dati(), periodo()), risultato(NULL), ignoreNULL = FALSE)

    observeEvent(input$genera, {
      d <- dati()
      p <- periodo()
      if (is.null(d) || is.null(p)) {
        showNotification(
          "Carica un file CSV e scegli un periodo valido.",
          type = "warning"
        )
        return()
      }
      eps_m <- input$eps_m
      min_pts <- input$min_pts
      if (!isTRUE(eps_m > 0) || !isTRUE(min_pts >= 1)) {
        showNotification(
          "Parametri DBSCAN non validi: il raggio deve essere positivo e le letture minime almeno 1.",
          type = "error"
        )
        return()
      }
      inizio <- Sys.time()
      analisi <- withProgress(
        message = "Calcolo dei cluster in corso\u2026",
        value = 0.6,
        calcola_analisi_cluster(
          d,
          p[1],
          p[2],
          eps_m = eps_m,
          min_pts = round(min_pts)
        )
      )
      secondi(as.numeric(difftime(Sys.time(), inizio, units = "secs")))
      risultato(analisi)
    })

    pronta <- reactive(!is.null(risultato()) && nrow(risultato()) > 0)
    output$pronta <- reactive(pronta())
    outputOptions(output, "pronta", suspendWhenHidden = FALSE)

    # --- Barra laterale ----------------------------------------------------------
    output$periodo <- renderUI({
      testo <- etichetta()
      if (is.null(dati())) {
        p(class = "suggerimento", "Carica un file CSV per avviare l'analisi.")
      } else if (is.null(testo)) {
        p(class = "suggerimento", "Scegli un periodo di analisi valido.")
      } else {
        p(class = "suggerimento", "Periodo analizzato:", tags$b(testo))
      }
    })

    output$esito <- renderUI({
      analisi <- risultato()
      req(analisi)
      if (nrow(analisi) == 0) {
        return(div(
          class = "esito-ricerca esito-avviso",
          "Nessuna lettura nel periodo selezionato."
        ))
      }
      parametri <- attr(analisi, "parametri")
      div(
        class = "esito-ricerca esito-ok",
        tags$div(
          conta(nrow(analisi), "cluster", "cluster"),
          " di ",
          conta(dplyr::n_distinct(analisi$RFID), "RFID", "RFID")
        ),
        tags$div(
          class = "esito-non-trovati",
          sprintf(
            "Raggio %s m, letture minime %s \u00b7 %s giorni osservati \u00b7 %.1f s",
            formatta_numero(parametri$eps_m),
            parametri$min_pts,
            formatta_numero(parametri$giorni_osservati),
            secondi()
          )
        )
      )
    })

    output$esporta <- downloadHandler(
      filename = function() {
        p <- isolate(periodo())
        nome_file_analisi_cluster(p[1], p[2])
      },
      content = function(file) esporta_analisi_cluster(risultato(), file)
    )

    # --- Intestazione e invito -------------------------------------------------------
    output$riepilogo <- renderText({
      if (!pronta()) {
        return("")
      }
      analisi <- risultato()
      paste(
        conta(nrow(analisi), "cluster", "cluster"),
        "di",
        conta(dplyr::n_distinct(analisi$RFID), "RFID", "RFID"),
        "\u00b7",
        etichetta()
      )
    })

    output$invito <- renderText({
      if (is.null(dati())) {
        "Carica un file CSV dal pannello laterale per analizzare i cluster spaziali."
      } else if (is.null(periodo())) {
        "Scegli un periodo di analisi valido nel pannello laterale."
      } else if (!is.null(risultato())) {
        "Nessuna lettura nel periodo selezionato."
      } else {
        paste0(
          "Premi \u00abGenera Analisi\u00bb nel pannello laterale per calcolare i cluster spaziali del periodo: ",
          etichetta(),
          "."
        )
      }
    })

    # --- Tabella -------------------------------------------------------------------
    output$tabella <- DT::renderDT({
      req(pronta())
      tabella_cluster_dt(risultato())
    })

    # Righe mostrate dalla tabella: `NULL` vuol dire tutte, un vettore vuoto
    # nessuna. Una nuova analisi le azzera: il browser riporta le righe della
    # tabella precedente finché quella nuova non viene disegnata, e la tabella
    # non si ridisegna mentre la sua vista è nascosta.
    righe_tabella <- reactiveVal(NULL)
    observeEvent(
      risultato(),
      righe_tabella(NULL),
      ignoreNULL = FALSE,
      priority = 10
    )
    # Il browser comunica un elenco vuoto quando il filtro non lascia righe,
    # ma anche, per qualche centesimo di secondo, ogni volta che ridisegna la
    # tabella: un elenco vale solo se dura.
    righe_browser <- debounce(
      reactive(input$tabella_rows_all),
      attesa_righe_tabella()
    )
    observeEvent(
      righe_browser(),
      {
        indici <- as.integer(unlist(righe_browser()))
        # Tutte le righe, in qualunque ordine: come nessun filtro.
        tutte <- length(indici) == NROW(risultato())
        righe_tabella(if (!tutte) indici)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # Grafici e mappa seguono i filtri della tabella.
    righe_filtrate <- reactive({
      req(pronta())
      analisi <- risultato()
      indici <- righe_tabella()
      if (is.null(indici)) {
        return(analisi)
      }
      # Un indice arrivato in ritardo può superare la fine della tabella nuova.
      analisi[indici[indici <= nrow(analisi)], , drop = FALSE]
    })

    # --- Grafici ---------------------------------------------------------------------
    output$grafico_indicatori <- plotly::renderPlotly({
      grafico_indicatori(righe_filtrate())
    })

    output$grafico_dispersione <- plotly::renderPlotly({
      righe <- righe_filtrate()
      validate(need(
        nrow(righe) > 0,
        "Nessun cluster corrisponde ai filtri della tabella."
      ))
      parametri <- attr(risultato(), "parametri")
      grafico_dispersione(righe, parametri$soglie, parametri$giorni_osservati)
    })

    # --- Mappa -----------------------------------------------------------------------
    output$mappa <- leaflet::renderLeaflet({
      mappa_cluster(righe_filtrate())
    })

    # --- Selezione di un cluster: riga della tabella o punto sulla mappa -----------------
    selezione <- reactiveVal(NULL)
    scegli <- function(rfid) selezione(list(rfid = rfid, quando = Sys.time()))

    observeEvent(input$tabella_rows_selected, {
      scegli(risultato()$RFID[input$tabella_rows_selected])
    })
    observeEvent(input$mappa_marker_click, {
      righe <- righe_filtrate()
      riga <- suppressWarnings(as.integer(input$mappa_marker_click$id))
      req(length(riga) == 1, !is.na(riga), riga >= 1, riga <= nrow(righe))
      scegli(righe$RFID[riga])
    })

    selezione
  })
}
