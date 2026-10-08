#' Modulo periodo di analisi: logica
#'
#' Il periodo predefinito è l'anno solare più recente del dataset. Lo stato
#' vive sul server, come per i filtri: al caricamento di un nuovo dataset il
#' periodo si riallinea subito, senza attendere il browser.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con il dataset validato (o `NULL`).
#' @return Lista di reactive: `periodo` (due date, `NULL` se non valido),
#'   `etichetta` (descrizione breve) e `dati` (letture del periodo, con il
#'   servizio da usare per l'icona; `NULL` senza dataset o periodo).
#' @noRd
mod_periodo_server <- function(id, dati) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      anno = NULL,
      personalizzato = FALSE,
      intervallo = NULL
    )

    output$caricato <- reactive(!is.null(dati()))
    outputOptions(output, "caricato", suspendWhenHidden = FALSE)

    imposta_anno <- function(anno) {
      stato$anno <- anno
      stato$intervallo <- periodo_anno(anno)
      updateDateRangeInput(
        session,
        "intervallo",
        start = stato$intervallo[1],
        end = stato$intervallo[2]
      )
    }

    # Nuovo dataset: anno più recente, periodo personalizzato disattivato.
    observeEvent(
      dati(),
      {
        d <- dati()
        stato$personalizzato <- FALSE
        updateCheckboxInput(session, "personalizzato", value = FALSE)
        if (is.null(d)) {
          stato$anno <- NULL
          stato$intervallo <- NULL
          return(invisible())
        }
        anni <- anni_disponibili(d)
        updateSelectInput(
          session,
          "anno_filtro",
          choices = anni,
          selected = anni[1]
        )
        imposta_anno(anni[1])
      },
      ignoreNULL = FALSE,
      priority = 200
    )

    observeEvent(
      input$anno_filtro,
      {
        anno <- suppressWarnings(as.integer(input$anno_filtro))
        req(!is.na(anno))
        if (!identical(anno, as.integer(stato$anno))) imposta_anno(anno)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$personalizzato,
      {
        stato$personalizzato <- isTRUE(input$personalizzato)
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$intervallo,
      {
        stato$intervallo <- lubridate::as_date(input$intervallo)
      },
      ignoreInit = TRUE
    )

    periodo <- reactive({
      if (is.null(stato$anno)) {
        return(NULL)
      }
      if (!stato$personalizzato) {
        return(periodo_anno(stato$anno))
      }
      intervallo <- stato$intervallo
      if (
        length(intervallo) != 2 ||
          anyNA(intervallo) ||
          intervallo[1] > intervallo[2]
      ) {
        return(NULL)
      }
      intervallo
    })

    dati_periodo <- reactive({
      d <- dati()
      p <- periodo()
      if (is.null(d) || is.null(p)) {
        return(NULL)
      }
      aggiungi_servizio_icona(filtra_periodo(d, p))
    })

    output$riepilogo <- renderUI({
      req(dati())
      p <- periodo()
      if (is.null(p)) {
        return(div(
          class = "esito-ricerca esito-avviso",
          "Intervallo non valido: indica due date, con l'inizio non successivo alla fine."
        ))
      }
      d <- dati_periodo()
      div(
        class = paste(
          "esito-ricerca",
          if (nrow(d) > 0) "esito-ok" else "esito-avviso"
        ),
        tags$div(paste(format(p, "%d/%m/%Y"), collapse = " \u2013 ")),
        tags$div(
          if (nrow(d) > 0) {
            paste(
              conta(nrow(d), "lettura", "letture"),
              "di",
              conta(dplyr::n_distinct(d$RFID), "RFID", "RFID")
            )
          } else {
            "Nessuna lettura nel periodo."
          }
        )
      )
    })

    list(
      periodo = periodo,
      etichetta = reactive(etichetta_periodo(periodo())),
      dati = dati_periodo
    )
  })
}
