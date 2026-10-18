#' Modulo periodo di analisi: logica
#'
#' Il periodo è un intervallo di date. Al caricamento di un file vale l'anno
#' solare più recente del file, dal primo gennaio al 31 dicembre. Lo stato
#' vive sul server, come per i filtri: al caricamento di un nuovo dataset il
#' periodo si riallinea subito, senza attendere il browser.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con l'archivio delle letture (o `NULL`).
#' @return Lista di reactive: `periodo` (due date, `NULL` se non valido),
#'   `etichetta` (descrizione breve: "Anno 2025" per un anno intero, vedi
#'   `etichetta_periodo()`) e `dati` (selezione delle letture del periodo,
#'   vedi `seleziona_periodo()`; `NULL` senza dataset o periodo).
#' @noRd
mod_periodo_server <- function(id, dati) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(intervallo = NULL)

    output$caricato <- reactive(!is.null(dati()))
    outputOptions(output, "caricato", suspendWhenHidden = FALSE)

    # Nuovo dataset: l'anno più recente. Il calendario va dal primo gennaio
    # dell'anno più vecchio al 31 dicembre del più recente.
    observeEvent(
      dati(),
      {
        d <- dati()
        anni <- anni_disponibili(d)
        if (length(anni) == 0) {
          stato$intervallo <- NULL
          return(invisible())
        }
        stato$intervallo <- periodo_anno(anni[1])
        updateDateRangeInput(
          session,
          "intervallo",
          start = stato$intervallo[1],
          end = stato$intervallo[2],
          min = periodo_anno(min(anni))[1],
          max = stato$intervallo[2]
        )
      },
      ignoreNULL = FALSE,
      priority = 200
    )

    observeEvent(
      input$intervallo,
      {
        stato$intervallo <- lubridate::as_date(input$intervallo)
      },
      ignoreInit = TRUE
    )

    periodo <- reactive({
      intervallo <- stato$intervallo
      if (
        is.null(intervallo) ||
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
      seleziona_periodo(d, p)
    })

    # Nella barra in alto compare solo un avviso: un intervallo non valido o
    # un periodo senza letture. Il numero delle letture lo riporta
    # l'intestazione della mappa principale.
    output$riepilogo <- renderUI({
      req(dati(), !is.null(stato$intervallo))
      p <- periodo()
      if (is.null(p)) {
        return(span(
          class = "periodo-riepilogo periodo-avviso",
          "Intervallo non valido: indica due date, con l'inizio non successivo alla fine."
        ))
      }
      if (dati_periodo()$n == 0) {
        return(span(
          class = "periodo-riepilogo periodo-avviso",
          "Nessuna lettura nel periodo."
        ))
      }
      NULL
    })

    list(
      periodo = periodo,
      etichetta = reactive(etichetta_periodo(periodo())),
      dati = dati_periodo
    )
  })
}
