#' Ricerca dei giri: logica
#'
#' Trova le letture di un mezzo, o di tutti, in un intervallo di giorni, e le
#' riassume giro per giro. Lo stato sta sul server: al caricamento di nuove
#' letture la ricerca torna al giorno più recente, per tutti i mezzi.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con le letture del periodo di analisi (o `NULL`).
#' @return Lista di reactive: `risultati`, le letture da disegnare sulla
#'   mappa, cioè quelle trovate o quelle del giro scelto in tabella;
#'   `riepilogo`, la tabella dei giri; `messaggio`, l'avviso da mostrare sulla
#'   mappa quando non c'è niente da disegnare.
#' @noRd
mod_giro_server <- function(id, dati) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      mezzo = "",
      dal = NULL,
      al = NULL,
      solo = character(0)
    )

    # --- Valori predefiniti: nuove letture -------------------------------------
    observeEvent(
      dati(),
      {
        d <- dati()
        giorni <- giorni_letture(d)
        stato$mezzo <- ""
        stato$solo <- character(0)
        # Si parte dal giorno più recente: un giorno di letture sta sulla mappa.
        stato$dal <- giorni[2]
        stato$al <- giorni[2]
        updateSelectInput(
          session,
          "mezzo",
          choices = c("Tutti i mezzi" = "", mezzi_presenti(d)),
          selected = ""
        )
        updateCheckboxGroupInput(session, "solo", selected = character(0))
        if (!is.null(giorni)) {
          updateDateRangeInput(
            session,
            "giorni",
            start = giorni[2],
            end = giorni[2],
            min = giorni[1],
            max = giorni[2]
          )
        }
      },
      ignoreNULL = FALSE,
      priority = 100
    )

    # --- Dagli input allo stato --------------------------------------------------
    observeEvent(
      input$mezzo,
      {
        stato$mezzo <- input$mezzo %||% ""
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    observeEvent(
      input$giorni,
      {
        giorni <- lubridate::as_date(input$giorni)
        # Mentre si scrive una data l'intervallo può essere incompleto.
        if (length(giorni) == 2 && !anyNA(giorni) && giorni[1] <= giorni[2]) {
          stato$dal <- giorni[1]
          stato$al <- giorni[2]
        }
      },
      ignoreInit = TRUE
    )
    observeEvent(
      input$solo,
      {
        stato$solo <- input$solo %||% character(0)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # --- Letture trovate e giri ----------------------------------------------------
    trovate <- reactive({
      d <- dati()
      if (is.null(d) || is.null(stato$dal) || is.null(stato$al)) {
        return(NULL)
      }
      cerca_giri(d, stato$mezzo, stato$dal, stato$al, stato$solo)
    })
    riepilogo <- reactive(riepilogo_giri(trovate()))

    output$con_giri <- reactive(!is.null(trovate()) && nrow(trovate()) > 0)
    outputOptions(output, "con_giri", suspendWhenHidden = FALSE)

    output$tabella <- DT::renderDT(
      tabella_giri_dt(riepilogo()),
      server = TRUE
    )

    # Giro scelto in tabella: la riga vale finché la ricerca non cambia.
    giro_scelto <- reactiveVal(NULL)
    observeEvent(trovate(), giro_scelto(NULL), ignoreNULL = FALSE, priority = 10)
    observeEvent(
      input$tabella_rows_selected,
      {
        riga <- input$tabella_rows_selected
        giri <- riepilogo()
        giro_scelto(
          if (length(riga) == 1 && riga >= 1 && riga <= nrow(giri)) {
            giri[riga, ]
          }
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    risultati <- reactive({
      letture <- trovate()
      giro <- giro_scelto()
      if (is.null(letture) || is.null(giro)) {
        return(letture)
      }
      letture_del_giro(letture, giro)
    })

    output$esito <- renderUI({
      letture <- trovate()
      req(letture)
      giri <- riepilogo()
      div(
        class = "esito-ricerca",
        sprintf(
          "%s \u00b7 %s",
          conta(nrow(giri), "giro", "giri"),
          conta(nrow(letture), "lettura", "letture")
        ),
        if (sum(giri$fuori_giro) > 0) {
          div(
            sprintf(
              "%s con letture fuori giro",
              conta(sum(giri$fuori_giro > 0), "giro", "giri")
            )
          )
        }
      )
    })

    messaggio <- reactive({
      letture <- trovate()
      if (!is.null(letture) && nrow(letture) == 0) {
        "Nessuna lettura per il mezzo e i giorni scelti."
      }
    })

    list(risultati = risultati, riepilogo = riepilogo, messaggio = messaggio)
  })
}
