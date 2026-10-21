#' Ricerca dei giri: letture di un mezzo in un intervallo di giorni, giro per giro
#'
#' Stato sul server; nuove letture riportano all'ultimo giorno. La mappa
#' riceve solo le letture del giro scelto in tabella.
#' @param dati Reactive con la selezione del periodo, o `NULL`.
#' @return Lista di reactive: `risultati` (letture del giro scelto, o
#'   `NULL`), `riepilogo`, `messaggio`, `vista` (`area_giri()`).
#' @noRd
mod_giro_server <- function(id, dati) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      mezzo = "",
      dal = NULL,
      al = NULL,
      solo = character(0)
    )

    # --- Nuove letture --------------------------------------------------------------
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

    output$tabella <- reactable::renderReactable(tabella_giri(riepilogo()))

    # Il giro scelto vale finché la ricerca non cambia.
    giro_scelto <- reactiveVal(NULL)
    observeEvent(
      trovate(),
      giro_scelto(NULL),
      ignoreNULL = FALSE,
      priority = 10
    )
    observeEvent(
      reactable::getReactableState("tabella", "selected"),
      {
        riga <- reactable::getReactableState("tabella", "selected")
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
        return(NULL)
      }
      letture_del_giro(letture, giro)
    })

    # Con un giro scelto: le sue letture e il suo comune; senza, tutti i giri.
    vista <- reactive({
      area_giri(risultati() %||% trovate())
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
      if (is.null(letture)) {
        return(NULL)
      }
      if (nrow(letture) == 0) {
        return("Nessuna lettura per il mezzo e i giorni scelti.")
      }
      if (is.null(giro_scelto())) {
        "Scegli un giro nella tabella per vederne le letture sulla mappa."
      }
    })

    list(
      risultati = risultati,
      riepilogo = riepilogo,
      messaggio = messaggio,
      vista = vista
    )
  })
}
