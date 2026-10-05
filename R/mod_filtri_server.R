#' Modulo filtri laterali: logica
#'
#' Lo stato dei filtri vive sul server e viene riallineato agli input: al
#' caricamento di un nuovo dataset i filtri tornano subito ai valori
#' predefiniti, senza attendere la risposta del browser.
#'
#' I servizi deselezionati sono memorizzati per esclusione, così le scelte
#' dell'utente sopravvivono quando l'elenco dei servizi cambia con il periodo.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con le letture del periodo di analisi (o `NULL`).
#' @param azzera Reactive il cui cambiamento riporta i filtri ai valori
#'   predefiniti: di norma il dataset caricato.
#' @return Lista con `dati_filtrati`: reactive con l'ultima lettura di ogni
#'   RFID tra quelle che superano i filtri.
#' @noRd
mod_filtri_server <- function(id, dati, azzera = dati) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      stato_db = stati_database(),
      transponder_esclusi = character(0),
      includi_non_censiti = TRUE,
      atteso_esclusi = character(0),
      includi_senza_stima = TRUE,
      reimpostazioni = 0L
    )

    output$caricato <- reactive(!is.null(dati()))
    outputOptions(output, "caricato", suspendWhenHidden = FALSE)

    # --- Valori predefiniti: nuovo dataset o pulsante "Reset Filtri" -----------
    reimposta <- function() {
      stato$stato_db <- stati_database()
      stato$transponder_esclusi <- character(0)
      stato$includi_non_censiti <- TRUE
      stato$atteso_esclusi <- character(0)
      stato$includi_senza_stima <- TRUE
      stato$reimpostazioni <- stato$reimpostazioni + 1L
      updateCheckboxGroupInput(session, "presente_filter", selected = stati_database())
      updateCheckboxInput(session, "includi_non_censiti", value = TRUE)
      updateCheckboxInput(session, "includi_senza_stima", value = TRUE)
    }
    observeEvent(azzera(), reimposta(), ignoreNULL = FALSE, priority = 100)
    observeEvent(input$reset, reimposta(), priority = 100)

    # --- Scelte dinamiche dei servizi: valori presenti nel periodo -------------
    scelte_transponder <- reactive(ordina_servizi(dati()$servizio_transponder))
    scelte_atteso <- reactive(ordina_servizi(dati()$servizio_atteso))

    aggiorna_gruppo <- function(id_input, scelte, esclusi) {
      etichette <- paste(info_servizio(scelte)$emoji, scelte)
      updateCheckboxGroupInput(
        session, id_input,
        choices = stats::setNames(scelte, etichette),
        selected = setdiff(scelte, esclusi)
      )
    }
    observe({
      stato$reimpostazioni
      aggiorna_gruppo(
        "servizio_transponder_check", scelte_transponder(), isolate(stato$transponder_esclusi)
      )
    })
    observe({
      stato$reimpostazioni
      aggiorna_gruppo("servizio_atteso_check", scelte_atteso(), isolate(stato$atteso_esclusi))
    })

    # --- Dagli input allo stato --------------------------------------------------
    observeEvent(input$presente_filter,
      {
        stato$stato_db <- input$presente_filter %||% character(0)
      },
      ignoreNULL = FALSE, ignoreInit = TRUE
    )
    observeEvent(input$includi_non_censiti,
      {
        stato$includi_non_censiti <- isTRUE(input$includi_non_censiti)
      },
      ignoreInit = TRUE
    )
    observeEvent(input$includi_senza_stima,
      {
        stato$includi_senza_stima <- isTRUE(input$includi_senza_stima)
      },
      ignoreInit = TRUE
    )

    # Un servizio visibile è escluso se non è spuntato; i servizi fuori dal
    # periodo corrente conservano lo stato precedente.
    aggiorna_esclusi <- function(esclusi, scelte, selezionati) {
      sort(union(setdiff(esclusi, scelte), setdiff(scelte, selezionati %||% character(0))))
    }
    observeEvent(input$servizio_transponder_check,
      {
        stato$transponder_esclusi <- aggiorna_esclusi(
          stato$transponder_esclusi, scelte_transponder(), input$servizio_transponder_check
        )
      },
      ignoreNULL = FALSE, ignoreInit = TRUE
    )
    observeEvent(input$servizio_atteso_check,
      {
        stato$atteso_esclusi <- aggiorna_esclusi(
          stato$atteso_esclusi, scelte_atteso(), input$servizio_atteso_check
        )
      },
      ignoreNULL = FALSE, ignoreInit = TRUE
    )

    # --- Dataset filtrato: prima i filtri, poi l'ultima lettura per RFID -------
    dati_filtrati <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(NULL)
      }
      filtra_letture(
        d,
        stato_db = stato$stato_db,
        transponder_esclusi = stato$transponder_esclusi,
        includi_non_censiti = stato$includi_non_censiti,
        atteso_esclusi = stato$atteso_esclusi,
        includi_senza_stima = stato$includi_senza_stima
      ) |>
        deduplica_ultimo_rfid()
    })

    output$stat_box <- renderUI({
      df <- dati_filtrati()
      req(df)
      crea_box_statistiche(calcola_statistiche(df))
    })

    list(dati_filtrati = dati_filtrati)
  })
}
