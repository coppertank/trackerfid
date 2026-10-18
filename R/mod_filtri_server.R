#' Modulo filtri della mappa principale: logica
#'
#' Lo stato dei filtri vive sul server e viene riallineato agli input: al
#' caricamento di un nuovo dataset i filtri tornano subito ai valori
#' predefiniti, senza attendere la risposta del browser.
#'
#' I servizi e i cantieri deselezionati sono memorizzati per esclusione, così
#' le scelte dell'utente sopravvivono quando l'elenco cambia con il periodo.
#' I comuni si scelgono tra quelli dei cantieri selezionati: uno, più di uno,
#' anche di cantieri diversi. Nessuna scelta vale per tutti i comuni.
#'
#' I servizi stanno in due elenchi, che valgono per i bidoni di tutti e due
#' gli stati a database. Il primo riporta il servizio del contenitore: per un
#' censito quello a database, per un non censito quello stimato. Sono gli
#' stessi che danno l'icona ai marker. I non censiti con la stima incerta, che
#' sulla mappa hanno il punto di domanda, hanno una casella a parte. Il
#' secondo riporta i giri del calendario, con il loro nome. "Tutti" e
#' "Nessuno" spuntano o tolgono in un colpo tutte le voci di un elenco. Un
#' terzo filtro riguarda la coerenza tra servizio a database e servizio
#' atteso dal giro; un quarto, tra quelli geografici, la coerenza tra il
#' comune di lettura e il comune dichiarato.
#'
#' Filtri, ultima lettura e statistiche sono interrogazioni su DuckDB, vedi
#' `ultime_letture()`: in R arriva solo una riga per bidone.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con la selezione delle letture del periodo di analisi,
#'   vedi `seleziona_periodo()` (o `NULL`).
#' @param azzera Reactive il cui cambiamento riporta i filtri ai valori
#'   predefiniti: di norma il dataset caricato.
#' @return Lista di reactive: `dati_filtrati`, l'ultima lettura di ogni RFID
#'   tra quelle che superano i filtri, e `area`, gli angoli del riquadro delle
#'   letture dei cantieri e dei comuni scelti, su cui inquadrare la mappa.
#' @noRd
mod_filtri_server <- function(id, dati, azzera = dati) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      stato_db = stati_database(),
      servizi_esclusi = character(0),
      includi_stima_incerta = TRUE,
      giri_esclusi = character(0),
      coerenza_escluse = character(0),
      cantieri_esclusi = character(0),
      comuni = character(0),
      coerenza_comune_escluse = character(0),
      reimpostazioni = 0L
    )

    output$caricato <- reactive(!is.null(dati()))
    outputOptions(output, "caricato", suspendWhenHidden = FALSE)

    # --- Valori predefiniti: nuovo dataset o pulsante "Reset Filtri" -----------
    reimposta <- function() {
      stato$stato_db <- stati_database()
      stato$servizi_esclusi <- character(0)
      stato$includi_stima_incerta <- TRUE
      stato$giri_esclusi <- character(0)
      stato$coerenza_escluse <- character(0)
      stato$cantieri_esclusi <- character(0)
      stato$comuni <- character(0)
      stato$coerenza_comune_escluse <- character(0)
      stato$reimpostazioni <- stato$reimpostazioni + 1L
      updateCheckboxGroupInput(
        session,
        "presente_filter",
        selected = stati_database()
      )
      updateCheckboxGroupInput(
        session,
        "coerenza_comune_check",
        selected = names(stati_coerenza_comune())
      )
      updateCheckboxGroupInput(
        session,
        "coerenza_check",
        selected = names(stati_coerenza())
      )
      updateCheckboxInput(session, "includi_stima_incerta", value = TRUE)
    }
    observeEvent(azzera(), reimposta(), ignoreNULL = FALSE, priority = 100)
    observeEvent(input$reset, reimposta(), priority = 100)

    # --- Valori presenti nel periodo, per le voci dei filtri -------------------
    # Il servizio: per i censiti quello del contenitore a database, per i non
    # censiti quello stimato. Il giro: quello del calendario, con il suo nome.
    scelte <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(NULL)
      }
      scelte_periodo(d)
    })
    scelte_servizio <- reactive(scelte()$servizi %||% character(0))
    scelte_giro <- reactive(scelte()$giri %||% character(0))

    aggiorna_gruppo <- function(id_input, scelte, esclusi) {
      etichette <- paste(info_servizio(scelte)$emoji, scelte)
      updateCheckboxGroupInput(
        session,
        id_input,
        choices = stats::setNames(scelte, etichette),
        selected = setdiff(scelte, esclusi)
      )
    }
    observe({
      stato$reimpostazioni
      aggiorna_gruppo(
        "servizio_check",
        scelte_servizio(),
        isolate(stato$servizi_esclusi)
      )
    })
    observe({
      stato$reimpostazioni
      aggiorna_gruppo(
        "servizio_giro_check",
        scelte_giro(),
        isolate(stato$giri_esclusi)
      )
    })

    # --- Scelte geografiche: cantieri e comuni presenti nel periodo -------------
    # Una riga per ogni comune, con il suo cantiere.
    comuni_presenti <- reactive(scelte()$comuni)
    scelte_cantieri <- reactive(scelte()$cantieri %||% character(0))

    # I filtri geografici compaiono solo se almeno una lettura ha un cantiere.
    con_geografia <- reactive(isTRUE(scelte()$con_geografia))
    output$con_geografia <- reactive(con_geografia())
    outputOptions(output, "con_geografia", suspendWhenHidden = FALSE)

    observe({
      stato$reimpostazioni
      scelte <- scelte_cantieri()
      updateCheckboxGroupInput(
        session,
        "cantiere_check",
        choices = scelte,
        selected = setdiff(scelte, isolate(stato$cantieri_esclusi))
      )
    })

    # I comuni offerti sono quelli dei cantieri selezionati, divisi per cantiere.
    scelte_comuni <- reactive({
      coppie <- comuni_presenti()
      if (is.null(coppie) || nrow(coppie) == 0) {
        return(list())
      }
      coppie$cantiere[is.na(coppie$cantiere)] <- senza_cantiere()
      coppie <- coppie[!coppie$cantiere %in% stato$cantieri_esclusi, ]
      gruppi <- split(coppie$comune_assegnato, coppie$cantiere)
      purrr::map(
        gruppi[intersect(ordina_cantieri(coppie$cantiere), names(gruppi))],
        function(comuni) as.list(sort(unique(comuni)))
      )
    })
    observe({
      stato$reimpostazioni
      scelte <- scelte_comuni()
      # I comuni di un cantiere deselezionato non restano scelti.
      scelti <- intersect(isolate(stato$comuni), unlist(scelte))
      stato$comuni <- scelti
      updateSelectizeInput(
        session,
        "comuni",
        choices = scelte,
        selected = scelti
      )
    })

    # --- Dagli input allo stato --------------------------------------------------
    observeEvent(
      input$presente_filter,
      {
        stato$stato_db <- input$presente_filter %||% character(0)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    observeEvent(
      input$includi_stima_incerta,
      {
        stato$includi_stima_incerta <- isTRUE(input$includi_stima_incerta)
      },
      ignoreInit = TRUE
    )

    # Una voce visibile è esclusa se non è spuntata; le voci fuori dal periodo
    # corrente conservano lo stato precedente.
    aggiorna_esclusi <- function(esclusi, scelte, selezionati) {
      sort(union(
        setdiff(esclusi, scelte),
        setdiff(scelte, selezionati %||% character(0))
      ))
    }
    observeEvent(
      input$servizio_giro_check,
      {
        stato$giri_esclusi <- aggiorna_esclusi(
          stato$giri_esclusi,
          scelte_giro(),
          input$servizio_giro_check
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    observeEvent(
      input$coerenza_check,
      {
        stato$coerenza_escluse <- setdiff(
          names(stati_coerenza()),
          input$coerenza_check %||% character(0)
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    observeEvent(
      input$servizio_check,
      {
        stato$servizi_esclusi <- aggiorna_esclusi(
          stato$servizi_esclusi,
          scelte_servizio(),
          input$servizio_check
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    # --- Scelte rapide: tutte le voci di un elenco di servizi, o nessuna ---------
    # Lo stato cambia subito, senza attendere che il browser rimandi le spunte.
    # "Tutti" toglie ogni esclusione. "Nessuno" esclude le voci in elenco e
    # tutti i servizi noti: così la scelta resta "nessuno" anche in un periodo
    # in cui l'elenco ha voci diverse.
    spunta_elenco <- function(campo, id_input, scelte, tutti) {
      stato[[campo]] <- if (tutti) {
        character(0)
      } else {
        sort(unique(c(stato[[campo]], scelte, servizi_config()$servizio)))
      }
      updateCheckboxGroupInput(
        session,
        id_input,
        selected = if (tutti) scelte else character(0)
      )
    }
    # L'elenco dei servizi comprende la casella della stima incerta.
    spunta_servizi <- function(tutti) {
      spunta_elenco(
        "servizi_esclusi",
        "servizio_check",
        scelte_servizio(),
        tutti
      )
      stato$includi_stima_incerta <- tutti
      updateCheckboxInput(session, "includi_stima_incerta", value = tutti)
    }
    observeEvent(input$servizio_tutti, spunta_servizi(TRUE))
    observeEvent(input$servizio_nessuno, spunta_servizi(FALSE))
    observeEvent(input$giro_tutti, {
      spunta_elenco("giri_esclusi", "servizio_giro_check", scelte_giro(), TRUE)
    })
    observeEvent(input$giro_nessuno, {
      spunta_elenco("giri_esclusi", "servizio_giro_check", scelte_giro(), FALSE)
    })

    observeEvent(
      input$cantiere_check,
      {
        stato$cantieri_esclusi <- aggiorna_esclusi(
          stato$cantieri_esclusi,
          scelte_cantieri(),
          input$cantiere_check
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    observeEvent(
      input$coerenza_comune_check,
      {
        stato$coerenza_comune_escluse <- setdiff(
          names(stati_coerenza_comune()),
          input$coerenza_comune_check %||% character(0)
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    # Senza comuni scelti l'input è `NULL`: valgono tutti.
    observeEvent(
      input$comuni,
      {
        stato$comuni <- input$comuni %||% character(0)
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )

    # --- Dataset filtrato: prima i filtri, poi l'ultima lettura per RFID -------
    filtri <- reactive(list(
      stato_db = stato$stato_db,
      servizi_esclusi = stato$servizi_esclusi,
      includi_stima_incerta = stato$includi_stima_incerta,
      giri_esclusi = stato$giri_esclusi,
      coerenza_escluse = stato$coerenza_escluse,
      cantieri_esclusi = stato$cantieri_esclusi,
      comuni = stato$comuni,
      coerenza_comune_escluse = stato$coerenza_comune_escluse
    ))

    dati_filtrati <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(NULL)
      }
      # L'ultima lettura di ogni RFID, con il numero di tutte le letture che
      # superano i filtri: lo riporta l'intestazione della mappa.
      structure(
        do.call(ultime_letture, c(list(d), filtri())),
        letture = do.call(conta_letture, c(list(d), filtri()))
      )
    })

    output$stat_box <- renderUI({
      df <- dati_filtrati()
      req(df)
      crea_box_statistiche(calcola_statistiche(
        df,
        do.call(conteggi_cantieri, c(list(dati()), filtri()))
      ))
    })

    # Area da inquadrare: cambia con cantieri e comuni, non con gli altri
    # filtri, così la mappa non si sposta a ogni spunta sui servizi.
    area <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(NULL)
      }
      area_filtri(d, stato$cantieri_esclusi, stato$comuni)
    })

    list(dati_filtrati = dati_filtrati, area = area)
  })
}
