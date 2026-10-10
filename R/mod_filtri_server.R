#' Modulo filtri laterali: logica
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
#' I servizi stanno in due elenchi, uno per stato a database. Quello dei
#' bidoni "Presente" riporta i servizi transponder. Quello dei bidoni "Non
#' Presente" riporta i servizi stimati dai giri, gli stessi che danno l'icona
#' ai marker: i non censiti con la stima incerta, che sulla mappa hanno il
#' punto di domanda, hanno una casella a parte. "Tutti" e "Nessuno" spuntano
#' o tolgono in un colpo tutte le voci di un elenco.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con le letture del periodo di analisi (o `NULL`).
#' @param azzera Reactive il cui cambiamento riporta i filtri ai valori
#'   predefiniti: di norma il dataset caricato.
#' @return Lista di reactive: `dati_filtrati`, l'ultima lettura di ogni RFID
#'   tra quelle che superano i filtri, e `area`, le coordinate delle letture
#'   dei cantieri e dei comuni scelti, su cui inquadrare la mappa.
#' @noRd
mod_filtri_server <- function(id, dati, azzera = dati) {
  moduleServer(id, function(input, output, session) {
    stato <- reactiveValues(
      stato_db = stati_database(),
      transponder_esclusi = character(0),
      atteso_esclusi = character(0),
      includi_stima_incerta = TRUE,
      cantieri_esclusi = character(0),
      comuni = character(0),
      reimpostazioni = 0L
    )

    output$caricato <- reactive(!is.null(dati()))
    outputOptions(output, "caricato", suspendWhenHidden = FALSE)

    # --- Valori predefiniti: nuovo dataset o pulsante "Reset Filtri" -----------
    reimposta <- function() {
      stato$stato_db <- stati_database()
      stato$transponder_esclusi <- character(0)
      stato$atteso_esclusi <- character(0)
      stato$includi_stima_incerta <- TRUE
      stato$cantieri_esclusi <- character(0)
      stato$comuni <- character(0)
      stato$reimpostazioni <- stato$reimpostazioni + 1L
      updateCheckboxGroupInput(
        session,
        "presente_filter",
        selected = stati_database()
      )
      updateCheckboxInput(session, "includi_stima_incerta", value = TRUE)
    }
    observeEvent(azzera(), reimposta(), ignoreNULL = FALSE, priority = 100)
    observeEvent(input$reset, reimposta(), priority = 100)

    # --- Scelte dinamiche dei servizi: valori presenti nel periodo -------------
    # Per i censiti i servizi del database. Per i non censiti i servizi
    # stimati dai giri: quelli che danno l'icona ai marker.
    scelte_transponder <- reactive({
      d <- dati()
      ordina_servizi(d$servizio_transponder[
        d$presente_a_database == "Presente"
      ])
    })
    scelte_atteso <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(character(0))
      }
      ordina_servizi(servizio_mostrato(d)[
        d$presente_a_database == "Non Presente"
      ])
    })

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
        "servizio_transponder_check",
        scelte_transponder(),
        isolate(stato$transponder_esclusi)
      )
    })
    observe({
      stato$reimpostazioni
      aggiorna_gruppo(
        "servizio_atteso_check",
        scelte_atteso(),
        isolate(stato$atteso_esclusi)
      )
    })

    # --- Scelte geografiche: cantieri e comuni presenti nel periodo -------------
    # Una riga per ogni comune, con il suo cantiere.
    comuni_presenti <- reactive({
      d <- dati()
      if (is.null(d) || !"comune_assegnato" %in% names(d)) {
        return(NULL)
      }
      coppie <- dplyr::distinct(d[, c("cantiere", "comune_assegnato")])
      coppie[!is.na(coppie$comune_assegnato), , drop = FALSE]
    })
    scelte_cantieri <- reactive(ordina_cantieri(dati()[["cantiere"]]))

    # I filtri geografici compaiono solo se almeno una lettura ha un cantiere.
    con_geografia <- reactive(any(!is.na(dati()[["cantiere"]])))
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
      input$servizio_transponder_check,
      {
        stato$transponder_esclusi <- aggiorna_esclusi(
          stato$transponder_esclusi,
          scelte_transponder(),
          input$servizio_transponder_check
        )
      },
      ignoreNULL = FALSE,
      ignoreInit = TRUE
    )
    observeEvent(
      input$servizio_atteso_check,
      {
        stato$atteso_esclusi <- aggiorna_esclusi(
          stato$atteso_esclusi,
          scelte_atteso(),
          input$servizio_atteso_check
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
    # L'elenco dei non censiti comprende la casella della stima incerta.
    spunta_non_censiti <- function(tutti) {
      spunta_elenco(
        "atteso_esclusi",
        "servizio_atteso_check",
        scelte_atteso(),
        tutti
      )
      stato$includi_stima_incerta <- tutti
      updateCheckboxInput(session, "includi_stima_incerta", value = tutti)
    }
    observeEvent(input$transponder_tutti, {
      spunta_elenco(
        "transponder_esclusi",
        "servizio_transponder_check",
        scelte_transponder(),
        TRUE
      )
    })
    observeEvent(input$transponder_nessuno, {
      spunta_elenco(
        "transponder_esclusi",
        "servizio_transponder_check",
        scelte_transponder(),
        FALSE
      )
    })
    observeEvent(input$atteso_tutti, spunta_non_censiti(TRUE))
    observeEvent(input$atteso_nessuno, spunta_non_censiti(FALSE))

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
    # La maschera dice quali letture superano i filtri: la deduplica lavora
    # su quella, senza una copia filtrata di tutte le letture.
    maschera <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(NULL)
      }
      maschera_filtri(
        d,
        stato_db = stato$stato_db,
        transponder_esclusi = stato$transponder_esclusi,
        atteso_esclusi = stato$atteso_esclusi,
        includi_stima_incerta = stato$includi_stima_incerta,
        cantieri_esclusi = stato$cantieri_esclusi,
        comuni = stato$comuni
      )
    })

    dati_filtrati <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(NULL)
      }
      deduplica_ultimo_rfid(d, maschera())
    })

    output$stat_box <- renderUI({
      df <- dati_filtrati()
      req(df)
      crea_box_statistiche(calcola_statistiche(
        df,
        dati()[["cantiere"]][maschera()]
      ))
    })

    # Area da inquadrare: cambia con cantieri e comuni, non con gli altri
    # filtri, così la mappa non si sposta a ogni spunta sui servizi.
    area <- reactive({
      d <- dati()
      if (is.null(d)) {
        return(NULL)
      }
      scelte <- maschera_filtri(
        d,
        stato_db = stati_database(),
        cantieri_esclusi = stato$cantieri_esclusi,
        comuni = stato$comuni
      )
      d[scelte, c("latitudine", "longitudine"), drop = FALSE]
    })

    list(dati_filtrati = dati_filtrati, area = area)
  })
}
