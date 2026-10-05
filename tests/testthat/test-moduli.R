test_that("il caricamento restituisce il dataset validato", {
  shiny::testServer(mod_caricamento_server, {
    expect_null(session$getReturned()())

    session$setInputs(file_upload = data.frame(
      name = "letture.csv", datapath = percorso_dataset_esempio(), stringsAsFactors = FALSE
    ))
    dati <- session$getReturned()()
    expect_equal(nrow(dati), 728)
    html <- as.character(output$esito$html)
    expect_match(html, "<b>728</b>\\s*righe caricate")
    expect_match(html, "<b>236</b>\\s*RFID univoci")
    expect_match(html, "Periodo:")
    expect_match(html, "01/09/2025")
    expect_match(html, "30/09/2025")
  })
})

test_that("un file non valido azzera il dataset e mostra l'errore", {
  errato <- scrivi_csv(c("giorno_lettura,RFID", "2025-09-15 08:30:00,R1"))
  shiny::testServer(mod_caricamento_server, {
    session$setInputs(carica_esempio = 1)
    expect_equal(nrow(session$getReturned()()), 728)

    session$setInputs(file_upload = data.frame(
      name = "errato.csv", datapath = errato, stringsAsFactors = FALSE
    ))
    expect_null(session$getReturned()())
    html <- as.character(output$esito$html)
    expect_match(html, "Caricamento non riuscito")
    expect_match(html, "Colonne mancanti")
  })
})

test_that("i filtri partono senza esclusioni e reagiscono agli input", {
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    expect_equal(nrow(filtrati()), 236)

    session$setInputs(presente_filter = "Presente")
    expect_equal(nrow(filtrati()), 200)
    expect_true(all(filtrati()$presente_a_database == "Presente"))
    expect_match(as.character(output$stat_box$html), "<b>200</b>", fixed = TRUE)

    session$setInputs(presente_filter = c("Presente", "Non Presente"))
    session$setInputs(servizio_transponder_check = c("SECCO", "CARTA"))
    expect_setequal(unique(filtrati()$servizio_transponder), c("SECCO", "CARTA", NA))

    session$setInputs(includi_non_censiti = FALSE)
    expect_setequal(unique(filtrati()$servizio_transponder), c("SECCO", "CARTA"))

    session$setInputs(reset = 1)
    expect_equal(nrow(filtrati()), 236)
  })
})

test_that("il filtro per periodo restringe letture e scelte", {
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()

    session$setInputs(date_range = as.Date(c("2025-09-01", "2025-09-03")))
    expect_true(all(as.Date(filtrati()$giorno_lettura) <= as.Date("2025-09-03")))
    expect_lt(nrow(filtrati()), 236)
    # nel periodo ristretto il bidone speciale ha ancora il servizio iniziale
    riga <- filtrati()[filtrati()$RFID == "RFD20250901001", ]
    expect_identical(riga$servizio_transponder, "CARTA")
    expect_identical(format(riga$giorno_lettura, "%d/%m"), "01/09")
  })
})

test_that("senza dataset i filtri non restituiscono nulla e si riallineano al nuovo", {
  dati <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    expect_null(filtrati())

    dati(dati_esempio())
    session$flushReact()
    expect_equal(nrow(filtrati()), 236)

    # un nuovo dataset azzera le esclusioni impostate sul precedente
    session$setInputs(presente_filter = "Non Presente")
    expect_equal(nrow(filtrati()), 36)
    dati(dati_esempio()[1:100, ])
    session$flushReact()
    expect_equal(nrow(filtrati()), dplyr::n_distinct(dati_esempio()$RFID[1:100]))
  })
})

test_that("la ricerca RFID parte dal pulsante e si azzera con Pulisci", {
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(mod_ricerca_server, args = list(dati = dati, tipo = "rfid"), {
    ricerca <- session$getReturned()
    expect_null(ricerca$risultati())

    session$setInputs(testo = "RFD20250901001, inesistente")
    expect_null(ricerca$risultati())

    session$setInputs(cerca = 1)
    expect_equal(nrow(ricerca$risultati()), 3)
    expect_identical(ricerca$codici(), c("RFD20250901001", "inesistente"))
    html <- as.character(output$esito$html)
    expect_match(html, "1 RFID su 2 trovati")
    expect_match(html, "Non trovati: \\s*inesistente")

    session$setInputs(pulisci = 1)
    expect_null(ricerca$risultati())
    expect_null(ricerca$codici())
  })
})

test_that("la ricerca utenza mostra solo i bidoni dell'utenza attuale", {
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(mod_ricerca_server, args = list(dati = dati, tipo = "utenza"), {
    ricerca <- session$getReturned()

    session$setInputs(testo = "UTZ001", cerca = 1)
    expect_false("RFD20250901050" %in% ricerca$risultati()$RFID)
    expect_equal(anyDuplicated(ricerca$risultati()$RFID), 0)

    session$setInputs(testo = "UTZ025\nUTZ999", cerca = 2)
    expect_true("RFD20250901050" %in% ricerca$risultati()$RFID)
    expect_true(all(ricerca$risultati()$id_utenza == "UTZ025"))
    expect_match(as.character(output$esito$html), "Nessun bidone attualmente associato a: \\s*UTZ999")
  })
})

test_that("il click su un marker restituisce l'RFID della lettura", {
  trovate <- cerca_per_rfid(dati_esempio(), c("RFD20250901001", "RFD20250905100"))
  dati <- shiny::reactiveVal(trovate)
  shiny::testServer(mod_mappa_server, args = list(dati = dati, modalita = "rfid"), {
    selezione <- session$getReturned()
    expect_null(selezione())

    # il browser comunica lo zoom quando la mappa è pronta
    session$setInputs(mappa_zoom = 11)
    expect_match(output$riepilogo, "5 letture di 2 RFID")

    session$setInputs(mappa_marker_click = list(id = "4", lat = 42, lng = 12.5))
    expect_identical(selezione()$rfid, "RFD20250905100")

    session$setInputs(mappa_marker_click = list(id = "99", lat = 42, lng = 12.5))
    expect_identical(selezione()$rfid, "RFD20250905100")
  })
})

test_that("l'interfaccia contiene le tre schede e i controlli richiesti", {
  html <- as.character(app_ui(NULL))
  for (testo in c(
    "DASHBOARD RFID BIDONI RIFIUTI", "Mappa Principale", "Ricerca RFID", "Ricerca Utenza",
    "Caricamento Dati", "Filtro Periodo", "Filtro Stato Database",
    "Filtro Servizio Transponder", "Filtro Servizio Atteso", "Includi Non Censiti",
    "Reset Filtri", "Pulisci Ricerca", "Cerca RFID", "Cerca Utenza",
    "Visualizzazione: Clustering", "Visualizzazione: Ricerca RFID"
  )) {
    expect_match(html, testo, fixed = TRUE)
  }
})
