test_that("il caricamento restituisce il dataset validato", {
  atteso <- dati_esempio()
  shiny::testServer(mod_caricamento_server, {
    expect_null(session$getReturned()$letture())
    expect_null(session$getReturned()$storico())

    session$setInputs(
      file_upload = data.frame(
        name = "letture.csv",
        datapath = percorso_dataset_esempio(),
        stringsAsFactors = FALSE
      )
    )
    dati <- session$getReturned()$letture()
    expect_equal(nrow(dati), nrow(atteso))
    # le letture storiche sono un file a parte
    expect_null(session$getReturned()$storico())
    html <- as.character(output$esito$html)
    expect_match(
      html,
      sprintf("<b>%s</b>\\s*righe caricate", formatta_numero(nrow(atteso)))
    )
    expect_match(html, "<b>239</b>\\s*RFID univoci")
    expect_match(html, "Periodo:")
    expect_match(html, "01/10/2024")
    expect_match(html, "31/12/2025")
  })
})

test_that("un file non valido azzera il dataset e mostra l'errore", {
  errato <- scrivi_csv(c("giorno_lettura,RFID", "2025-09-15 08:30:00,R1"))
  shiny::testServer(mod_caricamento_server, {
    session$setInputs(carica_esempio = 1)
    expect_equal(nrow(session$getReturned()$letture()), nrow(dati_esempio()))

    session$setInputs(
      file_upload = data.frame(
        name = "errato.csv",
        datapath = errato,
        stringsAsFactors = FALSE
      )
    )
    expect_null(session$getReturned()$letture())
    html <- as.character(output$esito$html)
    expect_match(html, "Caricamento non riuscito")
    expect_match(html, "Colonne mancanti")
  })
})

test_that("le letture storiche si caricano a parte e l'esempio le comprende", {
  errato <- scrivi_csv(c("RFID,giorno_lettura", "R1,ieri"))
  shiny::testServer(mod_caricamento_server, {
    caricati <- session$getReturned()

    session$setInputs(carica_esempio = 1)
    expect_equal(nrow(caricati$storico()), nrow(storico_esempio()))
    html <- as.character(output$esito_storico$html)
    expect_match(html, "letture storiche")
    expect_match(html, "01/01/2020")
    expect_match(html, "30/09/2024")

    # un file di letture caricato a mano toglie le letture storiche di esempio
    session$setInputs(
      file_upload = data.frame(
        name = "letture.csv",
        datapath = percorso_dataset_esempio(),
        stringsAsFactors = FALSE
      )
    )
    expect_null(caricati$storico())
    expect_equal(nrow(caricati$letture()), nrow(dati_esempio()))

    # quelle caricate a mano restano anche se cambiano le letture
    session$setInputs(
      file_storico = data.frame(
        name = "storiche.csv",
        datapath = percorso_storico_esempio(),
        stringsAsFactors = FALSE
      )
    )
    expect_equal(nrow(caricati$storico()), nrow(storico_esempio()))
    session$setInputs(
      file_upload = data.frame(
        name = "letture.csv",
        datapath = percorso_dataset_esempio(),
        stringsAsFactors = FALSE
      )
    )
    expect_equal(nrow(caricati$storico()), nrow(storico_esempio()))

    # un file storico non valido non tocca le letture con le antenne
    session$setInputs(
      file_storico = data.frame(
        name = "errato.csv",
        datapath = errato,
        stringsAsFactors = FALSE
      )
    )
    expect_null(caricati$storico())
    expect_equal(nrow(caricati$letture()), nrow(dati_esempio()))
    expect_match(
      as.character(output$esito_storico$html),
      "Caricamento non riuscito"
    )
  })
})

test_that("il periodo predefinito è l'anno più recente del dataset", {
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(mod_periodo_server, args = list(dati = dati), {
    periodo <- session$getReturned()
    session$flushReact()
    expect_identical(periodo$periodo(), as.Date(c("2025-01-01", "2025-12-31")))
    expect_identical(periodo$etichetta(), "Anno 2025")
    letture <- periodo$dati()
    expect_true(all(lubridate::year(letture$giorno_lettura) == 2025))
    # le letture del periodo portano il servizio da usare per l'icona
    expect_true("servizio_icona" %in% names(letture))
    expect_match(as.character(output$riepilogo$html), "01/01/2025")

    session$setInputs(anno_filtro = "2024")
    expect_identical(periodo$periodo(), as.Date(c("2024-01-01", "2024-12-31")))
    expect_identical(periodo$etichetta(), "Anno 2024")
    expect_true(all(lubridate::year(periodo$dati()$giorno_lettura) == 2024))
    expect_lt(nrow(periodo$dati()), nrow(letture))
  })
})

test_that("il periodo personalizzato sostituisce l'anno e viene validato", {
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(mod_periodo_server, args = list(dati = dati), {
    periodo <- session$getReturned()
    session$flushReact()

    # le date personalizzate contano solo con l'opzione attiva
    session$setInputs(intervallo = as.Date(c("2025-03-15", "2025-09-30")))
    expect_identical(periodo$etichetta(), "Anno 2025")
    session$setInputs(personalizzato = TRUE)
    expect_identical(periodo$periodo(), as.Date(c("2025-03-15", "2025-09-30")))
    expect_identical(periodo$etichetta(), "15/03/2025 - 30/09/2025")
    giorni <- as.Date(periodo$dati()$giorno_lettura)
    expect_true(all(
      giorni >= as.Date("2025-03-15") & giorni <= as.Date("2025-09-30")
    ))

    # intervallo incompleto o rovesciato: nessun periodo, con messaggio
    session$setInputs(intervallo = as.Date(c("2025-09-30", "2025-03-15")))
    expect_null(periodo$periodo())
    expect_null(periodo$dati())
    expect_match(as.character(output$riepilogo$html), "Intervallo non valido")
    session$setInputs(intervallo = as.Date(c("2025-03-15", NA)))
    expect_null(periodo$periodo())

    # periodo senza letture: dataset vuoto, non errore
    session$setInputs(intervallo = as.Date(c("2030-01-01", "2030-12-31")))
    expect_equal(nrow(periodo$dati()), 0)
    expect_match(
      as.character(output$riepilogo$html),
      "Nessuna lettura nel periodo"
    )

    session$setInputs(personalizzato = FALSE)
    expect_identical(periodo$etichetta(), "Anno 2025")
  })
})

test_that("senza dataset non c'è periodo e un nuovo dataset lo reimposta", {
  dati <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_periodo_server, args = list(dati = dati), {
    periodo <- session$getReturned()
    session$flushReact()
    expect_null(periodo$periodo())
    expect_null(periodo$dati())
    expect_null(periodo$etichetta())

    dati(dati_esempio())
    session$flushReact()
    session$setInputs(
      personalizzato = TRUE,
      intervallo = as.Date(c("2025-05-01", "2025-05-31"))
    )
    expect_identical(periodo$etichetta(), "01/05/2025 - 31/05/2025")

    solo_2024 <- dati_esempio()[
      lubridate::year(dati_esempio()$giorno_lettura) == 2024,
    ]
    dati(solo_2024)
    session$flushReact()
    expect_identical(periodo$etichetta(), "Anno 2024")
    expect_equal(nrow(periodo$dati()), nrow(solo_2024))
  })
})

test_that("i filtri partono senza esclusioni e reagiscono agli input", {
  ultimi <- deduplica_ultimo_rfid(dati_esempio())
  n_presenti <- sum(ultimi$presente_a_database == "Presente")
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    expect_equal(nrow(filtrati()), nrow(ultimi))

    session$setInputs(presente_filter = "Presente")
    expect_equal(nrow(filtrati()), n_presenti)
    expect_true(all(filtrati()$presente_a_database == "Presente"))
    expect_match(
      as.character(output$stat_box$html),
      sprintf("<b>%d</b>", n_presenti),
      fixed = TRUE
    )

    session$setInputs(presente_filter = c("Presente", "Non Presente"))
    session$setInputs(servizio_transponder_check = c("SECCO", "CARTA"))
    expect_setequal(
      unique(filtrati()$servizio_transponder),
      c("SECCO", "CARTA", NA)
    )

    session$setInputs(includi_non_censiti = FALSE)
    expect_setequal(
      unique(filtrati()$servizio_transponder),
      c("SECCO", "CARTA")
    )

    session$setInputs(reset = 1)
    expect_equal(nrow(filtrati()), nrow(ultimi))
  })
})

test_that("i filtri si azzerano con un nuovo dataset ma non al cambio di periodo", {
  origine <- shiny::reactiveVal(1)
  dati <- shiny::reactiveVal(NULL)
  shiny::testServer(
    mod_filtri_server,
    args = list(dati = dati, azzera = origine),
    {
      filtrati <- function() session$getReturned()$dati_filtrati()
      session$flushReact()
      expect_null(filtrati())

      dati(filtra_periodo(dati_esempio(), periodo_anno(2025)))
      session$flushReact()
      session$setInputs(presente_filter = "Non Presente")
      expect_true(all(filtrati()$presente_a_database == "Non Presente"))

      # cambio di periodo: le scelte dell'utente restano
      dati(filtra_periodo(dati_esempio(), periodo_anno(2024)))
      session$flushReact()
      expect_true(all(filtrati()$presente_a_database == "Non Presente"))
      expect_true(all(lubridate::year(filtrati()$giorno_lettura) == 2024))

      # nuovo dataset: filtri ai valori predefiniti
      origine(2)
      session$flushReact()
      expect_true("Presente" %in% filtrati()$presente_a_database)
    }
  )
})

test_that("la ricerca RFID parte dal pulsante e si azzera con Pulisci", {
  n_letture <- sum(dati_esempio()$RFID == "RFD20250901001")
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(
    mod_ricerca_server,
    args = list(dati = dati, tipo = "rfid"),
    {
      ricerca <- session$getReturned()
      expect_null(ricerca$risultati())

      session$setInputs(testo = "RFD20250901001, inesistente")
      expect_null(ricerca$risultati())

      session$setInputs(cerca = 1)
      expect_equal(nrow(ricerca$risultati()), n_letture)
      expect_identical(ricerca$codici(), c("RFD20250901001", "inesistente"))
      html <- as.character(output$esito$html)
      expect_match(html, "1 RFID su 2 trovati")
      expect_match(html, "Non trovati: \\s*inesistente")

      session$setInputs(pulisci = 1)
      expect_null(ricerca$risultati())
      expect_null(ricerca$codici())
    }
  )
})

test_that("la ricerca utenza mostra solo i bidoni dell'utenza attuale", {
  dati <- shiny::reactiveVal(dati_esempio())
  shiny::testServer(
    mod_ricerca_server,
    args = list(dati = dati, tipo = "utenza"),
    {
      ricerca <- session$getReturned()

      session$setInputs(testo = "UTZ001", cerca = 1)
      expect_false("RFD20250901050" %in% ricerca$risultati()$RFID)
      expect_equal(anyDuplicated(ricerca$risultati()$RFID), 0)

      session$setInputs(testo = "UTZ025\nUTZ999", cerca = 2)
      expect_true("RFD20250901050" %in% ricerca$risultati()$RFID)
      expect_true(all(ricerca$risultati()$id_utenza == "UTZ025"))
      expect_match(
        as.character(output$esito$html),
        "Nessun bidone attualmente associato a: \\s*UTZ999"
      )
    }
  )
})

test_that("il click su un marker restituisce l'RFID della lettura", {
  trovate <- cerca_per_rfid(
    dati_esempio(),
    c("RFD20250901001", "RFD20250905100")
  )
  dati <- shiny::reactiveVal(trovate)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "rfid"),
    {
      selezione <- session$getReturned()
      expect_null(selezione())

      # il browser comunica lo zoom quando la mappa è pronta
      session$setInputs(mappa_zoom = 11)
      expect_match(
        output$riepilogo,
        sprintf("%d letture di 2 RFID", nrow(trovate))
      )

      ultima <- nrow(trovate)
      session$setInputs(
        mappa_marker_click = list(
          id = as.character(ultima),
          lat = 42,
          lng = 12.5
        )
      )
      expect_identical(selezione()$rfid, trovate$RFID[ultima])

      session$setInputs(
        mappa_marker_click = list(id = "9999", lat = 42, lng = 12.5)
      )
      expect_identical(selezione()$rfid, trovate$RFID[ultima])
    }
  )
})

test_that("l'analisi dei cluster parte dal pulsante e segue il periodo", {
  dati <- shiny::reactiveVal(dati_esempio())
  periodo <- shiny::reactiveVal(as.Date(c("2025-01-01", "2025-12-31")))
  shiny::testServer(
    mod_cluster_analysis_server,
    args = list(dati = dati, periodo = periodo, etichetta = function() {
      etichetta_periodo(periodo())
    }),
    {
      selezione <- session$getReturned()
      session$flushReact()
      expect_match(output$invito, "Genera Analisi")
      expect_match(output$invito, "Anno 2025")
      expect_identical(output$riepilogo, "")

      session$setInputs(eps_m = 100, min_pts = 1, genera = 1)
      attesa <- analisi_esempio()
      expect_match(
        output$riepilogo,
        sprintf(
          "%d cluster di %d RFID",
          nrow(attesa),
          dplyr::n_distinct(attesa$RFID)
        )
      )
      expect_match(as.character(output$esito$html), "365 giorni osservati")
      expect_equal(nrow(righe_filtrate()), nrow(attesa))

      # i filtri della tabella valgono per grafici e mappa
      session$setInputs(tabella_rows_all = 1:10)
      expect_equal(nrow(righe_filtrate()), 10)

      # selezione da tabella e da mappa
      session$setInputs(tabella_rows_selected = 3)
      expect_identical(selezione()$rfid, attesa$RFID[3])
      session$setInputs(mappa_marker_click = list(id = "7"))
      expect_identical(selezione()$rfid, attesa$RFID[7])

      # raggio più ampio: meno cluster
      session$setInputs(eps_m = 2000, genera = 2)
      expect_lt(nrow(risultato()), nrow(attesa))

      # parametri non validi: l'analisi precedente resta
      precedente <- risultato()
      session$setInputs(eps_m = -5, genera = 3)
      expect_identical(risultato(), precedente)

      # cambio di periodo: il risultato superato viene scartato
      periodo(as.Date(c("2024-01-01", "2024-12-31")))
      session$flushReact()
      expect_null(risultato())
      expect_match(output$invito, "Anno 2024")

      # periodo senza letture
      periodo(as.Date(c("2030-01-01", "2030-12-31")))
      session$setInputs(eps_m = 100, genera = 4)
      expect_match(output$invito, "Nessuna lettura nel periodo")
    }
  )
})

test_that("l'esportazione dalla scheda usa il nome con le date del periodo", {
  dati <- shiny::reactiveVal(dati_esempio())
  periodo <- shiny::reactiveVal(as.Date(c("2025-03-15", "2025-09-30")))
  shiny::testServer(
    mod_cluster_analysis_server,
    args = list(dati = dati, periodo = periodo),
    {
      session$setInputs(eps_m = 100, min_pts = 1, genera = 1)
      file <- output$esporta
      expect_identical(
        basename(file),
        "cluster_analysis_2025-03-15_2025-09-30.csv"
      )
      esportato <- utils::read.csv(file, stringsAsFactors = FALSE)
      expect_identical(names(esportato), colonne_analisi_cluster())
      expect_equal(nrow(esportato), nrow(risultato()))
    }
  )
})

test_that("l'interfaccia contiene le schede e i controlli richiesti", {
  html <- as.character(app_ui(NULL))
  for (testo in c(
    "Mappa Principale",
    "Ricerca RFID",
    "Ricerca Utenza",
    "Analisi Cluster Spaziale",
    "Caricamento Dati",
    "Seleziona Anno di Analisi",
    "Periodo personalizzato",
    "Filtro Stato Database",
    "Filtro Servizio Transponder",
    "Filtro Servizio Atteso",
    "Includi Non Censiti",
    "Reset Filtri",
    "Pulisci Ricerca",
    "Cerca RFID",
    "Cerca Utenza",
    "Visualizzazione: Clustering",
    "Visualizzazione: Ricerca RFID",
    "Genera Analisi",
    "Esporta CSV",
    "Parametri DBSCAN"
  )) {
    expect_match(html, testo, fixed = TRUE)
  }
  # lo slider del periodo è stato sostituito dalla selezione dell'anno
  expect_no_match(html, "Filtro Periodo", fixed = TRUE)
  expect_no_match(html, "js-range-slider", fixed = TRUE)
})

test_that("la pagina ha come favicon il simbolo aziendale", {
  pagina <- htmltools::renderTags(app_ui(NULL))
  # golem mette il favicon nella sezione head
  expect_match(pagina$head, '<link rel="shortcut icon" href="www/favicon.png"/>', fixed = TRUE)
  expect_no_match(pagina$head, "favicon.ico", fixed = TRUE)
  expect_true(file.exists(app_sys("app/www", "favicon.png")))

  # il favicon è quadrato: larghezza e altezza stanno nell'intestazione del PNG
  intestazione <- as.integer(readBin(app_sys("app/www", "favicon.png"), "raw", 24))
  lato <- function(da) sum(intestazione[da:(da + 3)] * 256^(3:0))
  expect_equal(lato(17), 192)
  expect_equal(lato(21), 192)

  # nell'intestazione non c'è nessun logo
  expect_no_match(pagina$html, "<img", fixed = TRUE)
})
