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
    expect_match(html, "<b>251</b>\\s*RFID univoci")
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
  esempio <- aggiungi_servizio_icona(dati_esempio())
  ultimi <- deduplica_ultimo_rfid(esempio)
  n_presenti <- sum(ultimi$presente_a_database == "Presente")
  dati <- shiny::reactiveVal(esempio)
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

    # il servizio atteso vale per i censiti e per i non censiti: per i primi
    # quello della lettura, per i secondi quello stimato
    session$setInputs(presente_filter = c("Presente", "Non Presente"))
    session$setInputs(servizio_atteso_check = c("SECCO", "CARTA"))
    expect_setequal(stato$atteso_esclusi, setdiff(scelte_atteso(), c("SECCO", "CARTA")))
    censiti <- filtrati()$presente_a_database == "Presente"
    expect_setequal(
      unique(filtrati()$servizio_atteso[censiti]),
      c("SECCO", "CARTA")
    )
    # tra i non censiti restano quelli stimati secco o carta e gli incerti
    expect_setequal(
      unique(filtrati()$servizio_icona[!censiti]),
      c("SECCO", "CARTA", NA)
    )
    # lo stato a database si sceglie a parte
    session$setInputs(presente_filter = "Presente")
    expect_true(all(filtrati()$presente_a_database == "Presente"))
    expect_setequal(unique(filtrati()$servizio_atteso), c("SECCO", "CARTA"))

    session$setInputs(reset = 1)
    expect_equal(nrow(filtrati()), nrow(ultimi))
    expect_length(stato$atteso_esclusi, 0)
  })
})

test_that("i filtri sul giro e sulla coerenza dei servizi restringono la mappa", {
  esempio <- aggiungi_servizio_icona(dati_esempio())
  n_rfid <- dplyr::n_distinct(esempio$RFID)
  dati <- shiny::reactiveVal(esempio)
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    # i giri hanno il nome del calendario
    expect_identical(scelte_giro(), ordina_servizi(esempio$servizio_giro))
    expect_length(scelte_giro(), 12)
    expect_true(all(
      c("SECCO PAP", "CARTA CONT.STRADALI", "CARTA/CARTONE PAP") %in%
        scelte_giro()
    ))
    expect_false("SECCO" %in% scelte_giro())

    # il solo secco porta a porta: censiti e non censiti
    session$setInputs(servizio_giro_check = "SECCO PAP")
    expect_setequal(stato$giri_esclusi, setdiff(scelte_giro(), "SECCO PAP"))
    expect_equal(nrow(filtrati()), 115)
    expect_true(all(filtrati()$servizio_giro == "SECCO PAP"))
    expect_setequal(
      unique(filtrati()$presente_a_database),
      c("Presente", "Non Presente")
    )
    expect_match(
      as.character(output$stat_box$html),
      "SECCO PAP:</span>\\s*<b>115</b>"
    )
    # con lo stato a database: i soli censiti letti da quel giro
    session$setInputs(presente_filter = "Presente")
    expect_true(all(filtrati()$presente_a_database == "Presente"))
    expect_lt(nrow(filtrati()), 115)
    session$setInputs(presente_filter = c("Presente", "Non Presente"))

    # le scelte rapide
    session$setInputs(giro_nessuno = 1)
    expect_true(all(scelte_giro() %in% stato$giri_esclusi))
    expect_equal(nrow(filtrati()), 0)
    session$setInputs(giro_tutti = 1)
    expect_length(stato$giri_esclusi, 0)
    expect_equal(nrow(filtrati()), n_rfid)

    # le sole letture non coerenti: quattro censiti letti in un giro di un
    # altro rifiuto. Per due è anche l'ultima lettura
    session$setInputs(coerenza_check = "non_coerente")
    expect_identical(stato$coerenza_escluse, "coerente")
    expect_setequal(
      filtrati()$RFID,
      c("RFD20241007190", "RFD20241011121", "RFD20250901001", "RFD20250905100")
    )
    expect_true(all(filtrati()$coerenza == "Non coerente"))
    expect_match(
      as.character(output$stat_box$html),
      "Non coerenti:</span>\\s*<b>4</b>"
    )
    # le sole coerenti: restano tutti gli RFID, ognuno con la sua ultima
    # lettura coerente
    session$setInputs(coerenza_check = "coerente")
    expect_identical(stato$coerenza_escluse, "non_coerente")
    expect_equal(nrow(filtrati()), n_rfid)
    expect_false(any(filtrati()$coerenza %in% "Non coerente"))
    # nessuna voce: nessuna lettura
    session$setInputs(coerenza_check = character(0))
    expect_equal(nrow(filtrati()), 0)

    # il reset riporta tutto ai valori predefiniti
    session$setInputs(servizio_giro_check = "VETRO PAP")
    expect_length(stato$giri_esclusi, 11)
    session$setInputs(reset = 1)
    expect_length(stato$giri_esclusi, 0)
    expect_length(stato$coerenza_escluse, 0)
    expect_equal(nrow(filtrati()), n_rfid)
    expect_match(
      as.character(output$stat_box$html),
      "Non coerenti:</span>\\s*<b>2</b>"
    )
  })
})

test_that("le scelte rapide spuntano o tolgono tutti i servizi attesi", {
  # Come nell'app: le letture del periodo, con il servizio dell'icona.
  esempio <- aggiungi_servizio_icona(
    filtra_periodo(dati_esempio(), periodo_anno(2025))
  )
  ultimi <- deduplica_ultimo_rfid(esempio)
  # Il periodo cambia senza che cambi il dataset: i filtri non si azzerano.
  origine <- shiny::reactiveVal(1)
  dati <- shiny::reactiveVal(esempio)
  shiny::testServer(mod_filtri_server, args = list(dati = dati, azzera = origine), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    non_censiti <- function() {
      filtrati()[filtrati()$presente_a_database == "Non Presente", ]
    }
    session$flushReact()

    # un elenco solo: il servizio atteso delle letture dei censiti e il
    # servizio stimato dei non censiti
    expect_identical(
      scelte_atteso(),
      ordina_servizi(servizio_dei_filtri(esempio))
    )
    expect_identical(
      scelte_atteso(),
      c(
        "SECCO",
        "CARTA",
        "VETRO",
        "UMIDO",
        "PLASTICA E METALLI",
        "VERDE E RAMAGLIE",
        servizio_sacchetti()
      )
    )
    # un giro che non è né il servizio atteso di un censito né la stima di un
    # non censito non è tra le voci
    expect_false("SERVIZI MERCATI" %in% scelte_atteso())
    expect_true("SERVIZI MERCATI" %in% esempio$servizio_atteso)

    # "Nessuno" toglie tutte le voci e anche i non censiti con la stima incerta
    session$setInputs(atteso_nessuno = 1)
    expect_true(all(scelte_atteso() %in% stato$atteso_esclusi))
    expect_false(stato$includi_stima_incerta)
    expect_equal(nrow(filtrati()), 0)

    # la sola stima incerta: i marker con il punto di domanda
    session$setInputs(includi_stima_incerta = TRUE)
    expect_equal(nrow(filtrati()), 19)
    expect_true(all(filtrati()$presente_a_database == "Non Presente"))
    expect_true(all(is.na(filtrati()$servizio_icona)))
    expect_true("RFD20250920250" %in% filtrati()$RFID)
    expect_match(
      as.character(output$stat_box$html),
      "Stima incerta:</span>\\s*<b>19</b>"
    )

    # un solo servizio, senza gli incerti: i censiti del secco e i non censiti
    # stimati secco
    session$setInputs(
      includi_stima_incerta = FALSE,
      servizio_atteso_check = "SECCO"
    )
    expect_equal(nrow(filtrati()), 87)
    expect_equal(nrow(non_censiti()), 5)
    # per i censiti è il servizio del contenitore a database
    expect_true(all(
      filtrati()$servizio_transponder[
        filtrati()$presente_a_database == "Presente"
      ] ==
        "SECCO"
    ))
    expect_true(all(servizio_dei_filtri(filtrati()) == "SECCO"))
    expect_true("RFD20250915201" %in% non_censiti()$RFID)
    expect_no_match(as.character(output$stat_box$html), "Stima incerta")
    # con lo stato a database: i soli non censiti
    session$setInputs(presente_filter = "Non Presente")
    expect_equal(nrow(filtrati()), 5)
    session$setInputs(presente_filter = c("Presente", "Non Presente"))

    # "Tutti": di nuovo tutti, incerti compresi
    session$setInputs(atteso_tutti = 1)
    expect_length(stato$atteso_esclusi, 0)
    expect_true(stato$includi_stima_incerta)
    expect_equal(nrow(filtrati()), nrow(ultimi))

    # "Nessuno" resta tale anche in un periodo con un elenco diverso: nel
    # 2024 tra i servizi attesi c'è un giro che nel 2025 non compare
    voci_2025 <- scelte_atteso()
    session$setInputs(atteso_nessuno = 2)
    expect_equal(nrow(filtrati()), 0)
    dati(aggiungi_servizio_icona(
      filtra_periodo(dati_esempio(), periodo_anno(2024))
    ))
    session$flushReact()
    expect_gt(length(setdiff(scelte_atteso(), voci_2025)), 0)
    expect_equal(nrow(filtrati()), 0)
    # e una voce spuntata vale solo per sé
    session$setInputs(servizio_atteso_check = "SECCO")
    expect_gt(nrow(filtrati()), 0)
    expect_true(all(servizio_dei_filtri(filtrati()) == "SECCO"))

    # il reset riporta tutto ai valori predefiniti
    session$setInputs(reset = 1)
    expect_true(stato$includi_stima_incerta)
    expect_length(stato$atteso_esclusi, 0)
    expect_equal(
      nrow(filtrati()),
      dplyr::n_distinct(filtra_periodo(dati_esempio(), periodo_anno(2024))$RFID)
    )
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

      # Il browser comunica le righe mostrate dalla tabella: valgono dopo
      # l'attesa di `attesa_righe_tabella()`.
      dura <- attesa_righe_tabella() + 100
      righe_dalla_tabella <- function(indici) {
        session$setInputs(tabella_rows_all = indici)
        session$elapse(dura)
      }

      # i filtri della tabella valgono per grafici e mappa, se durano
      session$setInputs(tabella_rows_all = 1:10)
      expect_equal(nrow(righe_filtrate()), nrow(attesa))
      session$elapse(dura)
      expect_equal(nrow(righe_filtrate()), 10)

      # selezione da tabella e da mappa
      session$setInputs(tabella_rows_selected = 3)
      expect_identical(selezione()$rfid, attesa$RFID[3])
      session$setInputs(mappa_marker_click = list(id = "7"))
      expect_identical(selezione()$rfid, attesa$RFID[7])

      # un filtro che non lascia righe: grafici e mappa restano vuoti
      righe_dalla_tabella(NULL)
      expect_equal(nrow(righe_filtrate()), 0)
      expect_s3_class(grafico_indicatori(righe_filtrate()), "plotly")
      expect_s3_class(mappa_cluster(righe_filtrate()), "leaflet")
      # l'elenco vuoto che accompagna ogni ridisegno della tabella non conta
      righe_dalla_tabella(1:10)
      session$setInputs(tabella_rows_all = NULL)
      session$elapse(50)
      expect_equal(nrow(righe_filtrate()), 10)
      righe_dalla_tabella(1:5)
      expect_equal(nrow(righe_filtrate()), 5)
      # tutte le righe, anche in un altro ordine: come nessun filtro
      righe_dalla_tabella(rev(seq_len(nrow(attesa))))
      expect_null(righe_tabella())
      expect_identical(righe_filtrate(), risultato())
      righe_dalla_tabella(1:10)

      # raggio più ampio: meno cluster
      session$setInputs(eps_m = 2000, genera = 2)
      expect_lt(nrow(risultato()), nrow(attesa))

      # le righe della tabella precedente non valgono per l'analisi nuova
      expect_identical(righe_filtrate(), risultato())
      session$elapse(dura)
      expect_identical(righe_filtrate(), risultato())
      # un indice oltre la fine della tabella viene scartato
      righe_dalla_tabella(c(2, nrow(attesa) + 50))
      expect_identical(righe_filtrate()$RFID, risultato()$RFID[2])

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
    "Filtro Servizio Atteso",
    "Filtro Servizio Giro",
    "Filtro Coerenza Servizio",
    "Includi Non Censiti con stima incerta (?)",
    "Tutti",
    "Nessuno",
    "filtri-atteso_tutti",
    "filtri-atteso_nessuno",
    "filtri-giro_tutti",
    "filtri-giro_nessuno",
    "filtri-servizio_giro_check",
    "filtri-coerenza_check",
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
  # il servizio del contenitore a database non è più un filtro
  expect_no_match(html, "Filtro Servizio Transponder", fixed = TRUE)
  expect_no_match(html, "transponder_tutti", fixed = TRUE)
  # lo stato a database fa già da filtro per i non censiti
  expect_no_match(html, "includi_non_censiti", fixed = TRUE)
  expect_no_match(html, "senza stima", fixed = TRUE)
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

# Scrive una copia compressa con gzip di un file.
comprimi <- function(origine, env = parent.frame()) {
  destinazione <- withr::local_tempfile(fileext = ".gz", .local_envir = env)
  uscita <- gzfile(destinazione, "wb")
  writeBin(readBin(origine, "raw", file.size(origine)), uscita)
  close(uscita)
  destinazione
}

test_that("il caricamento accetta i file compressi", {
  compresso <- comprimi(percorso_dataset_esempio())
  storiche <- comprimi(percorso_storico_esempio())
  shiny::testServer(mod_caricamento_server, {
    caricati <- session$getReturned()
    # il formato si riconosce dal nome originale del file
    session$setInputs(
      file_upload = data.frame(
        name = "letture.csv.gz",
        datapath = compresso,
        stringsAsFactors = FALSE
      ),
      file_storico = data.frame(
        name = "storiche.csv.gz",
        datapath = storiche,
        stringsAsFactors = FALSE
      )
    )
    expect_identical(caricati$letture(), dati_esempio())
    expect_identical(caricati$storico(), storico_esempio())
    expect_match(as.character(output$esito$html), "letture.csv.gz", fixed = TRUE)
    expect_match(
      as.character(output$esito_storico$html),
      "storiche.csv.gz",
      fixed = TRUE
    )
  })
})

test_that("i file indicati all'avvio sono caricati senza passare dal browser", {
  shiny::testServer(
    mod_caricamento_server,
    args = list(
      percorsi = list(
        letture = percorso_dataset_esempio(),
        storico = percorso_storico_esempio()
      )
    ),
    {
      caricati <- session$getReturned()
      expect_identical(caricati$letture(), dati_esempio())
      expect_identical(caricati$storico(), storico_esempio())
      esito <- as.character(output$esito$html)
      expect_match(esito, "sample_rfid_dataset.csv", fixed = TRUE)
      expect_match(esito, "251</b>\\s*RFID univoci")
      expect_match(
        as.character(output$esito_storico$html),
        "sample_letture_storiche.csv",
        fixed = TRUE
      )

      # un file caricato a mano prende il posto di quello dell'avvio; le
      # letture storiche dell'avvio restano
      session$setInputs(
        file_upload = data.frame(
          name = "altre.csv",
          datapath = percorso_dataset_esempio(),
          stringsAsFactors = FALSE
        )
      )
      expect_match(as.character(output$esito$html), "altre.csv", fixed = TRUE)
      expect_identical(caricati$storico(), storico_esempio())
    }
  )
})

test_that("le opzioni di avvio mancanti o errate non bloccano l'app", {
  # senza opzioni: niente di caricato
  shiny::testServer(mod_caricamento_server, {
    expect_null(session$getReturned()$letture())
    expect_null(session$getReturned()$storico())
  })

  # demo: i due dataset di esempio
  shiny::testServer(mod_caricamento_server, args = list(demo = TRUE), {
    caricati <- session$getReturned()
    expect_equal(nrow(caricati$letture()), nrow(dati_esempio()))
    expect_equal(nrow(caricati$storico()), nrow(storico_esempio()))
  })

  # un percorso che non esiste: l'errore compare al posto dell'esito
  shiny::testServer(
    mod_caricamento_server,
    args = list(
      percorsi = list(
        letture = file.path(tempdir(), "letture_che_non_esistono.csv"),
        storico = NULL
      )
    ),
    {
      expect_null(session$getReturned()$letture())
      html <- as.character(output$esito$html)
      expect_match(html, "Caricamento non riuscito")
      expect_match(html, "File non trovato")
      expect_match(html, "letture_che_non_esistono.csv", fixed = TRUE)
    }
  )
})

test_that("i filtri geografici restringono cantieri e comuni e danno l'area da inquadrare", {
  tutte <- dati_esempio()
  dati <- shiny::reactiveVal(tutte)
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    restituito <- session$getReturned()
    session$flushReact()
    # i filtri geografici compaiono quando il dataset ha dei cantieri
    expect_true(con_geografia())
    expect_identical(scelte_cantieri(), cantieri())
    expect_named(restituito, c("dati_filtrati", "area"))
    expect_named(restituito$area(), c("latitudine", "longitudine"))
    expect_equal(nrow(restituito$area()), nrow(tutte))

    # i cantieri si deselezionano: restano le letture di quelli spuntati
    session$setInputs(cantiere_check = c("ASIAGO", "RUBANO"))
    expect_setequal(
      unique(restituito$dati_filtrati()$cantiere),
      c("ASIAGO", "RUBANO")
    )
    expect_equal(
      nrow(restituito$area()),
      sum(tutte$cantiere %in% c("ASIAGO", "RUBANO"))
    )
    statistiche <- as.character(output$stat_box$html)
    expect_match(statistiche, "ASIAGO:", fixed = TRUE)
    expect_no_match(statistiche, "BASSANO:", fixed = TRUE)
    # i comuni offerti sono quelli dei cantieri spuntati, divisi per cantiere
    expect_named(scelte_comuni(), c("ASIAGO", "RUBANO"))
    expect_true("LIMENA" %in% unlist(scelte_comuni()$RUBANO))

    # un comune restringe ancora
    session$setInputs(comuni = "LIMENA")
    expect_true(all(restituito$dati_filtrati()$comune_assegnato == "LIMENA"))
    expect_equal(
      nrow(restituito$area()),
      sum(tutte$comune_assegnato == "LIMENA", na.rm = TRUE)
    )

    # più comuni insieme, anche di cantieri diversi
    session$setInputs(comuni = c("LIMENA", "GALLIO"))
    scelti <- restituito$dati_filtrati()
    expect_setequal(unique(scelti$comune_assegnato), c("LIMENA", "GALLIO"))
    expect_setequal(unique(scelti$cantiere), c("RUBANO", "ASIAGO"))
    nei_due_comuni <- sum(tutte$comune_assegnato %in% c("LIMENA", "GALLIO"))
    expect_equal(nrow(restituito$area()), nei_due_comuni)
    statistiche <- as.character(output$stat_box$html)
    expect_match(statistiche, "ASIAGO:", fixed = TRUE)
    expect_match(statistiche, "RUBANO:", fixed = TRUE)

    # l'area segue cantieri e comuni, non gli altri filtri
    session$setInputs(presente_filter = "Non Presente")
    expect_equal(nrow(restituito$area()), nei_due_comuni)
    session$setInputs(presente_filter = stati_database())

    # deselezionare un cantiere toglie dalla scelta i suoi comuni, non gli altri
    session$setInputs(cantiere_check = "ASIAGO")
    session$flushReact()
    expect_identical(stato$comuni, "GALLIO")
    expect_true(all(restituito$dati_filtrati()$comune_assegnato == "GALLIO"))

    # senza comuni scelti valgono tutti quelli dei cantieri spuntati
    session$setInputs(comuni = NULL)
    expect_identical(stato$comuni, character(0))
    filtrati <- restituito$dati_filtrati()
    expect_true(all(filtrati$cantiere == "ASIAGO"))
    expect_gt(dplyr::n_distinct(filtrati$comune_assegnato), 1)

    session$setInputs(reset = 1)
    expect_equal(
      nrow(restituito$dati_filtrati()),
      nrow(deduplica_ultimo_rfid(tutte))
    )
    expect_equal(nrow(restituito$area()), nrow(tutte))
  })
})

test_that("le letture senza cantiere hanno una voce tra i cantieri", {
  # Nel dataset di esempio ogni lettura ha un cantiere: in un file può mancare.
  tutte <- dati_esempio()
  vuote <- tutte$RFID == "RFD20241001301"
  tutte$comune_lettura[vuote] <- fuori_servizio()
  tutte$comune_geografico[vuote] <- NA
  tutte$comune_assegnato[vuote] <- NA
  tutte$cantiere[vuote] <- NA
  dati <- shiny::reactiveVal(tutte)
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    restituito <- session$getReturned()
    session$flushReact()
    expect_identical(scelte_cantieri(), c(cantieri(), senza_cantiere()))
    expect_match(
      as.character(output$stat_box$html),
      paste0(senza_cantiere(), ":"),
      fixed = TRUE
    )
    session$setInputs(cantiere_check = senza_cantiere())
    expect_identical(restituito$dati_filtrati()$RFID, "RFD20241001301")
    expect_true(is.na(restituito$dati_filtrati()$cantiere))
    expect_equal(nrow(restituito$area()), sum(vuote))
  })
})

test_that("le scelte geografiche restano al cambio di periodo e si azzerano con un nuovo dataset", {
  origine <- shiny::reactiveVal(1)
  dati <- shiny::reactiveVal(filtra_periodo(dati_esempio(), periodo_anno(2025)))
  shiny::testServer(
    mod_filtri_server,
    args = list(dati = dati, azzera = origine),
    {
      filtrati <- function() session$getReturned()$dati_filtrati()
      session$flushReact()
      session$setInputs(cantiere_check = "BASSANO", comuni = "BASSANO DEL GRAPPA")
      expect_gt(nrow(filtrati()), 0)
      expect_true(all(filtrati()$comune_assegnato == "BASSANO DEL GRAPPA"))

      # cambio di periodo: cantiere e comune scelti restano
      dati(filtra_periodo(dati_esempio(), periodo_anno(2024)))
      session$flushReact()
      expect_gt(nrow(filtrati()), 0)
      expect_true(all(filtrati()$comune_assegnato == "BASSANO DEL GRAPPA"))
      expect_true(all(lubridate::year(filtrati()$giorno_lettura) == 2024))

      # nuovo dataset: tutti i cantieri e tutti i comuni
      origine(2)
      session$flushReact()
      expect_gt(dplyr::n_distinct(filtrati()$cantiere), 1)
      expect_gt(dplyr::n_distinct(filtrati()$comune_assegnato), 1)
    }
  )
})

test_that("un dataset senza cantieri non mostra i filtri geografici", {
  dati <- shiny::reactiveVal(validate_dataset(letture_test())$dati)
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    session$flushReact()
    expect_false(con_geografia())
    expect_length(scelte_comuni(), 0)
    expect_equal(nrow(session$getReturned()$dati_filtrati()), 3)
    expect_no_match(as.character(output$stat_box$html), "Cantiere")
  })
})

test_that("oltre il limite la mappa principale passa alla vista aggregata", {
  withr::local_options(trackerfid.max_marker = 50)
  ultimi <- deduplica_ultimo_rfid(dati_esempio())
  zona <- riquadro_zona()
  tutta <- list(
    north = zona$lat_max,
    south = zona$lat_min,
    east = zona$lng_max,
    west = zona$lng_min
  )
  centro <- comuni_cantieri()[comuni_cantieri()$comune == "ASIAGO", ]
  piccola <- list(
    north = centro$latitudine + 0.02,
    south = centro$latitudine - 0.02,
    east = centro$longitudine + 0.03,
    west = centro$longitudine - 0.03
  )
  inquadrati <- ultimi[nel_riquadro(ultimi, allarga_riquadro(piccola)), ]

  dati <- shiny::reactiveVal(ultimi)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "cluster"),
    {
      selezione <- session$getReturned()
      expect_null(vista())

      # tutta la zona: una bolla per cantiere
      session$setInputs(mappa_bounds = tutta, mappa_zoom = 9)
      expect_identical(vista()$tipo, "cantiere")
      expect_equal(nrow(bolle()), 4)
      expect_equal(sum(bolle()$n), nrow(ultimi))
      expect_null(disegnati())
      expect_match(output$riepilogo, "251 bidoni sulla mappa")
      expect_match(output$riepilogo, "una bolla per cantiere")

      # il click su una bolla ingrandisce, non seleziona un bidone
      session$setInputs(mappa_marker_click = list(id = "bolla-1"))
      expect_null(selezione())
      session$setInputs(mappa_marker_click = list(id = "bolla-99"))
      expect_null(selezione())

      # uno zoom che resta nella stessa fascia non ridisegna: i cantieri
      # restano fino a zoom 11
      disegnate <- bolle()
      session$setInputs(mappa_zoom = 11)
      session$elapse(400)
      expect_identical(vista()$tipo, "cantiere")
      expect_identical(vista()$zoom, 9)
      expect_identical(bolle(), disegnate)

      # ingrandendo: una bolla per comune, subito, senza attendere
      session$setInputs(mappa_zoom = 12)
      expect_identical(vista()$tipo, "comune")
      expect_identical(vista()$zoom, 12)
      expect_equal(nrow(bolle()), dplyr::n_distinct(ultimi$comune_assegnato))
      expect_match(output$riepilogo, "una bolla per comune")
      # passata l'attesa non si ridisegna una seconda volta
      disegnate <- bolle()
      session$elapse(400)
      expect_identical(bolle(), disegnate)

      # a zoom 13 e 14 restano i comuni: stessa fascia, nessun ridisegno
      session$setInputs(mappa_zoom = 13)
      session$elapse(400)
      expect_identical(vista()$tipo, "comune")
      expect_identical(bolle(), disegnate)
      session$setInputs(mappa_zoom = 14)
      session$elapse(400)
      expect_identical(vista()$tipo, "comune")
      expect_identical(bolle(), disegnate)

      # ancora: bolle per riquadro, valide per l'area caricata. Da vicino la
      # mappa aspetta la fine degli spostamenti
      session$setInputs(mappa_zoom = 15)
      expect_identical(vista()$tipo, "comune")
      session$elapse(400)
      expect_identical(vista()$tipo, "griglia")
      expect_identical(vista()$riquadro, allarga_riquadro(tutta))

      # un'area con pochi bidoni: i singoli marker
      session$setInputs(mappa_bounds = piccola, mappa_zoom = 16)
      session$elapse(400)
      expect_identical(vista()$tipo, "singoli")
      expect_null(bolle())
      expect_identical(disegnati()$RFID, inquadrati$RFID)
      expect_match(output$riepilogo, "solo i bidoni dell'area inquadrata")

      # il click su un marker risale al bidone tra quelli inquadrati
      session$setInputs(mappa_marker_click = list(id = "2"))
      expect_identical(selezione()$rfid, inquadrati$RFID[2])
      session$setInputs(
        mappa_marker_click = list(id = as.character(nrow(inquadrati) + 1))
      )
      expect_identical(selezione()$rfid, inquadrati$RFID[2])

      # uno spostamento dentro l'area caricata non ridisegna i marker
      poco_oltre <- piccola
      poco_oltre$north <- piccola$north + 0.005
      poco_oltre$south <- piccola$south + 0.005
      session$setInputs(mappa_bounds = poco_oltre)
      session$elapse(400)
      expect_identical(vista()$riquadro, allarga_riquadro(piccola))

      # con meno bidoni del limite tornano tutti, raggruppati dal browser
      dati(ultimi[1:40, ])
      session$flushReact()
      expect_identical(vista()$tipo, "tutti")
      expect_equal(nrow(disegnati()), 40)
      expect_no_match(output$riepilogo, "bolla")
    }
  )
})

test_that("sotto il limite la mappa principale segue lo zoom: cantieri, comuni, bidoni", {
  withr::local_options(trackerfid.max_marker = NULL)
  ultimi <- deduplica_ultimo_rfid(dati_esempio())
  dati <- shiny::reactiveVal(ultimi)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "cluster"),
    {
      selezione <- session$getReturned()
      # da lontano una bolla per cantiere
      session$setInputs(mappa_zoom = 9)
      expect_identical(vista()$tipo, "cantiere")
      expect_identical(sort(bolle()$nome), cantieri())
      expect_equal(sum(bolle()$n), nrow(ultimi))
      expect_null(disegnati())
      expect_match(output$riepilogo, "^251 bidoni sulla mappa \\(ultima lettura\\)")
      expect_match(output$riepilogo, "una bolla per cantiere")

      # a zoom 11 restano i cantieri
      session$setInputs(mappa_zoom = 11)
      session$elapse(400)
      expect_identical(vista()$tipo, "cantiere")
      expect_match(output$riepilogo, "una bolla per cantiere")

      # avvicinandosi ancora una bolla per comune, senza attesa
      session$setInputs(mappa_zoom = 12)
      expect_identical(vista()$tipo, "comune")
      # e di nuovo i cantieri allontanandosi, sempre senza attesa
      session$setInputs(mappa_zoom = 11)
      expect_identical(vista()$tipo, "cantiere")
      session$setInputs(mappa_zoom = 12)
      session$elapse(400)
      expect_identical(vista()$tipo, "comune")
      expect_equal(nrow(bolle()), dplyr::n_distinct(ultimi$comune_assegnato))
      expect_match(output$riepilogo, "una bolla per comune")
      # i comuni restano anche a zoom 13 e 14
      session$setInputs(mappa_zoom = 13)
      session$elapse(400)
      expect_identical(vista()$tipo, "comune")
      session$setInputs(mappa_zoom = 14)
      session$elapse(400)
      expect_identical(vista()$tipo, "comune")

      # da vicino tutti i bidoni, uno per uno: dopo l'attesa
      session$setInputs(mappa_zoom = 15)
      expect_identical(vista()$tipo, "comune")
      session$elapse(400)
      expect_identical(vista()$tipo, "tutti")
      expect_equal(nrow(disegnati()), nrow(ultimi))
      expect_null(bolle())
      expect_identical(
        output$riepilogo,
        "251 bidoni sulla mappa (ultima lettura)"
      )
      session$setInputs(mappa_marker_click = list(id = "3"))
      expect_identical(selezione()$rfid, ultimi$RFID[3])

      # da vicino spostamenti e zoom non ridisegnano niente
      session$setInputs(
        mappa_zoom = 17,
        mappa_bounds = list(north = 45.8, south = 45.7, east = 11.8, west = 11.7)
      )
      session$elapse(400)
      expect_identical(vista()$tipo, "tutti")
      expect_identical(vista()$zoom, 15)

      # tornando lontano tornano le bolle, subito
      session$setInputs(mappa_zoom = 11)
      expect_identical(vista()$tipo, "cantiere")
      expect_null(disegnati())
      # un click su una bolla non seleziona un bidone, con o senza l'area
      # inquadrata comunicata dal browser
      session$setInputs(mappa_marker_click = list(id = "bolla-2"))
      expect_identical(selezione()$rfid, ultimi$RFID[3])
      session$setInputs(
        mappa_bounds = list(north = 46.1, south = 45.2, east = 12.2, west = 11.2)
      )
      session$setInputs(mappa_marker_click = list(id = "bolla-3"))
      expect_identical(selezione()$rfid, ultimi$RFID[3])
      expect_identical(vista()$tipo, "cantiere")
    }
  )
})

test_that("la ricerca RFID disegna al massimo le letture previste dal limite", {
  withr::local_options(trackerfid.max_letture_ricerca = 5)
  trovate <- cerca_per_rfid(dati_esempio(), "RFD20250901001")
  dati <- shiny::reactiveVal(trovate)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "rfid"),
    {
      selezione <- session$getReturned()
      session$setInputs(mappa_zoom = 11)
      expect_equal(nrow(disegnati()), 5)
      expect_identical(attr(disegnati(), "totale"), nrow(trovate))
      expect_match(
        output$riepilogo,
        sprintf("%d letture di 1 RFID", nrow(trovate))
      )
      expect_match(output$riepilogo, "sulla mappa le 5 più recenti")
      # la prima riga disegnata è l'ultima lettura dell'RFID
      expect_true(disegnati()$is_ultimo[1])
      session$setInputs(mappa_marker_click = list(id = "1"))
      expect_identical(selezione()$rfid, "RFD20250901001")
    }
  )
})

test_that("l'interfaccia ha i filtri geografici e le sole quattro schede previste", {
  html <- as.character(app_ui(NULL))
  for (testo in c(
    "Filtro Cantiere",
    "Filtro Comune",
    "Tutti i comuni",
    "CSV, CSV.GZ o Parquet"
  )) {
    expect_match(html, testo, fixed = TRUE)
  }
  # l'elenco dei comuni permette più scelte; senza scelte vale per tutti
  expect_match(html, '<select[^>]*id="filtri-comuni"[^>]*multiple')
  expect_match(html, '"placeholder":"Tutti i comuni"', fixed = TRUE)
  # i campi di caricamento accettano i tre formati
  expect_match(html, 'accept=".csv,.txt,.gz,.parquet', fixed = TRUE)
  # le schede sono le tre mappe, la ricerca dei giri e l'analisi dei cluster,
  # con le sue tre viste
  schede <- regmatches(
    html,
    gregexpr('data-toggle="tab"[^>]*data-value="[a-z]+"', html)
  )[[1]]
  expect_identical(
    sub('.*data-value="([a-z]+)"', "\\1", schede),
    c("mappa", "rfid", "giro", "utenza", "cluster", "tabella", "grafici", "mappa")
  )
})

test_that("i sacchetti hanno la loro voce tra i servizi e seguono le scelte rapide", {
  sacchetto <- function(n) sprintf("00BD%020d", n)
  grezzi <- dplyr::bind_rows(
    # censito senza servizio a database: lo assegna la validazione
    letture_rfid(
      sacchetto(1),
      date_2025(3),
      servizio = NA_character_,
      atteso = "SECCO PAP"
    ),
    letture_rfid(
      sacchetto(2),
      date_2025(3),
      presente = "Non Presente",
      atteso = "VETRO PAP"
    ),
    letture_rfid(
      "0000ABC123",
      date_2025(3),
      servizio = "CARTA",
      atteso = "CARTA/CARTONE PAP"
    ),
    letture_rfid(
      "0000ABC124",
      date_2025(3),
      presente = "Non Presente",
      atteso = "SECCO PAP"
    )
  )
  dati <- shiny::reactiveVal(
    aggiungi_servizio_icona(validate_dataset(grezzi)$dati)
  )
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    # il sacchetto censito è letto da un giro del secco, ma la sua voce è
    # quella dei sacchetti. Il censito della carta sta sotto il servizio del
    # suo contenitore, non sotto il giro che lo ha letto
    expect_identical(
      scelte_atteso(),
      c("SECCO PAP", "CARTA", servizio_sacchetti())
    )
    expect_equal(nrow(filtrati()), 4)
    statistiche <- as.character(output$stat_box$html)
    expect_equal(
      lengths(regmatches(statistiche, gregexpr("SACCHETTI:", statistiche))),
      2
    )

    # tolta la voce dei sacchetti escono tutti e due, censito e non censito
    session$setInputs(
      servizio_atteso_check = c("SECCO PAP", "CARTA")
    )
    expect_setequal(filtrati()$RFID, c("0000ABC123", "0000ABC124"))
    # la sola voce dei sacchetti
    session$setInputs(servizio_atteso_check = servizio_sacchetti())
    expect_setequal(filtrati()$RFID, c(sacchetto(1), sacchetto(2)))
    # con lo stato a database: il solo sacchetto non censito
    session$setInputs(presente_filter = "Non Presente")
    expect_identical(filtrati()$RFID, sacchetto(2))
    session$setInputs(presente_filter = c("Presente", "Non Presente"))
    # "Nessuno" toglie anche i sacchetti, "Tutti" li rimette
    session$setInputs(atteso_nessuno = 1)
    expect_equal(nrow(filtrati()), 0)
    session$setInputs(atteso_tutti = 1)
    expect_equal(nrow(filtrati()), 4)
  })
})

test_that("il filtro sulla posizione restringe la mappa alle letture fuori comune o fuori giro", {
  dati <- shiny::reactiveVal(
    aggiungi_servizio_icona(filtra_periodo(dati_esempio(), periodo_anno(2025)))
  )
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    tutti <- nrow(filtrati())
    expect_equal(tutti, 248)

    # solo le letture fuori comune: il contenitore spostato e il tag sul mezzo
    session$setInputs(posizione_check = "fuori_comune")
    expect_setequal(filtrati()$RFID, c("RFD20250905100", "RFD20250203302"))
    expect_true(all(filtrati()$fuori_comune))
    expect_match(
      as.character(output$stat_box$html),
      "Fuori comune:</span>\\s*<b>2</b>"
    )

    # solo fuori giro: la mappa mostra l'ultima lettura fuori giro di ogni RFID
    session$setInputs(posizione_check = "fuori_giro")
    expect_true(all(filtrati()$fuori_giro))
    expect_equal(
      nrow(filtrati()),
      dplyr::n_distinct(dati()$RFID[dati()$fuori_giro])
    )
    expect_gt(nrow(filtrati()), 30)
    expect_true(all(c("RFD20241001301", "RFD20250203302") %in% filtrati()$RFID))

    # solo le letture coerenti
    session$setInputs(posizione_check = "regolare")
    expect_false(any(filtrati()$fuori_comune | filtrati()$fuori_giro))
    # nessuna voce: nessun bidone
    session$setInputs(posizione_check = NULL)
    expect_equal(nrow(filtrati()), 0)
    # il reset rimette tutte le voci
    session$setInputs(reset = 1)
    expect_equal(nrow(filtrati()), tutti)
  })
})

test_that("la ricerca dei giri parte dal giorno pi\u00f9 recente e segue mezzo, giorni e filtri", {
  dati <- shiny::reactiveVal(filtra_periodo(dati_esempio(), periodo_anno(2025)))
  shiny::testServer(mod_giro_server, args = list(dati = dati), {
    restituito <- session$getReturned()
    session$flushReact()
    # all'avvio: tutti i mezzi, ultimo giorno con letture
    expect_identical(stato$mezzo, "")
    expect_identical(stato$dal, as.Date("2025-12-31"))
    expect_identical(stato$al, as.Date("2025-12-31"))
    all_avvio <- restituito$risultati()
    expect_true(all(all_avvio$giorno == as.Date("2025-12-31")))
    expect_gt(nrow(all_avvio), 0)
    expect_null(restituito$messaggio())

    # un mezzo, un giorno: il giro sbagliato
    session$setInputs(
      mezzo = "VEH001",
      giorni = as.Date(c("2025-09-24", "2025-09-24"))
    )
    expect_equal(nrow(restituito$risultati()), 10)
    expect_true(all(restituito$risultati()$matricola_veicolo == "VEH001"))
    giri <- restituito$riepilogo()
    expect_equal(nrow(giri), 1)
    expect_identical(giri$comune_giro, "LIMENA")
    expect_identical(giri$fuori_giro, 10L)
    expect_match(as.character(output$esito$html), "1 giro \u00b7 10 letture")
    expect_match(as.character(output$esito$html), "1 giro con letture fuori giro")

    # un intervallo di giorni, tutti i mezzi, solo le letture fuori giro
    session$setInputs(
      mezzo = "",
      giorni = as.Date(c("2025-09-22", "2025-09-28")),
      solo = "fuori_giro"
    )
    settimana <- restituito$risultati()
    expect_true(all(settimana$fuori_giro))
    expect_gt(dplyr::n_distinct(settimana$matricola_veicolo), 1)
    expect_true(all(
      settimana$giorno >= as.Date("2025-09-22") &
        settimana$giorno <= as.Date("2025-09-28")
    ))
    giri <- restituito$riepilogo()
    expect_true(all(giri$fuori_giro > 0))

    # un click su una riga della tabella: le sole letture di quel giro
    riga <- which(giri$matricola_veicolo == "VEH001" & giri$fuori_giro == 10)
    session$setInputs(tabella_rows_selected = riga)
    expect_equal(nrow(restituito$risultati()), 10)
    expect_true(all(restituito$risultati()$comune_giro == "LIMENA"))
    expect_true(all(restituito$risultati()$comune_lettura == "BORGORICCO"))
    # tolta la selezione tornano tutte
    session$setInputs(tabella_rows_selected = NULL)
    expect_equal(nrow(restituito$risultati()), nrow(settimana))
    # una riga che non esiste non sceglie niente
    session$setInputs(tabella_rows_selected = 9999)
    expect_equal(nrow(restituito$risultati()), nrow(settimana))

    # cambiare la ricerca toglie il giro scelto
    session$setInputs(tabella_rows_selected = riga)
    session$setInputs(solo = NULL)
    expect_gt(nrow(restituito$risultati()), nrow(settimana))

    # un intervallo scritto a met\u00e0, o rovesciato, non cambia la ricerca
    session$setInputs(giorni = as.Date(c("2025-09-28", "2025-09-22")))
    expect_identical(stato$dal, as.Date("2025-09-22"))
    session$setInputs(giorni = as.Date(c("2025-09-22", NA)))
    expect_identical(stato$al, as.Date("2025-09-28"))

    # nessuna lettura: avviso sulla mappa
    session$setInputs(
      mezzo = "VEH005",
      giorni = as.Date(c("2025-09-28", "2025-09-28"))
    )
    expect_equal(nrow(restituito$risultati()), 0)
    expect_match(restituito$messaggio(), "Nessuna lettura")

    # un nuovo periodo riporta la ricerca ai valori predefiniti
    dati(filtra_periodo(dati_esempio(), periodo_anno(2024)))
    session$flushReact()
    expect_identical(stato$mezzo, "")
    expect_identical(stato$solo, character(0))
    expect_identical(stato$dal, as.Date("2024-12-31"))
    expect_true(all(restituito$risultati()$giorno == as.Date("2024-12-31")))

    # senza letture non c'\u00e8 niente da cercare
    dati(NULL)
    session$flushReact()
    expect_null(restituito$risultati())
    expect_equal(nrow(restituito$riepilogo()), 0)
  })
})

test_that("la mappa dei giri disegna le letture trovate, anche ripetute", {
  letture <- cerca_giri(dati_esempio(), "VEH001", "2025-09-22", "2025-09-28")
  dati <- shiny::reactiveVal(letture)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "giro"),
    {
      selezione <- session$getReturned()
      session$setInputs(mappa_zoom = 11)
      # nessuna bolla: tutte le letture, a qualsiasi zoom
      expect_identical(vista()$tipo, "tutti")
      expect_equal(nrow(disegnati()), nrow(letture))
      expect_match(
        output$riepilogo,
        sprintf("^%d letture di %d bidoni", nrow(letture), dplyr::n_distinct(letture$RFID))
      )
      expect_match(
        output$riepilogo,
        sprintf("%d fuori giro", sum(letture$fuori_giro)),
        fixed = TRUE
      )
      expect_gte(sum(letture$fuori_giro), 10)
      session$setInputs(mappa_marker_click = list(id = "4"))
      expect_identical(selezione()$rfid, letture$RFID[4])
    }
  )
  # oltre il limite la mappa tiene per prime le letture con un'incongruenza
  withr::local_options(trackerfid.max_letture_ricerca = 12)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "giro"),
    {
      session$setInputs(mappa_zoom = 11)
      expect_equal(nrow(disegnati()), 12)
      expect_gte(sum(disegnati()$fuori_giro), 10)
      expect_match(output$riepilogo, "sulla mappa 12 letture")
    }
  )
})

test_that("l'interfaccia ha la scheda dei giri, il suo pannello e il filtro sulla posizione", {
  html <- as.character(app_ui(NULL))
  expect_match(html, "Ricerca Giro", fixed = TRUE)
  expect_match(html, 'id="giro-mezzo"', fixed = TRUE)
  expect_match(html, 'id="giro-giorni"', fixed = TRUE)
  expect_match(html, 'id="giro-solo"', fixed = TRUE)
  expect_match(html, 'id="giro-tabella"', fixed = TRUE)
  expect_match(html, 'id="mappa_giro-mappa"', fixed = TRUE)
  expect_match(html, "input.scheda_attiva == &#39;giro&#39;", fixed = TRUE)
  expect_match(html, "Visualizzazione: Ricerca Giro", fixed = TRUE)
  # la scheda dei giri sta dopo la ricerca per RFID
  expect_lt(
    regexpr('data-value="rfid"', html, fixed = TRUE),
    regexpr('data-value="giro"', html, fixed = TRUE)
  )
  expect_lt(
    regexpr('data-value="giro"', html, fixed = TRUE),
    regexpr('data-value="utenza"', html, fixed = TRUE)
  )
  # il filtro sulla posizione, con le tre voci spuntate
  expect_match(html, 'id="filtri-posizione_check"', fixed = TRUE)
  for (voce in names(stati_posizione())) {
    expect_match(
      html,
      sprintf('name="filtri-posizione_check" value="%s" checked', voce),
      fixed = TRUE
    )
  }
})
