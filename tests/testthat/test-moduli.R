test_that("il caricamento restituisce il dataset validato", {
  atteso <- dati_esempio()
  shiny::testServer(mod_caricamento_server, {
    expect_null(session$getReturned()())

    session$setInputs(
      file_upload = data.frame(
        name = "letture.csv",
        datapath = percorso_dataset_esempio(),
        stringsAsFactors = FALSE
      )
    )
    dati <- session$getReturned()()
    # le letture stanno in un archivio DuckDB
    expect_s3_class(dati, "archivio_letture")
    expect_equal(dati$n, nrow(atteso))
    expect_identical(letture_archivio(dati), atteso)
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
    esempio <- session$getReturned()()
    expect_equal(esempio$n, nrow(dati_esempio()))
    # l'archivio di esempio è condiviso tra le sessioni: nessuna lo chiude
    expect_true(esempio$condiviso)

    session$setInputs(
      file_upload = data.frame(
        name = "errato.csv",
        datapath = errato,
        stringsAsFactors = FALSE
      )
    )
    expect_null(session$getReturned()())
    # l'archivio di esempio sostituito resta aperto per le altre sessioni
    expect_true(DBI::dbIsValid(esempio$con))
    html <- as.character(output$esito$html)
    expect_match(html, "Caricamento non riuscito")
    expect_match(html, "Colonne mancanti")
  })
})

test_that("un file caricato a mano sostituisce il precedente e ne chiude l'archivio", {
  shiny::testServer(mod_caricamento_server, {
    caricati <- session$getReturned()
    session$setInputs(carica_esempio = 1)
    esempio <- caricati()
    expect_true(esempio$condiviso)

    carica_a_mano <- function() {
      session$setInputs(
        file_upload = data.frame(
          name = "letture.csv",
          datapath = percorso_dataset_esempio(),
          stringsAsFactors = FALSE
        )
      )
      caricati()
    }
    caricato <- carica_a_mano()
    expect_equal(caricato$n, nrow(dati_esempio()))
    # un file caricato dal browser appartiene alla sessione
    expect_false(caricato$condiviso)
    # l'archivio di esempio resta aperto per le altre sessioni
    expect_true(DBI::dbIsValid(esempio$con))

    # l'archivio caricato a mano e sostituito si chiude
    secondo <- carica_a_mano()
    expect_false(DBI::dbIsValid(caricato$con))
    expect_true(DBI::dbIsValid(secondo$con))
  })
})

test_that("il periodo predefinito è l'anno più recente del dataset", {
  dati <- shiny::reactiveVal(archivio_esempio())
  shiny::testServer(mod_periodo_server, args = list(dati = dati), {
    periodo <- session$getReturned()
    session$flushReact()
    expect_identical(periodo$periodo(), as.Date(c("2025-01-01", "2025-12-31")))
    expect_identical(periodo$etichetta(), "Anno 2025")
    # il periodo è una selezione dell'archivio, senza copie delle letture
    selezione <- periodo$dati()
    expect_s3_class(selezione, "selezione_letture")
    letture <- letture_periodo(selezione)
    expect_true(all(lubridate::year(letture$giorno_lettura) == 2025))
    expect_equal(selezione$n, nrow(letture))
    # con letture nel periodo la barra in alto non mostra avvisi
    expect_null(output$riepilogo$html)

    # un anno intero scelto con le date ha l'etichetta dell'anno
    session$setInputs(intervallo = as.Date(c("2024-01-01", "2024-12-31")))
    expect_identical(periodo$periodo(), as.Date(c("2024-01-01", "2024-12-31")))
    expect_identical(periodo$etichetta(), "Anno 2024")
    expect_true(all(
      lubridate::year(letture_periodo(periodo$dati())$giorno_lettura) == 2024
    ))
    expect_lt(periodo$dati()$n, nrow(letture))
  })
})

test_that("il periodo è l'intervallo di date scelto, e viene validato", {
  dati <- shiny::reactiveVal(archivio_esempio())
  shiny::testServer(mod_periodo_server, args = list(dati = dati), {
    periodo <- session$getReturned()
    session$flushReact()

    session$setInputs(intervallo = as.Date(c("2025-03-15", "2025-09-30")))
    expect_identical(periodo$periodo(), as.Date(c("2025-03-15", "2025-09-30")))
    expect_identical(periodo$etichetta(), "15/03/2025 - 30/09/2025")
    giorni <- as.Date(letture_periodo(periodo$dati())$giorno_lettura)
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
    expect_equal(periodo$dati()$n, 0)
    expect_match(
      as.character(output$riepilogo$html),
      "Nessuna lettura nel periodo"
    )
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

    dati(archivio_esempio())
    session$flushReact()
    session$setInputs(intervallo = as.Date(c("2025-05-01", "2025-05-31")))
    expect_identical(periodo$etichetta(), "01/05/2025 - 31/05/2025")

    solo_2024 <- dati_esempio()[
      lubridate::year(dati_esempio()$giorno_lettura) == 2024,
    ]
    dati(archivio_da_letture(solo_2024))
    session$flushReact()
    expect_identical(periodo$etichetta(), "Anno 2024")
    expect_equal(periodo$dati()$n, nrow(solo_2024))

    dati(NULL)
    session$flushReact()
    expect_null(periodo$periodo())
  })
})

test_that("i filtri partono senza esclusioni e reagiscono agli input", {
  ultimi <- ultime_esempio()
  n_presenti <- sum(ultimi$presente_a_database == "Presente")
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio()))
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
    session$setInputs(servizio_check = c("SECCO", "CARTA"))
    expect_setequal(stato$servizi_esclusi, setdiff(scelte_servizio(), c("SECCO", "CARTA")))
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
    expect_length(stato$servizi_esclusi, 0)
  })
})

test_that("i filtri sul giro e sulla coerenza dei servizi restringono la mappa", {
  esempio <- dati_esempio()
  n_rfid <- dplyr::n_distinct(esempio$RFID)
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio()))
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
    expect_true(all(filtrati()$coerenza_servizio == "NO"))
    expect_match(
      as.character(output$stat_box$html),
      "Non coerenti:</span>\\s*<b>4</b>"
    )
    # le sole coerenti: restano tutti gli RFID, ognuno con la sua ultima
    # lettura coerente
    session$setInputs(coerenza_check = "coerente")
    expect_identical(stato$coerenza_escluse, "non_coerente")
    expect_equal(nrow(filtrati()), n_rfid)
    expect_false(any(filtrati()$coerenza_servizio %in% "NO"))
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
  ultimi <- ultime_esempio(periodo = periodo_anno(2025))
  # Il periodo cambia senza che cambi il dataset: i filtri non si azzerano.
  origine <- shiny::reactiveVal(1)
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio(), periodo_anno(2025)))
  shiny::testServer(mod_filtri_server, args = list(dati = dati, azzera = origine), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    non_censiti <- function() {
      filtrati()[filtrati()$presente_a_database == "Non Presente", ]
    }
    session$flushReact()

    # un elenco solo: il servizio a database delle letture dei censiti e il
    # servizio stimato dei non censiti
    expect_identical(
      scelte_servizio(),
      ordina_servizi(servizio_mostrato(esempio))
    )
    expect_identical(
      scelte_servizio(),
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
    # un servizio atteso che non è né il servizio a database di un censito né
    # la stima di un non censito non è tra le voci
    expect_false("SERVIZI" %in% scelte_servizio())
    expect_true("SERVIZI" %in% esempio$servizio_atteso)

    # "Nessuno" toglie tutte le voci e anche i non censiti con la stima incerta
    session$setInputs(servizio_nessuno = 1)
    expect_true(all(scelte_servizio() %in% stato$servizi_esclusi))
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
      servizio_check = "SECCO"
    )
    expect_equal(nrow(filtrati()), 87)
    expect_equal(nrow(non_censiti()), 5)
    # per i censiti è il servizio del contenitore a database
    censiti <- filtrati()[filtrati()$presente_a_database == "Presente", ]
    expect_true(all(censiti$servizio_transponder == "SECCO"))
    expect_true(all(servizio_mostrato(filtrati()) == "SECCO"))
    expect_true("RFD20250915201" %in% non_censiti()$RFID)
    expect_no_match(as.character(output$stat_box$html), "Stima incerta")
    # con lo stato a database: i soli non censiti
    session$setInputs(presente_filter = "Non Presente")
    expect_equal(nrow(filtrati()), 5)
    session$setInputs(presente_filter = c("Presente", "Non Presente"))

    # "Tutti": di nuovo tutti, incerti compresi
    session$setInputs(servizio_tutti = 1)
    expect_length(stato$servizi_esclusi, 0)
    expect_true(stato$includi_stima_incerta)
    expect_equal(nrow(filtrati()), nrow(ultimi))

    # "Nessuno" resta tale anche in un periodo con un elenco diverso: nel
    # 2024 tra i servizi attesi c'è un giro che nel 2025 non compare
    voci_2025 <- scelte_servizio()
    session$setInputs(servizio_nessuno = 2)
    expect_equal(nrow(filtrati()), 0)
    dati(seleziona_periodo(archivio_esempio(), periodo_anno(2024)))
    session$flushReact()
    expect_gt(length(setdiff(scelte_servizio(), voci_2025)), 0)
    expect_equal(nrow(filtrati()), 0)
    # e una voce spuntata vale solo per sé
    session$setInputs(servizio_check = "SECCO")
    expect_gt(nrow(filtrati()), 0)
    expect_true(all(servizio_mostrato(filtrati()) == "SECCO"))

    # il reset riporta tutto ai valori predefiniti
    session$setInputs(reset = 1)
    expect_true(stato$includi_stima_incerta)
    expect_length(stato$servizi_esclusi, 0)
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

      dati(seleziona_periodo(archivio_esempio(), periodo_anno(2025)))
      session$flushReact()
      session$setInputs(presente_filter = "Non Presente")
      expect_true(all(filtrati()$presente_a_database == "Non Presente"))

      # cambio di periodo: le scelte dell'utente restano
      dati(seleziona_periodo(archivio_esempio(), periodo_anno(2024)))
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
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio()))
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
      # la tabella sopra la mappa riporta tutte le letture trovate
      expect_identical(
        output$nota_letture,
        sprintf("%d letture nel periodo. Sulla mappa le ultime 10 di ogni RFID.", n_letture)
      )
      tabella <- jsonlite::fromJSON(output$letture)$x$tag$attribs
      expect_identical(tabella$groupBy, "RFID")
      expect_length(tabella$data$RFID, n_letture)

      session$setInputs(pulisci = 1)
      expect_null(ricerca$risultati())
      expect_null(ricerca$codici())
      expect_error(output$letture)
    }
  )
})

test_that("la ricerca utenza mostra solo i bidoni dell'utenza attuale", {
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio()))
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

# Sostituisce `invia_mappa()` con una funzione che registra i messaggi, che la
# sessione di prova di Shiny non conserva.
registra_messaggi <- function(env = parent.frame()) {
  registro <- new.env()
  registro$messaggi <- list()
  testthat::local_mocked_bindings(
    invia_mappa = function(session, id, tipo, ...) {
      registro$messaggi[[length(registro$messaggi) + 1]] <-
        c(list(id = id, tipo = tipo), list(...))
    },
    .env = env
  )
  registro
}
tipi_messaggi <- function(registro) {
  vapply(registro$messaggi, function(m) m$tipo, character(1))
}

test_that("il click su un marker restituisce l'RFID della lettura e apre il popup", {
  registro <- registra_messaggi()
  trovate <- cerca_per_rfid(
    seleziona_periodo(archivio_esempio()),
    c("RFD20250901001", "RFD20250905100")
  )
  dati <- shiny::reactiveVal(trovate)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "rfid"),
    {
      selezione <- session$getReturned()
      expect_null(selezione())
      session$flushReact()
      expect_match(
        output$riepilogo,
        sprintf("%d letture di 2 RFID", nrow(trovate))
      )
      # dati e inquadratura arrivano alla mappa
      expect_identical(tipi_messaggi(registro), c("dati", "inquadra"))
      expect_identical(registro$messaggi[[1]]$id, session$ns("mappa"))
      expect_identical(registro$messaggi[[1]]$dati, dati_mappa(trovate, "rfid"))
      expect_identical(registro$messaggi[[2]]$riquadro, riquadro_vista(trovate))

      # il browser manda la posizione del marker nell'elenco, da 0
      ultima <- nrow(trovate)
      session$setInputs(mappa_clic = list(i = ultima - 1, nonce = 1))
      expect_identical(selezione()$rfid, trovate$RFID[ultima])
      popup <- registro$messaggi[[3]]
      expect_identical(popup$tipo, "popup")
      expect_identical(popup$html, popup_lettura(trovate, ultima))
      expect_identical(c(popup$lng, popup$lat), c(trovate$longitudine[ultima], trovate$latitudine[ultima]))

      # un marker che non esiste non sceglie niente
      session$setInputs(mappa_clic = list(i = 9999, nonce = 2))
      expect_identical(selezione()$rfid, trovate$RFID[ultima])
      expect_length(registro$messaggi, 3)
    }
  )
})

test_that("l'analisi dei cluster parte dal pulsante e segue il periodo", {
  registro <- registra_messaggi()
  dati <- shiny::reactiveVal(archivio_esempio())
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
      # la mappa riceve i cluster mostrati dalla tabella
      ultimi_dati <- Filter(function(m) m$tipo == "dati", registro$messaggi)
      expect_length(ultimi_dati[[length(ultimi_dati)]]$dati$punti$x, 10)
      session$setInputs(mappa_clic = list(i = 6, nonce = 1))
      expect_identical(selezione()$rfid, attesa$RFID[7])
      expect_identical(registro$messaggi[[length(registro$messaggi)]]$tipo, "popup")

      # un filtro che non lascia righe: grafici e mappa restano vuoti
      righe_dalla_tabella(NULL)
      expect_equal(nrow(righe_filtrate()), 0)
      expect_s3_class(grafico_indicatori(righe_filtrate()), "plotly")
      expect_length(dati_mappa_cluster(righe_filtrate())$punti$x, 0)
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
  dati <- shiny::reactiveVal(archivio_esempio())
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

test_that("il caricamento si chiude e il periodo sta nella barra in alto", {
  html <- as.character(app_ui(NULL))
  # la sezione del caricamento è collassabile, aperta all'avvio, e chiusa
  # mostra il nome del file caricato
  expect_match(
    html,
    '<details open class="sezione-sidebar sezione-collassabile">\\s*<summary class="titolo-sezione" title="Mostra o nascondi la sezione">'
  )
  expect_match(html, 'id="caricamento-riassunto"', fixed = TRUE)
  # il periodo sta nell'intestazione, non nella barra laterale
  intestazione <- regmatches(html, regexpr("<header.*</header>", html))
  laterale <- regmatches(html, regexpr("<aside.*</aside>", html))
  for (id in c("periodo-intervallo", "periodo-riepilogo")) {
    expect_match(intestazione, sprintf('id="%s"', id), fixed = TRUE)
    expect_no_match(laterale, sprintf('id="%s"', id), fixed = TRUE)
  }
  expect_match(intestazione, '<li class="dropdown periodo-intestazione">', fixed = TRUE)
  expect_no_match(laterale, "Periodo di analisi", fixed = TRUE)
  # solo le date: niente elenco dell'anno né casella del periodo personalizzato
  expect_no_match(html, "periodo-anno_filtro", fixed = TRUE)
  expect_no_match(html, "periodo-personalizzato", fixed = TRUE)
})

test_that("chiusa, la sezione del caricamento riporta il file caricato", {
  shiny::testServer(mod_caricamento_server, args = list(demo = FALSE, percorso = NULL), {
    expect_identical(output$riassunto, "")
    session$setInputs(carica_esempio = 1)
    expect_match(output$riassunto, basename(percorso_dataset_esempio()), fixed = TRUE)
  })
})

test_that("l'interfaccia contiene le schede e i controlli richiesti", {
  html <- as.character(app_ui(NULL))
  for (testo in c(
    "Mappa Principale",
    "Ricerca RFID",
    "Ricerca Utenza",
    "Analisi Cluster Spaziale",
    "Caricamento Dati",
    "Periodo di analisi",
    "Stato database",
    "Servizio del contenitore",
    "Giro del calendario",
    "Coerenza",
    "Non censiti con stima incerta (?)",
    "Tutti",
    "Nessuno",
    "filtri-servizio_tutti",
    "filtri-servizio_nessuno",
    "filtri-giro_tutti",
    "filtri-giro_nessuno",
    "filtri-servizio_giro_check",
    "filtri-coerenza_check",
    "filtri-reset",
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

  # i filtri della mappa principale stanno sopra la mappa, in menu a
  # discesa; nella barra laterale restano le statistiche
  barra <- regmatches(
    html,
    regexpr("(?s)<aside[^>]*main-sidebar.*?</aside>", html, perl = TRUE)
  )
  expect_match(barra, "filtri-stat_box", fixed = TRUE)
  expect_no_match(barra, "filtri-cantiere_check", fixed = TRUE)
  expect_no_match(barra, "filtri-servizio_check", fixed = TRUE)
  expect_lt(
    regexpr("barra-filtri", html, fixed = TRUE),
    regexpr("mappa_principale-mappa", html, fixed = TRUE)
  )
  menu <- as.character(mod_filtri_ui("x"))
  expect_equal(
    lengths(regmatches(menu, gregexpr("data-toggle=\"dropdown\"", menu))),
    6
  )
  for (filtro in c(
    "x-cantiere_check",
    "x-comuni",
    "x-coerenza_comune_check",
    "x-presente_filter",
    "x-servizio_check",
    "x-includi_stima_incerta",
    "x-servizio_giro_check",
    "x-coerenza_check",
    "x-reset"
  )) {
    expect_match(menu, filtro, fixed = TRUE)
  }
  # le voci della coerenza hanno parole, non i valori del file
  expect_match(menu, ">Non coerente<", fixed = TRUE)
  expect_match(menu, 'value="non_coerente"', fixed = TRUE)
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

  # nell'intestazione non c'è nessun logo: le sole immagini della pagina sono
  # i simboli della legenda delle mappe
  intestazione_pagina <- regmatches(
    pagina$html,
    regexpr("(?s)<header.*?</header>", pagina$html, perl = TRUE)
  )
  expect_length(intestazione_pagina, 1)
  expect_no_match(intestazione_pagina, "<img", fixed = TRUE)
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
  shiny::testServer(mod_caricamento_server, {
    caricati <- session$getReturned()
    # il formato si riconosce dal nome originale del file
    session$setInputs(
      file_upload = data.frame(
        name = "letture.csv.gz",
        datapath = compresso,
        stringsAsFactors = FALSE
      )
    )
    expect_identical(letture_archivio(caricati()), dati_esempio())
    expect_match(as.character(output$esito$html), "letture.csv.gz", fixed = TRUE)
  })
})

test_that("il file indicato all'avvio è caricato senza passare dal browser", {
  shiny::testServer(
    mod_caricamento_server,
    args = list(percorso = percorso_dataset_esempio()),
    {
      caricati <- session$getReturned()
      expect_identical(letture_archivio(caricati()), dati_esempio())
      # un file letto all'avvio è condiviso tra le sessioni
      expect_true(caricati()$condiviso)
      esito <- as.character(output$esito$html)
      expect_match(esito, "sample_rfid_dataset.csv", fixed = TRUE)
      expect_match(esito, "251</b>\\s*RFID univoci")

      # un file caricato a mano prende il posto di quello dell'avvio
      session$setInputs(
        file_upload = data.frame(
          name = "altre.csv",
          datapath = percorso_dataset_esempio(),
          stringsAsFactors = FALSE
        )
      )
      expect_match(as.character(output$esito$html), "altre.csv", fixed = TRUE)
    }
  )
})

test_that("le opzioni di avvio mancanti o errate non bloccano l'app", {
  # senza opzioni: niente di caricato
  shiny::testServer(mod_caricamento_server, {
    expect_null(session$getReturned()())
  })

  # demo: il dataset di esempio
  shiny::testServer(mod_caricamento_server, args = list(demo = TRUE), {
    expect_equal(session$getReturned()()$n, nrow(dati_esempio()))
  })

  # un percorso che non esiste: l'errore compare al posto dell'esito
  shiny::testServer(
    mod_caricamento_server,
    args = list(
      percorso = file.path(tempdir(), "letture_che_non_esistono.csv")
    ),
    {
      expect_null(session$getReturned()())
      html <- as.character(output$esito$html)
      expect_match(html, "Caricamento non riuscito")
      expect_match(html, "File non trovato")
      expect_match(html, "letture_che_non_esistono.csv", fixed = TRUE)
    }
  )
})

# Angoli del riquadro di alcune letture, come li dà `area_filtri()`.
angoli <- function(letture) {
  dplyr::tibble(
    latitudine = range(letture$latitudine),
    longitudine = range(letture$longitudine)
  )
}

test_that("i filtri geografici restringono cantieri e comuni e danno l'area da inquadrare", {
  tutte <- dati_esempio()
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio()))
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    restituito <- session$getReturned()
    session$flushReact()
    # i filtri geografici compaiono quando il dataset ha dei cantieri
    expect_true(con_geografia())
    expect_identical(scelte_cantieri(), cantieri())
    expect_named(restituito, c("dati_filtrati", "area"))
    expect_named(restituito$area(), c("latitudine", "longitudine"))
    expect_equal(restituito$area(), angoli(tutte))

    # i cantieri si deselezionano: restano le letture di quelli spuntati
    session$setInputs(cantiere_check = c("ASIAGO", "RUBANO"))
    expect_setequal(
      unique(restituito$dati_filtrati()$cantiere),
      c("ASIAGO", "RUBANO")
    )
    expect_equal(
      restituito$area(),
      angoli(tutte[tutte$cantiere %in% c("ASIAGO", "RUBANO"), ])
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
      restituito$area(),
      angoli(tutte[tutte$comune_assegnato %in% "LIMENA", ])
    )

    # più comuni insieme, anche di cantieri diversi
    session$setInputs(comuni = c("LIMENA", "GALLIO"))
    scelti <- restituito$dati_filtrati()
    expect_setequal(unique(scelti$comune_assegnato), c("LIMENA", "GALLIO"))
    expect_setequal(unique(scelti$cantiere), c("RUBANO", "ASIAGO"))
    nei_due_comuni <- angoli(tutte[tutte$comune_assegnato %in% c("LIMENA", "GALLIO"), ])
    expect_equal(restituito$area(), nei_due_comuni)
    statistiche <- as.character(output$stat_box$html)
    expect_match(statistiche, "ASIAGO:", fixed = TRUE)
    expect_match(statistiche, "RUBANO:", fixed = TRUE)

    # l'area segue cantieri e comuni, non gli altri filtri
    session$setInputs(presente_filter = "Non Presente")
    expect_equal(restituito$area(), nei_due_comuni)
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
    expect_equal(nrow(restituito$dati_filtrati()), nrow(ultime_esempio()))
    expect_equal(restituito$area(), angoli(tutte))
  })
})

test_that("le letture senza cantiere hanno una voce tra i cantieri", {
  # Nel dataset di esempio ogni lettura ha un cantiere: in un file può mancare.
  tutte <- dati_esempio()
  vuote <- tutte$RFID == "RFD20241001301"
  tutte$comune_lettura[vuote] <- NA
  tutte$comune_geografico[vuote] <- NA
  tutte$comune_assegnato[vuote] <- NA
  tutte$cantiere[vuote] <- NA
  dati <- shiny::reactiveVal(selezione_di(tutte))
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
    expect_equal(restituito$area(), angoli(tutte[vuote, ]))
  })
})

test_that("le scelte geografiche restano al cambio di periodo e si azzerano con un nuovo dataset", {
  origine <- shiny::reactiveVal(1)
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio(), periodo_anno(2025)))
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
      dati(seleziona_periodo(archivio_esempio(), periodo_anno(2024)))
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
  dati <- shiny::reactiveVal(selezione_di(letture_test()))
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    session$flushReact()
    expect_false(con_geografia())
    expect_length(scelte_comuni(), 0)
    expect_equal(nrow(session$getReturned()$dati_filtrati()), 3)
    expect_no_match(as.character(output$stat_box$html), "Cantiere")
  })
})

test_that("la mappa principale riceve bidoni e bolle in un messaggio solo", {
  registro <- registra_messaggi()
  ultimi <- ultime_esempio()
  dati <- shiny::reactiveVal(ultimi)
  attiva <- shiny::reactiveVal(FALSE)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "cluster", attiva = attiva),
    {
      selezione <- session$getReturned()
      session$flushReact()
      # una mappa nascosta non riceve niente
      expect_length(registro$messaggi, 0)
      attiva(TRUE)
      session$flushReact()
      # prima le bolle, poi i bidoni, poi l'inquadratura
      expect_identical(tipi_messaggi(registro), c("bolle", "dati", "inquadra"))
      attesi <- dati_mappa(ultimi, "cluster")
      expect_identical(registro$messaggi[[1]]$dati, attesi[campi_bolle()])
      expect_identical(registro$messaggi[[2]]$dati, attesi["punti"])
      expect_length(registro$messaggi[[2]]$dati$punti$x, nrow(ultimi))
      expect_identical(sort(registro$messaggi[[1]]$dati$bolle_cantiere$nome), cantieri())
      expect_true(registro$messaggi[[1]]$dati$territorio)
      expect_identical(disegnati(), ultimi)

      # zoom e spostamenti restano nel browser: cambia solo l'intestazione
      expect_match(output$riepilogo, "^251 RFID \\(ultima lettura registrata\\) \u00b7 ")
      expect_match(output$riepilogo, "una bolla per cantiere")
      session$setInputs(mappa_zoom = 11.5)
      expect_match(output$riepilogo, "una bolla per comune")
      session$setInputs(mappa_zoom = 14.2)
      expect_identical(output$riepilogo, "251 RFID (ultima lettura registrata)")
      expect_length(registro$messaggi, 3)
      # con il numero delle letture che superano i filtri, dal modulo dei filtri
      dati(structure(ultimi, letture = 6475L))
      session$flushReact()
      expect_identical(
        output$riepilogo,
        "6.475 letture di 251 RFID (ultima lettura registrata)"
      )
      dati(ultimi)
      session$flushReact()
      registro$messaggi <- registro$messaggi[1:3]

      # il click su un bidone
      session$setInputs(mappa_clic = list(i = 2, nonce = 1))
      expect_identical(selezione()$rfid, ultimi$RFID[3])
      expect_identical(registro$messaggi[[4]]$html, popup_lettura(ultimi, 3))

      # filtri nuovi: bolle, bidoni e inquadratura
      dati(ultimi[1:40, ])
      session$flushReact()
      expect_identical(tipi_messaggi(registro)[5:7], c("bolle", "dati", "inquadra"))
      expect_length(registro$messaggi[[6]]$dati$punti$x, 40)
      expect_equal(sum(registro$messaggi[[5]]$dati$bolle_cantiere$n), 40)

      # mentre la scheda è nascosta gli aggiornamenti aspettano
      attiva(FALSE)
      dati(ultimi[1:10, ])
      session$flushReact()
      expect_length(registro$messaggi, 7)
      attiva(TRUE)
      session$flushReact()
      expect_length(registro$messaggi[[9]]$dati$punti$x, 10)

      # nessun bidone: una mappa vuota, senza bolle e senza inquadratura
      dati(ultimi[0, ])
      session$flushReact()
      ultimi_due <- utils::tail(registro$messaggi, 2)
      expect_identical(tipi_messaggi(list(messaggi = ultimi_due)), c("bolle", "dati"))
      expect_length(ultimi_due[[1]]$dati$bolle_cantiere$n, 0)
      expect_false(ultimi_due[[1]]$dati$territorio)
      expect_length(ultimi_due[[2]]$dati$punti$x, 0)
      expect_identical(output$riepilogo, "")
    }
  )
})

test_that("la ricerca RFID disegna al massimo le letture previste dal limite", {
  withr::local_options(trackerfid.max_letture_ricerca = 5)
  registro <- registra_messaggi()
  trovate <- cerca_per_rfid(seleziona_periodo(archivio_esempio()), "RFD20250901001")
  dati <- shiny::reactiveVal(trovate)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "rfid"),
    {
      selezione <- session$getReturned()
      session$flushReact()
      expect_equal(nrow(disegnati()), 5)
      expect_identical(attr(disegnati(), "totale"), nrow(trovate))
      # i riquadri coprono tutte le letture trovate
      expect_identical(registro$messaggi[[1]]$dati$riquadri, riquadri_rfid(trovate))
      expect_match(
        output$riepilogo,
        sprintf("%d letture di 1 RFID", nrow(trovate))
      )
      expect_match(output$riepilogo, "sulla mappa 5: le più recenti")
      # la prima riga disegnata è l'ultima lettura dell'RFID
      expect_true(disegnati()$is_ultimo[1])
      session$setInputs(mappa_clic = list(i = 0, nonce = 1))
      expect_identical(selezione()$rfid, "RFD20250901001")
    }
  )
})

test_that("la ricerca RFID disegna le ultime 10 letture di ogni RFID", {
  registro <- registra_messaggi()
  trovate <- cerca_per_rfid(
    seleziona_periodo(archivio_esempio()),
    c("RFD20241001301", "RFD20250905100")
  )
  dati <- shiny::reactiveVal(trovate)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "rfid"),
    {
      session$flushReact()
      deposito <- sum(trovate$RFID == "RFD20241001301")
      spostato <- sum(trovate$RFID == "RFD20250905100")
      expect_gt(deposito, 10)
      expect_equal(nrow(disegnati()), 10 + min(spostato, 10))
      expect_identical(attr(disegnati(), "totale"), nrow(trovate))
      expect_length(registro$messaggi[[1]]$dati$punti$x, nrow(disegnati()))
      # l'area di spostamento e l'inquadratura comprendono tutte le letture
      expect_identical(registro$messaggi[[1]]$dati$riquadri, riquadri_rfid(trovate))
      expect_match(
        output$riepilogo,
        sprintf(
          "%d letture di 2 RFID \u00b7 sulla mappa %d: le ultime 10 di ogni RFID",
          nrow(trovate),
          nrow(disegnati())
        ),
        fixed = TRUE
      )
      # con poche letture la mappa le disegna tutte, e non lo dice
      dati(trovate[trovate$RFID == "RFD20250905100", ])
      session$flushReact()
      expect_no_match(output$riepilogo, "sulla mappa", fixed = TRUE)
    }
  )
})

test_that("l'interfaccia ha i filtri geografici e le sole quattro schede previste", {
  html <- as.character(app_ui(NULL))
  for (testo in c(
    "Cantiere",
    "Coerenza comune",
    "Coerenza servizio",
    "Tutti i comuni",
    "CSV, CSV.GZ, Parquet o DuckDB"
  )) {
    expect_match(html, testo, fixed = TRUE)
  }
  # l'elenco dei comuni permette più scelte; senza scelte vale per tutti
  expect_match(html, '<select[^>]*id="filtri-comuni"[^>]*multiple')
  expect_match(html, '"placeholder":"Tutti i comuni"', fixed = TRUE)
  # un solo campo di caricamento, che accetta anche l'archivio DuckDB
  expect_match(html, 'accept=".csv,.txt,.gz,.parquet,.duckdb', fixed = TRUE)
  expect_length(gregexpr('type="file"', html, fixed = TRUE)[[1]], 1)
  expect_no_match(html, "storic", fixed = TRUE)
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
  dati <- shiny::reactiveVal(selezione_di(grezzi))
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    # il sacchetto censito è letto da un giro del secco, ma la sua voce è
    # quella dei sacchetti. Il censito della carta sta sotto il servizio del
    # suo contenitore, non sotto il giro che lo ha letto
    expect_identical(
      scelte_servizio(),
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
      servizio_check = c("SECCO PAP", "CARTA")
    )
    expect_setequal(filtrati()$RFID, c("0000ABC123", "0000ABC124"))
    # la sola voce dei sacchetti
    session$setInputs(servizio_check = servizio_sacchetti())
    expect_setequal(filtrati()$RFID, c(sacchetto(1), sacchetto(2)))
    # con lo stato a database: il solo sacchetto non censito
    session$setInputs(presente_filter = "Non Presente")
    expect_identical(filtrati()$RFID, sacchetto(2))
    session$setInputs(presente_filter = c("Presente", "Non Presente"))
    # "Nessuno" toglie anche i sacchetti, "Tutti" li rimette
    session$setInputs(servizio_nessuno = 1)
    expect_equal(nrow(filtrati()), 0)
    session$setInputs(servizio_tutti = 1)
    expect_equal(nrow(filtrati()), 4)
  })
})

test_that("il filtro sulla coerenza del comune restringe la mappa alle voci spuntate", {
  selezione <- seleziona_periodo(archivio_esempio(), periodo_anno(2025))
  dati <- shiny::reactiveVal(selezione)
  letture <- letture_periodo(selezione)
  shiny::testServer(mod_filtri_server, args = list(dati = dati), {
    filtrati <- function() session$getReturned()$dati_filtrati()
    session$flushReact()
    tutti <- nrow(filtrati())
    expect_equal(tutti, 248)
    # con l'ultima lettura di ogni RFID arriva il numero di tutte le letture
    # che superano i filtri, per l'intestazione della mappa
    expect_identical(attr(filtrati(), "letture"), nrow(letture))

    # solo le letture fuori comune: il contenitore spostato, il tag sul mezzo
    # e quattro censiti letti appena oltre il confine del loro comune
    session$setInputs(coerenza_comune_check = "fuori_comune")
    expect_setequal(
      filtrati()$RFID,
      c(
        "RFD20250905100",
        "RFD20250203302",
        "RFD20241007067",
        "RFD20241017188",
        "RFD20241030027",
        "RFD20241002198"
      )
    )
    expect_true(all(filtrati()$coerenza_comune == "Fuori comune"))
    expect_match(
      as.character(output$stat_box$html),
      "Fuori comune:</span>\\s*<b>6</b>"
    )

    # solo fuori giro: la mappa mostra l'ultima lettura fuori giro di ogni RFID
    session$setInputs(coerenza_comune_check = "fuori_giro")
    expect_true(all(filtrati()$coerenza_comune == "Fuori giro"))
    expect_equal(
      nrow(filtrati()),
      dplyr::n_distinct(letture$RFID[letture$coerenza_comune == "Fuori giro"])
    )
    expect_equal(nrow(filtrati()), 36)
    expect_identical(attr(filtrati(), "letture"), sum(letture$coerenza_comune == "Fuori giro"))
    expect_true("RFD20241001301" %in% filtrati()$RFID)
    # un censito non è mai fuori giro
    expect_true(all(filtrati()$presente_a_database == "Non Presente"))

    # solo la coerenza ignota: i sacchetti censiti, senza comune a database
    session$setInputs(coerenza_comune_check = "ignota")
    expect_true(all(rfid_sacchetto(filtrati()$RFID)))
    expect_equal(nrow(filtrati()), 7)
    expect_match(
      as.character(output$stat_box$html),
      "Coerenza ignota:</span>\\s*<b>7</b>"
    )

    # solo le letture coerenti
    session$setInputs(coerenza_comune_check = "coerente")
    expect_true(all(filtrati()$coerenza_comune == "Coerente"))
    # nessuna voce: nessun bidone
    session$setInputs(coerenza_comune_check = NULL)
    expect_equal(nrow(filtrati()), 0)
    # il reset rimette tutte le voci
    session$setInputs(reset = 1)
    expect_equal(nrow(filtrati()), tutti)
  })
})

test_that("la ricerca dei giri parte dal giorno pi\u00f9 recente e segue mezzo, giorni e filtri", {
  dati <- shiny::reactiveVal(seleziona_periodo(archivio_esempio(), periodo_anno(2025)))
  shiny::testServer(mod_giro_server, args = list(dati = dati), {
    restituito <- session$getReturned()
    session$flushReact()
    # all'avvio: tutti i mezzi, ultimo giorno con letture
    expect_identical(stato$mezzo, "")
    expect_identical(stato$dal, as.Date("2025-12-31"))
    expect_identical(stato$al, as.Date("2025-12-31"))
    all_avvio <- trovate()
    expect_true(all(all_avvio$giorno == as.Date("2025-12-31")))
    expect_gt(nrow(all_avvio), 0)
    # senza un giro scelto la mappa non riceve letture: inquadra la zona dei
    # giri trovati e invita a sceglierne uno
    expect_null(restituito$risultati())
    expect_match(restituito$messaggio(), "Scegli un giro nella tabella")
    expect_identical(restituito$vista(), area_giri(all_avvio))

    # un mezzo, un giorno: il giro sbagliato
    session$setInputs(
      mezzo = "VEH001",
      giorni = as.Date(c("2025-09-24", "2025-09-24"))
    )
    expect_equal(nrow(trovate()), 10)
    expect_true(all(trovate()$matricola_veicolo == "VEH001"))
    giri <- restituito$riepilogo()
    expect_equal(nrow(giri), 1)
    expect_identical(giri$comune_giro, "LIMENA")
    # fuori giro c'è solo il non censito: i censiti sono nel loro comune
    expect_identical(giri$fuori_giro, 1L)
    expect_match(as.character(output$esito$html), "1 giro \u00b7 10 letture")
    expect_match(as.character(output$esito$html), "1 giro con letture fuori giro")

    # un intervallo di giorni, tutti i mezzi, solo le letture fuori giro
    session$setInputs(
      mezzo = "",
      giorni = as.Date(c("2025-09-22", "2025-09-28")),
      solo = "fuori_giro"
    )
    settimana <- trovate()
    expect_true(all(settimana$coerenza_comune == "Fuori giro"))
    expect_gt(dplyr::n_distinct(settimana$matricola_veicolo), 1)
    expect_true(all(
      settimana$giorno >= as.Date("2025-09-22") &
        settimana$giorno <= as.Date("2025-09-28")
    ))
    giri <- restituito$riepilogo()
    expect_true(all(giri$fuori_giro > 0))

    # un click su una riga della tabella: le sole letture di quel giro
    riga <- which(
      giri$matricola_veicolo == "VEH001" &
        giri$giorno == as.Date("2025-09-24")
    )
    expect_identical(giri$comune_giro[riga], "LIMENA")
    session$setInputs(tabella__reactable__selected = riga)
    expect_equal(nrow(restituito$risultati()), 1)
    expect_true(all(restituito$risultati()$comune_giro == "LIMENA"))
    expect_true(all(restituito$risultati()$comune_lettura == "BORGORICCO"))
    # la mappa inquadra il giro scelto, con il suo comune, e non ha avvisi
    expect_identical(restituito$vista(), area_giri(restituito$risultati()))
    expect_null(restituito$messaggio())
    # tolta la selezione la mappa resta senza letture
    session$setInputs(tabella__reactable__selected = NULL)
    expect_null(restituito$risultati())
    # una riga che non esiste non sceglie niente
    session$setInputs(tabella__reactable__selected = 9999)
    expect_null(restituito$risultati())

    # cambiare la ricerca toglie il giro scelto
    session$setInputs(tabella__reactable__selected = riga)
    expect_equal(nrow(restituito$risultati()), 1)
    session$setInputs(solo = NULL)
    expect_null(restituito$risultati())
    expect_gt(nrow(trovate()), nrow(settimana))

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
    expect_equal(nrow(trovate()), 0)
    expect_null(restituito$risultati())
    expect_match(restituito$messaggio(), "Nessuna lettura")

    # un nuovo periodo riporta la ricerca ai valori predefiniti
    dati(seleziona_periodo(archivio_esempio(), periodo_anno(2024)))
    session$flushReact()
    expect_identical(stato$mezzo, "")
    expect_identical(stato$solo, character(0))
    expect_identical(stato$dal, as.Date("2024-12-31"))
    expect_true(all(trovate()$giorno == as.Date("2024-12-31")))

    # senza letture non c'\u00e8 niente da cercare
    dati(NULL)
    session$flushReact()
    expect_null(restituito$risultati())
    expect_equal(nrow(restituito$riepilogo()), 0)
  })
})

test_that("la mappa dei giri disegna le letture trovate, anche ripetute", {
  registro <- registra_messaggi()
  letture <- cerca_giri(seleziona_periodo(archivio_esempio()), "VEH001", "2025-09-22", "2025-09-28")
  dati <- shiny::reactiveVal(letture)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "giro", dati_vista = reactive(area_giri(dati()))),
    {
      selezione <- session$getReturned()
      session$flushReact()
      # tutte le letture, con il contorno dei comuni dei giri
      expect_equal(nrow(disegnati()), nrow(letture))
      inviati <- registro$messaggi[[1]]$dati
      expect_length(inviati$punti$x, nrow(letture))
      expect_identical(inviati$comuni_giro, contorni_comuni_giro(letture$comune_giro))
      # la vista comprende i comuni dei giri
      expect_identical(registro$messaggi[[2]]$riquadro, riquadro_vista(area_giri(letture)))
      expect_match(
        output$riepilogo,
        sprintf("^%d letture di %d bidoni", nrow(letture), dplyr::n_distinct(letture$RFID))
      )
      fuori_giro <- sum(letture$coerenza_comune == "Fuori giro")
      expect_match(
        output$riepilogo,
        sprintf("%d fuori giro", fuori_giro),
        fixed = TRUE
      )
      expect_equal(fuori_giro, 6)
      session$setInputs(mappa_clic = list(i = 3, nonce = 1))
      expect_identical(selezione()$rfid, letture$RFID[4])
    }
  )
  # oltre il limite la mappa tiene per prime le letture con un'incongruenza
  withr::local_options(trackerfid.max_letture_ricerca = 12)
  shiny::testServer(
    mod_mappa_server,
    args = list(dati = dati, modalita = "giro"),
    {
      session$flushReact()
      expect_equal(nrow(disegnati()), 12)
      expect_equal(sum(disegnati()$coerenza_comune == "Fuori giro"), 6)
      expect_match(output$riepilogo, "sulla mappa 12 letture")
    }
  )
})

test_that("l'interfaccia ha la scheda dei giri, il suo pannello e il filtro sulla coerenza del comune", {
  html <- as.character(app_ui(NULL))
  expect_match(html, "Ricerca Giro", fixed = TRUE)
  expect_match(html, 'id="giro-mezzo"', fixed = TRUE)
  expect_match(html, 'id="giro-giorni"', fixed = TRUE)
  expect_match(html, 'id="giro-solo"', fixed = TRUE)
  expect_match(html, 'id="giro-tabella"', fixed = TRUE)
  expect_match(html, 'id="mappa_giro-mappa"', fixed = TRUE)
  # le tabelle delle due ricerche stanno sopra la mappa, prima della legenda,
  # in un riquadro aperto
  for (coppia in list(c("mappa_giro-mappa", "giro-tabella"), c("mappa_rfid-mappa", "ricerca_rfid-letture"))) {
    mappa <- regexpr(sprintf('id="%s"', coppia[1]), html, fixed = TRUE)
    tabella <- regexpr(sprintf('id="%s"', coppia[2]), html, fixed = TRUE)
    legenda <- regexpr('class="legenda-mappa"', substring(html, mappa), fixed = TRUE) + mappa
    expect_gt(tabella, mappa)
    expect_lt(tabella, legenda)
  }
  expect_match(html, '<div class="tabella-mappa tabella-giri">', fixed = TRUE)
  expect_match(html, '<div class="tabella-mappa tabella-letture">', fixed = TRUE)
  expect_match(html, 'data-display-if="output.con_letture" data-ns-prefix="ricerca_rfid-"', fixed = TRUE)
  expect_length(gregexpr('<details open class="tabella-dettagli">', html, fixed = TRUE)[[1]], 2)
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
  # il filtro sulla coerenza del comune, con le quattro voci spuntate
  expect_match(html, 'id="filtri-coerenza_comune_check"', fixed = TRUE)
  for (voce in names(stati_coerenza_comune())) {
    expect_match(
      html,
      sprintf('name="filtri-coerenza_comune_check" value="%s" checked', voce),
      fixed = TRUE
    )
  }
})
