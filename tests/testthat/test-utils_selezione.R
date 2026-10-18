# Filtri, ricerche e giri sulle selezioni hanno i loro test accanto alle regole
# che applicano: test-utils_data_processing.R e test-utils_giri.R. Qui ci sono
# la selezione del periodo e le interrogazioni che servono al resto dell'app.

test_that("la selezione di un periodo conta le sue letture e i suoi RFID", {
  esempio <- dati_esempio()
  tutte <- seleziona_periodo(archivio_esempio())
  expect_s3_class(tutte, "selezione_letture")
  expect_null(tutte$periodo)
  expect_identical(tutte$n, nrow(esempio))
  expect_identical(tutte$n_rfid, 251L)
  expect_identical(sql_periodo(tutte), "TRUE")

  anno <- seleziona_periodo(archivio_esempio(), periodo_anno(2025))
  nel_2025 <- filtra_periodo(esempio, periodo_anno(2025))
  expect_identical(anno$periodo, periodo_anno(2025))
  expect_identical(anno$n, nrow(nel_2025))
  expect_identical(anno$n_rfid, 248L)
  expect_identical(anno$stima, "stima_20250101_20251231")
  # la tabella della stima si calcola una volta: una seconda selezione la usa
  expect_identical(seleziona_periodo(archivio_esempio(), periodo_anno(2025))$stima, anno$stima)
})

test_that("le letture del periodo sono quelle del filtro in R, nell'ordine del file", {
  for (anno in c(2024, 2025)) {
    selezione <- seleziona_periodo(archivio_esempio(), periodo_anno(anno))
    expect_identical(
      letture_periodo(selezione),
      filtra_periodo(dati_esempio(), periodo_anno(anno))
    )
  }
  expect_identical(letture_periodo(seleziona_periodo(archivio_esempio())), dati_esempio())
})

test_that("la condizione SQL sui sacchetti è quella di rfid_sacchetto", {
  codici <- c(
    "00BD00000000000000000005",
    "00bd00000000000000000005",
    "00BD0000000000000000000",
    "00BD000000000000000000051",
    "XXBD00000000000000000005",
    "0000ABC123",
    ""
  )
  con <- archivio_esempio()$con
  esito <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT %s AS sacchetto FROM (SELECT unnest([%s]) AS RFID)",
      sql_sacchetto(),
      sql_elenco(con, codici)
    )
  )$sacchetto
  expect_identical(esito, rfid_sacchetto(codici))
})

test_that("la stima dei non censiti in SQL è quella della regola in R", {
  confronta <- function(periodo, soglia) {
    selezione <- seleziona_periodo(archivio_esempio(), periodo)
    tabella <- tabella_stima(selezione, soglia)
    if (soglia != 0.8) {
      # una soglia diversa vuole una tabella sua
      DBI::dbExecute(selezione$archivio$con, paste("DROP TABLE", tabella))
      tabella <- tabella_stima(selezione, soglia)
      withr::defer(DBI::dbExecute(selezione$archivio$con, paste("DROP TABLE", tabella)))
    }
    stima <- interroga(selezione$archivio, paste("SELECT * FROM", tabella, "ORDER BY RFID"))
    letture <- if (is.null(periodo)) dati_esempio() else filtra_periodo(dati_esempio(), periodo)
    attesa <- servizio_prevalente_non_censiti(letture, soglia)
    attesa <- attesa[order(attesa$RFID), ]
    expect_identical(stima$RFID, attesa$RFID)
    expect_identical(stima$stimato, attesa$servizio_prevalente)
  }
  confronta(NULL, 0.8)
  confronta(periodo_anno(2025), 0.8)
  confronta(periodo_anno(2024), 0.8)
  confronta(periodo_anno(2025), 0.5)

  # 2025: 18 non censiti con il servizio stimato e 19 con la stima incerta,
  # più i sacchetti non censiti, letti da un giro solo
  selezione <- seleziona_periodo(archivio_esempio(), periodo_anno(2025))
  stima <- interroga(selezione$archivio, paste("SELECT * FROM", selezione$stima))
  bidoni <- stima[!rfid_sacchetto(stima$RFID), ]
  expect_equal(sum(!is.na(bidoni$stimato)), 18)
  expect_equal(sum(is.na(bidoni$stimato)), 19)
})

test_that("il servizio dell'icona in SQL è quello di aggiungi_servizio_icona", {
  for (periodo in list(NULL, periodo_anno(2025))) {
    selezione <- seleziona_periodo(archivio_esempio(), periodo)
    sql <- interroga(
      selezione$archivio,
      sprintf("SELECT riga, servizio_icona FROM %s ORDER BY riga", sql_con_icona(selezione))
    )
    letture <- if (is.null(periodo)) dati_esempio() else filtra_periodo(dati_esempio(), periodo)
    expect_identical(sql$servizio_icona, aggiungi_servizio_icona(letture)$servizio_icona)
  }
})

test_that("i filtri senza scelte non escludono niente", {
  con <- archivio_esempio()$con
  expect_identical(
    condizione_filtri(con),
    "(presente_a_database IN ('Presente', 'Non Presente'))"
  )
  # nessuno stato scelto: nessuna lettura
  expect_match(condizione_filtri(con, stato_db = character(0)), "(FALSE)", fixed = TRUE)
  # un nome con l'apostrofo resta un testo
  ultime <- ultime_esempio(comuni = "ROSA' D'ORO")
  expect_equal(nrow(ultime), 0)
})

test_that("l'ultima lettura restituisce le colonne chieste, con la riga del file", {
  ultime <- ultime_esempio(colonne = c("RFID", "latitudine"))
  expect_named(ultime, c("RFID", "latitudine", "riga"))
  expect_equal(nrow(ultime), 251)
  tutte <- ultime_esempio()
  expect_named(tutte, c(colonne_dataset(), "servizio_icona", "riga"))
  # la riga riporta alla lettura del file
  expect_identical(dati_esempio()$RFID[tutte$riga], tutte$RFID)
  expect_equal(dati_esempio()$giorno_lettura[tutte$riga], tutte$giorno_lettura)
})

test_that("le scelte dei filtri sono i valori presenti nel periodo", {
  periodo <- periodo_anno(2025)
  selezione <- seleziona_periodo(archivio_esempio(), periodo)
  scelte <- scelte_periodo(selezione)
  letture <- aggiungi_servizio_icona(filtra_periodo(dati_esempio(), periodo))
  expect_identical(scelte$servizi, ordina_servizi(unique(letture$servizio_icona)))
  expect_identical(scelte$giri, ordina_servizi(unique(letture$servizio_giro)))
  expect_identical(scelte$cantieri, cantieri())
  expect_true(scelte$con_geografia)
  expect_setequal(scelte$comuni$comune_assegnato, unique(letture$comune_assegnato))
  expect_named(scelte$comuni, c("cantiere", "comune_assegnato"))

  # un file senza geografia
  senza <- scelte_periodo(selezione_di(letture_test()))
  expect_false(senza$con_geografia)
  expect_equal(nrow(senza$comuni), 0)
  expect_identical(senza$servizi, ordina_servizi(c("CARTA", "VETRO", "SECCO")))
})

test_that("l'area da inquadrare è il riquadro dei cantieri e dei comuni scelti", {
  selezione <- seleziona_periodo(archivio_esempio())
  esempio <- dati_esempio()
  riquadro <- function(righe) {
    dplyr::tibble(
      latitudine = range(esempio$latitudine[righe]),
      longitudine = range(esempio$longitudine[righe])
    )
  }
  expect_equal(area_filtri(selezione), riquadro(TRUE))
  expect_equal(
    area_filtri(selezione, comuni = c("LIMENA", "GALLIO")),
    riquadro(esempio$comune_assegnato %in% c("LIMENA", "GALLIO"))
  )
  expect_equal(
    area_filtri(selezione, cantieri_esclusi = c("ASIAGO", "BASSANO", "CAMPOSAMPIERO")),
    riquadro(esempio$cantiere %in% "RUBANO")
  )
  vuota <- area_filtri(selezione, comuni = "COMUNE INESISTENTE")
  expect_named(vuota, c("latitudine", "longitudine"))
  expect_equal(nrow(vuota), 0)
})

test_that("le letture di un RFID vengono dal periodo o da tutto l'archivio", {
  rfid <- "RFD20250901001"
  esempio <- dati_esempio()
  tutte <- letture_del_rfid(archivio_esempio(), rfid)
  attese <- esempio[esempio$RFID == rfid, ]
  attese <- attese[order(attese$giorno_lettura), ]
  expect_identical(tutte, attese)

  anno <- letture_del_rfid(seleziona_periodo(archivio_esempio(), periodo_anno(2025)), rfid)
  expect_identical(anno, filtra_periodo(attese, periodo_anno(2025)))
  expect_equal(nrow(letture_del_rfid(archivio_esempio(), "NESSUNO'")), 0)
})

test_that("gli anni disponibili vanno dal più recente", {
  expect_identical(anni_disponibili(archivio_esempio()), c(2025, 2024))
  expect_identical(anni_disponibili(NULL), numeric(0))
  vuoto <- archivio_da_letture(letture_test()[0, ])
  withr::defer(chiudi_archivio(vuoto))
  expect_identical(anni_disponibili(vuoto), numeric(0))
})

test_that("le letture che superano i filtri si contano tutte, non solo le ultime", {
  selezione <- seleziona_periodo(archivio_esempio(), periodo_anno(2025))
  letture <- letture_periodo(selezione)
  expect_identical(conta_letture(selezione), nrow(letture))
  expect_identical(
    conta_letture(selezione, stato_db = "Non Presente"),
    sum(letture$presente_a_database == "Non Presente")
  )
  # gli stessi filtri di conteggi_cantieri()
  expect_identical(
    conta_letture(selezione, cantieri_esclusi = "ASIAGO"),
    sum(conteggi_cantieri(selezione, cantieri_esclusi = "ASIAGO"))
  )
})
