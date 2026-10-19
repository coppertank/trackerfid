# La validazione delle letture, che vive in `valida_archivio()`, ha i suoi test
# in test-utils_data_processing.R, insieme a quelli sul formato del file: qui
# ci sono l'archivio, la sua connessione e il file .duckdb.

test_that("il database di un archivio conosce la tipologia di ogni servizio", {
  con <- connessione_duckdb()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  tipologie <- DBI::dbGetQuery(con, "SELECT servizio, tipologia FROM servizi_tipologie")
  expect_false(anyDuplicated(tipologie$servizio) > 0)
  expect_identical(
    tipologie$tipologia,
    unname(tipologia_servizio(tipologie$servizio))
  )
  expect_true(all(c("SECCO PAP", "SECCO", "SERVIZI MERCATI") %in% tipologie$servizio))
})

test_that("le colonne dell'archivio sono quelle del dataset più la riga del file", {
  expect_identical(colonne_archivio(), c(colonne_app(), "riga"))
  expect_identical(colonne_archivio("tutte"), c(colonne_dataset(), "riga"))
  expect_identical(names(tipi_archivio("tutte")), colonne_archivio("tutte"))
  tipi <- tipi_archivio()
  expect_identical(names(tipi), colonne_archivio())
  expect_identical(tipi[["giorno_lettura"]], "TIMESTAMP")
  expect_identical(tipi[["riga"]], "BIGINT")
  expect_true(all(tipi[c("latitudine", "longitudine", colonne_quantita())] == "DOUBLE"))
  expect_identical(tipi[["RFID"]], "VARCHAR")
})

test_that("l'archivio riassume le letture che contiene", {
  esempio <- dati_esempio()
  archivio <- archivio_esempio()
  expect_s3_class(archivio, "archivio_letture")
  expect_identical(archivio$n, nrow(esempio))
  expect_identical(archivio$n_rfid, dplyr::n_distinct(esempio$RFID))
  expect_equal(archivio$primo, min(esempio$giorno_lettura))
  expect_equal(archivio$ultimo, max(esempio$giorno_lettura))
  expect_identical(archivio$origine, basename(percorso_dataset_esempio()))
  expect_false(archivio$condiviso)
})

test_that("le letture si estraggono tutte o con una condizione, nell'ordine del file", {
  archivio <- archivio_esempio()
  expect_identical(letture_archivio(archivio), dati_esempio())
  limena <- letture_archivio(archivio, "comune_lettura = 'LIMENA'")
  attese <- dati_esempio()[dati_esempio()$comune_lettura %in% "LIMENA", ]
  expect_identical(limena, attese)
  expect_equal(nrow(letture_archivio(archivio, "FALSE")), 0)
  expect_named(letture_archivio(archivio, "FALSE"), colonne_app())
  expect_identical(archivio$colonne, colonne_app())
})

test_that("le letture di un data frame vanno in un archivio con tutte le colonne", {
  letture <- letture_test()
  archivio <- archivio_da_letture(letture, origine = "prova")
  withr::defer(chiudi_archivio(archivio))
  expect_identical(archivio$origine, "prova")
  expect_identical(archivio$n, nrow(letture))
  estratte <- letture_archivio(archivio)
  expect_named(estratte, colonne_app())
  expect_identical(estratte$RFID, letture$RFID)
  expect_equal(estratte$giorno_lettura, letture$giorno_lettura)
  # le colonne che mancano sono vuote, con il loro tipo
  expect_true(all(is.na(estratte$comune_giro)))
  expect_type(estratte$volume_previsto, "double")
  # senza il giro, il giro è il servizio atteso, come nella validazione
  expect_identical(estratte$servizio_giro, letture$servizio_atteso)
  # la riga è la posizione nel data frame
  righe <- DBI::dbGetQuery(archivio$con, "SELECT riga FROM letture ORDER BY riga")$riga
  expect_equal(righe, seq_len(nrow(letture)))
})

test_that("chiudere un archivio chiude la connessione, non quella di uno condiviso", {
  archivio <- archivio_da_letture(letture_test())
  con <- archivio$con
  chiudi_archivio(archivio)
  expect_false(DBI::dbIsValid(con))
  # una seconda chiusura non fa niente
  expect_null(chiudi_archivio(archivio))
  expect_null(chiudi_archivio(NULL))

  condiviso <- archivio_da_letture(letture_test())
  condiviso$condiviso <- TRUE
  chiudi_archivio(condiviso)
  expect_true(DBI::dbIsValid(condiviso$con))
  condiviso$condiviso <- FALSE
  chiudi_archivio(condiviso)
  expect_false(DBI::dbIsValid(condiviso$con))
})

test_that("i testi delle query sono tra apici, anche con un apostrofo", {
  con <- archivio_esempio()$con
  expect_identical(sql_elenco(con, c("ROSA'", "LIMENA")), "'ROSA''', 'LIMENA'")
  valori <- DBI::dbGetQuery(
    con,
    sprintf("SELECT unnest([%s]) AS v", sql_elenco(con, c("ROSA'", "LIMENA")))
  )$v
  expect_identical(valori, c("ROSA'", "LIMENA"))
})

test_that("la pulizia SQL del testo fa come str_squish, e in un CSV NA è mancante", {
  con <- connessione_duckdb()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  valori <- c("  a  b ", "a\u00a0\u00a0b", "", "   ", "NA", "x\ty\n", "ROS\u00c0", NA)
  DBI::dbWriteTable(con, "t", data.frame(v = valori, stringsAsFactors = FALSE))
  pulite <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT %s AS csv, %s AS parquet FROM t",
      sql_pulito("v"),
      sql_pulito("v", csv = FALSE)
    )
  )
  attese <- stringr::str_squish(valori)
  attese[!is.na(attese) & !nzchar(attese)] <- NA
  expect_identical(pulite$parquet, attese)
  attese[attese %in% "NA"] <- NA
  expect_identical(pulite$csv, attese)
})

test_that("i formati di data e ora sono quelli della conversione in R", {
  esempi <- c(
    "2025-09-01 08:30:15",
    "2025-09-01T08:30:15",
    "2025-09-01 08:30",
    "01/09/2025 08:30:15",
    "01-09-2025 08:30",
    "01.09.2025 08:30:15",
    "2025-09-01",
    "01/09/2025"
  )
  con <- connessione_duckdb()
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))
  convertite <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT coalesce(%s) AS t FROM (SELECT unnest([%s]) AS v)",
      paste(
        sprintf("try_strptime(v, %s)", DBI::dbQuoteString(con, formati_data_ora())),
        collapse = ", "
      ),
      sql_elenco(con, esempi)
    )
  )$t
  expect_equal(
    as.numeric(convertite),
    as.numeric(converti_data_ora(esempi))
  )
})

test_that("un archivio scritto in un file .duckdb si riapre uguale, con gli avvisi", {
  # una riga senza RFID: la validazione la scarta con un avviso
  csv <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-01 08:00:00,AB123CD,1601,A,Presente,CARTA,U1,41.9,12.4",
    "2025-09-02 08:00:00,AB123CD,1601,,Presente,CARTA,U1,41.9,12.4",
    "2025-09-03 08:00:00,AB123CD,1601,B,Non Presente,SECCO,,41.8,12.3"
  ))
  dal_csv <- carica_archivio(csv)
  withr::defer(chiudi_archivio(dal_csv$archivio))
  expect_gte(length(dal_csv$avvisi), 1)

  cartella <- withr::local_tempdir()
  percorso <- file.path(cartella, "letture.duckdb")
  expect_identical(scrivi_archivio(csv, percorso), percorso)
  aperto <- carica_archivio(percorso)
  withr::defer(chiudi_archivio(aperto$archivio))
  expect_identical(aperto$archivio$tabella, "archivio_file.letture")
  expect_identical(aperto$archivio$origine, "letture.duckdb")
  expect_identical(aperto$avvisi, dal_csv$avvisi)
  expect_identical(letture_archivio(aperto$archivio), letture_archivio(dal_csv$archivio))
  # le informazioni sul file: versione e origine
  informazioni <- DBI::dbGetQuery(
    aperto$archivio$con,
    "SELECT chiave, valore FROM archivio_file.informazioni"
  )
  expect_identical(
    informazioni$valore[informazioni$chiave == "versione"],
    as.character(versione_archivio())
  )
  expect_identical(
    informazioni$valore[informazioni$chiave == "origine"],
    basename(csv)
  )
  # il file è aperto in sola lettura
  expect_error(
    DBI::dbExecute(aperto$archivio$con, "DELETE FROM archivio_file.letture")
  )
})

test_that("il file .duckdb del dataset di esempio dà gli stessi risultati del CSV", {
  cartella <- withr::local_tempdir()
  percorso <- file.path(cartella, "esempio.duckdb")
  scrivi_archivio(archivio_esempio(), percorso)
  aperto <- carica_archivio(percorso)
  withr::defer(chiudi_archivio(aperto$archivio))
  campi <- c("n", "n_rfid", "primo", "ultimo")
  expect_identical(aperto$archivio[campi], archivio_esempio()[campi])
  expect_identical(aperto$avvisi, character(0))
  periodo <- periodo_anno(2025)
  expect_identical(
    ultime_letture(seleziona_periodo(aperto$archivio, periodo)),
    ultime_esempio(periodo = periodo)
  )
  # un file con lo stesso nome viene sostituito
  scrivi_archivio(archivio_da_letture(letture_test()), percorso)
  sostituito <- carica_archivio(percorso)
  withr::defer(chiudi_archivio(sostituito$archivio))
  expect_identical(sostituito$archivio$n, nrow(letture_test()))
})

test_that("l'app tiene le sue colonne, e su richiesta tutte quelle della pipeline", {
  esempio <- percorso_dataset_esempio()
  app <- carica_dataset(esempio)$dati
  tutte <- carica_dataset(esempio, colonne = "tutte")$dati
  expect_named(app, colonne_app())
  expect_named(tutte, colonne_dataset())
  expect_identical(tutte[colonne_app()], app)
  # il file .duckdb tiene le colonne con cui è scritto, e l'app lo apre
  cartella <- withr::local_tempdir()
  percorso <- file.path(cartella, "tutte.duckdb")
  scrivi_archivio(esempio, percorso, colonne = "tutte")
  aperto <- carica_archivio(percorso)
  withr::defer(chiudi_archivio(aperto$archivio))
  expect_identical(aperto$archivio$colonne, colonne_dataset())
  expect_identical(letture_archivio(aperto$archivio), tutte)
  expect_identical(
    ultime_letture(seleziona_periodo(aperto$archivio)),
    ultime_esempio()
  )
  expect_error(carica_archivio(esempio, colonne = "alcune"), "should be one of")
})

test_that("un file .duckdb che non è un archivio valido ferma il caricamento", {
  cartella <- withr::local_tempdir()
  expect_error(
    carica_archivio(file.path(cartella, "manca.duckdb")),
    "File non trovato"
  )

  # un file qualsiasi chiamato .duckdb
  finto <- file.path(cartella, "finto.duckdb")
  writeLines("non sono un database", finto)
  expect_error(carica_archivio(finto), "Impossibile aprire il file come archivio DuckDB")

  # un database DuckDB senza le tabelle dell'archivio
  altro <- file.path(cartella, "altro.duckdb")
  con <- DBI::dbConnect(duckdb::duckdb(), altro)
  DBI::dbWriteTable(con, "prova", data.frame(x = 1))
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(carica_archivio(altro), "non contiene un archivio di letture")

  # un archivio scritto con un'altra versione dello schema
  vecchio <- file.path(cartella, "vecchio.duckdb")
  scrivi_archivio(archivio_da_letture(letture_test()), vecchio)
  con <- DBI::dbConnect(duckdb::duckdb(), vecchio)
  DBI::dbExecute(con, "UPDATE informazioni SET valore = '0' WHERE chiave = 'versione'")
  DBI::dbDisconnect(con, shutdown = TRUE)
  expect_error(carica_archivio(vecchio), "scritto con lo schema 0")
})
