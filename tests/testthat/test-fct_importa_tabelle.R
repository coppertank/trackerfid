test_that("i file estratti hanno sempre lo stesso nome", {
  file <- file_origine("origine")
  expect_named(file, c(
    "test_mezzi", "letture", "contenitori", "gruppi_sacchetti", "servizi_sacchetti",
    "calendario", "componenti", "manovre", "storiche"
  ))
  expect_identical(file$test_mezzi, file.path("origine", "AppSheet.ViewData.csv"))
  expect_error(leggi_tabella_origine("non_esiste.csv"), "File non trovato")
})

test_that("ogni tabella estratta si importa con i nomi e i tipi del progetto", {
  file <- scrivi_origine_prova(withr::local_tempdir())

  test <- importa_test_mezzi(file$test_mezzi)
  expect_named(test, c("targa_veicolo", "matricola_veicolo", "esito", "data_test"))
  expect_identical(test$data_test, as.Date(c("2026-07-31", "2026-07-31", NA)))
  # il trattino vale come mancante
  expect_true(is.na(test$esito[3]))

  letture <- importa_letture(file$letture)
  expect_identical(
    names(letture),
    c("giorno_lettura", "giorno", "targa_veicolo", "RFID", "comune_lettura",
      "NumeroLetture", "latitudine", "longitudine", "cantiere")
  )
  # fuori la posizione zero e la lettura senza comune
  expect_equal(nrow(letture), 7)
  # il comune è il pezzo dopo la prima virgola, in maiuscolo
  expect_true(all(letture$comune_lettura == "LIMENA"))
  expect_true(all(letture$cantiere == "RUBANO"))
  expect_equal(letture$latitudine[1], 45.47)
  expect_identical(format(letture$giorno_lettura[1], "%Y-%m-%d %H:%M:%S"), "2026-08-03 08:15:00")
  expect_identical(letture$giorno[1], as.Date("2026-08-03"))

  contenitori <- importa_contenitori(file$contenitori)
  # fuori il contenitore senza codice
  expect_equal(nrow(contenitori), 1)
  expect_identical(contenitori$id_fornitura, "F1")
  expect_identical(contenitori$volume, 120)
  expect_identical(contenitori$data_cessazione_contenitore, as.Date("2099-12-31"))
  # i nomi dei comuni si correggono
  corretto <- importa_contenitori(scrivi_csv(c(
    readLines(file$contenitori)[1],
    "1,SECCO,120,Q,26,2020-01-01,2024-12-31,F9,CESSATO,U9,VALSTAGNA,UD",
    "2,SECCO,120,Q,26,2020-01-01,,F8,APERTO,U8,LUSINA CONCO,UD"
  )))
  expect_identical(corretto$comune_fornitura, c("VALBRENTA", "LUSIANA CONCO"))
  expect_identical(corretto$data_cessazione_contenitore[1], as.Date("2024-12-31"))

  # di un gruppo di sacchetti vale l'associazione più recente
  sacchetti <- importa_sacchetti(file$gruppi_sacchetti, file$servizi_sacchetti)
  expect_identical(sacchetti$id_fornitura, "S1")

  calendario <- importa_calendario(file$calendario)
  # fuori il turno senza mezzo; la matricola perde la lettera iniziale
  expect_equal(nrow(calendario), 3)
  expect_true(all(calendario$matricola_mezzo == "VEH001"))
  expect_identical(calendario$comune_giro[3], "ROSÀ")
  expect_s3_class(calendario$giorno, "Date")

  componenti <- importa_componenti(file$componenti)
  expect_identical(componenti$nucleo_residenti, c(2L, 3L))
  expect_identical(format(componenti$data_inizio_decorrenza[2], "%Y-%m-%d"), "2026-08-15")
  expect_true(is.na(componenti$data_fine_decorrenza[2]))

  manovre <- importa_manovre(file$manovre)
  expect_named(manovre, c("id_fornitura", "data_inizio_dichiarazione", "data_fine_dichiarazione"))
  expect_identical(manovre$id_fornitura, "F1")
  expect_identical(manovre$data_fine_dichiarazione, as.Date("2099-12-31"))

  storiche <- importa_letture_storiche(file$storiche)
  # una sola riga: ripetuta, sacchetto e senza codice restano fuori
  expect_identical(storiche$RFID, "0000000123")
  expect_error(
    importa_letture_storiche(file$storiche, colonna_rfid = "rfid"),
    "mancano le colonne: rfid"
  )
})

test_that("dai file estratti si arriva al CSV dell'app, senza tabelle intermedie", {
  cartella <- withr::local_tempdir()
  file <- scrivi_origine_prova(file.path(cartella, "origine"))
  letture <- prepara_letture_app(file)
  expect_identical(
    names(letture)[29:35],
    colonne_nucleo_manovra()
  )
  expect_equal(ncol(letture), 35)
  # censito tre volte, non censito una; il sacchetto resta fuori
  expect_identical(sort(unique(letture$RFID)), c("0000000123", "0000000999"))
  censite <- letture[letture$RFID == "0000000123", ]
  expect_true(all(censite$presente_a_database == "Presente"))
  expect_true(all(censite$id_fornitura == "F1"))
  expect_true(all(censite$servizio_giro == "SECCO PAP"))
  expect_true(all(censite$servizio_atteso == "SECCO"))
  # il nucleo cambia a metà agosto, il potenziamento comincia il 20
  expect_identical(censite$nucleo_residenti, c(2L, 3L, 3L))
  expect_identical(censite$potenziamento_secco, c(FALSE, FALSE, TRUE))
  # un non censito non ha fornitura: il potenziamento manca
  expect_true(is.na(letture$potenziamento_secco[letture$RFID == "0000000999"]))
  # l'ultimo giro del calendario era a ROSÀ, ma un censito si confronta con
  # il comune del database
  expect_true(all(censite$coerenza_comune == "Coerente"))
  expect_identical(filtra_codici_app(data.frame(RFID = c("0000000000123", "00BD1", "12")))$RFID, "12")

  # CSV e archivio .duckdb: l'app li carica
  percorsi <- scrivi_letture_app(letture, file.path(cartella, "output"))
  expect_identical(basename(percorsi), c("letture_app.csv", "letture_app.duckdb"))
  caricato <- carica_archivio(percorsi[2])
  withr::defer(chiudi_archivio(caricato$archivio))
  expect_identical(caricato$archivio$n, nrow(letture))
  # l'analisi dei conferimenti legge il CSV
  expect_equal(nrow(leggi_letture_conferimenti(percorsi[1])), nrow(letture))

  storiche <- scrivi_letture_storiche(file, file.path(cartella, "output"))
  expect_identical(basename(storiche), "letture_storiche.csv")
  expect_identical(utils::read.csv(storiche, colClasses = "character")$RFID, "0000000123")
})
