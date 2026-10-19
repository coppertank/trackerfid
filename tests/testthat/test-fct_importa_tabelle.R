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
  expect_equal(nrow(letture), 9)
  # il comune è il pezzo dopo la prima virgola, in maiuscolo; il cantiere
  # viene dalla tabella, per nome esatto. La frazione e VENEZIA restano come
  # sono: il comune dalla posizione lo trova `correggi_comune_lettura()`
  expect_setequal(letture$comune_lettura, c("LIMENA", "TAGGI DI PROVA", "VENEZIA"))
  expect_true(all(letture$cantiere[letture$comune_lettura == "LIMENA"] == "RUBANO"))
  expect_true(all(is.na(letture$cantiere[letture$comune_lettura != "LIMENA"])))
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
    "1,SECCO,120,Q,26,2020-01-01,2024-12-31,F9,CESSATA,U9,VALSTAGNA,31",
    "2,SECCO,120,Q,26,2020-01-01,,F8,ATTIVA,U8,LUSINA CONCO,31"
  )))
  expect_identical(corretto$comune_fornitura, c("VALBRENTA", "LUSIANA CONCO"))
  expect_identical(corretto$data_cessazione_contenitore[1], as.Date("2024-12-31"))

  # di un gruppo di sacchetti vale l'associazione più recente
  sacchetti <- importa_sacchetti(file$gruppi_sacchetti, file$servizi_sacchetti)
  expect_identical(sacchetti$id_fornitura, "S1")

  calendario <- importa_calendario(file$calendario)
  # fuori il turno senza mezzo; la matricola perde la lettera iniziale
  expect_equal(nrow(calendario), 6)
  expect_setequal(calendario$matricola_mezzo, c("1601", "1602"))
  expect_identical(calendario$comune_giro[6], "ROSÀ")
  expect_identical(calendario$eseguito, c("SI", "NO", "SI", "SI", "SI", "SI"))
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
  # il comune della frazione viene dalla posizione; due giri da controllare
  messaggi <- character(0)
  avvisi <- character(0)
  letture <- withCallingHandlers(
    prepara_letture_app(file),
    message = function(m) {
      messaggi <<- c(messaggi, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      avvisi <<- c(avvisi, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(messaggi, "per 1 letture.*TAGGI DI PROVA \u2192 LIMENA \\(1\\)", all = FALSE)
  expect_length(avvisi, 2)
  expect_match(avvisi[1], "^1 giri sono ancora non confermati dopo pi\u00f9 di 7 giorni")
  expect_match(avvisi[2], "^1 giri confermati di mezzi con l'antenna non hanno nessuna lettura")
  expect_identical(
    names(letture)[29:35],
    colonne_nucleo_manovra()
  )
  expect_equal(ncol(letture), 35)
  # censito tre volte, tre non censiti una; il sacchetto resta fuori
  expect_identical(
    sort(unique(letture$RFID)),
    c("0000000123", "0000000128", "0000000129", "0000000999")
  )
  # la frazione era dentro LIMENA: il comune del giro, coerente
  frazione <- letture[letture$RFID == "0000000128", ]
  expect_identical(frazione$comune_lettura, "LIMENA")
  expect_identical(frazione$cantiere, "RUBANO")
  expect_identical(frazione$coerenza_comune, "Coerente")
  expect_true(is.na(frazione$distanza_comune_dichiarato))
  # VENEZIA non e' un comune servito: resta, fuori giro, lontano da LIMENA
  venezia <- letture[letture$RFID == "0000000129", ]
  expect_identical(venezia$comune_lettura, "VENEZIA")
  expect_identical(venezia$coerenza_comune, "Fuori giro")
  expect_gt(venezia$distanza_comune_dichiarato, 25000)
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
  expect_identical(
    basename(percorsi),
    c("letture_app.csv", "letture_app.duckdb", "controllo_giri.csv")
  )
  # i giri da controllare: il giro ancora NO e quello confermato senza
  # letture; il giro confermato del mezzo senza antenna non conta
  giri <- utils::read.csv(percorsi[3], colClasses = "character")
  expect_identical(giri$giorno, c("2026-08-05", "2026-08-10"))
  expect_identical(giri$id_giro, c("12", "10"))
  expect_identical(giri$problema, c(
    "non confermato dopo 7 giorni",
    "confermato, senza letture dell'antenna"
  ))
  caricato <- carica_archivio(percorsi[2])
  withr::defer(chiudi_archivio(caricato$archivio))
  expect_identical(caricato$archivio$n, nrow(letture))
  # il CSV ha tutte le colonne della pipeline; l'archivio .duckdb le sole
  # colonne dell'app
  dal_csv <- carica_dataset(percorsi[1], colonne = "tutte")$dati
  dal_duckdb <- carica_dataset(percorsi[2])$dati
  expect_named(dal_duckdb, colonne_app())
  expect_identical(dal_duckdb, dal_csv[colonne_app()])
  expect_true(all(names(letture) %in% c(names(dal_csv), "volume", "numero_raccolte_annue")))
  expect_identical(dal_csv$potenziamento_secco, letture$potenziamento_secco)
  expect_identical(dal_csv$nucleo_residenti, as.integer(letture$nucleo_residenti))
  expect_identical(dal_csv$id_fornitura, letture$id_fornitura)

  storiche <- scrivi_letture_storiche(file, file.path(cartella, "output"))
  expect_identical(basename(storiche), "letture_storiche.csv")
  expect_identical(utils::read.csv(storiche, colClasses = "character")$RFID, "0000000123")
})
