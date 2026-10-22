test_that("i file estratti hanno sempre lo stesso nome", {
  file <- file_origine("origine")
  expect_named(
    file,
    c(
      "test_mezzi",
      "letture",
      "contenitori",
      "gruppi_sacchetti",
      "servizi_sacchetti",
      "calendario",
      "non_pianificati",
      "componenti",
      "manovre",
      "storiche"
    )
  )
  expect_identical(
    file$test_mezzi,
    file.path("origine", "AppSheet.ViewData.csv")
  )
  expect_error(leggi_tabella_origine("non_esiste.csv"), "File non trovato")
})

test_that("ogni tabella estratta si importa con i nomi e i tipi del progetto", {
  file <- scrivi_origine_prova(withr::local_tempdir())

  test <- importa_test_mezzi(file$test_mezzi)
  expect_named(
    test,
    c("targa_veicolo", "matricola_veicolo", "esito", "data_test")
  )
  expect_identical(test$data_test, as.Date(c("2026-07-31", "2026-07-31", NA)))
  # il trattino vale come mancante
  expect_true(is.na(test$esito[3]))

  letture <- importa_letture(file$letture)
  expect_identical(
    names(letture),
    c(
      "data_ora_lettura",
      "data_lettura",
      "targa_veicolo",
      "RFID",
      "comune_lettura",
      "NumeroLetture",
      "latitudine",
      "longitudine",
      "cantiere"
    )
  )
  # fuori la posizione zero e la lettura senza comune
  expect_equal(nrow(letture), 9)
  # il comune è il pezzo dopo la prima virgola, in maiuscolo; il cantiere
  # viene dalla tabella, per nome esatto. La frazione e VENEZIA restano come
  # sono: il comune dalla posizione lo trova `correggi_comune_lettura()`
  expect_setequal(
    letture$comune_lettura,
    c("LIMENA", "TAGGI DI PROVA", "VENEZIA")
  )
  expect_true(all(
    letture$cantiere[letture$comune_lettura == "LIMENA"] == "RUBANO"
  ))
  expect_true(all(is.na(letture$cantiere[letture$comune_lettura != "LIMENA"])))
  # un RFID letto più volte nello stesso giorno: resta la prima lettura valida
  doppie <- importa_letture(scrivi_csv(c(
    "IndirizzoLocalizzazione;DataeOraPrimaLettura;RFID(EPC);Targa;Latitudine;Longitudine;NumeroLetture",
    "VIA ROMA 1, LIMENA;03/08/2026 10.00.00;123;AB123CD;45,4700;11,8400;1",
    "VIA ROMA 1, LIMENA;03/08/2026 08.00.00;123;AB123CD;45,4700;11,8400;1",
    # più presto, ma sporche: non contano
    "VIA ROMA 1, LIMENA;03/08/2026 07.00.00;123;AB123CD;0;0;1",
    "SENZA COMUNE;03/08/2026 07.30.00;123;AB123CD;45,4700;11,8400;1",
    "VIA ROMA 1, LIMENA;04/08/2026 08.00.00;123;AB123CD;45,4700;11,8400;1"
  )))
  expect_identical(format(doppie$data_ora_lettura, "%d %H"), c("03 08", "04 08"))
  # le letture escono in ordine di istante
  expect_false(is.unsorted(letture$data_ora_lettura))
  prima <- letture[letture$RFID == "123", ][1, ]
  expect_equal(prima$latitudine, 45.47)
  expect_identical(
    format(prima$data_ora_lettura, "%Y-%m-%d %H:%M:%S"),
    "2026-08-03 08:15:00"
  )
  expect_identical(prima$data_lettura, as.Date("2026-08-03"))

  contenitori <- importa_contenitori(file$contenitori)
  # fuori il contenitore senza codice
  expect_equal(nrow(contenitori), 1)
  expect_identical(contenitori$id_fornitura, "F1")
  expect_identical(contenitori$volume, 120)
  expect_identical(
    contenitori$data_cessazione_contenitore,
    as.Date("2099-12-31")
  )
  # i nomi dei comuni si correggono
  corretto <- importa_contenitori(scrivi_csv(c(
    readLines(file$contenitori)[1],
    "1,SECCO,120,Q,26,2020-01-01,2024-12-31,F9,CESSATA,U9,VALSTAGNA,31",
    "2,SECCO,120,Q,26,2020-01-01,,F8,ATTIVA,U8,LUSINA CONCO,31"
  )))
  expect_identical(corretto$comune_fornitura, c("VALBRENTA", "LUSIANA CONCO"))
  expect_identical(
    corretto$data_cessazione_contenitore[1],
    as.Date("2024-12-31")
  )

  # di un gruppo di sacchetti vale l'associazione più recente
  sacchetti <- importa_sacchetti(file$gruppi_sacchetti, file$servizi_sacchetti)
  expect_identical(sacchetti$id_fornitura, "S1")

  calendario <- importa_calendario(file$calendario)
  # fuori il turno senza mezzo; la matricola perde la lettera iniziale
  expect_equal(nrow(calendario), 6)
  expect_setequal(calendario$matricola_mezzo, c("1601", "1602"))
  expect_identical(calendario$comune_giro[6], "ROSÀ")
  expect_identical(calendario$eseguito, c("SI", "NO", "SI", "SI", "SI", "SI"))
  expect_s3_class(calendario$data_giro, "Date")
  # un file estratto con il nome vecchio della data vale lo stesso
  vecchio <- importa_calendario(scrivi_csv(c(
    "giorno,ora_inizio,ora_fine,matricola_mezzo,id_giro,eseguito,descrizione_servizio,comune_giro",
    "2026-08-03,06:00:00,12:00:00,X1601,10,SI,SECCO PAP,LIMENA"
  )))
  expect_identical(vecchio$data_giro, as.Date("2026-08-03"))
  # i giri non pianificati: stesse regole, senza orari
  non_pianificati <- importa_giri_non_pianificati(file$non_pianificati)
  expect_named(
    non_pianificati,
    c("data_giro", "matricola_mezzo", "descrizione_servizio", "comune_giro")
  )
  expect_identical(non_pianificati$matricola_mezzo, "1601")
  expect_identical(non_pianificati$data_giro, as.Date("2026-08-03"))
  expect_error(
    importa_giri_non_pianificati(scrivi_csv(c(
      "data_giro,matricola_mezzo",
      "2026-08-03,X1601"
    ))),
    "mancano le colonne: descrizione_servizio, comune_giro"
  )

  componenti <- importa_componenti(file$componenti)
  expect_identical(componenti$nucleo_residenti, c(2L, 3L))
  expect_identical(
    format(componenti$data_inizio_decorrenza[2], "%Y-%m-%d"),
    "2026-08-15"
  )
  expect_true(is.na(componenti$data_fine_decorrenza[2]))

  manovre <- importa_manovre(file$manovre)
  expect_named(
    manovre,
    c("id_fornitura", "data_inizio_dichiarazione", "data_fine_dichiarazione")
  )
  expect_identical(manovre$id_fornitura, "F1")
  expect_identical(manovre$data_fine_dichiarazione, as.Date("2099-12-31"))

  storiche <- importa_letture_storiche(file$storiche)
  # la riga ripetuta conta una volta, quella senza codice resta fuori; i
  # sacchetti si tolgono dopo, come per le letture con le antenne
  expect_named(
    storiche,
    c("data_ora_lettura", "data_lettura", "id_trasmissione", "RFID")
  )
  expect_identical(
    storiche$RFID,
    c("0000000123", "00BD00000000000000000001", "0000000123")
  )
  expect_identical(storiche$id_trasmissione, c("T1", "T1", "T3"))
  expect_identical(
    storiche$data_lettura,
    as.Date(c("2025-03-01", "2025-03-02", "2026-08-21"))
  )
  expect_identical(
    format(storiche$data_ora_lettura[1], "%Y-%m-%d %H:%M:%S"),
    "2025-03-01 07:00:00"
  )
  expect_error(
    importa_letture_storiche(file$storiche, colonna_rfid = "rfid"),
    "mancano le colonne: rfid"
  )
  # anche le date italiane; un istante illeggibile si scarta, con un avviso
  expect_warning(
    italiane <- importa_letture_storiche(scrivi_csv(c(
      "data_lettura,codice_transponder,id_trasmissione",
      "01/03/2025 07.00.00,123,T1",
      "ieri,124,T1"
    ))),
    "1 letture storiche hanno un istante illeggibile.*ieri"
  )
  expect_identical(italiane$RFID, "0000000123")
  expect_identical(italiane$data_lettura, as.Date("2025-03-01"))
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
  expect_match(
    messaggi,
    "per 1 letture.*TAGGI DI PROVA \u2192 LIMENA \\(1\\)",
    all = FALSE
  )
  # quante letture ha associato ogni passo
  expect_match(
    messaggi,
    "^Giro associato alle letture: 5 giro pianificato, nell'orario e nel comune",
    all = FALSE
  )
  expect_length(avvisi, 2)
  expect_match(
    avvisi[1],
    "^1 giri sono ancora non confermati dopo pi\u00f9 di 7 giorni"
  )
  expect_match(
    avvisi[2],
    "^1 giri confermati di mezzi con l'antenna non hanno nessuna lettura"
  )
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
  # fuori dal comune del pianificato ma nel suo orario: l'orario conta più
  # del comune, e vince il pianificato sul non pianificato
  expect_identical(venezia$servizio_giro, "SECCO PAP")
  expect_identical(venezia$id_giro, "10")
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
  expect_identical(
    filtra_codici_app(data.frame(
      RFID = c("0000000000123", "00BD1", "12")
    ))$RFID,
    "12"
  )

  # CSV e archivio .duckdb, scritti come nella pipeline: l'app li carica
  output <- file.path(cartella, "output")
  csv <- scrivi_csv_output(letture, "letture_app.csv", output)
  percorsi <- c(
    csv,
    scrivi_archivio(csv, file.path(output, "letture_app.duckdb")),
    scrivi_csv_output(giri_da_controllare(letture), "controllo_giri.csv", output, na = "")
  )
  expect_identical(
    basename(percorsi),
    c("letture_app.csv", "letture_app.duckdb", "controllo_giri.csv")
  )
  # le letture con il comune del GPS sostituito: la frazione
  corretti <- comuni_corretti(letture)
  expect_identical(corretti$RFID, "0000000128")
  expect_identical(corretti$comune_gps, "TAGGI DI PROVA")
  expect_identical(corretti$comune_posizione, "LIMENA")
  # senza l'attributo i giri da controllare sono una tabella vuota
  expect_equal(nrow(giri_da_controllare(data.frame(data_lettura = as.Date(character(0))))), 0)
  # i giri da controllare: il giro ancora NO e quello confermato senza
  # letture; il giro confermato del mezzo senza antenna non conta
  giri <- utils::read.csv(percorsi[3], colClasses = "character")
  expect_identical(giri$data_giro, c("2026-08-05", "2026-08-10"))
  expect_identical(giri$id_giro, c("12", "10"))
  expect_identical(
    giri$problema,
    c(
      "non confermato dopo 7 giorni",
      "confermato, senza letture dell'antenna"
    )
  )
  caricato <- carica_archivio(percorsi[2])
  withr::defer(chiudi_archivio(caricato$archivio))
  expect_identical(caricato$archivio$n, nrow(letture))
  # il CSV ha tutte le colonne della pipeline; l'archivio .duckdb le sole
  # colonne dell'app
  dal_csv <- carica_dataset(percorsi[1], colonne = "tutte")$dati
  dal_duckdb <- carica_dataset(percorsi[2])$dati
  expect_named(dal_duckdb, colonne_app())
  expect_identical(dal_duckdb, dal_csv[colonne_app()])
  expect_true(all(
    names(letture) %in% c(names(dal_csv), "volume", "numero_raccolte_annue")
  ))
  expect_identical(dal_csv$potenziamento_secco, letture$potenziamento_secco)
  expect_identical(
    dal_csv$nucleo_residenti,
    as.integer(letture$nucleo_residenti)
  )
  expect_identical(dal_csv$id_fornitura, letture$id_fornitura)

  # le letture storiche: la stessa anagrafica, senza sacchetti, con le
  # colonne in comune con quelle dell'app e la trasmissione
  storiche <- prepara_letture_storiche(file)
  expect_named(storiche, colonne_letture_storiche())
  expect_true(all(
    setdiff(names(storiche), "id_trasmissione") %in% names(letture)
  ))
  expect_identical(storiche$RFID, c("0000000123", "0000000123"))
  expect_identical(storiche$presente_a_database, c("Presente", "Presente"))
  expect_identical(storiche$id_fornitura, c("F1", "F1"))
  expect_identical(storiche$servizio_transponder, c("SECCO", "SECCO"))
  # nucleo e potenziamento in vigore nel giorno della lettura
  expect_identical(storiche$nucleo_residenti, c(2L, 3L))
  expect_identical(storiche$potenziamento_secco, c(FALSE, TRUE))
  percorso <- scrivi_csv_output(storiche, "letture_storiche.csv", output)
  expect_identical(basename(percorso), "letture_storiche.csv")
  scritte <- utils::read.csv(percorso, colClasses = "character")
  expect_identical(names(scritte), colonne_letture_storiche())
  expect_identical(scritte$id_trasmissione, c("T1", "T3"))
})
