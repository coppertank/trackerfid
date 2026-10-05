test_that("il dataset di esempio viene letto e convertito nei tipi corretti", {
  esito <- carica_dataset(percorso_dataset_esempio())
  dati <- esito$dati

  expect_identical(names(dati), colonne_obbligatorie())
  expect_s3_class(dati$giorno_lettura, "POSIXct")
  expect_type(dati$latitudine, "double")
  expect_type(dati$longitudine, "double")
  expect_type(dati$RFID, "character")
  expect_setequal(unique(dati$presente_a_database), stati_database())
  expect_length(esito$avvisi, 0)
})

test_that("il separatore viene rilevato automaticamente", {
  virgola <- scrivi_csv(c("a,b,c", "1,2,3"))
  punto_virgola <- scrivi_csv(c("a;b;c", "1;2;3"))
  expect_identical(rileva_separatore(virgola), ",")
  expect_identical(rileva_separatore(punto_virgola), ";")
})

test_that("legge CSV con punto e virgola, virgola decimale e date italiane", {
  percorso <- scrivi_csv(c(
    gsub(",", ";", intestazione_csv),
    "15/09/2025 08:30:00;AB123CD;VEH001;R1;Presente;carta;NA;007;41,9028;12,4964",
    "16/09/2025 09:00;AB123CD;VEH001;R2;non presente;;SECCO;;41,9100;12,5000"
  ))
  dati <- carica_dataset(percorso)$dati

  expect_equal(dati$giorno_lettura[1], as.POSIXct("2025-09-15 08:30:00", tz = "UTC"))
  expect_equal(dati$giorno_lettura[2], as.POSIXct("2025-09-16 09:00:00", tz = "UTC"))
  expect_equal(dati$latitudine, c(41.9028, 41.91))
  expect_equal(dati$longitudine, c(12.4964, 12.5))
  # normalizzazione dei testi e zeri iniziali conservati
  expect_identical(dati$servizio_transponder, c("CARTA", NA))
  expect_identical(dati$presente_a_database, c("Presente", "Non Presente"))
  expect_identical(dati$id_utenza, c("007", NA))
})

test_that("i nomi delle colonne non dipendono da maiuscole e ordine", {
  percorso <- scrivi_csv(c(
    "rfid,GIORNO_LETTURA,targa_veicolo,matricola_veicolo,presente_a_database,servizio_transponder,servizio_atteso,id_utenza,longitudine,latitudine,extra",
    "R1,2025-09-15 08:30:00,AB123CD,VEH001,Presente,CARTA,NA,UTZ001,12.4964,41.9028,x"
  ))
  dati <- carica_dataset(percorso)$dati
  expect_identical(names(dati), colonne_obbligatorie())
  expect_equal(dati$latitudine, 41.9028)
})

test_that("le colonne mancanti producono un errore esplicito", {
  percorso <- scrivi_csv(c(
    "giorno_lettura,targa_veicolo,RFID,latitudine",
    "2025-09-15 08:30:00,AB123CD,R1,41.9"
  ))
  errore <- expect_error(carica_dataset(percorso), class = "errore_validazione")
  expect_match(conditionMessage(errore), "Colonne mancanti")
  expect_match(conditionMessage(errore), "matricola_veicolo")
  expect_match(conditionMessage(errore), "longitudine")
})

test_that("i valori non interpretabili sono segnalati con il numero di riga", {
  percorso <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,NA,UTZ001,41.9,12.5",
    "non-una-data,AB123CD,VEH001,R2,Forse,CARTA,NA,UTZ001,abc,999"
  ))
  errore <- expect_error(carica_dataset(percorso), class = "errore_validazione")
  messaggi <- errore$messaggi

  expect_length(messaggi, 4)
  expect_match(messaggi[1], "giorno_lettura.*Righe del file: 3")
  expect_match(messaggi[2], "latitudine.*non numerici")
  expect_match(messaggi[3], "longitudine.*fuori dall'intervallo")
  expect_match(messaggi[4], "presente_a_database.*Forse")
})

test_that("un file vuoto o senza righe produce un errore esplicito", {
  expect_error(carica_dataset(scrivi_csv("")), class = "errore_validazione")
  expect_error(carica_dataset(scrivi_csv(intestazione_csv)), class = "errore_validazione")
})

test_that("le righe senza campi essenziali sono scartate con un avviso", {
  percorso <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,NA,UTZ001,41.9,12.5",
    "2025-09-15 09:30:00,AB123CD,VEH001,R2,Presente,CARTA,NA,UTZ001,,12.5",
    "2025-09-15 10:30:00,AB123CD,VEH001,,Presente,CARTA,NA,UTZ001,41.9,12.5"
  ))
  esito <- carica_dataset(percorso)
  expect_identical(esito$dati$RFID, "R1")
  expect_match(esito$avvisi, "2 righe scartate")
})

test_that("le incoerenze del dataset generano avvisi non bloccanti", {
  percorso <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Non Presente,CARTA,NA,UTZ001,41.9,12.5",
    "2025-09-15 09:30:00,AB123CD,VEH002,R2,Presente,CARTA,NA,UTZ001,41.9,12.5"
  ))
  esito <- carica_dataset(percorso)
  expect_equal(nrow(esito$dati), 2)
  expect_length(esito$avvisi, 2)
  expect_match(esito$avvisi[1], "Non Presente")
  expect_match(esito$avvisi[2], "univoca")
})

test_that("la deduplica mantiene solo l'ultima lettura di ogni RFID", {
  ultimi <- deduplica_ultimo_rfid(letture_test())
  expect_equal(nrow(ultimi), 3)
  expect_setequal(ultimi$RFID, c("A", "B", "C"))
  expect_equal(
    ultimi$giorno_lettura[ultimi$RFID == "A"],
    as.POSIXct("2025-09-20 08:00:00", tz = "UTC")
  )
  expect_identical(names(ultimi), names(letture_test()))
})

test_that("la percentuale di presenti è corretta anche senza righe", {
  ultimi <- deduplica_ultimo_rfid(letture_test())
  expect_equal(calcola_pct_presente(ultimi), 2 / 3)
  expect_equal(calcola_pct_presente(ultimi[0, ]), 0)
})

test_that("il filtro per periodo include gli estremi", {
  filtrate <- filtra_periodo(letture_test(), as.Date(c("2025-09-05", "2025-09-10")))
  expect_setequal(
    format(filtrate$giorno_lettura, "%d"),
    c("05", "06", "07", "10")
  )
  expect_equal(nrow(filtra_periodo(letture_test(), NULL)), 6)
})

test_that("i filtri per stato e servizio si combinano correttamente", {
  d <- letture_test()

  expect_equal(nrow(filtra_letture(d)), 6)
  expect_setequal(filtra_letture(d, stato_db = "Presente")$RFID, c("A", "B"))
  expect_equal(nrow(filtra_letture(d, stato_db = character(0))), 0)

  # servizio transponder: per esclusione; i non censiti hanno un interruttore a parte
  senza_carta <- filtra_letture(d, transponder_esclusi = "CARTA")
  expect_false("CARTA" %in% senza_carta$servizio_transponder)
  expect_true("C" %in% senza_carta$RFID)
  expect_false("C" %in% filtra_letture(d, includi_non_censiti = FALSE)$RFID)

  # servizio atteso: riguarda solo i non censiti
  senza_secco <- filtra_letture(d, atteso_esclusi = "SECCO")
  expect_equal(sum(senza_secco$RFID == "C"), 1)
  expect_equal(sum(senza_secco$presente_a_database == "Presente"), 4)
  senza_stima <- filtra_letture(d, includi_senza_stima = FALSE)
  expect_equal(sum(senza_stima$RFID == "C"), 1)
  expect_equal(sum(senza_stima$presente_a_database == "Presente"), 4)
})

test_that("i filtri si applicano prima della deduplica", {
  # Escludendo CARTA, del bidone A resta visibile l'ultima lettura SECCO.
  ultimi <- deduplica_ultimo_rfid(filtra_letture(letture_test(), transponder_esclusi = "CARTA"))
  riga <- ultimi[ultimi$RFID == "A", ]
  expect_identical(riga$servizio_transponder, "SECCO")
  expect_equal(riga$giorno_lettura, as.POSIXct("2025-09-10 08:00:00", tz = "UTC"))
})

test_that("le statistiche contano una riga per RFID", {
  stat <- calcola_statistiche(deduplica_ultimo_rfid(letture_test()))
  expect_equal(stat$totale, 3)
  expect_equal(stat$presente, 2)
  expect_equal(stat$non_presente, 1)
  expect_identical(stat$transponder, c(CARTA = 1L, VETRO = 1L))
  expect_length(stat$atteso, 0)
  expect_equal(stat$senza_stima, 1)
})

test_that("i servizi seguono l'ordine canonico", {
  expect_identical(
    ordina_servizi(c("VETRO", "ZETA", NA, "SECCO", "ALFA", "SECCO")),
    c("SECCO", "VETRO", "ALFA", "ZETA")
  )
  expect_identical(ordina_servizi(NULL), character(0))
  expect_identical(ordina_servizi(c(NA, NA)), character(0))
})

test_that("il testo di ricerca viene scomposto in codici", {
  expect_identical(
    analizza_input_ricerca(" R1 , R2\nR3\r\n\n R1 ,, "),
    c("R1", "R2", "R3")
  )
  expect_length(analizza_input_ricerca(""), 0)
  expect_length(analizza_input_ricerca(NULL), 0)
})

test_that("la ricerca per RFID restituisce tutte le letture e marca l'ultima", {
  trovate <- cerca_per_rfid(letture_test(), c("a", "C", "inesistente"))
  expect_equal(nrow(trovate), 5)
  expect_equal(sum(trovate$is_ultimo), 2)
  ultima_a <- trovate[trovate$RFID == "A" & trovate$is_ultimo, ]
  expect_equal(ultima_a$giorno_lettura, as.POSIXct("2025-09-20 08:00:00", tz = "UTC"))
  expect_equal(nrow(cerca_per_rfid(letture_test(), "inesistente")), 0)
})

test_that("la ricerca per utenza considera solo l'utenza attuale", {
  d <- letture_test()
  # A era di U1 ma ora è di U2: cercando U1 compare solo B.
  expect_identical(filtra_ultimo_per_utenza(d, "U1")$RFID, "B")
  expect_identical(filtra_ultimo_per_utenza(d, "u2")$RFID, "A")
  expect_setequal(filtra_ultimo_per_utenza(d, c("U1", "U2"))$RFID, c("A", "B"))
  expect_equal(nrow(filtra_ultimo_per_utenza(d, "U9")), 0)

  esempio <- dati_esempio()
  expect_false("RFD20250901050" %in% filtra_ultimo_per_utenza(esempio, "UTZ001")$RFID)
  expect_true("RFD20250901050" %in% filtra_ultimo_per_utenza(esempio, "UTZ025")$RFID)
})

test_that("il bounding box copre tutte le letture", {
  d <- letture_test()
  expect_identical(
    calcola_bbox(d[d$RFID == "A", ]),
    list(lat_min = 41.90, lat_max = 41.92, lng_min = 12.40, lng_max = 12.42)
  )
})

test_that("la cronologia accorpa le letture consecutive uguali", {
  d <- letture_test()
  servizi <- cronologia_transizioni(d[d$RFID == "A", ], "servizio_transponder")
  expect_identical(servizi$valore, c("CARTA", "SECCO", "CARTA"))
  expect_identical(servizi$attuale, c(FALSE, FALSE, TRUE))

  utenze <- cronologia_transizioni(d[d$RFID == "A", ], "id_utenza")
  expect_identical(utenze$valore, c("U1", "U2"))
  expect_equal(utenze$giorno_lettura[2], as.POSIXct("2025-09-20 08:00:00", tz = "UTC"))

  expect_equal(nrow(cronologia_transizioni(d[d$RFID == "C", ], "id_utenza")), 0)
})

test_that("l'analisi di un RFID riconosce i quattro casi", {
  d <- letture_test()

  a <- analizza_rfid(d[d$RFID == "A", ])
  expect_identical(a$caso, "cambio_servizio")
  expect_identical(a$servizio_attuale, "CARTA")
  expect_true(a$cambio_utenza)
  expect_equal(a$n_letture, 3)

  b <- analizza_rfid(d[d$RFID == "B", ])
  expect_identical(b$caso, "censito_coerente")
  expect_false(b$cambio_utenza)

  c_stima <- analizza_rfid(d[d$RFID == "C", ])
  expect_identical(c_stima$caso, "non_censito_con_stima")
  expect_equal(c_stima$stima$pct, 100)

  senza <- d[d$RFID == "C" & is.na(d$servizio_atteso), ]
  expect_identical(analizza_rfid(senza)$caso, "non_censito_senza_stima")
})

test_that("i casi speciali del dataset di esempio sono riconosciuti", {
  d <- dati_esempio()
  caso <- function(rfid) analizza_rfid(d[d$RFID == rfid, ])

  cambio <- caso("RFD20250901001")
  expect_identical(cambio$caso, "cambio_servizio")
  expect_identical(cambio$cronologia_servizio$valore, c("CARTA", "SECCO", "CARTA"))

  utenza <- caso("RFD20250901050")
  expect_true(utenza$cambio_utenza)
  expect_identical(utenza$cronologia_utenza$valore, c("UTZ001", "UTZ025"))

  stima <- caso("RFD20250915201")
  expect_identical(stima$caso, "non_censito_con_stima")
  expect_identical(stima$stima$servizio, "SECCO PAP")
  expect_equal(stima$stima$pct, 100)

  expect_identical(caso("RFD20250920250")$caso, "non_censito_senza_stima")
})

test_that("la stima del servizio atteso somma a 100", {
  letture <- dplyr::tibble(servizio_atteso = c("SECCO", "SECCO", "CARTA", NA, "SECCO"))
  stima <- stima_servizio_atteso(letture)
  expect_identical(stima$servizio, c("SECCO", "CARTA"))
  expect_equal(stima$pct, c(75, 25))
})
