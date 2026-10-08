# Tabelle di prova nella forma di quelle estratte dai sistemi aziendali.

ora <- function(testo) as.POSIXct(testo)

letture_grezze <- function(quando, targa = "AA111AA", rfid = "abc123") {
  dplyr::tibble(
    giorno_lettura = ora(quando),
    giorno = as.Date(substr(quando, 1, 10)),
    targa_veicolo = targa,
    RFID = rfid,
    latitudine = "45,46420",
    longitudine = "9.19000",
    IndirizzoLocalizzazione = "Via Prova 1",
    NumeroLetture = "1"
  )
}

test_mezzi_prova <- dplyr::tibble(
  targa_veicolo = c("AA111AA", "BB222BB"),
  matricola_veicolo = c("101", "102"),
  esito = c("POSITIVO", "NEGATIVO"),
  data_test = as.Date(c("2026-02-01", "2026-02-01"))
)

contenitori_prova <- dplyr::tibble(
  codice_transponder = c("0000ABC123", "0000ABC123", "0000000001"),
  descrizione_rifiuto = c("CARTA", "SECCO", "VETRO"),
  volume = c(240, 240, 1100),
  frequenza = "Q",
  numero_raccolte_annue = c(26, 26, 13),
  data_attivazione_contenitore = as.Date(c(
    "2025-01-01",
    "2026-03-10",
    "2025-06-01"
  )),
  data_cessazione_contenitore = as.Date(c(
    "2026-03-09",
    "2099-12-31",
    "2099-12-31"
  )),
  stato_servizio = "ATTIVO",
  id_utenza = c("U1", "U2", "U3"),
  comune_servizio = "X"
)
tag_prova <- dplyr::tibble(codice_transponder = c("0000ABC123", "0000000042"))
sacchetti_prova <- dplyr::tibble(
  codice_uhf = "000000000000012345678901",
  id_servizio = "S9"
)

calendario_prova <- dplyr::tibble(
  giorno = as.Date("2026-03-02"),
  ora_inizio = c("06:00:00", "13:00", "06:00:00", "22:00:00"),
  ora_fine = c("12:00:00", "18:00", "12:00:00", "04:00:00"),
  matricola_mezzo = c("101", "101", "101", "103"),
  descrizione_servizio = c(
    "SECCO PAP",
    "CARTA/CARTONE PAP",
    "SECCO PAP",
    "VETRO PAP"
  )
)

# Letture già associate al database, pronte per il calendario.
letture_associate <- function(quando, matricola = "101") {
  n <- length(quando)
  dplyr::tibble(
    giorno_lettura = ora(quando),
    giorno = as.Date(substr(quando, 1, 10)),
    targa_veicolo = "AA111AA",
    matricola_veicolo = matricola,
    RFID = sprintf("%010d", seq_len(n)),
    latitudine = 45.4642,
    longitudine = 9.19,
    descrizione_rifiuto = "CARTA",
    volume = 240,
    numero_raccolte_annue = 26,
    id_utenza = "U1",
    presente_a_database = "Presente"
  )
}

test_that("restano solo le letture dei mezzi collaudati, dopo il collaudo", {
  letture <- dplyr::bind_rows(
    letture_grezze("2026-01-31 08:00:00"),
    # il giorno del test non conta: serve una data successiva
    letture_grezze("2026-02-01 08:00:00"),
    letture_grezze("2026-02-02 08:00:00"),
    # test negativo
    letture_grezze("2026-02-10 08:00:00", targa = "BB222BB"),
    # mezzo mai collaudato
    letture_grezze("2026-02-10 08:00:00", targa = "CC333CC")
  )
  valide <- filtra_letture_post_test(letture, test_mezzi_prova)

  expect_equal(nrow(valide), 1)
  expect_identical(valide$giorno, as.Date("2026-02-02"))
  expect_identical(valide$matricola_veicolo, "101")
  expect_identical(
    names(valide),
    c(
      "giorno_lettura",
      "giorno",
      "targa_veicolo",
      "matricola_veicolo",
      "RFID",
      "latitudine",
      "longitudine",
      "IndirizzoLocalizzazione",
      "NumeroLetture"
    )
  )
})

test_that("i codici RFID vengono estratti dal testo degli eventi", {
  eventi <- dplyr::tibble(
    Evento = c(
      "Lettura Tag ABC123, antenna 1",
      "Lettura Tag AAA;BBB ; CCC, antenna 2",
      "Svuotamento contenitore",
      NA
    )
  )
  estratti <- estrai_rfid(eventi)
  expect_identical(estratti$RFID, c("ABC123", "AAA", "BBB", "CCC"))
  expect_true(all(grepl("Lettura Tag", estratti$Evento)))
})

test_that("i codici RFID sono portati a 10 o 24 caratteri", {
  formato <- function(codici) formatta_rfid(dplyr::tibble(RFID = codici))$RFID

  # maiuscolo e riempimento a 10
  expect_identical(formato("abc123"), "0000ABC123")
  expect_identical(formato("0000000001"), "0000000001")
  # gli zeri in eccesso spariscono prima del riempimento
  expect_identical(formato("000000000000000000000042"), "0000000042")
  # oltre 10 caratteri significativi: riempimento a 24
  expect_identical(formato("12345678901"), "000000000000012345678901")
  expect_identical(
    formato("E28011700000020F1A2B3C4D"),
    "E28011700000020F1A2B3C4D"
  )
  # un carattere letto male tra i primi 14, che dovrebbero essere zeri
  expect_identical(formato("00000100000000ABCDEF1234"), "ABCDEF1234")
  expect_identical(formato("00000000000000ABCDEF1234"), "ABCDEF1234")
  # i codici mancanti restano mancanti
  expect_identical(formato(NA_character_), NA_character_)
  expect_identical(
    formato(c("abc123", "12345678901")),
    c("0000ABC123", "000000000000012345678901")
  )
})

test_that("ogni lettura riceve il contenitore valido nel suo giorno", {
  letture <- dplyr::bind_rows(
    # stesso transponder, prima e dopo il cambio di associazione del 10 marzo
    letture_grezze("2026-03-02 08:00:00", rfid = "0000ABC123"),
    letture_grezze("2026-03-09 08:00:00", rfid = "0000ABC123"),
    letture_grezze("2026-03-10 08:00:00", rfid = "0000ABC123"),
    # trovato solo tra i sacchetti
    letture_grezze("2026-03-02 09:00:00", rfid = "000000000000012345678901"),
    # sconosciuto, ma presente nell'anagrafica dei tag
    letture_grezze("2026-03-02 10:00:00", rfid = "0000000042"),
    # sconosciuto ovunque
    letture_grezze("2026-03-02 11:00:00", rfid = "FFFFFFFFFF")
  )
  associate <- associa_servizio(
    letture,
    contenitori_prova,
    tag_prova,
    sacchetti_prova
  )

  expect_equal(nrow(associate), nrow(letture))
  expect_identical(
    associate$descrizione_rifiuto,
    c("CARTA", "CARTA", "SECCO", NA, NA, NA)
  )
  # l'utenza dei sacchetti è il loro servizio
  expect_identical(associate$id_utenza, c("U1", "U1", "U2", "S9", NA, NA))
  expect_identical(
    associate$presente_a_database,
    c(rep("Presente", 4), rep("Non Presente", 2))
  )
  expect_identical(
    associate$presente_in_tag_contenitori,
    c(
      "Presente",
      "Presente",
      "Presente",
      "Non Presente",
      "Presente",
      "Non Presente"
    )
  )
  # coordinate numeriche, anche con la virgola decimale
  expect_equal(associate$latitudine, rep(45.4642, 6))
  expect_equal(associate$longitudine, rep(9.19, 6))
  expect_false("id_servizio" %in% names(associate))
})

test_that("il servizio atteso è il turno in cui cade la lettura, o il più vicino", {
  letture <- letture_associate(c(
    "2026-03-02 08:30:00", # dentro il turno del mattino
    "2026-03-02 15:00:00", # dentro il turno del pomeriggio
    "2026-03-02 12:10:00", # tra i due turni, più vicina alla fine del mattino
    "2026-03-02 12:50:00", # tra i due turni, più vicina all'inizio del pomeriggio
    "2026-03-02 05:30:00", # prima di ogni turno
    "2026-03-02 20:00:00" # dopo ogni turno
  ))
  risultato <- associa_servizio_atteso_da_calendario(letture, calendario_prova)

  # una riga per lettura, anche con turni ripetuti nel calendario
  expect_equal(nrow(risultato), nrow(letture))
  expect_identical(
    risultato$servizio_atteso,
    c(
      "SECCO PAP",
      "CARTA/CARTONE PAP",
      "SECCO PAP",
      "CARTA/CARTONE PAP",
      "SECCO PAP",
      "CARTA/CARTONE PAP"
    )
  )
})

test_that("senza turni il servizio atteso resta vuoto", {
  senza_turni <- associa_servizio_atteso_da_calendario(
    letture_associate("2026-03-02 08:30:00", matricola = "999"),
    calendario_prova
  )
  expect_equal(nrow(senza_turni), 1)
  expect_true(is.na(senza_turni$servizio_atteso))

  # un altro giorno, per lo stesso mezzo, non ha turni
  altro_giorno <- associa_servizio_atteso_da_calendario(
    letture_associate("2026-03-05 08:30:00"),
    calendario_prova
  )
  expect_true(is.na(altro_giorno$servizio_atteso))
})

test_that("un turno a cavallo della mezzanotte vale anche per le letture del giorno dopo", {
  # mezzo 103: turno 22:00 - 04:00 iniziato il 2 marzo
  letture <- letture_associate(
    c(
      "2026-03-02 23:30:00", # prima di mezzanotte, dentro il turno
      "2026-03-03 02:00:00", # dopo mezzanotte, dentro il turno
      "2026-03-03 04:30:00", # poco dopo la fine: resta il turno più vicino
      "2026-03-02 21:00:00" # poco prima dell'inizio
    ),
    matricola = "103"
  )
  risultato <- associa_servizio_atteso_da_calendario(letture, calendario_prova)
  expect_equal(nrow(risultato), nrow(letture))
  expect_identical(risultato$servizio_atteso, rep("VETRO PAP", 4))

  # la notte tra l'1 e il 2 marzo non ha turni: vale il turno più vicino dello stesso giorno
  mattina <- associa_servizio_atteso_da_calendario(
    letture_associate("2026-03-02 03:00:00", matricola = "103"),
    calendario_prova
  )
  expect_identical(mattina$servizio_atteso, "VETRO PAP")

  # due giorni dopo il turno non c'entra più
  dopo <- associa_servizio_atteso_da_calendario(
    letture_associate("2026-03-04 02:00:00", matricola = "103"),
    calendario_prova
  )
  expect_true(is.na(dopo$servizio_atteso))
})

test_that("il turno notturno e quello del mattino dopo si dividono le letture", {
  calendario <- dplyr::tibble(
    giorno = as.Date(c("2026-03-02", "2026-03-03")),
    ora_inizio = c("22:00:00", "08:00:00"),
    ora_fine = c("04:00:00", "12:00:00"),
    matricola_mezzo = "103",
    descrizione_servizio = c("VETRO PAP", "CARTA CONT.STRADALI")
  )
  letture <- letture_associate(
    c(
      "2026-03-03 02:00:00", # dentro il turno notturno
      "2026-03-03 05:00:00", # a 60 minuti dalla fine del notturno, a 180 dal mattino
      "2026-03-03 07:00:00", # a 180 minuti dal notturno, a 60 dal mattino
      "2026-03-03 09:00:00" # dentro il turno del mattino
    ),
    matricola = "103"
  )
  risultato <- associa_servizio_atteso_da_calendario(letture, calendario)
  expect_identical(
    risultato$servizio_atteso,
    c("VETRO PAP", "VETRO PAP", "CARTA CONT.STRADALI", "CARTA CONT.STRADALI")
  )
})

test_that("i turni che finiscono in giornata non passano al giorno dopo", {
  # mezzo 101: il 2 marzo ha solo turni diurni, il 3 marzo nessun turno
  giorno_dopo <- associa_servizio_atteso_da_calendario(
    letture_associate(c("2026-03-03 01:00:00", "2026-03-03 08:30:00")),
    calendario_prova
  )
  expect_true(all(is.na(giorno_dopo$servizio_atteso)))
  # senza `comune_servizio` in ingresso il risultato non ha la colonna `comune`
  expect_setequal(names(giorno_dopo), setdiff(colonne_dataset(), "comune"))
})

test_that("il risultato ha le colonne dell'app e supera la sua validazione", {
  letture <- dplyr::bind_rows(
    letture_grezze("2026-03-02 08:30:00", rfid = "abc123"),
    letture_grezze("2026-03-02 15:00:00", rfid = "0000000001"),
    letture_grezze("2026-03-10 08:30:00", rfid = "abc123"),
    letture_grezze("2026-03-02 09:00:00", rfid = "ffffffffff"),
    # scartate: prima del collaudo e mezzo non collaudato
    letture_grezze("2026-01-20 08:30:00", rfid = "abc123"),
    letture_grezze("2026-03-02 08:30:00", targa = "BB222BB")
  )
  risultato <- letture |>
    filtra_letture_post_test(test_mezzi_prova) |>
    formatta_rfid() |>
    associa_servizio(contenitori_prova, tag_prova, sacchetti_prova) |>
    associa_servizio_atteso_da_calendario(calendario_prova)

  expect_setequal(names(risultato), colonne_dataset())
  expect_equal(nrow(risultato), 4)

  file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(risultato, file, row.names = FALSE)
  esito <- carica_dataset(file)

  expect_length(esito$avvisi, 0)
  expect_equal(nrow(esito$dati), 4)
  caricate <- dplyr::arrange(esito$dati, giorno_lettura)
  expect_identical(
    caricate$RFID,
    c("0000ABC123", "FFFFFFFFFF", "0000000001", "0000ABC123")
  )
  expect_identical(
    caricate$servizio_transponder,
    c("CARTA", NA, "VETRO", "SECCO")
  )
  expect_identical(
    caricate$presente_a_database,
    c("Presente", "Non Presente", "Presente", "Presente")
  )
  expect_equal(caricate$volume_previsto, c(240, NA, 1100, 240))
  # il comune arriva dall'anagrafica dei contenitori: solo per i censiti
  expect_identical(caricate$comune, c("X", NA, "X", "X"))
  # l'orario scritto nel CSV è quello della lettura, senza spostamenti di fuso
  expect_identical(format(caricate$giorno_lettura[1], "%H:%M"), "08:30")
})
