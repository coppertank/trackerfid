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
  comune_servizio = "Rosà"
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
  ),
  # il calendario dice in quale comune lavora ogni giro
  comune = "Bassano del Grappa"
)
# Colonne del CSV dell'app: quelle del file, più il comune di lettura, che
# viene dalla posizione.
colonne_csv <- function() {
  c(
    setdiff(colonne_dataset(), colonne_derivate()),
    "comune_lettura"
  )
}

# Letture già associate al database, pronte per il calendario. Senza un
# rifiuto sono di contenitori non censiti: il loro servizio atteso viene dal
# giro, e mostra quale turno è stato scelto.
letture_associate <- function(quando, matricola = "101", rifiuto = NA_character_) {
  n <- length(quando)
  dplyr::tibble(
    giorno_lettura = ora(quando),
    giorno = as.Date(substr(quando, 1, 10)),
    targa_veicolo = "AA111AA",
    matricola_veicolo = matricola,
    RFID = sprintf("%010d", seq_len(n)),
    latitudine = 45.4642,
    longitudine = 9.19,
    descrizione_rifiuto = rifiuto,
    volume = 240,
    numero_raccolte_annue = 26,
    id_utenza = "U1",
    comune_da_database = "Rosà",
    presente_a_database = if (is.na(rifiuto)) "Non Presente" else "Presente"
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
  # il comune di servizio dell'anagrafica diventa il comune del database:
  # lo hanno solo le letture che trovano un contenitore
  expect_false("comune_servizio" %in% names(associate))
  expect_identical(
    associate$comune_da_database,
    c(rep("Rosà", 3), rep(NA, 3))
  )

  # l'anagrafica dei contenitori deve riportare il comune di servizio
  expect_error(
    associa_servizio(
      letture,
      dplyr::select(contenitori_prova, -"comune_servizio"),
      tag_prova,
      sacchetti_prova
    ),
    "comune_servizio"
  )
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
  # il giro è quello del turno scelto, con il nome del calendario
  expect_identical(
    risultato$servizio_giro,
    rep(c("SECCO PAP", "CARTA/CARTONE PAP"), 3)
  )
  # per un non censito il servizio atteso è il giro, con il nome del database
  expect_identical(
    risultato$servizio_atteso,
    c("SECCO", "CARTA", "SECCO", "CARTA", "SECCO", "CARTA")
  )
  # e l'origine lo dice
  expect_identical(risultato$origine_servizio, rep("Calendario", 6))

  # per un censito il servizio atteso è quello del contenitore a database,
  # qualunque sia il turno: il giro resta quello del calendario
  censiti <- associa_servizio_atteso_da_calendario(
    letture_associate(
      c("2026-03-02 08:30:00", "2026-03-02 15:00:00"),
      rifiuto = "VETRO"
    ),
    calendario_prova
  )
  expect_identical(censiti$servizio_atteso, c("VETRO", "VETRO"))
  expect_identical(censiti$origine_servizio, c("Database", "Database"))
  # il servizio del database non ha una colonna sua
  expect_false("servizio_transponder" %in% names(censiti))
  expect_identical(censiti$servizio_giro, c("SECCO PAP", "CARTA/CARTONE PAP"))
  # anche senza turni: il servizio del contenitore non dipende dal calendario
  senza_turni <- associa_servizio_atteso_da_calendario(
    letture_associate("2026-03-05 08:30:00", rifiuto = "VETRO"),
    calendario_prova
  )
  expect_identical(senza_turni$servizio_atteso, "VETRO")
  expect_identical(senza_turni$origine_servizio, "Database")
  expect_true(is.na(senza_turni$servizio_giro))
})

test_that("a parità di distanza resta il primo turno, e le letture restano in ordine", {
  # 12:30 dista mezz'ora dalla fine del mattino e dall'inizio del pomeriggio
  quando <- c(
    "2026-03-02 15:00:00",
    "2026-03-02 12:30:00",
    "2026-03-02 08:30:00"
  )
  risultato <- associa_servizio_atteso_da_calendario(
    letture_associate(quando),
    calendario_prova
  )
  expect_identical(format(risultato$giorno_lettura, "%H:%M"), substr(quando, 12, 16))
  expect_identical(
    risultato$servizio_atteso,
    c("CARTA", "SECCO", "SECCO")
  )
  # con il pomeriggio scritto per primo nel calendario, vince il pomeriggio
  rovesciato <- associa_servizio_atteso_da_calendario(
    letture_associate(quando),
    calendario_prova[c(2, 1), ]
  )
  expect_identical(rovesciato$servizio_atteso[2], "CARTA")
})

test_that("senza turni il servizio atteso resta vuoto", {
  senza_turni <- associa_servizio_atteso_da_calendario(
    letture_associate("2026-03-02 08:30:00", matricola = "999"),
    calendario_prova
  )
  expect_equal(nrow(senza_turni), 1)
  expect_true(is.na(senza_turni$servizio_atteso))
  expect_true(is.na(senza_turni$origine_servizio))

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
  expect_identical(risultato$servizio_atteso, rep("VETRO", 4))

  # la notte tra l'1 e il 2 marzo non ha turni: vale il turno più vicino dello stesso giorno
  mattina <- associa_servizio_atteso_da_calendario(
    letture_associate("2026-03-02 03:00:00", matricola = "103"),
    calendario_prova
  )
  expect_identical(mattina$servizio_atteso, "VETRO")

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
    descrizione_servizio = c("VETRO PAP", "CARTA CONT.STRADALI"),
    comune = c("Nove", "Cartigliano")
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
    c("VETRO", "VETRO", "CARTA", "CARTA")
  )
  # il giro resta scritto come nel calendario, senza conversioni
  expect_identical(
    risultato$servizio_giro,
    c("VETRO PAP", "VETRO PAP", "CARTA CONT.STRADALI", "CARTA CONT.STRADALI")
  )
  # il comune del giro è quello del turno scelto
  expect_identical(
    risultato$comune_giro,
    c("Nove", "Nove", "Cartigliano", "Cartigliano")
  )
})

test_that("il servizio atteso è il giro del calendario con il nome del database", {
  giri <- c(
    "CARTA/CARTONE PAP",
    "CARTA CONT.STRADALI",
    " secco pap ",
    "UMIDO",
    "SERVIZI MERCATI",
    NA
  )
  expect_identical(
    servizio_da_giro(giri),
    c("CARTA", "CARTA", "SECCO", "UMIDO", "SERVIZI MERCATI", NA)
  )
  # un nome già uniforme non cambia
  expect_identical(servizio_da_giro(c("SECCO", "CARTA")), c("SECCO", "CARTA"))
  expect_identical(servizio_da_giro(character(0)), character(0))
  # un nome che la tabella non conosce resta com'è, con un avviso
  expect_warning(
    ignoto <- servizio_da_giro(c("SECCO PAP", "SPAZZAMENTO NEVE")),
    "SPAZZAMENTO NEVE"
  )
  expect_identical(ignoto, c("SECCO", "SPAZZAMENTO NEVE"))

  # nelle letture il giro resta quello del calendario: la conversione
  # riguarda il solo servizio atteso
  risultato <- associa_servizio_atteso_da_calendario(
    letture_associate(c("2026-03-02 08:30:00", "2026-03-02 15:00:00")),
    calendario_prova
  )
  expect_true(all(
    risultato$servizio_giro %in% calendario_prova$descrizione_servizio
  ))
  expect_identical(
    risultato$servizio_atteso,
    uniforma_servizio(risultato$servizio_giro)
  )
  expect_false(identical(risultato$servizio_atteso, risultato$servizio_giro))
  expect_warning(
    neve <- associa_servizio_atteso_da_calendario(
      letture_associate("2026-03-02 08:30:00"),
      dplyr::mutate(calendario_prova, descrizione_servizio = "SPAZZAMENTO NEVE")
    ),
    "tabella di conversione"
  )
  expect_identical(neve$servizio_giro, "SPAZZAMENTO NEVE")
  expect_identical(neve$servizio_atteso, "SPAZZAMENTO NEVE")
})

test_that("il comune di lettura viene dalla posizione, non dal calendario", {
  comuni <- comuni_cantieri()
  centro <- function(comune) comuni[comuni$comune == comune, ]
  letture <- letture_associate(rep("2026-03-02 08:30:00", 4))
  # a Cittadella, a Rosà, a Milano, e senza posizione
  letture$latitudine <- c(
    centro("CITTADELLA")$latitudine,
    centro("ROSA'")$latitudine,
    45.4642,
    NA
  )
  letture$longitudine <- c(
    centro("CITTADELLA")$longitudine,
    centro("ROSA'")$longitudine,
    9.19,
    NA
  )
  # l'ultimo contenitore non è censito
  letture$presente_a_database[4] <- "Non Presente"
  letture$comune_da_database[4] <- NA
  risultato <- associa_servizio_atteso_da_calendario(letture, calendario_prova)
  # il calendario mandava il mezzo a Bassano del Grappa
  expect_identical(risultato$comune_giro, rep("Bassano del Grappa", 4))
  expect_identical(
    risultato$comune_lettura,
    c("CITTADELLA", "ROSA'", fuori_servizio(), fuori_servizio())
  )
  expect_identical(fuori_servizio(), "FUORI SERVIZIO AZIENDA")
  # il cantiere segue la posizione; fuori dai comuni serviti quello del
  # comune del database, che un non censito non ha
  expect_identical(
    risultato$cantiere,
    c("CAMPOSAMPIERO", "BASSANO", "BASSANO", NA)
  )

  # coordinate ancora testo, con la virgola decimale: stesso risultato, e nel
  # file restano come sono
  testo <- letture
  testo$latitudine <- sub(".", ",", format(letture$latitudine, nsmall = 4), fixed = TRUE)
  testo$longitudine <- as.character(letture$longitudine)
  testo$latitudine[4] <- NA
  da_testo <- associa_servizio_atteso_da_calendario(testo, calendario_prova)
  expect_identical(da_testo$comune_lettura, risultato$comune_lettura)
  expect_identical(da_testo$cantiere, risultato$cantiere)
  expect_identical(da_testo$latitudine, testo$latitudine)
})

test_that("i turni che finiscono in giornata non passano al giorno dopo", {
  # mezzo 101: il 2 marzo ha solo turni diurni, il 3 marzo nessun turno
  giorno_dopo <- associa_servizio_atteso_da_calendario(
    letture_associate(c("2026-03-03 01:00:00", "2026-03-03 08:30:00")),
    calendario_prova
  )
  expect_true(all(is.na(giorno_dopo$servizio_atteso)))
  # senza turni manca anche il comune del giro. Il comune di lettura c'è
  # sempre: qui le letture sono a Milano, e il cantiere viene dal comune del
  # database
  expect_true(all(is.na(giorno_dopo$comune_giro)))
  expect_identical(giorno_dopo$comune_lettura, rep(fuori_servizio(), 2))
  expect_identical(giorno_dopo$cantiere, rep("BASSANO", 2))
  expect_setequal(names(giorno_dopo), colonne_csv())
})

# Letture di un mezzo in un giorno, come escono dall'associazione al
# calendario: `calendario` è il giro che il calendario dava al mezzo. Il
# servizio atteso è quello del database per i censiti, il giro per gli altri.
letture_giro <- function(
  orari,
  rfid,
  presente = "Presente",
  servizio = "SECCO",
  calendario = "CARTA",
  matricola = "101"
) {
  n <- length(orari)
  presente <- rep_len(presente, n)
  dplyr::tibble(
    giorno_lettura = ora(paste("2026-03-02", orari)),
    targa_veicolo = "AA111AA",
    matricola_veicolo = matricola,
    RFID = rfid,
    presente_a_database = presente,
    servizio_atteso = ifelse(
      presente == "Presente",
      rep_len(servizio, n),
      rep_len(calendario, n)
    ),
    origine_servizio = ifelse(presente == "Presente", "Database", "Calendario"),
    servizio_giro = rep_len(calendario, n)
  )
}
ogni_cinque_minuti <- function(n, dalle = "08:00:00") {
  format(ora(paste("2026-03-02", dalle)) + 300 * (seq_len(n) - 1), "%H:%M:%S")
}
# Sei censiti del secco letti tra le 8:00 e le 8:25 da un mezzo a cui il
# calendario dava la carta, e un non censito in mezzo.
giro_sostituito <- function(n_censiti = 6, servizio = "SECCO") {
  dplyr::bind_rows(
    letture_giro(
      ogni_cinque_minuti(n_censiti),
      sprintf("C%02d", seq_len(n_censiti)),
      servizio = servizio
    ),
    letture_giro("08:12:00", "N01", presente = "Non Presente")
  )
}
non_censito <- function(esito) esito[esito$RFID == "N01", ]
# Le stesse letture senza il calendario: manca il giro, e con il giro il
# servizio di chi non ha quello del database.
senza_calendario <- function(letture) {
  senza_database <- letture$origine_servizio != "Database"
  letture$servizio_giro <- NA_character_
  letture$servizio_atteso[senza_database] <- NA_character_
  letture$origine_servizio[senza_database] <- NA_character_
  letture
}

test_that("il servizio atteso di un non censito viene dai censiti letti dallo stesso mezzo", {
  esito <- associa_servizio_atteso_dai_censiti(giro_sostituito())
  expect_equal(nrow(esito), 7)
  # il mezzo ha sostituito quello del secco: il calendario dice carta, i
  # censiti dicono secco
  expect_identical(non_censito(esito)$servizio_atteso, "SECCO")
  expect_identical(non_censito(esito)$origine_servizio, "Censiti")
  # il servizio atteso e il giro non concordano: il mezzo non raccoglieva la
  # carta del calendario
  expect_identical(non_censito(esito)$coerenza, "Non coerente")
  # il giro del calendario resta in una colonna sua, per tutte le letture
  expect_identical(esito$servizio_giro, rep("CARTA", 7))
  # per i censiti il servizio atteso è quello del contenitore. Sono
  # contenitori del secco letti in un giro della carta: non coerenti
  censiti <- esito[esito$presente_a_database == "Presente", ]
  expect_identical(censiti$servizio_atteso, rep("SECCO", 6))
  expect_identical(censiti$coerenza, rep("Non coerente", 6))
  # origine, giro e coerenza stanno dopo il servizio atteso, le altre colonne
  # non cambiano
  expect_identical(
    names(esito),
    c(
      "giorno_lettura",
      "targa_veicolo",
      "matricola_veicolo",
      "RFID",
      "presente_a_database",
      "servizio_atteso",
      "origine_servizio",
      "servizio_giro",
      "coerenza"
    )
  )
  expect_identical(esito$RFID, giro_sostituito()$RFID)
  # il servizio dei censiti resta quello del database
  expect_identical(esito$origine_servizio, c(rep("Database", 6), "Censiti"))

  # quando i censiti confermano il calendario tutto concorda
  confermato <- associa_servizio_atteso_dai_censiti(
    giro_sostituito(servizio = "CARTA")
  )
  expect_identical(non_censito(confermato)$servizio_atteso, "CARTA")
  # il servizio è quello del calendario, ma lo dicono i censiti
  expect_identical(non_censito(confermato)$origine_servizio, "Censiti")
  expect_identical(confermato$coerenza, rep("Coerente", 7))

  # il giro ha il nome del calendario: resta com'è, e il ripiego per il
  # servizio atteso è lo stesso giro con il nome del database
  con_giro <- giro_sostituito(4)
  con_giro$servizio_giro <- "CARTA/CARTONE PAP"
  esito_giro <- associa_servizio_atteso_dai_censiti(con_giro)
  expect_identical(esito_giro$servizio_giro, rep("CARTA/CARTONE PAP", 5))
  expect_identical(esito_giro$servizio_atteso, c(rep("SECCO", 4), "CARTA"))
  expect_identical(non_censito(esito_giro)$origine_servizio, "Calendario")
  # il non censito segue il calendario: coerente. I censiti del secco no
  expect_identical(non_censito(esito_giro)$coerenza, "Coerente")
  expect_identical(
    esito_giro$coerenza[esito_giro$presente_a_database == "Presente"],
    rep("Non coerente", 4)
  )
  # con abbastanza censiti il servizio atteso viene da loro, il giro no
  con_giro <- giro_sostituito()
  con_giro$servizio_giro <- "CARTA/CARTONE PAP"
  esito_giro <- associa_servizio_atteso_dai_censiti(con_giro)
  expect_identical(non_censito(esito_giro)$servizio_atteso, "SECCO")
  expect_identical(non_censito(esito_giro)$servizio_giro, "CARTA/CARTONE PAP")
  expect_identical(non_censito(esito_giro)$coerenza, "Non coerente")
})

test_that("con pochi censiti, o con censiti misti, resta il servizio del calendario", {
  # quattro censiti non bastano, cinque sì
  pochi <- associa_servizio_atteso_dai_censiti(giro_sostituito(4))
  expect_identical(non_censito(pochi)$servizio_atteso, "CARTA")
  expect_identical(non_censito(pochi)$origine_servizio, "Calendario")
  expect_identical(non_censito(pochi)$coerenza, "Coerente")
  expect_identical(pochi$servizio_giro, rep("CARTA", 5))
  cinque <- associa_servizio_atteso_dai_censiti(giro_sostituito(5))
  expect_identical(non_censito(cinque)$servizio_atteso, "SECCO")
  # la soglia sul numero si può cambiare
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(
      giro_sostituito(4),
      minimo_censiti = 4
    ))$servizio_atteso,
    "SECCO"
  )

  # quattro su cinque dello stesso servizio sono l'80%: basta
  misto <- function(servizi) {
    associa_servizio_atteso_dai_censiti(giro_sostituito(
      length(servizi),
      servizio = servizi
    ))
  }
  quasi <- misto(c("SECCO", "SECCO", "UMIDO", "SECCO", "SECCO"))
  expect_identical(non_censito(quasi)$servizio_atteso, "SECCO")
  expect_identical(non_censito(quasi)$coerenza, "Non coerente")
  # tre su cinque no: come vicino al passaggio da un giro all'altro
  diviso <- misto(c("SECCO", "SECCO", "UMIDO", "UMIDO", "SECCO"))
  expect_identical(non_censito(diviso)$servizio_atteso, "CARTA")
  expect_identical(non_censito(diviso)$coerenza, "Coerente")
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(
      giro_sostituito(5, servizio = c("SECCO", "SECCO", "UMIDO", "UMIDO", "SECCO")),
      soglia = 0.6
    ))$servizio_atteso,
    "SECCO"
  )

  # un contenitore letto sei volte vota una volta sola
  ripetuto <- dplyr::bind_rows(
    letture_giro(ogni_cinque_minuti(6), "C01"),
    letture_giro("08:12:00", "N01", presente = "Non Presente")
  )
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(ripetuto))$servizio_atteso,
    "CARTA"
  )
  # un censito senza servizio a database, come un sacchetto, non vota: il suo
  # servizio è quello del giro, e resta quello
  senza_servizio <- giro_sostituito()
  senza_servizio$servizio_atteso <- "CARTA"
  senza_servizio$origine_servizio <- "Calendario"
  esito <- associa_servizio_atteso_dai_censiti(senza_servizio)
  expect_identical(esito$servizio_atteso, rep("CARTA", 7))
  expect_identical(esito$origine_servizio, rep("Calendario", 7))
  # se le letture non hanno la colonna dell'origine, il servizio di un
  # censito vale come quello del database
  senza_origine <- giro_sostituito()
  senza_origine$origine_servizio <- NULL
  expect_identical(
    associa_servizio_atteso_dai_censiti(senza_origine),
    associa_servizio_atteso_dai_censiti(giro_sostituito())
  )
})

test_that("votano solo i censiti dello stesso mezzo, nella mezz'ora attorno alla lettura", {
  # i censiti sono stati letti tra le 7:00 e le 7:25, il non censito alle 8:12
  lontani <- dplyr::bind_rows(
    letture_giro(ogni_cinque_minuti(6, "07:00:00"), sprintf("C%02d", 1:6)),
    letture_giro("08:12:00", "N01", presente = "Non Presente")
  )
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(lontani))$servizio_atteso,
    "CARTA"
  )
  # con una finestra più larga votano
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(lontani, minuti = 75))$servizio_atteso,
    "SECCO"
  )
  # il limite della mezz'ora è compreso: cinque censiti tra le 7:42 e le 8:02,
  # il primo a trenta minuti esatti
  al_limite <- dplyr::bind_rows(
    letture_giro(ogni_cinque_minuti(5, "07:42:00"), sprintf("C%02d", 1:5)),
    letture_giro("08:12:00", "N01", presente = "Non Presente")
  )
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(al_limite))$servizio_atteso,
    "SECCO"
  )
  al_limite$giorno_lettura[1] <- al_limite$giorno_lettura[1] - 1
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(al_limite))$servizio_atteso,
    "CARTA"
  )

  # i censiti letti da un altro mezzo non votano
  altro_mezzo <- dplyr::bind_rows(
    letture_giro(ogni_cinque_minuti(6), sprintf("C%02d", 1:6), matricola = "102"),
    letture_giro("08:12:00", "N01", presente = "Non Presente")
  )
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(altro_mezzo))$servizio_atteso,
    "CARTA"
  )
  # senza il mezzo una lettura non si può stimare
  senza_mezzo <- giro_sostituito()
  senza_mezzo$matricola_veicolo[senza_mezzo$RFID == "N01"] <- NA
  expect_identical(
    non_censito(associa_servizio_atteso_dai_censiti(senza_mezzo))$servizio_atteso,
    "CARTA"
  )
})

test_that("un mezzo che fa due giri nello stesso giorno dà a ognuno il suo servizio", {
  # al mattino il secco, al pomeriggio l'umido; il calendario dava la carta
  due_giri <- dplyr::bind_rows(
    letture_giro(ogni_cinque_minuti(6, "08:00:00"), sprintf("S%02d", 1:6)),
    letture_giro(
      ogni_cinque_minuti(6, "14:00:00"),
      sprintf("U%02d", 1:6),
      servizio = "UMIDO"
    ),
    letture_giro(
      c("08:12:00", "14:12:00", "11:00:00"),
      c("N01", "N02", "N03"),
      presente = "Non Presente"
    )
  )
  esito <- associa_servizio_atteso_dai_censiti(due_giri)
  per_rfid <- function(rfid, colonna) esito[[colonna]][esito$RFID == rfid]
  expect_identical(per_rfid("N01", "servizio_atteso"), "SECCO")
  expect_identical(per_rfid("N02", "servizio_atteso"), "UMIDO")
  # tra un giro e l'altro non c'è nessun censito vicino: resta il calendario
  expect_identical(per_rfid("N03", "servizio_atteso"), "CARTA")
  expect_identical(per_rfid("N03", "coerenza"), "Coerente")
  # negli altri due il servizio dei censiti non è la carta del calendario
  expect_identical(per_rfid("N01", "coerenza"), "Non coerente")
  # l'ordine delle righe non conta, e nel risultato resta quello di partenza
  rovesciato <- associa_servizio_atteso_dai_censiti(due_giri[rev(seq_len(nrow(due_giri))), ])
  expect_identical(rovesciato$RFID, rev(due_giri$RFID))
  expect_identical(rovesciato$servizio_atteso, rev(esito$servizio_atteso))
})

test_that("senza calendario e senza censiti il servizio atteso di un non censito resta vuoto", {
  esito <- associa_servizio_atteso_dai_censiti(
    senza_calendario(giro_sostituito(3))
  )
  expect_true(is.na(non_censito(esito)$servizio_atteso))
  # senza servizio non c'è nemmeno un'origine
  expect_true(is.na(non_censito(esito)$origine_servizio))
  # i censiti hanno comunque il servizio del loro contenitore
  expect_identical(
    esito$servizio_atteso[esito$presente_a_database == "Presente"],
    rep("SECCO", 3)
  )
  expect_true(all(is.na(esito$servizio_giro)))
  # senza il giro la coerenza non si può dire
  expect_true(all(is.na(esito$coerenza)))
  # senza calendario, ma con abbastanza censiti, il servizio viene da loro
  esito <- associa_servizio_atteso_dai_censiti(
    senza_calendario(giro_sostituito())
  )
  expect_identical(non_censito(esito)$servizio_atteso, "SECCO")
  expect_identical(non_censito(esito)$origine_servizio, "Censiti")
  expect_true(is.na(non_censito(esito)$servizio_giro))

  # applicata due volte dà lo stesso risultato: il servizio del calendario
  # non viene sostituito da quello stimato
  una_volta <- associa_servizio_atteso_dai_censiti(giro_sostituito())
  expect_identical(associa_servizio_atteso_dai_censiti(una_volta), una_volta)
  # senza letture
  vuota <- associa_servizio_atteso_dai_censiti(giro_sostituito()[0, ])
  expect_equal(nrow(vuota), 0)
  expect_true(all(
    c("origine_servizio", "servizio_giro", "coerenza") %in% names(vuota)
  ))
})

test_that("il calendario deve riportare il comune di ogni giro", {
  expect_error(
    associa_servizio_atteso_da_calendario(
      letture_associate("2026-03-02 08:30:00"),
      dplyr::select(calendario_prova, -"comune")
    ),
    "comune"
  )
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
    associa_servizio_atteso_da_calendario(calendario_prova) |>
    associa_servizio_atteso_dai_censiti()

  # le colonne del CSV: il comune assegnato lo calcola l'app. L'ultimo passo
  # aggiunge la coerenza tra i servizi
  expect_setequal(names(risultato), c(colonne_csv(), "coerenza"))
  # per i censiti il servizio atteso è quello del database. Le letture di
  # prova sono poche: per il non censito resta il giro, con il nome del database
  censita <- risultato$presente_a_database == "Presente"
  expect_identical(
    risultato$origine_servizio,
    ifelse(censita, "Database", "Calendario")
  )
  expect_identical(
    risultato$servizio_atteso[!censita],
    uniforma_servizio(risultato$servizio_giro[!censita])
  )
  expect_false("servizio_transponder" %in% names(risultato))
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
    caricate$origine_servizio,
    c("Database", "Calendario", "Database", "Database")
  )
  expect_identical(
    servizio_a_database(caricate),
    c("CARTA", NA, "VETRO", "SECCO")
  )
  expect_identical(
    caricate$presente_a_database,
    c("Presente", "Non Presente", "Presente", "Presente")
  )
  expect_equal(caricate$volume_previsto, c(240, NA, 1100, 240))
  # il servizio atteso: quello del contenitore per i censiti, il giro con il
  # nome del database per il non censito
  expect_identical(
    caricate$servizio_atteso,
    c("CARTA", "SECCO", "VETRO", "SECCO")
  )
  # l'app tiene il giro con il nome del calendario
  expect_identical(
    caricate$servizio_giro,
    c("SECCO PAP", "SECCO PAP", "CARTA/CARTONE PAP", NA)
  )
  # la coerenza del file è la stessa che l'app ricalcola: un contenitore della
  # carta e uno del vetro letti in giri di un altro rifiuto
  expect_identical(
    caricate$coerenza,
    c("Non coerente", "Coerente", "Non coerente", NA)
  )
  expect_identical(
    dplyr::arrange(risultato, giorno_lettura)$coerenza,
    caricate$coerenza
  )
  # il comune del database arriva dall'anagrafica dei contenitori, solo per i
  # censiti; quello del giro dal calendario del mezzo
  expect_identical(
    caricate$comune_da_database,
    c("ROSA'", NA, "ROSA'", "ROSA'")
  )
  expect_identical(
    caricate$comune_giro,
    c("BASSANO DEL GRAPPA", "BASSANO DEL GRAPPA", "BASSANO DEL GRAPPA", NA)
  )
  # le letture di prova sono a Milano: il comune di lettura è lo stesso nel
  # CSV e nell'app, e senza comune del database non c'è cantiere
  expect_identical(risultato$comune_lettura, rep(fuori_servizio(), 4))
  expect_identical(caricate$comune_lettura, rep(fuori_servizio(), 4))
  expect_identical(caricate$cantiere, c("BASSANO", NA, "BASSANO", "BASSANO"))
  expect_identical(
    caricate$comune_assegnato,
    c("ROSA'", NA, "ROSA'", "ROSA'")
  )
  # l'orario scritto nel CSV è quello della lettura, senza spostamenti di fuso
  expect_identical(format(caricate$giorno_lettura[1], "%H:%M"), "08:30")
})
