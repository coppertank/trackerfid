# Tabelle di prova nella forma di quelle estratte dai sistemi aziendali e
# ripulite da `R/fct_importa_tabelle.R`.

ora <- function(testo) as.POSIXct(testo)

# Letture delle antenne: il comune di lettura arriva dal sistema di
# localizzazione dei mezzi, il cantiere dalla tabella dei comuni.
letture_grezze <- function(quando, targa = "AA111AA", rfid = "abc123") {
  dplyr::tibble(
    giorno_lettura = ora(quando),
    giorno = as.Date(substr(quando, 1, 10)),
    targa_veicolo = targa,
    RFID = rfid,
    comune_lettura = "ROSÀ",
    cantiere = "BASSANO",
    NumeroLetture = "1",
    latitudine = 45.7244,
    longitudine = 11.7627
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
  servizio_transponder = c("CARTA", "SECCO", "VETRO"),
  volume = c(240, 240, 1100),
  frequenza = "QUINDICINALE",
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
  # la fornitura lega il contenitore all'utenza: il 10 marzo il contenitore
  # passa a un'altra fornitura, di un'altra utenza
  id_fornitura = c("F1", "F2", "F3"),
  stato_fornitura = "APERTO",
  id_utenza = c("U1", "U2", "U3"),
  comune_fornitura = "ROSÀ",
  codice_categoria = "UD"
)
# un sacchetto ha solo la fornitura del suo gruppo
sacchetti_prova <- dplyr::tibble(
  codice_uhf = "000000000000012345678901",
  id_fornitura = "S9"
)

calendario_prova <- dplyr::tibble(
  giorno = as.Date("2026-03-02"),
  ora_inizio = c("06:00:00", "13:00", "06:00:00", "22:00:00"),
  ora_fine = c("12:00:00", "18:00", "12:00:00", "04:00:00"),
  matricola_mezzo = c("101", "101", "101", "103"),
  # il terzo turno ripete il primo
  id_giro = c(11, 12, 11, 13),
  eseguito = "SI",
  descrizione_servizio = c(
    "SECCO PAP",
    "CARTA/CARTONE PAP",
    "SECCO PAP",
    "VETRO PAP"
  ),
  # il calendario dice in quale comune lavora ogni giro
  comune_giro = "BASSANO DEL GRAPPA"
)

# Colonne del CSV dell'app, nell'ordine in cui le scrive la pipeline.
colonne_passo_3 <- c(
  "giorno_lettura",
  "giorno",
  "targa_veicolo",
  "matricola_veicolo",
  "id_giro",
  "eseguito",
  "RFID",
  "presente_a_database",
  "servizio_transponder",
  "servizio_atteso",
  "coerenza_servizio",
  "servizio_giro",
  "volume",
  "frequenza",
  "numero_raccolte_annue",
  "codice_categoria",
  "comune_fornitura",
  "comune_giro",
  "comune_lettura",
  "coerenza_comune",
  "cantiere",
  "data_attivazione_contenitore",
  "data_cessazione_contenitore",
  "id_fornitura",
  "stato_fornitura",
  "id_utenza",
  "latitudine",
  "longitudine"
)

# Letture già associate al database, pronte per il calendario. Senza un
# rifiuto sono di contenitori non censiti.
letture_associate <- function(
  quando,
  matricola = "101",
  rifiuto = NA_character_
) {
  n <- length(quando)
  rifiuto <- rep_len(rifiuto, n)
  censito <- !is.na(rifiuto)
  dplyr::tibble(
    giorno_lettura = ora(quando),
    giorno = as.Date(substr(quando, 1, 10)),
    targa_veicolo = "AA111AA",
    matricola_veicolo = matricola,
    RFID = sprintf("%010d", seq_len(n)),
    comune_lettura = "ROSÀ",
    cantiere = "BASSANO",
    NumeroLetture = "1",
    latitudine = 45.7244,
    longitudine = 11.7627,
    servizio_transponder = rifiuto,
    volume = ifelse(censito, 240, NA_real_),
    frequenza = ifelse(censito, "QUINDICINALE", NA_character_),
    numero_raccolte_annue = ifelse(censito, 26, NA_real_),
    data_attivazione_contenitore = as.Date(ifelse(censito, "2025-01-01", NA)),
    data_cessazione_contenitore = as.Date(ifelse(censito, "2099-12-31", NA)),
    id_fornitura = ifelse(censito, "F1", NA_character_),
    stato_fornitura = ifelse(censito, "APERTO", NA_character_),
    id_utenza = ifelse(censito, "U1", NA_character_),
    codice_categoria = ifelse(censito, "UD", NA_character_),
    presente_a_database = ifelse(censito, "Presente", "Non Presente"),
    comune_fornitura = ifelse(censito, "ROSÀ", NA_character_)
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
  # il comune di lettura e il cantiere arrivano con le letture
  expect_identical(
    names(valide),
    c(
      "giorno_lettura",
      "giorno",
      "targa_veicolo",
      "matricola_veicolo",
      "RFID",
      "comune_lettura",
      "cantiere",
      "NumeroLetture",
      "latitudine",
      "longitudine"
    )
  )
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
    # sconosciuto
    letture_grezze("2026-03-02 11:00:00", rfid = "FFFFFFFFFF")
  )
  associate <- associa_servizio(letture, contenitori_prova, sacchetti_prova)

  expect_equal(nrow(associate), nrow(letture))
  expect_identical(
    associate$servizio_transponder,
    c("CARTA", "CARTA", "SECCO", NA, NA)
  )
  # censito vuol dire legato a una fornitura: quella del contenitore o, per
  # un sacchetto, quella del suo gruppo
  expect_identical(associate$id_fornitura, c("F1", "F1", "F2", "S9", NA))
  expect_identical(
    associate$presente_a_database,
    c(rep("Presente", 4), "Non Presente")
  )
  # l'utenza viene dalla fornitura del contenitore: un sacchetto non ce l'ha
  expect_identical(associate$id_utenza, c("U1", "U1", "U2", NA, NA))
  # i dati del contenitore e della fornitura li hanno solo le letture che
  # trovano il contenitore
  expect_equal(associate$volume, c(240, 240, 240, NA, NA))
  expect_identical(
    associate$stato_fornitura,
    c(rep("APERTO", 3), NA, NA)
  )
  expect_identical(
    associate$comune_fornitura,
    c(rep("ROSÀ", 3), NA, NA)
  )
  expect_false("fornitura_sacchetto" %in% names(associate))
  expect_false("presente_in_tag_contenitori" %in% names(associate))

  # le due tabelle possono scrivere la fornitura come numero o come testo
  numeriche <- contenitori_prova
  numeriche$id_fornitura <- c(101, 102, 103)
  expect_identical(
    associa_servizio(letture, numeriche, sacchetti_prova)$id_fornitura,
    c("101", "101", "102", "S9", NA)
  )

  # l'anagrafica dei contenitori deve riportare la fornitura
  expect_error(
    associa_servizio(
      letture,
      dplyr::select(contenitori_prova, -"id_fornitura"),
      sacchetti_prova
    ),
    "id_fornitura"
  )
})

test_that("il giro è il turno in cui cade la lettura, o il più vicino", {
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
  expect_identical(names(risultato), colonne_passo_3)
  # il giro è quello del turno scelto, con il nome del calendario
  expect_identical(
    risultato$servizio_giro,
    rep(c("SECCO PAP", "CARTA/CARTONE PAP"), 3)
  )
  expect_equal(risultato$id_giro, rep(c(11, 12), 3))
  expect_identical(risultato$eseguito, rep("SI", 6))
  expect_identical(risultato$comune_giro, rep("BASSANO DEL GRAPPA", 6))
  # il servizio atteso è il giro, con il nome del database
  expect_identical(
    risultato$servizio_atteso,
    rep(c("SECCO", "CARTA"), 3)
  )
  # un non censito non ha un servizio a database: la coerenza manca
  expect_true(all(is.na(risultato$coerenza_servizio)))
  # i non censiti sono letti a Rosà da un giro di Bassano del Grappa
  expect_identical(risultato$coerenza_comune, rep("Fuori giro", 6))

  # per un censito il servizio atteso è comunque quello del giro, e la
  # coerenza lo confronta con il servizio a database
  censiti <- associa_servizio_atteso_da_calendario(
    letture_associate(
      c("2026-03-02 08:30:00", "2026-03-02 15:00:00"),
      rifiuto = "CARTA"
    ),
    calendario_prova
  )
  expect_identical(censiti$servizio_transponder, c("CARTA", "CARTA"))
  expect_identical(censiti$servizio_atteso, c("SECCO", "CARTA"))
  expect_identical(censiti$coerenza_servizio, c("NO", "SI"))
  expect_identical(censiti$servizio_giro, c("SECCO PAP", "CARTA/CARTONE PAP"))
  # per un censito il comune di lettura si confronta con quello del
  # database, non con quello del giro
  expect_identical(censiti$coerenza_comune, c("Coerente", "Coerente"))
  spostato <- letture_associate("2026-03-02 08:30:00", rifiuto = "CARTA")
  spostato$comune_fornitura <- "CASSOLA"
  expect_identical(
    associa_servizio_atteso_da_calendario(spostato, calendario_prova)$coerenza_comune,
    "Fuori comune"
  )
  # senza turni il giro manca, e con lui il servizio atteso e la coerenza
  # del servizio; la coerenza del comune manca solo al non censito, che non
  # ha un comune dichiarato
  senza_turni <- associa_servizio_atteso_da_calendario(
    dplyr::bind_rows(
      letture_associate("2026-03-05 08:30:00", rifiuto = "VETRO"),
      letture_associate("2026-03-05 09:30:00")
    ),
    calendario_prova
  )
  expect_identical(senza_turni$servizio_transponder, c("VETRO", NA))
  expect_true(all(is.na(senza_turni$servizio_giro)))
  expect_true(all(is.na(senza_turni$servizio_atteso)))
  expect_true(all(is.na(senza_turni$coerenza_servizio)))
  expect_identical(
    senza_turni$coerenza_comune,
    c("Coerente", "Coerenza ignota")
  )
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
  expect_true(is.na(senza_turni$id_giro))

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
  expect_equal(risultato$id_giro, rep(13, 4))

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
    id_giro = c(31, 32),
    eseguito = "SI",
    descrizione_servizio = c("VETRO PAP", "CARTA CONT.STRADALI"),
    comune_giro = c("NOVE", "CARTIGLIANO")
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
    c("NOVE", "NOVE", "CARTIGLIANO", "CARTIGLIANO")
  )
})

test_that("i turni che finiscono in giornata non passano al giorno dopo", {
  # mezzo 101: il 2 marzo ha solo turni diurni, il 3 marzo nessun turno
  giorno_dopo <- associa_servizio_atteso_da_calendario(
    letture_associate(c("2026-03-03 01:00:00", "2026-03-03 08:30:00")),
    calendario_prova
  )
  expect_true(all(is.na(giorno_dopo$servizio_atteso)))
  expect_true(all(is.na(giorno_dopo$comune_giro)))
  # comune di lettura e cantiere arrivano con le letture, giro o no
  expect_identical(giorno_dopo$comune_lettura, rep("ROSÀ", 2))
  expect_identical(giorno_dopo$cantiere, rep("BASSANO", 2))
})

test_that("il servizio atteso è il giro del calendario con il nome del database", {
  giri <- c(
    "CARTA/CARTONE PAP",
    "CARTA CONT.STRADALI",
    " secco pap ",
    "UMIDO",
    "SERVIZI MERCATI",
    "PULIZIA TERRIT.",
    NA
  )
  # i giri che non raccolgono un rifiuto prendono il nome della tipologia
  expect_identical(
    servizio_da_giro(giri),
    c("CARTA", "CARTA", "SECCO", "UMIDO", "SERVIZI", "PULIZIA TERRITORIO", NA)
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

test_that("nei giri dei servizi il servizio atteso viene dai censiti letti nel giro", {
  calendario <- dplyr::tibble(
    giorno = as.Date(c("2026-03-02", "2026-03-03", "2026-03-02", "2026-03-02")),
    ora_inizio = "06:00:00",
    ora_fine = "12:00:00",
    matricola_mezzo = c("101", "101", "102", "103"),
    id_giro = c(21, 21, 22, 23),
    eseguito = "SI",
    descrizione_servizio = c(
      "SERVIZI MERCATI",
      "SERVIZI MERCATI",
      "SERV.MANIFESTAZIONE",
      "SECCO PAP"
    ),
    comune_giro = "BASSANO DEL GRAPPA"
  )
  letture <- dplyr::bind_rows(
    # il mercato del 2 marzo: tre contenitori della carta, uno del secco e un
    # non censito
    letture_associate(
      paste("2026-03-02", c("07:00:00", "07:10:00", "07:20:00", "07:30:00", "07:40:00")),
      rifiuto = c("CARTA", "CARTA", "SECCO", "CARTA", NA)
    ),
    # lo stesso giro il giorno dopo: tanta carta quanto secco
    letture_associate(
      paste("2026-03-03", c("07:00:00", "07:10:00")),
      rifiuto = c("CARTA", "SECCO")
    ),
    # una manifestazione con i soli non censiti
    letture_associate("2026-03-02 08:00:00", matricola = "102"),
    # un giro del secco con un contenitore della carta: vale il giro
    letture_associate("2026-03-02 08:00:00", matricola = "103", rifiuto = "CARTA")
  )
  risultato <- associa_servizio_atteso_da_calendario(letture, calendario)
  expect_identical(
    risultato$servizio_atteso,
    c(rep("CARTA", 5), "SERVIZI", "SERVIZI", "SERVIZI", "SECCO")
  )
  # il giro resta quello del calendario
  expect_identical(
    risultato$servizio_giro,
    c(rep("SERVIZI MERCATI", 7), "SERV.MANIFESTAZIONE", "SECCO PAP")
  )
  expect_identical(
    risultato$coerenza_servizio,
    c("SI", "SI", "NO", "SI", NA, "NO", "NO", NA, "NO")
  )
  # senza giri dei servizi vale sempre il giro
  senza <- associa_servizio_atteso_da_calendario(
    letture,
    calendario,
    servizi_applica_moda = character(0)
  )
  expect_identical(
    senza$servizio_atteso,
    c(rep("SERVIZI", 8), "SECCO")
  )
  # i giri dei servizi sono quelli della tipologia "Servizi"
  config <- servizi_config()
  expect_true(all(
    config$tipologia[match(giri_servizio_dai_censiti(), config$servizio)] ==
      "Servizi"
  ))

  # la regola da sola, sulle colonne che le servono
  expect_identical(
    servizio_atteso_dal_giro(dplyr::tibble(
      giorno = as.Date("2026-03-02"),
      id_giro = c(1, 1, 1, NA),
      servizio_giro = c("SERVIZI MERCATI", "SERVIZI MERCATI", "SERVIZI MERCATI", NA),
      servizio_transponder = c("VETRO", "VETRO", NA, "VETRO")
    )),
    c("VETRO", "VETRO", "VETRO", NA)
  )
  # il valore più frequente di ogni gruppo, se è uno solo
  expect_identical(
    moda_unica_per_gruppo(
      c("A", "A", "B", "A", "B", NA, "", "C"),
      c(1, 1, 1, 2, 2, 3, 3, 4)
    ),
    c("A", "A", "A", NA, NA, NA, NA, "C")
  )
})

test_that("il calendario deve riportare il comune di ogni giro", {
  expect_error(
    associa_servizio_atteso_da_calendario(
      letture_associate("2026-03-02 08:30:00"),
      dplyr::select(calendario_prova, -"comune_giro")
    ),
    "comune_giro"
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
    associa_servizio(contenitori_prova, sacchetti_prova) |>
    associa_servizio_atteso_da_calendario(calendario_prova)

  expect_identical(names(risultato), colonne_passo_3)
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
  # volume e raccolte arrivano con i nomi dell'anagrafica
  expect_equal(caricate$volume_previsto, c(240, NA, 1100, 240))
  expect_equal(caricate$numero_raccolte_annue_previste, c(26, NA, 13, 26))
  # il servizio atteso è il giro con il nome del database; il 10 marzo il
  # mezzo non aveva turni
  expect_identical(
    caricate$servizio_atteso,
    c("SECCO", "SECCO", "CARTA", NA)
  )
  # l'app tiene il giro con il nome del calendario
  expect_identical(
    caricate$servizio_giro,
    c("SECCO PAP", "SECCO PAP", "CARTA/CARTONE PAP", NA)
  )
  # la coerenza del file è la stessa che l'app ricalcola: un contenitore della
  # carta e uno del vetro letti in giri di un altro rifiuto
  expect_identical(
    caricate$coerenza_servizio,
    c("NO", NA, "NO", NA)
  )
  expect_identical(
    dplyr::arrange(risultato, giorno_lettura)$coerenza_servizio,
    caricate$coerenza_servizio
  )
  # il comune della fornitura arriva dall'anagrafica dei contenitori, solo
  # per i censiti; quello del giro dal calendario del mezzo; quello di
  # lettura con le letture
  expect_identical(
    caricate$comune_fornitura,
    c("ROSÀ", NA, "ROSÀ", "ROSÀ")
  )
  expect_identical(
    caricate$comune_giro,
    c("BASSANO DEL GRAPPA", "BASSANO DEL GRAPPA", "BASSANO DEL GRAPPA", NA)
  )
  expect_identical(caricate$comune_lettura, rep("ROSÀ", 4))
  expect_identical(caricate$cantiere, rep("BASSANO", 4))
})

test_that("i CSV estratti si leggono come testo, con il separatore del file", {
  virgola <- scrivi_csv(c("a,b,c", "1,2,3"))
  punto_virgola <- scrivi_csv(c("RFID;giorno_lettura", "0000012345; 2025-01-10 08:00:00", ";"))
  expect_identical(rileva_separatore(virgola), ",")
  expect_identical(rileva_separatore(punto_virgola), ";")

  letto <- leggi_tabella_origine(punto_virgola)
  expect_identical(names(letto), c("RFID", "giorno_lettura"))
  # gli zeri iniziali restano, gli spazi ai margini no, i vuoti mancano
  expect_identical(letto$RFID, c("0000012345", NA))
  expect_identical(letto$giorno_lettura, c("2025-01-10 08:00:00", NA))
  expect_s3_class(letto, "data.frame")
  expect_true(all(vapply(leggi_tabella_origine(virgola), is.character, logical(1))))

  vuoto <- scrivi_csv("")
  expect_error(leggi_tabella_origine(vuoto), class = "errore_validazione")
})

test_that("nucleo e categoria sono quelli in vigore al momento della lettura", {
  letture <- data.frame(
    id_fornitura = c("F1", "F1", "F1", "F2", NA),
    giorno_lettura = as.POSIXct(
      c("2026-03-01 08:00:00", "2026-07-01 08:00:00", "2026-12-01 08:00:00", "2026-05-01 08:00:00", "2026-05-01 08:00:00"),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  componenti <- data.frame(
    # F1 ha 2 residenti fino a metà giugno, poi 3; F2 ha una decorrenza finita
    id_fornitura = c("F1", "F1", "F2"),
    data_inizio_decorrenza = as.POSIXct(c("2020-01-01", "2026-06-15", "2020-01-01"), tz = "UTC"),
    data_fine_decorrenza = as.POSIXct(c("2026-06-14", NA, "2026-01-01"), tz = "UTC"),
    nucleo_residenti = c(2L, 3L, 4L),
    nucleo_non_residenti = c(0L, 1L, 0L),
    nucleo_totale = c(2L, 4L, 4L),
    tipo_categoria = "DOMESTICA",
    categoria = "ABITAZIONE",
    sottocategoria = "RESIDENTE",
    stringsAsFactors = FALSE
  )
  esito <- associa_componenti(letture, componenti)
  expect_identical(names(esito), c(names(letture), colonne_componenti()))
  expect_identical(esito$nucleo_residenti, c(2L, 3L, 3L, NA, NA))
  expect_identical(esito$nucleo_totale, c(2L, 4L, 4L, NA, NA))
  expect_identical(esito$tipo_categoria, c("DOMESTICA", "DOMESTICA", "DOMESTICA", NA, NA))
  # una colonna che la tabella non ha resta vuota
  senza <- associa_componenti(letture, componenti[names(componenti) != "sottocategoria"])
  expect_true(all(is.na(senza$sottocategoria)))
  expect_identical(colonne_nucleo_manovra(), c(colonne_componenti(), "potenziamento_secco"))
})

test_that("il potenziamento vale se la fornitura ha almeno una manovra in vigore", {
  letture <- data.frame(
    id_fornitura = c("F1", "F1", "F1", "F1", "F2", NA),
    giorno = as.Date(c("2026-03-01", "2026-06-01", "2026-07-15", "2026-12-01", "2026-05-01", "2026-05-01")),
    stringsAsFactors = FALSE
  )
  manovre <- data.frame(
    # F1: due dichiarazioni sovrapposte da giugno, l'ultima ancora aperta
    id_fornitura = c("F1", "F1"),
    data_inizio_dichiarazione = as.Date(c("2026-06-01", "2026-07-01")),
    data_fine_dichiarazione = as.Date(c("2026-07-31", NA)),
    stringsAsFactors = FALSE
  )
  esito <- associa_potenziamento(letture, manovre)
  expect_identical(names(esito), c(names(letture), "potenziamento_secco"))
  # il primo giorno conta; con due dichiarazioni basta una; senza fornitura manca
  expect_identical(esito$potenziamento_secco, c(FALSE, TRUE, TRUE, TRUE, FALSE, NA))
  expect_no_warning(associa_potenziamento(letture, manovre))

  # una dichiarazione che finisce prima di cominciare: annullata, non conta
  annullata <- rbind(manovre, data.frame(
    id_fornitura = "F2",
    data_inizio_dichiarazione = as.Date("2026-04-01"),
    data_fine_dichiarazione = as.Date("2026-01-01")
  ))
  expect_warning(
    con_annullata <- associa_potenziamento(letture, annullata),
    "1 dichiarazioni di manovra sociale finiscono prima di cominciare.*Forniture: F2"
  )
  expect_identical(con_annullata$potenziamento_secco, esito$potenziamento_secco)
  # senza dichiarazioni nessuna fornitura ha il potenziamento
  expect_identical(associa_potenziamento(letture, manovre[0, ])$potenziamento_secco, c(rep(FALSE, 5), NA))
})
