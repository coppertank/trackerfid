# Tabelle di prova nella forma di quelle estratte dai sistemi aziendali e
# ripulite da `R/fct_importa_tabelle.R`.

ora <- function(testo) as.POSIXct(testo)

# Letture delle antenne: il comune di lettura arriva dal sistema di
# localizzazione dei mezzi, il cantiere dalla tabella dei comuni.
letture_grezze <- function(quando, targa = "AA111AA", rfid = "abc123") {
  dplyr::tibble(
    data_ora_lettura = ora(quando),
    data_lettura = as.Date(substr(quando, 1, 10)),
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
  stato_fornitura = "ATTIVA",
  id_utenza = c("U1", "U2", "U3"),
  comune_fornitura = "ROSÀ"
)
# un sacchetto ha solo la fornitura del suo gruppo
sacchetti_prova <- dplyr::tibble(
  codice_uhf = "000000000000012345678901",
  id_fornitura = "S9"
)

calendario_prova <- dplyr::tibble(
  data_giro = as.Date("2026-03-02"),
  ora_inizio = c("06:00:00", "13:00", "06:00:00", "08:00"),
  ora_fine = c("12:00:00", "18:00", "12:00:00", "12:00"),
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
  "data_ora_lettura",
  "data_lettura",
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
  "comune_fornitura",
  "comune_giro",
  "comune_lettura",
  "coerenza_comune",
  "distanza_comune_dichiarato",
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
    data_ora_lettura = ora(quando),
    data_lettura = as.Date(substr(quando, 1, 10)),
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
    stato_fornitura = ifelse(censito, "ATTIVA", NA_character_),
    id_utenza = ifelse(censito, "U1", NA_character_),
    presente_a_database = ifelse(censito, "PIA", "NP"),
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
  valide <- filtra_letture_dopo_il_collaudo(letture, test_mezzi_prova)

  expect_equal(nrow(valide), 1)
  expect_identical(valide$data_lettura, as.Date("2026-02-02"))
  expect_identical(valide$matricola_veicolo, "101")
  # il comune di lettura e il cantiere arrivano con le letture
  expect_identical(
    names(valide),
    c(
      "data_ora_lettura",
      "data_lettura",
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

test_that("i codici RFID sono portati a 10 caratteri; sacchetti e HERA restano come sono", {
  formato <- function(codici) formatta_rfid(dplyr::tibble(RFID = codici))$RFID

  # maiuscolo e riempimento a 10
  expect_identical(formato("abc123"), "0000ABC123")
  expect_identical(formato("0000000001"), "0000000001")
  # gli zeri in eccesso spariscono prima del riempimento
  expect_identical(formato("000000000000000000000042"), "0000000042")
  # oltre 10 caratteri significativi: nessun riempimento (non è compatibile)
  expect_identical(formato("12345678901"), "12345678901")
  expect_identical(
    formato("E28011700000020F1A2B3C4D"),
    "E28011700000020F1A2B3C4D"
  )
  # i sacchetti (00BD) restano come sono, di qualunque lunghezza
  expect_identical(
    formato(c("00bd00000000000000000001", "00BD0001")),
    c("00BD00000000000000000001", "00BD0001")
  )
  # i bidoni HERA (48455241) non si riempiono, nemmeno dopo gli zeri iniziali
  expect_identical(
    formato(c("48455241aabbccdd11", "004845524100", "48455241")),
    c("48455241AABBCCDD11", "4845524100", "48455241")
  )
  # un carattere letto male tra i primi 14, che dovrebbero essere zeri
  expect_identical(formato("00000100000000ABCDEF1234"), "ABCDEF1234")
  expect_identical(formato("00000000000000ABCDEF1234"), "ABCDEF1234")
  # i codici mancanti restano mancanti
  expect_identical(formato(NA_character_), NA_character_)
  expect_identical(
    formato(c("abc123", "12345678901")),
    c("0000ABC123", "12345678901")
  )
})

test_that("si scartano i codici con piu' di 10 cifre che non sono sacchetti ne' HERA", {
  codici <- c(
    "0000ABC123",
    "12345678901",
    "00BD00000000000000000001",
    "48455241AABBCCDD11",
    "E28011700000020F1A2B3C4D",
    NA
  )
  expect_identical(
    codice_compatibile(codici),
    c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)
  )
  letture <- dplyr::tibble(RFID = codici, n = seq_along(codici))
  expect_message(
    tenute <- filtra_codici_compatibili(letture),
    "Letture scartate per codice RFID non compatibile.*: 2\\."
  )
  expect_identical(tenute$n, c(1L, 3L, 4L, 6L))
  # senza codici da scartare non dice niente
  expect_no_message(filtra_codici_compatibili(letture[c(1, 3), ]))
})

test_that("la trasposizione inverte i quattro bit di ogni cifra esadecimale", {
  # la tabella della funzione di trasposizione del database
  cifre <- strsplit("0123456789ABCDEF", "")[[1]]
  trasposte <- strsplit("084C2A6E195D3B7F", "")[[1]]
  expect_identical(trasponi_codice(cifre), trasposte)
  expect_identical(trasponi_codice("0123456789ABCDEF"), "084C2A6E195D3B7F")
  # e' una involuzione: applicata due volte restituisce il codice
  codici <- c("D250901001", "00AB12CD34", "FFFF000012")
  expect_identical(trasponi_codice(trasponi_codice(codici)), codici)
  expect_identical(trasponi_codice("abc"), "5D3")
  expect_identical(trasponi_codice(NA_character_), NA_character_)
})

test_that("lo scambio a coppie e' quello di inverti_coppie() del proprietario", {
  inverti_coppie <- function(testo) {
    sapply(testo, function(x) {
      if (is.na(x) || nchar(x) <= 1) return(x)
      caratteri <- strsplit(x, "")[[1]]
      n <- length(caratteri)
      limite <- n - (n %% 2)
      dispari <- seq(1, limite, by = 2)
      pari <- dispari + 1
      caratteri[c(dispari, pari)] <- caratteri[c(pari, dispari)]
      paste(caratteri, collapse = "")
    }, USE.NAMES = FALSE)
  }
  expect_identical(scambia_coppie("A1B2C3"), "1A2B3C")
  # con un numero dispari di caratteri l'ultimo resta dov'e'
  expect_identical(scambia_coppie("ABCDE"), "BADCE")
  set.seed(7)
  casuali <- vapply(
    1:300,
    function(i) paste(sample(c(0:9, LETTERS[1:6]), sample(1:25, 1), TRUE), collapse = ""),
    ""
  )
  codici <- c(casuali, "", "A", NA)
  expect_identical(scambia_coppie(codici), inverti_coppie(codici))
  # trasposizione e scambio commutano: l'ordine dei due passi non conta
  expect_identical(
    scambia_coppie(trasponi_codice(casuali)),
    trasponi_codice(scambia_coppie(casuali))
  )
})

test_that("un codice letto male si ripara con i tre passi, nell'ordine, e il database lo riconosce", {
  contenitori <- dplyr::tibble(
    codice_transponder = c("0012345678", "00AB12CD34", "0091827364"),
    data_attivazione_contenitore = as.Date(c("2025-01-01", "2025-01-01", "2025-01-01")),
    data_cessazione_contenitore = as.Date(c("2099-12-31", "2099-12-31", "2026-02-28")),
    id_fornitura = c("F1", "F2", "F3")
  )
  tag <- dplyr::tibble(codice_transponder = "0012F3A4B5")
  giorno <- as.Date("2026-03-10")
  # il codice del database, poi lo stesso passato per ognuno dei tre passi
  letture <- dplyr::tibble(
    RFID = c(
      "0012345678", # gia' noto: resta
      trasponi_codice("00AB12CD34"), # passo 1
      scambia_coppie("0091827364"), # sarebbe il passo 2, ma il contenitore e' cessato
      scambia_coppie("0012F3A4B5"), # passo 2 (tag)
      scambia_coppie(trasponi_codice("0012345678")), # passo 3
      "0000000042", # nessun passo: resta com'e'
      "00BD00000000000000000001" # un sacchetto non si prova
    ),
    data_lettura = giorno,
    n = 1:7
  )
  expect_message(
    riparate <- ripara_codici_rfid(letture, contenitori, tag),
    "Codici RFID riparati \\(letture\\): 1 trasposizione; 1 coppie; 1 trasposizione_e_coppie\\."
  )
  expect_identical(
    riparate$RFID,
    c(
      "0012345678",
      "00AB12CD34",
      scambia_coppie("0091827364"),
      "0012F3A4B5",
      "0012345678",
      "0000000042",
      "00BD00000000000000000001"
    )
  )
  # le altre colonne e l'ordine non cambiano
  expect_identical(riparate$n, 1:7)

  # il contenitore del passo 2 era cessato il 28 febbraio: la sera prima si ripara
  prima <- dplyr::mutate(letture[3, ], data_lettura = as.Date("2026-02-20"))
  expect_identical(
    suppressMessages(ripara_codici_rfid(prima, contenitori, tag))$RFID,
    "0091827364"
  )

  # nessun codice da riparare: nessun messaggio
  intatte <- letture[c(1, 6), ]
  expect_no_message(ripara_codici_rfid(intatte, contenitori, tag))

  # un codice che non cambia con il passo non e' riparato (palindromo di coppie)
  expect_identical(
    suppressMessages(
      ripara_codici_rfid(
        dplyr::tibble(RFID = "0000000000", data_lettura = giorno),
        contenitori,
        tag
      )$RFID
    ),
    "0000000000"
  )
})

test_that("presente_a_database dice dove il database conosce il codice: PIA, PIT o NP", {
  letture <- dplyr::tibble(
    RFID = c("0000ABC123", "0000ABC123", "0000000001", "00FF00FF00", "0000000042", "0000ABC124", "000000000000012345678901"),
    data_lettura = as.Date(c("2026-03-02", "2026-03-10", "2026-03-02", "2026-03-02", "2026-03-02", "2026-03-02", "2026-03-02"))
  )
  tag <- dplyr::tibble(codice_transponder = c("00FF00FF00", "0000000001", "0000ABC124"))
  associate <- associa_contenitore_e_fornitura(
    letture,
    contenitori_prova,
    sacchetti_prova,
    tag
  )
  # 0000000001 e' in associazione dal 1/6/2025 ma e' anche tra i tag: vince l'associazione;
  # 0000ABC124 e' solo tra i tag; il sacchetto ha la fornitura del suo gruppo
  expect_identical(
    associate$presente_a_database,
    c("PIA", "PIA", "PIA", "PIT", "NP", "PIT", "PIA")
  )
  expect_identical(
    associate$id_fornitura,
    c("F1", "F2", "F3", NA, NA, NA, "S9")
  )
  # un contenitore in associazione ma non ancora attivo non e' PIA: se e' tra i tag e' PIT
  presto <- associa_contenitore_e_fornitura(
    dplyr::tibble(RFID = "0000000001", data_lettura = as.Date("2025-01-01")),
    contenitori_prova,
    sacchetti_prova,
    tag
  )
  expect_identical(presto$presente_a_database, "PIT")
  # senza la tabella dei tag nessuna lettura e' PIT
  expect_identical(
    associa_contenitore_e_fornitura(letture, contenitori_prova, sacchetti_prova)$presente_a_database,
    c("PIA", "PIA", "PIA", "NP", "NP", "NP", "PIA")
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
  associate <- associa_contenitore_e_fornitura(letture, contenitori_prova, sacchetti_prova)

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
    c(rep("PIA", 4), "NP")
  )
  # l'utenza viene dalla fornitura del contenitore: un sacchetto non ce l'ha
  expect_identical(associate$id_utenza, c("U1", "U1", "U2", NA, NA))
  # i dati del contenitore e della fornitura li hanno solo le letture che
  # trovano il contenitore
  expect_equal(associate$volume, c(240, 240, 240, NA, NA))
  expect_identical(
    associate$stato_fornitura,
    c(rep("ATTIVA", 3), NA, NA)
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
    associa_contenitore_e_fornitura(letture, numeriche, sacchetti_prova)$id_fornitura,
    c("101", "101", "102", "S9", NA)
  )

  # l'anagrafica dei contenitori deve riportare la fornitura
  expect_error(
    associa_contenitore_e_fornitura(
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
    "2026-03-02 12:10:00", # tra i due turni, entro il ritardo dalla fine del mattino
    "2026-03-02 12:50:00", # tra i due turni, più vicina all'inizio del pomeriggio
    "2026-03-02 05:30:00", # prima di ogni turno, ma entro un'ora
    "2026-03-02 20:00:00" # dopo ogni turno, oltre un'ora: nessun giro
  ))
  risultato <- associa_giro_e_servizio_atteso(letture, calendario_prova)

  # una riga per lettura, anche con turni ripetuti nel calendario
  expect_equal(nrow(risultato), nrow(letture))
  expect_identical(names(risultato), colonne_passo_3)
  # il giro è quello del turno scelto, con il nome del calendario
  expect_identical(
    risultato$servizio_giro,
    c("SECCO PAP", "CARTA/CARTONE PAP", "SECCO PAP", "CARTA/CARTONE PAP", "SECCO PAP", NA)
  )
  expect_identical(risultato$id_giro, c("11", "12", "11", "12", "11", NA))
  expect_identical(risultato$eseguito, c(rep("SI", 5), NA))
  expect_identical(risultato$comune_giro, c(rep("BASSANO DEL GRAPPA", 5), NA))
  # il servizio atteso è il giro, con il nome del database
  expect_identical(
    risultato$servizio_atteso,
    c("SECCO", "CARTA", "SECCO", "CARTA", "SECCO", NA)
  )
  # un non censito non ha un servizio a database: la coerenza manca
  expect_true(all(is.na(risultato$coerenza_servizio)))
  # i non censiti sono letti a Rosà da un giro di Bassano del Grappa
  expect_identical(
    risultato$coerenza_comune,
    c(rep("Fuori giro", 5), "Coerenza ignota")
  )

  # per un censito il servizio atteso è comunque quello del giro, e la
  # coerenza lo confronta con il servizio a database
  censiti <- associa_giro_e_servizio_atteso(
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
    associa_giro_e_servizio_atteso(
      spostato,
      calendario_prova
    )$coerenza_comune,
    "Fuori comune"
  )
  # senza giri nel giorno il giro manca; il servizio atteso è quello dei
  # censiti letti dal mezzo quel giorno, per tutte le sue letture. La
  # coerenza del comune manca solo al non censito, senza comune dichiarato
  senza_turni <- associa_giro_e_servizio_atteso(
    dplyr::bind_rows(
      letture_associate(
        paste("2026-03-05", c("08:30:00", "08:40:00", "08:50:00")),
        rifiuto = "VETRO"
      ),
      letture_associate("2026-03-05 09:00:00")
    ),
    calendario_prova
  )
  expect_identical(
    senza_turni$servizio_transponder,
    c("VETRO", "VETRO", "VETRO", NA)
  )
  expect_true(all(is.na(senza_turni$servizio_giro)))
  expect_identical(senza_turni$servizio_atteso, rep("VETRO", 4))
  expect_identical(senza_turni$coerenza_servizio, c("SI", "SI", "SI", NA))
  expect_identical(
    senza_turni$coerenza_comune,
    c("Coerente", "Coerente", "Coerente", "Coerenza ignota")
  )
})

test_that("una lettura entro mezz'ora dalla fine resta nel giro, e allunga il giro", {
  expect_identical(ritardo_giro_minuti(), 30)
  calendario <- unisci_calendari(dplyr::tibble(
    data_giro = as.Date("2026-03-02"),
    ora_inizio = c("06:00", "14:00"),
    ora_fine = c("12:00", "18:00"),
    matricola_mezzo = "301",
    id_giro = c("51", "52"),
    eseguito = "SI",
    descrizione_servizio = c("SECCO PAP", "CARTA/CARTONE PAP"),
    comune_giro = "ROSÀ"
  ))
  quando <- paste(
    "2026-03-02",
    c("12:20:00", "12:45:00", "13:10:00", "13:50:00", "08:00:00")
  )
  letture <- letture_associate(quando, matricola = "301")
  risultato <- associa_giro_e_servizio_atteso(letture, calendario)
  # le letture restano nell'ordine del file
  expect_identical(
    format(risultato$data_ora_lettura, "%H:%M"),
    substr(quando, 12, 16)
  )
  # 12:20 è a 20 minuti dalla fine, 12:45 a 25 da 12:20, 13:10 a 25 da 12:45:
  # tutte nel giro del secco. 13:50 arriva 40 minuti dopo: la catena si
  # ferma, ed è più vicina all'inizio della carta
  expect_identical(
    risultato$servizio_giro,
    c("SECCO PAP", "SECCO PAP", "SECCO PAP", "CARTA/CARTONE PAP", "SECCO PAP")
  )
  expect_identical(
    attr(risultato, "associazione")[[passi_associazione()[["orario_comune"]]]],
    4L
  )

  # la catena si ferma all'inizio del giro seguente: le sue letture restano sue
  vicino <- calendario
  vicino$ora_inizio[2] <- "12:30"
  vicine <- associa_giro_e_servizio_atteso(
    letture_associate(
      paste("2026-03-02", c("12:20:00", "12:40:00")),
      matricola = "301"
    ),
    vicino
  )
  expect_identical(vicine$servizio_giro, c("SECCO PAP", "CARTA/CARTONE PAP"))

  # la fine allungata, giro per giro
  giri <- dplyr::tibble(
    riga = 1:2,
    mezzo = "301",
    data_giro = "2026-03-02",
    inizio = c(21600, 50400),
    fine = c(43200, 64800)
  )
  letture_ora <- dplyr::tibble(
    mezzo = "301",
    data_giro = "2026-03-02",
    ora = 43200 + c(600, 1500, 4000)
  )
  expect_identical(allunga_fine(giri, letture_ora)$fine, c(43200 + 1500, 64800))
})

test_that("un mezzo senza giri prende il servizio dei bidoni censiti che ha letto", {
  censito <- function(quando, mezzo, rfid, rifiuto) {
    l <- letture_associate(
      paste("2026-03-02", quando),
      matricola = mezzo,
      rifiuto = rifiuto
    )
    l$RFID <- rfid
    l
  }
  letture <- dplyr::bind_rows(
    # 401: un bidone del secco letto tre volte, due della carta: vince la
    # carta, perché ogni bidone conta una volta
    censito(c("08:00:00", "08:10:00", "08:20:00"), "401", "B1", "SECCO"),
    censito("08:30:00", "401", "B2", "CARTA"),
    censito("08:40:00", "401", "B3", "CARTA"),
    letture_associate("2026-03-02 08:50:00", matricola = "401"),
    # 402: un bidone per servizio, in parità: niente
    censito("09:00:00", "402", "B4", "SECCO"),
    censito("09:10:00", "402", "B5", "CARTA"),
    # 403: soli non censiti: niente
    letture_associate("2026-03-02 09:20:00", matricola = "403")
  )
  risultato <- associa_giro_e_servizio_atteso(letture, calendario_prova)
  expect_identical(
    risultato$servizio_atteso,
    c(rep("CARTA", 6), NA, NA, NA)
  )
  # nessun giro: il giro e il suo comune restano vuoti
  expect_true(all(is.na(risultato$servizio_giro)))
  expect_true(all(is.na(risultato$comune_giro)))
  associazione <- attr(risultato, "associazione")
  expect_identical(associazione[[passi_associazione()[["censiti"]]]], 6L)
  expect_identical(associazione[["senza giro"]], 3L)
  # un mezzo con giri nel giorno non prende il servizio dai censiti
  expect_true(all(is.na(servizio_dei_censiti_del_mezzo(dplyr::mutate(
    letture,
    senza_giri = FALSE
  )))))
})

test_that("un mezzo senza giri prende il servizio dei censiti del suo tratto, con almeno tre bidoni", {
  censiti <- function(quando, rfid, rifiuto) {
    l <- letture_associate(paste("2026-03-02", quando), matricola = "501", rifiuto = rifiuto)
    l$RFID <- rfid
    l
  }
  letture <- dplyr::bind_rows(
    # primo tratto: tre bidoni del secco e un non censito
    censiti(c("08:00:00", "08:10:00", "08:20:00"), c("A1", "A2", "A3"), "SECCO"),
    letture_associate("2026-03-02 08:25:00", matricola = "501"),
    # una pausa di oltre mezz'ora: secondo tratto, tre bidoni della carta
    censiti(c("10:00:00", "10:10:00", "10:20:00"), c("B1", "B2", "B3"), "CARTA"),
    # terzo tratto: due bidoni soltanto, sotto il minimo
    censiti(c("12:00:00", "12:10:00"), c("C1", "C2"), "SECCO")
  )
  risultato <- associa_giro_e_servizio_atteso(letture, calendario_prova)
  # il servizio atteso non attraversa le pause: il secco non diventa la carta
  expect_identical(
    risultato$servizio_atteso,
    c(rep("SECCO", 4), rep("CARTA", 3), NA, NA)
  )
  expect_identical(censiti_minimi_per_tratto(), 3L)
  # nessun giro inventato: il giro, il comune del giro e l'id restano vuoti
  expect_true(all(is.na(risultato$servizio_giro)))
  expect_true(all(is.na(risultato$id_giro)))
  associazione <- attr(risultato, "associazione")
  expect_identical(associazione[[passi_associazione()[["censiti"]]]], 7L)
  expect_identical(associazione[["senza giro"]], 2L)
})

test_that("nel tratto un bidone letto piu' volte conta una volta e un pareggio non assegna niente", {
  censiti <- function(quando, rfid, rifiuto) {
    l <- letture_associate(paste("2026-03-02", quando), matricola = "502", rifiuto = rifiuto)
    l$RFID <- rfid
    l
  }
  letture <- dplyr::bind_rows(
    # un bidone del secco letto quattro volte e due della carta: vince la carta
    censiti(paste0("08:0", 0:3, ":00"), rep("A1", 4), "SECCO"),
    censiti(c("08:10:00", "08:20:00"), c("A2", "A3"), "CARTA"),
    # secondo tratto: due contro due, parit\u00e0
    censiti(c("11:00:00", "11:10:00"), c("B1", "B2"), "SECCO"),
    censiti(c("11:20:00", "11:30:00"), c("B3", "B4"), "CARTA")
  )
  risultato <- associa_giro_e_servizio_atteso(letture, calendario_prova)
  expect_identical(risultato$servizio_atteso, c(rep("CARTA", 6), rep(NA, 4)))
})

test_that("un mezzo con giri nel giorno non prende il servizio dai censiti del tratto", {
  letture <- letture_associate(
    paste("2026-03-02", c("08:00:00", "08:10:00", "08:20:00")),
    rifiuto = "VETRO"
  )
  letture$senza_giri <- FALSE
  expect_true(all(is.na(servizio_dei_censiti_del_mezzo(letture))))
  expect_identical(
    servizio_dei_censiti_del_mezzo(dplyr::mutate(letture, senza_giri = TRUE)),
    rep("VETRO", 3)
  )
})

test_that("la matricola del calendario e quella delle letture si confrontano senza zeri iniziali", {
  calendario <- dplyr::mutate(
    calendario_prova,
    matricola_mezzo = sub("^1", "01", .data$matricola_mezzo)
  )
  risultato <- associa_giro_e_servizio_atteso(
    letture_associate("2026-03-02 08:00:00", matricola = "101"),
    calendario
  )
  expect_identical(risultato$servizio_giro, "SECCO PAP")
  expect_identical(chiave_matricola(c("0933", " 933", "A07")), c("933", "933", "A07"))
})

test_that("i giri aggiunti a mano si rimettono tra i pianificati, senza ripeterne uno gi\u00e0 presente", {
  aggiunti <- dplyr::tibble(
    data_giro = as.Date(c("2026-03-02", "2026-03-03")),
    ora_inizio = c("06:00", "07:00"),
    ora_fine = c("12:00", "11:00"),
    matricola_mezzo = c("0101", "0101"),
    id_giro = c(NA, "G9"),
    eseguito = "SI",
    descrizione_servizio = c("SECCO PAP", "SECCO PAP"),
    comune_giro = c("Bassano del Grappa", "BASSANO DEL GRAPPA")
  )
  # il primo c'\u00e8 gi\u00e0 nel calendario (stesso giorno, mezzo, servizio e comune)
  expect_message(
    pianificati <- aggiungi_giri_a_mano(calendario_prova, aggiunti),
    "1 nuovi, 1 gi\u00e0 nel calendario"
  )
  expect_identical(nrow(pianificati), nrow(calendario_prova) + 1L)
  nuovo <- pianificati[nrow(pianificati), ]
  expect_identical(nuovo$data_giro, as.Date("2026-03-03"))
  expect_identical(nuovo$ora_inizio, "07:00")
  expect_identical(nuovo$id_giro, "G9")
  # senza giri aggiunti non cambia niente
  expect_identical(aggiungi_giri_a_mano(calendario_prova, NULL), calendario_prova)
})

test_that("un giro aggiunto a mano d\u00e0 il giro a un mezzo che il calendario non aveva", {
  aggiunti <- dplyr::tibble(
    data_giro = as.Date("2026-03-05"),
    ora_inizio = "07:00",
    ora_fine = "11:00",
    matricola_mezzo = "0501",
    id_giro = "G5",
    eseguito = "SI",
    descrizione_servizio = "SECCO PAP",
    comune_giro = "ROS\u00c0"
  )
  calendario <- unisci_calendari(
    suppressMessages(aggiungi_giri_a_mano(calendario_prova, aggiunti))
  )
  # un solo censito: senza il giro il servizio atteso resterebbe vuoto
  senza <- associa_giro_e_servizio_atteso(
    letture_associate("2026-03-05 08:00:00", matricola = "501", rifiuto = "SECCO"),
    calendario_prova
  )
  expect_true(is.na(senza$servizio_atteso))
  con <- associa_giro_e_servizio_atteso(
    letture_associate("2026-03-05 08:00:00", matricola = "501", rifiuto = "SECCO"),
    calendario
  )
  # nell'orario e nel comune del pianificato aggiunto, con i suoi dati
  expect_identical(con$servizio_giro, "SECCO PAP")
  expect_identical(con$servizio_atteso, "SECCO")
  expect_identical(con$comune_giro, "ROS\u00c0")
  expect_identical(con$id_giro, "G5")
  expect_identical(con$eseguito, "SI")
  expect_identical(
    attr(con, "associazione")[[passi_associazione()[["orario_comune"]]]],
    1L
  )
})

test_that("senza turni il servizio atteso resta vuoto", {
  senza_turni <- associa_giro_e_servizio_atteso(
    letture_associate("2026-03-02 08:30:00", matricola = "999"),
    calendario_prova
  )
  expect_equal(nrow(senza_turni), 1)
  expect_true(is.na(senza_turni$servizio_atteso))
  expect_true(is.na(senza_turni$id_giro))

  # un altro giorno, per lo stesso mezzo, non ha turni
  altro_giorno <- associa_giro_e_servizio_atteso(
    letture_associate("2026-03-05 08:30:00"),
    calendario_prova
  )
  expect_true(is.na(altro_giorno$servizio_atteso))
})

test_that("i due calendari diventano uno solo", {
  pianificati <- dplyr::tibble(
    data_giro = as.Date("2026-03-02"),
    ora_inizio = "06:00",
    ora_fine = "12:00",
    matricola_mezzo = "201",
    id_giro = c("21", "21"),
    eseguito = "SI",
    descrizione_servizio = "SECCO PAP",
    comune_giro = "ROSÀ"
  )
  non_pianificati <- dplyr::tibble(
    data_giro = as.Date("2026-03-02"),
    matricola_mezzo = "201",
    descrizione_servizio = c("SECCO PAP", "VETRO PAP"),
    comune_giro = c("ROSÀ", "NOVE")
  )
  calendario <- unisci_calendari(pianificati, non_pianificati)
  # la riga ripetuta conta una volta
  expect_equal(nrow(calendario), 3)
  expect_identical(calendario$pianificato, c(TRUE, FALSE, FALSE))
  # un non pianificato non ha codice del giro, orari ne' conferma, anche con
  # lo stesso servizio e comune di un pianificato
  expect_identical(calendario$id_giro, c("21", NA, NA))
  expect_true(all(is.na(calendario$ora_inizio[2:3])))
  expect_true(all(is.na(calendario$eseguito[2:3])))
  expect_identical(unisci_calendari(pianificati)$pianificato, TRUE)
  expect_error(
    unisci_calendari(pianificati[, -1]),
    "mancano le colonne: data_giro"
  )
  expect_identical(
    secondi_del_giorno(c("08:30", "13:00:30", "8.30", NA)),
    c(30600, 46830, NA, NA)
  )
})

test_that("una lettura prende il giro con i passi chiesti, mai di un altro mezzo o giorno", {
  pianificato <- function(mezzo, id, servizio, comune) {
    dplyr::tibble(
      data_giro = as.Date("2026-03-02"),
      ora_inizio = "06:00",
      ora_fine = "12:00",
      matricola_mezzo = mezzo,
      id_giro = id,
      eseguito = "SI",
      descrizione_servizio = servizio,
      comune_giro = comune
    )
  }
  non_pianificato <- function(mezzo, servizio, comune) {
    dplyr::tibble(
      data_giro = as.Date("2026-03-02"),
      matricola_mezzo = mezzo,
      descrizione_servizio = servizio,
      comune_giro = comune
    )
  }
  calendario <- unisci_calendari(
    dplyr::bind_rows(
      # 201: due pianificati alla stessa ora, in comuni diversi
      pianificato("201", "21", "SECCO PAP", "ROSÀ"),
      pianificato("201", "22", "CARTA/CARTONE PAP", "CASSOLA"),
      # 203 e 204: un pianificato e dei non pianificati
      pianificato("203", "31", "SECCO PAP", "ROSÀ"),
      pianificato("204", "41", "SECCO PAP", "ROSÀ")
    ),
    dplyr::bind_rows(
      # 202: un solo giro, non pianificato
      non_pianificato("202", "VETRO PAP", "NOVE"),
      # 203: comuni tutti diversi
      non_pianificato("203", "CARTA CONT.STRADALI", "CASSOLA"),
      # 204: due non pianificati nello stesso comune del pianificato
      non_pianificato("204", "VETRO PAP", "ROSÀ"),
      non_pianificato("204", "UMIDO PAP", "ROSÀ"),
      # 205: soli non pianificati, in comuni diversi
      non_pianificato("205", "VETRO PAP", "ROSÀ"),
      non_pianificato("205", "UMIDO PAP", "CASSOLA")
    )
  )
  caso <- function(quando, mezzo, comune) {
    l <- letture_associate(paste("2026-03-02", quando), matricola = mezzo)
    l$comune_lettura <- comune
    l
  }
  letture <- dplyr::bind_rows(
    caso("08:00:00", "201", "ROSÀ"), # 1. nell'orario e nel comune
    caso("08:00:00", "201", "CASSOLA"), # 1. l'altro giro, nel suo comune
    caso("08:00:00", "201", "NOVE"), # 3. nell'orario, in un altro comune: il primo dei due
    caso("20:00:00", "202", "ROSÀ"), # 4. unico servizio non pianificato
    caso("15:00:00", "203", "CASSOLA"), # 2. non pianificato nel comune
    caso("15:00:00", "203", "ROSÀ"), # 4. fuori orario: l'unico servizio non pianificato
    caso("08:00:00", "203", "NOVE"), # 3. nell'orario del pianificato, in un altro comune
    caso("15:00:00", "204", "ROSÀ"), # nessun passo: due servizi nello stesso comune
    caso("09:00:00", "204", "ROSÀ"), # 1. nell'orario del pianificato
    caso("15:00:00", "204", "NOVE"), # nessun passo: due servizi, nessun comune
    caso("09:00:00", "205", "ROSÀ"), # 2. non pianificato nel comune
    caso("09:00:00", "205", "NOVE"), # 5. come la lettura di seguito, nello stesso tratto
    caso("09:00:00", "999", "ROSÀ") # mezzo senza giri
  )
  # un giorno senza giri: niente, anche se il mezzo li ha il giorno prima
  altro_giorno <- letture_associate("2026-03-03 09:00:00", matricola = "202")
  risultato <- associa_giro_e_servizio_atteso(
    dplyr::bind_rows(letture, altro_giorno),
    calendario
  )
  expect_identical(
    risultato$servizio_giro,
    c(
      "SECCO PAP",
      "CARTA/CARTONE PAP",
      "SECCO PAP",
      "VETRO PAP",
      "CARTA CONT.STRADALI",
      "CARTA CONT.STRADALI",
      "SECCO PAP",
      NA,
      "SECCO PAP",
      NA,
      "VETRO PAP",
      "VETRO PAP",
      NA,
      NA
    )
  )
  expect_identical(risultato$id_giro[1:3], c("21", "22", "21"))
  # un giro non pianificato non ha conferma
  expect_identical(risultato$eseguito[4:5], c(NA_character_, NA_character_))
  expect_identical(risultato$servizio_atteso[c(4, 10)], c("VETRO", NA))
  # quante letture ha associato ogni passo
  associazione <- attr(risultato, "associazione")
  expect_identical(
    names(associazione),
    c(unname(passi_associazione()), "senza giro")
  )
  expect_equal(as.integer(associazione), c(3, 2, 2, 2, 1, 0, 0, 0, 4))
  # l'unico servizio non pianificato vale solo fuori dagli orari dei pianificati
  fuori_orario <- associa_giro_e_servizio_atteso(
    caso("20:00:00", "203", "NOVE"),
    calendario
  )
  expect_identical(fuori_orario$servizio_giro, "CARTA CONT.STRADALI")
})

test_that("i giri non pianificati: servizio unico, tratto, comune vicino e tolleranza", {
  pianificato <- function(mezzo, id, servizio, comune) {
    dplyr::tibble(
      data_giro = as.Date("2026-03-02"), ora_inizio = "06:00", ora_fine = "12:00",
      matricola_mezzo = mezzo, id_giro = id, eseguito = "SI",
      descrizione_servizio = servizio, comune_giro = comune
    )
  }
  non_pianificato <- function(mezzo, id, servizio, comune) {
    dplyr::tibble(
      data_giro = as.Date("2026-03-02"), matricola_mezzo = mezzo, id_giro = id,
      descrizione_servizio = servizio, comune_giro = comune
    )
  }
  calendario <- unisci_calendari(
    pianificato("501", "P1", "SECCO PAP", "ROSÀ"),
    dplyr::bind_rows(
      # 501: lo stesso servizio in due comuni, più un pianificato
      non_pianificato("501", "N1", "CARTA CONT.STRADALI", "NOVE"),
      non_pianificato("501", "N2", "CARTA CONT.STRADALI", "MAROSTICA"),
      # 502 e 503: due servizi in due comuni
      non_pianificato("502", "N3", "VETRO PAP", "ROSÀ"),
      non_pianificato("502", "N4", "UMIDO PAP", "CASSOLA"),
      non_pianificato("503", "N3", "VETRO PAP", "ROSÀ"),
      non_pianificato("503", "N4", "UMIDO PAP", "CASSOLA")
    )
  )
  # un non pianificato ha il codice del suo file, anche uguale a un altro
  expect_identical(calendario$id_giro[calendario$pianificato], "P1")
  expect_identical(calendario$id_giro[!calendario$pianificato], c("N1", "N2", "N3", "N4", "N3", "N4"))

  lettura <- function(quando, mezzo, comune, lat, lng) {
    l <- letture_associate(paste("2026-03-02", quando), matricola = mezzo)
    l$comune_lettura <- comune
    l$latitudine <- lat
    l$longitudine <- lng
    l
  }
  nove <- comuni_cantieri()[comuni_cantieri()$comune == "NOVE", ]
  letture <- dplyr::bind_rows(
    # 501, fuori orario, a Bassano: lo stesso servizio in due comuni, la
    # lettura non è in nessuno; il giro è quello del comune più vicino (Nove)
    lettura("15:00:00", "501", "BASSANO DEL GRAPPA", 45.72942, 11.70269),
    # 501, fuori orario, a Marostica: il comune di un giro
    lettura("15:10:00", "501", "MAROSTICA", 45.74, 11.66),
    # 502, due servizi: la prima lettura è a Rosà (comune di un giro) e la
    # seconda, dieci minuti dopo, a Nove (nessun giro): stesso tratto, stesso giro
    lettura("08:00:00", "502", "ROS\u00c0", 45.72442, 11.76269),
    lettura("08:10:00", "502", "NOVE", nove$latitudine, nove$longitudine),
    # 502, più tardi e da sola, a Nove: oltre 2 km da Rosà (2081 m)
    lettura("14:00:00", "502", "NOVE", nove$latitudine, nove$longitudine),
    # 503, da sola, a Tezze sul Brenta, a 313 m da Rosà
    lettura("14:00:00", "503", "TEZZE SUL BRENTA", 45.69442, 11.76769)
  )
  risultato <- associa_giro_e_servizio_atteso(letture, calendario)

  expect_identical(
    risultato$servizio_giro,
    c("CARTA CONT.STRADALI", "CARTA CONT.STRADALI", "VETRO PAP", "VETRO PAP", NA, "VETRO PAP")
  )
  expect_identical(risultato$id_giro, c("N1", "N2", "N3", "N3", NA, "N3"))
  associazione <- attr(risultato, "associazione")
  passo <- function(nome) associazione[[passi_associazione()[[nome]]]]
  expect_identical(passo("servizio_unico"), 1L)
  expect_identical(passo("comune_non_pianificato"), 2L)
  expect_identical(passo("tratto"), 1L)
  expect_identical(passo("comune_vicino"), 1L)
  expect_identical(associazione[["senza giro"]], 1L)
})

test_that("un pianificato fuori orario vale entro la tolleranza di un'ora, non oltre", {
  calendario <- unisci_calendari(dplyr::tibble(
    data_giro = as.Date("2026-03-02"), ora_inizio = "08:00", ora_fine = "12:00",
    matricola_mezzo = "601", id_giro = "P9", eseguito = "SI",
    descrizione_servizio = "SECCO PAP", comune_giro = "ROSÀ"
  ))
  letture <- letture_associate(
    paste("2026-03-02", c("07:15:00", "06:30:00", "14:30:00")),
    matricola = "601"
  )
  # 45 minuti prima dell'inizio: dentro la tolleranza; 90 minuti prima e
  # due ore e mezza dopo la fine: fuori
  risultato <- associa_giro_e_servizio_atteso(letture, calendario)
  expect_identical(risultato$id_giro, c("P9", NA, NA))
  expect_identical(
    attr(risultato, "associazione")[[passi_associazione()[["vicino_orario"]]]],
    1L
  )
})

test_that("i turni che finiscono in giornata non passano al giorno dopo", {
  # mezzo 101: il 2 marzo ha solo turni diurni, il 3 marzo nessun turno
  giorno_dopo <- associa_giro_e_servizio_atteso(
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
  risultato <- associa_giro_e_servizio_atteso(
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
    neve <- associa_giro_e_servizio_atteso(
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
    data_giro = as.Date(c("2026-03-02", "2026-03-03", "2026-03-02", "2026-03-02")),
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
      paste(
        "2026-03-02",
        c("07:00:00", "07:10:00", "07:20:00", "07:30:00", "07:40:00")
      ),
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
    letture_associate(
      "2026-03-02 08:00:00",
      matricola = "103",
      rifiuto = "CARTA"
    )
  )
  risultato <- associa_giro_e_servizio_atteso(letture, calendario)
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
  senza <- associa_giro_e_servizio_atteso(
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
      data_lettura = as.Date("2026-03-02"),
      id_giro = c(1, 1, 1, NA),
      servizio_giro = c(
        "SERVIZI MERCATI",
        "SERVIZI MERCATI",
        "SERVIZI MERCATI",
        NA
      ),
      servizio_transponder = c("VETRO", "VETRO", NA, "VETRO")
    )),
    c("VETRO", "VETRO", "VETRO", NA)
  )
  # un non pianificato non ha codice: il suo giro è mezzo, servizio e comune
  expect_identical(
    servizio_atteso_dal_giro(dplyr::tibble(
      data_lettura = as.Date("2026-03-02"),
      matricola_veicolo = c("1", "1", "2"),
      id_giro = NA_character_,
      servizio_giro = "SERVIZI MERCATI",
      comune_giro = "NOVE",
      servizio_transponder = c("VETRO", "VETRO", "CARTA")
    )),
    c("VETRO", "VETRO", "CARTA")
  )
  # il valore più frequente, se è uno solo
  expect_identical(moda_unica(c("A", "A", "B")), "A")
  expect_identical(moda_unica(c("A", "B")), NA_character_)
  expect_identical(moda_unica(c(NA, "")), NA_character_)
  expect_identical(moda_unica("C"), "C")
})

test_that("il calendario deve riportare il comune di ogni giro", {
  expect_error(
    associa_giro_e_servizio_atteso(
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
    filtra_letture_dopo_il_collaudo(test_mezzi_prova) |>
    formatta_rfid() |>
    associa_contenitore_e_fornitura(contenitori_prova, sacchetti_prova) |>
    associa_giro_e_servizio_atteso(calendario_prova)

  expect_identical(names(risultato), colonne_passo_3)
  expect_equal(nrow(risultato), 4)

  file <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(risultato, file, row.names = FALSE)
  esito <- carica_dataset(file)

  expect_length(esito$avvisi, 0)
  expect_equal(nrow(esito$dati), 4)
  caricate <- dplyr::arrange(esito$dati, data_ora_lettura)
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
    c("PIA", "NP", "PIA", "PIA")
  )
  # volume e raccolte arrivano con i nomi dell'anagrafica
  expect_equal(caricate$volume_previsto, c(240, NA, 1100, 240))
  expect_equal(caricate$numero_raccolte_annue_previste, c(26, NA, 13, 26))
  # il servizio atteso è il giro con il nome del database; il 10 marzo il
  # mezzo non aveva giri e ha letto un solo censito: con meno di tre bidoni
  # nel tratto il servizio atteso resta vuoto
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
    dplyr::arrange(risultato, data_ora_lettura)$coerenza_servizio,
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
  punto_virgola <- scrivi_csv(c(
    "RFID;data_ora_lettura",
    "0000012345; 2025-01-10 08:00:00",
    ";"
  ))
  expect_identical(rileva_separatore(virgola), ",")
  expect_identical(rileva_separatore(punto_virgola), ";")

  letto <- leggi_tabella_origine(punto_virgola)
  expect_identical(names(letto), c("RFID", "data_ora_lettura"))
  # gli zeri iniziali restano, gli spazi ai margini no, i vuoti mancano
  expect_identical(letto$RFID, c("0000012345", NA))
  expect_identical(letto$data_ora_lettura, c("2025-01-10 08:00:00", NA))
  expect_s3_class(letto, "data.frame")
  expect_true(all(vapply(
    leggi_tabella_origine(virgola),
    is.character,
    logical(1)
  )))

  vuoto <- scrivi_csv("")
  expect_error(leggi_tabella_origine(vuoto), class = "errore_validazione")
})

test_that("nucleo e categoria sono quelli in vigore al momento della lettura", {
  letture <- data.frame(
    id_fornitura = c("F1", "F1", "F1", "F2", NA),
    data_ora_lettura = as.POSIXct(
      c(
        "2026-03-01 08:00:00",
        "2026-07-01 08:00:00",
        "2026-12-01 08:00:00",
        "2026-05-01 08:00:00",
        "2026-05-01 08:00:00"
      ),
      tz = "UTC"
    ),
    stringsAsFactors = FALSE
  )
  componenti <- data.frame(
    # F1 ha 2 residenti fino a metà giugno, poi 3; F2 ha una decorrenza finita
    id_fornitura = c("F1", "F1", "F2"),
    data_inizio_decorrenza = as.POSIXct(
      c("2020-01-01", "2026-06-15", "2020-01-01"),
      tz = "UTC"
    ),
    data_fine_decorrenza = as.POSIXct(
      c("2026-06-14", NA, "2026-01-01"),
      tz = "UTC"
    ),
    nucleo_residenti = c(2L, 3L, 4L),
    nucleo_non_residenti = c(0L, 1L, 0L),
    nucleo_totale = c(2L, 4L, 4L),
    tipo_categoria = "DOMESTICA",
    categoria = "ABITAZIONE",
    sottocategoria = "D",
    stringsAsFactors = FALSE
  )
  esito <- associa_componenti(letture, componenti)
  expect_identical(names(esito), c(names(letture), colonne_componenti()))
  expect_identical(esito$nucleo_residenti, c(2L, 3L, 3L, NA, NA))
  expect_identical(esito$nucleo_totale, c(2L, 4L, 4L, NA, NA))
  expect_identical(
    esito$tipo_categoria,
    c("DOMESTICA", "DOMESTICA", "DOMESTICA", NA, NA)
  )
  # una colonna che la tabella non ha resta vuota
  senza <- associa_componenti(
    letture,
    componenti[names(componenti) != "sottocategoria"]
  )
  expect_true(all(is.na(senza$sottocategoria)))
  expect_identical(
    colonne_nucleo_manovra(),
    c(colonne_componenti(), "potenziamento_secco")
  )
  # il dataset validato le tiene tutte
  expect_true(all(colonne_nucleo_manovra() %in% colonne_preparazione()))
})

test_that("il potenziamento vale se la fornitura ha almeno una manovra in vigore", {
  letture <- data.frame(
    id_fornitura = c("F1", "F1", "F1", "F1", "F2", NA),
    data_lettura = as.Date(c(
      "2026-03-01",
      "2026-06-01",
      "2026-07-15",
      "2026-12-01",
      "2026-05-01",
      "2026-05-01"
    )),
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
  expect_identical(
    esito$potenziamento_secco,
    c(FALSE, TRUE, TRUE, TRUE, FALSE, NA)
  )
  expect_no_warning(associa_potenziamento(letture, manovre))

  # una dichiarazione che finisce prima di cominciare: annullata, non conta
  annullata <- rbind(
    manovre,
    data.frame(
      id_fornitura = "F2",
      data_inizio_dichiarazione = as.Date("2026-04-01"),
      data_fine_dichiarazione = as.Date("2026-01-01")
    )
  )
  expect_warning(
    con_annullata <- associa_potenziamento(letture, annullata),
    "1 dichiarazioni di manovra sociale finiscono prima di cominciare.*Forniture: F2"
  )
  expect_identical(con_annullata$potenziamento_secco, esito$potenziamento_secco)
  # senza dichiarazioni nessuna fornitura ha il potenziamento
  expect_identical(
    associa_potenziamento(letture, manovre[0, ])$potenziamento_secco,
    c(rep(FALSE, 5), NA)
  )
})

test_that("dentro i comuni serviti il comune di lettura viene dalla posizione", {
  cc <- comuni_cantieri()
  limena <- cc[cc$comune == "LIMENA", ]
  rosa <- cc[cc$comune == "ROSÀ", ]
  letture <- data.frame(
    comune_lettura = c(
      "LIMENA",
      "rosa'",
      "TAGGI DI PROVA",
      "VENEZIA",
      "BOH",
      "CITTADELLA"
    ),
    cantiere = c("RUBANO", NA, NA, NA, NA, "CAMPOSAMPIERO"),
    latitudine = c(
      limena$latitudine,
      rosa$latitudine,
      limena$latitudine,
      45.44,
      NA,
      limena$latitudine
    ),
    longitudine = c(
      limena$longitudine,
      rosa$longitudine,
      limena$longitudine,
      12.33,
      NA,
      limena$longitudine
    ),
    stringsAsFactors = FALSE
  )
  expect_message(
    corrette <- correggi_comune_lettura(letture),
    "per 2 letture.*CITTADELLA → LIMENA \\(1\\), TAGGI DI PROVA → LIMENA \\(1\\)"
  )
  # dentro i comuni serviti vale il comune che contiene il punto, anche se il
  # sistema di localizzazione ne dice un altro o una frazione; fuori, o senza
  # posizione, resta il nome del file, nella grafia della tabella
  expect_identical(
    corrette$comune_lettura,
    c("LIMENA", "ROSÀ", "LIMENA", "VENEZIA", "BOH", "LIMENA")
  )
  expect_identical(
    corrette$cantiere,
    c("RUBANO", "BASSANO", "RUBANO", NA, NA, "RUBANO")
  )
  # l'elenco delle letture cambiate, con il comune del GPS e quello della posizione
  elenco <- comuni_corretti(corrette)
  expect_named(elenco, c("latitudine", "longitudine", "comune_gps", "comune_posizione"))
  expect_identical(elenco$comune_gps, c("TAGGI DI PROVA", "CITTADELLA"))
  expect_identical(elenco$comune_posizione, c("LIMENA", "LIMENA"))
  expect_equal(nrow(comuni_corretti(data.frame())), 0)
  # senza nomi da correggere, nessun messaggio
  expect_no_message(correggi_comune_lettura(letture[1:2, ]))
})

test_that("la distanza dal comune dichiarato c'e' solo per le letture in un altro comune", {
  cc <- comuni_cantieri()
  limena <- cc[cc$comune == "LIMENA", ]
  letture <- data.frame(
    presente_a_database = c(
      "PIA",
      "PIA",
      "NP",
      "NP",
      "PIA"
    ),
    comune_fornitura = c("LIMENA", "CITTADELLA", NA, NA, NA),
    comune_giro = c(
      "CITTADELLA",
      "LIMENA",
      "LIMENA",
      "MASSICCIO DEL GRAPPA",
      "LIMENA"
    ),
    comune_lettura = c("Limena", "LIMENA", "CITTADELLA", "LIMENA", "LIMENA"),
    latitudine = limena$latitudine,
    longitudine = limena$longitudine,
    stringsAsFactors = FALSE
  )
  distanza <- distanza_comune_dichiarato(letture)
  # stesso comune, anche scritto in un altro modo: manca
  expect_true(is.na(distanza[1]))
  # un censito letto a LIMENA ma a database a CITTADELLA: lontano
  expect_gt(distanza[2], 10000)
  # un non censito letto da un giro di LIMENA: il punto e' dentro LIMENA
  expect_identical(distanza[3], 0)
  # un giro di zona, o un censito senza comune a database: manca
  expect_true(all(is.na(distanza[4:5])))
  expect_identical(distanza_comune_dichiarato(letture[0, ]), numeric(0))
  # senza le colonne dei comuni nessuna distanza
  expect_true(all(is.na(distanza_comune_dichiarato(letture[, c(
    "presente_a_database",
    "latitudine",
    "longitudine"
  )]))))
})

test_that("si segnalano i giri non confermati da tempo e quelli confermati senza letture", {
  letture <- data.frame(
    data_lettura = as.Date(c("2026-08-03", "2026-08-31")),
    matricola_veicolo = "1601",
    id_giro = "10",
    stringsAsFactors = FALSE
  )
  calendario <- data.frame(
    data_giro = as.Date(c(
      "2026-08-03",
      "2026-08-05",
      "2026-08-10",
      "2026-08-10",
      "2026-08-28",
      "2026-08-31",
      "2026-07-30",
      "2026-08-12",
      "2026-08-12"
    )),
    matricola_mezzo = c(
      "1601",
      "1601",
      "1601",
      "1602",
      "1601",
      "1601",
      "1601",
      "1601",
      "1601"
    ),
    id_giro = c("10", "12", "10", "13", "12", "10", "10", "14", "14"),
    eseguito = c("SI", "NO", "si", "SI", "NO", "SI", "SI", NA, NA),
    descrizione_servizio = "SECCO PAP",
    comune_giro = "LIMENA",
    stringsAsFactors = FALSE
  )
  test_mezzi <- data.frame(
    targa_veicolo = c("AB123CD", "ZZ999ZZ"),
    matricola_veicolo = c("1601", "1602"),
    esito = c("POSITIVO", "NEGATIVO"),
    data_test = as.Date("2026-07-31"),
    stringsAsFactors = FALSE
  )
  expect_warning(
    expect_warning(
      giri <- controllo_giri(letture, calendario, test_mezzi),
      "1 giri sono ancora non confermati"
    ),
    "1 giri confermati di mezzi con l'antenna non hanno nessuna lettura"
  )
  # il NO del 5 agosto ha piu' di sette giorni, quello del 28 no; il giro
  # confermato del 10 agosto non ha letture; quello del mezzo senza antenna,
  # quello prima del periodo e quelli senza conferma non contano
  expect_named(
    giri,
    c(
      "data_giro",
      "matricola_veicolo",
      "id_giro",
      "servizio_giro",
      "comune_giro",
      "eseguito",
      "problema"
    )
  )
  expect_identical(giri$data_giro, as.Date(c("2026-08-05", "2026-08-10")))
  expect_identical(giri$id_giro, c("12", "10"))
  expect_identical(
    giri$problema,
    c("non confermato dopo 7 giorni", "confermato, senza letture dell'antenna")
  )
  # con un'attesa piu' lunga il NO non conta
  expect_warning(
    piu_lunga <- controllo_giri(
      letture,
      calendario,
      test_mezzi,
      giorni_conferma = 30
    ),
    "senza|nessuna lettura"
  )
  expect_identical(piu_lunga$problema, "confermato, senza letture dell'antenna")
  # niente da segnalare: tabella vuota, senza avvisi
  expect_no_warning(
    vuoto <- controllo_giri(letture, calendario[1, ], test_mezzi)
  )
  expect_equal(nrow(vuoto), 0)
  expect_named(vuoto, names(giri))
  expect_equal(nrow(controllo_giri(letture[0, ], calendario, test_mezzi)), 0)
})

test_that("il comune del GPS vale se coincide con quello del contenitore, altrimenti quello della posizione", {
  limena <- comuni_cantieri()[comuni_cantieri()$comune == "LIMENA", ]
  letture <- data.frame(
    # tre letture dentro Limena, con il GPS che dice altro
    comune_lettura = c("Cittadella", "CITTADELLA", "CITTADELLA", "LIMENA"),
    comune_fornitura = c("CITTADELLA", "LIMENA", NA, "CITTADELLA"),
    cantiere = NA_character_,
    latitudine = limena$latitudine,
    longitudine = limena$longitudine,
    RFID = c("0000000001", "0000000002", "0000000003", "0000000004"),
    stringsAsFactors = FALSE
  )
  corrette <- suppressMessages(correggi_comune_lettura(letture))

  # 1: il GPS coincide con il contenitore: resta, anche se il punto è a Limena
  # 2 e 3: il contenitore è a Limena, o non c'è: vale la posizione
  # 4: il GPS è già la posizione
  expect_identical(
    corrette$comune_lettura,
    c("CITTADELLA", "LIMENA", "LIMENA", "LIMENA")
  )
  expect_identical(corrette$cantiere, c("CAMPOSAMPIERO", "RUBANO", "RUBANO", "RUBANO"))

  # nell'elenco solo le letture cambiate, con il comune del contenitore
  elenco <- comuni_corretti(corrette)
  expect_identical(elenco$RFID, c("0000000002", "0000000003"))
  expect_identical(elenco$comune_gps, c("CITTADELLA", "CITTADELLA"))
  expect_identical(elenco$comune_fornitura, c("LIMENA", NA))
  expect_identical(elenco$comune_posizione, c("LIMENA", "LIMENA"))
})

test_that("due codici che diventano lo stesso RFID contano una lettura al giorno", {
  letture <- dplyr::tibble(
    data_ora_lettura = ora(c(
      "2026-03-02 08:00:00", "2026-03-02 09:00:00",
      "2026-03-02 10:00:00", "2026-03-03 08:00:00"
    )),
    data_lettura = as.Date(c("2026-03-02", "2026-03-02", "2026-03-02", "2026-03-03")),
    # il primo e il terzo sono lo stesso codice, il secondo letto male
    RFID = c("00000000000000ABCDEF1234", "00000100000000ABCDEF1234", "ABCDEF1234", "ABCDEF1234")
  )
  una <- una_lettura_al_giorno(formatta_rfid(letture))
  expect_identical(format(una$data_ora_lettura, "%d %H"), c("02 08", "03 08"))
  expect_identical(una$RFID, rep("ABCDEF1234", 2))
})

test_that("il controllo dei giri confronta le matricole senza zeri iniziali", {
  calendario <- dplyr::tibble(
    data_giro = as.Date("2026-03-02"),
    matricola_mezzo = "0101",
    id_giro = "G1",
    descrizione_servizio = "SECCO PAP",
    comune_giro = "ROSÀ",
    eseguito = "SI",
    pianificato = TRUE
  )
  letture <- dplyr::tibble(
    matricola_veicolo = "101",
    data_lettura = as.Date("2026-03-02"),
    id_giro = "G1"
  )
  test_mezzi <- dplyr::tibble(
    matricola_veicolo = "101",
    esito = "POSITIVO",
    data_test = as.Date("2026-01-01")
  )
  # il giro confermato ha le sue letture: niente da controllare
  expect_no_warning(
    da_controllare <- controllo_giri(letture, calendario, test_mezzi)
  )
  expect_identical(nrow(da_controllare), 0L)
})
