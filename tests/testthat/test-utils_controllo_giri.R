giorno <- as.Date("2026-10-20")

scrivi_giri <- function(righe, env = parent.frame()) {
  percorso <- withr::local_tempfile(fileext = ".csv", .local_envir = env)
  writeLines(righe, percorso)
  percorso
}

giri_prova <- function(env = parent.frame()) {
  giri_di_controllo(scrivi_giri(
    c(
      "data_giro,matricola_veicolo,servizio_giro",
      "2026-10-20,0101,SECCO PaP",
      "2026-10-20,303,SECCO PaP",
      "2026-10-20,202,SECCO PaP"
    ),
    env
  ))
}

antenne_prova <- function() {
  dplyr::tibble(
    data_ora_lettura = c(
      rep("2026-10-20 07:00:00", 9),
      "2026-10-21 07:00:00"
    ),
    matricola_veicolo = c("101", "101", "101", "303", "303", "202", "1000", "1000", "0101", "303"),
    servizio_giro = c(
      rep("SECCO PAP", 5), "UMIDO PAP", "SECCO PAP", "SECCO PAP", "SECCO PAP", "SECCO PAP"
    ),
    RFID = c("A", "B", "C", "D", "E", "F", "G", "Y", "A", "H"),
    servizio_atteso = c("SECCO", "SECCO", "SECCO", "SECCO", "SECCO", "UMIDO", "SECCO", "SECCO", "SECCO", "SECCO"),
    servizio_transponder = c("SECCO", "SECCO", "SECCO", "SECCO", NA, "UMIDO", "SECCO", "SECCO", "SECCO", "SECCO")
  )
}

storiche_prova <- function() {
  dplyr::tibble(
    data_ora_lettura = c(
      rep("2026-10-20 08:00:00", 4), # T1: A B C X
      rep("2026-10-20 09:00:00", 2), # T2: D Y
      rep("2026-10-20 10:00:00", 2), # T3: nessun RFID dei giri
      "2026-10-20 11:00:00", # T4: G, letto da un mezzo fuori lista
      "2026-10-21 08:00:00" # T1 di un altro giorno
    ),
    id_trasmissione = c(rep("T1", 4), rep("T2", 2), rep("T3", 2), "T4", "T1"),
    RFID = c("A", "B", "C", "X", "D", "Y", "Z", "W", "G", "A"),
    servizio_transponder = "SECCO"
  )
}

test_that("la lista dei giri si legge dal CSV", {
  giri <- giri_prova()
  expect_identical(nrow(giri), 3L)
  expect_identical(giri$data_giro, rep(giorno, 3))
  # gli zeri iniziali restano
  expect_identical(giri$matricola_veicolo, c("0101", "303", "202"))
  expect_identical(unique(giri$servizio_giro), "SECCO PaP")
})

test_that("il CSV dei giri accetta punto e virgola, date italiane, maiuscole e righe vuote", {
  percorso <- scrivi_giri(c(
    "DATA_GIRO;Matricola_Veicolo;servizio_giro",
    "20/10/2026;0101;SECCO PaP",
    ";;",
    "2026-10-21;303;SECCO PaP",
    "2026-10-21;303;SECCO PaP"
  ))
  giri <- giri_di_controllo(percorso)
  expect_identical(giri$matricola_veicolo, c("0101", "303"))
  expect_identical(giri$data_giro, as.Date(c("2026-10-20", "2026-10-21")))
})

test_that("il CSV dei giri dice che cosa non va", {
  expect_error(giri_di_controllo("non_esiste.csv"), "File dei giri non trovato")
  expect_error(
    giri_di_controllo(scrivi_giri(c("data_giro,matricola", "2026-10-20,1"))),
    "matricola_veicolo"
  )
  expect_error(
    giri_di_controllo(scrivi_giri(c(
      "data_giro,matricola_veicolo,servizio_giro",
      "2026-10-20,1,SECCO",
      "2026-10-20,,SECCO"
    ))),
    "riga del file\\): 3"
  )
  expect_error(
    giri_di_controllo(scrivi_giri(c(
      "data_giro,matricola_veicolo,servizio_giro",
      "ottobre,1,SECCO"
    ))),
    "AAAA-MM-GG o GG/MM/AAAA"
  )
})

test_that("le antenne dei giri tengono giorno, mezzo e servizio, senza badare a maiuscole e zeri", {
  risultato <- antenne_dei_giri(antenne_prova(), giri_prova())
  # 101 e 0101 sono lo stesso mezzo; 202 era in UMIDO PAP; 1000 non è in lista;
  # H è di un altro giorno
  expect_setequal(risultato$RFID, c("A", "B", "C", "D", "E"))
  expect_true(all(risultato$data_lettura == giorno))
  expect_true(all(risultato$chiave_mezzo %in% c("101", "303")))
})

test_that("le trasmissioni dei giri sono quelle che hanno letto lo stesso giorno un RFID dei giri", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  storiche_giri <- trasmissioni_dei_giri(storiche_prova(), antenne_giri)
  # T1 e T2; T3 non ha RFID in comune, T4 ha solo G (di un mezzo fuori lista),
  # T1 del 21/10 è un altro giro
  expect_setequal(unique(storiche_giri$id_trasmissione), c("T1", "T2"))
  expect_identical(nrow(storiche_giri), 6L)
  expect_true(all(storiche_giri$data_lettura == giorno))
})

test_that("ogni trasmissione va al mezzo con cui ha piu' RFID in comune", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  storiche_giri <- trasmissioni_dei_giri(storiche_prova(), antenne_giri)
  abbinamenti <- abbina_trasmissioni(storiche_giri, antenne_giri)
  expect_identical(abbinamenti$id_trasmissione, c("T1", "T2"))
  expect_identical(abbinamenti$chiave_mezzo, c("101", "303"))
  expect_identical(abbinamenti$condivisi, c(3L, 1L))
  expect_identical(abbinamenti$mezzi_in_comune, c(1L, 1L))
  fasce <- fasce_di_abbinamento(abbinamenti)
  expect_identical(fasce$trasmissioni, c(1L, 1L, 0L))
})

test_that("una trasmissione che condivide RFID con due mezzi va a quello con piu' RFID", {
  antenne_giri <- dplyr::tibble(
    data_lettura = giorno,
    chiave_mezzo = c("1", "1", "1", "2"),
    RFID = c("A", "B", "C", "D")
  )
  storiche_giri <- dplyr::tibble(
    data_lettura = giorno,
    id_trasmissione = "T1",
    RFID = c("A", "B", "C", "D")
  )
  abbinamenti <- abbina_trasmissioni(storiche_giri, antenne_giri)
  expect_identical(abbinamenti$chiave_mezzo, "1")
  expect_identical(abbinamenti$condivisi, 3L)
  expect_identical(abbinamenti$mezzi_in_comune, 2L)
})

test_that("le classi dicono chi ha letto che cosa", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  storiche_giri <- trasmissioni_dei_giri(storiche_prova(), antenne_giri)
  classi <- classi_rfid(antenne_giri, storiche_giri)
  per_rfid <- stats::setNames(as.character(classi$classe), classi$RFID)
  expect_identical(
    per_rfid[c("A", "B", "C", "D", "E", "X", "Y")],
    c(A = "Entrambi", B = "Entrambi", C = "Entrambi", D = "Entrambi", E = "Solo antenne", X = "Solo saponette", Y = "Solo saponette")
  )
  expect_s3_class(classi$classe, "factor")
})

test_that("il confronto per mezzo conta anche i mezzi senza letture", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  storiche_giri <- trasmissioni_dei_giri(storiche_prova(), antenne_giri)
  abbinamenti <- abbina_trasmissioni(storiche_giri, antenne_giri)
  per_mezzo <- confronto_per_mezzo(giri_prova(), antenne_giri, storiche_giri, abbinamenti)
  expect_identical(per_mezzo$matricola_veicolo, c("0101", "202", "303"))
  expect_identical(per_mezzo$trasmissioni, c(1L, 0L, 1L))
  expect_identical(per_mezzo$chip_antenne, c(3L, 0L, 2L))
  expect_identical(per_mezzo$chip_saponette, c(4L, 0L, 2L))
  expect_identical(per_mezzo$chip_entrambi, c(3L, 0L, 1L))
  expect_identical(per_mezzo$solo_antenne, c(0L, 0L, 1L))
  expect_identical(per_mezzo$solo_saponette, c(1L, 0L, 1L))
})

test_that("gli RFID solo delle saponette si dividono tra letti da un'antenna fuori lista e non letti", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  storiche_giri <- trasmissioni_dei_giri(storiche_prova(), antenne_giri)
  classi <- classi_rfid(antenne_giri, storiche_giri)
  altrove <- solo_saponette_altrove(classi, antenne_prova())
  # X non è letto da nessuna antenna; Y lo ha letto il mezzo 1000, fuori lista
  expect_identical(sum(altrove$n), 2L)
  expect_identical(
    altrove$n[grepl("fuori dai giri", altrove$motivo)],
    1L
  )
  expect_identical(
    altrove$n[grepl("Non letto", altrove$motivo)],
    1L
  )
})

test_that("i servizi per classe usano l'anagrafica di uno dei due sistemi", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  storiche_giri <- trasmissioni_dei_giri(storiche_prova(), antenne_giri)
  classi <- classi_rfid(antenne_giri, storiche_giri)
  servizi <- servizi_per_classe(classi, antenne_giri, storiche_giri)
  # E non ha servizio nelle antenne e non è nelle storiche: non censito
  e <- servizi[servizi$classe == "Solo antenne", ]
  expect_identical(e$servizio, "(non censito)")
  expect_identical(sum(servizi$n), nrow(classi))
})

test_that("i servizi dei mezzi della lista dicono che cosa hanno letto, anche senza giro", {
  antenne <- dplyr::tibble(
    data_ora_lettura = c(
      "2026-10-20 07:00:00", "2026-10-20 07:05:00", "2026-10-20 07:10:00",
      "2026-10-20 07:15:00", "2026-10-21 07:00:00"
    ),
    matricola_veicolo = c("101", "101", "101", "303", "101"),
    servizio_giro = c("SECCO PAP", NA, NA, "UMIDO PAP", "SECCO PAP"),
    servizio_atteso = c("SECCO", "SECCO", "CARTA", "UMIDO", "SECCO")
  )
  risultato <- servizi_dei_mezzi_della_lista(antenne, giri_prova())
  # 0101 e 101 sono lo stesso mezzo; il 21/10 non è in lista
  letture_101 <- risultato[risultato$matricola_veicolo == "0101", ]
  expect_identical(letture_101$servizio_giro, c("(senza giro)", "(senza giro)", "SECCO PAP"))
  expect_identical(letture_101$letture, c(1L, 1L, 1L))
  # la lettura senza giro con servizio atteso SECCO entra, quella con CARTA no
  expect_identical(
    as.character(letture_101$criterio),
    c(NA, "servizio dedotto dai censiti", "giro del calendario")
  )
  expect_identical(risultato$letture[risultato$matricola_veicolo == "303"], 1L)
  expect_false("202" %in% risultato$matricola_veicolo)
})

test_that("un mezzo senza giro entra con le letture del servizio atteso della lista", {
  antenne <- dplyr::tibble(
    data_ora_lettura = rep("2026-10-20 07:00:00", 6),
    matricola_veicolo = c("0101", "0101", "0101", "0101", "303", "303"),
    servizio_giro = c(NA, NA, NA, "UMIDO PAP", "SECCO PaP", NA),
    servizio_atteso = c("SECCO", "SECCO", "CARTA", "SECCO", "SECCO", "SECCO"),
    RFID = c("A", "B", "C", "D", "E", "F")
  )
  risultato <- antenne_dei_giri(antenne, giri_prova())
  # A e B: senza giro, servizio atteso SECCO. C: atteso CARTA. D: ha un altro giro.
  # E: giro della lista. F: senza giro e SECCO
  expect_setequal(risultato$RFID, c("A", "B", "E", "F"))
  criteri <- stats::setNames(as.character(risultato$criterio), risultato$RFID)
  expect_identical(
    criteri[c("A", "B", "E", "F")],
    c(A = "servizio dedotto dai censiti", B = "servizio dedotto dai censiti", E = "giro del calendario", F = "servizio dedotto dai censiti")
  )
})

test_that("il servizio atteso della lista si ricava dalla tabella di conversione", {
  percorso <- scrivi_giri(c(
    "data_giro,matricola_veicolo,servizio_giro",
    "2026-10-20,1,SECCO PaP",
    "2026-10-20,2,CARTA CONT.STRADALI"
  ))
  expect_identical(giri_di_controllo(percorso)$servizio_atteso, c("SECCO", "CARTA"))
  # un nome che la tabella non conosce ferma il report, con il modo di risolvere
  ignoto <- scrivi_giri(c(
    "data_giro,matricola_veicolo,servizio_giro",
    "2026-10-20,1,CARTA/CART.CONT.STRADALI"
  ))
  expect_error(giri_di_controllo(ignoto), "tipologie_servizio")
  # oppure lo si indica nel file
  con_colonna <- scrivi_giri(c(
    "data_giro,matricola_veicolo,servizio_giro,servizio_atteso",
    "2026-10-20,1,CARTA/CART.CONT.STRADALI,carta",
    "2026-10-20,2,SECCO PaP,"
  ))
  giri <- giri_di_controllo(con_colonna)
  expect_identical(giri$servizio_atteso, c("CARTA", "SECCO"))
})
