giorno <- as.Date("2026-10-20")

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

saponette_prova <- function() {
  dplyr::tibble(
    data_ora_lettura = c(
      rep("2026-10-20 08:00:00", 4), # T1: A B C X
      rep("2026-10-20 09:00:00", 2), # T2: D Y
      rep("2026-10-20 10:00:00", 2), # T3: nessun RFID dei giri
      "2026-10-20 11:00:00", # T4: G, letto da un mezzo fuori lista
      "2026-10-21 08:00:00" # T1 di un altro giorno
    ),
    matricola_saponetta = c(rep("T1", 4), rep("T2", 2), rep("T3", 2), "T4", "T1"),
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
  saponette_giri <- trasmissioni_dei_giri(saponette_prova(), antenne_giri)
  # T1 e T2; T3 non ha RFID in comune, T4 ha solo G (di un mezzo fuori lista),
  # T1 del 21/10 è un altro giro
  expect_setequal(unique(saponette_giri$matricola_saponetta), c("T1", "T2"))
  expect_identical(nrow(saponette_giri), 6L)
  expect_true(all(saponette_giri$data_lettura == giorno))
})

test_that("ogni trasmissione va al mezzo con cui ha piu' RFID in comune", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  saponette_giri <- trasmissioni_dei_giri(saponette_prova(), antenne_giri)
  abbinamenti <- abbina_trasmissioni(saponette_giri, antenne_giri)
  expect_identical(abbinamenti$matricola_saponetta, c("T1", "T2"))
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
  saponette_giri <- dplyr::tibble(
    data_lettura = giorno,
    matricola_saponetta = "T1",
    RFID = c("A", "B", "C", "D")
  )
  abbinamenti <- abbina_trasmissioni(saponette_giri, antenne_giri)
  expect_identical(abbinamenti$chiave_mezzo, "1")
  expect_identical(abbinamenti$condivisi, 3L)
  expect_identical(abbinamenti$mezzi_in_comune, 2L)
})

test_that("le classi dicono chi ha letto che cosa", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  saponette_giri <- trasmissioni_dei_giri(saponette_prova(), antenne_giri)
  classi <- classi_rfid(antenne_giri, saponette_giri)
  per_rfid <- stats::setNames(as.character(classi$classe), classi$RFID)
  expect_identical(
    per_rfid[c("A", "B", "C", "D", "E", "X", "Y")],
    c(A = "Entrambi", B = "Entrambi", C = "Entrambi", D = "Entrambi", E = "Solo antenne", X = "Solo saponette", Y = "Solo saponette")
  )
  expect_s3_class(classi$classe, "factor")
})

test_that("il confronto per mezzo conta anche i mezzi senza letture", {
  antenne_giri <- antenne_dei_giri(antenne_prova(), giri_prova())
  saponette_giri <- trasmissioni_dei_giri(saponette_prova(), antenne_giri)
  abbinamenti <- abbina_trasmissioni(saponette_giri, antenne_giri)
  per_mezzo <- confronto_per_mezzo(giri_prova(), antenne_giri, saponette_giri, abbinamenti)
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
  saponette_giri <- trasmissioni_dei_giri(saponette_prova(), antenne_giri)
  classi <- classi_rfid(antenne_giri, saponette_giri)
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
  saponette_giri <- trasmissioni_dei_giri(saponette_prova(), antenne_giri)
  classi <- classi_rfid(antenne_giri, saponette_giri)
  servizi <- servizi_per_classe(classi, antenne_giri, saponette_giri)
  # E non ha servizio nelle antenne e non è nelle saponette: non censito
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

# ---- Tipo di chip dei bidoni letti dalle saponette e non dalle antenne ----------

# Antenne dei giri (mezzi 101 e 303) e due trasmissioni delle saponette:
# - letti da entrambi: A, B, C (101) ed E (303);
# - solo saponette: L1 e L2 (chip solo LF), U1 e U2 (chip con UHF), P1 (possibile
#   UHF), N1 (senza tipo) e N2 (SCONOSCIUTO);
# - solo antenne: F.
chip_prova <- function() {
  antenne_giri <- dplyr::tibble(
    data_lettura = giorno,
    chiave_mezzo = c("101", "101", "101", "303", "303"),
    RFID = c("A", "B", "C", "E", "F"),
    descrizione_tipo_supporto_chip = c(
      "RECTANGLE UHF", "FASCETTA INTEGRATA LF + UHF", "POSSIBILE UHF",
      "RECTANGLE UHF", "RECTANGLE LF + UHF"
    )
  )
  saponette_giri <- dplyr::tibble(
    data_lettura = giorno,
    matricola_saponetta = c(rep("T1", 6), rep("T2", 5)),
    RFID = c("A", "B", "C", "L1", "U1", "P1", "E", "L2", "U2", "N1", "N2"),
    descrizione_tipo_supporto_chip = c(
      "RECTANGLE UHF", "FASCETTA INTEGRATA LF + UHF", "POSSIBILE UHF",
      "BINTAG LF 125", "RECTANGLE UHF", "POSSIBILE UHF",
      "RECTANGLE UHF", "TOKEN - PORTACHIAVI LF", "FASCETTA METALLICA LF + UHF",
      NA, "SCONOSCIUTO"
    ),
    servizio_transponder = c(rep("SECCO", 10), NA)
  )
  classi <- classi_rfid(antenne_giri, saponette_giri)
  list(
    antenne_giri = antenne_giri,
    saponette_giri = saponette_giri,
    classi = classi,
    classi_chip = classi_con_chip(classi, antenne_giri, saponette_giri),
    abbinamenti = abbina_trasmissioni(saponette_giri, antenne_giri),
    # U1 lo ha letto anche un'antenna fuori dai giri della lista
    antenne_tutte = dplyr::tibble(
      data_ora_lettura = "2026-10-20 07:00:00",
      RFID = c("A", "U1")
    )
  )
}

test_that("le categorie di chip dicono se l'antenna poteva leggerlo", {
  categoria <- categoria_chip(c(
    "BINTAG LF 125", "TOKEN - PORTACHIAVI LF", "RECTANGLE UHF", "FASCETTA INTEGRATA LF + UHF",
    "SACCHETTO UHF", "POSSIBILE UHF", "SCONOSCIUTO", NA, "UN TIPO MAI VISTO"
  ))
  expect_identical(
    as.character(categoria),
    c(
      "Chip solo LF", "Chip solo LF", "Chip con UHF", "Chip con UHF", "Chip con UHF",
      "Possibile UHF", "Tipo non determinabile", "Tipo non determinabile", "Tipo non determinabile"
    )
  )
  expect_identical(levels(categoria), categorie_chip())
  # ogni tipo della tabella ha una categoria, e solo BINTAG e TOKEN sono solo LF
  tutti <- categoria_chip(tipi_supporto_chip()$descrizione_tipo_supporto_chip)
  expect_identical(
    tipi_supporto_chip()$id_tipo_supporto_chip[tutti == "Chip solo LF"],
    c(2L, 9L)
  )
})

test_that("l'esito di un bidone letto dalle saponette dipende dalle antenne e dal chip", {
  esito <- esito_antenne(
    c("Entrambi", "Solo saponette", "Solo saponette", "Solo saponette", "Solo saponette", "Solo antenne"),
    c("Chip con UHF", "Chip solo LF", "Chip con UHF", "Possibile UHF", "Tipo non determinabile", "Chip con UHF")
  )
  expect_identical(as.character(esito), c(esiti_antenne()[1:5], NA))
  expect_identical(levels(esito), esiti_antenne())
})

test_that("il tipo di chip si prende da uno dei due sistemi e si lega alle classi", {
  dati <- chip_prova()
  chip <- dati$classi_chip
  tipo <- stats::setNames(chip$descrizione_tipo_supporto_chip, chip$RFID)
  expect_identical(nrow(chip), nrow(dati$classi))
  expect_identical(unname(tipo[c("A", "L1", "F", "N1")]), c("RECTANGLE UHF", "BINTAG LF 125", "RECTANGLE LF + UHF", NA))
  # se un sistema non ha il tipo si usa quello dell'altro
  senza_tipo_saponette <- dplyr::select(dati$saponette_giri, -"descrizione_tipo_supporto_chip")
  solo_antenne <- chip_degli_rfid(dati$antenne_giri, senza_tipo_saponette)
  expect_identical(solo_antenne$descrizione_tipo_supporto_chip[solo_antenne$RFID == "A"], "RECTANGLE UHF")
  expect_true(is.na(solo_antenne$descrizione_tipo_supporto_chip[solo_antenne$RFID == "L1"]))
  # esito per classe: i solo antenne non ne hanno
  esiti <- stats::setNames(as.character(chip$esito), chip$RFID)
  expect_identical(
    unname(esiti[c("A", "L1", "U1", "P1", "N1", "N2", "F")]),
    c(
      "Letto dalle antenne", "Non letto: chip solo LF", "Non letto: chip con UHF",
      "Non letto: possibile UHF", "Non letto: tipo non determinabile",
      "Non letto: tipo non determinabile", NA
    )
  )
})

test_that("il riepilogo per chip conta i bidoni letti dalle saponette e la quota delle antenne", {
  chip <- chip_prova()$classi_chip
  per_categoria <- riepilogo_per_chip(chip, "categoria")
  expect_identical(as.character(per_categoria$categoria), categorie_chip())
  # solo LF: 2 bidoni, nessuno letto dalle antenne; con UHF: 5 (A, B, E, U1, U2) di cui
  # 3 letti, e F letto solo dalle antenne
  expect_identical(per_categoria$saponette, c(2L, 5L, 2L, 2L))
  expect_identical(per_categoria$entrambi, c(0L, 3L, 1L, 0L))
  expect_identical(per_categoria$solo_saponette, c(2L, 2L, 1L, 2L))
  expect_identical(per_categoria$solo_antenne, c(0L, 1L, 0L, 0L))
  expect_equal(per_categoria$quota_antenne, c(0, 3 / 5, 1 / 2, 0))
  # i totali tornano con le classi
  expect_identical(sum(per_categoria$saponette), sum(chip$classe != "Solo antenne"))

  per_tipo <- riepilogo_per_chip(chip)
  riga <- function(tipo) per_tipo[per_tipo$descrizione == tipo, ]
  expect_identical(riga("BINTAG LF 125")$solo_saponette, 1L)
  expect_identical(riga("RECTANGLE UHF")$saponette, 3L)
  expect_identical(riga("RECTANGLE UHF")$entrambi, 2L)
  expect_identical(riga("(nessun tipo)")$solo_saponette, 1L)
  # l'ordine: per categoria e poi nell'ordine dei tipi, con (nessun tipo) in fondo alla sua
  expect_identical(per_tipo$descrizione[per_tipo$categoria == "Tipo non determinabile"], c("SCONOSCIUTO", "(nessun tipo)"))
  expect_identical(per_tipo$categoria, sort(per_tipo$categoria))
})

test_that("la sintesi dice se le antenne sono giustificate", {
  sintesi <- sintesi_chip(chip_prova()$classi_chip)
  expect_identical(sintesi$saponette, 11L)
  expect_identical(sintesi$antenne, 4L)
  expect_identical(sintesi$non_letti, 7L)
  expect_identical(
    c(sintesi$non_letti_solo_lf, sintesi$non_letti_con_uhf, sintesi$non_letti_possibile_uhf, sintesi$non_letti_non_determinabile),
    c(2L, 2L, 1L, 2L)
  )
  # le parti tornano al totale
  expect_identical(
    sintesi$non_letti,
    sintesi$non_letti_solo_lf + sintesi$non_letti_con_uhf +
      sintesi$non_letti_possibile_uhf + sintesi$non_letti_non_determinabile
  )
  expect_equal(sintesi$copertura, 4 / 11)
  # senza i non letti solo LF: 4 / (11 - 2)
  expect_equal(sintesi$copertura_senza_lf, 4 / 9)
  # chip con UHF noto: letti A, B, E contro 2 non letti
  expect_equal(sintesi$copertura_con_uhf, 3 / 5)
  # con i possibili UHF: (3 + 1) / (3 + 1 + 2 + 1)
  expect_equal(sintesi$copertura_con_uhf_possibile, 4 / 7)
  # nessun chip solo LF letto dalle antenne
  expect_identical(sintesi$antenne_su_chip_solo_lf, 0L)

  # se un'antenna ha letto un chip solo LF e' un dato da controllare
  anomalo <- chip_prova()$classi_chip
  anomalo$descrizione_tipo_supporto_chip[anomalo$RFID == "F"] <- "BINTAG LF 125"
  anomalo$categoria <- categoria_chip(anomalo$descrizione_tipo_supporto_chip)
  expect_identical(sintesi_chip(anomalo)$antenne_su_chip_solo_lf, 1L)

  # senza bidoni letti dalle saponette le coperture sono vuote
  vuoto <- sintesi_chip(chip_prova()$classi_chip[0, ])
  expect_identical(vuoto$saponette, 0L)
  expect_true(is.na(vuoto$copertura))
})

test_that("i bidoni solo delle saponette si dividono per chip e per dove li hanno letti le antenne", {
  dati <- chip_prova()
  conteggi <- solo_saponette_per_categoria(dati$classi_chip, dati$antenne_tutte)
  # tutte le categorie, con letto_altrove falso e vero
  expect_identical(nrow(conteggi), 8L)
  n <- function(categoria, altrove) conteggi$n[conteggi$categoria == categoria & conteggi$letto_altrove == altrove]
  # U1 lo ha letto un'antenna fuori lista; U2 nessuna
  expect_identical(n("Chip con UHF", TRUE), 1L)
  expect_identical(n("Chip con UHF", FALSE), 1L)
  expect_identical(n("Chip solo LF", FALSE), 2L)
  expect_identical(n("Chip solo LF", TRUE), 0L)
  expect_identical(sum(conteggi$n), sum(dati$classi_chip$classe == "Solo saponette"))
})

test_that("l'esito per mezzo conta i bidoni delle saponette abbinate a ogni mezzo", {
  dati <- chip_prova()
  giri <- dplyr::tibble(
    data_giro = giorno,
    matricola_veicolo = c("0101", "303"),
    servizio_giro = "SECCO PaP"
  )
  persi <- persi_per_mezzo(giri, dati$saponette_giri, dati$abbinamenti, dati$classi_chip)
  # tutti e cinque gli esiti per ogni mezzo
  expect_identical(nrow(persi), 10L)
  conta <- function(mezzo, esito) persi$n[persi$matricola_veicolo == mezzo & persi$esito == esito]
  # T1 e' del 101 (matricola 0101 nella lista): A, B, C letti; L1, U1, P1 no
  expect_identical(conta("0101", "Letto dalle antenne"), 3L)
  expect_identical(conta("0101", "Non letto: chip solo LF"), 1L)
  expect_identical(conta("0101", "Non letto: chip con UHF"), 1L)
  expect_identical(conta("0101", "Non letto: possibile UHF"), 1L)
  # T2 e' del 303: E letto; L2 (LF), U2 (UHF), N1 e N2 (non determinabili)
  expect_identical(conta("303", "Letto dalle antenne"), 1L)
  expect_identical(conta("303", "Non letto: tipo non determinabile"), 2L)
  expect_identical(sum(persi$n), sum(dati$classi_chip$classe != "Solo antenne"))
})

test_that("l'elenco dei bidoni con UHF non letti ha mezzo, tipo, servizio e dove li hanno letti", {
  dati <- chip_prova()
  giri <- dplyr::tibble(
    data_giro = giorno,
    matricola_veicolo = c("0101", "303"),
    servizio_giro = "SECCO PaP"
  )
  elenco <- bidoni_uhf_non_letti(dati$classi_chip, dati$saponette_giri, dati$abbinamenti, giri, dati$antenne_tutte)
  # U1 e U2 (con UHF) e P1 (possibile UHF); i LF e i non determinabili non ci sono
  expect_setequal(elenco$RFID, c("U1", "U2", "P1"))
  # prima i chip con UHF non letti da nessuna antenna: U2, poi U1 (letto altrove), poi P1
  expect_identical(elenco$RFID, c("U2", "U1", "P1"))
  riga <- function(rfid) elenco[elenco$RFID == rfid, ]
  expect_identical(riga("U1")$matricola_veicolo, "0101")
  expect_identical(riga("U2")$matricola_veicolo, "303")
  expect_identical(riga("U2")$tipo_chip, "FASCETTA METALLICA LF + UHF")
  expect_identical(riga("U1")$servizio, "SECCO")
  expect_true(riga("U1")$letto_altrove)
  expect_false(riga("U2")$letto_altrove)
})
