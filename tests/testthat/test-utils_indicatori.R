istante <- function(x) as.POSIXct(x, tz = "UTC")

lettura <- function(rfid, ora, stato = "PIA", fornitura = NA, volume = NA, servizio = NA, mezzo = NA, saponetta = NA, giro = NA, atteso = NA) {
  dplyr::tibble(
    data_ora_lettura = istante(ora),
    data_lettura = as.Date(ora),
    RFID = rfid,
    presente_a_database = stato,
    id_fornitura = fornitura,
    volume = volume,
    servizio_transponder = servizio,
    servizio_atteso = atteso,
    servizio_giro = giro,
    matricola_veicolo = mezzo,
    matricola_saponetta = saponetta
  )
}

scartata <- function(rfid, ora, motivo, mezzo = NA, saponetta = NA) {
  dplyr::tibble(
    data_ora_lettura = istante(ora),
    data_lettura = as.Date(ora),
    RFID = rfid,
    matricola_veicolo = mezzo,
    matricola_saponetta = saponetta,
    motivo = motivo
  )
}

unita_prova <- function() {
  g <- "2026-10-20 07:00:00"
  unita_di_lettura(
    antenne = dplyr::bind_rows(
      lettura("R1", g, "PIA", "F1", 120, "SECCO"),
      lettura("R2", g, "PIA", "F2", 120, "SECCO"),
      lettura("R3", g, "PIT"),
      lettura("R4", g, "NP")
    ),
    saponette = dplyr::bind_rows(
      lettura("R1", "2026-10-20 08:10:00", "PIA", "F1", 120, "SECCO"),
      lettura("R5", g, "PIA", "F5", 120, "SECCO"),
      lettura("R6", g, "NP")
    ),
    scartate_antenne = dplyr::bind_rows(
      scartata("R7", g, "codice_non_compatibile"),
      scartata("R8", g, "doppione")
    ),
    scartate_saponette = dplyr::bind_rows(
      scartata("R7", g, "codice_non_compatibile"),
      scartata(NA, g, "senza_rfid")
    )
  )
}

test_that("una lettura in comune ai due sistemi conta una volta, anche se scartata", {
  unita <- unita_prova()
  sistemi <- stats::setNames(as.character(unita$sistema), unita$RFID)
  expect_identical(sistemi[["R1"]], "Entrambe")
  expect_identical(sistemi[["R7"]], "Entrambe")
  expect_identical(sistemi[["R5"]], "Solo saponette")
  expect_identical(sistemi[["R2"]], "Solo antenne")
  stati <- stats::setNames(as.character(unita$stato), unita$RFID)
  expect_identical(stati[["R1"]], "PIA")
  expect_identical(stati[["R3"]], "PIT")
  expect_identical(stati[["R4"]], "NP")
  expect_identical(stati[["R7"]], "Scartata")
  # un doppione e una riga senza RFID non sono letture distinte
  expect_false(any(c("R8") %in% unita$RFID))
  expect_equal(nrow(unita), 7)
  # i dati della fornitura vengono dal sistema che li ha
  expect_identical(unita$id_fornitura[unita$RFID == "R5"], "F5")
})

test_that("lo stato di una lettura e' il migliore dei due sistemi", {
  g <- "2026-10-20 07:00:00"
  unita <- unita_di_lettura(
    antenne = lettura("R1", g, "NP"),
    saponette = lettura("R1", g, "PIA", "F1", 120, "SECCO"),
    scartate_antenne = scartata(character(0), character(0), character(0)),
    scartate_saponette = scartata(character(0), character(0), character(0))
  )
  expect_identical(as.character(unita$stato), "PIA")
  expect_identical(as.character(unita$sistema), "Entrambe")
})

test_that("gli indicatori distinguono compatibili, associate (PIA), non associate e scartate", {
  indicatori <- indicatori_letture(unita_prova())
  expect_identical(indicatori$letture_solo_saponette, 2)
  expect_identical(indicatori$letture_solo_antenne, 3)
  expect_identical(indicatori$letture_in_comune, 2)
  expect_identical(indicatori$letture_totali, 7)
  # compatibili: quelle che la pulizia tiene, anche se il database non le conosce
  expect_identical(indicatori$compatibili_solo_saponette, 2)
  expect_identical(indicatori$compatibili_solo_antenne, 3)
  expect_identical(indicatori$compatibili_in_comune, 1)
  expect_identical(indicatori$compatibili_totali, 6)
  # associate: le compatibili con una fornitura
  expect_identical(indicatori$associate_solo_saponette, 1)
  expect_identical(indicatori$associate_solo_antenne, 1)
  expect_identical(indicatori$associate_in_comune, 1)
  expect_identical(indicatori$associate_totali, 3)
  # non associate: PIT e NP
  expect_identical(indicatori$non_associate_solo_saponette, 1)
  expect_identical(indicatori$non_associate_solo_antenne, 2)
  expect_identical(indicatori$non_associate_in_comune, 0)
  expect_identical(indicatori$non_associate_totali, 3)
  # scartate: solo quello che la pulizia toglie
  expect_identical(indicatori$scartate_solo_saponette, 0)
  expect_identical(indicatori$scartate_solo_antenne, 0)
  expect_identical(indicatori$scartate_in_comune, 1)
  expect_identical(indicatori$scartate_totali, 1)
  # indice di scarto: (sole del sistema + in comune) / letture totali
  expect_equal(indicatori$indice_scarto_saponette, 1 / 7)
  expect_equal(indicatori$indice_scarto_antenne, 1 / 7)
  expect_equal(indicatori$indice_scarto_totale, 1 / 7)
  # indice di associazione: associate (del sistema + in comune) / compatibili totali
  expect_equal(indicatori$associazione_in_comune, 1 / 6)
  expect_equal(indicatori$associazione_saponette, 2 / 6)
  expect_equal(indicatori$associazione_antenne, 2 / 6)
  # indice di efficacia: compatibili (del sistema + in comune) / compatibili totali
  expect_equal(indicatori$efficacia_in_comune, 1 / 6)
  expect_equal(indicatori$efficacia_saponette, 3 / 6)
  expect_equal(indicatori$efficacia_antenne, 4 / 6)
  # le letture totali sono la somma dei tre insiemi disgiunti, e le compatibili
  # sono associate piu' non associate
  expect_identical(
    indicatori$letture_totali,
    indicatori$letture_solo_saponette + indicatori$letture_solo_antenne + indicatori$letture_in_comune
  )
  expect_identical(
    indicatori$compatibili_totali,
    indicatori$associate_totali + indicatori$non_associate_totali
  )
  expect_identical(
    indicatori$letture_totali,
    indicatori$compatibili_totali + indicatori$scartate_totali
  )
})

test_that("senza letture di un sistema gli indici che non si possono calcolare mancano", {
  vuote <- unita_prova()[0, ]
  indicatori <- indicatori_letture(vuote)
  expect_identical(indicatori$letture_totali, 0)
  expect_true(is.na(indicatori$indice_scarto_totale))
  expect_true(is.na(indicatori$efficacia_saponette))
  # solo saponette: nessuna lettura in comune, efficacia delle saponette 1
  g <- "2026-10-20 07:00:00"
  solo <- unita_di_lettura(
    lettura(character(0), character(0)),
    lettura("R1", g, "PIA", "F1", 120, "SECCO"),
    scartata(character(0), character(0), character(0)),
    scartata(character(0), character(0), character(0))
  )
  indicatori <- indicatori_letture(solo)
  expect_identical(indicatori$letture_solo_antenne, 0)
  expect_equal(indicatori$efficacia_saponette, 1)
  expect_equal(indicatori$efficacia_antenne, 0)
})

test_that("la composizione dice come si dividono le letture di ogni sistema", {
  composizione <- composizione_letture(unita_prova())
  conta <- function(sistema, stato) {
    composizione$letture[composizione$sistema == sistema & composizione$stato == stato]
  }
  # le saponette: R1 e R5 PIA, R6 NP, R7 scartata
  expect_identical(conta("Saponette", "PIA"), 2L)
  expect_identical(conta("Saponette", "PIT"), 0L)
  expect_identical(conta("Saponette", "NP"), 1L)
  expect_identical(conta("Saponette", "Scartata"), 1L)
  expect_identical(conta("Antenne", "PIT"), 1L)
  expect_equal(
    sum(composizione$quota[composizione$sistema == "Antenne"]),
    1
  )
})

test_that("le righe senza chiave si contano a parte", {
  g <- "2026-10-20 07:00:00"
  nonconteggiate <- righe_non_conteggiate(
    dplyr::bind_rows(scartata("R7", g, "codice_nullo"), scartata("R8", g, "doppione")),
    dplyr::bind_rows(scartata(NA, g, "senza_rfid"), scartata("R1", g, "doppione"), scartata("R2", g, "doppione"))
  )
  expect_identical(nonconteggiate$sistema, c("Antenne", "Saponette", "Saponette"))
  expect_identical(nonconteggiate$righe, c(1L, 2L, 1L))
  expect_identical(nonconteggiate$motivo, c("doppione", "doppione", "senza_rfid"))
})

test_that("le letture si leggono dai CSV della pipeline con i tipi giusti", {
  percorso <- scrivi_csv(c(
    "data_ora_lettura,data_lettura,RFID,presente_a_database,volume,matricola_veicolo",
    "2026-10-20 07:00:00,2026-10-20,R1,PIA,120,101",
    "2026-10-21,,R2,NP,,102"
  ))
  letture <- leggi_letture_indicatori(percorso)
  expect_s3_class(letture$data_ora_lettura, "POSIXct")
  expect_identical(letture$data_lettura, as.Date(c("2026-10-20", "2026-10-21")))
  expect_identical(letture$volume, c(120, NA))
  # le colonne che mancano ci sono, vuote
  expect_true(all(is.na(letture$matricola_saponetta)))
  expect_true(all(colonne_letture_indicatori() %in% names(letture)))
  # un sistema che non c'e': tabella senza righe, con le stesse colonne
  senza <- leggi_letture_indicatori("non_esiste.csv")
  expect_identical(nrow(senza), 0L)
  expect_identical(names(senza), names(letture))
  expect_identical(nrow(leggi_letture_indicatori(NULL)), 0L)
  expect_identical(nrow(leggi_letture_indicatori("")), 0L)
})

test_that("il periodo tiene i giorni compresi, estremi inclusi, e vuoto non filtra", {
  x <- dplyr::tibble(data_lettura = as.Date(c("2026-01-01", "2026-06-30", "2026-07-01", "2026-12-31")))
  expect_identical(nrow(nel_periodo(x)), 4L)
  expect_identical(nrow(nel_periodo(x, "2026-06-30", "2026-07-01")), 2L)
  expect_identical(nrow(nel_periodo(x, "2026-07-01", "")), 2L)
  expect_identical(nrow(nel_periodo(x, NULL, as.Date("2026-01-01"))), 1L)
})

giri_analisi_prova <- function(righe, env = parent.frame()) {
  giri_di_analisi(scrivi_giri(
    c("data_giro,matricola_veicolo,servizio_giro,matricola_saponetta,ora_inizio,ora_fine", righe),
    env
  ))
}

letture_giri_prova <- function() {
  g <- "2026-10-20"
  list(
    antenne = dplyr::bind_rows(
      lettura("R1", paste(g, "07:00:00"), "PIA", "F1", 120, "SECCO", mezzo = "101", giro = "SECCO PAP", atteso = "SECCO"),
      lettura("R2", paste(g, "07:20:00"), "PIA", "F2", 120, "SECCO", mezzo = "101", giro = "SECCO PAP", atteso = "SECCO"),
      # un altro mezzo, e lo stesso mezzo in un altro servizio
      lettura("R3", paste(g, "07:10:00"), "PIA", "F3", 120, "SECCO", mezzo = "303", giro = "SECCO PAP", atteso = "SECCO"),
      lettura("R4", paste(g, "10:00:00"), "PIA", "F4", 120, "CARTA", mezzo = "101", giro = "CARTA PAP", atteso = "CARTA")
    ),
    saponette = dplyr::bind_rows(
      lettura("R1", paste(g, "07:05:00"), "PIA", "F1", 120, "SECCO", saponetta = "S1"),
      lettura("R9", paste(g, "07:40:00"), "NP", saponetta = "S1"),
      # fuori dalla finestra: 06:30 - 07:50
      lettura("R10", paste(g, "09:00:00"), "PIA", "F10", 120, "SECCO", saponetta = "S1"),
      # un'altra saponetta
      lettura("R11", paste(g, "07:10:00"), "PIA", "F11", 120, "SECCO", saponetta = "S2"),
      # lo stesso giorno dopo
      lettura("R12", "2026-10-21 07:10:00", "PIA", "F12", 120, "SECCO", saponetta = "S1")
    ),
    scartate_antenne = dplyr::bind_rows(
      scartata("R7", paste(g, "07:30:00"), "codice_non_compatibile", mezzo = "0101"),
      scartata("R13", paste(g, "10:00:00"), "codice_non_compatibile", mezzo = "101"),
      scartata("R14", paste(g, "07:30:00"), "codice_non_compatibile", mezzo = "303")
    ),
    scartate_saponette = dplyr::bind_rows(
      scartata("R7", paste(g, "07:45:00"), "codice_non_compatibile", saponetta = "S1"),
      scartata("R15", paste(g, "09:30:00"), "codice_nullo", saponetta = "S1")
    )
  )
}

test_that("il file dei giri ha la saponetta e, facoltativi, gli orari", {
  giri <- giri_analisi_prova(c(
    "2026-10-20,0101,SECCO PaP,S1,,",
    "2026-10-21,202,SECCO PaP,,06:00,12:30:00"
  ))
  expect_identical(giri$matricola_saponetta, c("S1", NA))
  expect_identical(giri$ora_inizio, c(NA, "06:00"))
  expect_identical(giri$ora_fine, c(NA, "12:30:00"))
  # senza queste colonne il file vale lo stesso, con i campi vuoti
  senza <- giri_di_analisi(scrivi_giri(c(
    "data_giro,matricola_veicolo,servizio_giro",
    "2026-10-20,101,SECCO PaP"
  )))
  expect_true(is.na(senza$matricola_saponetta))
  expect_true(is.na(senza$ora_inizio))
})

test_that("antenne e saponette di un giro stanno nella stessa finestra oraria", {
  giri <- giri_analisi_prova("2026-10-20,0101,SECCO PaP,S1,,")
  selezione <- seleziona_per_giri(letture_giri_prova(), giri, margine_minuti = 30)

  # antenne: R1 e R2 del mezzo 101 con il servizio del giro; non R4 (CARTA), non R3
  expect_setequal(selezione$antenne$RFID, c("R1", "R2"))
  expect_true(all(selezione$antenne$riga_giro == 1L))
  # la finestra e' dalla prima all'ultima lettura delle antenne, piu' 30 minuti
  finestra <- selezione$finestre
  expect_identical(finestra$finestra, "letture delle antenne")
  expect_equal(finestra$da, istante("2026-10-20 06:30:00"))
  expect_equal(finestra$a, istante("2026-10-20 07:50:00"))
  # saponette: della saponetta S1, in quel giorno e dentro la finestra
  expect_setequal(selezione$saponette$RFID, c("R1", "R9"))
  # le scartate dello stesso mezzo e della stessa saponetta, dentro la finestra
  expect_identical(selezione$scartate_antenne$RFID, "R7")
  expect_identical(selezione$scartate_saponette$RFID, "R7")

  indicatori <- indicatori_per_giro(selezione)
  # R1 in comune, R2 solo antenne, R9 solo saponette, R7 scartata in comune
  expect_identical(indicatori$giro, c("1", "Totale"))
  expect_identical(indicatori$letture_totali, c(4, 4))
  expect_identical(indicatori$letture_in_comune, c(2, 2))
  expect_identical(indicatori$finestra, c("letture delle antenne", NA))
})

test_that("la finestra puo' essere indicata nel file, o essere la giornata intera", {
  # orari indicati: valgono al posto delle letture delle antenne
  giri <- giri_analisi_prova("2026-10-20,101,SECCO PaP,S1,09:00,10:30")
  indicata <- seleziona_per_giri(letture_giri_prova(), giri)
  expect_identical(indicata$finestre$finestra, "orari indicati")
  expect_setequal(indicata$saponette$RFID, "R10")
  expect_setequal(indicata$scartate_saponette$RFID, "R15")
  expect_setequal(indicata$scartate_antenne$RFID, "R13")

  # senza letture delle antenne ne' orari: tutta la giornata
  intera <- seleziona_per_giri(
    letture_giri_prova(),
    giri_analisi_prova("2026-10-20,404,SECCO PaP,S1,,")
  )
  expect_identical(intera$finestre$finestra, "giornata intera")
  expect_identical(nrow(intera$antenne), 0L)
  expect_setequal(intera$saponette$RFID, c("R1", "R9", "R10"))

  # nessuna saponetta indicata: solo antenne
  senza_saponetta <- seleziona_per_giri(
    letture_giri_prova(),
    giri_analisi_prova("2026-10-20,101,SECCO PaP,,,")
  )
  expect_identical(nrow(senza_saponetta$saponette), 0L)
  expect_setequal(senza_saponetta$antenne$RFID, c("R1", "R2"))
})

test_that("piu' giri valgono insieme e una lettura in due giri conta una volta nel totale", {
  giri <- giri_analisi_prova(c(
    "2026-10-20,101,SECCO PaP,S1,,",
    "2026-10-20,101,SECCO PaP,S1,,"
  ))
  # due righe uguali nel file sono lo stesso giro
  expect_equal(nrow(giri), 1)
  giri <- giri_analisi_prova(c(
    "2026-10-20,101,SECCO PaP,S1,06:00,08:00",
    "2026-10-20,101,SECCO PaP,S1,07:30,09:30"
  ))
  selezione <- seleziona_per_giri(letture_giri_prova(), giri)
  indicatori <- indicatori_per_giro(selezione)
  # R9 (07:40) sta in tutte e due le finestre: nel totale una volta sola
  expect_gt(sum(selezione$saponette$RFID == "R9"), 1)
  totale <- indicatori[indicatori$giro == "Totale", ]
  unita <- unita_di_lettura(
    selezione$antenne,
    selezione$saponette,
    selezione$scartate_antenne,
    selezione$scartate_saponette
  )
  expect_identical(totale$letture_totali, as.numeric(nrow(unita)))
  expect_lt(totale$letture_totali, sum(indicatori$letture_totali[indicatori$giro != "Totale"]))
})
