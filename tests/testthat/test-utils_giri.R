# Letture di prova: due mezzi, tre giorni, giri in comuni diversi.
letture_giri <- function() {
  quando <- function(testo) as.POSIXct(testo, tz = "UTC")
  dplyr::tibble(
    giorno_lettura = quando(c(
      "2025-03-03 06:10:00",
      "2025-03-03 06:40:00",
      "2025-03-03 07:05:00",
      "2025-03-03 14:20:00",
      "2025-03-03 08:00:00",
      "2025-03-04 06:15:00",
      "2025-03-05 23:59:59"
    )),
    targa_veicolo = c(rep("AA111AA", 4), "BB222BB", "AA111AA", "BB222BB"),
    matricola_veicolo = c(rep("M1", 4), "M2", "M1", "M2"),
    RFID = c("R1", "R2", "R1", "R3", "R4", "R1", "R5"),
    presente_a_database = "Presente",
    servizio_atteso = c(rep("SECCO PAP", 3), "CARTA/CARTONE PAP", "VETRO PAP", "SECCO PAP", NA),
    comune_lettura = c(rep("ASIAGO", 3), "GALLIO", "ROANA", "ASIAGO", NA),
    comune_geografico = c("ASIAGO", "ASIAGO", "ROANA", "GALLIO", "ROANA", "FOZA", NA),
    fuori_comune = c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE),
    fuori_giro = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE),
    latitudine = 45.87,
    longitudine = 11.51
  )
}

test_that("l'elenco dei mezzi e i giorni delle letture", {
  d <- letture_giri()
  expect_identical(
    mezzi_presenti(d),
    c("M1 \u00b7 AA111AA" = "M1", "M2 \u00b7 BB222BB" = "M2")
  )
  expect_identical(mezzi_presenti(d[0, ]), character(0))
  expect_identical(mezzi_presenti(NULL), character(0))
  expect_identical(giorni_letture(d), as.Date(c("2025-03-03", "2025-03-05")))
  expect_null(giorni_letture(d[0, ]))
  expect_null(giorni_letture(NULL))
  # sul dataset di esempio: cinque mezzi
  expect_length(mezzi_presenti(dati_esempio()), 5)
})

test_that("la ricerca trova le letture di un mezzo nei giorni scelti", {
  d <- letture_giri()
  # un giorno, tutti i mezzi: in ordine di tempo, con il giorno
  giorno <- cerca_giri(d, "", "2025-03-03", "2025-03-03")
  expect_identical(giorno$RFID, c("R1", "R2", "R1", "R4", "R3"))
  expect_true(all(giorno$giorno == as.Date("2025-03-03")))
  expect_identical(cerca_giri(d, NULL, "2025-03-03", "2025-03-03"), giorno)
  expect_identical(cerca_giri(d, NA, "2025-03-03", "2025-03-03"), giorno)
  # un mezzo
  expect_identical(
    cerca_giri(d, "M1", "2025-03-03", "2025-03-03")$RFID,
    c("R1", "R2", "R1", "R3")
  )
  # un intervallo: gli estremi sono compresi, fino all'ultimo secondo
  expect_equal(nrow(cerca_giri(d, "", "2025-03-03", "2025-03-05")), 7)
  expect_identical(cerca_giri(d, "M2", "2025-03-04", "2025-03-05")$RFID, "R5")
  expect_equal(nrow(cerca_giri(d, "M2", "2025-03-04", "2025-03-04")), 0)
  expect_equal(nrow(cerca_giri(d, "M9", "2025-03-03", "2025-03-05")), 0)
  # solo le letture con un'incongruenza
  intervallo <- function(...) cerca_giri(d, "", "2025-03-03", "2025-03-05", ...)
  expect_identical(intervallo(solo = "fuori_giro")$RFID, c("R1", "R1"))
  expect_identical(intervallo(solo = "fuori_comune")$RFID, "R2")
  expect_identical(
    intervallo(solo = c("fuori_giro", "fuori_comune"))$RFID,
    c("R2", "R1", "R1")
  )
  expect_equal(nrow(intervallo(solo = "altro")), 7)
})

test_that("il riepilogo ha una riga per giorno, mezzo, servizio e comune del giro", {
  d <- letture_giri()
  giri <- riepilogo_giri(cerca_giri(d, "", "2025-03-03", "2025-03-05"))
  expect_named(giri, colonne_riepilogo_giri())
  expect_equal(nrow(giri), 5)
  # in ordine di giorno, mezzo e prima lettura
  expect_identical(giri$matricola_veicolo, c("M1", "M1", "M2", "M1", "M2"))
  expect_identical(giri$targa_veicolo[1], "AA111AA")
  expect_identical(
    giri$servizio_atteso,
    c("SECCO PAP", "CARTA/CARTONE PAP", "VETRO PAP", "SECCO PAP", NA)
  )
  expect_identical(giri$comune_giro, c("ASIAGO", "GALLIO", "ROANA", "ASIAGO", NA))
  expect_identical(giri$letture, c(3L, 1L, 1L, 1L, 1L))
  # lo stesso bidone letto due volte conta una volta
  expect_identical(giri$bidoni, c(2L, 1L, 1L, 1L, 1L))
  expect_identical(giri$fuori_giro, c(1L, 0L, 0L, 1L, 0L))
  expect_identical(giri$fuori_comune, c(1L, 0L, 0L, 0L, 0L))
  expect_identical(
    format(giri$prima_lettura[1:2], "%H:%M", tz = "UTC"),
    c("06:10", "14:20")
  )
  expect_identical(format(giri$ultima_lettura[1], "%H:%M", tz = "UTC"), "07:05")
  # dove cadono le letture del giro
  expect_identical(
    giri$letto_in,
    c("ASIAGO e un altro comune", "GALLIO", "ROANA", "FOZA", "fuori dai comuni serviti")
  )
  # il riepilogo si calcola anche sulle letture non ancora ordinate
  expect_identical(riepilogo_giri(d[nrow(d):1, ]), giri)

  vuoto <- riepilogo_giri(d[0, ])
  expect_equal(nrow(vuoto), 0)
  expect_named(vuoto, colonne_riepilogo_giri())
  expect_identical(riepilogo_giri(NULL), vuoto)

  # letture di un giro del riepilogo
  trovate <- cerca_giri(d, "", "2025-03-03", "2025-03-05")
  expect_identical(letture_del_giro(trovate, giri[1, ])$RFID, c("R1", "R2", "R1"))
  expect_identical(letture_del_giro(trovate, giri[4, ])$RFID, "R1")
  # un giro senza servizio e senza comune nel calendario
  expect_identical(letture_del_giro(trovate, giri[5, ])$RFID, "R5")
})

test_that("frase, limite e tabella dei giri", {
  d <- cerca_giri(letture_giri(), "", "2025-03-03", "2025-03-05")
  expect_identical(
    descrizione_giri(d),
    "7 letture di 5 bidoni \u00b7 2 fuori giro \u00b7 1 fuori comune"
  )
  expect_identical(descrizione_giri(d[0, ]), "")
  expect_identical(descrizione_giri(NULL), "")

  # oltre il limite restano prima le letture con un'incongruenza
  poche <- limita_letture_giro(d, 4)
  expect_equal(nrow(poche), 4)
  expect_identical(attr(poche, "totale"), 7L)
  expect_true(all(c("R2") %in% poche$RFID))
  expect_equal(sum(poche$fuori_giro | poche$fuori_comune), 3)
  expect_false(is.unsorted(poche$giorno_lettura))
  tutte <- limita_letture_giro(d, 100)
  expect_equal(nrow(tutte), 7)
  expect_null(limita_letture_giro(NULL))

  # l'area da inquadrare comprende i comuni dei giri, se sono pochi
  esempio <- cerca_giri(dati_esempio(), "VEH001", "2025-09-24", "2025-09-24")
  area <- area_giri(esempio)
  expect_named(area, c("latitudine", "longitudine"))
  expect_equal(nrow(area), nrow(esempio) + 2)
  # le letture sono a Borgoricco, il giro era a Limena: l'area arriva fin lì
  limena <- lati_comuni()[[indice_comune("LIMENA")]]
  expect_lte(min(area$longitudine), limena$x_min)
  expect_gt(min(esempio$longitudine), limena$x_max)
  # con molti comuni, o senza comuni noti, valgono le sole letture
  anno <- cerca_giri(dati_esempio(), "", "2025-09-01", "2025-09-30")
  expect_equal(nrow(area_giri(anno)), nrow(anno))
  expect_equal(nrow(area_giri(d)), nrow(d) + 2 * 3)
  senza <- d
  senza$comune_lettura <- NA_character_
  senza$latitudine <- 45.8
  senza$longitudine <- 11.5
  expect_equal(nrow(area_giri(senza)), nrow(senza))
  expect_null(area_giri(d[0, ]))
  expect_null(area_giri(NULL))

  tabella <- tabella_giri_dt(riepilogo_giri(d))
  expect_s3_class(tabella, "datatables")
  dati <- tabella$x$data
  expect_named(
    dati,
    c(
      "Data",
      "Mezzo",
      "Giro previsto",
      "Comune del giro",
      "Orario",
      "Letture",
      "Bidoni",
      "Fuori giro",
      "Fuori comune",
      "Letto a",
      "ordine_giorno"
    )
  )
  expect_identical(dati$Data[1], "03/03/2025")
  expect_identical(dati$Mezzo[1], "M1 AA111AA")
  expect_identical(dati$Orario[1], "06:10\u201307:05")
  # i giri con letture fuori giro stanno in cima; la data si ordina come data
  expect_identical(tabella$x$options$order[[1]], list(7, "desc"))
  nascosta <- Filter(
    function(k) isFALSE(k$visible),
    tabella$x$options$columnDefs
  )[[1]]
  expect_identical(nascosta$targets, 10L)
  expect_s3_class(tabella_giri_dt(riepilogo_giri(d[0, ])), "datatables")
})

test_that("sul dataset di esempio un giro lavora in un comune, e uno \u00e8 sbagliato", {
  d <- dati_esempio()
  giri <- riepilogo_giri(cerca_giri(d, "", "2025-01-01", "2025-12-31"))
  # un mezzo fa uno o due giri al giorno
  per_giorno <- table(paste(giri$giorno, giri$matricola_veicolo))
  expect_lte(max(per_giorno), 2)
  expect_false(anyNA(giri$comune_giro))
  expect_false(anyNA(giri$servizio_atteso))
  expect_equal(sum(giri$letture), sum(lubridate::year(d$giorno_lettura) == 2025))

  # mercoled\u00ec 24 settembre 2025 il mezzo VEH001 doveva lavorare a Limena e
  # ha letto a Borgoricco: tutte le letture sono fuori giro
  sbagliato <- cerca_giri(d, "VEH001", "2025-09-24", "2025-09-24")
  riga <- riepilogo_giri(sbagliato)
  expect_equal(nrow(riga), 1)
  expect_identical(riga$servizio_atteso, "SECCO PAP")
  expect_identical(riga$comune_giro, "LIMENA")
  expect_identical(riga$letture, 10L)
  expect_identical(riga$fuori_giro, 10L)
  expect_identical(riga$fuori_comune, 0L)
  expect_identical(riga$letto_in, "BORGORICCO")
  # \u00e8 il giro con pi\u00f9 letture fuori giro dell'anno
  expect_identical(max(giri$fuori_giro), 10L)
  # la settimana prima lo stesso giro era a posto
  prima <- riepilogo_giri(cerca_giri(d, "VEH001", "2025-09-17", "2025-09-17"))
  expect_identical(prima$comune_giro, "BORGORICCO")
  expect_lte(prima$fuori_giro, 1)
})
