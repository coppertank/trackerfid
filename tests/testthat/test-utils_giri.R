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
    comune_giro = c(rep("ASIAGO", 3), "GALLIO", "ROANA", "ASIAGO", NA),
    comune_lettura = c("ASIAGO", "ASIAGO", "ROANA", "GALLIO", "ROANA", "FOZA", NA),
    coerenza_comune = c(
      "Coerente",
      "Fuori comune",
      "Fuori giro",
      "Coerente",
      "Coerente",
      "Fuori giro",
      "Coerenza ignota"
    ),
    latitudine = 45.87,
    longitudine = 11.51
  )
}

test_that("l'elenco dei mezzi e i giorni delle letture", {
  d <- selezione_di(letture_giri())
  expect_identical(
    mezzi_presenti(d),
    c("M1 \u00b7 AA111AA" = "M1", "M2 \u00b7 BB222BB" = "M2")
  )
  vuota <- selezione_di(letture_giri()[0, ])
  expect_identical(mezzi_presenti(vuota), character(0))
  expect_identical(mezzi_presenti(NULL), character(0))
  expect_identical(giorni_letture(d), as.Date(c("2025-03-03", "2025-03-05")))
  expect_null(giorni_letture(vuota))
  expect_null(giorni_letture(NULL))
  # sul dataset di esempio: cinque mezzi
  expect_length(mezzi_presenti(seleziona_periodo(archivio_esempio())), 5)
})

test_that("la ricerca trova le letture di un mezzo nei giorni scelti", {
  d <- selezione_di(letture_giri())
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
  selezione <- selezione_di(d)
  giri <- riepilogo_giri(cerca_giri(selezione, "", "2025-03-03", "2025-03-05"))
  expect_named(giri, colonne_riepilogo_giri())
  expect_equal(nrow(giri), 5)
  # in ordine di giorno, mezzo e prima lettura
  expect_identical(giri$matricola_veicolo, c("M1", "M1", "M2", "M1", "M2"))
  expect_identical(giri$targa_veicolo[1], "AA111AA")
  # senza la colonna del servizio del calendario vale il servizio atteso,
  # come nella validazione
  expect_identical(
    giri$servizio_giro,
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
  # in quali comuni sono state fatte le letture del giro
  expect_identical(
    giri$letto_in,
    c("ASIAGO e un altro comune", "GALLIO", "ROANA", "FOZA", "comune sconosciuto")
  )
  # il riepilogo si calcola anche sulle letture non ancora ordinate
  trovate <- cerca_giri(selezione, "", "2025-03-03", "2025-03-05")
  expect_identical(riepilogo_giri(trovate[nrow(trovate):1, ]), giri)

  vuoto <- riepilogo_giri(d[0, ])
  expect_equal(nrow(vuoto), 0)
  expect_named(vuoto, colonne_riepilogo_giri())
  expect_identical(riepilogo_giri(NULL), vuoto)

  # letture di un giro del riepilogo
  expect_identical(letture_del_giro(trovate, giri[1, ])$RFID, c("R1", "R2", "R1"))
  expect_identical(letture_del_giro(trovate, giri[4, ])$RFID, "R1")
  # un giro senza servizio e senza comune nel calendario
  expect_identical(letture_del_giro(trovate, giri[5, ])$RFID, "R5")
})

test_that("frase, limite e tabella dei giri", {
  d <- cerca_giri(selezione_di(letture_giri()), "", "2025-03-03", "2025-03-05")
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
  expect_equal(
    sum(poche$coerenza_comune %in% c("Fuori giro", "Fuori comune")),
    3
  )
  expect_false(is.unsorted(poche$giorno_lettura))
  tutte <- limita_letture_giro(d, 100)
  expect_equal(nrow(tutte), 7)
  expect_null(limita_letture_giro(NULL))

  # l'area da inquadrare comprende i comuni dei giri, se sono pochi
  esempio <- cerca_giri(seleziona_periodo(archivio_esempio()), "VEH001", "2025-09-24", "2025-09-24")
  area <- area_giri(esempio)
  expect_named(area, c("latitudine", "longitudine"))
  expect_equal(nrow(area), nrow(esempio) + 2)
  # le letture sono a Borgoricco, il giro era a Limena: l'area arriva fin lì
  limena <- lati_comuni()[[indice_comune("LIMENA")]]
  expect_lte(min(area$longitudine), limena$x_min)
  expect_gt(min(esempio$longitudine), limena$x_max)
  # con molti comuni, o senza comuni noti, valgono le sole letture
  anno <- cerca_giri(seleziona_periodo(archivio_esempio()), "", "2025-09-01", "2025-09-30")
  expect_equal(nrow(area_giri(anno)), nrow(anno))
  expect_equal(nrow(area_giri(d)), nrow(d) + 2 * 3)
  senza <- d
  senza$comune_giro <- NA_character_
  senza$latitudine <- 45.8
  senza$longitudine <- 11.5
  expect_equal(nrow(area_giri(senza)), nrow(senza))
  expect_null(area_giri(d[0, ]))
  expect_null(area_giri(NULL))

  giri <- riepilogo_giri(d)
  tabella <- tabella_giri(giri)
  expect_s3_class(tabella, "reactable")
  dati <- dati_reactable(tabella)
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
      "Letto a"
    )
  )
  # le righe sono quelle del riepilogo, nello stesso ordine: la riga scelta
  # nel browser è la riga del riepilogo
  expect_equal(nrow(dati), nrow(giri))
  expect_identical(dati$Data, format(giri$giorno, "%Y-%m-%d"))
  expect_identical(dati$Mezzo[1], "M1 AA111AA")
  expect_identical(dati$Orario[1], "06:10\u201307:05")
  attributi <- attributi_reactable(tabella)
  # i giri con letture fuori giro stanno in cima, poi i più recenti
  expect_identical(
    attributi$defaultSorted,
    list(list(id = "Fuori giro", desc = TRUE), list(id = "Data", desc = TRUE))
  )
  # una riga si sceglie con un click; la colonna della scelta non si vede
  expect_identical(attributi$selection, "single")
  expect_identical(attributi$onClick, "select")
  selezione <- Filter(function(k) identical(k$id, ".selection"), attributi$columns)
  expect_false(selezione[[1]]$show)
  # la data si mostra all'italiana
  colonna_data <- Filter(function(k) identical(k$id, "Data"), attributi$columns)
  expect_match(as.character(colonna_data[[1]]$cell), "p[2] + '/' + p[1]", fixed = TRUE)
  # ogni colonna si filtra: la data come si vede, i numeri con un confronto
  expect_true(attributi$filterable)
  colonne <- stats::setNames(attributi$columns, vapply(attributi$columns, function(k) k$id, ""))
  expect_match(as.character(colonne$Data$filterMethod), "p[2] + '/' + p[1]", fixed = TRUE)
  for (numero in c("Letture", "Bidoni", "Fuori giro", "Fuori comune")) {
    expect_identical(as.character(colonne[[numero]]$filterMethod), as.character(filtro_numerico_js()))
  }
  expect_null(colonne$Mezzo$filterMethod)
  expect_s3_class(tabella_giri(riepilogo_giri(d[0, ])), "reactable")
})

test_that("un giro \u00e8 quello del calendario anche se il servizio atteso viene dai censiti", {
  # Il calendario dava la carta; il servizio atteso di un non censito viene
  # dai censiti letti nello stesso giro, ed \u00e8 il secco
  d <- letture_giri()[1:3, ]
  d$giorno <- lubridate::as_date(d$giorno_lettura)
  d$presente_a_database <- c("Presente", "Presente", "Non Presente")
  d$servizio_giro <- "CARTA"
  d$servizio_atteso <- c("CARTA", "CARTA", "SECCO")
  expect_identical(servizio_del_giro(d), rep("CARTA", 3))
  giri <- riepilogo_giri(d)
  # un giro solo, con il servizio previsto dal calendario
  expect_equal(nrow(giri), 1)
  expect_identical(giri$servizio_giro, "CARTA")
  expect_identical(giri$letture, 3L)
  expect_equal(nrow(letture_del_giro(d, giri[1, ])), 3)
  expect_identical(dati_reactable(tabella_giri(giri))[["Giro previsto"]], "CARTA")
  # senza quella colonna il giro \u00e8 quello del servizio atteso
  senza <- d[names(d) != "servizio_giro"]
  expect_identical(servizio_del_giro(senza), c("CARTA", "CARTA", "SECCO"))
  expect_equal(nrow(riepilogo_giri(senza)), 2)
  expect_identical(
    servizio_del_giro(senza[names(senza) != "servizio_atteso"]),
    rep(NA_character_, 3)
  )
})

test_that("sul dataset di esempio un giro lavora in un comune, e uno \u00e8 sbagliato", {
  d <- seleziona_periodo(archivio_esempio())
  giri <- riepilogo_giri(cerca_giri(d, "", "2025-01-01", "2025-12-31"))
  # un mezzo fa uno o due giri al giorno
  per_giorno <- table(paste(giri$giorno, giri$matricola_veicolo))
  expect_lte(max(per_giorno), 2)
  expect_false(anyNA(giri$comune_giro))
  expect_false(anyNA(giri$servizio_giro))
  expect_equal(sum(giri$letture), seleziona_periodo(archivio_esempio(), periodo_anno(2025))$n)

  # mercoled\u00ec 24 settembre 2025 il mezzo VEH001 doveva lavorare a Limena e
  # ha letto a Borgoricco. I censiti sono nel loro comune: fuori giro resta il
  # solo non censito
  sbagliato <- cerca_giri(d, "VEH001", "2025-09-24", "2025-09-24")
  riga <- riepilogo_giri(sbagliato)
  expect_equal(nrow(riga), 1)
  # il giro ha il nome del calendario
  expect_identical(riga$servizio_giro, "SECCO PAP")
  expect_identical(riga$comune_giro, "LIMENA")
  # il comune di lettura non viene dal calendario
  expect_identical(unique(sbagliato$comune_lettura), "BORGORICCO")
  expect_identical(unique(sbagliato$comune_giro), "LIMENA")
  expect_identical(riga$letture, 10L)
  expect_identical(riga$fuori_giro, 1L)
  expect_identical(riga$fuori_comune, 0L)
  expect_identical(riga$letto_in, "BORGORICCO")
  expect_identical(
    sbagliato$RFID[sbagliato$coerenza_comune == "Fuori giro"],
    "RFD20241009227"
  )
  # la settimana prima lo stesso giro era a Borgoricco
  prima <- riepilogo_giri(cerca_giri(d, "VEH001", "2025-09-17", "2025-09-17"))
  expect_identical(prima$comune_giro, "BORGORICCO")
  expect_lte(prima$fuori_giro, 1)
})

test_that("il filtro dei numeri accetta un valore o un confronto", {
  js <- as.character(filtro_numerico_js())
  expect_s3_class(filtro_numerico_js(), "JS_EVAL")
  for (operatore in c("'>'", "'<'", "'>='", "'<='")) {
    expect_match(js, operatore, fixed = TRUE)
  }
  # senza un numero valido il filtro lascia tutte le righe
  expect_match(js, "if (!m) return rows;", fixed = TRUE)
})
