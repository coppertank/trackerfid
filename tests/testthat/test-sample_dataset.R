# Il dataset di esempio deve rispettare le specifiche del progetto.

test_that("dimensioni, periodo e mezzi", {
  d <- dati_esempio()
  expect_gte(nrow(d), 600)
  expect_gte(dplyr::n_distinct(d$RFID), 200)
  expect_lte(dplyr::n_distinct(d$RFID), 250)
  expect_equal(as.Date(range(d$giorno_lettura)), as.Date(c("2025-09-01", "2025-09-30")))

  ore <- as.numeric(format(d$giorno_lettura, "%H", tz = "UTC")) +
    as.numeric(format(d$giorno_lettura, "%M", tz = "UTC")) / 60
  expect_true(all(ore >= 6 & ore <= 19))

  mezzi <- dplyr::distinct(d, targa_veicolo, matricola_veicolo)
  expect_equal(nrow(mezzi), 5)
  expect_equal(anyDuplicated(mezzi$targa_veicolo), 0)
  expect_equal(anyDuplicated(mezzi$matricola_veicolo), 0)
})

test_that("coerenza tra stato a database, servizio e utenza", {
  d <- dati_esempio()
  non_presente <- d$presente_a_database == "Non Presente"
  expect_true(all(is.na(d$servizio_transponder[non_presente])))
  expect_true(all(is.na(d$id_utenza[non_presente])))
  expect_false(anyNA(d$servizio_transponder[!non_presente]))
  expect_false(anyNA(d$id_utenza[!non_presente]))
  expect_true(all(is.na(d$servizio_atteso[!non_presente])))
  expect_false(anyNA(d$latitudine))
  expect_false(anyNA(d$longitudine))
})

test_that("distribuzioni vicine a quelle richieste", {
  d <- dati_esempio()
  ultimi <- deduplica_ultimo_rfid(d)

  expect_equal(calcola_pct_presente(ultimi), 0.85, tolerance = 0.02)

  quote <- prop.table(table(ultimi$servizio_transponder))
  attese <- c(SECCO = 0.45, CARTA = 0.30, VETRO = 0.12, UMIDO = 0.10, PLASTICA = 0.03)
  expect_equal(as.numeric(quote[names(attese)]), unname(attese), tolerance = 0.1)

  n_letture <- table(d$RFID)
  expect_equal(mean(n_letture <= 2), 0.6, tolerance = 0.05)
  expect_equal(mean(n_letture >= 3 & n_letture <= 5), 0.3, tolerance = 0.05)
  expect_equal(mean(n_letture >= 6 & n_letture <= 15), 0.1, tolerance = 0.05)

  expect_equal(dplyr::n_distinct(d$id_utenza, na.rm = TRUE), 50)
  cambi_utenza <- tapply(d$id_utenza, d$RFID, function(x) dplyr::n_distinct(x, na.rm = TRUE))
  expect_gte(sum(cambi_utenza > 1), 5)
  expect_lte(sum(cambi_utenza > 1), 10)

  attesi <- d$servizio_atteso[d$presente_a_database == "Non Presente"]
  expect_gt(sum(is.na(attesi)), 0)
  expect_gt(sum(!is.na(attesi)), 0)
})

test_that("casi speciali presenti", {
  d <- dplyr::arrange(dati_esempio(), giorno_lettura)
  letture <- function(rfid) d[d$RFID == rfid, ]

  cambio_servizio <- letture("RFD20250901001")
  expect_identical(cambio_servizio$servizio_transponder, c("CARTA", "SECCO", "CARTA"))
  expect_identical(format(cambio_servizio$giorno_lettura, "%d/%m"), c("01/09", "15/09", "28/09"))

  cambio_utenza <- letture("RFD20250901050")
  expect_identical(cambio_utenza$id_utenza[1], "UTZ001")
  expect_identical(format(cambio_utenza$giorno_lettura[1], "%d/%m"), "01/09")
  prima_nuova <- cambio_utenza[cambio_utenza$id_utenza == "UTZ025", ][1, ]
  expect_identical(format(prima_nuova$giorno_lettura, "%d/%m"), "20/09")

  con_stima <- letture("RFD20250915201")
  expect_equal(nrow(con_stima), 4)
  expect_true(all(con_stima$servizio_atteso == "SECCO"))

  senza_stima <- letture("RFD20250920250")
  expect_equal(nrow(senza_stima), 2)
  expect_true(all(is.na(senza_stima$servizio_atteso)))

  spostato <- letture("RFD20250905100")
  expect_equal(spostato$latitudine, c(41.85, 42.00))
  expect_equal(spostato$longitudine, c(12.35, 12.50))
})
