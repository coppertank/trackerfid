test_that("ogni RFID conta al più una volta al giorno, con la sua origine", {
  storiche <- dplyr::tibble(
    giorno_lettura = c(
      "2026-08-03 07:00:00", # A: anche dalle antenne lo stesso giorno
      "2026-08-03 07:05:00", # A di nuovo, stessa trasmissione e giorno
      "2026-08-03 07:10:00", # B: solo saponette, nella trasmissione di A
      "2026-08-04 07:00:00", # C: stessa trasmissione, ma un altro giorno: fuori
      "2026-08-03 08:00:00" # D: trasmissione senza RFID visti dalle antenne
    ),
    id_trasmissione = c("T1", "T1", "T1", "T1", "T2"),
    RFID = c("A", "A", "B", "C", "D")
  )
  antenne <- dplyr::tibble(
    giorno_lettura = c("2026-08-03 09:00:00", "2026-08-03 10:00:00", "2026-08-05 09:00:00"),
    RFID = c("A", "A", "E")
  )
  confronto <- confronta_letture(storiche, antenne)

  expect_named(confronto, c("giorno_lettura", "RFID", "origine"))
  expect_identical(confronto$giorno_lettura, as.Date(c("2026-08-03", "2026-08-03", "2026-08-05")))
  expect_identical(confronto$RFID, c("A", "B", "E"))
  expect_identical(confronto$origine, c("Entrambe", "Saponette", "Antenne"))
})

test_that("le letture si leggono anche dai CSV della pipeline", {
  cartella <- withr::local_tempdir()
  storiche <- file.path(cartella, "letture_storiche.csv")
  antenne <- file.path(cartella, "letture_app.csv")
  writeLines(c("giorno_lettura,giorno,id_trasmissione,RFID", "2026-08-03 07:00:00,2026-08-03,T1,0000000123"), storiche)
  writeLines(c("giorno_lettura,RFID", "2026-08-03 09:00:00,0000000123"), antenne)

  confronto <- confronta_letture(storiche, antenne)
  # gli zeri iniziali del codice restano
  expect_identical(confronto$RFID, "0000000123")
  expect_identical(confronto$origine, "Entrambe")
})
