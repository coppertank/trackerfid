test_that("ogni RFID conta al più una volta al giorno, con la sua origine", {
  saponette <- dplyr::tibble(
    data_ora_lettura = c(
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
    data_ora_lettura = c("2026-08-03 09:00:00", "2026-08-03 10:00:00", "2026-08-05 09:00:00"),
    RFID = c("A", "A", "E")
  )
  confronto <- confronta_letture(saponette, antenne)

  expect_named(confronto, c("data_lettura", "RFID", "origine"))
  expect_identical(confronto$data_lettura, as.Date(c("2026-08-03", "2026-08-03", "2026-08-05")))
  expect_identical(confronto$RFID, c("A", "B", "E"))
  expect_identical(confronto$origine, c("Entrambe", "Saponette", "Antenne"))
})

test_that("le letture si leggono anche dai CSV della pipeline", {
  cartella <- withr::local_tempdir()
  saponette <- file.path(cartella, "letture_saponette.csv")
  antenne <- file.path(cartella, "letture_antenne.csv")
  writeLines(c("data_ora_lettura,data_lettura,id_trasmissione,RFID", "2026-08-03 07:00:00,2026-08-03,T1,0000000123"), saponette)
  writeLines(c("data_ora_lettura,RFID", "2026-08-03 09:00:00,0000000123"), antenne)

  confronto <- confronta_letture(saponette, antenne)
  # gli zeri iniziali del codice restano
  expect_identical(confronto$RFID, "0000000123")
  expect_identical(confronto$origine, "Entrambe")
})

antenne_filtri <- function() {
  dplyr::tibble(
    RFID = c("A", "B", "C", "D", "E"),
    servizio_atteso = c("SECCO", "CARTA", "CARTA", NA, "SECCO"),
    servizio_transponder = c("SECCO", "SECCO", "CARTA", NA, "CARTA"),
    categoria = c("31", "34", "34", NA, "44"),
    sottocategoria = c("D", "D", "ND", NA, "ND")
  )
}

test_that("filtra_letture senza parametri non filtra", {
  antenne <- antenne_filtri()
  expect_identical(filtra_letture(antenne), antenne)
  expect_identical(
    filtra_letture(antenne, servizio = "", categorie_escluse = NA, sottocategoria = character(0)),
    antenne
  )
})

test_that("il servizio vale per il servizio atteso o per quello del transponder", {
  antenne <- antenne_filtri()
  # B ha il secco solo nel transponder, E solo come servizio atteso
  expect_identical(
    filtra_letture(antenne, servizio = "SECCO")$RFID,
    c("A", "B", "E")
  )
  expect_identical(
    filtra_letture(antenne, servizio = "secco")$RFID,
    c("A", "B", "E")
  )
  expect_identical(
    filtra_letture(antenne, servizio = c("SECCO", "CARTA"))$RFID,
    c("A", "B", "C", "E")
  )
})

test_that("le categorie escluse tolgono quelle categorie e tengono i non censiti", {
  antenne <- antenne_filtri()
  expect_identical(
    filtra_letture(antenne, categorie_escluse = "34")$RFID,
    c("A", "D", "E")
  )
  # anche un numero
  expect_identical(
    filtra_letture(antenne, categorie_escluse = c(34, 44))$RFID,
    c("A", "D")
  )
})

test_that("la sottocategoria tiene solo quella, senza i non censiti", {
  expect_identical(
    filtra_letture(antenne_filtri(), sottocategoria = "D")$RFID,
    c("A", "B")
  )
})

test_that("i filtri si combinano e una colonna mancante dà un errore chiaro", {
  expect_identical(
    filtra_letture(
      antenne_filtri(),
      servizio = "SECCO",
      categorie_escluse = "34",
      sottocategoria = "D"
    )$RFID,
    "A"
  )
  expect_error(
    filtra_letture(dplyr::tibble(RFID = "A"), servizio = "SECCO"),
    "servizio_atteso, servizio_transponder"
  )
  expect_error(
    filtra_letture(dplyr::tibble(RFID = "A"), sottocategoria = "D"),
    "sottocategoria"
  )
})

test_that("le saponette di confronto sono quelle dei giri degli RFID filtrati", {
  saponette <- dplyr::tibble(
    data_ora_lettura = c(
      "2026-08-03 07:00:00", # A, giro T1 del 3/8
      "2026-08-03 07:10:00", # B, stesso giro
      "2026-08-04 07:00:00", # C, T1 ma un altro giorno
      "2026-08-03 08:00:00", # D, giro T2
      "2026-08-05 08:00:00" # A, giro T3 del 5/8
    ),
    id_trasmissione = c("T1", "T1", "T1", "T2", "T3"),
    RFID = c("A", "B", "C", "D", "A")
  )
  antenne <- dplyr::tibble(RFID = "A")
  risultato <- saponette_di_confronto(saponette, antenne)
  expect_identical(risultato$RFID, c("A", "B", "A"))
  expect_identical(
    risultato$data_lettura,
    as.Date(c("2026-08-03", "2026-08-03", "2026-08-05"))
  )
  expect_identical(nrow(saponette_di_confronto(saponette, dplyr::tibble(RFID = "Z"))), 0L)
})

test_that("il confronto usa le letture delle antenne già filtrate", {
  saponette <- dplyr::tibble(
    data_ora_lettura = c(
      "2026-08-03 07:00:00", # A
      "2026-08-03 07:10:00", # B, stesso giro di A
      "2026-08-03 08:00:00" # C, giro T2
    ),
    id_trasmissione = c("T1", "T1", "T2"),
    RFID = c("A", "B", "C")
  )
  antenne <- dplyr::tibble(
    data_ora_lettura = rep("2026-08-03 09:00:00", 3),
    RFID = c("A", "C", "E"),
    servizio_atteso = c("SECCO", "CARTA", "CARTA"),
    servizio_transponder = c("SECCO", "CARTA", "CARTA")
  )
  tutte <- confronta_letture(saponette, antenne)
  expect_identical(tutte$RFID, c("A", "B", "C", "E"))
  expect_identical(tutte$origine, c("Entrambe", "Saponette", "Entrambe", "Antenne"))

  # Solo il secco: resta il giro T1, quindi A e B; C ed E escono
  secco <- confronta_letture(saponette, antenne, servizio = "SECCO")
  expect_identical(secco$RFID, c("A", "B"))
  expect_identical(secco$origine, c("Entrambe", "Saponette"))
})
