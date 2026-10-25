giorni <- function(...) as.Date(c(...))

test_that("i mesi parziali contano solo i giorni osservati", {
  confronto <- dplyr::tibble(
    data_lettura = giorni("2025-01-30", "2025-01-31", "2025-01-31", "2025-02-01", "2025-02-02"),
    RFID = c("A", "A", "B", "A", "A"),
    origine = c("Saponette", "Saponette", "Saponette", "Saponette", "Antenne")
  )
  mensile <- riepilogo_mensile(confronto, giorni("2025-01-30"), giorni("2025-02-02"))
  saponette_gennaio <- mensile[
    mensile$mese == giorni("2025-01-01") & mensile$origine == "Saponette",
  ]
  expect_identical(saponette_gennaio$n, 3L)
  expect_identical(saponette_gennaio$giorni, 2L)
  expect_equal(saponette_gennaio$per_giorno, 1.5)
  antenne_febbraio <- mensile[mensile$origine == "Antenne", ]
  expect_equal(antenne_febbraio$per_giorno, 0.5)
})

test_that("le saponette dei giri si dividono per rapporto con le antenne", {
  saponette <- dplyr::tibble(
    data_lettura = giorni(rep("2026-08-03", 5)),
    id_trasmissione = c("T1", "T1", "T1", "T1", "T2"),
    RFID = c("A", "B", "C", "D", "A")
  )
  antenne <- dplyr::tibble(RFID = "A")
  antenne_tutte <- dplyr::tibble(RFID = c("A", "B"))

  gruppi <- classifica_rfid_dei_giri_saponette(saponette, antenne, antenne_tutte)
  expect_identical(
    as.character(gruppi$gruppo[match(c("A", "B", "C"), gruppi$RFID)]),
    c(
      "RFID letti dalle antenne",
      "RFID letti dalle antenne, ma fuori dai filtri",
      "RFID mai letti dalle antenne"
    )
  )
  expect_s3_class(gruppi$gruppo, "factor")

  giri <- copertura_antenne_per_giro(saponette, antenne)
  expect_identical(giri$id_trasmissione, c("T1", "T2"))
  expect_identical(giri$rfid, c(4L, 1L))
  expect_identical(giri$rfid_antenne, c(1L, 1L))
  expect_equal(giri$quota, c(0.25, 1))
})

test_that("il confronto con l'anno prima allinea i giorni della settimana", {
  confronto <- dplyr::tibble(
    # 364 giorni prima di 2026-01-05 e 2026-01-06 sono il 2025-01-06 e 07
    data_lettura = giorni(
      "2025-01-06", "2025-01-06", "2025-01-07",
      "2026-01-05", "2026-01-05", "2026-01-05", "2026-01-06", "2026-01-06", "2026-01-06"
    ),
    RFID = c("A", "B", "A", "A", "B", "C", "A", "B", "C"),
    origine = c(rep("Saponette", 3), rep("Entrambe", 6))
  )
  risultato <- anno_su_anno(
    confronto,
    giorni("2026-01-05"),
    giorni("2026-01-11"),
    ripetizioni = 200L
  )
  riepilogo <- risultato$riepilogo
  expect_identical(riepilogo$giorni, 7L)
  expect_identical(riepilogo$dal_base, giorni("2025-01-06"))
  expect_identical(riepilogo$rfid_giorno_base, 3L)
  expect_identical(riepilogo$rfid_giorno_corrente, 6L)
  expect_equal(riepilogo$rapporto, 2)
  expect_lte(riepilogo$rapporto_basso, 2)
  expect_gte(riepilogo$rapporto_alto, 2)
  expect_identical(nrow(risultato$giorni), 7L)
})

test_that("senza letture nell'anno prima il rapporto non c'è", {
  confronto <- dplyr::tibble(
    data_lettura = giorni("2026-01-05"),
    RFID = "A",
    origine = "Antenne"
  )
  riepilogo <- anno_su_anno(
    confronto, giorni("2026-01-05"), giorni("2026-01-06"), ripetizioni = 10L
  )$riepilogo
  expect_true(is.na(riepilogo$rapporto_basso))
})

test_that("la copertura conta i contenitori attivi letti in ogni finestra", {
  colonne <- function(rfid, quando) {
    dplyr::tibble(
      RFID = rfid,
      data_ora_lettura = quando,
      presente_a_database = "Presente",
      data_attivazione_contenitore = "2025-01-01",
      data_cessazione_contenitore = NA_character_
    )
  }
  saponette <- colonne(c("A", "B"), c("2025-01-01 07:00:00", "2025-01-02 07:00:00"))
  antenne <- colonne(c("C", "B"), rep("2025-01-03 08:00:00", 2))
  copertura <- copertura_contenitori(saponette, antenne, finestra = 2L, passo = 1L)
  expect_identical(copertura$data_fine, giorni("2025-01-02", "2025-01-03"))
  expect_identical(copertura$attivi, c(3L, 3L))
  expect_identical(copertura$letti_saponette, c(2L, 1L))
  expect_identical(copertura$letti_antenne, c(0L, 2L))
  expect_identical(copertura$letti_unione, c(2L, 2L))
  expect_identical(copertura$antenne_complete, c(FALSE, FALSE))
})

test_that("un contenitore cessato prima della finestra non conta tra gli attivi", {
  saponette <- dplyr::tibble(
    RFID = c("A", "B"),
    data_ora_lettura = c("2025-01-01 07:00:00", "2025-01-05 07:00:00"),
    presente_a_database = "Presente",
    data_attivazione_contenitore = "2025-01-01",
    data_cessazione_contenitore = c("2025-01-02", NA)
  )
  antenne <- saponette[0, ]
  antenne[1, ] <- list("B", "2025-01-05 08:00:00", "Presente", "2025-01-01", NA)
  copertura <- copertura_contenitori(saponette, antenne, finestra = 2L, passo = 2L)
  # finestra 04-05/01: A è cessato il 2/1
  ultima <- copertura[nrow(copertura), ]
  expect_identical(ultima$attivi, 1L)
})

test_that("i giri in comune abbinano la trasmissione al mezzo con più RFID condivisi", {
  saponette <- dplyr::tibble(
    data_lettura = giorni(c(rep("2026-09-01", 5), rep("2026-09-02", 2))),
    id_trasmissione = c(rep("T1", 4), "T1", "T2", "T2"),
    RFID = c("A", "B", "C", "D", "A", "A", "Z"),
    presente_a_database = "Presente"
  )
  antenne <- dplyr::tibble(
    data_ora_lettura = c(
      rep("2026-09-01 08:00:00", 5), "2026-09-01 09:00:00", rep("2026-09-02 08:00:00", 1)
    ),
    matricola_veicolo = c("M1", "M1", "M1", "M1", "M2", "M2", "M1"),
    RFID = c("A", "B", "C", "E", "A", "F", "A"),
    presente_a_database = "Presente"
  )
  risultato <- giri_con_entrambi_i_sistemi(saponette, antenne, chip_minimi = 3L)
  # T1 condivide A, B, C con M1 e solo A con M2; T2 solo A con M1: non abbinata
  expect_identical(nrow(risultato), 1L)
  expect_identical(risultato$matricola_veicolo, "M1")
  expect_identical(risultato$giri, 1L)
  expect_identical(risultato$chip_saponette, 4L)
  expect_identical(risultato$chip_antenne, 4L)
  expect_identical(risultato$chip_entrambi, 3L)
})

test_that("la produttivita conta giri per giorno e RFID per giro", {
  saponette <- dplyr::tibble(
    data_lettura = giorni(c(rep("2025-01-10", 5), "2025-01-11")),
    id_trasmissione = c("T1", "T1", "T2", "T2", "T2", "T1"),
    RFID = c("A", "B", "A", "B", "C", "A"),
    presente_a_database = "Presente"
  )
  antenne <- dplyr::tibble(
    data_ora_lettura = rep("2025-02-01 08:00:00", 3),
    matricola_veicolo = "M1",
    RFID = c("A", "B", "C"),
    presente_a_database = "Presente"
  )
  produttivita <- produttivita_giri(
    saponette, antenne, giorni("2025-01-10"), giorni("2025-02-02")
  )
  saponette <- produttivita[produttivita$sistema == "Saponette", ]
  expect_identical(saponette$giri, 3L)
  expect_equal(saponette$rfid_per_giro, 2)
  expect_equal(saponette$giri_per_giorno, 3 / 22)
  antenne_febbraio <- produttivita[produttivita$sistema == "Antenne", ]
  expect_identical(antenne_febbraio$giri, 1L)
  expect_equal(antenne_febbraio$rfid_per_giro, 3)
  expect_equal(antenne_febbraio$giri_per_giorno, 1 / 2)
})

test_that("la serie settimanale conta i mezzi attrezzati e le letture per giorno", {
  confronto <- dplyr::tibble(
    data_lettura = giorni(
      "2025-01-06", "2025-01-07",
      "2026-01-05", "2026-01-05", "2026-01-06", "2026-01-06"
    ),
    RFID = c("A", "B", "A", "B", "C", "D"),
    origine = c("Saponette", "Saponette", "Saponette", "Entrambe", "Antenne", "Antenne")
  )
  antenne_tutte <- dplyr::tibble(
    matricola_veicolo = c("M1", "M2", "M2"),
    data_ora_lettura = c("2026-01-05 07:00:00", "2026-01-08 07:00:00", "2026-01-09 07:00:00")
  )
  serie <- serie_settimanale(
    confronto, antenne_tutte, giorni("2026-01-05"), giorni("2026-01-11")
  )
  expect_identical(nrow(serie), 1L)
  expect_identical(serie$settimana, giorni("2026-01-05"))
  expect_identical(serie$giorni, 7L)
  expect_identical(serie$mezzi_attrezzati, 2L)
  expect_equal(serie$per_giorno, 4 / 7)
  expect_equal(serie$per_giorno_saponette, 2 / 7)
  expect_equal(serie$per_giorno_antenne, 3 / 7)
  expect_equal(serie$per_giorno_base, 2 / 7)
  expect_equal(serie$rapporto, 2)
})

test_that("i chip senza saponette si dividono per gruppo", {
  antenne <- dplyr::tibble(
    RFID = c("A", "B", "C", "C"),
    presente_a_database = c("Presente", "Presente", "Non Presente", "Non Presente"),
    data_attivazione_contenitore = c("2024-12-01", "2026-06-01", NA, NA)
  )
  saponette_tutte <- dplyr::tibble(
    data_ora_lettura = c("2025-01-05 07:00:00", "2025-01-06 07:00:00"),
    RFID = c("A", "X")
  )
  risultato <- chip_senza_saponette(antenne, saponette_tutte)
  expect_identical(risultato$chip, c(1L, 1L, 1L))
  expect_identical(risultato$mai_letti, c(0L, 1L, 1L))
  expect_equal(risultato$quota, c(0, 1, 1))
  expect_identical(
    as.character(risultato$gruppo),
    c(
      "Censiti attivi dall'inizio delle saponette",
      "Censiti attivati dopo l'inizio delle saponette",
      "Non censiti"
    )
  )
})

test_that("la qualita riassume la coerenza di servizio e comune", {
  antenne <- dplyr::tibble(
    coerenza_servizio = c("SI", "SI", "NO", NA),
    coerenza_comune = c("Coerente", "Coerente", "Fuori comune", "Fuori giro")
  )
  qualita <- qualita_letture_antenne(antenne)
  servizio <- qualita[qualita$indicatore == "Coerenza del servizio", ]
  expect_equal(servizio$quota[servizio$valore == "SI"], 0.5)
  expect_equal(servizio$quota[servizio$valore == "(vuoto)"], 0.25)
  comune <- qualita[qualita$indicatore == "Coerenza del comune", ]
  expect_equal(sum(comune$quota), 1)
  expect_identical(nrow(qualita_letture_antenne(dplyr::tibble(x = 1))), 0L)
})

test_that("la proiezione a flotta intera scala per i mezzi attrezzati", {
  confronto <- dplyr::tibble(
    data_lettura = giorni(
      "2025-01-06", "2025-01-06", "2025-01-07", "2025-01-07", "2025-01-08", "2025-01-08", "2025-01-09",
      "2026-01-05", "2026-01-05", "2026-01-06", "2026-01-06",
      "2026-01-07", "2026-01-07", "2026-01-07", "2026-01-08", "2026-01-08", "2026-01-08"
    ),
    RFID = c(
      "A", "B", "A", "B", "A", "B", "A",
      "A", "B", "A", "B", "A", "B", "C", "A", "B", "C"
    ),
    origine = c(rep("Saponette", 7), rep("Antenne", 10))
  )
  antenne_tutte <- dplyr::tibble(
    matricola_veicolo = c("M1", "M2"),
    data_ora_lettura = c("2026-01-05 07:00:00", "2026-01-07 07:00:00")
  )
  proiezione <- proiezione_flotta(
    confronto, antenne_tutte, mezzi_totali = 4L,
    dal = giorni("2026-01-05"), al = giorni("2026-01-08"), ripetizioni = 100L
  )
  # antenne 2, 2, 3, 3 con 1, 1, 2, 2 mezzi su 4: 8, 8, 6, 6
  expect_equal(proiezione$per_giorno_antenne, 2.5)
  expect_equal(proiezione$per_giorno_proiettato, 7)
  expect_equal(proiezione$per_giorno_base, 7 / 4)
  expect_equal(proiezione$rapporto, 4)
  expect_equal(proiezione$quota_mezzi, 1.5 / 4)
})

test_that("l'intervallo del rapporto e' ripetibile e contiene il valore", {
  numeratore <- c(10, 12, 9, 11, 13, 10, 12)
  denominatore <- c(5, 6, 5, 5, 7, 5, 6)
  primo <- intervallo_rapporto(numeratore, denominatore, 300L, 7L)
  secondo <- intervallo_rapporto(numeratore, denominatore, 300L, 7L)
  expect_identical(primo, secondo)
  expect_lt(primo[1], sum(numeratore) / sum(denominatore))
  expect_gt(primo[2], sum(numeratore) / sum(denominatore))
})

test_that("le saponette degli stessi chip tolgono gli RFID mai letti dalle antenne", {
  saponette <- dplyr::tibble(
    data_ora_lettura = c("2025-03-01 07:00:00", "2025-03-01 07:05:00"),
    RFID = c("A", "B")
  )
  risultato <- saponette_degli_stessi_rfid(saponette, dplyr::tibble(RFID = "A"))
  expect_identical(risultato$RFID, "A")
  expect_identical(risultato$data_lettura, giorni("2025-03-01"))
})

test_that("il filtro del servizio funziona anche senza servizio atteso", {
  saponette <- dplyr::tibble(
    RFID = c("A", "B"),
    servizio_transponder = c("SECCO", "CARTA")
  )
  expect_identical(filtra_letture(saponette, servizio = "SECCO")$RFID, "A")
})
