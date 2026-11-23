# Contenitori di prova per il periodo 2025: un bidone per riga, salvo C7 (due
# associazioni attive, la piu' recente e' di un altro comune).
contenitori_chip_prova <- function() {
  riga <- function(codice, servizio, comune, da, a = "2099-12-31", fornitura = "F1") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = servizio,
      comune_fornitura = comune,
      data_attivazione_contenitore = as.Date(da),
      data_cessazione_contenitore = as.Date(a),
      id_fornitura = fornitura,
      stato_fornitura = "ATTIVA"
    )
  }
  dplyr::bind_rows(
    riga("C1", "SECCO", "LIMENA", "2010-01-01"),
    riga("C2", "SECCO", "LIMENA", "2020-01-01"),
    riga("C3", "CARTA", "ROANA", "2018-01-01"),
    riga("C4", "CARTA", "ROANA", "2012-01-01"),
    # cessato prima del periodo e attivato dopo
    riga("C5", "SECCO", "ROANA", "2010-01-01", "2024-12-31"),
    riga("C6", "SECCO", "ROANA", "2026-01-01"),
    # due associazioni attive: vale la piu' recente
    riga("C7", "SECCO", "LIMENA", "2011-01-01", fornitura = "F7vecchia"),
    riga("C7", "VETRO", "ASIAGO", "2019-01-01", fornitura = "F7nuova"),
    # codici che non sono bidoni
    riga("0000000000", "SECCO", "LIMENA", "2020-01-01"),
    riga(NA, "SECCO", "LIMENA", "2020-01-01"),
    # senza servizio e comune
    riga("C8", NA, NA, "2020-01-01"),
    riga("C9", "SECCO", "LIMENA", "2020-01-01")
  )
}

tag_chip_prova <- function() {
  dplyr::tibble(
    codice_transponder = c("C1", "C2", "C9"),
    id_tipo_supporto_chip = c(2L, 3L, 4L),
    descrizione_tipo_supporto_chip = c(
      "BINTAG LF 125", "FASCETTA INTEGRATA LF + UHF", "FASCETTA METALLICA LF + UHF"
    )
  )
}

unita_chip_prova <- function() {
  lettura <- function(rfid, sistema, stato = "PIA") {
    dplyr::tibble(
      RFID = rfid,
      data_lettura = as.Date("2025-03-01"),
      sistema = factor(sistema, sistemi_lettura()),
      stato = factor(stato, stati_lettura())
    )
  }
  dplyr::bind_rows(
    lettura("C1", "Solo saponette"),
    lettura("C2", "Entrambe"),
    # letto non associato: il chip si legge
    lettura("C3", "Solo antenne", "PIT"),
    # una lettura scartata non conta
    lettura("C4", "Solo saponette", "Scartata"),
    lettura("C9", "Solo saponette"),
    # un codice che non e' un bidone dell'associazione
    lettura("ALTRO", "Entrambe")
  )
}

test_that("i bidoni attivi nel periodo hanno servizio, comune e tipo di chip", {
  bidoni <- bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "2025-01-01", "2025-12-31")
  # fuori C5 (cessato), C6 (attivato dopo), lo zero e il mancante; C7 una volta sola
  expect_identical(bidoni$codice_transponder, c("C1", "C2", "C3", "C4", "C7", "C8", "C9"))
  riga <- function(codice) bidoni[bidoni$codice_transponder == codice, ]
  # dai tag
  expect_identical(riga("C1")$descrizione_tipo_supporto_chip, "BINTAG LF 125")
  expect_identical(as.character(riga("C1")$categoria), "Chip solo LF")
  expect_identical(as.character(riga("C2")$categoria), "Chip con UHF")
  # non nei tag: attivato dal 2016 POSSIBILE UHF, 2015 o prima nessun tipo
  expect_identical(riga("C3")$descrizione_tipo_supporto_chip, "POSSIBILE UHF")
  expect_identical(as.character(riga("C3")$categoria), "Possibile UHF")
  expect_true(is.na(riga("C4")$descrizione_tipo_supporto_chip))
  expect_identical(as.character(riga("C4")$categoria), "Tipo non determinabile")
  # tipo_chip e' leggibile e ordinato: i tipi della tabella, POSSIBILE UHF, (nessun tipo)
  expect_s3_class(bidoni$tipo_chip, "factor")
  expect_identical(as.character(riga("C4")$tipo_chip), "(nessun tipo)")
  expect_identical(as.character(riga("C1")$tipo_chip), "BINTAG LF 125")
  expect_identical(levels(bidoni$tipo_chip)[c(1, 10, 11)], c("SCONOSCIUTO", "POSSIBILE UHF", "(nessun tipo)"))
  # C7: l'associazione piu' recente (VETRO, ASIAGO, 2019: possibile UHF)
  expect_identical(riga("C7")$servizio, "VETRO")
  expect_identical(riga("C7")$comune, "ASIAGO")
  expect_identical(riga("C7")$id_fornitura, "F7nuova")
  expect_identical(riga("C7")$descrizione_tipo_supporto_chip, "POSSIBILE UHF")
  # il cantiere viene dal comune; un comune mancante non ha cantiere
  expect_identical(riga("C3")$cantiere, cantiere_di_comune("ROANA"))
  expect_identical(riga("C8")$comune, "(senza comune)")
  expect_identical(riga("C8")$servizio, "(senza servizio)")
  expect_identical(riga("C8")$cantiere, "(fuori tabella)")
  # il periodo sceglie i bidoni: nel 2024 c'e' C5 (cessato il 31/12/2024) e non C6
  nel_2024 <- bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "2024-01-01", "2024-12-31")
  expect_true("C5" %in% nel_2024$codice_transponder)
  expect_false("C6" %in% nel_2024$codice_transponder)
  # senza bidoni attivi: tabella vuota con le stesse colonne
  vuoto <- bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "1990-01-01", "1990-12-31")
  expect_identical(nrow(vuoto), 0L)
  expect_identical(names(vuoto), names(bidoni))
})

test_that("un bidone e' letto se il suo codice compare in una lettura compatibile, da uno o da tutti e due i sistemi", {
  bidoni <- bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "2025-01-01", "2025-12-31") |>
    bidoni_letti(unita_chip_prova())
  stati <- stats::setNames(as.character(bidoni$lettura), bidoni$codice_transponder)
  expect_identical(
    stati[c("C1", "C2", "C3", "C4", "C7", "C8", "C9")],
    c(C1 = "Solo saponette", C2 = "Entrambi", C3 = "Solo antenne", C4 = "Nessuno",
      C7 = "Nessuno", C8 = "Nessuno", C9 = "Solo saponette")
  )
  expect_identical(levels(bidoni$lettura), stati_di_lettura_bidone())
  # le due colonne logiche tornano con lo stato
  expect_identical(bidoni$letto_saponette, bidoni$lettura %in% c("Entrambi", "Solo saponette"))
  expect_identical(bidoni$letto_antenne, bidoni$lettura %in% c("Entrambi", "Solo antenne"))
  # nessuna lettura: nessun bidone letto
  nessuno <- bidoni_letti(
    bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "2025-01-01", "2025-12-31"),
    unita_chip_prova()[0, ]
  )
  expect_true(all(nessuno$lettura == "Nessuno"))
})

test_that("la copertura conta bidoni letti e non letti per scenario, con le chiavi indicate", {
  bidoni <- bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "2025-01-01", "2025-12-31") |>
    bidoni_letti(unita_chip_prova())
  per_categoria <- copertura_bidoni(bidoni, "categoria")
  # tre categorie su quattro hanno bidoni: solo LF (C1), con UHF (C2, C9), possibile
  # UHF (C3, C7, C8), non determinabile (C4)
  expect_identical(as.character(per_categoria$categoria), categorie_chip())
  expect_identical(per_categoria$bidoni, c(1L, 2L, 3L, 1L))
  expect_identical(per_categoria$letti, c(1L, 2L, 1L, 0L))
  expect_identical(per_categoria$non_letti, c(0L, 0L, 2L, 1L))
  # per scenario
  expect_identical(per_categoria$letti_saponette, c(1L, 2L, 0L, 0L))
  expect_identical(per_categoria$letti_antenne, c(0L, 1L, 1L, 0L))
  expect_identical(per_categoria$non_letti_saponette, per_categoria$bidoni - per_categoria$letti_saponette)
  expect_identical(per_categoria$non_letti_antenne, per_categoria$bidoni - per_categoria$letti_antenne)
  # gli stati si sommano ai bidoni, e entrambi + solo = letti per sistema
  expect_identical(
    per_categoria$entrambi + per_categoria$solo_saponette + per_categoria$solo_antenne + per_categoria$non_letti,
    per_categoria$bidoni
  )
  expect_identical(per_categoria$entrambi + per_categoria$solo_saponette, per_categoria$letti_saponette)
  expect_identical(per_categoria$entrambi + per_categoria$solo_antenne, per_categoria$letti_antenne)
  # il totale per comune e servizio: nessun bidone si perde
  per_comune <- copertura_bidoni(bidoni, c("comune", "servizio"))
  expect_identical(sum(per_comune$bidoni), nrow(bidoni))
  expect_identical(
    per_comune$bidoni[per_comune$comune == "LIMENA" & per_comune$servizio == "SECCO"],
    3L
  )
  # in ordine delle chiavi
  expect_identical(per_comune$comune, sort(per_comune$comune))
})

test_that("i bidoni con UHF non letti dalle antenne si elencano, prima quelli letti dalle saponette", {
  bidoni <- bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "2025-01-01", "2025-12-31") |>
    bidoni_letti(unita_chip_prova())
  elenco <- bidoni_uhf_non_letti_dalle_antenne(bidoni)
  # C2 e C3 li hanno letti le antenne; C1 e' solo LF; C4 e' non determinabile.
  # Restano C9 (UHF, letto dalle saponette), poi C8 e C7 (possibile UHF, mai letti) per
  # comune: "(senza comune)" viene prima di ASIAGO
  expect_identical(elenco$codice_transponder, c("C9", "C8", "C7"))
  expect_identical(as.character(elenco$lettura), c("Solo saponette", "Nessuno", "Nessuno"))
  expect_identical(
    names(elenco),
    c("codice_transponder", "comune", "cantiere", "servizio", "id_fornitura",
      "tipo_chip", "categoria", "lettura")
  )
  expect_identical(as.character(elenco$tipo_chip[1]), "FASCETTA METALLICA LF + UHF")
  # se le antenne leggono tutto non resta nessun caso
  tutte <- unita_chip_prova()
  tutte$sistema <- factor("Entrambe", sistemi_lettura())
  tutti_letti <- bidoni_con_chip(contenitori_chip_prova(), tag_chip_prova(), "2025-01-01", "2025-12-31") |>
    bidoni_letti(dplyr::bind_rows(tutte, dplyr::mutate(tutte, RFID = c("C7", "C8", "C3", "C4", "C9", "C1"))))
  expect_identical(nrow(bidoni_uhf_non_letti_dalle_antenne(tutti_letti)), 0L)
})
