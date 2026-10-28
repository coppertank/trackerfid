contenitori_forniture_prova <- function() {
  contenitore <- function(codice, fornitura, categoria, volume = 120, da = "2020-01-01", a = "2099-12-31", raccolte = 26, servizio = "SECCO") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = servizio,
      volume = volume,
      numero_raccolte_annue = raccolte,
      data_attivazione_contenitore = as.Date(da),
      data_cessazione_contenitore = as.Date(a),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = categoria
    )
  }
  dplyr::bind_rows(
    contenitore("A1", "F1", "31"),
    # due contenitori, uno da 40 litri; cessata a metà anno
    contenitore("A2", "F2", "31", a = "2025-06-30"),
    contenitore("A3", "F2", "31", volume = 40, a = "2025-06-30"),
    contenitore("A4", "F3", "34", volume = 240),
    contenitore("A5", "F4", "44", volume = 240),
    contenitore("A6", "F5", "31"),
    contenitore("A7", "F6", "32"),
    # cessato prima del periodo, e di un altro servizio
    contenitore("A8", "F7", "31", a = "2024-12-31"),
    contenitore("A9", "F8", "31", servizio = "CARTA")
  )
}

componenti_forniture_prova <- function() {
  riga <- function(fornitura, da, a, residenti, sottocategoria = "D") {
    dplyr::tibble(
      id_fornitura = fornitura,
      data_inizio_decorrenza = as.POSIXct(da, tz = ""),
      data_fine_decorrenza = as.POSIXct(a, tz = ""),
      nucleo_residenti = residenti,
      sottocategoria = sottocategoria
    )
  }
  dplyr::bind_rows(
    # la fine di una riga è l'inizio della seguente: il giorno non si conta due volte
    riga("F1", "2020-01-01", "2025-07-01", 2L),
    riga("F1", "2025-07-01", NA, 4L),
    riga("F2", "2020-01-01", NA, 1L),
    riga("F3", "2020-01-01", NA, 0L, "D"),
    riga("F4", "2020-01-01", NA, 0L, "ND"),
    riga("F5", "2020-01-01", NA, 3L),
    # senza inizio la riga non vale (come nella pipeline); F6 resta senza componenti
    riga("F6", NA, NA, 5L),
    riga("F7", "2020-01-01", NA, 2L),
    riga("F8", "2020-01-01", NA, 2L)
  )
}

manovre_forniture_prova <- function() {
  dplyr::tibble(
    id_fornitura = c("F5", "F5", "F1", "F2"),
    data_inizio_dichiarazione = as.Date(c("2025-04-01", "2025-06-01", "2025-03-01", "2025-01-01")),
    data_fine_dichiarazione = as.Date(c(NA, "2025-08-31", "2025-02-01", "2025-02-15"))
  )
}

anagrafica_prova <- function(dal = "2025-01-01", al = "2025-12-31") {
  anagrafica_forniture(
    contenitori_forniture_prova(),
    componenti_forniture_prova(),
    manovre_forniture_prova(),
    dal,
    al
  )
}

test_that("il tipo di fornitura ha la precedenza delle domestiche", {
  expect_identical(
    tipo_fornitura(
      c("31", "32", "34", "44", "31", NA, "99"),
      c("D", "ND", "ND", "ND", "ND", "ND", "D")
    ),
    c("domestica", "domestica", "condominio", "non domestica", "domestica", "non domestica", "altra")
  )
  expect_identical(tipo_fornitura(31, "d"), "domestica")
})

test_that("i giorni dell'anno sono 366 solo per un periodo dentro un anno bisestile", {
  expect_identical(giorni_dell_anno("2024-01-01", "2024-06-30"), 366)
  expect_identical(giorni_dell_anno("2025-01-01", "2025-12-31"), 365)
  expect_identical(giorni_dell_anno("2024-07-01", "2025-06-30"), 365)
})

test_that("i giorni coperti da intervalli sovrapposti si contano una volta", {
  coperti <- giorni_coperti(dplyr::tibble(
    id_fornitura = c("a", "a", "a", "b"),
    da = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-03-01")),
    a = as.Date(c("2025-01-10", "2025-01-20", "2025-02-02", "2025-03-01"))
  ))
  # 1-20 gennaio e 1-2 febbraio; un giorno
  expect_identical(coperti$giorni, c(22, 1))
})

test_that("l'anagrafica tiene le forniture attive nel periodo, con i loro contenitori", {
  anagrafica <- anagrafica_prova()
  # F7 e' cessata prima del periodo
  expect_setequal(anagrafica$id_fornitura, c("F1", "F2", "F3", "F4", "F5", "F6", "F8"))
  f2 <- anagrafica[anagrafica$id_fornitura == "F2", ]
  expect_identical(f2$contenitori, 2L)
  expect_identical(f2$volume_totale, 160)
  expect_identical(f2$volume_contenitore, 120)
  expect_identical(f2$servizio, "SECCO")
  # attiva dal 1 gennaio al 30 giugno
  expect_identical(f2$giorni_attivi, 181)
  expect_equal(f2$fattore, 181 / 365)
  expect_identical(f2$raccolte_annue, 26)
  # il periodo riduce l'attività: dal 1 aprile, 275 giorni
  parziale <- anagrafica_prova("2025-04-01", "2025-12-31")
  expect_identical(parziale$giorni_attivi[parziale$id_fornitura == "F1"], 275)
  expect_false("F2" %in% parziale$id_fornitura[parziale$giorni_attivi > 100])
})

test_that("i componenti sono la media pesata sul tempo, senza contare due volte il giorno del cambio", {
  anagrafica <- anagrafica_prova()
  f1 <- anagrafica[anagrafica$id_fornitura == "F1", ]
  # 181 giorni con 2 residenti (fino al 30 giugno) e 184 con 4
  expect_equal(f1$componenti_medi, (181 * 2 + 184 * 4) / 365)
  expect_identical(f1$giorni_con_componenti, 365)
  # la riga senza inizio non vale: nessun componente
  f6 <- anagrafica[anagrafica$id_fornitura == "F6", ]
  expect_true(is.na(f6$componenti_medi))
  expect_identical(as.character(f6$gruppo), "Componenti non noti")
  # in un periodo prima del cambio la media e' quella della riga
  prima <- anagrafica_prova("2025-01-01", "2025-06-30")
  expect_equal(prima$componenti_medi[prima$id_fornitura == "F1"], 2)
})

test_that("il potenziamento vale se una manovra e' in vigore per almeno meta' dei giorni", {
  anagrafica <- anagrafica_prova()
  potenziamento <- stats::setNames(anagrafica$potenziamento, anagrafica$id_fornitura)
  # F5: dal 1 aprile, 275 giorni; la seconda dichiarazione e' dentro la prima
  expect_true(potenziamento[["F5"]])
  # F1: la dichiarazione finisce prima di cominciare (annullata); F2: 46 giorni su 181
  expect_false(potenziamento[["F1"]])
  expect_false(potenziamento[["F2"]])
  expect_identical(as.character(anagrafica$gruppo[anagrafica$id_fornitura == "F5"]), "Potenziamento")
})

test_that("i gruppi dividono domestiche, condomini e non domestiche", {
  anagrafica <- anagrafica_prova()
  gruppo <- stats::setNames(as.character(anagrafica$gruppo), anagrafica$id_fornitura)
  expect_identical(gruppo[["F1"]], "3 componenti")
  expect_identical(gruppo[["F2"]], "1 componente")
  expect_identical(gruppo[["F3"]], "Condomini")
  expect_identical(gruppo[["F4"]], "Non domestiche")
  expect_identical(gruppo[["F5"]], "Potenziamento")
  expect_identical(
    as.character(gruppo_fornitura(rep("domestica", 4), c(0, 0.4, 5.6, 9), FALSE)),
    c("Casa vuota", "Casa vuota", "6 o più", "6 o più")
  )
  expect_identical(levels(anagrafica$gruppo), gruppi_forniture())
})

test_that("i conteggi di forniture e contenitori sommano le domestiche e le altre", {
  conteggi <- conta_forniture(anagrafica_prova())
  forniture <- stats::setNames(conteggi$forniture, conteggi$gruppo)
  contenitori <- stats::setNames(conteggi$contenitori, conteggi$gruppo)
  expect_identical(forniture[["3 componenti"]], 1L)
  expect_identical(forniture[["Condomini"]], 1L)
  expect_identical(contenitori[["1 componente"]], 2L)
  # domestiche: F1, F2, F5, F6, F8; tutte: più il condominio e la non domestica
  expect_identical(forniture[["Domestiche (totale)"]], 5L)
  expect_identical(forniture[["Tutte le forniture"]], 7L)
  expect_identical(forniture[["Tutte le forniture"]], sum(forniture[gruppi_forniture()]))
  expect_equal(conta_non_domestiche(anagrafica_prova())$categoria, "44")
})

test_that("l'anagrafica si legge dai file estratti, con la categoria del contenitore", {
  file <- scrivi_origine_prova(withr::local_tempdir())
  anagrafica <- leggi_anagrafica_forniture(file, "2026-01-01", "2026-12-31")
  expect_identical(anagrafica$id_fornitura, "F1")
  expect_identical(anagrafica$categoria, "31")
  expect_identical(anagrafica$tipo, "domestica")
  # nel 2026 i residenti sono 2 fino al 14 agosto e 3 dopo; la manovra dal 20 agosto
  expect_false(anagrafica$potenziamento)
  expect_equal(anagrafica$componenti_medi, (226 * 2 + 139 * 3) / 365)
})

test_that("la fascia di componenti del report si sceglie con un numero o un nome", {
  expect_null(gruppo_da_filtro(""))
  expect_null(gruppo_da_filtro(NULL))
  expect_null(gruppo_da_filtro(NA))
  expect_identical(gruppo_da_filtro("0"), "Casa vuota")
  expect_identical(gruppo_da_filtro("1"), "1 componente")
  expect_identical(gruppo_da_filtro(3), "3 componenti")
  expect_identical(gruppo_da_filtro("6+"), "6 o pi\u00f9")
  expect_identical(gruppo_da_filtro("6"), "6 o pi\u00f9")
  expect_identical(gruppo_da_filtro("Potenziamento"), "Potenziamento")
  expect_error(gruppo_da_filtro("7"), "non riconosciuto")
  expect_true(all(
    vapply(c("0", 1:5, "6+", "potenziamento"), gruppo_da_filtro, "") %in% gruppi_forniture()
  ))
})
