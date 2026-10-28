anagrafica_economia_prova <- function() {
  fornitura <- function(id, gruppo, componenti, tipo = "domestica", servizio = "SECCO", potenziamento = FALSE, fattore = 1, raccolte = 26) {
    dplyr::tibble(
      id_fornitura = id,
      servizio = servizio,
      tipo = tipo,
      gruppo = factor(gruppo, gruppi_forniture()),
      contenitori = 1L,
      volume_contenitore = 120,
      volume_totale = 120,
      raccolte_annue = raccolte,
      fattore = fattore,
      componenti_medi = componenti,
      potenziamento = potenziamento
    )
  }
  dplyr::bind_rows(
    fornitura("F1", "1 componente", 1),
    fornitura("F2", "2 componenti", 2),
    # mai letta
    fornitura("F3", "1 componente", 1),
    # attiva cinque giorni: non ci si aspettava nemmeno una raccolta
    fornitura("F4", "1 componente", 1, fattore = 5 / 365),
    fornitura("F5", "Condomini", NA, tipo = "condominio"),
    fornitura("F6", "Potenziamento", 3, potenziamento = TRUE),
    # una domestica di un altro servizio, mai letta
    fornitura("F7", "1 componente", 1, servizio = "CARTA"),
    # letta solo dalle saponette
    fornitura("F8", "1 componente", 1)
  )
}

unita_economia_prova <- function() {
  unita <- function(fornitura, n, sistema, giorno0 = 0) {
    dplyr::tibble(
      RFID = paste0(fornitura, "_", seq_len(n)),
      data_lettura = as.Date("2025-03-01") + giorno0 + seq_len(n),
      sistema = factor(sistema, sistemi_lettura()),
      stato = factor("PIA", stati_lettura()),
      id_fornitura = fornitura,
      volume = 120,
      servizio_transponder = "SECCO"
    )
  }
  dplyr::bind_rows(
    # F1: 3 letture, 2 in comune e una solo dell'antenna
    unita("F1", 2, "Entrambe"),
    unita("F1", 1, "Solo antenne", giorno0 = 10),
    # F2: 9 letture, 7 in comune e 2 solo dell'antenna
    unita("F2", 7, "Entrambe"),
    unita("F2", 2, "Solo antenne", giorno0 = 10),
    unita("F6", 3, "Entrambe"),
    unita("F8", 2, "Solo saponette"),
    # non compatibili: non sono conferimenti
    dplyr::mutate(unita("F1", 5, "Entrambe", 50), stato = factor("PIT", stati_lettura()))
  )
}

test_that("gli scenari sono l'unione dei sistemi e ciascuno da solo", {
  scenari <- scenari_rilevazione()
  expect_identical(names(scenari), c("Saponette e antenne", "Solo saponette", "Solo antenne"))
  expect_setequal(scenari[[2]], c("Solo saponette", "Entrambe"))
  expect_setequal(scenari[[3]], c("Solo antenne", "Entrambe"))
})

test_that("i conferimenti sono le letture compatibili e i litri il volume dei contenitori", {
  anagrafica <- anagrafica_economia_prova()
  unione <- conferimenti_per_fornitura(
    unita_economia_prova(),
    anagrafica,
    scenari_rilevazione()[[1]],
    "SECCO"
  )
  conferimenti <- stats::setNames(unione$conferimenti, unione$id_fornitura)
  expect_identical(conferimenti[["F1"]], 3L)
  expect_identical(conferimenti[["F2"]], 9L)
  expect_equal(unione$litri[unione$id_fornitura == "F2"], 9 * 120)
  # solo saponette: non vedono le letture della sola antenna
  saponette <- conferimenti_per_fornitura(
    unita_economia_prova(),
    anagrafica,
    scenari_rilevazione()[[2]],
    "SECCO"
  )
  expect_identical(saponette$conferimenti[saponette$id_fornitura == "F1"], 2L)
  # con un altro servizio non c'e' niente
  expect_identical(
    nrow(conferimenti_per_fornitura(unita_economia_prova(), anagrafica, scenari_rilevazione()[[1]], "CARTA")),
    0L
  )
  # se il volume della lettura manca vale quello del contenitore della fornitura
  senza_volume <- dplyr::mutate(unita_economia_prova(), volume = NA_real_)
  expect_equal(
    conferimenti_per_fornitura(senza_volume, anagrafica, scenari_rilevazione()[[1]], "SECCO")$litri[1],
    3 * 120
  )
})

test_that("le forniture senza conferimenti sono quelle da cui ci si aspettava almeno una raccolta", {
  senza <- forniture_senza_conferimenti(unita_economia_prova(), anagrafica_economia_prova())
  # F3 e F7 non hanno nessuna lettura; F4 non era attiva abbastanza; F5 e' un condominio ma e' senza letture
  expect_setequal(senza$id_fornitura, c("F3", "F5", "F7"))
  expect_true(all(senza$raccolte_attese >= 1))
  # una lettura non compatibile non basta
  solo_pit <- dplyr::filter(unita_economia_prova(), stato == "PIT")
  expect_true("F1" %in% forniture_senza_conferimenti(solo_pit, anagrafica_economia_prova())$id_fornitura)
})

test_that("l'analisi economica classifica le forniture domestiche del secco in ogni scenario", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  expect_identical(
    levels(economia$scenario),
    c("Saponette e antenne", "Solo saponette", "Solo antenne")
  )
  unione <- economia[economia$scenario == "Saponette e antenne", ]
  # restano le domestiche del secco lette: F1, F2, F6, F8; non F3, F4 (mai lette), F5, F7
  expect_setequal(unione$id_fornitura, c("F1", "F2", "F6", "F8"))
  riga <- function(scenario, fornitura) {
    economia[economia$scenario == scenario & economia$id_fornitura == fornitura, ]
  }
  # F1: 360 litri con un componente: premio di 10 euro
  expect_identical(as.character(riga("Saponette e antenne", "F1")$classe), "Premialità")
  expect_equal(riga("Saponette e antenne", "F1")$premio_eur, 10)
  # F2: 1080 litri con 2 componenti (960 inclusi): 120 litri eccedenti, 10 euro
  expect_identical(as.character(riga("Saponette e antenne", "F2")$classe), "Eccedente")
  expect_equal(riga("Saponette e antenne", "F2")$eccedenza_eur, 10)
  # F6 con potenziamento: 360 litri, ordinaria, senza premio
  expect_identical(as.character(riga("Saponette e antenne", "F6")$classe), "Ordinaria")
  expect_equal(riga("Saponette e antenne", "F6")$premio_eur, 0)
  # con le sole saponette F1 ha 240 litri (premio massimo) e F2 840 (ordinaria)
  expect_equal(riga("Solo saponette", "F1")$premio_eur, 15)
  expect_identical(as.character(riga("Solo saponette", "F2")$classe), "Ordinaria")
  # F8 non e' letta dalle antenne: litri zero, e senza dati il premio massimo
  f8 <- riga("Solo antenne", "F8")
  expect_false(f8$rilevata)
  expect_equal(f8$litri, 0)
  expect_equal(f8$premio_eur, 15)
  expect_true(riga("Solo saponette", "F8")$rilevata)
})

test_that("il riepilogo conta classi, litri ed euro per scenario e per gruppo", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  riepilogo <- riepilogo_economico(economia)
  unione <- riepilogo[riepilogo$scenario == "Saponette e antenne", ]
  expect_identical(unione$forniture, 4L)
  # F1 e F8 in premialita' (240 litri per F8), F2 eccedente, F6 ordinaria
  expect_identical(c(unione$premialita, unione$ordinarie, unione$eccedenti), c(2L, 1L, 1L))
  expect_equal(unione$quota_premialita, 0.5)
  expect_equal(unione$premi_eur, 10 + 15)
  expect_equal(unione$eccedenze_eur, 10)
  expect_equal(unione$importo_eur, 10 - 25)
  expect_equal(unione$litri, 3 * 120 + 9 * 120 + 3 * 120 + 2 * 120)
  expect_equal(unione$litri_eccedenti, 120)
  expect_equal(unione$litri_in_premialita, 3 * 120 + 2 * 120)
  # il massimo per 1 componente e' 480: F1 e' 120 litri sotto, F8 a 240 e' 240 sotto
  expect_equal(unione$litri_sotto_massimo, 120 + 240)

  per_gruppo <- riepilogo_economico(economia, per_gruppo = TRUE)
  expect_true("gruppo" %in% names(per_gruppo))
  uno <- per_gruppo[per_gruppo$scenario == "Saponette e antenne" & per_gruppo$gruppo == "1 componente", ]
  expect_identical(uno$forniture, 2L)
})

test_that("le forniture si dividono per fascia di eccedenza e per quota di premio", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  eccedenza <- forniture_per_fascia(economia, "eccedenza")
  unione <- eccedenza[eccedenza$scenario == "Saponette e antenne", ]
  # F2 ha 120 litri eccedenti: la prima fascia
  expect_identical(unione$forniture[unione$intervallo == "fino a 120 lt"], 1L)
  expect_identical(sum(unione$forniture), 1L)
  premio <- forniture_per_fascia(economia, "premio")
  unione <- premio[premio$scenario == "Saponette e antenne", ]
  # 10 euro (F1) e 15 euro (F8)
  expect_identical(unione$forniture[unione$intervallo == "da 5 a 10 €"], 1L)
  expect_identical(unione$forniture[unione$intervallo == "da 10 a 15 €"], 1L)
  expect_equal(sum(unione$quota), 2 / 4)
  # limiti diversi
  stretta <- forniture_per_fascia(economia, "eccedenza", limiti = c(60, 100))
  expect_identical(levels(stretta$intervallo), c("fino a 60 lt", "da 60 a 100 lt", "oltre 100 lt"))
})

test_that("l'impatto dei sistemi dice che cosa cambierebbe con uno solo", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  impatto <- impatto_dei_sistemi(economia)
  expect_identical(
    as.character(impatto$scenario),
    c("Solo saponette", "Solo antenne")
  )
  saponette <- impatto[impatto$scenario == "Solo saponette", ]
  antenne <- impatto[impatto$scenario == "Solo antenne", ]
  expect_identical(saponette$forniture, 4L)
  # saponette: F1 resta in premialita' (ma 15 euro invece di 10), F2 diventa ordinaria
  expect_identical(saponette$classe_diversa, 1L)
  expect_identical(saponette$stessa_classe, 3L)
  expect_identical(saponette$rilevate, 4L)
  expect_equal(saponette$importo_riferimento_eur, 10 - 10 - 15)
  expect_equal(saponette$importo_scenario_eur, -15 - 0 - 15)
  expect_equal(saponette$differenza_eur, -15)
  # antenne: non vedono F8, che resta in premialita' con litri zero (15 euro)
  expect_identical(antenne$rilevate, 3L)
  expect_identical(antenne$classe_diversa, 0L)
  expect_equal(antenne$differenza_eur, 0)
})

test_that("la propensione divide letture e litri per contenitori e capacita'", {
  anagrafica <- dplyr::filter(anagrafica_economia_prova(), servizio == "SECCO")
  propensione <- propensione_conferimento(unita_economia_prova(), anagrafica, "SECCO")
  uno <- propensione[propensione$gruppo == "1 componente", ]
  # F1, F3, F4, F8: quattro contenitori e 3 + 2 letture compatibili
  expect_identical(uno$forniture, 4L)
  expect_identical(uno$contenitori, 4L)
  expect_identical(uno$letture, 5L)
  expect_equal(uno$propensione_numerica, 5 / 4)
  attese <- 26 * 1 + 26 + 26 * 5 / 365 + 26
  expect_equal(uno$raccolte_attese, attese)
  expect_equal(uno$propensione_sulle_attese, 5 / attese)
  expect_equal(uno$propensione_volumetrica, (5 * 120) / (120 * attese))
  totale <- propensione[propensione$gruppo == "Domestiche (totale)", ]
  expect_identical(totale$forniture, 6L)
  expect_identical(totale$letture, 5L + 9L + 3L)
  # un gruppo senza forniture ha propensione mancante
  vuoto <- propensione[propensione$gruppo == "Casa vuota", ]
  expect_identical(vuoto$forniture, 0L)
  expect_true(is.na(vuoto$propensione_numerica))
})
