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
      conferimenti_massimi_annui = raccolte,
      litri_massimi_annui = raccolte * 120,
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

quota_testo <- function(x) ifelse(is.na(x), "", sprintf("%.0f%%", 100 * x))

test_that("gli scenari sono l'unione dei sistemi e ciascuno da solo", {
  scenari <- scenari_rilevazione()
  expect_identical(names(scenari), c("Saponette e antenne", "Solo saponette", "Solo antenne"))
  expect_setequal(scenari[[2]], c("Solo saponette", "Entrambe"))
  expect_setequal(scenari[[3]], c("Solo antenne", "Entrambe"))
})

test_that("gli scenari dipendono dai sistemi che hanno letture", {
  # senza argomenti: tutti e due, con il riferimento per primo
  expect_identical(scenari_rilevazione(), scenari_rilevazione(TRUE, TRUE))
  # con le sole saponette c'e' un solo scenario: l'antenna non si simula
  saponette <- scenari_rilevazione(con_antenne = FALSE, con_saponette = TRUE)
  expect_identical(names(saponette), "Rilevazione reale (Saponette)")
  expect_setequal(saponette[[1]], sistemi_lettura())
  antenne <- scenari_rilevazione(con_antenne = TRUE, con_saponette = FALSE)
  expect_identical(names(antenne), "Rilevazione reale (Antenne)")
  expect_length(scenari_rilevazione(FALSE, FALSE), 0)
})

test_that("un sistema ha conferimenti se ha letture associate del secco", {
  unita <- unita_economia_prova()
  expect_equal(sistemi_con_conferimenti(unita), c(antenne = TRUE, saponette = TRUE))
  solo_saponette <- dplyr::filter(unita, sistema == "Solo saponette")
  expect_equal(sistemi_con_conferimenti(solo_saponette), c(antenne = FALSE, saponette = TRUE))
  # letture non associate (PIT): niente conferimenti
  solo_pit <- dplyr::filter(unita, stato == "PIT")
  expect_equal(sistemi_con_conferimenti(solo_pit), c(antenne = FALSE, saponette = FALSE))
  # un'antenna che ha letto solo altri rifiuti non dice niente sul secco
  carta <- dplyr::filter(unita, sistema == "Solo antenne") |>
    dplyr::mutate(servizio_transponder = "CARTA")
  expect_equal(sistemi_con_conferimenti(carta), c(antenne = FALSE, saponette = FALSE))
})

test_that("con un solo sistema l'analisi economica ha un solo scenario, quello reale", {
  anagrafica <- anagrafica_economia_prova()
  unita <- dplyr::mutate(
    unita_economia_prova(),
    sistema = factor("Solo saponette", sistemi_lettura())
  )
  economia <- analisi_economica(unita, anagrafica, con_antenne = FALSE, con_saponette = TRUE)
  expect_identical(levels(economia$scenario), "Rilevazione reale (Saponette)")
  expect_identical(scenario_riferimento(economia), "Rilevazione reale (Saponette)")
  expect_false("Solo antenne" %in% economia$scenario)
  # la stessa fotografia dello scenario di riferimento con tutti e due i sistemi
  completa <- analisi_economica(unita_economia_prova(), anagrafica)
  unione <- completa[completa$scenario == "Saponette e antenne", ]
  expect_setequal(economia$id_fornitura, unione$id_fornitura)
  expect_equal(economia$litri[order(economia$id_fornitura)], unione$litri[order(unione$id_fornitura)])
  # una riga di riepilogo e nessun confronto tra sistemi
  expect_identical(nrow(riepilogo_economico(economia)), 1L)
  expect_identical(nrow(impatto_dei_sistemi(economia)), 0L)
  # con le sole antenne: lo scenario si chiama diversamente
  antenne <- dplyr::mutate(unita_economia_prova(), sistema = factor("Solo antenne", sistemi_lettura()))
  solo_antenne <- analisi_economica(antenne, anagrafica, con_antenne = TRUE, con_saponette = FALSE)
  expect_identical(levels(solo_antenne$scenario), "Rilevazione reale (Antenne)")
  # senza sistemi non c'e' niente da analizzare
  expect_identical(nrow(analisi_economica(unita_economia_prova(), anagrafica, FALSE, FALSE)), 0L)
})

test_that("il riferimento dell'impatto e' il primo scenario, qualunque nome abbia", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  expect_identical(scenario_riferimento(economia), "Saponette e antenne")
  expect_identical(
    as.character(impatto_dei_sistemi(economia)$scenario),
    c("Solo saponette", "Solo antenne")
  )
})

test_that("l'analisi economica annualizza i litri e usa le soglie annue", {
  anagrafica <- anagrafica_economia_prova()
  # F1 e' attiva mezzo anno nel periodo: i suoi litri valgono il doppio in un anno
  anagrafica$fattore[anagrafica$id_fornitura == "F1"] <- 0.5
  economia <- analisi_economica(unita_economia_prova(), anagrafica)
  f1 <- economia[economia$scenario == "Saponette e antenne" & economia$id_fornitura == "F1", ]
  expect_equal(f1$litri_annui, f1$litri / 0.5)
  expect_equal(f1$litri_limite, 720)
  # stessi litri, stessa classe e importo dimezzato scalando le soglie al periodo
  scalata <- analisi_economica(unita_economia_prova(), anagrafica, annualizza = FALSE)
  s1 <- scalata[scalata$scenario == "Saponette e antenne" & scalata$id_fornitura == "F1", ]
  expect_identical(as.character(s1$classe), as.character(f1$classe))
  expect_equal(s1$importo_eur, f1$importo_eur * 0.5)
  # il riepilogo distingue i litri rilevati da quelli annualizzati
  riepilogo <- riepilogo_economico(economia)
  unione <- riepilogo[riepilogo$scenario == "Saponette e antenne", ]
  tutte <- economia[economia$scenario == "Saponette e antenne", ]
  expect_equal(unione$litri_rilevati, sum(tutte$litri))
  expect_equal(unione$litri, sum(tutte$litri_annui))
  expect_gt(unione$litri, unione$litri_rilevati)
})

test_that("le fasce si dividono per scenario solo se gli scenari sono piu' di uno", {
  due <- forniture_per_fascia(
    analisi_economica(unita_economia_prova(), anagrafica_economia_prova()),
    "eccedenza"
  ) |>
    dplyr::mutate(quota = quota_testo(.data$quota))
  larga <- fasce_per_scenario(due)
  expect_true("Saponette e antenne: forniture" %in% names(larga))
  expect_true("Solo antenne: quota" %in% names(larga))
  uno <- forniture_per_fascia(
    analisi_economica(unita_economia_prova(), anagrafica_economia_prova(), con_antenne = FALSE),
    "eccedenza"
  ) |>
    dplyr::mutate(quota = quota_testo(.data$quota))
  semplice <- fasce_per_scenario(uno)
  expect_named(semplice, c("intervallo", "forniture", "quota"))
  expect_identical(nrow(semplice), nlevels(uno$intervallo))
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

test_that("le forniture senza conferimenti sono quelle che potevano conferire almeno una volta", {
  senza <- forniture_senza_conferimenti(unita_economia_prova(), anagrafica_economia_prova())
  # F3 e F7 non hanno nessuna lettura; F4 non era attiva abbastanza (massimo sotto uno); F5 e' un condominio ma e' senza letture
  expect_setequal(senza$id_fornitura, c("F3", "F5", "F7"))
  expect_true(all(senza$conferimenti_massimi >= 1))
  # una lettura non compatibile non basta
  solo_pit <- dplyr::filter(unita_economia_prova(), stato == "PIT")
  expect_true("F1" %in% forniture_senza_conferimenti(solo_pit, anagrafica_economia_prova())$id_fornitura)
})

test_that("le forniture senza letture che restano fuori dall'elenco si contano per motivo", {
  # F4 e' attiva cinque giorni: nel periodo poteva conferire meno di una volta
  senza <- forniture_senza_conferimenti(unita_economia_prova(), anagrafica_economia_prova())
  expect_equal(attr(senza, "escluse"), c(massimo_non_noto = 0, massimo_sotto_uno = 1))
  # se il massimo annuo di F3 manca nei dati, F3 non e' nell'elenco ma si conta
  anagrafica <- anagrafica_economia_prova()
  anagrafica$conferimenti_massimi_annui[anagrafica$id_fornitura == "F3"] <- NA
  senza <- forniture_senza_conferimenti(unita_economia_prova(), anagrafica)
  expect_setequal(senza$id_fornitura, c("F5", "F7"))
  expect_equal(attr(senza, "escluse"), c(massimo_non_noto = 1, massimo_sotto_uno = 1))
  # le forniture lette non sono escluse: non c'e' niente da segnalare
  tutte_lette <- dplyr::filter(anagrafica_economia_prova(), id_fornitura %in% c("F1", "F2"))
  vuota <- forniture_senza_conferimenti(unita_economia_prova(), tutte_lette)
  expect_identical(nrow(vuota), 0L)
  expect_equal(attr(vuota, "escluse"), c(massimo_non_noto = 0, massimo_sotto_uno = 0))
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
  # letture e litri per fornitura e per contenitore (5 letture da 120 litri, 4 forniture, 4 contenitori)
  expect_equal(uno$letture_per_fornitura, 5 / 4)
  expect_equal(uno$litri_per_fornitura, 600 / 4)
  expect_equal(uno$litri_per_contenitore, 600 / 4)
  # conferimenti massimi: 26 per ogni contenitore, ridotti dalla parte di anno di attivita'
  massimi <- 26 * 1 + 26 + 26 * 5 / 365 + 26
  expect_equal(uno$conferimenti_massimi, massimi)
  expect_equal(uno$propensione_sul_massimo, 5 / massimi)
  expect_equal(uno$propensione_volumetrica, (5 * 120) / (120 * massimi))
  totale <- propensione[propensione$gruppo == "Domestiche (totale)", ]
  expect_identical(totale$forniture, 6L)
  expect_identical(totale$letture, 5L + 9L + 3L)
  # un gruppo senza forniture ha propensione mancante
  vuoto <- propensione[propensione$gruppo == "Casa vuota", ]
  expect_identical(vuoto$forniture, 0L)
  expect_true(is.na(vuoto$propensione_numerica))
})

test_that("per ogni gruppo si scelgono le forniture con piu' eccedenza, non il potenziamento", {
  economia <- dplyr::tibble(
    scenario = factor("Saponette e antenne"),
    id_fornitura = c("F1", "F2", "F3", "F4", "F5", "F6", "F7"),
    id_utenza = paste0("U", 1:7),
    gruppo = factor(
      c("1 componente", "1 componente", "2 componenti", "Potenziamento", "3 componenti", "1 componente", "1 componente"),
      gruppi_forniture()
    ),
    componenti_medi = c(1, 1, 2, 3, 3, 1, 1),
    litri = c(900, 1500, 1000, 9000, 500, 1500, 800),
    litri_annui = c(900, 1500, 1000, 9000, 500, 1500, 800),
    litri_limite = c(720, 720, 960, 6240, 1200, 720, 720),
    litri_eccedenti = c(180, 780, 40, 0, 0, 780, 80),
    eccedenza_eur = c(15, 65, 3.33, 0, 0, 65, 6.67)
  )
  piu <- forniture_con_piu_eccedenza(economia)
  # il potenziamento (non paga) e il gruppo senza eccedenza non compaiono
  expect_false("Potenziamento" %in% piu$gruppo)
  expect_false("3 componenti" %in% piu$gruppo)
  # nel gruppo dell'1 componente: F2 e F6 pari (65 euro e 1500 litri: vince l'ordine), poi F1, poi F7
  uno <- piu[piu$gruppo == "1 componente", ]
  expect_identical(uno$id_fornitura, c("F2", "F6", "F1", "F7"))
  expect_identical(uno$posizione, 1:4)
  expect_identical(piu$id_fornitura[piu$gruppo == "2 componenti"], "F3")
  # al piu' n per gruppo; un gruppo con meno eccedenti ne mostra meno
  due <- forniture_con_piu_eccedenza(economia, n = 2)
  expect_identical(due$id_fornitura[due$gruppo == "1 componente"], c("F2", "F6"))
  expect_identical(nrow(due), 3L)
  # senza eccedenze la tabella e' vuota
  expect_identical(nrow(forniture_con_piu_eccedenza(dplyr::mutate(economia, eccedenza_eur = 0))), 0L)
})

test_that("la propensione ha il volume massimo conferibile, le medie e la soglia di fatturazione", {
  anagrafica <- dplyr::filter(anagrafica_economia_prova(), servizio == "SECCO")
  propensione <- propensione_conferimento(unita_economia_prova(), anagrafica, "SECCO")
  uno <- propensione[propensione$gruppo == "1 componente", ]
  # F1, F3, F4, F8: quattro forniture e quattro contenitori da 120 L e 26 conferimenti massimi l'anno
  expect_equal(uno$litri_massimi_annui, 4 * 26 * 120)
  expect_equal(uno$volumetria_media_per_fornitura, 26 * 120)
  expect_equal(uno$volumetria_media_per_contenitore, 26 * 120)
  # soglia di fatturazione della fascia, in litri all'anno
  expect_equal(uno$soglia_fatturazione_annua, 720)
  due <- propensione[propensione$gruppo == "2 componenti", ]
  expect_equal(due$soglia_fatturazione_annua, 960)
  # il potenziamento non ha un limite, e il totale non ha una fascia
  expect_true(is.na(propensione$soglia_fatturazione_annua[propensione$gruppo == "Potenziamento"]))
  expect_true(is.na(propensione$soglia_fatturazione_annua[propensione$gruppo == "Domestiche (totale)"]))
  # le medie dividono per le sole forniture e contenitori con il massimo noto
  anagrafica$conferimenti_massimi_annui[anagrafica$id_fornitura == "F3"] <- NA
  anagrafica$litri_massimi_annui[anagrafica$id_fornitura == "F3"] <- NA
  senza_massimo <- propensione_conferimento(unita_economia_prova(), anagrafica, "SECCO")
  uno <- senza_massimo[senza_massimo$gruppo == "1 componente", ]
  expect_equal(uno$litri_massimi_annui, 3 * 26 * 120)
  expect_equal(uno$volumetria_media_per_fornitura, 26 * 120)
  expect_equal(uno$volumetria_media_per_contenitore, 26 * 120)
})

test_that("le forniture eccedenti si contano per gruppo e per fascia di importo", {
  gruppi <- c(
    "1 componente", "1 componente", "1 componente", "1 componente", "1 componente",
    "2 componenti", "2 componenti", "Potenziamento", "3 componenti"
  )
  economia <- dplyr::tibble(
    scenario = factor("Saponette e antenne"),
    id_fornitura = paste0("F", 1:9),
    gruppo = factor(gruppi, gruppi_forniture()),
    # estremi inclusi a destra: 50 e' nella prima fascia, 50,01 nella seconda, 200 nella terza
    eccedenza_eur = c(50, 50.01, 200, 200.01, 0, 10, 120, 0, 0)
  )
  tabella <- forniture_eccedenti_per_fascia(economia)
  fasce <- attr(tabella, "fasce")
  expect_identical(
    fasce,
    c("fino a 50 \u20ac", "oltre 50 e fino a 100 \u20ac", "oltre 100 e fino a 200 \u20ac", "oltre 200 \u20ac")
  )
  uno <- tabella[tabella$gruppo == "1 componente", ]
  expect_equal(uno$forniture, 5)
  # F5 non ha eccedenza: non e' eccedente
  expect_equal(uno$eccedenti, 4)
  expect_equal(unlist(uno[fasce], use.names = FALSE), c(1, 1, 1, 1))
  due <- tabella[tabella$gruppo == "2 componenti", ]
  expect_equal(unlist(due[fasce], use.names = FALSE), c(1, 0, 1, 0))
  expect_equal(due$eccedenza_totale_eur, 130)
  # il potenziamento e il gruppo senza eccedenze compaiono con zero eccedenti
  expect_equal(tabella$eccedenti[tabella$gruppo == "Potenziamento"], 0)
  expect_equal(tabella$forniture[tabella$gruppo == "3 componenti"], 1)
  # il totale somma i gruppi
  totale <- tabella[tabella$gruppo == "Totale", ]
  expect_equal(totale$forniture, 9)
  expect_equal(totale$eccedenti, 6)
  expect_equal(unlist(totale[fasce], use.names = FALSE), c(2, 1, 2, 1))
  expect_equal(sum(unlist(totale[fasce])), totale$eccedenti)
  # i gruppi non presenti non compaiono, e gli estremi si possono cambiare
  expect_false("4 componenti" %in% tabella$gruppo)
  altri <- forniture_eccedenti_per_fascia(economia, limiti = c(100))
  expect_identical(attr(altri, "fasce"), c("fino a 100 \u20ac", "oltre 100 \u20ac"))
})

test_that("gli estremi delle fasce di importo si leggono dal parametro del report", {
  # un vettore, una lista dello yaml o un testo
  expect_equal(limiti_fasce_da_parametro(c(50, 100, 200)), c(50, 100, 200))
  expect_equal(limiti_fasce_da_parametro(list(50, 100, 200)), c(50, 100, 200))
  expect_equal(limiti_fasce_da_parametro("50, 100, 200"), c(50, 100, 200))
  expect_equal(limiti_fasce_da_parametro("50;100 200"), c(50, 100, 200))
  # ordina, toglie i doppioni e accetta i decimali con il punto
  expect_equal(limiti_fasce_da_parametro(c(200, 50, 50, 12.5)), c(12.5, 50, 200))
  expect_equal(limiti_fasce_da_parametro(100), 100)
  # valori non validi fermano il report con un messaggio
  expect_error(limiti_fasce_da_parametro(NULL), "uno o pi")
  expect_error(limiti_fasce_da_parametro(""), "uno o pi")
  expect_error(limiti_fasce_da_parametro("50, abc"), "limiti_fasce_eccedenza")
  expect_error(limiti_fasce_da_parametro(c(50, -10)), "maggiori di zero")
  expect_error(limiti_fasce_da_parametro(c(50, 0)), "maggiori di zero")
  expect_error(limiti_fasce_da_parametro(c(50, NA)), "uno o pi")
  expect_error(limiti_fasce_da_parametro(Inf), "uno o pi")
})

