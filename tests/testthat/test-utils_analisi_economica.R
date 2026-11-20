anagrafica_economia_prova <- function() {
  fornitura <- function(id, gruppo, componenti, tipo = "domestica", servizio = "SECCO", potenziamento = FALSE, fattore = 1, raccolte = 26, quota = 1) {
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
      quota_attivita = quota,
      giorni_attivi = round(quota * 365),
      id_utenza = paste0("U", id),
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
    fornitura("F4", "1 componente", 1, fattore = 5 / 365, quota = 5 / 365),
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

test_that("le forniture senza letture sono quelle dell'anagrafica senza nessuna lettura associata", {
  senza <- forniture_senza_letture(unita_economia_prova(), anagrafica_economia_prova())
  # F3, F4 e F7 non hanno nessuna lettura (anche F4, attiva solo cinque giorni: non c'e'
  # nessun controllo sui conferimenti massimi); F5 e' un condominio ma e' senza letture
  expect_setequal(senza$id_fornitura, c("F3", "F4", "F5", "F7"))
  # le colonne sono quelle dell'anagrafica, senza altri calcoli
  expect_identical(names(senza), names(anagrafica_economia_prova()))
  # una lettura non compatibile non basta
  solo_pit <- dplyr::filter(unita_economia_prova(), stato == "PIT")
  expect_true("F1" %in% forniture_senza_letture(solo_pit, anagrafica_economia_prova())$id_fornitura)
  # un massimo annuo mancante non cambia niente: la fornitura senza letture c'e' lo stesso
  anagrafica <- anagrafica_economia_prova()
  anagrafica$conferimenti_massimi_annui[anagrafica$id_fornitura == "F3"] <- NA
  expect_true("F3" %in% forniture_senza_letture(unita_economia_prova(), anagrafica)$id_fornitura)
  # le forniture lette non compaiono
  tutte_lette <- dplyr::filter(anagrafica_economia_prova(), id_fornitura %in% c("F1", "F2"))
  expect_identical(nrow(forniture_senza_letture(unita_economia_prova(), tutte_lette)), 0L)
})

# Contenitori, composizione e letture per provare l'elenco delle forniture senza
# letture, nel periodo 1 gennaio - 31 dicembre 2026.
dati_senza_letture <- function() {
  contenitore <- function(codice, fornitura, da = "2020-01-01", a = "2099-12-31", stato = "ATTIVA", raccolte = 26, servizio = "SECCO", gratuito = "N", categoria = "31") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = servizio,
      volume = 120,
      numero_raccolte_annue = raccolte,
      contenitore_gratuito = gratuito,
      data_attivazione_contenitore = as.Date(da),
      data_cessazione_contenitore = as.Date(a),
      id_fornitura = fornitura,
      stato_fornitura = stato,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = categoria
    )
  }
  contenitori <- dplyr::bind_rows(
    # letta
    contenitore("M0", "F_letta"),
    # mai letta
    contenitore("M1", "F_mai"),
    # attivata l'ultimo giorno del periodo
    contenitore("M2", "F_corta", da = "2026-12-31"),
    # cessata a database, ma il contenitore non ha una data di cessazione
    contenitore("M3", "F_cessata", stato = "CESSATA"),
    # il bidone e' stato letto il 10 febbraio, prima della sua attivazione
    contenitore("M4", "F_fuori_data", da = "2026-06-01"),
    # lo stesso bidone passa da una fornitura all'altra il 31 marzo
    contenitore("M5", "F_vecchia", a = "2026-03-31"),
    contenitore("M5", "F_nuova", da = "2026-03-31"),
    # senza il massimo annuo di conferimenti
    contenitore("M6", "F_senza_massimo", raccolte = NA_real_),
    # un'altra fornitura, della carta: non e' del secco
    contenitore("M7", "F_carta", servizio = "CARTA"),
    # solo contenitori gratuiti: non si fattura, quindi non e' senza letture
    contenitore("M8", "F_gratuita", gratuito = "S"),
    # un bidone a pagamento mai letto e uno gratuito letto
    contenitore("M9", "F_mista"),
    contenitore("M10", "F_mista", gratuito = "S"),
    # un bidone del secco mai letto e uno della carta letto
    contenitore("M11", "F_altro_servizio"),
    contenitore("M12", "F_altro_servizio", servizio = "CARTA"),
    # un condominio mai letto: non e' nell'analisi economica
    contenitore("M13", "F_condominio", categoria = "34")
  )
  componenti <- dplyr::tibble(
    id_fornitura = unique(contenitori$id_fornitura),
    data_inizio_decorrenza = as.POSIXct("2020-01-01", tz = ""),
    data_fine_decorrenza = as.POSIXct(NA),
    nucleo_residenti = 2L,
    sottocategoria = "D"
  )
  manovre <- dplyr::tibble(
    id_fornitura = character(0),
    data_inizio_dichiarazione = as.Date(character(0)),
    data_fine_dichiarazione = as.Date(character(0))
  )
  lettura <- function(codice, giorno, stato, fornitura = NA_character_, servizio = "SECCO", gratuito = "N") {
    dplyr::tibble(
      RFID = codice,
      data_lettura = as.Date(giorno),
      sistema = factor("Solo saponette", sistemi_lettura()),
      stato = factor(stato, stati_lettura()),
      id_fornitura = fornitura,
      volume = 120,
      servizio_transponder = servizio,
      contenitore_gratuito = gratuito
    )
  }
  unita <- dplyr::bind_rows(
    lettura("M0", "2026-02-10", "PIA", "F_letta"),
    lettura("M4", "2026-02-10", "NP"),
    # una lettura di M5 il 31 marzo: associata alla fornitura vecchia
    lettura("M5", "2026-03-31", "PIA", "F_vecchia"),
    # una lettura scartata non conta
    lettura("M1", "2026-02-11", "Scartata"),
    # la lettura di un contenitore gratuito non e' un conferimento
    lettura("M10", "2026-02-12", "PIA", "F_mista", gratuito = "S"),
    # nemmeno quella di un bidone di un altro servizio
    lettura("M12", "2026-02-13", "PIA", "F_altro_servizio", servizio = "CARTA")
  )
  list(
    contenitori = contenitori,
    anagrafica = anagrafica_forniture(
      contenitori, componenti, manovre, "2026-01-01", "2026-12-31", "SECCO"
    ),
    unita = unita
  )
}

test_that("le forniture senza letture comprendono quelle attive da pochi giorni e senza il massimo annuo", {
  dati <- dati_senza_letture()
  tutte <- forniture_senza_letture(dati$unita, dati$anagrafica)
  expect_setequal(
    tutte$id_fornitura,
    c(
      "F_mai", "F_corta", "F_cessata", "F_fuori_data", "F_nuova", "F_senza_massimo",
      "F_mista", "F_altro_servizio", "F_condominio"
    )
  )
  # i soli contenitori gratuiti non si fatturano: la fornitura non c'e'
  expect_false("F_gratuita" %in% tutte$id_fornitura)
  # una lettura di un contenitore gratuito o di un altro servizio non e' un conferimento
  expect_true(all(c("F_mista", "F_altro_servizio") %in% tutte$id_fornitura))
  # una fornitura attiva un solo giorno e una senza massimo annuo ci sono come le altre
  expect_identical(tutte$giorni_attivi[tutte$id_fornitura == "F_corta"], 1)
  expect_true(is.na(tutte$conferimenti_massimi_annui[tutte$id_fornitura == "F_senza_massimo"]))
  # il secco e' un servizio: la carta non c'e'
  expect_false("F_carta" %in% tutte$id_fornitura)
})

test_that("le forniture senza letture della sezione sono quelle senza letture dell'analisi economica", {
  dati <- dati_senza_letture()
  senza <- forniture_senza_letture(dati$unita, dati$anagrafica)
  economia <- analisi_economica(dati$unita, dati$anagrafica)
  riferimento <- economia[economia$scenario == scenario_riferimento(economia), ]
  popolazione <- forniture_dell_economia(dati$anagrafica)
  # stessa popolazione e stesse forniture senza letture: i due numeri coincidono
  expect_setequal(riferimento$id_fornitura, popolazione$id_fornitura)
  expect_setequal(
    senza$id_fornitura[senza$id_fornitura %in% popolazione$id_fornitura],
    riferimento$id_fornitura[riferimento$senza_letture]
  )
  expect_identical(
    sum(senza$id_fornitura %in% popolazione$id_fornitura),
    sum(riferimento$senza_letture)
  )
  # la sezione ha in piu' le forniture che l'economia non analizza (il condominio)
  expect_true("F_condominio" %in% senza$id_fornitura)
  expect_false("F_condominio" %in% riferimento$id_fornitura)
  expect_gt(nrow(senza), sum(riferimento$senza_letture))
  # i contenitori gratuiti sono fuori da entrambe
  expect_false("F_gratuita" %in% c(senza$id_fornitura, riferimento$id_fornitura))
})

test_that("l'elenco delle forniture senza letture ha i dati per capire perche' sono senza letture", {
  dati <- dati_senza_letture()
  senza <- forniture_senza_letture(dati$unita, dati$anagrafica)
  elenco <- elenco_forniture_senza_letture(
    senza, dati$contenitori, dati$unita, "2026-01-01", "2026-12-31"
  )
  expect_identical(nrow(elenco), nrow(senza))
  expect_identical(anyDuplicated(elenco$id_fornitura), 0L)
  expect_true(all(c(
    "id_fornitura", "id_utenza", "gruppo", "bidoni", "stato_fornitura", "giorni_attivi",
    "letture_bidoni", "letture_non_associate",
    "prima_lettura_non_associata", "letture_altra_fornitura",
    "letture_altri_servizi", "letture_contenitori_gratuiti"
  ) %in% names(elenco)))
  # niente conferimenti massimi e niente esclusioni
  expect_false(any(c("conferimenti_massimi", "contata_nel_report", "motivo_esclusione") %in% names(elenco)))
  riga <- function(fornitura) elenco[elenco$id_fornitura == fornitura, ]

  # mai letta: nessuna lettura dei suoi bidoni, e la scartata non conta
  mai <- riga("F_mai")
  expect_identical(mai$letture_bidoni, 0L)
  expect_identical(mai$letture_non_associate, 0L)
  expect_identical(mai$letture_altra_fornitura, 0L)
  expect_true(is.na(mai$prima_lettura_non_associata))
  expect_identical(mai$bidoni, "M1 (120 L)")

  # attivata l'ultimo giorno: un giorno di attivita'
  expect_identical(riga("F_corta")$giorni_attivi, 1)
  expect_identical(riga("F_corta")$data_attivazione_bidoni, as.Date("2026-12-31"))

  # cessata a database con il contenitore ancora aperto
  expect_identical(riga("F_cessata")$stato_fornitura, "CESSATA")
  expect_identical(riga("F_cessata")$data_cessazione_bidoni, as.Date("2099-12-31"))

  # il bidone e' stato letto, ma la lettura non e' associata (fuori dalle date)
  fuori <- riga("F_fuori_data")
  expect_identical(fuori$letture_bidoni, 1L)
  expect_identical(fuori$letture_non_associate, 1L)
  expect_identical(fuori$prima_lettura_non_associata, as.Date("2026-02-10"))
  expect_identical(fuori$ultima_lettura_non_associata, as.Date("2026-02-10"))
  expect_identical(fuori$letture_altra_fornitura, 0L)
  expect_lt(fuori$data_attivazione_bidoni, fuori$prima_lettura_non_associata + 200)

  # il bidone e' stato letto e associato all'altra fornitura
  nuova <- riga("F_nuova")
  expect_identical(nuova$letture_bidoni, 1L)
  expect_identical(nuova$letture_altra_fornitura, 1L)
  expect_identical(nuova$letture_non_associate, 0L)

  # una fornitura senza massimo annuo c'e' lo stesso
  expect_identical(riga("F_senza_massimo")$letture_bidoni, 0L)

  # le letture associate che non sono conferimenti del secco a pagamento
  expect_identical(riga("F_mista")$letture_contenitori_gratuiti, 1L)
  expect_identical(riga("F_mista")$letture_altri_servizi, 0L)
  expect_identical(riga("F_altro_servizio")$letture_altri_servizi, 1L)
  expect_identical(riga("F_altro_servizio")$letture_contenitori_gratuiti, 0L)
  expect_identical(mai$letture_altri_servizi, 0L)
  expect_identical(mai$letture_contenitori_gratuiti, 0L)

  # file vecchi senza lo stato della fornitura: la colonna resta vuota
  vecchi <- dplyr::select(dati$contenitori, -"stato_fornitura")
  senza_stato <- elenco_forniture_senza_letture(senza, vecchi, dati$unita, "2026-01-01", "2026-12-31")
  expect_true(all(senza_stato$stato_fornitura == ""))

  # niente forniture senza letture: un elenco senza righe, con le stesse colonne
  vuoto <- elenco_forniture_senza_letture(senza[0, ], dati$contenitori, dati$unita, "2026-01-01", "2026-12-31")
  expect_identical(nrow(vuoto), 0L)
  expect_identical(names(vuoto), names(elenco))
})

test_that("l'analisi economica classifica le forniture domestiche del secco in ogni scenario", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  expect_identical(
    levels(economia$scenario),
    c("Saponette e antenne", "Solo saponette", "Solo antenne")
  )
  unione <- economia[economia$scenario == "Saponette e antenne", ]
  # tutte le domestiche del secco attive: anche F3 e F4, mai lette; non F5 (condominio) ne' F7 (carta)
  expect_setequal(unione$id_fornitura, c("F1", "F2", "F3", "F4", "F6", "F8"))
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
  # F8 non e' letta dalle antenne: nello scenario delle antenne ha zero litri e il premio massimo
  f8 <- riga("Solo antenne", "F8")
  expect_identical(nrow(f8), 1L)
  expect_true(f8$senza_letture)
  expect_equal(f8$litri, 0)
  expect_equal(f8$premio_eur, 15)
  expect_false(riga("Solo saponette", "F8")$senza_letture)
})

test_that("ogni scenario ha le stesse forniture e le non lette prendono il premio massimo proporzionale", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  per_scenario <- split(economia$id_fornitura, economia$scenario)
  tutte <- c("F1", "F2", "F3", "F4", "F6", "F8")
  expect_setequal(per_scenario[["Saponette e antenne"]], tutte)
  expect_setequal(per_scenario[["Solo saponette"]], tutte)
  expect_setequal(per_scenario[["Solo antenne"]], tutte)
  expect_false("rilevata" %in% names(economia))
  senza <- function(scenario) {
    sort(economia$id_fornitura[economia$scenario == scenario & economia$senza_letture])
  }
  # F3 e F4 non sono mai state lette; F8 solo dalle saponette
  expect_identical(senza("Saponette e antenne"), c("F3", "F4"))
  expect_identical(senza("Solo saponette"), c("F3", "F4"))
  expect_identical(senza("Solo antenne"), c("F3", "F4", "F8"))
  # chi non e' letto ha zero litri e conferimenti, e il premio massimo della fascia
  non_lette <- economia[economia$senza_letture, ]
  expect_true(all(non_lette$litri == 0 & non_lette$conferimenti == 0))
  expect_true(all(as.character(non_lette$classe) == "Premialità"))
  expect_true(all(non_lette$premio_pieno_eur == 15))
  # il premio e' proporzionale ai giorni di attivita': F3 attiva tutto il periodo, F4 cinque giorni su 365
  f3 <- economia[economia$scenario == "Saponette e antenne" & economia$id_fornitura == "F3", ]
  f4 <- economia[economia$scenario == "Saponette e antenne" & economia$id_fornitura == "F4", ]
  expect_equal(f3$premio_eur, 15)
  expect_equal(f4$premio_eur, 15 * 5 / 365)
  expect_equal(f4$premio_pieno_eur, 15)
  expect_equal(f4$quota_attivita, 5 / 365)
  # con i giri indicati (senza annualizzazione) il premio non si proporziona
  senza_quota <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova(), annualizza = FALSE)
  expect_true(all(senza_quota$quota_attivita == 1))
})

test_that("il riepilogo conta classi, litri ed euro per scenario e per gruppo", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  riepilogo <- riepilogo_economico(economia)
  unione <- riepilogo[riepilogo$scenario == "Saponette e antenne", ]
  expect_identical(unione$forniture, 6L)
  expect_identical(unione$senza_letture, 2L)
  # F1, F3, F4 e F8 in premialita', F2 eccedente, F6 ordinaria
  expect_identical(c(unione$premialita, unione$ordinarie, unione$eccedenti), c(4L, 1L, 1L))
  expect_equal(unione$quota_premialita, 4 / 6)
  # premialita' dovuta: F1 10 + F8 15 + F3 15 + F4 15 x 5/365
  expect_equal(unione$premi_con_letture_eur, 10 + 15)
  expect_equal(unione$premi_senza_letture_eur, 15 + 15 * 5 / 365)
  expect_equal(unione$premi_eur, 10 + 15 + 15 + 15 * 5 / 365)
  expect_equal(unione$eccedenze_eur, 10)
  expect_equal(unione$importo_eur, 10 - unione$premi_eur)
  expect_equal(unione$litri, 3 * 120 + 9 * 120 + 3 * 120 + 2 * 120)
  expect_equal(unione$litri_eccedenti, 120)
  expect_equal(unione$litri_in_premialita, 3 * 120 + 2 * 120)
  # il massimo per 1 componente e' 480: F1 e' 120 litri sotto, F8 a 240 e' 240 sotto, F3 e F4 (zero litri, minimo 240) 240 sotto
  expect_equal(unione$litri_sotto_massimo, 120 + 240 + 240 + 240)

  per_gruppo <- riepilogo_economico(economia, per_gruppo = TRUE)
  expect_true("gruppo" %in% names(per_gruppo))
  uno <- per_gruppo[per_gruppo$scenario == "Saponette e antenne" & per_gruppo$gruppo == "1 componente", ]
  expect_identical(uno$forniture, 4L)
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
  # F4 (15 euro per cinque giorni su 365), F1 (10 euro), F3 e F8 (15 euro)
  expect_identical(unione$forniture[unione$intervallo == "fino a 5 €"], 1L)
  expect_identical(unione$forniture[unione$intervallo == "da 5 a 10 €"], 1L)
  expect_identical(unione$forniture[unione$intervallo == "da 10 a 15 €"], 2L)
  expect_equal(sum(unione$quota), 4 / 6)
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
  # le forniture sono le stesse del riferimento; le saponette ne leggono quattro, le antenne tre
  expect_identical(saponette$forniture, 6L)
  expect_identical(saponette$rilevate, 4L)
  expect_identical(saponette$senza_letture, 2L)
  expect_identical(antenne$forniture, 6L)
  expect_identical(antenne$rilevate, 3L)
  expect_identical(antenne$senza_letture, 3L)
  # saponette: F1 resta in premialita' (ma 15 euro invece di 10), F2 diventa ordinaria
  expect_identical(saponette$classe_diversa, 1L)
  expect_identical(saponette$stessa_classe, 5L)
  expect_equal(saponette$differenza_eur, -15)
  expect_equal(saponette$importo_scenario_eur - saponette$importo_riferimento_eur, -15)
  # antenne: F8 non e' letta, ma il premio massimo (15 euro) e' lo stesso del riferimento
  expect_identical(antenne$classe_diversa, 0L)
  expect_identical(antenne$premi_in_piu, 0L)
  expect_equal(antenne$differenza_eur, 0)
})

# Contenitori delle forniture di prova: F2 ha due bidoni, F3 e' cessata a database, F8 ha un bidone gratuito.
contenitori_economia_prova <- function() {
  contenitore <- function(codice, fornitura, volume = 120, stato = "ATTIVA", gratuito = "N") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = "SECCO",
      volume = volume,
      contenitore_gratuito = gratuito,
      data_attivazione_contenitore = as.Date("2020-01-01"),
      data_cessazione_contenitore = as.Date("2099-12-31"),
      id_fornitura = fornitura,
      stato_fornitura = stato
    )
  }
  dplyr::bind_rows(
    contenitore("A1", "F1"),
    contenitore("A2", "F2"),
    contenitore("A2B", "F2", volume = 240),
    contenitore("A3", "F3", stato = "CESSATA"),
    contenitore("A4", "F4"),
    contenitore("A6", "F6"),
    contenitore("A8", "F8"),
    contenitore("A8G", "F8", volume = 60, gratuito = "S"),
    # un bidone di un'altra fornitura e di un altro servizio non c'entra
    dplyr::mutate(contenitore("A7", "F7"), servizio_transponder = "CARTA")
  )
}

test_that("l'elenco della premialita' ha le forniture con diritto, con i bidoni e le stime a fine anno", {
  economia <- analisi_economica(unita_economia_prova(), anagrafica_economia_prova())
  elenco <- elenco_premialita(
    economia, "Solo antenne", contenitori_economia_prova(), "2025-01-01", "2025-12-31"
  )
  # i campi richiesti, nell'ordine; poi le colonne di servizio
  expect_named(
    elenco,
    c(
      "id_fornitura", "componenti_medi", "fascia_componenti", "bidoni_attivi", "bidoni",
      "conferimenti_effettuati", "conferimenti_mancanti_attesi", "conferimenti_attesi_fine_anno",
      "volumetria_effettuata_l", "volumetria_mancante_attesa_l", "volumetria_attesa_fine_anno_l",
      "volumetria_minima_premialita_l", "volumetria_massima_premialita_l", "premialita_attesa_eur",
      "id_utenza", "stato_fornitura", "giorni_attivi", "quota_attivita", "senza_letture"
    )
  )
  # solo le forniture in premialita' nello scenario: F1 (360 litri con le antenne: 240 e' il minimo), F3, F4, F8
  # non le eccedenti (F2) ne' chi non ha premio (F6, potenziamento)
  expect_identical(elenco$id_fornitura, c("F1", "F3", "F4", "F8"))
  expect_identical(anyDuplicated(elenco$id_fornitura), 0L)
  riga <- function(fornitura) elenco[elenco$id_fornitura == fornitura, ]

  # F1: letta dalle antenne 3 volte (360 litri); un bidone attivo
  expect_identical(riga("F1")$componenti_medi, 1)
  expect_identical(riga("F1")$fascia_componenti, 1L)
  expect_identical(riga("F1")$bidoni_attivi, 1L)
  expect_identical(riga("F1")$bidoni, "A1 (120 L)")
  expect_identical(riga("F1")$conferimenti_effettuati, 3L)
  expect_equal(riga("F1")$volumetria_effettuata_l, 360)
  # il periodo e' un anno intero (fattore 1): niente di mancante
  expect_equal(riga("F1")$conferimenti_mancanti_attesi, 0)
  expect_equal(riga("F1")$conferimenti_attesi_fine_anno, 3)
  expect_equal(riga("F1")$volumetria_mancante_attesa_l, 0)
  expect_equal(riga("F1")$volumetria_attesa_fine_anno_l, 360)
  # soglie della fascia 1: minimo 240, massimo 480
  expect_equal(riga("F1")$volumetria_minima_premialita_l, 240)
  expect_equal(riga("F1")$volumetria_massima_premialita_l, 480)
  expect_equal(riga("F1")$premialita_attesa_eur, 10)
  expect_identical(riga("F1")$senza_letture, "NO")

  # F8 non e' letta dalle antenne: zero conferimenti, premio massimo; un bidone a pagamento e uno gratuito
  expect_identical(riga("F8")$senza_letture, "SI")
  expect_identical(riga("F8")$conferimenti_effettuati, 0L)
  expect_equal(riga("F8")$volumetria_effettuata_l, 0)
  expect_identical(riga("F8")$bidoni_attivi, 2L)
  expect_identical(riga("F8")$bidoni, "A8 (120 L), A8G (60 L, gratuito)")
  expect_equal(riga("F8")$premialita_attesa_eur, 15)

  # F3 e' cessata a database: nessun bidone attivo, ma la fornitura c'e' (era attiva nel periodo)
  expect_identical(riga("F3")$bidoni_attivi, 0L)
  expect_identical(riga("F3")$bidoni, "")
  expect_identical(riga("F3")$stato_fornitura, "CESSATA")
  expect_equal(riga("F3")$premialita_attesa_eur, 15)
  # F4: attiva cinque giorni su 365
  expect_equal(riga("F4")$premialita_attesa_eur, round(15 * 5 / 365, 2))
  expect_equal(riga("F4")$quota_attivita, round(5 / 365, 4))
  expect_equal(riga("F4")$giorni_attivi, 5)

  # nello scenario con entrambi i sistemi F1 ha 5 letture di cui 2 delle saponette: scenari a se' stanti
  saponette <- elenco_premialita(economia, "Solo saponette", contenitori_economia_prova(), "2025-01-01", "2025-12-31")
  expect_identical(saponette$conferimenti_effettuati[saponette$id_fornitura == "F1"], 2L)
  expect_equal(saponette$volumetria_effettuata_l[saponette$id_fornitura == "F1"], 240)
  expect_equal(saponette$premialita_attesa_eur[saponette$id_fornitura == "F1"], 15)
  # senza forniture in premialita': un elenco vuoto con le stesse colonne
  vuoto <- elenco_premialita(economia[0, ], "Solo antenne", contenitori_economia_prova(), "2025-01-01", "2025-12-31")
  expect_identical(nrow(vuoto), 0L)
  expect_identical(names(vuoto), names(elenco))
})

test_that("le stime a fine anno riportano il periodo a un anno: mancante e totale atteso", {
  # F2 e' eccedente (9 letture, 1080 litri) e il periodo e' meta' anno: i valori attesi sono il doppio
  anagrafica <- anagrafica_economia_prova()
  anagrafica$fattore[anagrafica$id_fornitura == "F2"] <- 0.5
  economia <- analisi_economica(unita_economia_prova(), anagrafica)
  elenco <- elenco_eccedenza(
    economia, "Saponette e antenne", contenitori_economia_prova(), "2025-01-01", "2025-06-30"
  )
  expect_named(
    elenco,
    c(
      "id_fornitura", "componenti_medi", "fascia_componenti", "bidoni_attivi", "bidoni",
      "conferimenti_effettuati", "conferimenti_mancanti_attesi", "conferimenti_attesi_fine_anno",
      "volumetria_effettuata_l", "volumetria_mancante_attesa_l", "volumetria_attesa_fine_anno_l",
      "volumetria_inclusa_l", "volumetria_eccedente_attesa_l", "eccedenza_attesa_eur",
      "id_utenza", "stato_fornitura", "giorni_attivi", "quota_attivita", "senza_letture"
    )
  )
  expect_identical(elenco$id_fornitura, "F2")
  # due bidoni attivi (120 e 240 litri) con i loro codici
  expect_identical(elenco$bidoni_attivi, 2L)
  expect_identical(elenco$bidoni, "A2 (120 L), A2B (240 L)")
  expect_identical(elenco$conferimenti_effettuati, 9L)
  expect_equal(elenco$conferimenti_attesi_fine_anno, 18)
  expect_equal(elenco$conferimenti_mancanti_attesi, 9)
  expect_equal(elenco$volumetria_effettuata_l, 1080)
  expect_equal(elenco$volumetria_attesa_fine_anno_l, 2160)
  expect_equal(elenco$volumetria_mancante_attesa_l, 1080)
  # con 2 componenti i litri inclusi sono 960: ne attesi 2160, 1200 eccedenti, 10 euro ogni 120 litri
  expect_equal(elenco$volumetria_inclusa_l, 960)
  expect_equal(elenco$volumetria_eccedente_attesa_l, 1200)
  expect_equal(elenco$eccedenza_attesa_eur, 100)
  # il totale atteso e' effettuato + mancante
  expect_equal(
    elenco$volumetria_attesa_fine_anno_l,
    elenco$volumetria_effettuata_l + elenco$volumetria_mancante_attesa_l
  )
  # l'eccedenza non e' proporzionata ai giorni di attivita': quota 1 e importo pieno
  expect_equal(elenco$quota_attivita, 1)
  # la premialita' non include le eccedenti, e l'eccedenza non include chi ha il premio
  premialita <- elenco_premialita(economia, "Saponette e antenne", contenitori_economia_prova(), "2025-01-01", "2025-06-30")
  expect_false("F2" %in% premialita$id_fornitura)
  # una fornitura senza lo stato nell'associazione (file vecchi): nessun bidone attivo, senza errori
  vecchi <- dplyr::select(contenitori_economia_prova(), -"stato_fornitura")
  senza_stato <- elenco_eccedenza(economia, "Saponette e antenne", vecchi, "2025-01-01", "2025-06-30")
  expect_identical(senza_stato$bidoni_attivi, 0L)
  expect_identical(senza_stato$stato_fornitura, "")
})

test_that("una fornitura dell'anagrafica senza letture conta nella propensione, con zero letture", {
  anagrafica <- dplyr::filter(anagrafica_economia_prova(), servizio == "SECCO")
  unita <- unita_economia_prova()
  con_letture <- propensione_conferimento(unita, anagrafica, "SECCO")
  uno <- con_letture[con_letture$gruppo == "1 componente", ]
  # tolte tutte le letture di F1, la fornitura resta: stessi contenitori e massimi, meno letture
  senza_f4 <- propensione_conferimento(unita[unita$id_fornitura != "F1", ], anagrafica, "SECCO")
  uno_senza <- senza_f4[senza_f4$gruppo == "1 componente", ]
  letture_f4 <- sum(unita$id_fornitura == "F1" & unita$stato %in% "PIA")
  expect_gt(letture_f4, 0)
  expect_identical(uno_senza$forniture, uno$forniture)
  expect_identical(uno_senza$contenitori, uno$contenitori)
  expect_equal(uno_senza$conferimenti_massimi, uno$conferimenti_massimi)
  expect_identical(uno_senza$letture, uno$letture - letture_f4)
  expect_lt(uno_senza$propensione_sul_massimo, uno$propensione_sul_massimo)
  # senza nessuna lettura la propensione e' zero, non mancante
  nessuna <- propensione_conferimento(unita[0, ], anagrafica, "SECCO")
  expect_identical(nessuna$letture[nessuna$gruppo == "Domestiche (totale)"], 0L)
  expect_equal(nessuna$propensione_sul_massimo[nessuna$gruppo == "Domestiche (totale)"], 0)
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

