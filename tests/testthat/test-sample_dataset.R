# Il dataset di esempio deve rispettare le specifiche del progetto.

test_that("dimensioni, periodo e mezzi", {
  d <- dati_esempio()
  expect_gte(nrow(d), 600)
  expect_gte(dplyr::n_distinct(d$RFID), 200)
  expect_lte(dplyr::n_distinct(d$RFID), 250)
  # 2025 completo, 2024 parziale: serve a provare la selezione dell'anno
  expect_equal(
    as.Date(range(d$giorno_lettura)),
    as.Date(c("2024-10-01", "2025-12-31"))
  )
  expect_identical(anni_disponibili(d), c(2025, 2024))

  ore <- as.numeric(format(d$giorno_lettura, "%H", tz = "UTC")) +
    as.numeric(format(d$giorno_lettura, "%M", tz = "UTC")) / 60
  expect_true(all(ore >= 6 & ore <= 19))

  mezzi <- dplyr::distinct(d, targa_veicolo, matricola_veicolo)
  expect_equal(nrow(mezzi), 5)
  expect_equal(anyDuplicated(mezzi$targa_veicolo), 0)
  expect_equal(anyDuplicated(mezzi$matricola_veicolo), 0)
})

test_that("coerenza tra stato a database, servizio, utenza, volume e raccolte", {
  d <- dati_esempio()
  non_presente <- d$presente_a_database == "Non Presente"
  for (campo in c(
    "servizio_transponder",
    "id_utenza",
    "volume_previsto",
    "numero_raccolte_annue_previste"
  )) {
    expect_true(all(is.na(d[[campo]][non_presente])), info = campo)
    expect_false(anyNA(d[[campo]][!non_presente]), info = campo)
  }
  expect_true(all(is.na(d$servizio_atteso[!non_presente])))
  expect_false(anyNA(d$latitudine))
  expect_false(anyNA(d$longitudine))
})

test_that("volume e raccolte previste sono realistici e univoci per RFID", {
  d <- dati_esempio()
  censiti <- d[d$presente_a_database == "Presente", ]
  expect_setequal(unique(censiti$volume_previsto), c(240, 770, 1100))
  expect_setequal(unique(censiti$numero_raccolte_annue_previste), c(13, 26))

  per_rfid <- dplyr::summarise(
    censiti,
    volumi = dplyr::n_distinct(volume_previsto),
    raccolte = dplyr::n_distinct(numero_raccolte_annue_previste),
    .by = "RFID"
  )
  expect_true(all(per_rfid$volumi == 1))
  expect_true(all(per_rfid$raccolte == 1))

  # i contenitori che hanno cambiato servizio conservano volume e raccolte iniziali
  stabili <- dplyr::filter(
    censiti,
    dplyr::n_distinct(servizio_transponder) == 1,
    .by = "RFID"
  )
  ultimi <- deduplica_ultimo_rfid(stabili)
  expect_true(all(
    ultimi$volume_previsto[
      ultimi$servizio_transponder == "PLASTICA E METALLI"
    ] ==
      770
  ))
  expect_true(all(
    ultimi$numero_raccolte_annue_previste[
      ultimi$servizio_transponder != "VETRO"
    ] ==
      26
  ))
  expect_setequal(
    unique(ultimi$numero_raccolte_annue_previste[
      ultimi$servizio_transponder == "VETRO"
    ]),
    c(13, 26)
  )
  quota_secco_1100 <- mean(
    ultimi$volume_previsto[ultimi$servizio_transponder == "SECCO"] == 1100
  )
  expect_equal(quota_secco_1100, 0.8, tolerance = 0.15)
})

test_that("distribuzioni vicine a quelle richieste", {
  d <- dati_esempio()
  ultimi <- deduplica_ultimo_rfid(d)

  expect_equal(calcola_pct_presente(ultimi), 0.85, tolerance = 0.02)

  quote <- prop.table(table(ultimi$servizio_transponder))
  attese <- c(
    SECCO = 0.40,
    CARTA = 0.27,
    VETRO = 0.11,
    UMIDO = 0.10,
    "PLASTICA E METALLI" = 0.07,
    "VERDE E RAMAGLIE" = 0.05
  )
  expect_setequal(names(quote), names(attese))
  expect_equal(
    as.numeric(quote[names(attese)]),
    unname(attese),
    tolerance = 0.1
  )

  # raccolta a cadenza fissa: un contenitore tipico ha una ventina di letture l'anno
  nel_2025 <- d[
    lubridate::year(d$giorno_lettura) == 2025 &
      d$presente_a_database == "Presente",
  ]
  letture <- as.integer(table(nel_2025$RFID))
  expect_gt(stats::median(letture), 15)
  expect_lt(stats::median(letture), 27)

  expect_equal(dplyr::n_distinct(d$id_utenza, na.rm = TRUE), 50)
  cambi_utenza <- tapply(d$id_utenza, d$RFID, function(x) {
    dplyr::n_distinct(x, na.rm = TRUE)
  })
  expect_gte(sum(cambi_utenza > 1), 5)
  expect_lte(sum(cambi_utenza > 1), 10)

  attesi <- d$servizio_atteso[d$presente_a_database == "Non Presente"]
  expect_gt(sum(is.na(attesi)), 0)
  expect_gt(sum(!is.na(attesi)), 0)
})

test_that("i servizi attesi sono i nomi dei giri e hanno tutti un'icona", {
  d <- dati_esempio()
  giri <- c(
    "ASSISTENTE SERVIZI",
    "CARTA CONT.STRADALI",
    "CARTA/CARTONE PAP",
    "PLAST.CONT.STRADALI",
    "PLASTICA PAP",
    "PULIZIA TERRIT.",
    "SECCO PAP",
    "SERVIZI MERCATI",
    "UMIDO CONT.STRADALI",
    "UMIDO PAP",
    "VERDE PAP",
    "VETRO PAP"
  )
  expect_setequal(unique(stats::na.omit(d$servizio_atteso)), giri)
  presenti <- unique(stats::na.omit(c(
    d$servizio_transponder,
    d$servizio_atteso
  )))
  expect_true(all(presenti %in% servizi_config()$servizio))
})

test_that("casi speciali presenti", {
  d <- dplyr::arrange(dati_esempio(), giorno_lettura)
  letture <- function(rfid) d[d$RFID == rfid, ]

  # righe di esempio della prima specifica
  expect_identical(
    format(letture("RFD20250901001")$giorno_lettura[1]),
    "2025-09-01 06:00:00"
  )
  expect_identical(
    format(letture("RFD20250901002")$giorno_lettura[1]),
    "2025-09-01 06:15:00"
  )
  expect_identical(
    format(letture("RFD20250901201")$giorno_lettura[1]),
    "2025-09-01 06:30:00"
  )

  cambio_servizio <- letture("RFD20250901001")
  expect_identical(
    cambio_servizio$servizio_transponder[1:3],
    c("CARTA", "SECCO", "CARTA")
  )
  expect_identical(
    format(cambio_servizio$giorno_lettura[1:3], "%d/%m"),
    c("01/09", "15/09", "28/09")
  )
  expect_true(all(cambio_servizio$servizio_transponder[-2] == "CARTA"))

  cambio_utenza <- letture("RFD20250901050")
  expect_identical(cambio_utenza$id_utenza[1], "UTZ001")
  expect_identical(format(cambio_utenza$giorno_lettura[1], "%d/%m"), "01/09")
  prima_nuova <- cambio_utenza[cambio_utenza$id_utenza == "UTZ025", ][1, ]
  expect_identical(format(prima_nuova$giorno_lettura, "%d/%m"), "20/09")

  con_stima <- letture("RFD20250915201")
  expect_equal(nrow(con_stima), 4)
  expect_true(all(con_stima$servizio_atteso == "SECCO PAP"))

  senza_stima <- letture("RFD20250920250")
  expect_equal(nrow(senza_stima), 2)
  expect_true(all(is.na(senza_stima$servizio_atteso)))

  spostato <- letture("RFD20250905100")
  expect_equal(spostato$latitudine[1:2], c(41.85, 42.00))
  expect_equal(spostato$longitudine[1:2], c(12.35, 12.50))
})

test_that("il dataset contiene un esempio per ogni indicatore di cluster", {
  analisi <- analisi_esempio()
  indicatore <- function(rfid) {
    unique(analisi$indicatore_cluster[analisi$RFID == rfid])
  }

  expect_identical(indicatore("RFD20241001301"), "DEPOT_STUCK")
  expect_identical(indicatore("RFD20250203302"), "TRUCK_STOWAWAY")
  expect_identical(indicatore("RFD20241003303"), "SCATTERED_READS / GPS_NOISE")
  expect_identical(indicatore("RFD20250905100"), "RELOCATED_BIN")
  expect_identical(indicatore("RFD20250915201"), "GHOST_TAG")
  expect_identical(indicatore("RFD20250901001"), "VALID_TARGET")

  conteggi <- table(analisi$indicatore_cluster)
  expect_setequal(
    names(conteggi),
    setdiff(indicatori_config()$indicatore, "UNCATEGORIZED")
  )
  # la maggior parte dei contenitori è al suo posto
  expect_gt(conteggi[["VALID_TARGET"]] / dplyr::n_distinct(analisi$RFID), 0.7)
})

test_that("l'output di esempio incluso corrisponde all'analisi del dataset", {
  file <- app_sys("extdata", "cluster_analysis_2025-01-01_2025-12-31.csv")
  expect_true(file.exists(file))
  salvato <- utils::read.csv(file, stringsAsFactors = FALSE)
  analisi <- analisi_esempio()

  expect_identical(names(salvato), colonne_analisi_cluster())
  expect_equal(nrow(salvato), nrow(analisi))
  expect_identical(salvato$RFID, analisi$RFID)
  expect_identical(salvato$indicatore_cluster, analisi$indicatore_cluster)
  expect_equal(salvato$cluster_indice_fiducia, analisi$cluster_indice_fiducia)
})

test_that("il comune è un dato dell'anagrafica: solo per i censiti, uno per RFID", {
  d <- dati_esempio()
  non_presente <- d$presente_a_database == "Non Presente"
  expect_true(all(is.na(d$comune[non_presente])))
  expect_false(anyNA(d$comune[!non_presente]))
  expect_setequal(
    unique(d$comune[!non_presente]),
    c("COMUNE NORD", "COMUNE EST", "COMUNE SUD", "COMUNE OVEST")
  )
  per_rfid <- tapply(
    d$comune[!non_presente],
    d$RFID[!non_presente],
    dplyr::n_distinct
  )
  expect_true(all(per_rfid == 1))

  # il contenitore spostato di 21 km resta registrato nel comune di partenza
  expect_identical(unique(d$comune[d$RFID == "RFD20250905100"]), "COMUNE SUD")
  expect_identical(unique(d$comune[d$RFID == "RFD20250901001"]), "COMUNE EST")
})

test_that("le letture storiche di esempio precedono quelle con le antenne", {
  esito <- carica_storico(percorso_storico_esempio())
  storico <- esito$dati
  d <- dati_esempio()

  expect_length(esito$avvisi, 0)
  expect_identical(names(storico), colonne_storico())
  expect_equal(nrow(storico), 7375)
  expect_lt(max(storico$giorno_lettura), min(d$giorno_lettura))
  expect_equal(
    as.Date(range(storico$giorno_lettura)),
    as.Date(c("2020-01-01", "2024-09-30"))
  )
  expect_setequal(lubridate::year(storico$giorno_lettura), 2020:2024)

  # RFID letti da entrambi i sistemi, solo dalle antenne, solo in passato
  rfid_storico <- unique(storico$RFID)
  rfid_antenne <- unique(d$RFID)
  expect_length(rfid_storico, 229)
  expect_length(intersect(rfid_storico, rfid_antenne), 185)
  expect_length(setdiff(rfid_antenne, rfid_storico), 54)
  expect_length(setdiff(rfid_storico, rfid_antenne), 44)

  # tra gli RFID con una storia ci sono anche dei non censiti
  ultimi <- deduplica_ultimo_rfid(d)
  con_storia <- ultimi[ultimi$RFID %in% rfid_storico, ]
  expect_gt(sum(con_storia$presente_a_database == "Non Presente"), 5)
  # i contenitori dei casi speciali, letti da settembre 2025, non hanno storia
  expect_false(any(
    c("RFD20250901001", "RFD20250901050", "RFD20250905100") %in% rfid_storico
  ))
})

test_that("le letture storiche contengono anni interi senza letture", {
  storico <- storico_esempio()
  # contenitore di esempio: letto nel 2020, 2022 e 2024, mai nel 2021 e nel 2023
  anni <- lubridate::year(storico$giorno_lettura[
    storico$RFID == "RFD20241004003"
  ])
  expect_identical(as.integer(table(anni)), c(16L, 16L, 6L))
  expect_identical(sort(unique(anni)), c(2020, 2022, 2024))

  tabella <- tabella_letture_annuali(
    unisci_letture(dati_esempio(), storico),
    anagrafica_rfid(dati_esempio())
  )
  expect_equal(nrow(tabella), 239)
  esempio <- tabella[tabella$RFID == "RFD20241004003", ]
  expect_equal(
    unlist(esempio[, paste0("letture_", 2020:2025)], use.names = FALSE),
    c(16, 0, 16, 0, 12, 25)
  )
  expect_identical(esempio$comune, "COMUNE EST")
  expect_equal(esempio$numero_raccolte_annue_previste, 26)
})
