# Il dataset di esempio deve rispettare le specifiche del progetto.

test_that("dimensioni, periodo e mezzi", {
  d <- dati_esempio()
  expect_gte(nrow(d), 600)
  expect_equal(nrow(d), 6475)
  expect_equal(dplyr::n_distinct(d$RFID), 251)
  # i contenitori: gli altri RFID sono sacchetti
  bidoni <- unique(d$RFID[!rfid_sacchetto(d$RFID)])
  expect_length(bidoni, 239)
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
  for (campo in c("servizio_transponder", "id_utenza")) {
    expect_true(all(is.na(d[[campo]][non_presente])), info = campo)
    expect_false(anyNA(d[[campo]][!non_presente]), info = campo)
  }
  # volume e raccolte sono dati del contenitore: un sacchetto non li ha
  # nemmeno da censito
  senza_contenitore <- non_presente | rfid_sacchetto(d$RFID)
  for (campo in c("volume_previsto", "numero_raccolte_annue_previste")) {
    expect_true(all(is.na(d[[campo]][senza_contenitore])), info = campo)
    expect_false(anyNA(d[[campo]][!senza_contenitore]), info = campo)
  }
  expect_false(anyNA(d$latitudine))
  expect_false(anyNA(d$longitudine))
})

test_that("volume e raccolte previste sono realistici e univoci per RFID", {
  d <- dati_esempio()
  censiti <- d[
    d$presente_a_database == "Presente" & !rfid_sacchetto(d$RFID),
  ]
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
  # le quote riguardano i contenitori: i sacchetti hanno un test a parte
  d <- dati_esempio()
  d <- d[!rfid_sacchetto(d$RFID), ]
  ultimi <- deduplica_ultimo_rfid(d)

  expect_equal(
    mean(ultimi$presente_a_database == "Presente"),
    0.85,
    tolerance = 0.02
  )

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

})

test_that("ogni lettura ha il giro e il comune del mezzo che l'ha fatta", {
  esito <- carica_dataset(percorso_dataset_esempio())
  expect_length(esito$avvisi, 0)
  d <- esito$dati
  # una lettura esiste solo se un mezzo passa durante un giro: servizio
  # atteso e comune di lettura ci sono sempre, anche per i censiti
  expect_false(anyNA(d$servizio_atteso))
  expect_false(anyNA(d$comune_lettura))
  expect_false(anyNA(d$comune_assegnato))
  expect_false(anyNA(d$cantiere))

  # per i censiti il giro è quasi sempre quello del loro servizio
  censita <- d$presente_a_database == "Presente"
  coerenti <- tipologia_servizio(d$servizio_atteso[censita]) ==
    tipologia_servizio(d$servizio_transponder[censita])
  expect_gt(mean(coerenti), 0.95)
  expect_lt(mean(coerenti), 1)

  # tra i contenitori non censiti del 2025 ci sono stime nette e stime incerte
  nel_2025 <- filtra_periodo(d, periodo_anno(2025))
  stime <- servizio_prevalente_non_censiti(nel_2025)
  non_censiti <- unique(nel_2025$RFID[nel_2025$presente_a_database == "Non Presente"])
  expect_setequal(stime$RFID, non_censiti)
  # tre dei non censiti sono sacchetti: non hanno un servizio da stimare
  expect_length(non_censiti, 40)
  stime <- stime[!rfid_sacchetto(stime$RFID), ]
  expect_equal(nrow(stime), 37)
  expect_equal(sum(!is.na(stime$servizio_prevalente)), 18)
  expect_equal(sum(is.na(stime$servizio_prevalente)), 19)
  expect_true(all(stime$quota[is.na(stime$servizio_prevalente)] < 0.8))
  # il tag fermo in deposito è letto dai giri di tutti i mezzi: stima incerta
  expect_true(is.na(stime$servizio_prevalente[stime$RFID == "RFD20241001301"]))
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

  # due letture in due giri di tipologie diverse: la stima resta incerta
  incerto <- letture("RFD20250920250")
  expect_equal(nrow(incerto), 2)
  expect_identical(incerto$servizio_atteso, c("SECCO PAP", "CARTA/CARTONE PAP"))
  expect_true(all(is.na(incerto$comune_da_database)))
  expect_false(anyNA(incerto$comune_lettura))

  spostato <- letture("RFD20250905100")
  expect_equal(spostato$latitudine[1:2], c(45.4744, 45.6488))
  expect_equal(spostato$longitudine[1:2], c(11.8449, 11.7836))
})

test_that("il dataset contiene alcuni sacchetti, censiti e non censiti", {
  d <- dati_esempio()
  sacchetto <- rfid_sacchetto(d$RFID)
  s <- d[sacchetto, ]
  # un sacchetto è monouso: dopo lo svuotamento non si legge più, quindi ha
  # una lettura sola
  expect_equal(nrow(s), 12)
  expect_equal(anyDuplicated(s$RFID), 0)
  ultimi <- deduplica_ultimo_rfid(s)
  expect_equal(nrow(ultimi), 12)
  expect_equal(sum(ultimi$presente_a_database == "Presente"), 8)
  expect_equal(sum(ultimi$presente_a_database == "Non Presente"), 4)
  # codice di 24 caratteri con il prefisso dei sacchetti: i contenitori ne
  # hanno un altro
  expect_true(all(nchar(s$RFID) == 24 & startsWith(s$RFID, "00BD")))
  expect_false(any(startsWith(d$RFID[!sacchetto], "00BD")))

  # a database un sacchetto ha solo l'utenza: la validazione gli dà il
  # servizio dei sacchetti, e il suo comune è quello del giro
  censito <- s$presente_a_database == "Presente"
  expect_true(all(s$servizio_transponder[censito] == servizio_sacchetti()))
  expect_true(all(is.na(s$servizio_transponder[!censito])))
  expect_setequal(s$id_utenza[censito], sprintf("SAC%03d", 1:4))
  expect_false(any(s$id_utenza[censito] %in% d$id_utenza[!sacchetto]))
  expect_true(all(is.na(s$comune_da_database)))
  expect_identical(s$comune_assegnato, s$comune_lettura)
  expect_setequal(s$cantiere, cantieri())
  # letti dai giri del secco
  expect_true(all(s$servizio_atteso == "SECCO PAP"))
  # due sono letti solo nel 2024, gli altri dieci nel 2025
  expect_equal(
    as.integer(table(lubridate::year(ultimi$giorno_lettura))),
    c(2, 10)
  )

  # nel 2025 icona, voce dei filtri e statistiche sono quelle dei sacchetti,
  # censiti o no
  nel_2025 <- aggiungi_servizio_icona(filtra_periodo(d, periodo_anno(2025)))
  expect_true(all(
    servizio_mostrato(nel_2025)[rfid_sacchetto(nel_2025$RFID)] ==
      servizio_sacchetti()
  ))
  stat <- calcola_statistiche(deduplica_ultimo_rfid(nel_2025))
  expect_equal(stat$transponder[[servizio_sacchetti()]], 7)
  expect_equal(stat$atteso[[servizio_sacchetti()]], 3)
  # i contenitori non censiti restano 18 con la stima e 19 con la stima incerta
  expect_equal(sum(stat$atteso) - 3, 18)
  expect_equal(stat$stima_incerta, 19)

  # l'analisi dei cluster riguarda i contenitori: i sacchetti restano fuori
  analisi <- analisi_esempio()
  expect_false(any(rfid_sacchetto(analisi$RFID)))
  expect_equal(dplyr::n_distinct(analisi$RFID), 238)
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

test_that("la geografia segue la tabella dei comuni e le quote dei cantieri", {
  d <- dati_esempio()
  non_presente <- d$presente_a_database == "Non Presente"

  # comune del database: solo per i contenitori censiti, uno per RFID. Un
  # sacchetto non lo ha nemmeno da censito
  sacchetto <- rfid_sacchetto(d$RFID)
  expect_true(all(is.na(d$comune_da_database[non_presente | sacchetto])))
  expect_false(anyNA(d$comune_da_database[!non_presente & !sacchetto]))
  per_rfid <- tapply(
    d$comune_da_database[!non_presente],
    d$RFID[!non_presente],
    dplyr::n_distinct
  )
  expect_true(all(per_rfid == 1))

  # tutti i comuni sono in tabella; il cantiere è quello del comune assegnato
  comuni <- stats::na.omit(c(d$comune_da_database, d$comune_lettura))
  expect_true(all(comuni %in% comuni_cantieri()$comune))
  expect_identical(
    d$comune_assegnato,
    dplyr::coalesce(d$comune_da_database, d$comune_lettura)
  )
  expect_identical(d$cantiere, cantiere_di_comune(d$comune_assegnato))
  # per un non censito il comune assegnato è quello del giro che l'ha letto
  expect_identical(d$comune_assegnato[non_presente], d$comune_lettura[non_presente])
  expect_false(anyNA(d$cantiere))

  # quote dei cantieri tra i contenitori: 12%, 26%, 37% e 25%
  anagrafica <- anagrafica_rfid(d)
  quote <- prop.table(table(anagrafica$cantiere))
  expect_equal(
    as.numeric(quote[c("ASIAGO", "BASSANO", "CAMPOSAMPIERO", "RUBANO")]),
    c(0.12, 0.26, 0.37, 0.25),
    tolerance = 0.1
  )
  expect_equal(dplyr::n_distinct(anagrafica$comune, na.rm = TRUE), 54)

  # tutte le letture cadono nella zona servita
  zona <- riquadro_zona()
  expect_true(all(d$latitudine > zona$lat_min & d$latitudine < zona$lat_max))
  expect_true(all(d$longitudine > zona$lng_min & d$longitudine < zona$lng_max))

  # il contenitore spostato resta registrato a Limena, cantiere di Rubano, ma
  # dalla seconda lettura in poi viene letto a Cittadella
  spostato <- d[d$RFID == "RFD20250905100", ]
  expect_identical(unique(spostato$comune_da_database), "LIMENA")
  expect_identical(unique(spostato$cantiere), "RUBANO")
  expect_identical(spostato$comune_lettura[1:2], c("LIMENA", "CITTADELLA"))
  expect_identical(
    unique(d$comune_da_database[d$RFID == "RFD20250901001"]),
    "BASSANO DEL GRAPPA"
  )
  # il tag rimasto sul mezzo viene letto in più comuni
  sul_mezzo <- d$comune_lettura[d$RFID == "RFD20250203302"]
  expect_gt(dplyr::n_distinct(sul_mezzo, na.rm = TRUE), 1)
})

test_that("le letture storiche di esempio precedono quelle con le antenne", {
  esito <- carica_storico(percorso_storico_esempio())
  storico <- esito$dati
  d <- dati_esempio()

  expect_length(esito$avvisi, 0)
  expect_identical(names(storico), colonne_storico())
  expect_equal(nrow(storico), 7025)
  expect_lt(max(storico$giorno_lettura), min(d$giorno_lettura))
  expect_equal(
    as.Date(range(storico$giorno_lettura)),
    as.Date(c("2020-01-01", "2024-09-30"))
  )
  expect_setequal(lubridate::year(storico$giorno_lettura), 2020:2024)

  # RFID letti da entrambi i sistemi, solo dalle antenne, solo in passato
  rfid_storico <- unique(storico$RFID)
  rfid_antenne <- unique(d$RFID)
  expect_length(rfid_storico, 227)
  expect_length(intersect(rfid_storico, rfid_antenne), 183)
  # solo dalle antenne: 56 contenitori e i 12 sacchetti
  solo_antenne <- setdiff(rfid_antenne, rfid_storico)
  expect_length(solo_antenne, 68)
  expect_equal(sum(rfid_sacchetto(solo_antenne)), 12)
  expect_false(any(rfid_sacchetto(rfid_storico)))
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
  expect_identical(as.integer(table(anni)), c(16L, 6L, 7L))
  expect_identical(sort(unique(anni)), c(2020, 2022, 2024))

  tabella <- tabella_letture_annuali(
    unisci_letture(dati_esempio(), storico),
    anagrafica_rfid(dati_esempio())
  )
  expect_equal(nrow(tabella), 251)
  esempio <- tabella[tabella$RFID == "RFD20241004003", ]
  expect_equal(
    unlist(esempio[, paste0("letture_", 2020:2025)], use.names = FALSE),
    c(16, 0, 6, 0, 13, 25)
  )
  expect_identical(esempio$comune, "RUBANO")
  expect_identical(esempio$cantiere, "RUBANO")
  expect_equal(esempio$numero_raccolte_annue_previste, 26)
})
