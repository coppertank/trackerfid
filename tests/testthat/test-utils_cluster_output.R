soglie <- soglie_indicatore()

test_that("la classificazione segue le regole nell'ordine previsto", {
  classifica <- function(
    frequenza,
    n_cluster,
    dispersione,
    tutti_compatti = dispersione < 50
  ) {
    classifica_cluster(frequenza, n_cluster, dispersione, tutti_compatti)
  }
  expect_identical(classifica(4, 1, 10), "GHOST_TAG")
  expect_identical(classifica(4.9, 5, 2000), "GHOST_TAG")
  expect_identical(classifica(26, 1, 20), "VALID_TARGET")
  expect_identical(classifica(5, 1, 49.9), "VALID_TARGET")
  expect_identical(classifica(300, 1, 0), "VALID_TARGET")
  expect_identical(classifica(26, 2, 20), "RELOCATED_BIN")
  expect_identical(classifica(26, 3, 20), "RELOCATED_BIN")
  expect_identical(classifica(400, 1, 20), "DEPOT_STUCK")
  expect_identical(classifica(400, 1, 2000), "TRUCK_STOWAWAY")
  expect_identical(classifica(400, 4, 800), "TRUCK_STOWAWAY")
  expect_identical(classifica(26, 1, 1500), "SCATTERED_READS / GPS_NOISE")
  # molti frammenti compatti sono letture sparse, non uno spostamento
  expect_identical(classifica(26, 4, 10), "SCATTERED_READS / GPS_NOISE")
  expect_identical(classifica(26, 17, 0), "SCATTERED_READS / GPS_NOISE")
})

test_that("i casi non previsti restano non classificati", {
  # un solo cluster né compatto né sparso
  expect_identical(classifica_cluster(26, 1, 200, FALSE), "UNCATEGORIZED")
  expect_identical(classifica_cluster(26, 1, 50, FALSE), "UNCATEGORIZED")
  # due cluster, di cui uno non compatto
  expect_identical(classifica_cluster(26, 2, 20, FALSE), "UNCATEGORIZED")
  # troppe letture, dispersione intermedia
  expect_identical(classifica_cluster(400, 1, 200, FALSE), "UNCATEGORIZED")
  # troppe letture in più cluster compatti: non è un tag fermo in deposito
  expect_identical(classifica_cluster(400, 3, 10, TRUE), "UNCATEGORIZED")
  # tutti gli esiti possibili hanno un colore
  expect_true("UNCATEGORIZED" %in% indicatori_config()$indicatore)
})

test_that("l'indice di fiducia è tra 0 e 1 e premia i cluster affidabili", {
  fiducia <- function(
    censito = TRUE,
    cluster = 26,
    globale = 26,
    dispersione = 10,
    stabile = TRUE,
    quota = NA,
    previste = 26,
    presunte = 26
  ) {
    indice_fiducia(
      censito,
      cluster,
      globale,
      dispersione,
      stabile,
      quota,
      previste,
      presunte
    )
  }
  expect_equal(sum(pesi_fiducia()), 1)
  expect_equal(fiducia(), 1)

  # ogni fattore negativo abbassa l'indice
  expect_lt(fiducia(censito = FALSE, previste = NA, quota = 1), fiducia())
  expect_lt(fiducia(cluster = 3, globale = 3), fiducia())
  expect_lt(fiducia(dispersione = 600), fiducia())
  expect_lt(fiducia(stabile = FALSE), fiducia())
  expect_lt(fiducia(presunte = 200), fiducia())
  expect_lt(fiducia(cluster = 13, globale = 26), fiducia())

  # compattezza: piena fino a 50 m, nulla da 500 m
  expect_equal(fiducia(dispersione = 0), fiducia(dispersione = 50))
  expect_equal(fiducia(dispersione = 500), fiducia(dispersione = 5000))
  expect_gt(fiducia(dispersione = 100), fiducia(dispersione = 300))

  # coerenza del servizio atteso per i non censiti: nulla fino al 50%, piena dall'80%
  non_censito <- function(quota) {
    fiducia(censito = FALSE, previste = NA, quota = quota)
  }
  expect_equal(non_censito(0.5), non_censito(NA))
  expect_equal(non_censito(0.8), non_censito(1))
  expect_gt(non_censito(0.7), non_censito(0.55))

  # caso peggiore e limiti
  peggiore <- indice_fiducia(FALSE, 1, 100, 5000, TRUE, 0.3, NA, 1)
  expect_gte(peggiore, 0)
  expect_lt(peggiore, 0.2)
  tutti <- analisi_esempio()$cluster_indice_fiducia
  expect_true(all(tutti >= 0 & tutti <= 1))
  expect_false(anyNA(tutti))
})

test_that("la tabella ha 25 colonne e una riga per cluster", {
  letture <- dplyr::bind_rows(
    letture_rfid("FERMO", date_2025(26)),
    letture_rfid(
      "MOSSO",
      date_2025(20),
      lat = 41.9 + gradi_lat(rep(c(0, 800), each = 10))
    ),
    letture_rfid("RARO", date_2025(2)),
    letture_rfid(
      "IGNOTO",
      date_2025(10),
      presente = "Non Presente",
      atteso = c(rep("SECCO PAP", 9), "VETRO PAP")
    )
  )
  analisi <- calcola_analisi_cluster(letture, "2025-01-01", "2025-12-31")

  expect_identical(names(analisi), colonne_analisi_cluster())
  expect_length(names(analisi), 25)
  expect_equal(nrow(analisi), 5)
  expect_identical(analisi$RFID, c("FERMO", "IGNOTO", "MOSSO", "MOSSO", "RARO"))
  expect_identical(analisi$cluster_id, c(1L, 1L, 1L, 2L, 1L))
  expect_identical(analisi$globale_conteggio_cluster, c(1L, 1L, 2L, 2L, 1L))
  expect_identical(analisi$globale_numero_letture, c(26L, 10L, 20L, 20L, 2L))
  expect_identical(analisi$cluster_numero_letture, c(26L, 10L, 10L, 10L, 2L))
  expect_identical(
    analisi$indicatore_cluster,
    c(
      "VALID_TARGET",
      "VALID_TARGET",
      "RELOCATED_BIN",
      "RELOCATED_BIN",
      "GHOST_TAG"
    )
  )

  # tempi globali e del cluster
  mosso <- analisi[analisi$RFID == "MOSSO", ]
  expect_true(all(
    mosso$globale_prima_lettura == min(mosso$cluster_prima_lettura)
  ))
  expect_true(all(
    mosso$globale_ultima_lettura == max(mosso$cluster_ultima_lettura)
  ))
  expect_lt(mosso$cluster_ultima_lettura[1], mosso$cluster_prima_lettura[2])
  expect_equal(
    mosso$lat_baricentro_cluster[2] - mosso$lat_baricentro_cluster[1],
    gradi_lat(800),
    tolerance = 1e-4
  )
  expect_equal(mosso$cluster_dispersione_90th_m, c(0, 0))

  # periodo di analisi e volumi
  expect_true(all(analisi$analisi_dal == as.Date("2025-01-01")))
  expect_true(all(analisi$analisi_al == as.Date("2025-12-31")))
  expect_identical(analisi$volume_atteso, analisi$volume_previsto)

  # servizio, volume e raccolte previste: transponder per i censiti, atteso per gli altri
  ignoto <- analisi[analisi$RFID == "IGNOTO", ]
  expect_identical(ignoto$presente_a_database, "Non Presente")
  expect_true(is.na(ignoto$servizio_transponder))
  expect_identical(ignoto$servizio_atteso, "SECCO PAP")
  expect_identical(
    ignoto$servizio_atteso_dettaglio,
    "SECCO PAP (90%); VETRO PAP (10%)"
  )
  expect_true(is.na(ignoto$volume_previsto))
  expect_true(is.na(ignoto$numero_raccolte_annue_previste))
  censiti <- analisi[analisi$RFID != "IGNOTO", ]
  expect_true(all(censiti$servizio_transponder == "CARTA"))
  expect_true(all(is.na(censiti$servizio_atteso)))
  expect_true(all(is.na(censiti$servizio_atteso_dettaglio)))
  expect_true(all(censiti$volume_previsto == 240))
  expect_true(all(censiti$numero_raccolte_annue_previste == 26))

  parametri <- attr(analisi, "parametri")
  expect_equal(parametri$eps_m, 100)
  expect_equal(parametri$giorni_osservati, 345)
})

test_that("il servizio atteso sotto l'80% resta vuoto ma la composizione è riportata", {
  letture <- letture_rfid(
    "INCERTO",
    date_2025(20),
    presente = "Non Presente",
    atteso = c(
      rep("CARTA CONT.STRADALI", 8),
      rep("SECCO PAP", 7),
      rep("UMIDO PAP", 5)
    )
  )
  analisi <- calcola_analisi_cluster(letture, "2025-01-01", "2025-12-31")
  expect_true(is.na(analisi$servizio_atteso))
  expect_identical(
    analisi$servizio_atteso_dettaglio,
    "CARTA CONT.STRADALI (40%); SECCO PAP (35%); UMIDO PAP (25%)"
  )

  senza_stima <- letture_rfid("MUTO", date_2025(6), presente = "Non Presente")
  analisi <- calcola_analisi_cluster(senza_stima, "2025-01-01", "2025-12-31")
  expect_true(is.na(analisi$servizio_atteso))
  expect_true(is.na(analisi$servizio_atteso_dettaglio))
})

test_that("il cambio di servizio riporta il servizio attuale e la cronologia", {
  letture <- letture_rfid(
    "CAMBIA",
    date_2025(9),
    servizio = c(rep("CARTA", 3), rep("SECCO", 3), rep("CARTA", 3))
  )
  analisi <- calcola_analisi_cluster(letture, "2025-01-01", "2025-12-31")
  expect_identical(analisi$servizio_transponder, "CARTA")
  expect_identical(
    analisi$servizio_transponder_cronologia,
    "CARTA > SECCO > CARTA"
  )

  stabile <- calcola_analisi_cluster(
    letture_rfid("FISSO", date_2025(9)),
    "2025-01-01",
    "2025-12-31"
  )
  expect_true(is.na(stabile$servizio_transponder_cronologia))
  # il cambio di servizio abbassa la fiducia
  expect_lt(analisi$cluster_indice_fiducia, stabile$cluster_indice_fiducia)

  esempio <- analisi_esempio()
  speciale <- esempio[esempio$RFID == "RFD20250901001", ]
  expect_identical(
    speciale$servizio_transponder_cronologia,
    "CARTA > SECCO > CARTA"
  )
  expect_identical(speciale$servizio_transponder, "CARTA")
})

test_that("le raccolte annue presunte seguono la formula e non divergono", {
  # 26 letture distribuite su tutto l'anno osservato
  anno <- calcola_analisi_cluster(
    letture_rfid(
      "ANNO",
      format(seq(as.Date("2025-01-01"), as.Date("2025-12-31"), length.out = 26))
    ),
    "2025-01-01",
    "2025-12-31"
  )
  expect_equal(attr(anno, "parametri")$giorni_osservati, 365)
  expect_equal(anno$numero_raccolte_annue_presunte, 26)

  # contenitore attivo solo per 90 giorni: conta la durata del cluster
  base <- letture_rfid("BASE", c("2025-01-01", "2025-12-31"))
  breve <- letture_rfid(
    "BREVE",
    format(seq(as.Date("2025-06-01"), by = "15 days", length.out = 7))
  )
  analisi <- calcola_analisi_cluster(
    dplyr::bind_rows(base, breve),
    "2025-01-01",
    "2025-12-31"
  )
  presunte <- analisi$numero_raccolte_annue_presunte[analisi$RFID == "BREVE"]
  expect_equal(presunte, floor(7 * 365 / 90))
  expect_gt(presunte, floor(7 * 365 / 365))

  # cluster di una lettura o di pochi giorni: nessuna divisione per zero o stima gonfiata
  corti <- dplyr::bind_rows(
    base,
    letture_rfid("UNA", "2025-05-10"),
    letture_rfid("DUE", c("2025-05-10", "2025-05-11"))
  )
  analisi <- calcola_analisi_cluster(corti, "2025-01-01", "2025-12-31")
  expect_equal(analisi$numero_raccolte_annue_presunte[analisi$RFID == "UNA"], 1)
  expect_equal(analisi$numero_raccolte_annue_presunte[analisi$RFID == "DUE"], 2)
  expect_true(all(is.finite(analisi$numero_raccolte_annue_presunte)))
})

test_that("con un dataset parziale le frequenze sono rapportate ai giorni osservati", {
  # tre mesi di dati, analisi sull'anno intero: 6 letture in 92 giorni sono circa 24 l'anno
  date <- format(seq(
    as.Date("2025-10-01"),
    as.Date("2025-12-31"),
    length.out = 6
  ))
  analisi <- calcola_analisi_cluster(
    letture_rfid("TRIMESTRE", date),
    "2025-01-01",
    "2025-12-31"
  )
  expect_equal(attr(analisi, "parametri")$giorni_osservati, 92)
  expect_identical(analisi$indicatore_cluster, "VALID_TARGET")
  expect_equal(analisi$numero_raccolte_annue_presunte, floor(6 * 365 / 91))

  # periodo personalizzato: contano solo le letture del periodo
  letture <- letture_rfid("ANNUALE", date_2025(24))
  parte <- calcola_analisi_cluster(letture, "2025-03-15", "2025-09-30")
  expect_equal(attr(parte, "parametri")$giorni_osservati, 200)
  expect_lt(parte$globale_numero_letture, 24)
  expect_true(
    parte$globale_prima_lettura >= as.POSIXct("2025-03-15", tz = "UTC")
  )
  expect_identical(parte$analisi_dal, as.Date("2025-03-15"))
})

test_that("periodi senza letture o non validi sono gestiti", {
  letture <- letture_rfid("A", date_2025(5))
  vuota <- calcola_analisi_cluster(letture, "2030-01-01", "2030-12-31")
  expect_equal(nrow(vuota), 0)
  expect_identical(names(vuota), colonne_analisi_cluster())
  expect_error(
    calcola_analisi_cluster(letture, "2025-12-31", "2025-01-01"),
    "Periodo di analisi non valido"
  )
  expect_error(
    calcola_analisi_cluster(letture, NA, "2025-01-01"),
    "Periodo di analisi non valido"
  )
})

test_that("l'analisi funziona senza le colonne facoltative e con contenitori incoerenti", {
  letture <- letture_rfid("A", date_2025(8))
  letture$volume_previsto <- NULL
  letture$numero_raccolte_annue_previste <- NULL
  # censito ma senza servizio a database
  letture$servizio_transponder <- NA_character_
  analisi <- calcola_analisi_cluster(letture, "2025-01-01", "2025-12-31")
  expect_true(is.na(analisi$volume_previsto))
  expect_true(is.na(analisi$numero_raccolte_annue_previste))
  expect_true(is.na(analisi$servizio_transponder))
  expect_false(is.na(analisi$cluster_indice_fiducia))
  expect_identical(analisi$indicatore_cluster, "VALID_TARGET")
})

test_that("i parametri di DBSCAN cambiano il numero di cluster", {
  letture <- letture_rfid(
    "A",
    date_2025(10),
    lat = 41.9 + gradi_lat(rep(c(0, 150), 5))
  )
  expect_equal(
    nrow(calcola_analisi_cluster(
      letture,
      "2025-01-01",
      "2025-12-31",
      eps_m = 100
    )),
    2
  )
  expect_equal(
    nrow(calcola_analisi_cluster(
      letture,
      "2025-01-01",
      "2025-12-31",
      eps_m = 200
    )),
    1
  )

  # con letture minime > 1 le letture isolate formano il cluster 0
  isolata <- dplyr::bind_rows(
    letture,
    letture_rfid("A", "2025-12-25", lat = 41.9 + gradi_lat(5000))
  )
  analisi <- calcola_analisi_cluster(
    isolata,
    "2025-01-01",
    "2025-12-31",
    min_pts = 2
  )
  expect_identical(analisi$cluster_id, c(0L, 1L, 2L))
  expect_equal(analisi$cluster_numero_letture[analisi$cluster_id == 0], 1)
})

test_that("l'esportazione scrive date leggibili e celle vuote per i mancanti", {
  file <- withr::local_tempfile(fileext = ".csv")
  esporta_analisi_cluster(analisi_esempio(), file)
  righe <- readLines(file, n = 2)
  expect_identical(
    strsplit(gsub("\"", "", righe[1]), ",")[[1]],
    colonne_analisi_cluster()
  )
  expect_match(righe[2], "\"2025-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}\"")
  expect_no_match(righe[2], "NA", fixed = TRUE)
  expect_identical(
    nome_file_analisi_cluster("2025-01-01", as.Date("2025-12-31")),
    "cluster_analysis_2025-01-01_2025-12-31.csv"
  )
})
