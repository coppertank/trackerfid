# Stessa partizione, a prescindere dai numeri assegnati ai gruppi.
stessa_partizione <- function(a, b) {
  identical(match(a, unique(a)), match(b, unique(b)))
}

test_that("la proiezione locale conserva le distanze", {
  lat <- 41.9 + c(0, gradi_lat(100), 0)
  lon <- 12.5 + c(0, 0, 0.01)
  xy <- proietta_metri(lat, lon)
  expect_equal(sqrt(sum((xy[1, ] - xy[2, ])^2)), 100, tolerance = 1e-6)
  attesa <- distanza_haversine_m(lat[3], lon[3], lat[1], lon[1])
  expect_equal(sqrt(sum((xy[1, ] - xy[3, ])^2)), attesa, tolerance = 1e-3)
})

test_that("la distanza di Haversine usa il raggio medio terrestre", {
  expect_equal(
    distanza_haversine_m(41.9 + gradi_lat(250), 12.5, 41.9, 12.5),
    250,
    tolerance = 1e-6
  )
  expect_equal(
    distanza_haversine_m(c(41.9, 41.9), c(12.5, 12.5), 41.9, 12.5),
    c(0, 0)
  )
  expect_length(distanza_haversine_m(numeric(0), numeric(0), 41.9, 12.5), 0)
  # un centro per ogni punto, e punti lontani: un grado di meridiano
  expect_equal(
    distanza_haversine_m(c(45, 0), c(11, 0), c(46, 0), c(11, 180)),
    raggio_terra_m() * c(pi / 180, pi),
    tolerance = 1e-9
  )
})

test_that("la distanza di Haversine coincide con quella di geosphere", {
  skip_if_not_installed("geosphere")
  set.seed(7)
  n <- 500
  lat <- 45.6 + stats::runif(n, -0.3, 0.3)
  lon <- 11.7 + stats::runif(n, -0.4, 0.4)
  lat_centro <- lat + stats::rnorm(n, 0, 0.01)
  lon_centro <- lon + stats::rnorm(n, 0, 0.01)
  expect_equal(
    distanza_haversine_m(lat, lon, lat_centro, lon_centro),
    geosphere::distHaversine(
      cbind(lon, lat),
      cbind(lon_centro, lat_centro),
      r = raggio_terra_m()
    ),
    tolerance = 1e-9
  )
})

test_that("la dispersione è il 90° percentile della distanza dal baricentro", {
  metri <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 1000)
  lat <- 41.9 + gradi_lat(metri)
  distanze <- distanza_haversine_m(lat, rep(12.5, length(lat)), 41.9, 12.5)
  expect_equal(
    quantile_per_gruppo(distanze, rep(1L, length(distanze)), 0.9),
    as.numeric(stats::quantile(metri, 0.9)),
    tolerance = 1e-6
  )
})

test_that("DBSCAN unisce le letture a catena entro il raggio e separa le altre", {
  # tre punti a 80 m l'uno dall'altro (catena), uno a 300 m dall'ultimo
  lat <- 41.9 + gradi_lat(c(0, 80, 160, 460))
  lon <- rep(12.5, 4)
  cluster <- cluster_dbscan(lat, lon, eps_m = 100, min_pts = 1)
  expect_identical(cluster[1:3], rep(cluster[1], 3))
  expect_false(cluster[4] == cluster[1])
  expect_true(all(cluster > 0))

  # raggio più stretto: tutti separati; più largo: tutti insieme
  expect_equal(dplyr::n_distinct(cluster_dbscan(lat, lon, eps_m = 50)), 4)
  expect_equal(dplyr::n_distinct(cluster_dbscan(lat, lon, eps_m = 350)), 1)

  # con più letture minime, la lettura isolata diventa rumore (cluster 0)
  expect_identical(
    cluster_dbscan(lat, lon, eps_m = 100, min_pts = 3),
    c(1L, 1L, 1L, 0L)
  )
})

test_that("i casi limite di DBSCAN non danno errori", {
  expect_identical(cluster_dbscan(numeric(0), numeric(0)), integer(0))
  expect_identical(cluster_dbscan(41.9, 12.5), 1L)
  expect_identical(cluster_dbscan(41.9, 12.5, min_pts = 2), 0L)
  expect_identical(cluster_dbscan(rep(41.9, 5), rep(12.5, 5)), rep(1L, 5))
})

test_that("i cluster sono numerati in ordine di prima lettura", {
  quando <- as.POSIXct("2025-01-01", tz = "UTC") + (1:6) * 86400
  expect_identical(
    ordina_cluster(c(2L, 2L, 3L, 1L, 3L, 1L), quando),
    c(1L, 1L, 2L, 3L, 2L, 3L)
  )
  # il rumore resta 0
  expect_identical(
    ordina_cluster(c(0L, 2L, 1L, 0L, 2L, 1L), quando),
    c(0L, 1L, 2L, 0L, 1L, 2L)
  )
  expect_identical(ordina_cluster(c(0L, 0L), quando[1:2]), c(0L, 0L))
})

test_that("il clustering è indipendente per ogni RFID", {
  letture <- dplyr::bind_rows(
    letture_rfid("A", date_2025(4), lat = 41.9 + gradi_lat(c(0, 5, 500, 505))),
    letture_rfid("B", date_2025(3), lat = 41.9 + gradi_lat(c(500, 0, 3)))
  )
  con_cluster <- assegna_cluster(letture)
  expect_identical(
    con_cluster$cluster_id[con_cluster$RFID == "A"],
    c(1L, 1L, 2L, 2L)
  )
  # per B il primo luogo visitato è quello a 500 m
  expect_identical(
    con_cluster$cluster_id[con_cluster$RFID == "B"],
    c(1L, 2L, 2L)
  )
  expect_equal(nrow(con_cluster), nrow(letture))
})

test_that("sul dataset di esempio la proiezione dà gli stessi cluster di Haversine", {
  skip_if_not_installed("geosphere")
  d <- filtra_periodo(dati_esempio(), periodo_anno(2025))
  per_rfid <- split(d, d$RFID)
  # il tag sul camion ha 570 letture: prova anche un caso numeroso
  campione <- per_rfid[c(
    "RFD20250203302",
    "RFD20241003303",
    "RFD20250905100",
    utils::head(names(per_rfid), 40)
  )]
  for (letture in campione) {
    if (nrow(letture) < 2) {
      next
    }
    coordinate <- cbind(letture$longitudine, letture$latitudine)
    distanze <- stats::as.dist(
      geosphere::distm(coordinate, fun = function(a, b) {
        geosphere::distHaversine(a, b, r = raggio_terra_m())
      })
    )
    esatto <- dbscan::dbscan(distanze, eps = 100, minPts = 1)$cluster
    proiettato <- cluster_dbscan(
      letture$latitudine,
      letture$longitudine,
      100,
      1
    )
    expect_true(stessa_partizione(esatto, proiettato), info = letture$RFID[1])
  }
})

test_that("gli RFID letti in un solo punto saltano DBSCAN con lo stesso risultato", {
  d <- filtra_periodo(dati_esempio(), periodo_anno(2025))
  per_rfid <- split(seq_len(nrow(d)), d$RFID)
  for (parametri in list(c(100, 1), c(100, 3), c(15, 1), c(2000, 2))) {
    veloce <- assegna_cluster(d, parametri[1], parametri[2])$cluster_id
    lento <- integer(nrow(d))
    for (righe in per_rfid) {
      lento[righe] <- ordina_cluster(
        cluster_dbscan(
          d$latitudine[righe],
          d$longitudine[righe],
          parametri[1],
          parametri[2]
        ),
        d$giorno_lettura[righe]
      )
    }
    expect_identical(veloce, lento, info = paste(parametri, collapse = ", "))
  }

  # con più letture minime un RFID compatto ma poco letto è rumore
  poche <- letture_rfid("A", date_2025(2))
  expect_identical(assegna_cluster(poche, 100, 3)$cluster_id, c(0L, 0L))
  expect_identical(assegna_cluster(poche, 100, 2)$cluster_id, c(1L, 1L))
  expect_identical(assegna_cluster(d[0, ])$cluster_id, integer(0))
})

test_that("le letture ripetute nello stesso punto entrano in DBSCAN con il loro peso", {
  # tre punti in catena a 60 m, un punto letto 48 volte a 1 km e uno letto
  # due volte a 3 km: 300 letture in ordine sparso
  metri <- rep(c(0, 60, 120, 1000, 3000), c(100, 80, 70, 48, 2))
  set.seed(3)
  metri <- sample(metri)
  lat <- 45.6 + gradi_lat(metri)
  lon <- rep(11.8, length(lat))
  esatto <- function(min_pts) {
    dbscan::dbscan(proietta_metri(lat, lon), eps = 100, minPts = min_pts)$cluster
  }

  tutti <- cluster_dbscan(lat, lon, 100, 1)
  expect_equal(dplyr::n_distinct(tutti), 3)
  expect_true(stessa_partizione(tutti, esatto(1)))
  expect_equal(dplyr::n_distinct(tutti[metri <= 120]), 1)

  # con tre letture minime il punto letto due volte è rumore, quello letto 48
  # volte no: conta il numero di letture, non di punti distinti
  con_minimo <- cluster_dbscan(lat, lon, 100, 3)
  expect_identical(con_minimo == 0, metri == 3000)
  expect_true(stessa_partizione(con_minimo, esatto(3)))
})

test_that("oltre la soglia le coordinate sono arrotondate senza cambiare i cluster", {
  expect_identical(soglia_arrotondamento_cluster(), 2000)
  # due luoghi a 400 m, 1.200 letture ciascuno con qualche metro di errore
  set.seed(4)
  n <- 1200
  lat <- 45.6 + gradi_lat(c(stats::rnorm(n, 0, 3), stats::rnorm(n, 400, 3)))
  lon <- 11.8 + gradi_lat(stats::rnorm(2 * n, 0, 3))
  cluster <- cluster_dbscan(lat, lon, 100, 1)
  expect_length(cluster, 2 * n)
  expect_equal(dplyr::n_distinct(cluster), 2)
  expect_equal(dplyr::n_distinct(cluster[1:n]), 1)
  esatto <- dbscan::dbscan(proietta_metri(lat, lon), eps = 100, minPts = 1)$cluster
  expect_true(stessa_partizione(cluster, esatto))
})
