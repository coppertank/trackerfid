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
})

test_that("la dispersione è il 90° percentile della distanza dal baricentro", {
  metri <- c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 1000)
  lat <- 41.9 + gradi_lat(metri)
  dispersione <- calcola_dispersione_90th(
    lat,
    rep(12.5, length(lat)),
    41.9,
    12.5
  )
  expect_equal(
    dispersione,
    as.numeric(stats::quantile(metri, 0.9)),
    tolerance = 1e-6
  )
  expect_equal(calcola_dispersione_90th(41.9, 12.5, 41.9, 12.5), 0)
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
