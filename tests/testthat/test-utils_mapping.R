chiamate <- function(mappa) vapply(mappa$x$calls, function(k) k$method, character(1))
chiamata <- function(mappa, metodo) {
  Filter(function(k) k$method == metodo, mappa$x$calls)[[1]]
}

test_that("la mappa di base ha sfondo, scala e legenda", {
  mappa <- mappa_base("cluster")
  expect_s3_class(mappa, "leaflet")
  expect_identical(chiamate(mappa), c("addTiles", "addScaleBar", "addControl"))
})

test_that("la mappa principale raggruppa i marker in cluster colorati", {
  ultimi <- deduplica_ultimo_rfid(dati_esempio())
  mappa <- disegna_marker(mappa_base("cluster"), ultimi, "cluster")
  marker <- chiamata(mappa, "addMarkers")

  # argomenti di addMarkers: lat, lng, icon, layerId, group, options, popup,
  # popupOptions, clusterOptions, ...
  expect_length(marker$args[[1]], nrow(ultimi))
  expect_equal(anyDuplicated(marker$args[[4]]), 0)
  opzioni <- marker$args[[6]]
  expect_identical(opzioni$censito, ultimi$presente_a_database == "Presente")
  cluster <- marker$args[[9]]
  expect_s3_class(cluster$iconCreateFunction, "JS_EVAL")
  expect_false("addRectangles" %in% chiamate(mappa))
})

test_that("la ricerca RFID mostra tutte le letture, senza cluster, con i riquadri", {
  trovate <- cerca_per_rfid(dati_esempio(), c("RFD20250901001", "RFD20250905100"))
  mappa <- disegna_marker(mappa_base("rfid"), trovate, "rfid")
  marker <- chiamata(mappa, "addMarkers")

  expect_length(marker$args[[1]], nrow(trovate))
  expect_null(marker$args[[9]])
  opzioni <- marker$args[[6]]
  expect_identical(opzioni$opacity, ifelse(trovate$is_ultimo, 1, 0.6))

  riquadri <- chiamata(mappa, "addRectangles")
  # argomenti di addRectangles: lat1, lng1, lat2, lng2, ...
  expect_length(riquadri$args[[1]], 2)
  spostato <- which(names(split(trovate, trovate$RFID)) == "RFD20250905100")
  expect_equal(riquadri$args[[1]][[spostato]], 41.85)
  expect_equal(riquadri$args[[3]][[spostato]], 42.00)
  expect_equal(riquadri$args[[2]][[spostato]], 12.35)
  expect_equal(riquadri$args[[4]][[spostato]], 12.50)
})

test_that("i bidoni mai spostati non hanno riquadro", {
  fermo <- letture_test()
  fermo <- cerca_per_rfid(fermo, "C")
  mappa <- disegna_marker(mappa_base("rfid"), fermo, "rfid")
  expect_false("addRectangles" %in% chiamate(mappa))
  expect_true("addMarkers" %in% chiamate(mappa))
})

test_that("la ricerca utenza non usa il clustering", {
  bidoni <- filtra_ultimo_per_utenza(dati_esempio(), "UTZ025")
  mappa <- disegna_marker(mappa_base("utenza"), bidoni, "utenza")
  expect_null(chiamata(mappa, "addMarkers")$args[[9]])
})

test_that("senza dati la mappa viene solo ripulita", {
  for (df in list(NULL, dati_esempio()[0, ])) {
    mappa <- disegna_marker(mappa_base("cluster"), df, "cluster")
    expect_false("addMarkers" %in% chiamate(mappa))
    expect_true("clearMarkers" %in% chiamate(mappa))
  }
  expect_identical(adatta_vista(mappa_base("cluster"), NULL), mappa_base("cluster"))
})

test_that("la vista si adatta alle letture", {
  mappa <- adatta_vista(mappa_base("rfid"), letture_test())
  limiti <- mappa$x$fitBounds
  # fitBounds: lat1, lng1, lat2, lng2
  expect_lt(limiti[[1]], 41.70)
  expect_gt(limiti[[3]], 41.92)
  expect_lt(limiti[[2]], 12.20)
  expect_gt(limiti[[4]], 12.42)
})
