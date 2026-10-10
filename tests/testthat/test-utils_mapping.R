chiamate <- function(mappa) {
  vapply(mappa$x$calls, function(k) k$method, character(1))
}
chiamata <- function(mappa, metodo) {
  Filter(function(k) k$method == metodo, mappa$x$calls)[[1]]
}

test_that("la mappa di base ha sfondo, confini dei comuni, scala e legenda", {
  mappa <- mappa_base("cluster")
  expect_s3_class(mappa, "leaflet")
  expect_identical(
    chiamate(mappa),
    c("addTiles", "createMapPane", "addGeoJSON", "addScaleBar", "addControl")
  )
  # inquadra tutta la zona servita, comuni nuovi compresi
  zona <- riquadro_zona()
  expect_equal(
    unlist(mappa$x$fitBounds[1:4]),
    c(zona$lat_min, zona$lng_min, zona$lat_max, zona$lng_max)
  )
})

test_that("i confini dei comuni sono colorati per cantiere e non disturbano", {
  testo <- geojson_confini()
  expect_type(testo, "character")
  expect_length(testo, 1)
  # pochi dati per il browser: la mappa li riceve a ogni disegno
  expect_lt(nchar(testo), 200000)
  # preparati una volta sola
  expect_identical(geojson_confini(), testo)

  confini <- jsonlite::fromJSON(testo, simplifyVector = FALSE)
  expect_identical(confini$type, "FeatureCollection")
  comuni <- comuni_cantieri()
  proprieta <- lapply(confini$features, function(elemento) elemento$properties)
  expect_identical(
    vapply(proprieta, function(p) p$comune, character(1)),
    comuni$comune
  )
  expect_identical(
    vapply(proprieta, function(p) p$cantiere, character(1)),
    comuni$cantiere
  )
  # bordo e riempimento hanno il colore del cantiere
  expect_identical(
    vapply(proprieta, function(p) p$style$color, character(1)),
    colore_cantiere(comuni$cantiere)
  )
  expect_identical(
    vapply(proprieta, function(p) p$style$fillColor, character(1)),
    colore_cantiere(comuni$cantiere)
  )
  tipi <- vapply(
    confini$features,
    function(elemento) elemento$geometry$type,
    character(1)
  )
  expect_true(all(tipi %in% c("Polygon", "MultiPolygon")))
  # coordinate con cinque decimali, dentro la zona servita
  coordinate <- unlist(lapply(confini$features, function(elemento) {
    elemento$geometry$coordinates
  }))
  expect_equal(coordinate, round(coordinate, 5))
  zona <- riquadro_zona()
  expect_gte(min(coordinate), zona$lng_min - 1e-5)
  expect_lte(max(coordinate), zona$lat_max + 1e-5)

  # sulla mappa: un livello a parte, sotto i marker, che non risponde al mouse
  mappa <- aggiungi_confini(leaflet::leaflet())
  riquadro <- chiamata(mappa, "createMapPane")
  expect_identical(riquadro$args[[1]], "confini")
  # i marker di Leaflet stanno a 600, riquadri e cerchi a 400
  expect_lt(riquadro$args[[2]], 400)
  disegno <- chiamata(mappa, "addGeoJSON")
  expect_identical(disegno$args[[1]], testo)
  expect_identical(disegno$args[[3]], "confini")
  opzioni <- disegno$args[[4]]
  expect_identical(opzioni$pane, "confini")
  expect_false(opzioni$interactive)
  stile <- stile_confini()
  expect_identical(opzioni$weight, stile$weight)
  expect_identical(opzioni$fillOpacity, stile$fillOpacity)
  expect_identical(opzioni$dashArray, stile$dashArray)
  # appena accennati: bordo sottile, riempimento molto trasparente
  expect_lte(stile$weight, 2)
  expect_lte(stile$fillOpacity, 0.15)

  # ridisegnare i marker non toglie i confini
  ripulita <- pulisci_mappa(mappa_base("rfid"))
  expect_false("clearGeoJSON" %in% chiamate(ripulita))
  expect_false("clearGroup" %in% chiamate(ripulita))
  # anche la mappa dei cluster spaziali ha i confini
  expect_true("addGeoJSON" %in% chiamate(mappa_cluster(NULL)))
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
  trovate <- cerca_per_rfid(
    dati_esempio(),
    c("RFD20250901001", "RFD20250905100")
  )
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
  # da Limena, a sud-est, a Cittadella, a nord-ovest
  expect_equal(riquadri$args[[1]][[spostato]], 45.4744)
  expect_equal(riquadri$args[[3]][[spostato]], 45.6488, tolerance = 1e-4)
  expect_equal(riquadri$args[[2]][[spostato]], 11.7836, tolerance = 1e-4)
  expect_equal(riquadri$args[[4]][[spostato]], 11.8449)
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
  expect_identical(
    adatta_vista(mappa_base("cluster"), NULL),
    mappa_base("cluster")
  )
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

test_that("l'icona dei non censiti segue il servizio atteso prevalente", {
  letture <- dplyr::tibble(
    giorno_lettura = as.POSIXct("2025-09-15 08:00:00", tz = "UTC"),
    targa_veicolo = "AB123CD",
    matricola_veicolo = "VEH001",
    RFID = c("N1", "N2"),
    presente_a_database = "Non Presente",
    servizio_transponder = NA_character_,
    servizio_atteso = c("VETRO PAP", NA),
    id_utenza = NA_character_,
    latitudine = c(41.9, 41.91),
    longitudine = c(12.5, 12.51),
    servizio_icona = c("VETRO PAP", NA)
  )
  icone <- chiamata(
    disegna_marker(mappa_base("utenza"), letture, "utenza"),
    "addMarkers"
  )$args[[3]]
  url <- icone$iconUrl$data[icone$iconUrl$index + 1]
  attese <- get_leaflet_icon(c("VETRO PAP", NA), "Non Presente")$iconUrl
  expect_identical(url, attese)
  expect_false(identical(url[1], url[2]))
  # il marker resta rosso anche con l'icona del servizio
  expect_match(utils::URLdecode(url[1]), "#e74c3c", fixed = TRUE)

  # senza la colonna dedicata si usa il servizio transponder
  senza <- letture[, setdiff(names(letture), "servizio_icona")]
  icone <- chiamata(
    disegna_marker(mappa_base("utenza"), senza, "utenza"),
    "addMarkers"
  )$args[[3]]
  expect_length(icone$iconUrl$data, 1)
})

test_that("la mappa di base inquadra la zona servita", {
  zona <- riquadro_zona()
  limiti <- mappa_base("cluster")$x$fitBounds
  # fitBounds: lat1, lng1, lat2, lng2
  expect_equal(
    unlist(limiti[1:4]),
    c(zona$lat_min, zona$lng_min, zona$lat_max, zona$lng_max)
  )
})

test_that("le bolle della vista aggregata hanno identificativo ed etichetta", {
  ultimi <- deduplica_ultimo_rfid(dati_esempio())
  per_cantiere <- aggrega_marker(ultimi, "cantiere")
  bolle <- chiamata(
    aggiungi_bolle(mappa_base("cluster"), per_cantiere),
    "addMarkers"
  )
  # argomenti di addMarkers: lat, lng, icon, layerId, group, options, popup,
  # popupOptions, clusterOptions, clusterId, label, labelOptions
  expect_equal(bolle$args[[1]], per_cantiere$latitudine)
  expect_equal(bolle$args[[2]], per_cantiere$longitudine)
  # l'identificativo distingue una bolla da un marker
  expect_identical(bolle$args[[4]], paste0("bolla-", seq_len(nrow(per_cantiere))))
  # le bolle sono già gruppi: il browser non le raggruppa ancora
  expect_null(bolle$args[[9]])
  # il nome di un cantiere resta sempre visibile
  expect_identical(
    bolle$args[[11]],
    dplyr::coalesce(per_cantiere$nome, senza_cantiere())
  )
  expect_true(bolle$args[[12]]$permanent)

  # le altre bolle descrivono il gruppo al passaggio del mouse
  per_comune <- aggrega_marker(ultimi, "comune")
  bolle <- chiamata(
    aggiungi_bolle(mappa_base("cluster"), per_comune),
    "addMarkers"
  )
  expect_false(bolle$args[[12]]$permanent)
  asiago <- which(per_comune$nome == "ASIAGO")
  expect_identical(
    bolle$args[[11]][asiago],
    sprintf(
      "ASIAGO: %d bidoni, %d censiti",
      per_comune$n[asiago],
      per_comune$censiti[asiago]
    )
  )
  griglia <- aggrega_marker(ultimi, "griglia", 15)
  bolle <- chiamata(
    aggiungi_bolle(mappa_base("cluster"), griglia),
    "addMarkers"
  )
  uno <- which(griglia$n == 1)[1]
  expect_match(bolle$args[[11]][uno], "^Zona: 1 bidone, [01] censiti$")

  # nessuna bolla: la mappa resta com'è
  expect_identical(
    aggiungi_bolle(mappa_base("cluster"), per_cantiere[0, ]),
    mappa_base("cluster")
  )
})

test_that("i riquadri della ricerca coprono tutte le letture anche con i marker limitati", {
  trovate <- cerca_per_rfid(dati_esempio(), "RFD20250905100")
  poche <- limita_letture_ricerca(trovate, 3)
  mappa <- disegna_marker(mappa_base("rfid"), poche, "rfid", riquadri = trovate)
  expect_length(chiamata(mappa, "addMarkers")$args[[1]], 3)
  # addRectangles: lat1, lng1, lat2, lng2
  riquadro <- chiamata(mappa, "addRectangles")
  expect_equal(
    c(riquadro$args[[1]], riquadro$args[[3]]),
    range(trovate$latitudine),
    ignore_attr = TRUE
  )
  expect_equal(
    c(riquadro$args[[2]], riquadro$args[[4]]),
    range(trovate$longitudine),
    ignore_attr = TRUE
  )
})

test_that("il popup dei marker riporta comune e cantiere della lettura", {
  spostato <- cerca_per_rfid(dati_esempio(), "RFD20250905100")
  popup <- chiamata(
    disegna_marker(mappa_base("rfid"), spostato, "rfid"),
    "addMarkers"
  )$args[[7]]
  expect_length(popup, nrow(spostato))
  expect_true(all(grepl("<b>Comune:</b> LIMENA", popup, fixed = TRUE)))
  # il cantiere è quello del comune in cui cade la lettura
  expect_true(any(grepl("<b>Cantiere:</b> RUBANO", popup, fixed = TRUE)))
  expect_true(any(grepl("<b>Cantiere:</b> CAMPOSAMPIERO", popup, fixed = TRUE)))
  # dopo lo spostamento lo legge il giro di un altro comune: la lettura è
  # fuori comune, e il marker porta il segnale
  expect_true(any(grepl(
    "<b>Comune del giro:</b> CITTADELLA",
    popup,
    fixed = TRUE
  )))
  expect_equal(
    sum(grepl(
      "Fuori comune:</b> a database LIMENA, letto a CITTADELLA",
      popup,
      fixed = TRUE
    )),
    sum(spostato$fuori_comune)
  )
  expect_false(any(grepl("Fuori giro", popup, fixed = TRUE)))
  icone <- chiamata(
    disegna_marker(mappa_base("utenza"), spostato, "utenza"),
    "addMarkers"
  )$args[[3]]$iconUrl
  # due disegni: senza segnale e con il segnale del fuori comune
  expect_length(icone$data, 2)
  coerente <- icone$index[!spostato$fuori_comune][1]
  expect_identical(icone$index != coerente, spostato$fuori_comune)
})

test_that("la mappa dei giri disegna le letture e mette in evidenza i comuni dei giri", {
  letture <- cerca_giri(dati_esempio(), "VEH001", "2025-09-24", "2025-09-24")
  mappa <- disegna_marker(mappa_base("giro"), letture, "giro")
  marker <- chiamata(mappa, "addMarkers")
  expect_length(marker$args[[1]], nrow(letture))
  # senza cluster
  expect_null(marker$args[[9]])
  # il confine del comune del giro, un contorno senza riempimento
  contorni <- Filter(function(k) k$method == "addPolygons", mappa$x$calls)
  expect_length(contorni, dplyr::n_distinct(letture$comune_lettura))
  expect_identical(contorni[[1]]$args[[3]], "comuni_giro")
  expect_false(contorni[[1]]$args[[4]]$fill)
  expect_match(contorni[[1]]$args[[7]], "Comune del giro: LIMENA", fixed = TRUE)

  # con troppi comuni il contorno non viene disegnato; i comuni fuori tabella
  # e quelli mancanti non contano
  vuota <- leaflet::leaflet()
  expect_false(
    "addPolygons" %in% chiamate(aggiungi_comuni_giro(vuota, comuni_cantieri()$comune))
  )
  expect_false("addPolygons" %in% chiamate(aggiungi_comuni_giro(vuota, c("MILANO", NA))))
  expect_false("addPolygons" %in% chiamate(aggiungi_comuni_giro(vuota, NULL)))
  due <- aggiungi_comuni_giro(vuota, c("Valbrenta", "ASIAGO", "ASIAGO"))
  expect_equal(sum(chiamate(due) == "addPolygons"), 2)
  # Valbrenta ha due poligoni: restano separati
  valbrenta <- chiamata(due, "addPolygons")$args[[1]]
  expect_length(valbrenta, 2)
  # ridisegnare toglie i contorni: sono forme
  expect_true("clearShapes" %in% chiamate(pulisci_mappa(due)))
})
