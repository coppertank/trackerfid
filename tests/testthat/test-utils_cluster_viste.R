test_that("la tabella interattiva mostra tutte le colonne con i filtri", {
  tabella <- tabella_cluster_dt(analisi_esempio())
  expect_s3_class(tabella, "datatables")
  expect_identical(names(tabella$x$data), colonne_analisi_cluster())
  expect_equal(nrow(tabella$x$data), nrow(analisi_esempio()))
  expect_identical(tabella$x$filter, "top")
  expect_true(all(tabella$x$data$analisi_dal == "01/01/2025"))
  expect_true(all(tabella$x$data$analisi_al == "31/12/2025"))
  # le colonne categoriche si filtrano da un elenco
  expect_s3_class(tabella$x$data$indicatore_cluster, "factor")
  expect_identical(
    levels(tabella$x$data$indicatore_cluster),
    intersect(
      indicatori_config()$indicatore,
      analisi_esempio()$indicatore_cluster
    )
  )
})

test_that("il grafico a barre conta i cluster per indicatore, in ordine fisso", {
  grafico <- plotly::plotly_build(grafico_indicatori(analisi_esempio()))
  barre <- grafico$x$data[[1]]
  config <- indicatori_config()
  attesi <- as.integer(table(factor(
    analisi_esempio()$indicatore_cluster,
    levels = config$indicatore
  )))

  expect_identical(barre$type, "bar")
  expect_identical(as.character(barre$y), config$indicatore)
  expect_equal(as.numeric(barre$x), attesi)
  # ogni indicatore mantiene il proprio colore
  expect_identical(as.character(barre$marker$color), config$colore)
  expect_equal(anyDuplicated(config$colore), 0)
})

test_that("il grafico a dispersione ha un riquadro per indicatore presente", {
  analisi <- analisi_esempio()
  grafico <- plotly::plotly_build(grafico_dispersione(analisi))
  presenti <- intersect(
    indicatori_config()$indicatore,
    analisi$indicatore_cluster
  )
  titoli <- vapply(
    grafico$x$layout$annotations,
    function(a) a$text,
    character(1)
  )
  for (indicatore in presenti) {
    expect_true(any(grepl(indicatore, titoli, fixed = TRUE)), info = indicatore)
  }
  # per ogni riquadro: contesto in grigio e cluster dell'indicatore in colore
  expect_length(grafico$x$data, 2 * length(presenti))
  colori <- vapply(
    grafico$x$data,
    function(traccia) traccia$marker$color[1],
    character(1)
  )
  expect_true(all(
    indicatori_config()$colore[match(
      presenti,
      indicatori_config()$indicatore
    )] %in%
      colori
  ))

  # un solo indicatore: nessuna traccia di contesto
  solo <- analisi[analisi$indicatore_cluster == "VALID_TARGET", ]
  expect_length(plotly::plotly_build(grafico_dispersione(solo))$x$data, 1)
})

test_that("la mappa mostra un baricentro e un cerchio di dispersione per cluster", {
  analisi <- analisi_esempio()
  mappa <- mappa_cluster(analisi)
  metodi <- vapply(mappa$x$calls, function(k) k$method, character(1))
  presenti <- intersect(
    indicatori_config()$indicatore,
    analisi$indicatore_cluster
  )
  expect_equal(sum(metodi == "addCircles"), length(presenti))
  expect_equal(sum(metodi == "addCircleMarkers"), length(presenti))
  expect_true("addLayersControl" %in% metodi)

  # argomenti di addCircleMarkers: lat, lng, radius, layerId, ...
  punti <- Filter(function(k) k$method == "addCircleMarkers", mappa$x$calls)
  identificativi <- unlist(lapply(punti, function(k) k$args[[4]]))
  expect_setequal(identificativi, as.character(seq_len(nrow(analisi))))

  # argomenti di addCircles: lat, lng, radius, ...; raggio pari alla dispersione
  cerchi <- Filter(function(k) k$method == "addCircles", mappa$x$calls)
  raggi <- unlist(lapply(cerchi, function(k) k$args[[3]]))
  expect_equal(max(raggi), max(analisi$cluster_dispersione_90th_m))
  expect_true(all(raggi >= 1))

  vuota <- mappa_cluster(analisi[0, ])
  expect_false(
    "addCircles" %in% vapply(vuota$x$calls, function(k) k$method, character(1))
  )
})

test_that("il popup del cluster riporta i dati principali e fa l'escape", {
  riga <- analisi_esempio()[1, ]
  riga$RFID <- "<b>X</b>"
  popup <- popup_cluster(riga)
  expect_match(popup, "&lt;b&gt;X&lt;/b&gt;", fixed = TRUE)
  expect_match(popup, "<b>Indicatore:</b>", fixed = TRUE)
  expect_match(popup, "<b>Dispersione:</b>", fixed = TRUE)
})
