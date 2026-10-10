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

test_that("l'attesa sulle righe della tabella copre l'elenco vuoto di un ridisegno", {
  # sul file da 2 milioni di letture l'elenco vuoto dura circa 50 millisecondi
  expect_gte(attesa_righe_tabella(), 150)
  # oltre il secondo grafici e mappa sembrerebbero non seguire la tabella
  expect_lte(attesa_righe_tabella(), 1000)
})

test_that("le viste disegnano un campione quando i cluster sono troppi", {
  analisi <- analisi_esempio()
  expect_identical(limiti_viste_cluster(), list(grafico = 20000, mappa = 5000))

  # sotto il limite restano tutti, senza avviso
  tutti <- campiona_cluster(analisi, nrow(analisi))
  expect_equal(nrow(tutti), nrow(analisi))
  expect_identical(attr(tutti, "totale"), nrow(analisi))
  expect_null(nota_campione(tutti))
  expect_null(nota_campione(analisi))

  # sopra il limite: tutti gli anomali e un campione dei regolari
  anomali <- sum(analisi$indicatore_cluster != "VALID_TARGET")
  expect_gt(anomali, 3)
  campione <- campiona_cluster(analisi, anomali + 20)
  expect_equal(nrow(campione), anomali + 20)
  expect_identical(attr(campione, "totale"), nrow(analisi))
  expect_equal(sum(campione$indicatore_cluster != "VALID_TARGET"), anomali)
  # la scelta è sempre la stessa e conserva l'ordine di partenza
  expect_identical(campione, campiona_cluster(analisi, anomali + 20))
  chiave <- function(righe) paste(righe$RFID, righe$cluster_id)
  expect_false(is.unsorted(match(chiave(campione), chiave(analisi))))
  expect_identical(
    nota_campione(campione),
    sprintf(
      "Mostrati %d cluster su %d: tutti gli anomali e un campione dei regolari. Per vederne altri restringi la tabella con i filtri.",
      anomali + 20,
      nrow(analisi)
    )
  )

  # meno posto degli anomali: restano i primi
  pochi <- campiona_cluster(analisi, 3)
  expect_equal(nrow(pochi), 3)
  expect_true(all(pochi$indicatore_cluster != "VALID_TARGET"))
})

test_that("grafico a dispersione e mappa dei cluster rispettano il limite", {
  analisi <- analisi_esempio()
  parametri <- attr(analisi, "parametri")
  anomali <- sum(analisi$indicatore_cluster != "VALID_TARGET")
  testthat::local_mocked_bindings(
    limiti_viste_cluster = function() list(grafico = 80, mappa = 70)
  )

  grafico <- plotly::plotly_build(grafico_dispersione(
    analisi,
    parametri$soglie,
    parametri$giorni_osservati
  ))
  expect_match(
    grafico$x$layout$title$text,
    sprintf("Mostrati 80 cluster su %d", nrow(analisi)),
    fixed = TRUE
  )
  # ogni cluster disegnato compare in colore una volta, nel suo riquadro
  in_colore <- Filter(
    function(traccia) identical(traccia$marker$size, 9),
    grafico$x$data
  )
  expect_equal(sum(lengths(lapply(in_colore, function(t) t$x))), 80)
  # i titoli dei riquadri contano tutti i cluster, non solo quelli disegnati
  titoli <- vapply(
    grafico$x$layout$annotations,
    function(a) a$text,
    character(1)
  )
  expect_true(any(grepl(
    sprintf("<b>VALID_TARGET</b> \u00b7 %d", nrow(analisi) - anomali),
    titoli,
    fixed = TRUE
  )))

  mappa <- mappa_cluster(analisi)
  metodi <- vapply(mappa$x$calls, function(k) k$method, character(1))
  expect_true("addControl" %in% metodi)
  nota <- Filter(function(k) k$method == "addControl", mappa$x$calls)[[1]]
  expect_match(
    nota$args[[1]],
    sprintf("Mostrati 70 cluster su %d", nrow(analisi)),
    fixed = TRUE
  )
  # argomenti di addCircleMarkers: lat, lng, radius, layerId, ...
  punti <- Filter(function(k) k$method == "addCircleMarkers", mappa$x$calls)
  identificativi <- as.integer(unlist(lapply(punti, function(k) k$args[[4]])))
  expect_length(identificativi, 70)
  expect_equal(anyDuplicated(identificativi), 0)
  # l'identificativo resta il numero di riga della tabella ricevuta: tutti gli
  # anomali sono sulla mappa
  expect_true(all(
    which(analisi$indicatore_cluster != "VALID_TARGET") %in% identificativi
  ))

  # meno posto degli anomali: la mappa si disegna lo stesso
  testthat::local_mocked_bindings(
    limiti_viste_cluster = function() list(grafico = 3, mappa = 3)
  )
  expect_s3_class(mappa_cluster(analisi), "leaflet")
  expect_s3_class(
    grafico_dispersione(analisi, parametri$soglie, parametri$giorni_osservati),
    "plotly"
  )
})
