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
  dati <- dati_mappa_cluster(analisi)
  punti <- dati$punti
  expect_named(punti, c("x", "y", "colore", "ind", "r22", "r"))
  # un punto per cluster, nel suo baricentro
  expect_length(punti$x, nrow(analisi))
  expect_equal(punti$y / 1e5, analisi$lat_baricentro_cluster, tolerance = 1e-5)
  expect_identical(punti$r, analisi$RFID)
  # il colore è quello dell'indicatore
  config <- indicatori_config()
  expect_identical(
    punti$colore,
    config$colore[match(analisi$indicatore_cluster, config$indicatore)]
  )
  # gli indicatori presenti, nell'ordine della configurazione
  expect_identical(
    dati$indicatori$nome,
    intersect(config$indicatore, analisi$indicatore_cluster)
  )
  # il raggio del cerchio è la dispersione in metri, almeno un metro: in
  # pixel a zoom 22, dove un pixel di MapLibre misura 2 pi R cos(lat) / 512 / 2^22
  metri <- punti$r22 * 2 * pi * raggio_terra_m() *
    cos(analisi$lat_baricentro_cluster * pi / 180) / 512 / 2^22
  expect_equal(metri, pmax(analisi$cluster_dispersione_90th_m, 1), tolerance = 1e-3)
  expect_null(dati$nota)

  vuota <- dati_mappa_cluster(analisi[0, ])
  expect_length(vuota$punti$x, 0)
  expect_length(vuota$indicatori$nome, 0)
  expect_identical(dati_mappa_cluster(NULL), vuota)
  # il riquadro da inquadrare comprende tutti i baricentri
  riquadro <- riquadro_cluster(analisi)
  expect_equal(
    riquadro,
    c(
      min(analisi$lon_baricentro_cluster) - 0.002,
      min(analisi$lat_baricentro_cluster) - 0.002,
      max(analisi$lon_baricentro_cluster) + 0.002,
      max(analisi$lat_baricentro_cluster) + 0.002
    )
  )
  expect_null(riquadro_cluster(analisi[0, ]))
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
  expect_identical(limiti_viste_cluster(), list(grafico = 20000, mappa = 50000))

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

  campione <- campiona_cluster(analisi, limiti_viste_cluster()$mappa)
  dati <- dati_mappa_cluster(campione)
  expect_match(
    dati$nota,
    sprintf("Mostrati 70 cluster su %d", nrow(analisi)),
    fixed = TRUE
  )
  expect_length(dati$punti$x, 70)
  # tutti gli anomali sono sulla mappa
  expect_identical(
    sum(dati$punti$ind != "VALID_TARGET"),
    sum(analisi$indicatore_cluster != "VALID_TARGET")
  )

  # meno posto degli anomali: la mappa si disegna lo stesso
  testthat::local_mocked_bindings(
    limiti_viste_cluster = function() list(grafico = 3, mappa = 3)
  )
  expect_length(
    dati_mappa_cluster(campiona_cluster(analisi, limiti_viste_cluster()$mappa))$punti$x,
    3
  )
  expect_s3_class(
    grafico_dispersione(analisi, parametri$soglie, parametri$giorni_osservati),
    "plotly"
  )
})
