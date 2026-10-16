# Livello di una configurazione, per identificativo.
livello <- function(livelli, id) {
  Filter(function(l) identical(l$id, id), livelli)[[1]]
}
identificativi <- function(livelli) vapply(livelli, function(l) l$id, character(1))

test_that("la mappa di base ha sfondo vettoriale, zona servita e configurazione", {
  mappa <- mappa_base("cluster")
  expect_s3_class(mappa, "maplibregl")
  expect_identical(mappa$x$style, stile_mappa())
  expect_identical(stile_mappa(), "https://tiles.openfreemap.org/styles/bright")
  expect_identical(mappa$x$projection, "mercator")
  # inquadra tutta la zona servita, comuni nuovi compresi
  zona <- riquadro_zona()
  expect_equal(
    mappa$x$additional_params$bounds,
    c(zona$lng_min, zona$lat_min, zona$lng_max, zona$lat_max)
  )
  # all'avvio il browser riceve la configurazione della modalità
  avvio <- mappa$jsHooks$render[[1]]
  expect_match(avvio$code, "TrackerfidMappa.inizia", fixed = TRUE)
  expect_identical(avvio$data$modalita, "cluster")
  expect_identical(avvio$data, configurazione_mappa("cluster"))
  # lo sfondo si cambia con un'opzione
  withr::local_options(trackerfid.stile_mappa = "https://esempio.it/stile.json")
  expect_identical(mappa_base("rfid")$x$style, "https://esempio.it/stile.json")
})

test_that("se lo sfondo non si scarica la mappa ha uno sfondo di ripiego", {
  config <- configurazione_mappa("cluster")
  expect_identical(config$stile_url, stile_mappa())
  ripiego <- config$stile_ripiego
  expect_identical(ripiego, stile_ripiego())
  expect_equal(ripiego$version, 8)
  # nessuna tessera da scaricare: solo un fondo
  expect_length(ripiego$sources, 0)
  expect_identical(identificativi(ripiego$layers), "sfondo")
  expect_identical(ripiego$layers[[1]]$type, "background")
  # i numeri di cluster e bolle hanno bisogno di un indirizzo dei caratteri
  expect_match(ripiego$glyphs, "{fontstack}", fixed = TRUE)
  expect_identical(
    as.character(jsonlite::toJSON(ripiego$sources)),
    "{}"
  )
  # uno sfondo dato come lista non ha un indirizzo da controllare
  withr::local_options(trackerfid.stile_mappa = stile_ripiego())
  expect_null(configurazione_mappa("rfid")$stile_url)
})

test_that("i confini dei comuni sono colorati per cantiere e non disturbano", {
  testo <- geojson_confini()
  expect_type(testo, "character")
  expect_length(testo, 1)
  # pochi dati per il browser
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
  expect_identical(
    vapply(proprieta, function(p) p$colore, character(1)),
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

  # su ogni mappa: riempimento e bordo tratteggiato, sotto le etichette dello
  # sfondo, colorati per cantiere, e nessuno dei due risponde al mouse
  stile <- stile_confini()
  for (modalita in c("cluster", "rfid", "utenza", "giro", "analisi")) {
    config <- livelli_mappa(modalita)
    expect_identical(config$sorgenti$confini$data, "confini")
    primi <- config$livelli[1:2]
    expect_identical(identificativi(primi), c("confini-riempimento", "confini-bordo"))
    expect_true(all(vapply(primi, function(l) isTRUE(l$sotto_etichette), logical(1))))
    expect_false(any(identificativi(primi) %in% names(config$interattivi)))
  }
  config <- livelli_mappa("cluster")
  riempimento <- livello(config$livelli, "confini-riempimento")
  bordo <- livello(config$livelli, "confini-bordo")
  expect_identical(riempimento$paint[["fill-color"]], list("get", "colore"))
  expect_identical(riempimento$paint[["fill-opacity"]], stile$fillOpacity)
  expect_identical(bordo$paint[["line-width"]], stile$weight)
  expect_identical(bordo$paint[["line-dasharray"]], list(5 / 1.5, 4 / 1.5))
  # appena accennati: bordo sottile, riempimento molto trasparente
  expect_lte(stile$weight, 2)
  expect_lte(stile$fillOpacity, 0.15)
  # la configurazione porta il GeoJSON dei confini così com'è
  expect_s3_class(configurazione_mappa("rfid")$confini, "json")
})

test_that("ogni combinazione di marker ha un'icona", {
  icone <- icone_mappa()
  expect_false(anyDuplicated(icone$id) > 0)
  # due stati, tre stili, tre combinazioni di segnali, tutti i glifi
  glifi <- unique(paste(icone$icona, icone$colore))
  expect_equal(nrow(icone), 2 * 3 * 3 * length(glifi))
  expect_true("question" %in% icone$icona)
  expect_true(all(startsWith(icone$svg, "<svg")))
  # preparate una volta sola
  expect_identical(icone_mappa(), icone)

  # il numero dell'icona parte da 0, come nell'elenco del browser
  k <- indice_icona(
    c("CARTA", "SECCO PAP", NA, "NOME IGNOTO", servizio_sacchetti()),
    c("Presente", "Presente", "Non Presente", "Non Presente", "Presente")
  )
  expect_false(anyNA(k))
  scelte <- icone[k + 1, ]
  expect_identical(scelte$presente, c("Presente", "Presente", "Non Presente", "Non Presente", "Presente"))
  expect_identical(scelte$icona[1], info_servizio("CARTA")$icona)
  # un giro del calendario ha l'icona della sua tipologia
  expect_identical(scelte$icona[2], info_servizio("SECCO")$icona)
  # un servizio mancante o sconosciuto ha il punto di domanda
  expect_identical(scelte$icona[3:4], c("question", "question"))
  expect_identical(scelte$icona[5], info_servizio(servizio_sacchetti())$icona)
  # stile e segnali
  segnati <- icone[
    indice_icona("CARTA", "Presente", "ultimo", fuori_comune = TRUE) + 1,
  ]
  expect_identical(segnati$stile, "ultimo")
  expect_true(segnati$fuori_comune)
  expect_false(segnati$fuori_giro)
  expect_identical(
    segnati$svg,
    svg_marker("CARTA", "Presente", "ultimo", fuori_comune = TRUE)
  )
  # il marker di un non censito resta rosso anche con l'icona del servizio
  expect_match(icone$svg[indice_icona("VETRO", "Non Presente") + 1], "#e74c3c", fixed = TRUE)
})

test_that("la mappa principale ha le bolle del territorio e i cluster colorati", {
  config <- livelli_mappa("cluster")
  expect_named(config$sorgenti, c("confini", "bidoni", "bolle_cantiere", "bolle_comune"))
  bidoni <- config$sorgenti$bidoni
  expect_true(bidoni$cluster)
  expect_identical(bidoni$clusterProperties, list(censiti = list("+", list("get", "c"))))
  soglie <- soglie_zoom()
  # una fascia di zoom per livello: cantieri, comuni, bidoni
  expect_equal(livello(config$livelli, "bolle-cantiere")$maxzoom, soglie$cantiere + 1)
  expect_equal(livello(config$livelli, "bolle-comune")$minzoom, soglie$cantiere + 1)
  expect_equal(livello(config$livelli, "bolle-comune")$maxzoom, soglie$comune + 1)
  expect_equal(livello(config$livelli, "cluster")$minzoom, soglie$comune + 1)
  expect_equal(livello(config$livelli, "bidoni")$minzoom, soglie$comune + 1)
  # i cluster hanno i loro conteggi, i bidoni no
  expect_identical(livello(config$livelli, "cluster")$filter, list("has", "point_count"))
  expect_identical(
    livello(config$livelli, "bidoni")$filter,
    list("!", list("has", "point_count"))
  )
  # il colore va dal rosso al verde con la quota di censiti, come prima
  colore <- livello(config$livelli, "cluster")$paint[["circle-color"]]
  colori <- colori_stato()
  expect_identical(colore[[1]], "interpolate")
  expect_identical(unlist(colore[c(5, 7, 9)]), c(colori$non_presente, colori$misto, colori$presente))
  # il raggio cresce con i bidoni come il lato delle bolle
  raggio <- livello(config$livelli, "cluster")$paint[["circle-radius"]]
  expect_equal(raggio[[3]], lato_bolla(1) / 2 - 2.5)
  expect_equal(raggio[[length(raggio)]], lato_bolla(1e5) / 2 - 2.5)
  # il nome del cantiere resta sotto la bolla
  expect_identical(
    livello(config$livelli, "bolle-cantiere-nome")$layout[["text-field"]],
    list("get", "nome")
  )
  # click: un bidone apre il dettaglio, un cluster e una bolla si aprono
  expect_identical(
    vapply(config$interattivi, function(r) r$click, character(1)),
    c("bolle-cantiere" = "bolla", "bolle-comune" = "bolla", cluster = "cluster", bidoni = "punto")
  )
})

test_that("ricerche, giri e cluster spaziali hanno i loro livelli", {
  rfid <- livelli_mappa("rfid")
  expect_identical(identificativi(rfid$livelli)[-(1:2)], c("riquadri", "letture"))
  marker <- livello(rfid$livelli, "letture")
  # l'ultima lettura sta sopra le altre, che sono attenuate
  expect_identical(marker$layout[["symbol-sort-key"]], list("get", "u"))
  expect_identical(marker$paint[["icon-opacity"]][[4]], 0.6)
  # senza cluster
  expect_null(rfid$sorgenti$letture$cluster)
  # la punta del marker cade sul punto della lettura
  expect_identical(marker$layout[["icon-anchor"]], "bottom")

  utenza <- livelli_mappa("utenza")
  expect_identical(identificativi(utenza$livelli)[-(1:2)], "letture")
  expect_null(livello(utenza$livelli, "letture")$paint)

  giro <- livelli_mappa("giro")
  expect_identical(identificativi(giro$livelli)[-(1:2)], c("comuni-giro", "letture"))
  expect_identical(
    livello(giro$livelli, "comuni-giro")$paint[["line-color"]],
    colori_stato()$bordo
  )

  analisi <- livelli_mappa("analisi")
  expect_identical(identificativi(analisi$livelli)[-(1:2)], c("dispersione", "baricentri"))
  # il raggio della dispersione è in metri: raddoppia a ogni livello di zoom
  raggio <- livello(analisi$livelli, "dispersione")$paint[["circle-radius"]]
  expect_identical(raggio[[2]], list("exponential", 2))
  expect_identical(names(analisi$interattivi), "baricentri")
})

test_that("i punti viaggiano in colonne compatte", {
  trovate <- cerca_per_rfid(
    seleziona_periodo(archivio_esempio()),
    "RFD20250905100"
  )
  punti <- punti_mappa(trovate, "rfid")
  expect_named(punti, c("x", "y", "k", "c", "r", "u"))
  # coordinate intere, in centomillesimi di grado
  expect_type(punti$x, "integer")
  expect_equal(punti$x / 1e5, trovate$longitudine, tolerance = 1e-5)
  expect_equal(punti$y / 1e5, trovate$latitudine, tolerance = 1e-5)
  expect_identical(punti$u, as.integer(trovate$is_ultimo))
  expect_identical(punti$r, trovate$RFID)
  expect_true(all(punti$c == 1L))
  # l'icona dell'ultima lettura ha il bordo spesso, le altre tratteggiato
  icone <- icone_mappa()
  expect_identical(icone$stile[punti$k + 1], ifelse(trovate$is_ultimo, "ultimo", "precedente"))
  # le letture fuori comune hanno il segnale C
  expect_identical(
    icone$fuori_comune[punti$k + 1],
    trovate$coerenza_comune == "Fuori comune"
  )
  # fuori dalla ricerca RFID niente `u`
  expect_named(punti_mappa(trovate, "utenza"), c("x", "y", "k", "c", "r"))

  # l'icona di un non censito segue il servizio stimato
  letture <- dplyr::tibble(
    RFID = c("N1", "N2"),
    presente_a_database = "Non Presente",
    servizio_icona = c("VETRO PAP", NA),
    latitudine = c(41.9, 41.91),
    longitudine = c(12.5, 12.51)
  )
  k <- punti_mappa(letture, "utenza")$k
  expect_identical(icone$icona[k + 1], c(info_servizio("VETRO")$icona, "question"))
  expect_identical(icone$presente[k + 1], rep("Non Presente", 2))
})

test_that("la mappa principale riceve bidoni e bolle di cantieri e comuni", {
  ultimi <- ultime_esempio(periodo = periodo_anno(2025))
  dati <- dati_mappa(ultimi, "cluster")
  expect_named(dati, c("punti", "bolle_cantiere", "bolle_comune", "territorio"))
  expect_length(dati$punti$x, nrow(ultimi))
  expect_true(dati$territorio)
  cantieri <- dati$bolle_cantiere
  expect_setequal(cantieri$nome, cantieri())
  expect_equal(sum(cantieri$n), nrow(ultimi))
  expect_true(all(cantieri$livello == "cantiere"))
  expect_match(cantieri$d[cantieri$nome == "BASSANO"], "^BASSANO: 64 bidoni, \\d+ censiti$")
  # la bolla di un comune sta nel baricentro del comune
  comuni <- dati$bolle_comune
  baricentri <- baricentri_comuni()
  riga <- match(comuni$nome, baricentri$comune)
  expect_equal(comuni$y / 1e5, baricentri$latitudine[riga], tolerance = 1e-5)
  # il riquadro dei bidoni, per il click
  bolle <- aggrega_marker(ultimi, "cantiere")
  expect_identical(cantieri$ovest, bolle$lng_min)
  expect_identical(cantieri$nord, bolle$lat_max)

  # senza cantieri né comuni le bolle non servono
  senza <- dati_mappa(ultime_di(letture_test()), "cluster")
  expect_false(senza$territorio)
  # senza bidoni: elenchi vuoti
  vuota <- dati_mappa(NULL, "cluster")
  expect_length(vuota$punti$x, 0)
  expect_length(vuota$bolle_cantiere$x, 0)
  expect_false(vuota$territorio)
  expect_identical(names(dati_mappa(ultimi[0, ], "utenza")), "punti")
})

test_that("la ricerca RFID riceve i riquadri degli spostamenti", {
  selezione <- seleziona_periodo(archivio_esempio())
  trovate <- cerca_per_rfid(selezione, c("RFD20250905100", "RFD20250901050"))
  dati <- dati_mappa(trovate, "rfid")
  riquadri <- dati$riquadri
  spostato <- trovate[trovate$RFID == "RFD20250905100", ]
  riga <- match("RFD20250905100", riquadri$r)
  expect_equal(c(riquadri$sud[riga], riquadri$nord[riga]), range(spostato$latitudine))
  expect_equal(c(riquadri$ovest[riga], riquadri$est[riga]), range(spostato$longitudine))
  # un RFID letto sempre nello stesso punto non ha riquadro
  fermo <- letture_test()[letture_test()$RFID == "C", ]
  expect_length(riquadri_rfid(fermo)$r, 0)
  # i riquadri coprono tutte le letture, anche quando i marker sono limitati
  poche <- limita_letture_ricerca(trovate, massimo = 3)
  expect_identical(dati_mappa(poche, "rfid", riquadri = trovate)$riquadri, riquadri)
  expect_length(dati_mappa(NULL, "rfid")$riquadri$r, 0)
})

test_that("la mappa dei giri riceve il contorno dei comuni dei giri", {
  selezione <- seleziona_periodo(archivio_esempio())
  letture <- cerca_giri(selezione, "VEH001", "2025-09-24", "2025-09-24")
  dati <- dati_mappa(letture, "giro")
  expect_length(dati$punti$x, nrow(letture))
  # il comune del giro è Limena: un anello
  limena <- lati_comuni()[[indice_comune("LIMENA")]]
  expect_length(dati$comuni_giro, max(limena$anello))
  expect_equal(dati$comuni_giro[[1]]$x, round(limena$x[limena$anello == 1], 5))

  # con troppi comuni il contorno non viene disegnato; i comuni fuori tabella
  # e quelli mancanti non contano
  expect_length(contorni_comuni_giro(comuni_cantieri()$comune), 0)
  expect_length(contorni_comuni_giro(c("MILANO", NA)), 0)
  expect_length(contorni_comuni_giro(NULL), 0)
  # Valbrenta ha due poligoni: restano due anelli separati
  due <- contorni_comuni_giro(c("Valbrenta", "ASIAGO", "ASIAGO"))
  valbrenta <- lati_comuni()[[indice_comune("VALBRENTA")]]
  asiago <- lati_comuni()[[indice_comune("ASIAGO")]]
  expect_length(due, max(valbrenta$anello) + max(asiago$anello))
  expect_gte(max(valbrenta$anello), 2)
})

test_that("la vista si adatta ai punti", {
  d <- letture_test()
  riquadro <- riquadro_vista(d)
  expect_equal(
    riquadro,
    c(min(d$longitudine), min(d$latitudine), max(d$longitudine), max(d$latitudine)) +
      c(-1, -1, 1, 1) * 0.0015
  )
  expect_null(riquadro_vista(NULL))
  expect_null(riquadro_vista(d[0, ]))
})

test_that("il popup di una lettura riporta comune e cantiere", {
  spostato <- cerca_per_rfid(seleziona_periodo(archivio_esempio()), "RFD20250905100")
  popup <- vapply(seq_len(nrow(spostato)), function(i) popup_lettura(spostato, i), character(1))
  expect_true(all(grepl("<b>Comune a database:</b> LIMENA", popup, fixed = TRUE)))
  # il cantiere è quello del comune di lettura
  expect_true(any(grepl("<b>Cantiere:</b> RUBANO", popup, fixed = TRUE)))
  expect_true(any(grepl("<b>Cantiere:</b> CAMPOSAMPIERO", popup, fixed = TRUE)))
  # dopo lo spostamento viene letto a Cittadella, dal giro di Cittadella: il
  # comune del giro è quello di lettura e non viene ripetuto
  expect_equal(sum(grepl("<b>Comune di lettura:</b> CITTADELLA", popup, fixed = TRUE)), 8)
  expect_equal(sum(grepl("<b>Comune di lettura:</b> LIMENA", popup, fixed = TRUE)), 1)
  expect_false(any(grepl("Comune del giro", popup, fixed = TRUE)))
  expect_equal(
    sum(grepl("Fuori comune:</b> a database LIMENA, letto a CITTADELLA", popup, fixed = TRUE)),
    sum(spostato$coerenza_comune == "Fuori comune")
  )
  expect_false(any(grepl("Fuori giro", popup, fixed = TRUE)))
})

test_that("i messaggi alla mappa hanno identificativo e tipo", {
  inviati <- list()
  sessione <- list(sendCustomMessage = function(tipo, messaggio) {
    inviati[[length(inviati) + 1]] <<- list(tipo = tipo, messaggio = messaggio)
  })
  invia_mappa(sessione, "mappa_rfid-mappa", "inquadra", riquadro = 1:4)
  expect_identical(inviati[[1]]$tipo, "trackerfid-mappa")
  expect_identical(
    inviati[[1]]$messaggio,
    list(id = "mappa_rfid-mappa", tipo = "inquadra", riquadro = 1:4)
  )
})
