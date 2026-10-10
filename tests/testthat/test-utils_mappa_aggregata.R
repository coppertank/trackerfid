# Area che contiene tutta la zona servita, nella forma comunicata da Leaflet.
tutta_la_zona <- function() {
  zona <- riquadro_zona()
  list(
    north = zona$lat_max,
    south = zona$lat_min,
    east = zona$lng_max,
    west = zona$lng_min
  )
}

# Area di pochi chilometri attorno al centro di un comune.
attorno_a <- function(comune) {
  centro <- comuni_cantieri()[comuni_cantieri()$comune == comune, ]
  list(
    north = centro$latitudine + 0.02,
    south = centro$latitudine - 0.02,
    east = centro$longitudine + 0.03,
    west = centro$longitudine - 0.03
  )
}

test_that("i limiti delle mappe si cambiano con le opzioni", {
  withr::local_options(
    trackerfid.max_marker = NULL,
    trackerfid.max_letture_ricerca = NULL
  )
  expect_identical(
    limiti_mappa(),
    list(marker = 10000, letture_ricerca = 3000)
  )
  withr::local_options(
    trackerfid.max_marker = 50,
    trackerfid.max_letture_ricerca = 7
  )
  expect_identical(limiti_mappa(), list(marker = 50, letture_ricerca = 7))
})

test_that("il livello di aggregazione segue lo zoom", {
  expect_identical(livello_aggregazione(NULL), "cantiere")
  expect_identical(
    vapply(c(8, 10, 11, 12, 13, 17), livello_aggregazione, character(1)),
    c("cantiere", "cantiere", "comune", "comune", "griglia", "griglia")
  )
  # una cella della griglia occupa 90 pixel e dimezza a ogni livello di zoom
  expect_equal(passo_griglia(0), 360 * 90 / 256)
  expect_equal(passo_griglia(13) / passo_griglia(14), 2)
})

test_that("i riquadri si allargano, si contengono e selezionano i bidoni", {
  riquadro <- list(north = 46, south = 45, east = 12, west = 11)
  largo <- allarga_riquadro(riquadro, 0.5)
  expect_identical(
    largo,
    list(north = 46.5, south = 44.5, east = 12.5, west = 10.5)
  )
  # il margine predefinito è il 30% dell'ampiezza
  expect_equal(allarga_riquadro(riquadro)$north, 46.3)
  expect_equal(allarga_riquadro(riquadro)$west, 10.7)

  expect_true(riquadro_contiene(largo, riquadro))
  expect_true(riquadro_contiene(riquadro, riquadro))
  expect_false(riquadro_contiene(riquadro, largo))
  expect_false(riquadro_contiene(NULL, riquadro))
  expect_false(riquadro_contiene(riquadro, NULL))

  punti <- data.frame(
    latitudine = c(45.5, 46, 46.1, 45.5),
    longitudine = c(11.5, 12, 11.5, 12.2)
  )
  expect_identical(nel_riquadro(punti, riquadro), c(TRUE, TRUE, FALSE, FALSE))
  # senza riquadro passano tutti
  expect_identical(nel_riquadro(punti, NULL), rep(TRUE, 4))
})

test_that("le bolle contano bidoni e censiti di ogni gruppo", {
  ultimi <- deduplica_ultimo_rfid(dati_esempio())
  per_cantiere <- aggrega_marker(ultimi, "cantiere")
  expect_named(
    per_cantiere,
    c(
      "livello",
      "nome",
      "latitudine",
      "longitudine",
      "n",
      "censiti",
      "lat_min",
      "lat_max",
      "lng_min",
      "lng_max"
    )
  )
  expect_true(all(per_cantiere$livello == "cantiere"))
  # i quattro cantieri: nel dataset di esempio ogni bidone ne ha uno
  expect_setequal(per_cantiere$nome, cantieri())
  expect_equal(sum(per_cantiere$n), nrow(ultimi))
  # i bidoni senza cantiere formano una bolla con il nome mancante
  senza <- ultimi
  senza$cantiere[senza$RFID == "RFD20241001301"] <- NA
  expect_setequal(aggrega_marker(senza, "cantiere")$nome, c(cantieri(), NA))
  expect_equal(
    sum(per_cantiere$censiti),
    sum(ultimi$presente_a_database == "Presente")
  )

  # una bolla sta nel punto medio dei suoi bidoni e ne riporta il riquadro
  bassano <- per_cantiere[which(per_cantiere$nome == "BASSANO"), ]
  suoi <- ultimi[which(ultimi$cantiere == "BASSANO"), ]
  expect_equal(bassano$n, 69)
  expect_equal(bassano$censiti, sum(suoi$presente_a_database == "Presente"))
  expect_equal(bassano$latitudine, mean(suoi$latitudine))
  expect_equal(bassano$longitudine, mean(suoi$longitudine))
  expect_equal(c(bassano$lat_min, bassano$lat_max), range(suoi$latitudine))
  expect_equal(c(bassano$lng_min, bassano$lng_max), range(suoi$longitudine))

  per_comune <- aggrega_marker(ultimi, "comune")
  expect_equal(nrow(per_comune), dplyr::n_distinct(ultimi$comune_assegnato))
  expect_equal(sum(per_comune$n), nrow(ultimi))
  expect_equal(
    per_comune$n[which(per_comune$nome == "BASSANO DEL GRAPPA")],
    sum(ultimi$comune_assegnato == "BASSANO DEL GRAPPA", na.rm = TRUE)
  )

  # la griglia si infittisce con lo zoom e non ha nomi
  larga <- aggrega_marker(ultimi, "griglia", zoom = 9)
  fitta <- aggrega_marker(ultimi, "griglia", zoom = 15)
  expect_lt(nrow(larga), nrow(fitta))
  expect_true(all(is.na(fitta$nome)))
  expect_equal(sum(larga$n), nrow(ultimi))
  expect_equal(sum(fitta$n), nrow(ultimi))
  expect_true(all(
    fitta$lat_min <= fitta$latitudine & fitta$latitudine <= fitta$lat_max
  ))

  # nessun bidone: nessuna bolla, stesse colonne
  vuote <- aggrega_marker(ultimi[0, ], "comune")
  expect_equal(nrow(vuote), 0)
  expect_named(vuote, names(per_comune))

  # senza colonne geografiche i bidoni formano una sola bolla senza nome
  senza <- aggrega_marker(deduplica_ultimo_rfid(letture_test()), "cantiere")
  expect_equal(nrow(senza), 1)
  expect_true(is.na(senza$nome))
  expect_equal(senza$n, 3)
  expect_equal(senza$censiti, 2)

  expect_error(aggrega_marker(ultimi, "provincia"))
})

test_that("la vista dipende dal numero di bidoni, dall'area e dallo zoom", {
  ultimi <- deduplica_ultimo_rfid(dati_esempio())
  tutta <- tutta_la_zona()

  # sotto il limite: tutti i bidoni, raggruppati dal browser
  expect_identical(scegli_vista(ultimi, tutta, 9), list(tipo = "tutti"))
  expect_identical(
    scegli_vista(ultimi, tutta, 9, limite = nrow(ultimi)),
    list(tipo = "tutti")
  )
  expect_identical(scegli_vista(NULL, tutta, 9), list(tipo = "tutti"))

  # sopra il limite: bolle secondo lo zoom
  expect_identical(
    scegli_vista(ultimi, tutta, 9, limite = 50),
    list(tipo = "cantiere")
  )
  expect_identical(
    scegli_vista(ultimi, tutta, 12, limite = 50),
    list(tipo = "comune")
  )
  griglia <- scegli_vista(ultimi, tutta, 14, limite = 50)
  expect_identical(griglia$tipo, "griglia")
  expect_identical(griglia$riquadro, allarga_riquadro(tutta))
  expect_equal(sum(griglia$righe), nrow(ultimi))

  # senza area inquadrata i singoli bidoni non si possono scegliere
  expect_identical(
    scegli_vista(ultimi, NULL, 15, limite = 50)$tipo,
    "griglia"
  )
  expect_identical(scegli_vista(ultimi, NULL, 9, limite = 50)$tipo, "cantiere")

  # un'area con pochi bidoni mostra i singoli, a qualsiasi zoom
  piccola <- attorno_a("ASIAGO")
  singoli <- scegli_vista(ultimi, piccola, 9, limite = 50)
  expect_identical(singoli$tipo, "singoli")
  expect_identical(singoli$riquadro, allarga_riquadro(piccola))
  expect_identical(
    singoli$righe,
    nel_riquadro(ultimi, allarga_riquadro(piccola))
  )
  expect_gt(sum(singoli$righe), 0)
  expect_lte(sum(singoli$righe), 50)

  # un dataset senza cantieri e comuni usa la griglia a ogni zoom
  senza <- deduplica_ultimo_rfid(letture_test())
  expect_identical(scegli_vista(senza, NULL, 9, limite = 2)$tipo, "griglia")
  expect_identical(scegli_vista(senza, NULL, 12, limite = 2)$tipo, "griglia")
})

test_that("la vista si ridisegna solo quando non copre più l'area inquadrata", {
  riquadro <- list(north = 46, south = 45, east = 12, west = 11)
  dentro <- list(north = 45.8, south = 45.2, east = 11.8, west = 11.2)
  fuori <- list(north = 46.5, south = 45.5, east = 12, west = 11)

  # primo disegno, o cambio di tipo
  expect_true(vista_da_rifare(NULL, list(tipo = "cantiere"), riquadro, 9))
  expect_true(vista_da_rifare(
    list(tipo = "cantiere", zoom = 10),
    list(tipo = "comune"),
    riquadro,
    11
  ))

  # le bolle di cantieri e comuni coprono tutto il territorio
  expect_false(vista_da_rifare(
    list(tipo = "cantiere", zoom = 9),
    list(tipo = "cantiere"),
    fuori,
    10
  ))
  expect_false(vista_da_rifare(
    list(tipo = "comune", zoom = 11),
    list(tipo = "comune"),
    fuori,
    12
  ))

  # i singoli bidoni valgono per l'area caricata, a qualsiasi zoom
  singoli <- list(tipo = "singoli", riquadro = riquadro, zoom = 15)
  expect_false(vista_da_rifare(singoli, list(tipo = "singoli"), dentro, 16))
  expect_true(vista_da_rifare(singoli, list(tipo = "singoli"), fuori, 15))

  # la griglia cambia anche con lo zoom
  griglia <- list(tipo = "griglia", riquadro = riquadro, zoom = 13)
  expect_false(vista_da_rifare(griglia, list(tipo = "griglia"), dentro, 13))
  expect_true(vista_da_rifare(griglia, list(tipo = "griglia"), dentro, 14))
  expect_true(vista_da_rifare(griglia, list(tipo = "griglia"), fuori, 13))
})

test_that("ogni vista aggregata ha una frase che la descrive", {
  expect_null(descrizione_vista("tutti"))
  expect_match(descrizione_vista("cantiere"), "una bolla per cantiere")
  expect_match(descrizione_vista("comune"), "una bolla per comune")
  expect_match(descrizione_vista("griglia"), "bolle per zona")
  expect_match(descrizione_vista("singoli"), "area inquadrata")
})

test_that("la ricerca disegna le letture più recenti quando sono troppe", {
  trovate <- cerca_per_rfid(
    dati_esempio(),
    c("RFD20250901001", "RFD20250905100")
  )
  tutte <- limita_letture_ricerca(trovate)
  expect_equal(nrow(tutte), nrow(trovate))
  expect_identical(attr(tutte, "totale"), nrow(trovate))

  poche <- limita_letture_ricerca(trovate, massimo = 5)
  expect_equal(nrow(poche), 5)
  expect_identical(attr(poche, "totale"), nrow(trovate))
  # restano l'ultima lettura di ogni RFID e, per il resto, le più recenti
  expect_equal(sum(poche$is_ultimo), 2)
  altre <- sort(
    trovate$giorno_lettura[!trovate$is_ultimo],
    decreasing = TRUE
  )
  expect_identical(
    sort(poche$giorno_lettura[!poche$is_ultimo], decreasing = TRUE),
    altre[1:3]
  )
  # l'ordine delle righe è quello di partenza
  chiave <- function(df) paste(df$RFID, format(df$giorno_lettura))
  expect_false(is.unsorted(match(chiave(poche), chiave(trovate))))

  # il limite predefinito segue l'opzione
  withr::local_options(trackerfid.max_letture_ricerca = 4)
  expect_equal(nrow(limita_letture_ricerca(trovate)), 4)
  expect_null(limita_letture_ricerca(NULL))
})
