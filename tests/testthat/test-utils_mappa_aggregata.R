test_that("il limite delle letture della ricerca si cambia con un'opzione", {
  withr::local_options(trackerfid.max_letture_ricerca = NULL)
  withr::local_options(trackerfid.letture_per_rfid = NULL)
  expect_identical(
    limiti_mappa(),
    list(letture_ricerca = 50000, letture_per_rfid = 10)
  )
  withr::local_options(
    trackerfid.max_letture_ricerca = 7,
    trackerfid.letture_per_rfid = 3
  )
  expect_identical(limiti_mappa(), list(letture_ricerca = 7, letture_per_rfid = 3))
})

test_that("il livello di aggregazione segue lo zoom di MapLibre", {
  # le fasce di Leaflet, cantieri fino a 11 e comuni fino a 14, alla stessa
  # scala: lo zoom di MapLibre è quello di Leaflet meno uno
  expect_identical(soglie_zoom(), list(cantiere = 10, comune = 13))
  expect_identical(livello_aggregazione(NULL), "cantiere")
  expect_identical(
    vapply(
      c(8, 10, 10.9, 11, 12, 13.5, 14, 16),
      livello_aggregazione,
      character(1)
    ),
    c(
      "cantiere",
      "cantiere",
      "cantiere",
      "comune",
      "comune",
      "comune",
      "bidoni",
      "bidoni"
    )
  )
  # le soglie stanno in un posto solo
  expect_identical(livello_aggregazione(soglie_zoom()$cantiere), "cantiere")
  expect_identical(livello_aggregazione(soglie_zoom()$cantiere + 1), "comune")
  expect_identical(livello_aggregazione(soglie_zoom()$comune + 1), "bidoni")
  expect_match(descrizione_vista("cantiere"), "una bolla per cantiere")
  expect_match(descrizione_vista("comune"), "una bolla per comune")
  expect_null(descrizione_vista("bidoni"))
})

test_that("il click su una bolla porta almeno al livello successivo", {
  # la bolla di un cantiere si apre sui comuni, quella di un comune sui bidoni
  expect_identical(zoom_minimo_bolla("cantiere"), soglie_zoom()$cantiere + 1)
  expect_identical(zoom_minimo_bolla("comune"), soglie_zoom()$comune + 1)
  expect_identical(
    livello_aggregazione(zoom_minimo_bolla("cantiere")),
    "comune"
  )
  expect_identical(livello_aggregazione(zoom_minimo_bolla("comune")), "bidoni")
  expect_true(is.na(zoom_minimo_bolla("altro")))
})

test_that("le bolle contano bidoni e censiti di ogni gruppo", {
  ultimi <- ultime_esempio()
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

  # la bolla di un cantiere sta nel punto medio dei suoi bidoni e ne riporta
  # il riquadro
  bassano <- per_cantiere[which(per_cantiere$nome == "BASSANO"), ]
  suoi <- ultimi[which(ultimi$cantiere == "BASSANO"), ]
  expect_equal(bassano$n, 66)
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
  # la bolla di un comune sta nel baricentro del comune, non tra i suoi bidoni
  baricentri <- baricentri_comuni()
  riga <- match(per_comune$nome, baricentri$comune)
  expect_false(anyNA(riga))
  expect_equal(per_comune$latitudine, baricentri$latitudine[riga])
  expect_equal(per_comune$longitudine, baricentri$longitudine[riga])
  # resta ferma quando i filtri lasciano solo una parte dei bidoni; il
  # riquadro, che serve al click, resta quello dei bidoni rimasti
  di_bassano <- ultimi[which(ultimi$comune_assegnato == "BASSANO DEL GRAPPA"), ]
  pochi <- di_bassano[1:2, ]
  una <- aggrega_marker(pochi, "comune")
  centro <- baricentri[baricentri$comune == "BASSANO DEL GRAPPA", ]
  expect_equal(nrow(una), 1)
  expect_equal(una$latitudine, centro$latitudine)
  expect_equal(una$longitudine, centro$longitudine)
  expect_equal(c(una$lat_min, una$lat_max), range(pochi$latitudine))
  expect_equal(c(una$lng_min, una$lng_max), range(pochi$longitudine))
  expect_false(isTRUE(all.equal(una$latitudine, mean(pochi$latitudine))))
  # un comune che non è nella tabella, o che manca, non ha un baricentro:
  # la bolla sta nel punto medio dei suoi bidoni
  altrove <- di_bassano[1:3, ]
  altrove$comune_assegnato <- c("MILANO", "MILANO", NA)
  fuori <- aggrega_marker(altrove, "comune")
  expect_identical(fuori$nome, c("MILANO", NA))
  expect_equal(
    fuori$latitudine,
    c(mean(altrove$latitudine[1:2]), altrove$latitudine[3])
  )
  expect_equal(
    fuori$longitudine,
    c(mean(altrove$longitudine[1:2]), altrove$longitudine[3])
  )
  # il nome si confronta senza badare a maiuscole e accenti
  minuscolo <- pochi
  minuscolo$comune_assegnato <- "Bassano del Grappa"
  expect_equal(
    aggrega_marker(minuscolo, "comune")$latitudine,
    centro$latitudine
  )

  # nessun bidone: nessuna bolla, stesse colonne
  vuote <- aggrega_marker(ultimi[0, ], "comune")
  expect_equal(nrow(vuote), 0)
  expect_named(vuote, names(per_comune))

  # senza colonne geografiche i bidoni formano una sola bolla senza nome
  senza <- aggrega_marker(ultime_di(letture_test()), "cantiere")
  expect_equal(nrow(senza), 1)
  expect_true(is.na(senza$nome))
  expect_equal(senza$n, 3)
  expect_equal(senza$censiti, 2)

  expect_error(aggrega_marker(ultimi, "provincia"))
  expect_equal(nrow(aggrega_marker(NULL, "cantiere")), 0)
})

test_that("la ricerca disegna le ultime letture di ogni RFID", {
  # il tag fermo in deposito ha centinaia di letture, il servizio cambiato 10
  trovate <- cerca_per_rfid(
    seleziona_periodo(archivio_esempio()),
    c("RFD20241001301", "RFD20250901001", "RFD20250905100")
  )
  conteggi <- table(trovate$RFID)
  expect_gt(conteggi[["RFD20241001301"]], 10)
  disegnate <- limita_letture_ricerca(trovate)
  expect_identical(attr(disegnate, "totale"), nrow(trovate))
  expect_equal(
    as.vector(table(disegnate$RFID)[names(conteggi)]),
    pmin(as.vector(conteggi), 10)
  )
  # di ogni RFID restano le più recenti, compresa l'ultima
  expect_equal(sum(disegnate$is_ultimo), 3)
  for (rfid in names(conteggi)) {
    tutte <- sort(trovate$giorno_lettura[trovate$RFID == rfid], decreasing = TRUE)
    tenute <- sort(disegnate$giorno_lettura[disegnate$RFID == rfid], decreasing = TRUE)
    expect_equal(tenute, utils::head(tutte, 10))
  }
  # l'ordine delle righe è quello di partenza
  chiave <- function(df) paste(df$RFID, format(df$giorno_lettura))
  expect_false(is.unsorted(match(chiave(disegnate), chiave(trovate))))

  # il numero per RFID segue l'argomento e l'opzione
  expect_equal(nrow(limita_letture_ricerca(trovate, per_rfid = 2)), 6)
  withr::local_options(trackerfid.letture_per_rfid = 1)
  una <- limita_letture_ricerca(trovate)
  expect_equal(nrow(una), 3)
  expect_true(all(una$is_ultimo))
  expect_equal(nrow(limita_letture_ricerca(trovate[0, ])), 0)
  expect_null(limita_letture_ricerca(NULL))
})

test_that("la ricerca disegna le letture più recenti quando sono troppe", {
  trovate <- cerca_per_rfid(
    seleziona_periodo(archivio_esempio()),
    c("RFD20250901001", "RFD20250905100")
  )
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
})
