test_that("la tabella dei comuni ha 57 comuni in quattro cantieri", {
  comuni <- comuni_cantieri()
  expect_named(comuni, c("comune", "cantiere", "latitudine", "longitudine"))
  expect_equal(nrow(comuni), 57)
  expect_equal(anyDuplicated(comuni$comune), 0)
  expect_identical(
    cantieri(),
    c("ASIAGO", "BASSANO", "CAMPOSAMPIERO", "RUBANO")
  )
  expect_identical(
    as.integer(table(comuni$cantiere)[cantieri()]),
    c(7L, 15L, 23L, 12L)
  )
  # i nomi sono in maiuscolo, come nel file dell'azienda
  expect_identical(comuni$comune, toupper(comuni$comune))
  # i centri abitati stanno nel Veneto centrale
  expect_true(all(comuni$latitudine > 45.2 & comuni$latitudine < 46.1))
  expect_true(all(comuni$longitudine > 11.3 & comuni$longitudine < 12.1))
  # il comune che dà il nome a un cantiere gli appartiene
  expect_identical(
    cantiere_di_comune(c(
      "ASIAGO",
      "BASSANO DEL GRAPPA",
      "CAMPOSAMPIERO",
      "RUBANO"
    )),
    cantieri()
  )
})

test_that("i confini hanno un comune per ogni riga della tabella dei comuni", {
  poligoni <- poligoni_comuni()
  comuni <- comuni_cantieri()
  expect_named(
    poligoni,
    c(
      "comune",
      "cantiere",
      "poligono",
      "anello",
      "punto",
      "longitudine",
      "latitudine"
    )
  )
  expect_false(anyNA(poligoni))
  # gli stessi comuni della tabella, nello stesso ordine e con lo stesso cantiere
  per_comune <- unique(poligoni[, c("comune", "cantiere")])
  expect_equal(nrow(per_comune), nrow(comuni))
  expect_identical(per_comune$comune, comuni$comune)
  expect_identical(per_comune$cantiere, comuni$cantiere)

  # il file dice da dove vengono i confini e di quale rilascio sono
  expect_identical(attr(poligoni, "origine"), origine_confini()$raccolta)
  expect_match(attr(poligoni, "versione"), "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")
  # nel file ogni comune porta anche il nome e il codice dell'ISTAT
  geojson <- jsonlite::fromJSON(
    percorso_confini_comuni(),
    simplifyVector = FALSE
  )
  expect_length(geojson$features, nrow(comuni))
  nomi_istat <- vapply(
    geojson$features,
    function(e) e$properties$nome_istat,
    character(1)
  )
  expect_identical(chiave_comune(nomi_istat), chiave_comune(comuni$comune))
  expect_identical(nomi_istat[comuni$comune == "ROSA'"], "Rosà")
  expect_identical(
    nomi_istat[comuni$comune == "ROMANO D'EZZELINO"],
    "Romano d'Ezzelino"
  )
  codici <- vapply(
    geojson$features,
    function(e) e$properties$codice_istat,
    character(1)
  )
  expect_equal(anyDuplicated(codici), 0)
  # i comuni serviti stanno nelle province di Vicenza e di Padova
  expect_true(all(substr(codici, 1, 3) %in% c("024", "028")))

  # ogni anello ha i punti in ordine, almeno tre lati, ed è chiuso
  anelli <- dplyr::summarise(
    poligoni,
    punti = dplyr::n(),
    in_ordine = identical(.data$punto, seq_len(dplyr::n())),
    chiuso = .data$longitudine[1] == .data$longitudine[dplyr::n()] &&
      .data$latitudine[1] == .data$latitudine[dplyr::n()],
    .by = c("poligono", "anello")
  )
  expect_true(all(anelli$punti >= 4))
  expect_true(all(anelli$in_ordine))
  expect_true(all(anelli$chiuso))

  # il centro abitato di ogni comune cade dentro il suo confine
  vertici <- split(poligoni, poligoni$comune)
  nel_suo <- vapply(
    seq_len(nrow(comuni)),
    function(i) {
      dentro_poligoni(
        comuni$longitudine[i],
        comuni$latitudine[i],
        vertici[[comuni$comune[i]]]
      )
    },
    logical(1)
  )
  expect_true(all(nel_suo))
  # e fuori da quello di un altro comune
  asiago <- comuni[comuni$comune == "ASIAGO", ]
  expect_false(dentro_poligoni(
    asiago$longitudine,
    asiago$latitudine,
    vertici[["RUBANO"]]
  ))

  # la zona servita è il riquadro di tutti i confini
  zona <- riquadro_zona()
  expect_named(zona, c("lat_min", "lat_max", "lng_min", "lng_max"))
  expect_identical(zona$lat_min, min(poligoni$latitudine))
  expect_identical(zona$lng_max, max(poligoni$longitudine))
  expect_lt(zona$lat_min, zona$lat_max)
  expect_lt(zona$lng_min, zona$lng_max)
})

test_that("un punto è dentro i poligoni se attraversa un numero dispari di lati", {
  quadrato <- function(x0, y0, lato, poligono = 1, anello = 1) {
    dplyr::tibble(
      poligono = poligono,
      anello = anello,
      longitudine = x0 + c(0, lato, lato, 0, 0),
      latitudine = y0 + c(0, 0, lato, lato, 0)
    )
  }
  # un quadrato con un foro al centro e un secondo quadrato staccato
  poligoni <- dplyr::bind_rows(
    quadrato(0, 0, 10),
    quadrato(4, 4, 2, anello = 2),
    quadrato(20, 0, 5, poligono = 2)
  )
  expect_true(dentro_poligoni(1, 1, poligoni))
  expect_true(dentro_poligoni(9, 9, poligoni))
  # nel foro, tra i due poligoni, fuori da tutto
  expect_false(dentro_poligoni(5, 5, poligoni))
  expect_false(dentro_poligoni(15, 2, poligoni))
  expect_false(dentro_poligoni(-1, 5, poligoni))
  expect_false(dentro_poligoni(5, 11, poligoni))
  # nel secondo poligono
  expect_true(dentro_poligoni(22, 2, poligoni))

  # un anello non chiuso vale come chiuso; nessun poligono, nessun punto dentro
  aperto <- quadrato(0, 0, 10)[1:4, ]
  expect_true(dentro_poligoni(5, 5, aperto))
  expect_false(dentro_poligoni(11, 5, aperto))
  expect_false(dentro_poligoni(1, 1, poligoni[0, ]))
})

test_that("i vertici si leggono da poligoni e multipoligoni", {
  anello <- function(x0, y0, lato) {
    list(
      list(x0, y0),
      list(x0 + lato, y0),
      list(x0 + lato, y0 + lato),
      list(x0, y0 + lato),
      list(x0, y0)
    )
  }
  semplice <- anelli_geometria(list(
    type = "Polygon",
    coordinates = list(anello(11, 45, 1), anello(11.4, 45.4, 0.2))
  ))
  expect_named(
    semplice,
    c("poligono", "anello", "punto", "longitudine", "latitudine")
  )
  expect_equal(nrow(semplice), 10)
  expect_identical(unique(semplice$poligono), 1L)
  expect_identical(unique(semplice$anello), 1:2)
  expect_identical(semplice$punto, c(1:5, 1:5))
  expect_equal(semplice$longitudine[1:5], c(11, 12, 12, 11, 11))
  expect_equal(semplice$latitudine[1:5], c(45, 45, 46, 46, 45))

  multiplo <- anelli_geometria(list(
    type = "MultiPolygon",
    coordinates = list(list(anello(11, 45, 1)), list(anello(13, 45, 1)))
  ))
  expect_identical(unique(multiplo$poligono), 1:2)
  expect_identical(unique(multiplo$anello), 1L)

  # una terza coordinata, la quota, viene ignorata
  con_quota <- anelli_geometria(list(
    type = "Polygon",
    coordinates = list(lapply(anello(11, 45, 1), function(p) c(p, list(120))))
  ))
  expect_equal(con_quota$longitudine, c(11, 12, 12, 11, 11))
  expect_equal(con_quota$latitudine, c(45, 45, 46, 46, 45))

  expect_error(
    anelli_geometria(list(type = "Point", coordinates = list(11, 45))),
    "Geometria non prevista"
  )
})

test_that("una funzione applicata ai valori distinti dà lo stesso risultato", {
  x <- c("b", "a", NA, "b", "a")
  valori_visti <- 0
  maiuscolo <- function(valori) {
    valori_visti <<- valori_visti + length(valori)
    toupper(valori)
  }
  expect_identical(per_valori_distinti(x, maiuscolo), toupper(x))
  expect_equal(valori_visti, 3)
  expect_identical(per_valori_distinti(character(0), toupper), character(0))
})

test_that("i nomi dei comuni si confrontano senza accenti e apostrofi", {
  expect_identical(
    chiave_comune(c("Rosà", "ROSA'", "rosa", " Romano  d'Ezzelino ", NA)),
    c("ROSA", "ROSA", "ROSA", "ROMANO D EZZELINO", NA)
  )
  # i nomi tornano alla grafia della tabella; quelli ignoti restano, in maiuscolo
  expect_identical(
    normalizza_comune(c("Rosà", "romano d'ezzelino", "Paese Ignoto", NA)),
    c("ROSA'", "ROMANO D'EZZELINO", "PAESE IGNOTO", NA)
  )
  expect_identical(
    cantiere_di_comune(c("Rosà", "limena", "Cittadella", "Paese Ignoto", NA)),
    c("BASSANO", "RUBANO", "CAMPOSAMPIERO", NA, NA)
  )
  expect_identical(normalizza_comune(character(0)), character(0))
  expect_identical(cantiere_di_comune(character(0)), character(0))
})

test_that("il comune assegnato è quello del database, altrimenti quello della lettura", {
  letture <- dplyr::tibble(
    comune_da_database = c("LIMENA", NA, NA, "PAESE IGNOTO", NA),
    comune_lettura = c(
      "CITTADELLA",
      "CITTADELLA",
      NA,
      "LIMENA",
      "PAESE IGNOTO"
    ),
    cantiere = c("BASSANO", NA, "ASIAGO", "ALTROVE", NA)
  )
  esito <- aggiungi_geografia(letture)
  expect_identical(
    esito$comune_assegnato,
    c("LIMENA", "CITTADELLA", NA, "PAESE IGNOTO", "PAESE IGNOTO")
  )
  # il cantiere segue la tabella dei comuni, anche contro quello scritto nel
  # file; per un comune ignoto o mancante resta quello del file
  expect_identical(
    esito$cantiere,
    c("RUBANO", "CAMPOSAMPIERO", "ASIAGO", "ALTROVE", NA)
  )

  # senza colonne geografiche le colonne vengono create vuote
  senza <- aggiungi_geografia(letture_test())
  expect_true(all(
    c(colonne_geografiche(), colonne_derivate()) %in% names(senza)
  ))
  expect_true(all(is.na(senza$comune_assegnato)))
  expect_true(all(is.na(senza$cantiere)))
  expect_equal(nrow(senza), nrow(letture_test()))
})

test_that("ogni RFID ha il comune del database o quello in cui è letto più spesso", {
  letture <- dplyr::tibble(
    RFID = c("A", "A", "A", "B", "B", "B", "C", "D", "D", "E"),
    comune_da_database = c(
      "LIMENA",
      "LIMENA",
      "RUBANO",
      NA,
      NA,
      NA,
      NA,
      "PAESE IGNOTO",
      NA,
      NA
    ),
    comune_lettura = c(
      "CITTADELLA",
      "CITTADELLA",
      "CITTADELLA",
      "ASIAGO",
      "GALLIO",
      "GALLIO",
      NA,
      NA,
      "ASIAGO",
      NA
    ),
    cantiere = c(NA, NA, NA, NA, NA, NA, NA, "ALTROVE", NA, "ASIAGO")
  )
  esito <- geografia_per_gruppo(letture, indice_gruppi(letture$RFID))
  expect_named(esito, c("comune", "cantiere"))
  # A: l'ultimo comune indicato dal database, non quello di lettura
  # B: non censito, il comune in cui è letto più spesso
  # C: nessun comune
  # D: il database vale anche se l'ultima lettura non lo riporta
  expect_identical(
    esito$comune,
    c("RUBANO", "GALLIO", NA, "PAESE IGNOTO", NA)
  )
  # D: comune fuori tabella, vale il cantiere scritto sulle sue letture
  # E: senza comune, resta il cantiere del file
  expect_identical(
    esito$cantiere,
    c("RUBANO", "ASIAGO", NA, "ALTROVE", "ASIAGO")
  )

  vuota <- geografia_per_gruppo(dati_esempio()[0, ], integer(0), 0)
  expect_equal(nrow(vuota), 0)
  expect_type(vuota$comune, "character")
  expect_type(vuota$cantiere, "character")
})

test_that("sul dataset di esempio ogni contenitore censito ha il comune del database", {
  dati <- dati_esempio()
  # in ordine di tempo dentro ogni RFID, come nell'anagrafica
  dati <- dati[order(dati$RFID, dati$giorno_lettura), ]
  gruppo <- indice_gruppi(dati$RFID)
  geografia <- geografia_per_gruppo(dati, gruppo)
  expect_equal(nrow(geografia), 251)
  ultimo_comune <- ultimo_valido_per_gruppo(dati$comune_da_database, gruppo)
  censiti <- !is.na(ultimo_comune)
  expect_identical(geografia$comune[censiti], ultimo_comune[censiti])
  expect_identical(
    geografia$cantiere[censiti],
    cantiere_di_comune(ultimo_comune[censiti])
  )
  # i non censiti e i sacchetti, che a database non hanno un comune, hanno
  # quello del giro che li ha letti più spesso
  expect_false(any(rfid_sacchetto(unique(dati$RFID)) & censiti))
  expect_identical(
    geografia$comune[!censiti],
    piu_frequente_per_gruppo(dati$comune_lettura, gruppo)[!censiti]
  )
  expect_equal(
    as.integer(table(geografia$cantiere)[cantieri()]),
    c(30, 69, 89, 63)
  )
  # ogni lettura ha il comune del giro: nessun RFID resta senza cantiere
  expect_false(anyNA(geografia$comune))
  expect_false(anyNA(geografia$cantiere))
})
