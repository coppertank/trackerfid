# Confini della Regione in miniatura, nella forma che dà
# `leggi_confini_regione()`: ogni comune è un quadrato attorno al suo centro,
# con le proprietà dello shapefile della Regione del Veneto.

anello_quadrato <- function(lng, lat, lato = 0.015) {
  list(
    list(lng - lato, lat - lato),
    list(lng + lato, lat - lato),
    list(lng + lato, lat + lato),
    list(lng - lato, lat + lato),
    list(lng - lato, lat - lato)
  )
}

comune_finto <- function(nome, lng, lat, provincia, codice, geometria = NULL) {
  list(
    type = "Feature",
    properties = list(
      # la Regione scrive il codice ISTAT come numero, senza lo zero iniziale
      codistat = as.character(as.integer(codice)),
      nomcom = nome,
      provincia = provincia,
      area = 1,
      perimetro = 1,
      id1 = 1L
    ),
    geometry = geometria %||%
      list(type = "Polygon", coordinates = list(anello_quadrato(lng, lat)))
  )
}

nazionale_finto <- function() {
  list(
    type = "FeatureCollection",
    features = list(
      # un comune con lo stesso nome, senza accento, in un'altra provincia
      comune_finto("Rosa", 9.00, 44.00, "XX", "099001"),
      comune_finto("Altro Comune", 10.00, 45.00, "YY", "099002"),
      comune_finto("Romano d'Ezzelino", 11.80, 45.80, "VI", "024086"),
      comune_finto("Rosa'", 11.76, 45.72, "VI", "024087"),
      # due poligoni staccati, il primo con un foro lontano dal centro
      comune_finto(
        "Bassano del Grappa",
        11.73,
        45.76,
        "VI",
        "024012",
        geometria = list(
          type = "MultiPolygon",
          coordinates = list(
            list(
              anello_quadrato(11.73, 45.76),
              anello_quadrato(11.74, 45.77, lato = 0.002)
            ),
            list(anello_quadrato(11.70, 45.80, lato = 0.005))
          )
        )
      )
    )
  )
}

serviti_finti <- function() {
  dplyr::tibble(
    comune = c("BASSANO DEL GRAPPA", "ROMANO D'EZZELINO", "ROSÀ"),
    cantiere = "BASSANO",
    latitudine = c(45.76, 45.80, 45.72),
    longitudine = c(11.73, 11.80, 11.76)
  )
}

proprieta <- function(confini, nome) {
  vapply(confini$features, function(e) e$properties[[nome]], character(1))
}

test_that("i nomi si associano senza badare ad accenti, apostrofi e maiuscole", {
  serviti <- serviti_finti()
  confini <- seleziona_confini_comuni(nazionale_finto(), serviti)

  expect_identical(confini$type, "FeatureCollection")
  expect_identical(confini$origine, origine_confini()$fonte)
  expect_match(confini$licenza, "IODL 2.0", fixed = TRUE)
  # un confine per ogni riga della tabella, nello stesso ordine
  expect_length(confini$features, nrow(serviti))
  expect_identical(proprieta(confini, "comune"), serviti$comune)
  expect_identical(proprieta(confini, "cantiere"), serviti$cantiere)
  # ROSÀ trova Rosa' della Regione, e non il comune Rosa di un'altra provincia
  expect_identical(
    proprieta(confini, "nome_istat"),
    c("Bassano del Grappa", "Romano d'Ezzelino", "Rosa'")
  )
  # il codice ISTAT torna di sei cifre
  expect_identical(
    proprieta(confini, "codice_istat"),
    c("024012", "024086", "024087")
  )
  expect_identical(proprieta(confini, "provincia"), rep("VI", 3))
  expect_false("codice_catastale" %in% names(confini$features[[1]]$properties))
  # la geometria è quella della fonte
  expect_identical(
    confini$features[[1]]$geometry,
    nazionale_finto()$features[[5]]$geometry
  )
  expect_identical(confini$features[[3]]$geometry$type, "Polygon")

  # altre grafie dello stesso nome
  altre <- serviti
  altre$comune <- c("Bassano Del Grappa", "ROMANO D’EZZELINO", "ROSA'")
  expect_identical(
    proprieta(seleziona_confini_comuni(nazionale_finto(), altre), "codice_istat"),
    c("024012", "024086", "024087")
  )
})

test_that("tra due comuni con lo stesso nome vale quello che contiene il centro", {
  serviti <- serviti_finti()
  # con il centro nell'altra provincia il nome trova l'altro comune
  altrove <- serviti
  altrove$latitudine[3] <- 44.00
  altrove$longitudine[3] <- 9.00
  expect_identical(
    proprieta(seleziona_confini_comuni(nazionale_finto(), altrove), "codice_istat")[3],
    "099001"
  )

  # un centro fuori da tutti i confini con quel nome ferma tutto
  fuori <- serviti
  fuori$latitudine[3] <- 44.50
  expect_error(
    seleziona_confini_comuni(nazionale_finto(), fuori),
    "il centro abitato cade fuori dal confine con lo stesso nome: ROSÀ"
  )
  # il centro nel foro di un poligono è fuori dal comune
  nel_foro <- serviti
  nel_foro$latitudine[1] <- 45.77
  nel_foro$longitudine[1] <- 11.74
  expect_error(
    seleziona_confini_comuni(nazionale_finto(), nel_foro),
    "cade fuori dal confine con lo stesso nome: BASSANO DEL GRAPPA"
  )
  # il secondo poligono fa parte del comune
  nel_secondo <- serviti
  nel_secondo$latitudine[1] <- 45.80
  nel_secondo$longitudine[1] <- 11.70
  expect_length(seleziona_confini_comuni(nazionale_finto(), nel_secondo)$features, 3)

  # senza coordinate un nome unico basta, un nome ripetuto no
  senza <- serviti
  senza$latitudine <- NA_real_
  senza$longitudine <- NA_real_
  expect_identical(
    proprieta(seleziona_confini_comuni(nazionale_finto(), senza[1:2, ]), "codice_istat"),
    c("024012", "024086")
  )
  expect_error(
    seleziona_confini_comuni(nazionale_finto(), senza),
    "più comuni con lo stesso nome, servono le coordinate del centro: ROSÀ"
  )
})

test_that("un comune che non si trova ferma la selezione e viene elencato", {
  serviti <- rbind(
    serviti_finti(),
    dplyr::tibble(
      comune = c("PAESE CHE NON C'E'", "ALTRO PAESE"),
      cantiere = "BASSANO",
      latitudine = 45.7,
      longitudine = 11.7
    )
  )
  errore <- expect_error(seleziona_confini_comuni(nazionale_finto(), serviti))
  expect_match(conditionMessage(errore), "Confini dei comuni non associati")
  expect_match(
    conditionMessage(errore),
    "non trovati: PAESE CHE NON C'E', ALTRO PAESE",
    fixed = TRUE
  )

  # una riga ripetuta nella tabella dei comuni
  doppia <- serviti_finti()[c(1, 2, 3, 3), ]
  expect_error(
    seleziona_confini_comuni(nazionale_finto(), doppia),
    "contiene più volte: ROSÀ"
  )
})

test_that("il file dei confini ha un comune per riga e si rilegge uguale", {
  confini <- seleziona_confini_comuni(nazionale_finto(), serviti_finti())
  confini$versione <- "2099-01-01"
  file <- withr::local_tempfile(fileext = ".geojson")
  expect_identical(scrivi_confini_comuni(confini, file), file)

  righe <- readLines(file, encoding = "UTF-8")
  expect_length(righe, 3 + 2)
  expect_match(righe[1], "^\\{\"type\":\"FeatureCollection\"")
  expect_match(righe[1], "\"versione\":\"2099-01-01\"", fixed = TRUE)
  expect_match(righe[2], "^\\{\"type\":\"Feature\".*\\},$")
  expect_match(righe[4], "\"nome_istat\":\"Rosa'\"", fixed = TRUE)
  expect_identical(righe[5], "]}")

  riletto <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(riletto$versione, "2099-01-01")
  expect_identical(riletto$origine, origine_confini()$fonte)
  expect_identical(proprieta(riletto, "comune"), serviti_finti()$comune)
  expect_equal(anelli_geojson(riletto), anelli_geojson(confini), tolerance = 1e-6)

  # i vertici: due poligoni per il primo comune, il primo con un foro
  vertici <- anelli_geojson(riletto)
  bassano <- vertici[vertici$comune == "BASSANO DEL GRAPPA", ]
  expect_identical(
    unique(bassano$poligono),
    c("BASSANO DEL GRAPPA 1", "BASSANO DEL GRAPPA 2")
  )
  expect_identical(
    unique(bassano$anello[bassano$poligono == "BASSANO DEL GRAPPA 1"]),
    1:2
  )
  expect_equal(nrow(bassano), 15)
})

# Scrive in una cartella uno shapefile in miniatura, con il sistema di
# coordinate della Regione del Veneto, Monte Mario fuso ovest.
shapefile_finto <- function(cartella) {
  dati <- do.call(rbind, lapply(nazionale_finto()$features, function(e) {
    data.frame(e$properties[c("codistat", "nomcom", "provincia")])
  }))
  geometrie <- lapply(nazionale_finto()$features, function(e) {
    anelli <- function(poligono) {
      lapply(poligono, function(anello) {
        matrix(unlist(anello), ncol = 2, byrow = TRUE)
      })
    }
    if (e$geometry$type == "Polygon") {
      sf::st_multipolygon(list(anelli(e$geometry$coordinates)))
    } else {
      sf::st_multipolygon(lapply(e$geometry$coordinates, anelli))
    }
  })
  regione <- sf::st_sf(dati, geometry = sf::st_sfc(geometrie, crs = 4326))
  regione <- sf::st_transform(regione, 3003)
  sf::st_write(
    regione,
    file.path(cartella, "comuni_regione.shp"),
    quiet = TRUE,
    layer_options = "ENCODING=ISO-8859-1"
  )
  writeLines("ISO-8859-1", file.path(cartella, "comuni_regione.cst"))
  invisible(cartella)
}

test_that("i confini si leggono dallo shapefile della Regione, in WGS84", {
  skip_if_not_installed("sf")
  cartella <- withr::local_tempdir()
  shapefile_finto(cartella)

  regione <- leggi_confini_regione(cartella)
  expect_identical(regione$type, "FeatureCollection")
  expect_length(regione$features, 5)
  expect_identical(
    proprieta(regione, "nomcom"),
    c("Rosa", "Altro Comune", "Romano d'Ezzelino", "Rosa'", "Bassano del Grappa")
  )
  # dal sistema della Regione tornano longitudine e latitudine, al centimetro
  romano <- anelli_geometria(regione$features[[3]]$geometry)
  expect_equal(range(romano$longitudine), c(11.785, 11.815), tolerance = 1e-6)
  expect_equal(range(romano$latitudine), c(45.785, 45.815), tolerance = 1e-6)
  # il foro e il secondo poligono di Bassano restano
  bassano <- anelli_geometria(regione$features[[5]]$geometry)
  expect_identical(unique(bassano$poligono), 1:2)
  expect_identical(unique(bassano$anello[bassano$poligono == 1]), 1:2)
  # basta anche il percorso del file
  expect_length(
    leggi_confini_regione(file.path(cartella, "comuni_regione.shp"))$features,
    5
  )

  # una cartella senza shapefile, o con due, ferma la lettura
  vuota <- withr::local_tempdir()
  expect_error(leggi_confini_regione(vuota), "uno e un solo shapefile")
  file.copy(
    file.path(cartella, "comuni_regione.shp"),
    file.path(cartella, "altro.shp")
  )
  expect_error(leggi_confini_regione(cartella), "trovati 2")
})

test_that("il file dei confini si prepara dallo shapefile, o non si scrive", {
  skip_if_not_installed("sf")
  cartella <- withr::local_tempdir()
  shapefile_finto(cartella)
  Sys.setFileTime(file.path(cartella, "comuni_regione.shp"), as.POSIXct("2020-01-02 10:00:00"))

  file <- withr::local_tempfile(fileext = ".geojson")
  expect_message(
    scritto <- prepara_confini_comuni(cartella, file, comuni = serviti_finti()),
    "Scritti i confini di 3 comuni .* dallo shapefile del 2020-01-02"
  )
  expect_identical(scritto, file)
  riletto <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(riletto$versione, "2020-01-02")
  expect_identical(riletto$origine, origine_confini()$fonte)
  expect_identical(proprieta(riletto, "comune"), serviti_finti()$comune)
  expect_identical(
    proprieta(riletto, "codice_istat"),
    c("024012", "024086", "024087")
  )
  # un comune per riga
  expect_length(readLines(file), 3 + 2)

  # se un comune manca il file non viene scritto
  nuovo <- withr::local_tempfile(fileext = ".geojson")
  mancante <- serviti_finti()
  mancante$comune[2] <- "COMUNE SCONOSCIUTO"
  expect_error(
    prepara_confini_comuni(cartella, nuovo, comuni = mancante),
    "non trovati: COMUNE SCONOSCIUTO"
  )
  expect_false(file.exists(nuovo))
})
