# File nazionale dei comuni in miniatura, nella forma di quello della raccolta
# geojson-italy: ogni comune è un quadrato attorno al suo centro.

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
      name = nome,
      com_istat_code = codice,
      com_catasto_code = paste0("X", substr(codice, 4, 6)),
      prov_acr = provincia,
      reg_name = "Veneto"
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
      comune_finto("Rosà", 11.76, 45.72, "VI", "024087"),
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
    comune = c("BASSANO DEL GRAPPA", "ROMANO D'EZZELINO", "ROSA'"),
    cantiere = "BASSANO",
    latitudine = c(45.76, 45.80, 45.72),
    longitudine = c(11.73, 11.80, 11.76)
  )
}

proprieta <- function(confini, nome) {
  vapply(confini$features, function(e) e$properties[[nome]], character(1))
}

test_that("il rilascio più aggiornato è l'ultimo entrato in vigore", {
  indice <- data.frame(
    valid_from = c("2025-10-10", "2026-01-01", "2026-02-21", "2026-05-14"),
    valid_to = c("2026-01-01", "2026-02-21", "2026-05-14", ""),
    release_tag = c("2025-10-10", "2026-01-01", "2026-02-21", "2026-05-14"),
    municipalities = c(7896, 7896, 7894, 7894),
    stringsAsFactors = FALSE
  )
  expect_identical(
    rilascio_confini(indice, as.Date("2026-10-09"))$release_tag,
    "2026-05-14"
  )
  # il giorno stesso dell'entrata in vigore vale già il rilascio nuovo
  expect_identical(
    rilascio_confini(indice, as.Date("2026-02-21"))$release_tag,
    "2026-02-21"
  )
  expect_identical(
    rilascio_confini(indice, as.Date("2026-02-20"))$release_tag,
    "2026-01-01"
  )
  # l'ordine delle righe dell'indice non conta
  expect_identical(
    rilascio_confini(indice[4:1, ], as.Date("2026-03-01"))$release_tag,
    "2026-02-21"
  )
  expect_equal(rilascio_confini(indice, as.Date("2026-03-01"))$municipalities, 7894)
  expect_error(
    rilascio_confini(indice, as.Date("2020-01-01")),
    "Nessun rilascio"
  )

  # gli indirizzi puntano alla raccolta indicata dal proprietario del progetto
  origine <- origine_confini()
  expect_identical(origine$raccolta, "https://github.com/guglielmo/geojson-italy")
  expect_match(origine$indice, "guglielmo/geojson-italy/main/temporal/INDEX.csv")
  expect_match(
    sprintf(origine$comuni, "2026-05-14"),
    "releases/download/2026-05-14/limits_IT_municipalities.geojson.gz$"
  )
})

test_that("i nomi si associano senza badare ad accenti, apostrofi e maiuscole", {
  serviti <- serviti_finti()
  confini <- seleziona_confini_comuni(nazionale_finto(), serviti)

  expect_identical(confini$type, "FeatureCollection")
  expect_identical(confini$origine, origine_confini()$raccolta)
  # un confine per ogni riga della tabella, nello stesso ordine
  expect_length(confini$features, nrow(serviti))
  expect_identical(proprieta(confini, "comune"), serviti$comune)
  expect_identical(proprieta(confini, "cantiere"), serviti$cantiere)
  # ROSA' trova Rosà, e non il comune Rosa di un'altra provincia
  expect_identical(
    proprieta(confini, "nome_istat"),
    c("Bassano del Grappa", "Romano d'Ezzelino", "Rosà")
  )
  expect_identical(
    proprieta(confini, "codice_istat"),
    c("024012", "024086", "024087")
  )
  expect_identical(proprieta(confini, "provincia"), rep("VI", 3))
  expect_identical(
    proprieta(confini, "codice_catastale"),
    c("X012", "X086", "X087")
  )
  # la geometria è quella della fonte
  expect_identical(
    confini$features[[1]]$geometry,
    nazionale_finto()$features[[5]]$geometry
  )
  expect_identical(confini$features[[3]]$geometry$type, "Polygon")

  # altre grafie dello stesso nome
  altre <- serviti
  altre$comune <- c("Bassano Del Grappa", "ROMANO D’EZZELINO", "rosà")
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
    "il centro abitato cade fuori dal confine con lo stesso nome: ROSA'"
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
    "più comuni con lo stesso nome, servono le coordinate del centro: ROSA'"
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
    "contiene più volte: ROSA'"
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
  expect_match(righe[4], "\"nome_istat\":\"Rosà\"", fixed = TRUE)
  expect_identical(righe[5], "]}")

  riletto <- jsonlite::fromJSON(file, simplifyVector = FALSE)
  expect_identical(riletto$versione, "2099-01-01")
  expect_identical(riletto$origine, origine_confini()$raccolta)
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

test_that("i confini si preparano anche da un file nazionale già scaricato", {
  nazionale <- withr::local_tempfile(fileext = ".geojson")
  jsonlite::write_json(nazionale_finto(), nazionale, auto_unbox = TRUE, digits = 8)
  compresso <- withr::local_tempfile(fileext = ".geojson.gz")
  uscita <- gzfile(compresso, "wb")
  writeBin(readBin(nazionale, "raw", file.size(nazionale)), uscita)
  close(uscita)

  for (sorgente in c(nazionale, compresso)) {
    file <- withr::local_tempfile(fileext = ".geojson")
    expect_message(
      scritto <- scarica_confini_comuni(
        file,
        comuni = serviti_finti(),
        sorgente = sorgente,
        versione = "2099-01-01"
      ),
      "Scritti i confini di 3 comuni .* dal rilascio 2099-01-01"
    )
    expect_identical(scritto, file)
    riletto <- jsonlite::fromJSON(file, simplifyVector = FALSE)
    expect_identical(riletto$versione, "2099-01-01")
    expect_identical(proprieta(riletto, "comune"), serviti_finti()$comune)
    expect_identical(
      proprieta(riletto, "codice_istat"),
      c("024012", "024086", "024087")
    )
  }

  # se un comune manca il file non viene scritto
  file <- withr::local_tempfile(fileext = ".geojson")
  mancante <- serviti_finti()
  mancante$comune[2] <- "COMUNE SCONOSCIUTO"
  expect_error(
    scarica_confini_comuni(file, comuni = mancante, sorgente = nazionale),
    "non trovati: COMUNE SCONOSCIUTO"
  )
  expect_false(file.exists(file))
})
