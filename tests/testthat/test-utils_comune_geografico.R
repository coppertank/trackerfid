# Punti della zona servita e dintorni, e il loro comune trovato punto per
# punto, confine per confine: il termine di paragone per la griglia.
punti_di_prova <- local({
  cache <- NULL
  function() {
    if (is.null(cache)) {
      set.seed(20251010)
      zona <- riquadro_zona()
      confini <- poligoni_comuni()
      # punti a caso sulla zona, e punti a pochi metri dai confini
      vicini <- sample(nrow(confini), 400)
      lat <- c(
        stats::runif(600, zona$lat_min - 0.02, zona$lat_max + 0.02),
        confini$latitudine[vicini] + stats::rnorm(400, 0, 0.0002)
      )
      lng <- c(
        stats::runif(600, zona$lng_min - 0.02, zona$lng_max + 0.02),
        confini$longitudine[vicini] + stats::rnorm(400, 0, 0.0002)
      )
      per_comune <- split(confini, confini$comune)
      comuni <- comuni_cantieri()$comune
      atteso <- vapply(
        seq_along(lat),
        function(i) {
          dentro <- vapply(
            comuni,
            function(comune) dentro_poligoni(lng[i], lat[i], per_comune[[comune]]),
            logical(1)
          )
          if (sum(dentro) == 1) comuni[dentro] else NA_character_
        },
        character(1)
      )
      cache <<- list(lat = lat, lng = lng, comune = atteso)
    }
    cache
  }
})

test_that("il comune di un punto \u00e8 quello il cui confine lo contiene", {
  punti <- punti_di_prova()
  # tra i punti ce ne sono dentro i comuni e fuori da tutti
  expect_gt(sum(!is.na(punti$comune)), 300)
  expect_gt(sum(is.na(punti$comune)), 100)
  # senza margine il comune \u00e8 quello che contiene il punto, e nient'altro
  expect_identical(
    comune_geografico(punti$lat, punti$lng, margine = 0),
    punti$comune
  )
  indice <- indice_comune_geografico(punti$lat, punti$lng)
  expect_identical(
    comuni_cantieri()$comune[replace(indice, indice == 0L, NA)],
    punti$comune
  )

  # il centro abitato di ogni comune sta nel suo comune
  comuni <- comuni_cantieri()
  expect_identical(
    comune_geografico(comuni$latitudine, comuni$longitudine),
    comuni$comune
  )
  # punti lontani, coordinate mancanti, nessun punto
  expect_identical(
    comune_geografico(c(41.9, NA, 45.7667, 45.7667), c(12.5, 11.7, NA, 11.7342)),
    c(NA, NA, NA, "BASSANO DEL GRAPPA")
  )
  expect_identical(comune_geografico(numeric(0), numeric(0)), character(0))
  expect_identical(indice_comune_geografico(c(41.9, NA), c(12.5, 11.7)), c(0L, 0L))
})

test_that("un punto appena fuori dai comuni serviti prende il comune pi\u00f9 vicino", {
  expect_identical(margine_confine_m(), 100)
  punti <- punti_di_prova()
  comuni <- comuni_cantieri()$comune
  # termine di paragone: la distanza di ogni punto da ogni comune, una per una
  fuori <- which(is.na(punti$comune))
  distanze <- vapply(
    comuni,
    function(comune) {
      distanza_dal_comune_m(punti$lat[fuori], punti$lng[fuori], comune)
    },
    numeric(length(fuori))
  )
  minima <- apply(distanze, 1, min)
  piu_vicino <- comuni[max.col(-distanze, ties.method = "first")]
  atteso <- punti$comune
  atteso[fuori] <- ifelse(minima <= 100, piu_vicino, NA_character_)
  # tra i punti di prova ce ne sono entro il margine e oltre
  expect_gt(sum(minima <= 100), 20)
  expect_gt(sum(minima > 100), 50)
  expect_identical(comune_geografico(punti$lat, punti$lng), atteso)
  # chi sta dentro un comune lo tiene, qualunque sia il margine
  dentro <- !is.na(punti$comune)
  expect_identical(
    comune_geografico(punti$lat, punti$lng, margine = 5000)[dentro],
    punti$comune[dentro]
  )
  # un margine pi\u00f9 largo raccoglie pi\u00f9 punti
  largo <- comune_geografico(punti$lat, punti$lng, margine = 500)
  atteso_largo <- punti$comune
  atteso_largo[fuori] <- ifelse(minima <= 500, piu_vicino, NA_character_)
  expect_identical(largo, atteso_largo)
  expect_gt(sum(!is.na(largo)), sum(!is.na(atteso)))

  # il punto pi\u00f9 a nord di tutto il territorio: 50 metri oltre si \u00e8 ancora
  # nel suo comune, 150 metri oltre no
  confini <- poligoni_comuni()
  nord <- confini[which.max(confini$latitudine), ]
  lat <- nord$latitudine + gradi_lat(c(50, 150))
  lng <- rep(nord$longitudine, 2)
  expect_identical(indice_comune_geografico(lat, lng), c(0L, 0L))
  expect_identical(comune_geografico(lat, lng), c(nord$comune, NA))
  expect_identical(
    indice_comune_vicino(lat, lng),
    c(match(nord$comune, comuni), 0L)
  )
  expect_identical(comune_geografico(lat, lng, margine = 200), rep(nord$comune, 2))
  # con il risultato della griglia gi\u00e0 calcolato il risultato \u00e8 lo stesso
  expect_identical(
    indice_comune_con_margine(lat, lng, geografico = c(0L, 0L)),
    c(match(nord$comune, comuni), 0L)
  )
  # punti lontani, senza coordinate, o nessun punto
  expect_identical(indice_comune_vicino(c(41.9, NA, 45.7), c(12.5, 11.7, NA)), c(0L, 0L, 0L))
  expect_identical(indice_comune_vicino(numeric(0), numeric(0)), integer(0))
})

test_that("la griglia lascia al controllo esatto le sole celle sui confini", {
  griglia <- griglia_comuni()
  expect_length(griglia$celle, griglia$n_righe * griglia$n_colonne)
  # celle di circa 55 metri
  scala <- metri_per_grado()
  expect_equal(griglia$passo_lng * scala[["lng"]], 55, tolerance = 0.1)
  expect_equal(griglia$passo_lat * scala[["lat"]], 55, tolerance = 0.1)
  # poche celle da controllare, e ogni comune ha le sue
  expect_lt(mean(is.na(griglia$celle)), 0.1)
  expect_setequal(
    setdiff(unique(griglia$celle), c(0L, NA)),
    seq_len(nrow(comuni_cantieri()))
  )
  # preparata una volta sola
  expect_identical(griglia_comuni(), griglia)

  # il centro di una cella non marcata sta nel comune della cella
  set.seed(7)
  certe <- sample(which(!is.na(griglia$celle) & griglia$celle > 0), 300)
  colonna <- (certe - 1) %/% griglia$n_righe + 1
  riga <- (certe - 1) %% griglia$n_righe + 1
  lng <- griglia$lng0 + (colonna - 0.5) * griglia$passo_lng
  lat <- griglia$lat0 + (riga - 0.5) * griglia$passo_lat
  expect_identical(indice_comune_esatto(lng, lat), griglia$celle[certe])
})

test_that("la distanza da un comune \u00e8 zero dentro e cresce allontanandosi", {
  comuni <- comuni_cantieri()
  asiago <- comuni[comuni$comune == "ASIAGO", ]
  bassano <- comuni[comuni$comune == "BASSANO DEL GRAPPA", ]
  # dentro il comune la distanza \u00e8 zero
  expect_identical(
    distanza_dal_comune_m(bassano$latitudine, bassano$longitudine, "Bassano del Grappa"),
    0
  )
  # dal centro di Bassano il confine di Asiago dista una quindicina di km
  dal_centro <- distanza_dal_comune_m(bassano$latitudine, bassano$longitudine, "ASIAGO")
  expect_gt(dal_centro, 10000)
  expect_lt(dal_centro, 20000)
  # non supera la distanza dal centro abitato di Asiago
  expect_lt(
    dal_centro,
    distanza_haversine_m(
      bassano$latitudine,
      bassano$longitudine,
      asiago$latitudine,
      asiago$longitudine
    )
  )

  # punti in fila verso sud dal centro di Asiago: dentro il comune la
  # distanza \u00e8 zero, fuori cresce con il passo dei punti
  lat <- asiago$latitudine - (0:60) * 0.003
  lng <- rep(asiago$longitudine, length(lat))
  distanza <- distanza_dal_comune_m(lat, lng, "ASIAGO")
  dentro <- comune_geografico(lat, lng) %in% "ASIAGO"
  expect_true(all(distanza[dentro] == 0))
  expect_true(all(distanza[!dentro] > 0))
  expect_gt(sum(!dentro), 20)
  fuori <- distanza[!dentro]
  expect_false(is.unsorted(fuori))
  # tra un punto e il successivo ci sono circa 333 metri
  expect_true(all(diff(fuori) < 0.003 * metri_per_grado()[["lat"]] + 1))

  # con un limite, i punti lontani hanno una distanza per difetto ma oltre il
  # limite; quelli vicini la distanza vera
  limitata <- distanza_dal_comune_m(lat, lng, "ASIAGO", limite = 1000)
  expect_identical(limitata[distanza <= 1000], distanza[distanza <= 1000])
  expect_true(all(limitata[distanza > 1000] > 1000))
  expect_true(all(limitata <= distanza))

  # un comune mancante, o assente dalla tabella, non ha distanza
  expect_identical(
    distanza_dal_comune_m(rep(45.7667, 3), rep(11.7342, 3), c(NA, "MILANO", "ASIAGO"))[1:2],
    c(NA_real_, NA_real_)
  )
  expect_true(is.na(distanza_dal_comune_m(NA_real_, 11.7, "ASIAGO")))
  expect_identical(distanza_dal_comune_m(numeric(0), numeric(0), character(0)), numeric(0))
})

test_that("una lettura \u00e8 fuori dal comune dichiarato oltre un chilometro dal confine", {
  expect_identical(tolleranza_confine_m(), 1000)
  comuni <- comuni_cantieri()
  asiago <- comuni[comuni$comune == "ASIAGO", ]
  lat <- asiago$latitudine - (0:60) * 0.003
  lng <- rep(asiago$longitudine, length(lat))
  distanza <- distanza_dal_comune_m(lat, lng, "ASIAGO")
  expect_identical(oltre_il_comune(lat, lng, "ASIAGO"), distanza > 1000)
  # c'\u00e8 una fascia di punti fuori dal confine ma entro la tolleranza
  expect_gt(sum(distanza > 0 & distanza <= 1000), 0)
  expect_identical(
    oltre_il_comune(lat, lng, "ASIAGO", tolleranza = 5000),
    distanza > 5000
  )
  # senza un comune con cui confrontare la lettura non \u00e8 fuori
  expect_identical(
    oltre_il_comune(rep(41.9, 2), rep(12.5, 2), c(NA, "MILANO")),
    c(FALSE, FALSE)
  )
})

test_that("la geografia di una lettura parte dalla posizione", {
  comuni <- comuni_cantieri()
  centro <- function(comune) comuni[comuni$comune == comune, ]
  bassano <- centro("BASSANO DEL GRAPPA")
  cittadella <- centro("CITTADELLA")
  letture <- dplyr::tibble(
    RFID = c("A", "B", "C", "D", "E", "F"),
    presente_a_database = c(
      "Presente",
      "Presente",
      "Non Presente",
      "Non Presente",
      "Presente",
      "Non Presente"
    ),
    comune_da_database = c("Bassano del Grappa", "LIMENA", NA, NA, "MILANO", NA),
    comune_giro = c(
      "BASSANO DEL GRAPPA",
      "CITTADELLA",
      "CITTADELLA",
      "ASIAGO",
      "ASIAGO",
      NA
    ),
    cantiere = c(NA, NA, NA, NA, "FUORI ZONA", NA),
    latitudine = c(
      bassano$latitudine,
      cittadella$latitudine,
      cittadella$latitudine,
      cittadella$latitudine,
      41.9,
      41.9
    ),
    longitudine = c(
      bassano$longitudine,
      cittadella$longitudine,
      cittadella$longitudine,
      cittadella$longitudine,
      12.5,
      12.5
    )
  )
  esito <- aggiungi_geografia(letture)
  expect_identical(
    esito$comune_geografico,
    c("BASSANO DEL GRAPPA", rep("CITTADELLA", 3), NA, NA)
  )
  # il comune di lettura \u00e8 quello della posizione, qualunque fosse il giro
  # del calendario; fuori dai comuni serviti ha una voce sua
  expect_identical(
    esito$comune_lettura,
    c(
      "BASSANO DEL GRAPPA",
      rep("CITTADELLA", 3),
      "FUORI SERVIZIO AZIENDA",
      "FUORI SERVIZIO AZIENDA"
    )
  )
  expect_identical(fuori_servizio(), "FUORI SERVIZIO AZIENDA")
  # il comune assegnato \u00e8 quello della posizione; fuori dai comuni serviti
  # vale quello del database, mai quello del giro
  expect_identical(
    esito$comune_assegnato,
    c("BASSANO DEL GRAPPA", rep("CITTADELLA", 3), "MILANO", NA)
  )
  expect_identical(
    esito$cantiere,
    c("BASSANO", rep("CAMPOSAMPIERO", 3), "FUORI ZONA", NA)
  )
  # B: censito a Limena e letto a Cittadella dal giro di Cittadella. D: non
  # censito letto a Cittadella da un giro di Asiago. E: censito in un comune
  # fuori tabella, non confrontabile; il suo giro per\u00f2 era ad Asiago.
  expect_identical(esito$fuori_comune, c(FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_identical(esito$fuori_giro, c(FALSE, FALSE, FALSE, TRUE, TRUE, FALSE))

  # una lettura a 50 metri dal confine esterno del territorio non \u00e8 fuori
  # servizio: prende il comune pi\u00f9 vicino e il suo cantiere. A 150 metri s\u00ec
  confini <- poligoni_comuni()
  nord <- confini[which.max(confini$latitudine), ]
  al_margine <- dplyr::tibble(
    RFID = c("M1", "M2", "M3"),
    presente_a_database = c("Non Presente", "Non Presente", "Presente"),
    comune_da_database = c(NA, NA, nord$comune),
    comune_giro = nord$comune,
    latitudine = nord$latitudine + gradi_lat(c(50, 150, 50)),
    longitudine = nord$longitudine
  )
  esito_margine <- aggiungi_geografia(al_margine)
  expect_identical(
    esito_margine$comune_lettura,
    c(nord$comune, fuori_servizio(), nord$comune)
  )
  expect_identical(
    esito_margine$comune_geografico,
    c(nord$comune, NA, nord$comune)
  )
  expect_identical(
    esito_margine$comune_assegnato,
    c(nord$comune, NA, nord$comune)
  )
  expect_identical(
    esito_margine$cantiere,
    c(cantiere_di_comune(nord$comune), NA, cantiere_di_comune(nord$comune))
  )
  # il confronto con i comuni dichiarati resta quello di prima: a 150 metri dal
  # comune del giro, o del database, una lettura non \u00e8 fuori giro n\u00e9 fuori comune
  expect_false(any(esito_margine$fuori_giro))
  expect_false(any(esito_margine$fuori_comune))

  # una posizione mancante vale come una posizione fuori dai comuni serviti
  senza_punto <- letture
  senza_punto$latitudine[1] <- NA
  expect_identical(
    aggiungi_geografia(senza_punto)$comune_lettura[1],
    fuori_servizio()
  )
  # senza le colonne delle coordinate nessun comune viene dalla posizione:
  # resta quello del database
  senza <- aggiungi_geografia(
    letture[, setdiff(names(letture), c("latitudine", "longitudine"))]
  )
  expect_true(all(is.na(senza$comune_lettura)))
  expect_true(all(is.na(senza$comune_geografico)))
  expect_identical(
    senza$comune_assegnato,
    c("Bassano del Grappa", "LIMENA", NA, NA, "MILANO", NA)
  )
  expect_identical(
    senza$cantiere,
    c("BASSANO", "RUBANO", NA, NA, "FUORI ZONA", NA)
  )
  expect_false(any(senza$fuori_comune | senza$fuori_giro))
  # il comune di un RFID, per report e cluster: quello del database per un
  # censito, quello in cui \u00e8 letto per un non censito. D \u00e8 a Cittadella anche
  # se il calendario mandava il mezzo ad Asiago
  per_rfid <- geografia_per_gruppo(esito, indice_gruppi(esito$RFID))
  expect_identical(
    per_rfid$comune,
    c("Bassano del Grappa", "LIMENA", "CITTADELLA", "CITTADELLA", "MILANO", NA)
  )
  expect_identical(
    per_rfid$cantiere,
    c("BASSANO", "RUBANO", "CAMPOSAMPIERO", "CAMPOSAMPIERO", "FUORI ZONA", NA)
  )
})

test_that("due milioni di letture trovano il loro comune in pochi secondi", {
  skip_on_cran()
  set.seed(3)
  n <- 2e5
  lat <- stats::runif(n, 45.3, 46)
  lng <- stats::runif(n, 11.4, 12.05)
  tempo <- system.time(indice <- indice_comune_geografico(lat, lng))[["elapsed"]]
  expect_length(indice, n)
  # un decimo delle letture di un anno: molto meno di un secondo
  expect_lt(tempo, 2)
})
