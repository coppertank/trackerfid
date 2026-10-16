test_that("il colore del marker dipende dallo stato a database", {
  expect_identical(
    get_color(c("Presente", "Non Presente", NA)),
    c("#2ecc71", "#e74c3c", "#e74c3c")
  )
})

test_that("il colore del cluster interpola rosso, giallo e verde", {
  expect_identical(genera_colore_cluster(0), "#e74c3c")
  expect_identical(genera_colore_cluster(0.5), "#f39c12")
  expect_identical(genera_colore_cluster(1), "#2ecc71")
  # punti intermedi: media dei canali
  expect_identical(genera_colore_cluster(0.25), "#ed7427")
  expect_identical(genera_colore_cluster(0.75), "#90b442")
  # vettoriale e limitato all'intervallo 0-1
  expect_identical(genera_colore_cluster(c(-1, 2)), c("#e74c3c", "#2ecc71"))
})

test_that("ogni servizio ha la sua icona, con ripiego per i non censiti", {
  info <- info_servizio(c(
    "SECCO",
    "CARTA",
    "VETRO",
    "UMIDO",
    "PLASTICA E METALLI",
    "VERDE E RAMAGLIE",
    NA,
    "ALTRO"
  ))
  expect_identical(
    info$icona,
    c(
      "trash-can",
      "file-lines",
      "wine-bottle",
      "leaf",
      "recycle",
      "tree",
      "question",
      "question"
    )
  )
  expect_equal(anyDuplicated(info$colore[1:7]), 0)

  # i giri del servizio atteso condividono l'icona della tipologia corrispondente
  giri <- info_servizio(c(
    "SECCO PAP",
    "CARTA CONT.STRADALI",
    "CARTA/CARTONE PAP",
    "VETRO PAP",
    "UMIDO CONT.STRADALI",
    "UMIDO PAP",
    "PLAST.CONT.STRADALI",
    "PLASTICA PAP",
    "VERDE PAP",
    "SERV.MANIFESTAZIONE",
    "PULIZIA TERRIT.",
    "SERVIZI MERCATI"
  ))
  expect_identical(
    giri$icona,
    c(
      "trash-can",
      "file-lines",
      "file-lines",
      "wine-bottle",
      "leaf",
      "leaf",
      "recycle",
      "recycle",
      "tree",
      "store",
      "broom",
      "store"
    )
  )
  expect_equal(anyDuplicated(servizi_config()$servizio), 0)
  expect_equal(nrow(info_servizio(character(0))), 0)
})

test_that("i sacchetti hanno una tipologia e un'icona loro", {
  sacchetti <- info_servizio(servizio_sacchetti())
  expect_identical(sacchetti$icona, "sack-xmark")
  expect_identical(tipologia_servizio(servizio_sacchetti()), "Sacchetti")
  # icona, colore ed emoji non sono quelli di un'altra tipologia
  altre <- servizi_config()[servizi_config()$tipologia != "Sacchetti", ]
  expect_false(sacchetti$icona %in% altre$icona)
  expect_false(sacchetti$colore %in% altre$colore)
  expect_false(sacchetti$emoji %in% altre$emoji)
  # il marker di un sacchetto non è quello con il punto di domanda
  expect_false(identical(
    svg_marker(servizio_sacchetti(), "Presente"),
    svg_marker(NA, "Presente")
  ))
  expect_match(html_legenda("cluster"), "Sacchetti", fixed = TRUE)
  # nell'ordine dei servizi stanno dopo le sei tipologie di rifiuto
  expect_identical(
    ordina_servizi(c("VERDE E RAMAGLIE", servizio_sacchetti(), "SECCO")),
    c("SECCO", "VERDE E RAMAGLIE", servizio_sacchetti())
  )
})

test_that("ogni cantiere ha un colore suo, lontano da quelli dello stato", {
  colori <- colori_cantieri()
  expect_setequal(names(colori), cantieri())
  expect_equal(anyDuplicated(colori), 0)
  expect_match(colori, "^#[0-9a-f]{6}$")
  # il verde, il rosso e l'arancio dicono lo stato a database e la quota di
  # censiti: i cantieri non li usano
  expect_false(any(colori %in% unlist(colori_stato())))
  expect_false(any(colori %in% genera_colore_cluster(c(0, 0.5, 1))))

  expect_identical(
    colore_cantiere(c("RUBANO", "ASIAGO", "RUBANO")),
    unname(colori[c("RUBANO", "ASIAGO", "RUBANO")])
  )
  # un cantiere senza colore, o mancante, è grigio
  expect_identical(colore_cantiere(c("ALTRO", NA)), rep("#6b7280", 2))
  expect_identical(colore_cantiere(character(0)), character(0))

  # la legenda di ogni mappa riporta i cantieri con il loro colore
  for (modalita in c("cluster", "rfid", "utenza")) {
    legenda <- html_legenda(modalita)
    expect_match(legenda, "Confini dei comuni: cantiere", fixed = TRUE)
    for (cantiere in names(colori)) {
      expect_match(legenda, cantiere, fixed = TRUE)
      expect_match(
        legenda,
        paste0("border-color: ", colori[[cantiere]]),
        fixed = TRUE
      )
    }
  }
})

test_that("le icone Font Awesome usate esistono", {
  for (nome in c(servizi_config()$icona, "question")) {
    glifo <- glifo_fa(nome)
    expect_match(glifo$viewBox, "^0 0 \\d+ \\d+$")
    expect_gt(nchar(glifo$path), 50)
  }
})

test_that("il marker SVG codifica stato, servizio e stile", {
  censito <- svg_marker("CARTA", "Presente")
  non_censito <- svg_marker(NA, "Non Presente")
  expect_match(censito, "#2ecc71", fixed = TRUE)
  expect_match(non_censito, "#e74c3c", fixed = TRUE)
  # forma diversa oltre al colore, per chi non distingue rosso e verde
  sagoma <- function(svg) sub(".*?<path d=\"([^\"]+)\".*", "\\1", svg)
  expect_false(identical(sagoma(censito), sagoma(non_censito)))

  expect_match(
    svg_marker("CARTA", "Presente", "ultimo"),
    "stroke-width=\"3\"",
    fixed = TRUE
  )
  precedente <- svg_marker("CARTA", "Presente", "precedente")
  expect_match(precedente, "stroke-width=\"1\"", fixed = TRUE)
  expect_match(precedente, "stroke-dasharray=\"5,5\"", fixed = TRUE)
})

test_that("i segnali sul marker dicono fuori comune e fuori giro", {
  semplice <- svg_marker("CARTA", "Presente")
  fuori_comune <- svg_marker("CARTA", "Presente", fuori_comune = TRUE)
  fuori_giro <- svg_marker("CARTA", "Presente", fuori_giro = TRUE)
  entrambi <- svg_marker("CARTA", "Presente", fuori_comune = TRUE, fuori_giro = TRUE)
  expect_no_match(semplice, "<text", fixed = TRUE)
  expect_match(fuori_comune, ">C</text>", fixed = TRUE)
  expect_no_match(fuori_comune, ">G</text>", fixed = TRUE)
  expect_match(fuori_giro, ">G</text>", fixed = TRUE)
  expect_match(entrambi, ">C</text>", fixed = TRUE)
  expect_match(entrambi, ">G</text>", fixed = TRUE)
  # colore, forma e icona del marker restano quelli di stato e servizio
  expect_true(startsWith(entrambi, sub("</svg>$", "", semplice)))

  icone <- indice_icona(
    rep("CARTA", 5),
    "Presente",
    fuori_comune = c(FALSE, TRUE, TRUE, NA, FALSE),
    fuori_giro = c(FALSE, FALSE, TRUE, FALSE, FALSE)
  )
  # la terza lettura è fuori comune e fuori giro insieme: con la coerenza del
  # comune non succede, e le icone non prevedono la combinazione
  expect_true(is.na(icone[3]))
  expect_equal(dplyr::n_distinct(icone[-3]), 2)
  # un valore mancante vale come nessun segnale
  expect_identical(icone[4], icone[1])
  expect_identical(icone[5], icone[1])

  # la legenda di ogni mappa spiega i due segnali; quella dei giri anche il
  # contorno del comune del giro
  for (modalita in c("cluster", "rfid", "utenza", "giro")) {
    legenda <- html_legenda(modalita)
    expect_match(legenda, "Coerenza comune", fixed = TRUE)
    expect_match(
      legenda,
      "C: censito fuori dal comune del database",
      fixed = TRUE
    )
    expect_match(
      legenda,
      "G: non censito fuori dal comune del giro",
      fixed = TRUE
    )
  }
  expect_match(html_legenda("giro"), "Comune del giro", fixed = TRUE)
  expect_no_match(html_legenda("cluster"), "legenda-comune-giro", fixed = TRUE)
})

test_that("la legenda cambia con la modalita", {
  expect_match(html_legenda("cluster"), "Cluster: % censiti", fixed = TRUE)
  expect_match(html_legenda("rfid"), "Ultima lettura", fixed = TRUE)
  expect_no_match(html_legenda("utenza"), "Cluster", fixed = TRUE)
  for (tipologia in unique(servizi_config()$tipologia)) {
    expect_match(html_legenda("utenza"), tipologia, fixed = TRUE)
  }
})

test_that("le bolle crescono con i bidoni e si colorano con i censiti", {
  expect_identical(
    lato_bolla(c(1, 9, 10, 99, 100, 999, 1000, 9999, 10000, 5e5)),
    c(40, 40, 46, 46, 54, 54, 62, 62, 72, 72)
  )

  # stesso colore dei cluster di marker di prima: da rosso a verde passando
  # per l'arancio
  colori <- colori_stato()
  expect_identical(
    genera_colore_cluster(c(0, 0.5, 1)),
    c(colori$non_presente, colori$misto, colori$presente)
  )
})

test_that("la tabella di conversione riporta i giri ai servizi del database", {
  tabella <- conversione_servizi()
  expect_named(tabella, c("servizio_calendario", "servizio", "tipologia"))
  # una riga per ogni nome delle tipologie, senza doppioni
  expect_identical(
    tabella$servizio_calendario,
    unlist(lapply(tipologie_servizio(), `[[`, "servizi"))
  )
  expect_equal(anyDuplicated(tabella$servizio_calendario), 0)
  # il nome uniforme è quello della tipologia, in maiuscolo, ed è un nome
  # riconosciuto, con l'icona della tipologia
  expect_identical(tabella$servizio, toupper(tabella$tipologia))
  expect_true(all(tabella$servizio %in% servizi_config()$servizio))
  # i sei servizi del database sono i nomi uniformi dei giri che raccolgono
  # un rifiuto
  del_database <- c(
    "SECCO",
    "CARTA",
    "VETRO",
    "UMIDO",
    "PLASTICA E METALLI",
    "VERDE E RAMAGLIE"
  )
  expect_true(all(del_database %in% tabella$servizio))
  # ogni nome uniforme converte in se stesso
  expect_identical(
    uniforma_servizio(unique(tabella$servizio)),
    unique(tabella$servizio)
  )
  converte <- function(nome) tabella$servizio[tabella$servizio_calendario == nome]
  expect_identical(converte("CARTA/CARTONE PAP"), "CARTA")
  expect_identical(converte("CARTA CONT.STRADALI"), "CARTA")
  expect_identical(converte("SECCO PAP"), "SECCO")
  expect_identical(converte("VETRO PAP"), "VETRO")
  expect_identical(converte("UMIDO CONT.STRADALI"), "UMIDO")
  expect_identical(converte("PLAST.CONT.STRADALI"), "PLASTICA E METALLI")
  expect_identical(converte("PLASTICA PAP"), "PLASTICA E METALLI")
  expect_identical(converte("VERDE PAP"), "VERDE E RAMAGLIE")
  # i giri che non raccolgono un rifiuto prendono il nome della loro tipologia
  expect_identical(converte("SERVIZI MERCATI"), "SERVIZI")
  expect_identical(converte("SERV.MANIFESTAZIONE"), "SERVIZI")
  expect_identical(converte("PULIZIA TERRIT."), "PULIZIA TERRITORIO")
  # la conversione non cambia la tipologia, quindi nemmeno icona e colore
  expect_identical(
    tipologia_servizio(tabella$servizio),
    tipologia_servizio(tabella$servizio_calendario)
  )
  expect_identical(tabella$tipologia, tipologia_servizio(tabella$servizio))
})

test_that("i nomi dei servizi si uniformano senza badare a maiuscole e spazi", {
  expect_identical(
    uniforma_servizio(c(
      "CARTA/CARTONE PAP",
      " carta cont.stradali ",
      "Secco Pap",
      "SECCO",
      "SERVIZI MERCATI",
      "GIRO NUOVO",
      NA
    )),
    c("CARTA", "CARTA", "SECCO", "SECCO", "SERVIZI", "GIRO NUOVO", NA)
  )
  # applicata due volte dà lo stesso risultato
  nomi <- servizi_config()$servizio
  expect_identical(
    uniforma_servizio(uniforma_servizio(nomi)),
    uniforma_servizio(nomi)
  )
  expect_identical(uniforma_servizio(character(0)), character(0))
  expect_identical(uniforma_servizio(NA), NA_character_)
  # vale anche per un fattore
  expect_identical(uniforma_servizio(factor(c("VETRO PAP", "UMIDO"))), c("VETRO", "UMIDO"))
})

test_that("la legenda si apre e si chiude dalla sua intestazione", {
  for (modalita in c("cluster", "rfid", "utenza", "giro")) {
    legenda <- html_legenda(modalita)
    # aperta all'avvio; `script.js` applica poi la scelta ricordata
    expect_match(legenda, '^<details open class="legenda-dettagli">')
    expect_match(
      legenda,
      '<summary class="legenda-intestazione" title="Mostra o nascondi la legenda">Legenda</summary>',
      fixed = TRUE
    )
  }
})
