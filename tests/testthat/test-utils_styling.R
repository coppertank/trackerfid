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
    "ASSISTENTE SERVIZI",
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
      "user-gear",
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

test_that("le icone Leaflet sono una per lettura", {
  icone <- get_leaflet_icon(
    c("SECCO", "SECCO", NA, "CARTA"),
    c("Presente", "Presente", "Non Presente", "Presente")
  )
  expect_length(icone$iconUrl, 4)
  expect_identical(icone$iconUrl[1], icone$iconUrl[2])
  expect_equal(dplyr::n_distinct(icone$iconUrl), 3)
  expect_true(all(startsWith(icone$iconUrl, "data:image/svg+xml")))
})

test_that("la funzione JavaScript dei cluster usa gli stessi colori di R", {
  js <- js_icona_cluster()
  expect_match(js, "[231,76,60]", fixed = TRUE)
  expect_match(js, "[243,156,18]", fixed = TRUE)
  expect_match(js, "[46,204,113]", fixed = TRUE)
  expect_match(js, "options.censito", fixed = TRUE)
})

test_that("la legenda cambia con la modalita", {
  expect_match(html_legenda("cluster"), "Cluster: % censiti", fixed = TRUE)
  expect_match(html_legenda("rfid"), "Ultima lettura", fixed = TRUE)
  expect_no_match(html_legenda("utenza"), "Cluster", fixed = TRUE)
  for (tipologia in unique(servizi_config()$tipologia)) {
    expect_match(html_legenda("utenza"), tipologia, fixed = TRUE)
  }
})

test_that("le bolle della vista aggregata crescono con i bidoni e si colorano con i censiti", {
  expect_identical(
    lato_bolla(c(1, 9, 10, 99, 100, 999, 1000, 9999, 10000, 5e5)),
    c(40, 40, 46, 46, 54, 54, 62, 62, 72, 72)
  )

  n <- c(120, 8, 0, 25000)
  censiti <- c(120, 2, 0, 12500)
  svg <- svg_bolla(n, censiti)
  expect_length(svg, 4)
  expect_match(svg[1], "width='54' height='54'", fixed = TRUE)
  expect_match(svg[1], ">120</text>", fixed = TRUE)
  expect_match(svg[1], ">100%</text>", fixed = TRUE)
  expect_match(svg[2], ">25%</text>", fixed = TRUE)
  expect_match(svg[4], ">25.000</text>", fixed = TRUE)
  expect_match(svg[4], ">50%</text>", fixed = TRUE)
  # un gruppo vuoto non divide per zero
  expect_match(svg[3], ">0%</text>", fixed = TRUE)
  # stesso colore dei cluster di marker disegnati dal browser
  colori <- genera_colore_cluster(c(1, 0.25, 0, 0.5))
  for (i in seq_along(svg)) {
    expect_match(svg[i], sprintf("fill='%s'", colori[i]), fixed = TRUE)
  }

  icone <- icone_bolle(n[1:2], censiti[1:2])
  expect_identical(icone$iconWidth, c(54, 40))
  expect_identical(icone$iconHeight, c(54, 40))
  # l'ancora è il centro della bolla
  expect_identical(icone$iconAnchorX, c(27, 20))
  expect_identical(icone$iconAnchorY, c(27, 20))
  expect_match(icone$iconUrl, "^data:image/svg\\+xml")
  expect_match(utils::URLdecode(icone$iconUrl[1]), ">120</text>", fixed = TRUE)
})
