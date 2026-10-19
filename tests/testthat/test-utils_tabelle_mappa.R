test_that("il riquadro di una tabella sopra la mappa nasce aperto, con classi sue", {
  html <- as.character(riquadro_tabella_mappa(
    "Giri trovati",
    htmltools::tags$p("contenuto"),
    nota = "Una nota",
    classe = "tabella-giri"
  ))
  expect_match(html, '^<div class="tabella-mappa tabella-giri">')
  expect_match(html, '<details open class="tabella-dettagli">', fixed = TRUE)
  expect_match(
    html,
    '<summary class="tabella-intestazione" title="Mostra o nascondi la tabella">Giri trovati</summary>',
    fixed = TRUE
  )
  expect_match(html, '<div class="tabella-nota">Una nota</div>', fixed = TRUE)
  expect_match(html, "<p>contenuto</p>", fixed = TRUE)
  # le classi della legenda no: il suo gestore in script.js non lo tocca
  expect_no_match(html, "legenda", fixed = TRUE)
  senza_nota <- as.character(riquadro_tabella_mappa("Titolo"))
  expect_match(senza_nota, '^<div class="tabella-mappa">')
  expect_no_match(senza_nota, "tabella-nota", fixed = TRUE)
})

test_that("i testi delle tabelle sono in italiano", {
  lingua <- lingua_reactable("Niente")
  expect_identical(lingua$noData, "Niente")
  expect_identical(lingua$pageNext, "Successiva")
  expect_identical(lingua$pagePrevious, "Precedente")
  expect_match(lingua$pageInfo, "di {rows} righe", fixed = TRUE)
  expect_s3_class(tema_tabelle_mappa(), "reactableTheme")
})

test_that("la tabella delle letture riporta tutte le letture, per RFID, dalla più recente", {
  selezione <- seleziona_periodo(archivio_esempio(), periodo_anno(2025))
  trovate <- cerca_per_rfid(
    selezione,
    c("D250905100", "D241001301", "D250901001")
  )
  tabella <- tabella_letture_rfid(trovate)
  expect_s3_class(tabella, "reactable")
  dati <- dati_reactable(tabella)
  expect_named(dati, c("RFID", "Lettura", "Mezzo", "Giro"))
  # tutte le letture, anche quelle che la mappa non disegna
  expect_equal(nrow(dati), nrow(trovate))
  expect_gt(nrow(dati), nrow(limita_letture_ricerca(trovate)))
  # in ordine di RFID e, per ogni RFID, dalla più recente
  expect_false(is.unsorted(dati$RFID))
  ordinate <- trovate[order(trovate$RFID, -as.numeric(trovate$giorno_lettura)), ]
  expect_identical(dati$Lettura, formatta_data_ora(ordinate$giorno_lettura))
  expect_identical(
    dati$Mezzo,
    paste(ordinate$matricola_veicolo, ordinate$targa_veicolo)
  )
  # il giro del calendario, con il suo nome
  expect_identical(dati$Giro, ordinate$servizio_giro)
  expect_true("SECCO PAP" %in% dati$Giro)

  attributi <- attributi_reactable(tabella)
  expect_identical(attributi$groupBy, list("RFID"))
  # la riga di un RFID riassume giorni, mezzi e giri
  colonne <- stats::setNames(attributi$columns, vapply(attributi$columns, `[[`, "", "id"))
  expect_identical(colonne$Lettura$name, "Data e ora")
  expect_s3_class(colonne$Lettura$aggregate, "JS_EVAL")
  expect_identical(colonne$Mezzo$aggregate, "frequency")
  expect_identical(colonne$Giro$name, "Servizio del giro")
  expect_identical(colonne$Giro$aggregate, "frequency")
  # più RFID: i gruppi sono chiusi; uno solo: è già aperto
  expect_null(attributi$defaultExpanded)
  uno <- tabella_letture_rfid(trovate[trovate$RFID == "D250905100", ])
  expect_true(attributi_reactable(uno)$defaultExpanded)
  expect_true(attributi$paginateSubRows)
})

test_that("la tabella delle letture usa il servizio atteso se manca il giro", {
  letture <- letture_test()
  letture$is_ultimo <- FALSE
  letture <- letture[names(letture) != "servizio_giro"]
  dati <- dati_reactable(tabella_letture_rfid(letture))
  ordinate <- letture[order(letture$RFID, -as.numeric(letture$giorno_lettura)), ]
  expect_identical(dati$Giro, ordinate$servizio_atteso)
})
