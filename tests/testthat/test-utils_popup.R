test_that("il popup riporta tutti i campi della lettura", {
  popup <- create_popup_html(
    rfid = "RFD20250915001", servizio = "CARTA", servizio_atteso = NA,
    targa = "AB123CD", matricola = "VEH001",
    giorno_lettura = as.POSIXct("2025-09-15 08:30:00", tz = "UTC"),
    id_utenza = "UTZ042", presente = "Presente"
  )
  expect_match(popup, "<b>RFID:</b> RFD20250915001", fixed = TRUE)
  expect_match(popup, "<b>Servizio:</b> CARTA", fixed = TRUE)
  expect_match(popup, "<b>Servizio Atteso:</b> N/A", fixed = TRUE)
  expect_match(popup, "<b>Targa:</b> AB123CD", fixed = TRUE)
  expect_match(popup, "<b>Matricola:</b> VEH001", fixed = TRUE)
  expect_match(popup, "<b>Lettura:</b> 15/09/2025 08:30:00", fixed = TRUE)
  expect_match(popup, "<b>Utenza:</b> UTZ042", fixed = TRUE)
  expect_match(popup, "<b>Censito:</b> Presente", fixed = TRUE)
})

test_that("il popup è vettoriale, gestisce i valori mancanti e fa l'escape dell'HTML", {
  d <- letture_test()
  popup <- create_popup_html(
    d$RFID, d$servizio_transponder, d$servizio_atteso, d$targa_veicolo,
    d$matricola_veicolo, d$giorno_lettura, d$id_utenza, d$presente_a_database
  )
  expect_length(popup, nrow(d))
  expect_match(popup[5], "<b>Servizio:</b> N/A", fixed = TRUE)
  expect_match(popup[5], "<b>Utenza:</b> Non Censito", fixed = TRUE)

  pericoloso <- create_popup_html(
    "<script>alert(1)</script>", "CARTA", NA, "T", "M",
    as.POSIXct("2025-09-15 08:30:00", tz = "UTC"), "U", "Presente"
  )
  expect_no_match(pericoloso, "<script>", fixed = TRUE)
  expect_match(pericoloso, "&lt;script&gt;", fixed = TRUE)
})

test_that("il pannello di dettaglio sceglie il messaggio giusto", {
  d <- letture_test()
  pannello <- function(letture) as.character(crea_info_panel(analizza_rfid(letture)))

  coerente <- pannello(d[d$RFID == "B", ])
  expect_match(coerente, "TRANSPONDER CENSITO", fixed = TRUE)
  expect_match(coerente, "regolarmente censito nel database aziendale come contenitore di VETRO", fixed = TRUE)
  expect_no_match(coerente, "CAMBIO DI PROPRIETARIO", fixed = TRUE)

  cambio <- pannello(d[d$RFID == "A", ])
  expect_match(cambio, "CAMBIO DI SERVIZIO RILEVATO", fixed = TRUE)
  expect_match(cambio, "01/09/2025 08:00", fixed = TRUE)
  expect_match(cambio, "(ATTUALE)", fixed = TRUE)
  expect_match(cambio, "Ultimo Servizio: </b>\\s*CARTA")
  # cambio di utenza segnalato in aggiunta
  expect_match(cambio, "CAMBIO DI PROPRIETARIO", fixed = TRUE)
  expect_match(cambio, "[ATTUALE]", fixed = TRUE)
  # la cronologia è in ordine di tempo
  expect_lt(regexpr("01/09/2025", cambio), regexpr("10/09/2025", cambio))

  con_stima <- pannello(d[d$RFID == "C", ])
  expect_match(con_stima, "NON CENSITO NEL DATABASE", fixed = TRUE)
  expect_match(con_stima, "Stima Predittiva (da Calendario Mezzi)", fixed = TRUE)
  expect_match(con_stima, "100%", fixed = TRUE)

  senza_stima <- pannello(d[d$RFID == "C" & is.na(d$servizio_atteso), ])
  expect_match(senza_stima, "NON CENSITO NEL DATABASE", fixed = TRUE)
  expect_match(senza_stima, "Non sono disponibili informazioni", fixed = TRUE)
  expect_no_match(senza_stima, "Stima Predittiva", fixed = TRUE)
})

test_that("il riquadro statistiche riporta i conteggi", {
  stat <- calcola_statistiche(deduplica_ultimo_rfid(dati_esempio()))
  html <- as.character(crea_box_statistiche(stat))
  expect_match(html, "STATISTICHE FILTRATE", fixed = TRUE)
  expect_match(html, "TOTALE RFID (Ultimo):", fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$totale), fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$presente), fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$non_presente), fixed = TRUE)
  expect_equal(stat$totale, 239)
  expect_equal(stat$presente + stat$non_presente, stat$totale)
  expect_match(html, "SECCO:", fixed = TRUE)

  vuoto <- as.character(crea_box_statistiche(calcola_statistiche(dati_esempio()[0, ])))
  expect_match(vuoto, "nessuno", fixed = TRUE)
})

test_that("i numeri usano il separatore italiano delle migliaia", {
  expect_identical(formatta_numero(1234567), "1.234.567")
  expect_identical(formatta_numero(42L), "42")
})
