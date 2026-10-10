test_that("il popup riporta tutti i campi della lettura", {
  popup <- create_popup_html(
    rfid = "RFD20250915001",
    servizio = "CARTA",
    servizio_atteso = NA,
    targa = "AB123CD",
    matricola = "VEH001",
    giorno_lettura = as.POSIXct("2025-09-15 08:30:00", tz = "UTC"),
    id_utenza = "UTZ042",
    presente = "Presente"
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
    d$RFID,
    d$servizio_transponder,
    d$servizio_atteso,
    d$targa_veicolo,
    d$matricola_veicolo,
    d$giorno_lettura,
    d$id_utenza,
    d$presente_a_database
  )
  expect_length(popup, nrow(d))
  expect_match(popup[5], "<b>Servizio:</b> N/A", fixed = TRUE)
  expect_match(popup[5], "<b>Utenza:</b> Non Censito", fixed = TRUE)

  # le righe geografiche compaiono solo nelle letture che hanno il dato
  expect_no_match(popup[1], "Comune", fixed = TRUE)
  expect_no_match(popup[1], "Cantiere", fixed = TRUE)
  con_geografia <- create_popup_html(
    d$RFID,
    d$servizio_transponder,
    d$servizio_atteso,
    d$targa_veicolo,
    d$matricola_veicolo,
    d$giorno_lettura,
    d$id_utenza,
    d$presente_a_database,
    comune = c(rep("LIMENA", 4), NA, NA),
    comune_lettura = c("LIMENA", "CITTADELLA", NA, NA, "ASIAGO", NA),
    cantiere = c(rep("RUBANO", 4), "ASIAGO", NA)
  )
  expect_match(
    con_geografia[1],
    "<b>Utenza:</b> U1<br><b>Comune:</b> LIMENA<br><b>Cantiere:</b> RUBANO<br>",
    fixed = TRUE
  )
  # il comune di lettura compare solo se è diverso da quello del database
  expect_no_match(con_geografia[1], "Comune di lettura", fixed = TRUE)
  expect_match(
    con_geografia[2],
    "<b>Comune:</b> LIMENA<br><b>Comune di lettura:</b> CITTADELLA<br>",
    fixed = TRUE
  )
  expect_match(
    con_geografia[5],
    "<b>Comune di lettura:</b> ASIAGO<br><b>Cantiere:</b> ASIAGO<br>",
    fixed = TRUE
  )
  expect_no_match(con_geografia[5], "<b>Comune:</b>", fixed = TRUE)
  expect_identical(con_geografia[6], popup[6])

  pericoloso <- create_popup_html(
    "<script>alert(1)</script>",
    "CARTA",
    NA,
    "T",
    "M",
    as.POSIXct("2025-09-15 08:30:00", tz = "UTC"),
    "U",
    "Presente"
  )
  expect_no_match(pericoloso, "<script>", fixed = TRUE)
  expect_match(pericoloso, "&lt;script&gt;", fixed = TRUE)
})

test_that("il pannello di dettaglio sceglie il messaggio giusto", {
  d <- letture_test()
  pannello <- function(letture) {
    as.character(crea_info_panel(analizza_rfid(letture)))
  }

  coerente <- pannello(d[d$RFID == "B", ])
  expect_match(coerente, "TRANSPONDER CENSITO", fixed = TRUE)
  expect_match(
    coerente,
    "regolarmente censito nel database aziendale come contenitore di VETRO",
    fixed = TRUE
  )
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
  expect_match(
    con_stima,
    "Stima Predittiva (da Calendario Mezzi)",
    fixed = TRUE
  )
  expect_match(con_stima, "100%", fixed = TRUE)

  senza_stima <- pannello(d[d$RFID == "C" & is.na(d$servizio_atteso), ])
  expect_match(senza_stima, "NON CENSITO NEL DATABASE", fixed = TRUE)
  expect_match(senza_stima, "Non sono disponibili informazioni", fixed = TRUE)
  expect_no_match(senza_stima, "Stima Predittiva", fixed = TRUE)
})

test_that("l'istogramma mostra una colonna per anno e mette in evidenza gli anni vuoti", {
  andamento <- dplyr::tibble(
    anno = 2021:2025,
    storico = c(4L, 0L, 8L, 6L, 0L),
    antenne = c(0L, 0L, 0L, 4L, 20L)
  )
  html <- as.character(istogramma_annuale(andamento))
  expect_match(html, "LETTURE PER ANNO", fixed = TRUE)
  expect_equal(lengths(regmatches(html, gregexpr("anni-colonna", html))), 5)
  expect_match(
    html,
    '<span class="anni-valore anni-zero">0</span>',
    fixed = TRUE
  )
  expect_equal(lengths(regmatches(html, gregexpr("anni-zero", html))), 1)
  # l'altezza è in proporzione all'anno con più letture: 20 nel 2025
  expect_match(
    html,
    'class="anni-segmento anni-antenne" style="height: 100.0%;"',
    fixed = TRUE
  )
  expect_match(
    html,
    'class="anni-segmento anni-storico" style="height: 40.0%;"',
    fixed = TRUE
  )
  # nel 2024 le due fonti stanno nella stessa colonna
  expect_match(html, "height: 20.0%;.*height: 30.0%;")
  expect_match(html, "Sistema precedente", fixed = TRUE)

  solo_antenne <- as.character(istogramma_annuale(
    dplyr::tibble(anno = 2024:2025, storico = c(0L, 0L), antenne = c(5L, 22L))
  ))
  expect_no_match(solo_antenne, "Sistema precedente", fixed = TRUE)
  expect_null(istogramma_annuale(andamento[0, ]))
  expect_null(istogramma_annuale(NULL))

  # nel pannello di dettaglio l'istogramma segue gli altri blocchi
  d <- letture_test()
  pannello <- as.character(crea_info_panel(
    analizza_rfid(d[d$RFID == "B", ]),
    andamento
  ))
  expect_lt(
    regexpr("TRANSPONDER CENSITO", pannello),
    regexpr("LETTURE PER ANNO", pannello)
  )
  expect_no_match(
    as.character(crea_info_panel(analizza_rfid(d[d$RFID == "B", ]))),
    "LETTURE PER ANNO"
  )
})

test_that("il riquadro statistiche riporta i conteggi", {
  esempio <- aggiungi_servizio_icona(dati_esempio())
  stat <- calcola_statistiche(deduplica_ultimo_rfid(esempio))
  html <- as.character(crea_box_statistiche(stat))
  expect_match(html, "STATISTICHE FILTRATE", fixed = TRUE)
  expect_match(html, "TOTALE RFID (Ultimo):", fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$totale), fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$presente), fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$non_presente), fixed = TRUE)
  expect_equal(stat$totale, 251)
  expect_equal(stat$presente + stat$non_presente, stat$totale)
  expect_match(html, "SECCO:", fixed = TRUE)
  # i non censiti: una riga per servizio stimato, poi quelli con la stima
  # incerta, che sulla mappa hanno il punto di domanda
  expect_match(html, "SECCO PAP:", fixed = TRUE)
  expect_match(
    html,
    sprintf("Stima incerta:</span>\\s*<b>%d</b>", stat$stima_incerta)
  )
  expect_gt(stat$stima_incerta, 5)
  expect_equal(sum(stat$atteso) + stat$stima_incerta, stat$non_presente)
  expect_equal(sum(stat$transponder), stat$presente)

  # con le letture filtrate compaiono RFID e letture di ogni cantiere
  expect_match(html, "RFID per Cantiere:", fixed = TRUE)
  con_letture <- as.character(crea_box_statistiche(calcola_statistiche(
    deduplica_ultimo_rfid(esempio),
    esempio$cantiere
  )))
  expect_match(con_letture, "RFID e letture per Cantiere:", fixed = TRUE)
  expect_match(con_letture, "<span>ASIAGO:</span>\\s*<b>30</b>")
  expect_match(con_letture, "669 letture", fixed = TRUE)
  # nel dataset di esempio ogni lettura ha un cantiere
  expect_no_match(con_letture, "Non assegnato:", fixed = TRUE)
  senza_alcune <- esempio
  senza_alcune$cantiere[senza_alcune$RFID == "RFD20241001301"] <- NA
  expect_match(
    as.character(crea_box_statistiche(calcola_statistiche(
      deduplica_ultimo_rfid(senza_alcune),
      senza_alcune$cantiere
    ))),
    "Non assegnato:",
    fixed = TRUE
  )
  # un dataset senza cantieri non ha la sezione
  senza_cantieri <- as.character(crea_box_statistiche(calcola_statistiche(
    letture_test()
  )))
  expect_no_match(senza_cantieri, "Cantiere", fixed = TRUE)

  vuoto <- as.character(crea_box_statistiche(calcola_statistiche(dati_esempio()[
    0,
  ])))
  expect_match(vuoto, "nessuno", fixed = TRUE)

  # soli non censiti con la stima incerta: sotto il servizio atteso resta la
  # loro riga, senza la voce "nessuno"
  incerti <- esempio[esempio$RFID == "RFD20250920250", ]
  solo_incerti <- as.character(crea_box_statistiche(calcola_statistiche(
    deduplica_ultimo_rfid(incerti)
  )))
  expect_match(solo_incerti, "Stima incerta:</span>\\s*<b>1</b>")
  expect_equal(lengths(regmatches(solo_incerti, gregexpr("nessuno", solo_incerti))), 1)
})

test_that("i numeri usano il separatore italiano delle migliaia", {
  expect_identical(formatta_numero(1234567), "1.234.567")
  expect_identical(formatta_numero(42L), "42")
  # i numeri tondi non passano alla notazione scientifica
  expect_identical(formatta_numero(1e5), "100.000")
  expect_identical(formatta_numero(c(2e6, 15)), c("2.000.000", "15"))
})

test_that("i conteggi scelgono tra singolare e plurale", {
  expect_identical(conta(1, "bidone", "bidoni"), "1 bidone")
  expect_identical(conta(0, "bidone", "bidoni"), "0 bidoni")
  expect_identical(conta(12500, "lettura", "letture"), "12.500 letture")
})

test_that("il pannello di dettaglio ha un blocco per i sacchetti", {
  sacchetto <- sprintf("00BD%020d", 1)
  pannello <- function(letture) {
    as.character(crea_info_panel(analizza_rfid(letture)))
  }

  censito <- pannello(letture_rfid(
    sacchetto,
    date_2025(3),
    servizio = servizio_sacchetti(),
    atteso = "SECCO PAP"
  ))
  expect_match(censito, "SACCHETTO CENSITO", fixed = TRUE)
  expect_match(censito, "Riconosciuto dal codice RFID", fixed = TRUE)
  # un sacchetto non è un contenitore con una tipologia di rifiuto
  expect_no_match(censito, "contenitore di", fixed = TRUE)
  expect_no_match(censito, "Tipo di Rifiuto", fixed = TRUE)

  # non censito: i giri che lo hanno letto, non una stima del servizio
  non_censito <- pannello(letture_rfid(
    sacchetto,
    date_2025(3),
    presente = "Non Presente",
    atteso = c("SECCO PAP", "SECCO PAP", "VETRO PAP")
  ))
  expect_match(non_censito, "SACCHETTO NON CENSITO NEL DATABASE", fixed = TRUE)
  expect_match(non_censito, "Giri che lo hanno letto", fixed = TRUE)
  expect_match(non_censito, "SECCO PAP:", fixed = TRUE)
  expect_no_match(non_censito, "Stima Predittiva", fixed = TRUE)
  # senza giri nel file resta il solo riconoscimento
  senza_giri <- pannello(letture_rfid(
    sacchetto,
    date_2025(2),
    presente = "Non Presente"
  ))
  expect_match(senza_giri, "SACCHETTO NON CENSITO NEL DATABASE", fixed = TRUE)
  expect_no_match(senza_giri, "Giri che lo hanno letto", fixed = TRUE)

  # un bidone conserva i suoi testi
  bidone <- pannello(letture_rfid("0000ABC123", date_2025(3)))
  expect_match(bidone, "TRANSPONDER CENSITO", fixed = TRUE)
  expect_no_match(bidone, "SACCHETTO", fixed = TRUE)
})
