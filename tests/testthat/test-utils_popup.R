test_that("il popup riporta tutti i campi della lettura", {
  popup <- create_popup_html(
    rfid = "RFD20250915001",
    servizio_atteso = "CARTA",
    targa = "AB123CD",
    matricola = "VEH001",
    giorno_lettura = as.POSIXct("2025-09-15 08:30:00", tz = "UTC"),
    id_utenza = "UTZ042",
    presente = "Presente"
  )
  expect_match(popup, "<b>RFID:</b> RFD20250915001", fixed = TRUE)
  expect_match(popup, "<b>Servizio Atteso:</b> CARTA", fixed = TRUE)
  # il servizio del database non ha più una riga sua: per un censito è il
  # servizio atteso
  expect_no_match(popup, "<b>Servizio:</b>", fixed = TRUE)
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
    d$servizio_atteso,
    d$targa_veicolo,
    d$matricola_veicolo,
    d$giorno_lettura,
    d$id_utenza,
    d$presente_a_database
  )
  expect_length(popup, nrow(d))
  expect_match(popup[5], "<b>Servizio Atteso:</b> SECCO", fixed = TRUE)
  expect_match(popup[6], "<b>Servizio Atteso:</b> N/A", fixed = TRUE)
  expect_match(popup[5], "<b>Utenza:</b> Non Censito", fixed = TRUE)
  # un censito senza utenza, come un sacchetto, non è un non censito
  sacchetto <- create_popup_html(
    "00BD00000000000000000001", "SECCO", "AB123CD", "VEH001",
    d$giorno_lettura[1], NA_character_, c("Presente", "Non Presente")
  )
  expect_match(sacchetto[1], "<b>Utenza:</b> N/A", fixed = TRUE)
  expect_match(sacchetto[2], "<b>Utenza:</b> Non Censito", fixed = TRUE)

  # le righe geografiche compaiono solo nelle letture che hanno il dato
  expect_no_match(popup[1], "Comune", fixed = TRUE)
  expect_no_match(popup[1], "Cantiere", fixed = TRUE)
  con_geografia <- create_popup_html(
    d$RFID,
    d$servizio_atteso,
    d$targa_veicolo,
    d$matricola_veicolo,
    d$giorno_lettura,
    d$id_utenza,
    d$presente_a_database,
    comune = c(rep("LIMENA", 4), NA, NA),
    comune_lettura = c("LIMENA", "LIMENA", "RUBANO", NA, "PADOVA", NA),
    comune_giro = c("LIMENA", "CITTADELLA", NA, NA, "ASIAGO", NA),
    cantiere = c(rep("RUBANO", 4), "ASIAGO", NA)
  )
  # il comune del database, quello della lettura, e il cantiere
  expect_match(
    con_geografia[1],
    "<b>Utenza:</b> U1<br><b>Comune a database:</b> LIMENA<br><b>Comune di lettura:</b> LIMENA<br><b>Cantiere:</b> RUBANO<br>",
    fixed = TRUE
  )
  # il comune del giro compare solo se è diverso da quello di lettura
  expect_no_match(con_geografia[1], "Comune del giro", fixed = TRUE)
  expect_match(
    con_geografia[2],
    "<b>Comune di lettura:</b> LIMENA<br><b>Comune del giro:</b> CITTADELLA<br>",
    fixed = TRUE
  )
  expect_match(
    con_geografia[3],
    "<b>Comune a database:</b> LIMENA<br><b>Comune di lettura:</b> RUBANO<br><b>Cantiere:</b>",
    fixed = TRUE
  )
  # una lettura senza comune di lettura riporta il solo comune del database
  expect_no_match(con_geografia[4], "Comune di lettura", fixed = TRUE)
  # un non censito letto fuori dai comuni serviti
  expect_match(
    con_geografia[5],
    "<b>Comune di lettura:</b> PADOVA<br><b>Comune del giro:</b> ASIAGO<br><b>Cantiere:</b> ASIAGO<br>",
    fixed = TRUE
  )
  expect_no_match(con_geografia[5], "Comune a database", fixed = TRUE)
  expect_identical(con_geografia[6], popup[6])

  pericoloso <- create_popup_html(
    "<script>alert(1)</script>",
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
  expect_match(con_stima, "Stima Predittiva del servizio:", fixed = TRUE)
  # la nota dice da dove viene la stima
  expect_match(
    con_stima,
    "la stima viene dal servizio atteso dei giri che lo hanno letto",
    fixed = TRUE
  )
  expect_no_match(con_stima, "da Calendario Mezzi", fixed = TRUE)
  expect_match(con_stima, "100%", fixed = TRUE)

  senza_stima <- pannello(d[d$RFID == "C" & is.na(d$servizio_atteso), ])
  expect_match(senza_stima, "NON CENSITO NEL DATABASE", fixed = TRUE)
  expect_match(senza_stima, "Non sono disponibili informazioni", fixed = TRUE)
  expect_no_match(senza_stima, "Stima Predittiva", fixed = TRUE)
})

test_that("il riquadro statistiche riporta i conteggi", {
  stat <- calcola_statistiche(ultime_esempio())
  html <- as.character(crea_box_statistiche(stat))
  expect_match(html, "STATISTICHE FILTRATE", fixed = TRUE)
  expect_match(html, "TOTALE RFID (Ultimo):", fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$totale), fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$presente), fixed = TRUE)
  expect_match(html, sprintf("<b>%d</b>", stat$non_presente), fixed = TRUE)
  expect_equal(stat$totale, 251)
  expect_equal(stat$presente + stat$non_presente, stat$totale)
  expect_match(html, "SECCO:", fixed = TRUE)
  # i censiti per servizio a database; i non censiti per servizio stimato,
  # poi quelli con la stima incerta, che sulla mappa hanno il punto di domanda
  expect_match(
    html,
    "Servizio a database, censiti:</div>\\s*<div class=\"stat-riga\">\\s*<span>[^<]*SECCO:"
  )
  expect_match(
    html,
    "Servizio stimato, non censiti:</div>\\s*<div class=\"stat-riga\">\\s*<span>[^<]*SECCO:"
  )
  expect_false(grepl("Servizio Transponder", html, fixed = TRUE))
  expect_match(
    html,
    sprintf("Stima incerta:</span>\\s*<b>%d</b>", stat$stima_incerta)
  )
  expect_gt(stat$stima_incerta, 5)
  expect_equal(sum(stat$servizio_stimato) + stat$stima_incerta, stat$non_presente)
  expect_equal(sum(stat$servizio_censiti), stat$presente)
  # i giri hanno il nome del calendario: il secco porta a porta ha la sua riga
  expect_match(
    html,
    "Giro del calendario:</div>\\s*<div class=\"stat-riga\">\\s*<span>[^<]*SECCO PAP:"
  )
  expect_equal(sum(stat$giri), stat$totale)
  expect_identical(names(stat$giri)[1], "SECCO PAP")
  # i bidoni con i servizi che non concordano
  expect_match(
    html,
    sprintf("Non coerenti:</span>\\s*<b>%d</b>", stat$non_coerenti)
  )
  expect_equal(stat$non_coerenti, 2)

  # con le letture filtrate compaiono RFID e letture di ogni cantiere
  expect_match(html, "RFID per Cantiere:", fixed = TRUE)
  tutte <- seleziona_periodo(archivio_esempio())
  con_letture <- as.character(crea_box_statistiche(calcola_statistiche(
    ultime_letture(tutte),
    conteggi_cantieri(tutte)
  )))
  expect_match(con_letture, "RFID e letture per Cantiere:", fixed = TRUE)
  expect_match(con_letture, "<span>ASIAGO:</span>\\s*<b>33</b>")
  expect_match(con_letture, "691 letture", fixed = TRUE)
  # nel dataset di esempio ogni lettura ha un cantiere
  expect_no_match(con_letture, "Non assegnato:", fixed = TRUE)
  senza_alcune <- dati_esempio()
  senza_alcune$cantiere[senza_alcune$RFID == "RFD20241001301"] <- NA
  selezione <- selezione_di(senza_alcune)
  expect_match(
    as.character(crea_box_statistiche(calcola_statistiche(
      ultime_letture(selezione),
      conteggi_cantieri(selezione)
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
  esempio <- dati_esempio()
  incerti <- esempio[esempio$RFID == "RFD20250920250", ]
  solo_incerti <- as.character(crea_box_statistiche(calcola_statistiche(
    ultime_di(incerti)
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
  expect_match(non_censito, "Servizio dei giri che lo hanno letto:", fixed = TRUE)
  expect_no_match(non_censito, "Calendario Mezzi", fixed = TRUE)
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

test_that("popup, dettaglio e statistiche segnalano le letture fuori comune e fuori giro", {
  popup <- create_popup_html(
    rfid = c("A", "B", "C", "D"),
    servizio_atteso = "CARTA",
    targa = "AB123CD",
    matricola = "VEH001",
    giorno_lettura = as.POSIXct("2025-03-01 08:00:00", tz = "UTC"),
    id_utenza = "U1",
    presente = "Presente",
    comune = c("LIMENA", "LIMENA", "LIMENA", "LIMENA"),
    comune_giro = c("LIMENA", "CITTADELLA", "ASIAGO", "ASIAGO"),
    cantiere = "RUBANO",
    comune_lettura = c("LIMENA", "CITTADELLA", "LIMENA", NA),
    coerenza_comune = c("Coerente", "Fuori comune", "Fuori giro", "Fuori comune")
  )
  expect_no_match(popup[1], "Fuori", fixed = TRUE)
  expect_match(
    popup[2],
    "Fuori comune:</b> a database LIMENA, letto a CITTADELLA<br>",
    fixed = TRUE
  )
  expect_no_match(popup[2], "Fuori giro", fixed = TRUE)
  expect_match(popup[3], "Fuori giro:</b> giro a ASIAGO, letto a LIMENA<br>", fixed = TRUE)
  expect_no_match(popup[3], "Fuori comune", fixed = TRUE)
  # una lettura senza comune di lettura
  expect_match(
    popup[4],
    "Fuori comune:</b> a database LIMENA, letto in un comune sconosciuto<br>",
    fixed = TRUE
  )
  expect_no_match(popup[4], "Fuori giro", fixed = TRUE)
  # senza la coerenza del comune il popup resta quello di sempre
  semplice <- create_popup_html("A", "CARTA", "AB123CD", "VEH001", Sys.time(), "U1", "Presente")
  expect_no_match(semplice, "Fuori", fixed = TRUE)

  # dettaglio: un blocco per il fuori comune, uno per il fuori giro
  d <- dati_esempio()
  spostato <- as.character(crea_info_panel(analizza_rfid(d[d$RFID == "RFD20250905100", ])))
  expect_match(spostato, "LETTO FUORI DAL SUO COMUNE", fixed = TRUE)
  expect_match(spostato, "<b>LIMENA</b>", fixed = TRUE)
  expect_match(spostato, "8 letture su 9", fixed = TRUE)
  expect_match(spostato, "in un altro comune", fixed = TRUE)
  expect_no_match(spostato, "km", fixed = TRUE)
  expect_match(spostato, "<b>CITTADELLA</b>", fixed = TRUE)
  expect_no_match(spostato, "FUORI DAL GIRO", fixed = TRUE)
  deposito <- as.character(crea_info_panel(analizza_rfid(d[d$RFID == "RFD20241001301", ])))
  expect_match(deposito, "LETTO FUORI DAL GIRO DEL MEZZO", fixed = TRUE)
  expect_match(deposito, "letto a CAMPOSAMPIERO", fixed = TRUE)
  expect_match(deposito, "scheda Ricerca Giro", fixed = TRUE)
  expect_equal(lengths(regmatches(deposito, gregexpr("giro a ", deposito, fixed = TRUE))), 5)
  coerente <- as.character(crea_info_panel(analizza_rfid(d[d$RFID == "RFD20250901001", ])))
  expect_no_match(coerente, "LETTO FUORI", fixed = TRUE)
  expect_null(blocco_coerenza_comune(NULL, 3))

  # statistiche: tre righe, anche a zero
  stat <- calcola_statistiche(ultime_esempio())
  html <- as.character(crea_box_statistiche(stat))
  expect_match(html, "Coerenza del comune:", fixed = TRUE)
  expect_match(
    html,
    sprintf("Coerenza ignota:</span>\\s*<b>%d</b>", stat$coerenza_comune_ignota)
  )
  expect_match(html, sprintf("Fuori comune:</span>\\s*<b>%d</b>", stat$fuori_comune))
  expect_match(html, sprintf("Fuori giro:</span>\\s*<b>%d</b>", stat$fuori_giro))
})

test_that("popup e dettaglio riportano il giro e i servizi che non concordano", {
  # senza le colonne il popup non ne parla
  semplice <- create_popup_html(
    rfid = "R1",
    servizio_atteso = "CARTA",
    targa = "AB123CD",
    matricola = "VEH001",
    giorno_lettura = as.POSIXct("2025-09-15 08:30:00", tz = "UTC"),
    id_utenza = "UTZ042",
    presente = "Presente"
  )
  expect_no_match(semplice, "Servizio Giro", fixed = TRUE)
  expect_no_match(semplice, "Servizio a Database", fixed = TRUE)
  expect_no_match(semplice, "non coerente", fixed = TRUE)

  popup <- create_popup_html(
    rfid = c("R1", "R2", "R3", "R4"),
    servizio_atteso = c("SECCO", "SECCO", "SECCO", NA),
    targa = "AB123CD",
    matricola = "VEH001",
    giorno_lettura = as.POSIXct("2025-09-15 08:30:00", tz = "UTC"),
    id_utenza = c("UTZ001", "UTZ002", NA, "UTZ003"),
    presente = c("Presente", "Presente", "Non Presente", "Presente"),
    servizio_giro = c("SECCO PAP", "SECCO PAP", "SECCO <PAP>", NA),
    coerenza_servizio = c("NO", "SI", NA, NA),
    servizio_transponder = c("CARTA", "SECCO", NA, "VETRO")
  )
  expect_length(popup, 4)
  # il servizio a database, quando c'è, sta prima del servizio atteso; il
  # giro ha il nome del calendario, e sta dopo
  expect_match(
    popup[1],
    paste0(
      "<b>Servizio a Database:</b> CARTA<br>",
      "<b>Servizio Atteso:</b> SECCO<br>",
      "<b>Servizio Giro:</b> SECCO PAP<br>"
    ),
    fixed = TRUE
  )
  expect_no_match(popup[3], "Servizio a Database", fixed = TRUE)
  expect_match(popup[3], "SECCO &lt;PAP&gt;", fixed = TRUE)
  expect_no_match(popup[4], "Servizio Giro", fixed = TRUE)
  # una lettura non coerente riporta il servizio a database e quello atteso
  expect_match(
    popup[1],
    "Servizio non coerente:</b> a database CARTA, atteso SECCO<br>",
    fixed = TRUE
  )
  expect_match(popup[1], "popup-incongruenza", fixed = TRUE)
  expect_no_match(popup[2], "non coerente", fixed = TRUE)
  expect_no_match(popup[3], "non coerente", fixed = TRUE)
  expect_no_match(popup[4], "non coerente", fixed = TRUE)

  # l'elenco dei servizi salta quello che manca
  expect_identical(
    descrivi_servizi(
      database = c("CARTA", "CARTA", NA, NA),
      atteso = c("SECCO", NA, "VETRO", NA),
      giro = c("SECCO PAP", "SECCO PAP", NA, NA)
    ),
    c(
      "a database CARTA, atteso SECCO, giro SECCO PAP",
      "a database CARTA, giro SECCO PAP",
      "atteso VETRO",
      ""
    )
  )
  expect_identical(descrivi_servizi(atteso = "CARTA"), "atteso CARTA")

  # nel dettaglio: un blocco con le letture non coerenti più recenti
  storia <- letture_rfid("X", date_2025(7), servizio = "CARTA")
  storia$servizio_giro <- "SECCO PAP"
  storia$servizio_giro[1] <- "CARTA/CARTONE PAP"
  storia$servizio_atteso <- uniforma_servizio(storia$servizio_giro)
  storia$coerenza_servizio <- coerenza_servizio(storia)
  pannello <- as.character(crea_info_panel(analizza_rfid(storia)))
  expect_match(pannello, "SERVIZI NON COERENTI", fixed = TRUE)
  expect_match(pannello, "6 letture su 7", fixed = TRUE)
  expect_match(
    pannello,
    "VEH001 \u00b7 a database CARTA, atteso SECCO, giro SECCO PAP",
    fixed = TRUE
  )
  expect_equal(
    lengths(regmatches(pannello, gregexpr("giro SECCO PAP", pannello))),
    5
  )
  expect_match(pannello, "letto in un giro di un altro rifiuto", fixed = TRUE)
  # una lettura sola, al singolare
  una <- storia[1:2, ]
  expect_match(
    as.character(crea_info_panel(analizza_rfid(una))),
    "1 lettura su 2",
    fixed = TRUE
  )
})

test_that("sulla mappa il popup riceve i servizi dalle letture", {
  righe <- ultime_esempio()[1:60, ]
  testo <- vapply(seq_len(nrow(righe)), function(i) popup_lettura(righe, i), character(1))
  con_database <- !is.na(righe$servizio_transponder)
  expect_true(any(con_database) && any(!con_database))
  expect_identical(
    grepl("Servizio a Database", testo, fixed = TRUE),
    con_database
  )
  expect_true(all(grepl("Servizio Giro", testo, fixed = TRUE)))
  # le letture non coerenti hanno una riga loro
  expect_identical(
    grepl("Servizio non coerente", testo, fixed = TRUE),
    righe$coerenza_servizio %in% "NO"
  )
})
