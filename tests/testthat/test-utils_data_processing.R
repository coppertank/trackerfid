test_that("il dataset di esempio viene letto e convertito nei tipi corretti", {
  esito <- carica_dataset(percorso_dataset_esempio())
  dati <- esito$dati

  expect_identical(names(dati), colonne_dataset())
  expect_s3_class(dati$giorno_lettura, "POSIXct")
  expect_type(dati$volume_previsto, "double")
  expect_type(dati$numero_raccolte_annue_previste, "double")
  for (campo in c(
    colonne_geografiche(),
    "comune_assegnato",
    "comune_geografico",
    "coerenza_comune"
  )) {
    expect_type(dati[[campo]], "character")
  }
  expect_true(all(dati$coerenza_comune %in% stati_coerenza_comune()))
  expect_type(dati$latitudine, "double")
  expect_type(dati$longitudine, "double")
  expect_type(dati$RFID, "character")
  expect_setequal(unique(dati$presente_a_database), stati_database())
  expect_length(esito$avvisi, 0)
})

test_that("legge CSV con punto e virgola, virgola decimale e date italiane", {
  percorso <- scrivi_csv(c(
    gsub(",", ";", intestazione_transponder),
    "15/09/2025 08:30:00;AB123CD;VEH001;R1;Presente;carta;secco pap;007;41,9028;12,4964",
    "16/09/2025 09:00;AB123CD;VEH001;R2;non presente;;SECCO;;41,9100;12,5000"
  ))
  dati <- carica_dataset(percorso)$dati

  expect_equal(
    dati$giorno_lettura[1],
    as.POSIXct("2025-09-15 08:30:00", tz = "UTC")
  )
  expect_equal(
    dati$giorno_lettura[2],
    as.POSIXct("2025-09-16 09:00:00", tz = "UTC")
  )
  expect_equal(dati$latitudine, c(41.9028, 41.91))
  expect_equal(dati$longitudine, c(12.4964, 12.5))
  # normalizzazione dei testi e zeri iniziali conservati
  expect_identical(dati$servizio_transponder, c("CARTA", NA))
  expect_identical(dati$servizio_atteso, c("SECCO PAP", "SECCO"))
  expect_identical(dati$presente_a_database, c("Presente", "Non Presente"))
  expect_identical(dati$id_utenza, c("007", NA))
})

test_that("i nomi delle colonne non dipendono da maiuscole e ordine", {
  percorso <- scrivi_csv(c(
    "rfid,GIORNO_LETTURA,targa_veicolo,matricola_veicolo,presente_a_database,Servizio_Atteso,ORIGINE_SERVIZIO,id_utenza,longitudine,latitudine,extra",
    "R1,2025-09-15 08:30:00,AB123CD,VEH001,Presente,CARTA,database,UTZ001,12.4964,41.9028,x"
  ))
  dati <- carica_dataset(percorso)$dati
  expect_identical(names(dati), colonne_dataset())
  expect_equal(dati$latitudine, 41.9028)
})

test_that("le colonne mancanti producono un errore esplicito", {
  percorso <- scrivi_csv(c(
    "giorno_lettura,targa_veicolo,RFID,latitudine",
    "2025-09-15 08:30:00,AB123CD,R1,41.9"
  ))
  errore <- expect_error(carica_dataset(percorso), class = "errore_validazione")
  expect_match(conditionMessage(errore), "Colonne mancanti")
  expect_match(conditionMessage(errore), "matricola_veicolo")
  expect_match(conditionMessage(errore), "longitudine")
})

test_that("i valori non interpretabili sono segnalati con il numero di riga", {
  percorso <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,41.9,12.5",
    "non-una-data,AB123CD,VEH001,R2,Forse,CARTA,UTZ001,abc,999"
  ))
  errore <- expect_error(carica_dataset(percorso), class = "errore_validazione")
  messaggi <- errore$messaggi

  expect_length(messaggi, 4)
  expect_match(messaggi[1], "giorno_lettura.*Righe del file: 3")
  expect_match(messaggi[2], "latitudine.*non numerici")
  expect_match(messaggi[3], "longitudine.*fuori dall'intervallo")
  expect_match(messaggi[4], "presente_a_database.*Forse")
})

test_that("un file vuoto o senza righe produce un errore esplicito", {
  expect_error(carica_dataset(scrivi_csv("")), class = "errore_validazione")
  expect_error(
    carica_dataset(scrivi_csv(intestazione_csv)),
    class = "errore_validazione"
  )
})

test_that("le righe senza campi essenziali sono scartate con un avviso", {
  percorso <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,41.9,12.5",
    "2025-09-15 09:30:00,AB123CD,VEH001,R2,Presente,CARTA,UTZ001,,12.5",
    "2025-09-15 10:30:00,AB123CD,VEH001,,Presente,CARTA,UTZ001,41.9,12.5"
  ))
  esito <- carica_dataset(percorso)
  expect_identical(esito$dati$RFID, "R1")
  expect_true(any(grepl("2 righe scartate", esito$avvisi)))
})

test_that("le incoerenze del dataset generano avvisi non bloccanti", {
  percorso <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Non Presente,CARTA,UTZ001,41.9,12.5",
    "2025-09-15 09:30:00,AB123CD,VEH002,R2,Presente,CARTA,UTZ001,41.9,12.5"
  ))
  esito <- carica_dataset(percorso)
  expect_equal(nrow(esito$dati), 2)
  expect_length(esito$avvisi, 3)
  expect_match(esito$avvisi[1], "Colonne facoltative assenti")
  expect_match(esito$avvisi[2], "Non Presente")
  expect_match(esito$avvisi[3], "univoca")
})

test_that("l'ultima lettura di ogni RFID si sceglie in DuckDB", {
  ultimi <- ultime_di(letture_test())
  expect_equal(nrow(ultimi), 3)
  expect_setequal(ultimi$RFID, c("A", "B", "C"))
  expect_equal(
    ultimi$giorno_lettura[ultimi$RFID == "A"],
    as.POSIXct("2025-09-20 08:00:00", tz = "UTC")
  )
  # le colonne del dataset, il servizio dell'icona e la posizione nel file
  expect_identical(
    names(ultimi),
    c(colonne_dataset(), "servizio_icona", "riga")
  )
  # dalla più recente alla meno recente
  expect_false(is.unsorted(rev(ultimi$giorno_lettura)))
  # a parità di istante resta la lettura che viene prima nel file
  doppie <- letture_test()[c(1, 1), ]
  doppie$servizio_transponder <- c("PRIMA", "SECONDA")
  expect_identical(ultime_di(doppie)$servizio_transponder, "PRIMA")
  # si possono chiedere solo alcune colonne
  expect_named(
    ultime_letture(selezione_di(letture_test()), colonne = c("RFID", "latitudine")),
    c("RFID", "latitudine", "riga")
  )
})

test_that("il filtro per periodo include gli estremi", {
  filtrate <- filtra_periodo(
    letture_test(),
    as.Date(c("2025-09-05", "2025-09-10"))
  )
  expect_setequal(
    format(filtrate$giorno_lettura, "%d"),
    c("05", "06", "07", "10")
  )
  expect_equal(nrow(filtra_periodo(letture_test(), NULL)), 6)
})

# Codice di un sacchetto: 24 caratteri con il prefisso 00BD.
codice_sacchetto <- function(n) sprintf("00BD%020d", n)

# Letture che superano i filtri.
filtrate <- function(d, ...) d[maschera_di(d, ...), , drop = FALSE]

test_that("i filtri per stato e servizio si combinano correttamente", {
  d <- letture_test()

  expect_equal(nrow(filtrate(d)), 6)
  expect_setequal(filtrate(d, stato_db = "Presente")$RFID, c("A", "B"))
  expect_equal(nrow(filtrate(d, stato_db = character(0))), 0)

  # servizio, per esclusione. Per un censito vale il servizio a database di
  # ogni lettura. Per un non censito vale il
  # servizio stimato dell'RFID: C ha una lettura con il giro del secco e una
  # senza giro, la sua stima è SECCO, e togliendo SECCO escono tutte e due,
  # con la lettura di A fatta quando a database era un contenitore del secco.
  expect_identical(filtrate(d, servizi_esclusi = "SECCO")$RFID, c("A", "A", "B"))
  # un servizio mancante non è mai tra quelli deselezionati
  expect_identical(
    filtrate(d, servizi_esclusi = c("SECCO", NA))$RFID,
    c("A", "A", "B")
  )
  expect_identical(
    filtrate(d, servizi_esclusi = "CARTA")$RFID,
    c("A", "B", "C", "C")
  )
  expect_equal(nrow(filtrate(d, servizi_esclusi = c("CARTA", "VETRO"))), 3)
  # la stima di C è netta: la casella della stima incerta non lo tocca
  expect_equal(nrow(filtrate(d, includi_stima_incerta = FALSE)), 6)
  # lo stato a database si sceglie a parte
  expect_identical(
    filtrate(d, stato_db = "Presente", servizi_esclusi = "VETRO")$RFID,
    c("A", "A", "A")
  )

  # sul dataset di esempio: la stessa regola scritta lettura per lettura
  esempio <- aggiungi_servizio_icona(dati_esempio())
  stimata <- esempio$presente_a_database == "Non Presente" |
    rfid_sacchetto(esempio$RFID)
  expect_identical(
    maschera_di(
      esempio,
      servizi_esclusi = "SECCO",
      includi_stima_incerta = FALSE
    ),
    ifelse(
      stimata,
      !is.na(esempio$servizio_icona) & esempio$servizio_icona != "SECCO",
      esempio$servizio_transponder != "SECCO"
    )
  )
  # per un censito la voce dei filtri è il servizio a database della lettura,
  # non quello atteso dal giro
  expect_identical(
    servizio_mostrato(esempio)[!stimata],
    esempio$servizio_transponder[!stimata]
  )
})

test_that("il servizio atteso filtra i censiti per lettura e i non censiti per stima", {
  # Come nei dati reali: ogni lettura ha il servizio atteso. Per un censito è
  # quello del database, che qui cambia una volta.
  d <- dplyr::bind_rows(
    letture_rfid(
      "CENSITO",
      date_2025(4),
      servizio = c("CARTA", "CARTA", "VETRO", "CARTA")
    ),
    letture_rfid(
      "NETTO",
      date_2025(5),
      presente = "Non Presente",
      atteso = c(rep("SECCO PAP", 4), "VETRO PAP")
    ),
    letture_rfid(
      "INCERTO",
      date_2025(4),
      presente = "Non Presente",
      atteso = c("SECCO PAP", "SECCO PAP", "VETRO PAP", "CARTA/CARTONE PAP")
    ),
    letture_rfid("MUTO", date_2025(2), presente = "Non Presente")
  )
  tutti <- c("CENSITO", "NETTO", "INCERTO", "MUTO")
  rimasti <- function(...) unique(filtrate(d, ...)$RFID)
  expect_setequal(rimasti(), tutti)

  # togliere il secco toglie tutte le letture del non censito stimato secco;
  # il censito, che a database è un contenitore della carta, resta
  expect_setequal(
    rimasti(servizi_esclusi = "SECCO PAP"),
    c("CENSITO", "INCERTO", "MUTO")
  )
  expect_equal(nrow(filtrate(d, servizi_esclusi = "SECCO PAP")), 4 + 4 + 2)
  # per il censito conta il servizio di ogni lettura: togliere il vetro toglie
  # la sola lettura fatta quando a database era un contenitore del vetro
  expect_setequal(rimasti(servizi_esclusi = "VETRO"), tutti)
  expect_equal(nrow(filtrate(d, servizi_esclusi = "VETRO")), 3 + 5 + 4 + 2)
  expect_setequal(
    rimasti(servizi_esclusi = c("CARTA", "VETRO")),
    c("NETTO", "INCERTO", "MUTO")
  )
  # un servizio che non è la stima di nessun RFID non toglie niente
  expect_setequal(rimasti(servizi_esclusi = "VETRO PAP"), tutti)
  # stima incerta: nessuna tipologia all'80%, oppure nessun giro
  expect_setequal(
    rimasti(includi_stima_incerta = FALSE),
    c("CENSITO", "NETTO")
  )
  # tolta ogni voce non resta niente; lo stato a database si sceglie a parte
  expect_false(any(maschera_di(
    d,
    servizi_esclusi = c("CARTA", "VETRO", "SECCO PAP"),
    includi_stima_incerta = FALSE
  )))
  expect_setequal(
    rimasti(stato_db = "Non Presente", servizi_esclusi = "SECCO PAP"),
    c("INCERTO", "MUTO")
  )
  expect_setequal(
    rimasti(stato_db = "Presente", servizi_esclusi = "VETRO PAP"),
    "CENSITO"
  )

  # con la colonna `servizio_icona` già calcolata il risultato è lo stesso
  con_icona <- aggiungi_servizio_icona(d)
  expect_identical(servizio_mostrato(d), con_icona$servizio_icona)
  expect_identical(servizio_mostrato(con_icona), con_icona$servizio_icona)
  expect_identical(
    maschera_di(con_icona, servizi_esclusi = "SECCO PAP"),
    maschera_di(d, servizi_esclusi = "SECCO PAP")
  )

  # le statistiche contano con gli stessi criteri dei filtri
  stat <- calcola_statistiche(ultime_di(d))
  expect_equal(stat$presente, 1)
  expect_equal(stat$non_presente, 3)
  expect_identical(stat$servizio_censiti, c(CARTA = 1L))
  expect_identical(stat$servizio_stimato, c("SECCO PAP" = 1L))
  expect_equal(stat$stima_incerta, 2)

  # la stima del pannello conta le sole letture non censite
  misto <- dplyr::bind_rows(
    letture_rfid("X", date_2025(3), servizio = "VETRO"),
    letture_rfid(
      "X",
      c("2025-12-21", "2025-12-22"),
      presente = "Non Presente",
      atteso = "SECCO PAP"
    )
  )
  stima <- stima_servizio_atteso(misto)
  expect_identical(stima$servizio, "SECCO PAP")
  expect_equal(stima$pct, 100)
})

test_that("i filtri sul giro e sulla coerenza dei servizi valgono per ogni stato", {
  d <- dplyr::bind_rows(
    letture_rfid("SECCO1", date_2025(2), servizio = "SECCO"),
    letture_rfid("CARTA1", date_2025(2), servizio = "CARTA"),
    letture_rfid("NON1", date_2025(2), presente = "Non Presente"),
    letture_rfid("NON2", date_2025(2), presente = "Non Presente")
  )
  d$servizio_giro <- c(
    "SECCO PAP",
    "SECCO CONT.STRADALI",
    "SECCO PAP",
    "SECCO PAP",
    "SECCO PAP",
    NA,
    "SECCO PAP",
    "SECCO PAP"
  )
  d$servizio_atteso <- uniforma_servizio(d$servizio_giro)
  d$coerenza_servizio <- coerenza_servizio(d)
  # i contenitori della carta letti nei giri del secco non sono coerenti; per
  # i non censiti la coerenza non si può dire
  expect_identical(
    d$coerenza_servizio,
    c("SI", "SI", "NO", "NO", NA, NA, NA, NA)
  )
  tutti <- unique(d$RFID)
  rimasti <- function(...) unique(filtrate(d, ...)$RFID)

  # il giro ha il nome del calendario: il secco porta a porta è una voce sua
  expect_setequal(rimasti(giri_esclusi = "SECCO PAP"), c("SECCO1", "NON1"))
  expect_equal(nrow(filtrate(d, giri_esclusi = "SECCO PAP")), 2)
  expect_setequal(rimasti(giri_esclusi = "SECCO"), tutti)
  # una lettura senza giro non è mai tra quelle deselezionate
  expect_identical(
    maschera_di(d, giri_esclusi = c("SECCO PAP", NA)),
    maschera_di(d, giri_esclusi = "SECCO PAP")
  )
  # vale per censiti e non censiti: lo stato si sceglie a parte
  expect_setequal(
    rimasti(stato_db = "Presente", giri_esclusi = "SECCO CONT.STRADALI"),
    c("SECCO1", "CARTA1")
  )
  expect_equal(
    nrow(filtrate(d, stato_db = "Presente", giri_esclusi = "SECCO CONT.STRADALI")),
    3
  )
  expect_setequal(
    rimasti(stato_db = "Non Presente", giri_esclusi = "SECCO PAP"),
    "NON1"
  )

  # coerenza dei servizi: le letture di cui non si può dire stanno con quelle
  # coerenti
  expect_setequal(rimasti(coerenza_escluse = "coerente"), "CARTA1")
  expect_equal(nrow(filtrate(d, coerenza_escluse = "coerente")), 2)
  expect_setequal(
    rimasti(coerenza_escluse = "non_coerente"),
    c("SECCO1", "NON1", "NON2")
  )
  expect_equal(nrow(filtrate(d, coerenza_escluse = "non_coerente")), 6)
  expect_equal(
    nrow(filtrate(d, coerenza_escluse = c("coerente", "non_coerente"))),
    0
  )
  expect_setequal(rimasti(coerenza_escluse = "altro"), tutti)
  # i filtri si combinano
  expect_equal(
    nrow(filtrate(
      d,
      giri_esclusi = "SECCO PAP",
      coerenza_escluse = "coerente"
    )),
    0
  )

  # senza le due colonne i filtri non tolgono niente, e nessuna lettura
  # risulta non coerente
  semplici <- letture_test()
  expect_equal(
    nrow(filtrate(
      semplici,
      giri_esclusi = "SECCO PAP",
      coerenza_escluse = "non_coerente"
    )),
    6
  )
  expect_equal(nrow(filtrate(semplici, coerenza_escluse = "coerente")), 0)

  # le statistiche contano i giri e i bidoni non coerenti, per ultima lettura
  stat <- calcola_statistiche(ultime_di(d))
  expect_identical(
    stat$giri,
    c("SECCO PAP" = 2L, "SECCO CONT.STRADALI" = 1L)
  )
  expect_equal(stat$non_coerenti, 1)
  expect_identical(stat$servizio_censiti, c(SECCO = 1L, CARTA = 1L))
  expect_identical(stat$servizio_stimato, c(SECCO = 2L))
})
test_that("la coerenza confronta le tipologie dei servizi di una lettura", {
  expect_identical(stati_coerenza(), c(coerente = "SI", non_coerente = "NO"))
  expect_identical(names(etichette_coerenza()), names(stati_coerenza()))
  casi <- dplyr::tribble(
    ~RFID, ~servizio_transponder, ~servizio_atteso, ~attesa,
    "A", "SECCO", "SECCO", "SI",
    # il servizio atteso può avere ancora il nome del calendario
    "B", "SECCO", "SECCO PAP", "SI",
    # un contenitore della carta letto in mezzo a quelli del secco
    "C", "CARTA", "SECCO", "NO",
    "D", "UMIDO", "VETRO", "NO",
    # senza uno dei due servizi non si può dire: un non censito non ha il
    # servizio a database
    "E", "SECCO", NA, NA,
    "F", NA, "SECCO", NA,
    "G", NA, NA, NA,
    # un nome che la tabella non conosce fa tipologia a sé
    "H", "SPAZZAMENTO NEVE", "SPAZZAMENTO NEVE", "SI",
    "I", "SECCO", "SPAZZAMENTO NEVE", "NO",
    # maiuscole e spazi ai margini non contano
    "L", "carta ", " carta/cartone pap", "SI",
    # un giro che non raccoglie quel rifiuto
    "M", "SECCO", "PULIZIA TERRITORIO", "NO",
    "N", "CARTA", "SERVIZI", "NO",
    # un testo vuoto vale come mancante
    "O", "", "SECCO", NA
  )
  expect_identical(coerenza_servizio(casi), casi$attesa)

  # il confronto è tra servizio a database e servizio atteso: il giro non
  # entra
  con_giro <- casi
  con_giro$servizio_giro <- "VETRO PAP"
  expect_identical(coerenza_servizio(con_giro), casi$attesa)

  # una colonna che manca vale come vuota
  expect_true(all(is.na(coerenza_servizio(casi[c("RFID", "servizio_atteso")]))))
  expect_true(all(is.na(coerenza_servizio(casi["RFID"]))))
  expect_identical(coerenza_servizio(casi[0, ]), character(0))

  # nel dettaglio: le letture non coerenti di un RFID, dalla più recente
  storia <- letture_rfid("X", date_2025(7), servizio = "CARTA")
  storia$servizio_giro <- "SECCO PAP"
  storia$servizio_giro[1] <- "CARTA/CARTONE PAP"
  storia$servizio_atteso <- uniforma_servizio(storia$servizio_giro)
  storia$coerenza_servizio <- coerenza_servizio(storia)
  riepilogo <- coerenza_rfid(storia)
  expect_equal(riepilogo$n, 6)
  expect_equal(nrow(riepilogo$letture), 5)
  expect_identical(riepilogo$letture$giorno_lettura, rev(storia$giorno_lettura)[1:5])
  expect_identical(
    names(riepilogo$letture),
    c(
      "giorno_lettura",
      "matricola_veicolo",
      "servizio_transponder",
      "servizio_atteso",
      "servizio_giro"
    )
  )
  expect_equal(analizza_rfid(storia)$coerenza$n, 6)
  # senza la colonna, o con tutte le letture coerenti, non c'è niente
  expect_identical(coerenza_rfid(letture_test()), list(n = 0L))
  expect_identical(coerenza_rfid(storia[1, ]), list(n = 0L))
})
test_that("i filtri si applicano prima della deduplica", {
  # Escludendo CARTA, del bidone A resta visibile l'ultima lettura SECCO.
  ultimi <- ultime_di(letture_test(), servizi_esclusi = "CARTA")
  riga <- ultimi[ultimi$RFID == "A", ]
  expect_identical(riga$servizio_transponder, "SECCO")
  expect_equal(
    riga$giorno_lettura,
    as.POSIXct("2025-09-10 08:00:00", tz = "UTC")
  )
})

test_that("le statistiche contano una riga per RFID", {
  stat <- calcola_statistiche(ultime_di(letture_test()))
  expect_equal(stat$totale, 3)
  expect_equal(stat$presente, 2)
  expect_equal(stat$non_presente, 1)
  # i censiti contano sotto il servizio dell'ultima lettura
  expect_identical(stat$servizio_censiti, c(CARTA = 1L, VETRO = 1L))
  # il non censito conta sotto il servizio stimato dai giri
  expect_identical(stat$servizio_stimato, c(SECCO = 1L))
  expect_equal(stat$stima_incerta, 0)
  # senza le colonne del giro e della coerenza
  expect_length(stat$giri, 0)
  expect_equal(stat$non_coerenti, 0)
})

test_that("i servizi seguono l'ordine canonico", {
  expect_identical(
    ordina_servizi(c("VETRO", "ZETA", NA, "SECCO", "ALFA", "SECCO")),
    c("SECCO", "VETRO", "ALFA", "ZETA")
  )
  expect_identical(ordina_servizi(NULL), character(0))
  expect_identical(ordina_servizi(c(NA, NA)), character(0))
})

test_that("il testo di ricerca viene scomposto in codici", {
  expect_identical(
    analizza_input_ricerca(" R1 , R2\nR3\r\n\n R1 ,, "),
    c("R1", "R2", "R3")
  )
  expect_length(analizza_input_ricerca(""), 0)
  expect_length(analizza_input_ricerca(NULL), 0)
})

test_that("il confronto tra codici ignora maiuscole e valori mancanti", {
  expect_identical(
    codice_in(c("a1", "B2", NA, "c3", "A1"), c("A1", "b2")),
    c(TRUE, TRUE, FALSE, FALSE, TRUE)
  )
  expect_identical(codice_in(c("a1", "B2"), character(0)), c(FALSE, FALSE))
  expect_identical(codice_in(character(0), "A1"), logical(0))
})

test_that("la ricerca per RFID restituisce tutte le letture e marca l'ultima", {
  selezione <- selezione_di(letture_test())
  trovate <- cerca_per_rfid(selezione, c("a", "C", "inesistente"))
  expect_equal(nrow(trovate), 5)
  expect_equal(sum(trovate$is_ultimo), 2)
  ultima_a <- trovate[trovate$RFID == "A" & trovate$is_ultimo, ]
  expect_equal(
    ultima_a$giorno_lettura,
    as.POSIXct("2025-09-20 08:00:00", tz = "UTC")
  )
  expect_equal(nrow(cerca_per_rfid(selezione, "inesistente")), 0)
  # le letture hanno il servizio dell'icona, e sono in ordine di RFID e di
  # data decrescente
  expect_true("servizio_icona" %in% names(trovate))
  expect_identical(trovate$RFID, c("A", "A", "A", "C", "C"))
  expect_false(is.unsorted(rev(trovate$giorno_lettura[trovate$RFID == "A"])))
})

test_that("la ricerca per utenza considera solo l'utenza attuale", {
  d <- selezione_di(letture_test())
  # A era di U1 ma ora è di U2: cercando U1 compare solo B.
  expect_identical(filtra_ultimo_per_utenza(d, "U1")$RFID, "B")
  expect_identical(filtra_ultimo_per_utenza(d, "u2")$RFID, "A")
  expect_setequal(filtra_ultimo_per_utenza(d, c("U1", "U2"))$RFID, c("A", "B"))
  expect_equal(nrow(filtra_ultimo_per_utenza(d, "U9")), 0)

  esempio <- seleziona_periodo(archivio_esempio())
  expect_false(
    "RFD20250901050" %in% filtra_ultimo_per_utenza(esempio, "UTZ001")$RFID
  )
  expect_true(
    "RFD20250901050" %in% filtra_ultimo_per_utenza(esempio, "UTZ025")$RFID
  )
})

test_that("il bounding box copre tutte le letture", {
  d <- letture_test()
  expect_identical(
    calcola_bbox(d[d$RFID == "A", ]),
    list(lat_min = 41.90, lat_max = 41.92, lng_min = 12.40, lng_max = 12.42)
  )
})

test_that("la cronologia accorpa le letture consecutive uguali", {
  d <- letture_test()
  servizi <- cronologia_transizioni(d[d$RFID == "A", ], "servizio_transponder")
  expect_identical(servizi$valore, c("CARTA", "SECCO", "CARTA"))
  expect_identical(servizi$attuale, c(FALSE, FALSE, TRUE))

  utenze <- cronologia_transizioni(d[d$RFID == "A", ], "id_utenza")
  expect_identical(utenze$valore, c("U1", "U2"))
  expect_equal(
    utenze$giorno_lettura[2],
    as.POSIXct("2025-09-20 08:00:00", tz = "UTC")
  )

  expect_equal(nrow(cronologia_transizioni(d[d$RFID == "C", ], "id_utenza")), 0)
})

test_that("l'analisi di un RFID riconosce i quattro casi", {
  d <- letture_test()

  a <- analizza_rfid(d[d$RFID == "A", ])
  expect_identical(a$caso, "cambio_servizio")
  expect_identical(a$servizio_attuale, "CARTA")
  expect_true(a$cambio_utenza)
  expect_equal(a$n_letture, 3)

  b <- analizza_rfid(d[d$RFID == "B", ])
  expect_identical(b$caso, "censito_stabile")
  expect_false(b$cambio_utenza)

  c_stima <- analizza_rfid(d[d$RFID == "C", ])
  expect_identical(c_stima$caso, "non_censito_con_stima")
  expect_equal(c_stima$stima$pct, 100)

  senza <- d[d$RFID == "C" & is.na(d$servizio_atteso), ]
  expect_identical(analizza_rfid(senza)$caso, "non_censito_senza_stima")
})

test_that("i casi speciali del dataset di esempio sono riconosciuti", {
  d <- dati_esempio()
  caso <- function(rfid) analizza_rfid(d[d$RFID == rfid, ])

  cambio <- caso("RFD20250901001")
  expect_identical(cambio$caso, "cambio_servizio")
  expect_identical(
    cambio$cronologia_servizio$valore,
    c("CARTA", "SECCO", "CARTA")
  )

  utenza <- caso("RFD20250901050")
  expect_true(utenza$cambio_utenza)
  expect_identical(utenza$cronologia_utenza$valore, c("UTZ001", "UTZ025"))

  stima <- caso("RFD20250915201")
  expect_identical(stima$caso, "non_censito_con_stima")
  expect_identical(stima$stima$servizio, "SECCO")
  expect_equal(stima$stima$pct, 100)

  # due letture, due giri di tipologie diverse: la stima c'è ma è incerta
  incerto <- caso("RFD20250920250")
  expect_identical(incerto$caso, "non_censito_con_stima")
  expect_setequal(incerto$stima$servizio, c("SECCO", "CARTA"))
  expect_equal(incerto$stima$pct, c(50, 50))
})

test_that("la stima del servizio atteso somma a 100", {
  letture <- dplyr::tibble(
    presente_a_database = "Non Presente",
    servizio_atteso = c("SECCO", "SECCO", "CARTA", NA, "SECCO")
  )
  stima <- stima_servizio_atteso(letture)
  expect_identical(stima$servizio, c("SECCO", "CARTA"))
  expect_equal(stima$pct, c(75, 25))
})

test_that("le colonne facoltative sono convertite se presenti e create vuote se assenti", {
  con <- scrivi_csv(c(
    paste0(
      intestazione_csv,
      ",numero_raccolte_annue_previste,VOLUME_PREVISTO,Comune_da_database,Comune_Giro,Comune_Lettura,CANTIERE"
    ),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.72,11.76,26,1100,Rosà,Bassano del Grappa,rosà,",
    "2025-09-15 09:30:00,AB123CD,VEH001,R2,Non Presente,SECCO PAP,NA,45.43,11.79,NA,,,rubano,RUBANO,"
  ))
  esito <- carica_dataset(con)
  expect_equal(esito$dati$volume_previsto, c(1100, NA))
  expect_equal(esito$dati$numero_raccolte_annue_previste, c(26, NA))
  # i nomi dei comuni tornano alla grafia della tabella dei comuni
  expect_identical(esito$dati$comune_fornitura, c("ROSÀ", NA))
  expect_identical(esito$dati$comune_giro, c("BASSANO DEL GRAPPA", "RUBANO"))
  # la prima lettura è a Rosà, anche se il calendario mandava il mezzo a
  # Bassano del Grappa
  expect_identical(esito$dati$comune_lettura, c("ROSÀ", "RUBANO"))
  # il comune assegnato è quello di lettura; il cantiere è il suo
  expect_identical(esito$dati$comune_assegnato, c("ROSÀ", "RUBANO"))
  expect_identical(esito$dati$cantiere, c("BASSANO", "RUBANO"))
  expect_length(esito$avvisi, 0)

  # senza il servizio a database la coerenza non si può dire
  expect_identical(esito$dati$servizio_transponder, c(NA_character_, NA))
  expect_identical(esito$dati$servizio_atteso, c("CARTA", "SECCO PAP"))
  expect_identical(esito$dati$coerenza_servizio, c(NA_character_, NA))
  # senza la colonna del giro vale il servizio atteso scritto nel file, e
  # nessun avviso la segnala
  expect_identical(esito$dati$servizio_giro, c("CARTA", "SECCO PAP"))

  # il file del passo 3: servizio a database e servizio atteso dal giro,
  # volume e raccolte con i nomi dell'anagrafica, colonne che l'app non usa
  passo_3 <- carica_dataset(scrivi_csv(c(
    paste0(
      intestazione_transponder,
      ",servizio_giro,coerenza_servizio,volume,numero_raccolte_annue,id_giro,frequenza"
    ),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,SECCO,UTZ001,45.72,11.76,SECCO PAP,SI,240,26,12,QUINDICINALE",
    "2025-09-15 08:35:00,AB123CD,VEH001,R2,Presente,secco,SECCO,UTZ002,45.72,11.76,SECCO PAP,NO,1100,13,12,MENSILE",
    "2025-09-15 08:40:00,AB123CD,VEH001,R3,Non Presente,NA,SECCO,NA,45.72,11.76,SECCO PAP,,NA,NA,12,"
  )))
  # le colonne che l'app non usa restano fuori
  expect_identical(names(passo_3$dati), colonne_dataset())
  expect_identical(passo_3$dati$servizio_transponder, c("CARTA", "SECCO", NA))
  expect_identical(passo_3$dati$servizio_atteso, rep("SECCO", 3))
  expect_identical(passo_3$dati$servizio_giro, rep("SECCO PAP", 3))
  # la coerenza la ricalcola la validazione, anche se il file la contiene
  expect_identical(passo_3$dati$coerenza_servizio, c("NO", "SI", NA))
  # volume e raccolte hanno i nomi dell'app
  expect_equal(passo_3$dati$volume_previsto, c(240, 1100, NA))
  expect_equal(passo_3$dati$numero_raccolte_annue_previste, c(26, 13, NA))
  expect_identical(alias_colonne()[["volume"]], "volume_previsto")
  expect_identical(
    alias_colonne()[["numero_raccolte_annue"]],
    "numero_raccolte_annue_previste"
  )
  # un file con tutti e due i nomi tiene quello dell'app
  doppio <- carica_dataset(scrivi_csv(c(
    paste0(intestazione_csv, ",volume,volume_previsto"),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.72,11.76,240,770"
  )))
  expect_equal(doppio$dati$volume_previsto, 770)

  # un file preparato tra il 13 e il 15 ottobre 2026: il servizio del
  # database stava nel servizio atteso, con l'origine "Database"
  fusione <- carica_dataset(scrivi_csv(c(
    paste0(intestazione_csv, ",servizio_giro,origine_servizio"),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.72,11.76,SECCO PAP,Database",
    "2025-09-15 08:35:00,AB123CD,VEH001,R2,Non Presente,SECCO,NA,45.72,11.76,SECCO PAP,Calendario",
    "2025-09-15 08:40:00,AB123CD,VEH001,R3,Presente,CARTA,UTZ003,45.72,11.76,CARTA/CARTONE PAP,database"
  )))
  expect_identical(fusione$dati$servizio_transponder, c("CARTA", NA, "CARTA"))
  expect_identical(fusione$dati$servizio_atteso, c("SECCO", "SECCO", "CARTA"))
  expect_identical(fusione$dati$coerenza_servizio, c("NO", NA, "SI"))
  expect_false("origine_servizio" %in% names(fusione$dati))
  expect_identical(names(fusione$dati), colonne_dataset())

  expect_identical(colonne_contenitore(), "servizio_transponder")
  expect_true("servizio_transponder" %in% colonne_dataset())
  expect_true("coerenza_servizio" %in% colonne_derivate())
  expect_true("servizio_giro" %in% colonne_dataset())

  # il comune di lettura è quello del file, nella grafia della tabella; la
  # coerenza del comune lo confronta con il comune dichiarato, senza guardare
  # le coordinate
  gia_scritto <- carica_dataset(scrivi_csv(c(
    paste0(
      intestazione_csv,
      ",comune_fornitura,comune_giro,comune_lettura"
    ),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,41.9,12.5,Bassano del Grappa,Rubano,BASSANO DEL GRAPPA",
    "2025-09-15 09:30:00,AB123CD,VEH001,R2,Presente,CARTA,UTZ002,41.9,12.5,Limena,Rubano,Rubano",
    "2025-09-15 10:30:00,AB123CD,VEH001,R3,Non Presente,SECCO,NA,41.9,12.5,,Rubano,rosa'",
    # un comune che la tabella non contiene conta come fuori dai comuni serviti
    "2025-09-15 11:30:00,AB123CD,VEH001,R4,Non Presente,SECCO,NA,41.9,12.5,,Padova,Padova",
    "2025-09-15 12:30:00,AB123CD,VEH001,R5,Non Presente,SECCO,NA,41.9,12.5,,Rubano,"
  )))
  expect_identical(gia_scritto$dati$comune_giro, c(rep("RUBANO", 3), "PADOVA", "RUBANO"))
  expect_identical(
    gia_scritto$dati$comune_lettura,
    c("BASSANO DEL GRAPPA", "RUBANO", "ROSÀ", "PADOVA", NA)
  )
  expect_identical(
    gia_scritto$dati$comune_geografico,
    c("BASSANO DEL GRAPPA", "RUBANO", "ROSÀ", NA, NA)
  )
  expect_identical(
    gia_scritto$dati$comune_assegnato,
    c("BASSANO DEL GRAPPA", "RUBANO", "ROSÀ", NA, NA)
  )
  expect_identical(
    gia_scritto$dati$cantiere,
    c("BASSANO", "RUBANO", "BASSANO", NA, NA)
  )
  expect_identical(
    gia_scritto$dati$coerenza_comune,
    c("Coerente", "Fuori comune", "Fuori giro", "Coerente", "Coerenza ignota")
  )
  expect_true(any(grepl(
    "Comuni di lettura assenti dalla tabella dei comuni: PADOVA.",
    gia_scritto$avvisi,
    fixed = TRUE
  )))
  # una coerenza scritta nel file viene ricalcolata
  ricalcolata <- carica_dataset(scrivi_csv(c(
    paste0(
      intestazione_csv,
      ",comune_fornitura,comune_giro,comune_lettura,coerenza_comune"
    ),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,41.9,12.5,Limena,Rubano,Rubano,Coerente"
  )))
  expect_identical(ricalcolata$dati$coerenza_comune, "Fuori comune")

  # i file meno recenti chiamavano `comune` il comune del database
  vecchio <- carica_dataset(scrivi_csv(c(
    paste0(
      intestazione_csv,
      ",volume_previsto,numero_raccolte_annue_previste,comune"
    ),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.47,11.84,1100,26,Limena"
  )))
  expect_identical(vecchio$dati$comune_fornitura, "LIMENA")
  expect_identical(vecchio$dati$cantiere, "RUBANO")
  # senza il comune di lettura la coerenza del comune è ignota
  expect_identical(vecchio$dati$coerenza_comune, "Coerenza ignota")
  expect_match(
    vecchio$avvisi,
    "Colonne facoltative assenti: comune_giro, comune_lettura, cantiere.",
    fixed = TRUE
  )

  # fino al 17 ottobre 2026 il comune della fornitura si chiamava
  # `comune_da_database`; con tutti e due i nomi vale quello nuovo
  expect_identical(alias_colonne()[["comune_da_database"]], "comune_fornitura")
  da_database <- carica_dataset(scrivi_csv(c(
    paste0(intestazione_csv, ",comune_da_database,comune_lettura"),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.47,11.84,Limena,Cittadella"
  )))
  expect_identical(da_database$dati$comune_fornitura, "LIMENA")
  expect_identical(da_database$dati$coerenza_comune, "Fuori comune")
  tutti_e_due <- carica_dataset(scrivi_csv(c(
    paste0(intestazione_csv, ",comune_da_database,comune_fornitura"),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.47,11.84,Limena,Cittadella"
  )))
  expect_identical(tutti_e_due$dati$comune_fornitura, "CITTADELLA")

  # un comune fuori dalla tabella tiene il cantiere scritto nel file
  fuori <- carica_dataset(scrivi_csv(c(
    paste0(
      intestazione_csv,
      ",volume_previsto,numero_raccolte_annue_previste,comune_fornitura,comune_giro,cantiere"
    ),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.4,11.9,1100,26,Padova,,ovest",
    "2025-09-15 09:30:00,AB123CD,VEH001,R2,Presente,CARTA,UTZ002,45.4,11.9,1100,26,Vicenza,,"
  )))
  expect_identical(fuori$dati$comune_lettura, rep(NA_character_, 2))
  expect_identical(fuori$dati$comune_assegnato, c("PADOVA", "VICENZA"))
  expect_identical(fuori$dati$cantiere, c("OVEST", NA))
  expect_true(any(grepl(
    "Comuni assenti dalla tabella dei comuni: PADOVA, VICENZA.",
    fuori$avvisi,
    fixed = TRUE
  )))

  # anche un comune del giro che la tabella non contiene viene segnalato:
  # se il nome è sbagliato, i non censiti letti in quel giro risultano fuori
  # giro
  soppresso <- carica_dataset(scrivi_csv(c(
    paste0(
      intestazione_csv,
      ",comune_fornitura,comune_giro,comune_lettura,cantiere"
    ),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.72,11.76,Rosà,Valstagna,Rosà,BASSANO",
    "2025-09-15 09:30:00,AB123CD,VEH001,R2,Non Presente,CARTA,NA,45.72,11.76,,Valstagna,Valbrenta,BASSANO"
  )))
  expect_identical(soppresso$dati$comune_giro, rep("VALSTAGNA", 2))
  expect_identical(soppresso$dati$comune_lettura, c("ROSÀ", "VALBRENTA"))
  expect_identical(soppresso$dati$coerenza_comune, c("Coerente", "Fuori giro"))
  expect_true(any(grepl(
    "Comuni dei giri assenti dalla tabella dei comuni: VALSTAGNA. Se il nome \u00e8 sbagliato, i non censiti letti in quei giri risultano fuori giro.",
    soppresso$avvisi,
    fixed = TRUE
  )))
  # il comune assegnato è Rosà, che è in tabella: nessun altro avviso sui comuni
  expect_false(any(grepl("^Comuni assenti", soppresso$avvisi)))

  senza <- carica_dataset(scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,41.9,12.5"
  )))
  expect_identical(names(senza$dati), colonne_dataset())
  expect_true(all(is.na(senza$dati$volume_previsto)))
  expect_true(all(is.na(senza$dati$comune_assegnato)))
  expect_true(all(is.na(senza$dati$cantiere)))
  # senza il comune di lettura nel file la lettura non ha un comune
  expect_true(is.na(senza$dati$comune_lettura))
  expect_identical(senza$dati$coerenza_comune, "Coerenza ignota")
  expect_match(
    senza$avvisi,
    "Colonne facoltative assenti: volume_previsto, numero_raccolte_annue_previste, comune_fornitura, comune_giro, comune_lettura, cantiere.",
    fixed = TRUE
  )

  errato <- scrivi_csv(c(
    paste0(intestazione_csv, ",volume_previsto"),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,41.9,12.5,grande"
  ))
  errore <- expect_error(carica_dataset(errato), class = "errore_validazione")
  expect_match(
    conditionMessage(errore),
    "volume_previsto: 1 valori non numerici"
  )
})

test_that("anni e periodi di analisi", {
  expect_identical(anni_disponibili(archivio_esempio()), c(2025, 2024))
  expect_identical(anni_disponibili(NULL), numeric(0))
  expect_identical(periodo_anno(2025), as.Date(c("2025-01-01", "2025-12-31")))
  expect_identical(etichetta_periodo(periodo_anno(2024)), "Anno 2024")
  expect_identical(
    etichetta_periodo(as.Date(c("2025-03-15", "2025-09-30"))),
    "15/03/2025 - 30/09/2025"
  )
  expect_null(etichetta_periodo(NULL))

  nel_2024 <- filtra_periodo(dati_esempio(), periodo_anno(2024))
  expect_true(all(lubridate::year(nel_2024$giorno_lettura) == 2024))
  expect_gt(nrow(nel_2024), 0)
})

# Letture non censite con i servizi attesi indicati.
non_censito <- function(rfid, attesi) {
  dplyr::tibble(
    RFID = rfid,
    presente_a_database = "Non Presente",
    servizio_transponder = NA_character_,
    servizio_atteso = attesi
  )
}

# Servizio che dà l'icona a un RFID non censito: mancante sotto la soglia, e
# per un RFID senza stime.
prevalente_di <- function(rfid, dati, soglia = 0.8) {
  prevalenti <- servizio_prevalente_non_censiti(dati, soglia)
  prevalenti$servizio_prevalente[match(rfid, prevalenti$RFID)]
}

test_that("il servizio prevalente dei non censiti richiede almeno l'80%", {
  # esempi della specifica: 5 su 6 (83%) e 2 su 4 (50%)
  dati <- dplyr::bind_rows(
    non_censito("ALTO", c(rep("CARTA CONT.STRADALI", 5), "SECCO PAP")),
    non_censito(
      "BASSO",
      c("CARTA CONT.STRADALI", "CARTA CONT.STRADALI", "SECCO PAP", "UMIDO PAP")
    ),
    non_censito("SOGLIA", c(rep("VETRO PAP", 4), "SECCO PAP")),
    non_censito("VUOTO", c(NA, NA)),
    non_censito("CON_NA", c("SECCO PAP", "SECCO PAP", NA, NA, NA))
  )
  expect_identical(prevalente_di("ALTO", dati), "CARTA CONT.STRADALI")
  expect_identical(prevalente_di("BASSO", dati), NA_character_)
  # esattamente 80%: la soglia è inclusa
  expect_identical(prevalente_di("SOGLIA", dati), "VETRO PAP")
  # nessuna stima disponibile
  expect_identical(prevalente_di("VUOTO", dati), NA_character_)
  expect_identical(prevalente_di("INESISTENTE", dati), NA_character_)
  # le letture senza stima non entrano nel calcolo della quota
  expect_identical(prevalente_di("CON_NA", dati), "SECCO PAP")
  # soglia configurabile
  expect_identical(
    prevalente_di("BASSO", dati, soglia = 0.5),
    "CARTA CONT.STRADALI"
  )

  prevalenti <- servizio_prevalente_non_censiti(dati)
  expect_setequal(prevalenti$RFID, c("ALTO", "BASSO", "SOGLIA", "CON_NA"))
  expect_equal(prevalenti$quota[prevalenti$RFID == "ALTO"], 5 / 6)
})

test_that("la quota del servizio prevalente si calcola per tipologia", {
  # due giri della carta: nessuno arriva all'80%, la tipologia sì
  dati <- non_censito(
    "MISTO",
    c(rep("CARTA CONT.STRADALI", 3), rep("CARTA/CARTONE PAP", 2))
  )
  prevalente <- prevalente_di("MISTO", dati)
  expect_identical(prevalente, "CARTA CONT.STRADALI")
  expect_identical(info_servizio(prevalente)$icona, "file-lines")
})

test_that("l'icona di un censito è il servizio a database della lettura", {
  dati <- dplyr::bind_rows(
    non_censito("N1", rep("SECCO PAP", 3)),
    non_censito("N2", c("SECCO PAP", "VETRO PAP")),
    dplyr::tibble(
      RFID = "C1",
      presente_a_database = "Presente",
      servizio_transponder = "UMIDO",
      servizio_atteso = "SECCO"
    )
  )
  con_icona <- aggiungi_servizio_icona(dati)
  expect_identical(
    con_icona$servizio_icona,
    c(rep("SECCO PAP", 3), NA, NA, "UMIDO")
  )
  expect_identical(prevalente_di("C1", dati), NA_character_)

  # dataset di esempio: stima netta, stima incerta
  esempio <- aggiungi_servizio_icona(dati_esempio())
  expect_true(all(
    esempio$servizio_icona[esempio$RFID == "RFD20250915201"] == "SECCO"
  ))
  expect_true(all(is.na(esempio$servizio_icona[
    esempio$RFID == "RFD20250920250"
  ])))
  censiti <- esempio$presente_a_database == "Presente"
  bidoni <- censiti & !rfid_sacchetto(esempio$RFID)
  expect_identical(
    esempio$servizio_icona[bidoni],
    esempio$servizio_transponder[bidoni]
  )
  expect_true(all(
    esempio$servizio_icona[censiti & !bidoni] == servizio_sacchetti()
  ))
  non_censiti <- esempio[!censiti & !duplicated(esempio$RFID), ]
  expect_gt(sum(!is.na(non_censiti$servizio_icona)), 5)
  expect_gt(sum(is.na(non_censiti$servizio_icona)), 5)
})

test_that("gli spazi superflui non cambiano il dataset letto", {
  pulito <- carica_dataset(scrivi_csv(c(
    paste0(intestazione_csv, ",comune_fornitura"),
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,UTZ001,45.72,11.76,Limena",
    "2025-09-16 09:30:00,AB123CD,VEH001,R2,Non Presente,SECCO PAP,NA,45.43,11.79,"
  )))
  sporco <- carica_dataset(scrivi_csv(c(
    paste0(intestazione_csv, ",comune_fornitura"),
    "\"2025-09-15  08:30:00 \",AB123CD , VEH001,\" R1 \",Presente ,\"CARTA \",UTZ001,\" 45.72\",11.76 ,\"  Limena \"",
    "2025-09-16 09:30:00,AB123CD,VEH001,R2,\"Non  Presente\",\"SECCO  PAP\",NA,45.43,11.79,\" \""
  )))
  expect_identical(sporco$dati, pulito$dati)
  expect_identical(sporco$avvisi, pulito$avvisi)
})

test_that("i formati accettati sono CSV, CSV compresso, Parquet e DuckDB", {
  expect_identical(
    names(formati_file()),
    c("csv", "txt", "gz", "parquet", "duckdb")
  )
  accettate <- estensioni_accettate()
  expect_true(all(c(".csv", ".txt", ".gz", ".parquet", ".duckdb") %in% accettate))
})

test_that("le letture si leggono da un CSV compresso con gzip", {
  atteso <- dati_esempio()
  compresso <- withr::local_tempfile(fileext = ".csv.gz")
  uscita <- gzfile(compresso, "wb")
  writeBin(
    readBin(
      percorso_dataset_esempio(),
      "raw",
      file.size(percorso_dataset_esempio())
    ),
    uscita
  )
  close(uscita)
  expect_lt(file.size(compresso), file.size(percorso_dataset_esempio()) / 3)

  esito <- carica_dataset(compresso)
  expect_identical(esito$dati, atteso)
  expect_length(esito$avvisi, 0)

  # un file caricato dal browser ha un nome temporaneo: conta quello originale
  anonimo <- withr::local_tempfile(fileext = ".dat")
  file.copy(compresso, anonimo)
  expect_identical(carica_dataset(anonimo, "letture.CSV.GZ")$dati, atteso)

  # un CSV non compresso con estensione .gz si legge lo stesso
  expect_identical(
    carica_dataset(percorso_dataset_esempio(), "letture.csv.gz")$dati,
    atteso
  )
  # la decompressione non lascia file temporanei
  prima <- list.files(tempdir())
  invisible(carica_dataset(compresso))
  expect_setequal(list.files(tempdir()), prima)

  # un file compresso vuoto è rifiutato come un CSV vuoto
  vuoto <- withr::local_tempfile(fileext = ".csv.gz")
  close(gzfile(vuoto, "wb"))
  expect_error(
    carica_dataset(vuoto),
    "vuoto",
    class = "errore_validazione"
  )
})

test_that("le letture si leggono da un file Parquet con le colonne già tipizzate", {
  skip_if_not_installed("nanoparquet")
  atteso <- dati_esempio()
  file <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(
    as.data.frame(atteso[, setdiff(names(atteso), colonne_derivate())]),
    file
  )
  esito <- carica_dataset(file)
  expect_equal(esito$dati, atteso)
  expect_length(esito$avvisi, 0)
  # data e ora restano quelle scritte, senza spostamenti di fuso
  expect_s3_class(esito$dati$giorno_lettura, "POSIXct")
  expect_identical(
    format(esito$dati$giorno_lettura[1:20], "%Y-%m-%d %H:%M:%S"),
    format(atteso$giorno_lettura[1:20], "%Y-%m-%d %H:%M:%S")
  )

  # un file che non è Parquet
  expect_error(
    carica_dataset(percorso_dataset_esempio(), "letture.parquet"),
    "Impossibile leggere il file come Parquet",
    class = "errore_validazione"
  )
})

test_that("un file indicato all'avvio viene letto una volta sola", {
  file <- withr::local_tempfile(fileext = ".csv")
  file.copy(percorso_dataset_esempio(), file)
  letture <- 0
  lettore <- function(percorso) {
    letture <<- letture + 1
    carica_dataset(percorso)
  }
  primo <- leggi_una_volta(lettore, file)
  secondo <- leggi_una_volta(lettore, file)
  expect_equal(letture, 1)
  expect_identical(secondo, primo)
  expect_equal(nrow(primo$dati), nrow(dati_esempio()))

  # se il file cambia viene riletto, e la versione precedente non resta
  writeLines(utils::head(readLines(file), 101), file)
  terzo <- leggi_una_volta(lettore, file)
  expect_equal(letture, 2)
  expect_equal(nrow(terzo$dati), 100)
  expect_identical(leggi_una_volta(lettore, file), terzo)
  expect_equal(letture, 2)
  expect_identical(memoria_file[[normalizePath(file)]]$esito, terzo)

  expect_error(
    leggi_una_volta(lettore, file.path(tempdir(), "non_esiste.csv")),
    "File non trovato",
    class = "errore_validazione"
  )
})

test_that("l'ultima lettura si sceglie tra quelle che superano i filtri", {
  dati <- dati_esempio()
  filtri <- list(stato_db = "Presente", servizi_esclusi = "SECCO")
  maschera <- do.call(maschera_di, c(list(dati), filtri))
  expect_identical(
    do.call(ultime_esempio, filtri)$RFID,
    ultime_di(dati[maschera, ])$RFID
  )
  # stesso risultato della regola: l'ultima lettura di ogni RFID
  attese <- dati |>
    dplyr::slice_max(.data$giorno_lettura, n = 1, by = "RFID", with_ties = FALSE)
  ultimi <- ultime_esempio()
  expect_identical(
    ultimi$giorno_lettura[match(attese$RFID, ultimi$RFID)],
    attese$giorno_lettura
  )
  # dalla più recente alla meno recente
  expect_false(is.unsorted(rev(ultimi$giorno_lettura)))
  expect_equal(nrow(ultime_esempio(stato_db = character(0))), 0)
})

test_that("un periodo che contiene tutte le letture restituisce il dataset com'è", {
  dati <- dati_esempio()
  expect_identical(
    filtra_periodo(dati, as.Date(c("2024-01-01", "2026-12-31"))),
    dati
  )
  # gli estremi contano a giornata intera
  giorni <- as.Date(range(dati$giorno_lettura))
  expect_identical(filtra_periodo(dati, giorni), dati)
  expect_lt(nrow(filtra_periodo(dati, giorni + c(1, 0))), nrow(dati))
  expect_lt(nrow(filtra_periodo(dati, giorni - c(0, 1))), nrow(dati))
})

test_that("i cantieri seguono l'ordine dei filtri, con i non assegnati in fondo", {
  dati <- dati_esempio()
  expect_identical(senza_cantiere(), "Non assegnato")
  expect_identical(ordina_cantieri(dati$cantiere), cantieri())
  expect_identical(
    ordina_cantieri(c(dati$cantiere, NA)),
    c(cantieri(), senza_cantiere())
  )
  # prima i cantieri della tabella dei comuni, poi gli altri
  expect_identical(
    ordina_cantieri(c("RUBANO", "ZETA", "ALTROVE", "ASIAGO")),
    c("ASIAGO", "RUBANO", "ALTROVE", "ZETA")
  )
  expect_identical(ordina_cantieri(c(NA, NA)), senza_cantiere())
  expect_identical(ordina_cantieri(NULL), character(0))

  expect_identical(
    conta_cantieri(c("RUBANO", NA, "RUBANO", "ASIAGO")),
    c(ASIAGO = 1L, RUBANO = 2L, "Non assegnato" = 1L)
  )
  vuoto <- stats::setNames(integer(0), character(0))
  expect_identical(conta_cantieri(c(NA, NA)), vuoto)
  expect_identical(conta_cantieri(NULL), vuoto)
})

test_that("i filtri per cantiere e comune si combinano con gli altri", {
  dati <- dati_esempio()
  tutte <- rep(TRUE, nrow(dati))
  expect_identical(maschera_di(dati), tutte)

  # i cantieri si filtrano per esclusione, come i servizi
  senza_asiago <- maschera_di(dati, cantieri_esclusi = "ASIAGO")
  expect_identical(
    senza_asiago,
    is.na(dati$cantiere) | dati$cantiere != "ASIAGO"
  )
  # le letture senza cantiere hanno una voce loro: nel dataset di esempio
  # non ce ne sono, in un file possono esserci
  expect_identical(
    maschera_di(dati, cantieri_esclusi = senza_cantiere()),
    tutte
  )
  con_vuoti <- dati
  con_vuoti$cantiere[con_vuoti$RFID == "RFD20241001301"] <- NA
  solo_assegnate <- maschera_di(
    con_vuoti,
    cantieri_esclusi = senza_cantiere()
  )
  expect_identical(solo_assegnate, !is.na(con_vuoti$cantiere))
  expect_equal(sum(!solo_assegnate), sum(dati$RFID == "RFD20241001301"))

  # il comune è quello assegnato alla lettura
  limena <- maschera_di(dati, comuni = "LIMENA")
  expect_identical(
    limena,
    !is.na(dati$comune_assegnato) & dati$comune_assegnato == "LIMENA"
  )
  expect_gt(sum(limena), 0)
  # più comuni, anche di cantieri diversi
  asiago <- maschera_di(dati, comuni = "ASIAGO")
  due <- maschera_di(dati, comuni = c("LIMENA", "ASIAGO"))
  expect_identical(due, limena | asiago)
  expect_setequal(unique(dati$cantiere[due]), c("RUBANO", "ASIAGO"))
  expect_equal(sum(due), sum(limena) + sum(asiago))
  # nessuna scelta: tutti i comuni
  expect_identical(maschera_di(dati, comuni = character(0)), tutte)
  expect_identical(maschera_di(dati, comuni = ""), tutte)
  expect_identical(maschera_di(dati, comuni = NULL), tutte)
  expect_identical(maschera_di(dati, comuni = NA_character_), tutte)
  # un comune che non c'è non lascia letture
  expect_false(any(maschera_di(dati, comuni = "PAESE IGNOTO")))

  # insieme agli altri filtri
  combinata <- maschera_di(
    dati,
    stato_db = "Presente",
    cantieri_esclusi = c("ASIAGO", "BASSANO"),
    comuni = "LIMENA"
  )
  expect_identical(combinata, limena & dati$presente_a_database == "Presente")
  # i comuni di un cantiere escluso non lasciano letture, gli altri sì
  expect_false(any(maschera_di(
    dati,
    cantieri_esclusi = "RUBANO",
    comuni = "LIMENA"
  )))
  expect_identical(
    maschera_di(
      dati,
      cantieri_esclusi = "RUBANO",
      comuni = c("LIMENA", "ASIAGO")
    ),
    asiago
  )

  rimaste <- filtrate(dati, cantieri_esclusi = "ASIAGO")
  expect_equal(nrow(rimaste), sum(senza_asiago))
  expect_false("ASIAGO" %in% rimaste$cantiere)

  # in un dataset senza geografia le letture non hanno cantiere: escludere un
  # cantiere non toglie niente, e nessuna lettura sta in un comune scelto
  expect_identical(
    maschera_di(letture_test(), cantieri_esclusi = "ASIAGO"),
    rep(TRUE, 6)
  )
  expect_identical(
    maschera_di(letture_test(), comuni = "LIMENA"),
    rep(FALSE, 6)
  )
})

test_that("le statistiche contano RFID e letture di ogni cantiere", {
  dati <- dati_esempio()
  selezione <- seleziona_periodo(archivio_esempio())
  ultimi <- ultime_letture(selezione)
  stat <- calcola_statistiche(ultimi, conteggi_cantieri(selezione))
  expect_identical(
    stat$rfid_cantieri,
    c(ASIAGO = 33L, BASSANO = 66L, CAMPOSAMPIERO = 93L, RUBANO = 59L)
  )
  expect_identical(
    stat$letture_cantieri,
    c(ASIAGO = 691L, BASSANO = 1879L, CAMPOSAMPIERO = 2585L, RUBANO = 1320L)
  )
  expect_equal(sum(stat$rfid_cantieri), stat$totale)
  expect_equal(sum(stat$letture_cantieri), nrow(dati))

  # le letture senza cantiere contano sotto una voce loro, in fondo
  con_vuoti <- dati
  con_vuoti$cantiere[con_vuoti$RFID == "RFD20241001301"] <- NA
  selezione_vuoti <- selezione_di(con_vuoti)
  vuoti <- calcola_statistiche(
    ultime_letture(selezione_vuoti),
    conteggi_cantieri(selezione_vuoti)
  )
  expect_identical(names(vuoti$rfid_cantieri), c(cantieri(), senza_cantiere()))
  expect_equal(vuoti$rfid_cantieri[[senza_cantiere()]], 1)
  expect_equal(
    vuoti$letture_cantieri[[senza_cantiere()]],
    sum(dati$RFID == "RFD20241001301")
  )

  # senza le letture restano i soli RFID; senza cantieri niente
  expect_length(calcola_statistiche(ultimi)$letture_cantieri, 0)
  senza <- calcola_statistiche(ultime_di(letture_test()))
  expect_length(senza$rfid_cantieri, 0)
  expect_length(senza$letture_cantieri, 0)
  expect_length(conteggi_cantieri(selezione_di(letture_test())), 0)
  # i conteggi seguono i filtri
  expect_identical(
    conteggi_cantieri(selezione, cantieri_esclusi = "ASIAGO"),
    stat$letture_cantieri[-1]
  )
})

# Codice RFID di un sacchetto: 24 caratteri, con il prefisso 00BD.
test_that("i sacchetti si riconoscono dal codice RFID", {
  expect_identical(servizio_sacchetti(), "SACCHETTI")
  expect_identical(
    rfid_sacchetto(c(
      codice_sacchetto(1),
      tolower(codice_sacchetto(2)),
      # il codice di un bidone ha 10 caratteri, anche se inizia allo stesso modo
      "00BD123456",
      # 24 caratteri, ma senza il prefisso
      sprintf("%024d", 7),
      "RFD20250901001",
      NA,
      ""
    )),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_identical(rfid_sacchetto(character(0)), logical(0))
  # nel dataset di esempio i sacchetti sono 12, gli altri RFID sono contenitori
  esempio <- unique(dati_esempio()$RFID)
  expect_equal(sum(rfid_sacchetto(esempio)), 12)
  expect_identical(
    rfid_sacchetto(esempio),
    startsWith(esempio, "00BD")
  )
})

test_that("le letture senza i sacchetti sono quelle dei contenitori", {
  d <- dplyr::bind_rows(
    letture_rfid("0000ABC123", date_2025(2)),
    letture_rfid(codice_sacchetto(1), "2025-03-01"),
    letture_rfid("0000ABC124", date_2025(1), presente = "Non Presente"),
    letture_rfid(codice_sacchetto(2), "2025-04-01", presente = "Non Presente")
  )
  soli <- senza_sacchetti(d)
  expect_identical(soli$RFID, c("0000ABC123", "0000ABC123", "0000ABC124"))
  expect_identical(names(soli), names(d))
  expect_s3_class(soli, class(d)[1])
  # senza sacchetti le letture tornano così come sono
  expect_identical(senza_sacchetti(soli), soli)
  expect_equal(nrow(senza_sacchetti(d[0, ])), 0)
  # basta la colonna RFID
  solo_codici <- d[, c("RFID", "giorno_lettura")]
  expect_identical(senza_sacchetti(solo_codici)$RFID, soli$RFID)
  # nel dataset di esempio toglie i 12 sacchetti e le loro 12 letture
  esempio <- dati_esempio()
  expect_equal(nrow(esempio) - nrow(senza_sacchetti(esempio)), 12)
  expect_equal(dplyr::n_distinct(senza_sacchetti(esempio)$RFID), 239)
})

test_that("un sacchetto tiene il servizio del giro, e l'icona gli dà il suo", {
  riga <- function(giorno, rfid, stato, servizio, utenza) {
    paste(
      paste(giorno, "08:00:00"),
      "AB123CD",
      "VEH001",
      rfid,
      stato,
      servizio,
      "SECCO PAP",
      utenza,
      "45.5",
      "11.8",
      sep = ","
    )
  }
  # Il servizio del database, che un sacchetto non ha, sta nella colonna prima
  # del servizio atteso dal giro.
  percorso <- scrivi_csv(c(
    intestazione_transponder,
    riga("2025-09-01", codice_sacchetto(1), "Presente", "", "S9"),
    riga("2025-09-02", codice_sacchetto(2), "Non Presente", "", ""),
    # un bidone censito senza servizio a database
    riga("2025-09-03", "0000ABC123", "Presente", "", "U1"),
    riga("2025-09-04", "0000ABC124", "Presente", "carta", "U2")
  ))
  d <- carica_dataset(percorso)$dati
  # il servizio atteso è quello del giro, per tutti
  expect_identical(d$servizio_atteso, rep("SECCO PAP", 4))
  # a database ha un servizio solo l'ultimo
  expect_identical(d$servizio_transponder, c(NA, NA, NA, "CARTA"))
  # la voce dei sacchetti viene dal codice, censiti o no; un censito senza
  # servizio a database ha il punto di domanda
  expect_identical(
    servizio_mostrato(d),
    c(servizio_sacchetti(), servizio_sacchetti(), NA, "CARTA")
  )
  expect_identical(
    d$presente_a_database,
    c("Presente", "Non Presente", "Presente", "Presente")
  )
})

test_that("i sacchetti stanno sotto la loro tipologia, censiti o no", {
  d <- dplyr::bind_rows(
    # un sacchetto censito non ha un servizio a database: tiene quello del giro
    letture_rfid(codice_sacchetto(1), date_2025(3), servizio = "SECCO PAP"),
    # letto da giri diversi: per un bidone la stima sarebbe incerta
    letture_rfid(
      codice_sacchetto(2),
      date_2025(4),
      presente = "Non Presente",
      atteso = c("SECCO PAP", "SECCO PAP", "VETRO PAP", "UMIDO PAP")
    ),
    letture_rfid("0000ABC123", date_2025(3), servizio = "CARTA"),
    letture_rfid(
      "0000ABC124",
      date_2025(3),
      presente = "Non Presente",
      atteso = "SECCO PAP"
    )
  )
  con_icona <- aggiungi_servizio_icona(d)
  icona <- function(rfid) {
    unique(con_icona$servizio_icona[con_icona$RFID == rfid])
  }
  expect_identical(icona(codice_sacchetto(1)), servizio_sacchetti())
  # un sacchetto non censito non ha un servizio da stimare
  expect_identical(icona(codice_sacchetto(2)), servizio_sacchetti())
  expect_identical(icona("0000ABC123"), "CARTA")
  expect_identical(icona("0000ABC124"), "SECCO PAP")

  # un sacchetto censito rimasto senza servizio
  grezzo <- d
  grezzo$servizio_atteso[grezzo$RFID == codice_sacchetto(1)] <- NA
  expect_identical(
    aggiungi_servizio_icona(grezzo)$servizio_icona,
    con_icona$servizio_icona
  )

  # i sacchetti stanno sotto la loro voce, censiti o no: lo stato a database
  # si sceglie a parte
  rimasti <- function(...) unique(filtrate(con_icona, ...)$RFID)
  tutti <- unique(d$RFID)
  expect_setequal(
    rimasti(servizi_esclusi = servizio_sacchetti()),
    c("0000ABC123", "0000ABC124")
  )
  expect_setequal(
    rimasti(stato_db = "Presente", servizi_esclusi = servizio_sacchetti()),
    "0000ABC123"
  )
  # il sacchetto censito è letto dai giri del secco, ma non è un contenitore
  # del secco: togliere il secco non lo toglie
  expect_setequal(
    rimasti(servizi_esclusi = "SECCO PAP"),
    setdiff(tutti, "0000ABC124")
  )
  # la casella della stima incerta non li riguarda
  expect_setequal(rimasti(includi_stima_incerta = FALSE), tutti)

  stat <- calcola_statistiche(ultime_di(d))
  expect_identical(stat$servizio_censiti, c(CARTA = 1L, SACCHETTI = 1L))
  expect_identical(stat$servizio_stimato, c("SECCO PAP" = 1L, SACCHETTI = 1L))
  expect_equal(stat$stima_incerta, 0)

  # nel dettaglio il sacchetto è riconosciuto, censito o no
  expect_true(analizza_rfid(d[d$RFID == codice_sacchetto(1), ])$sacchetto)
  expect_true(analizza_rfid(d[d$RFID == codice_sacchetto(2), ])$sacchetto)
  expect_false(analizza_rfid(d[d$RFID == "0000ABC123", ])$sacchetto)
})

test_that("la coerenza del comune confronta il comune di lettura con quello dichiarato", {
  expect_identical(
    names(stati_coerenza_comune()),
    c("coerente", "fuori_comune", "fuori_giro", "ignota")
  )
  d <- data.frame(
    presente_a_database = c(
      "Presente",
      "Presente",
      "Non Presente",
      "Non Presente",
      "Presente",
      "Non Presente",
      "Presente",
      "Non Presente"
    ),
    # per un censito conta il comune del database, mai quello del giro
    comune_fornitura = c("Limena", "LIMENA", NA, NA, NA, NA, "ROSÀ", "ASIAGO"),
    comune_giro = c(
      "RUBANO",
      "LIMENA",
      "Cittadella",
      "CITTADELLA",
      "ASIAGO",
      NA,
      "ROSÀ",
      "GALLIO"
    ),
    comune_lettura = c(
      "LIMENA",
      "CITTADELLA",
      "CITTADELLA",
      "ASIAGO",
      "ASIAGO",
      "ASIAGO",
      "Rosa'",
      "GALLIO"
    ),
    stringsAsFactors = FALSE
  )
  expect_identical(
    coerenza_comune(d),
    c(
      "Coerente",
      "Fuori comune",
      "Coerente",
      "Fuori giro",
      # un censito senza comune a database, come un sacchetto
      "Coerenza ignota",
      # un non censito senza comune del giro
      "Coerenza ignota",
      # i nomi si confrontano senza badare ad accenti e apostrofi
      "Coerente",
      # per un non censito conta il comune del giro
      "Coerente"
    )
  )
  # senza il comune di lettura la coerenza è ignota; una stringa vuota vale
  # come un comune mancante
  senza_lettura <- d
  senza_lettura$comune_lettura <- c("", rep(NA, 7))
  expect_true(all(coerenza_comune(senza_lettura) == "Coerenza ignota"))
  expect_true(all(
    coerenza_comune(d[, c("presente_a_database", "comune_giro")]) ==
      "Coerenza ignota"
  ))
  expect_identical(coerenza_comune(d[0, ]), character(0))

  incongruenze <- incongruenze_comune(dplyr::tibble(
    coerenza_comune = coerenza_comune(d)
  ))
  expect_identical(incongruenze$fuori_comune, 1:8 == 2)
  expect_identical(incongruenze$fuori_giro, 1:8 == 4)
  # senza la colonna nessuna lettura è fuori comune o fuori giro
  expect_false(any(unlist(incongruenze_comune(d))))
})

# Letture con la coerenza del comune scritta a mano: A coerente, B fuori
# comune, C fuori giro, D di coerenza ignota.
letture_coerenza_comune <- function() {
  d <- dplyr::bind_rows(
    letture_rfid("A", "2025-03-01"),
    letture_rfid("B", "2025-03-02"),
    letture_rfid("C", "2025-03-03", presente = "Non Presente", atteso = "SECCO PAP"),
    letture_rfid("D", "2025-03-04")
  )
  d$coerenza_comune <- unname(stati_coerenza_comune())
  d
}

test_that("il filtro sulla coerenza del comune tiene le voci spuntate", {
  d <- letture_coerenza_comune()
  rimasti <- function(...) d$RFID[maschera_di(d, ...)]
  expect_identical(rimasti(), c("A", "B", "C", "D"))
  expect_identical(
    rimasti(coerenza_comune_escluse = "coerente"),
    c("B", "C", "D")
  )
  expect_identical(
    rimasti(coerenza_comune_escluse = "fuori_comune"),
    c("A", "C", "D")
  )
  expect_identical(
    rimasti(coerenza_comune_escluse = "fuori_giro"),
    c("A", "B", "D")
  )
  expect_identical(rimasti(coerenza_comune_escluse = "ignota"), c("A", "B", "C"))
  expect_identical(
    rimasti(coerenza_comune_escluse = c("coerente", "ignota")),
    c("B", "C")
  )
  expect_identical(
    rimasti(coerenza_comune_escluse = names(stati_coerenza_comune())),
    character(0)
  )
  # una voce sconosciuta non filtra niente; gli altri filtri si sommano
  expect_identical(
    rimasti(coerenza_comune_escluse = "altro"),
    c("A", "B", "C", "D")
  )
  expect_identical(
    rimasti(coerenza_comune_escluse = "coerente", stato_db = "Presente"),
    c("B", "D")
  )
  # un dataset senza la colonna non viene filtrato
  semplice <- letture_test()
  expect_true(all(maschera_di(
    semplice,
    coerenza_comune_escluse = names(stati_coerenza_comune())
  )))
})

test_that("le statistiche contano i bidoni fuori comune e fuori giro", {
  stat <- calcola_statistiche(letture_coerenza_comune())
  expect_identical(stat$fuori_comune, 1L)
  expect_identical(stat$fuori_giro, 1L)
  expect_identical(stat$coerenza_comune_ignota, 1L)
  senza <- calcola_statistiche(ultime_di(letture_test()))
  expect_identical(senza$fuori_comune, 0L)
  expect_identical(senza$fuori_giro, 0L)
  expect_identical(senza$coerenza_comune_ignota, 0L)
  # sul dataset di esempio del 2025, per ultima lettura di ogni RFID: i
  # sacchetti censiti non hanno un comune a database
  esempio <- calcola_statistiche(ultime_esempio(periodo = periodo_anno(2025)))
  expect_identical(esempio$fuori_comune, 3L)
  expect_identical(esempio$fuori_giro, 20L)
  expect_identical(esempio$coerenza_comune_ignota, 7L)
})

test_that("il dettaglio di un RFID riporta le letture fuori dai comuni dichiarati", {
  d <- dati_esempio()
  # spostato: otto letture a Cittadella, registrato a Limena
  spostato <- analizza_rfid(d[d$RFID == "RFD20250905100", ])$coerenza_comune
  expect_identical(spostato$fuori_comune$n, 8L)
  expect_identical(spostato$fuori_comune$dichiarati, "LIMENA")
  expect_identical(spostato$fuori_comune$letti, "CITTADELLA")
  # un censito non è mai fuori giro
  expect_identical(spostato$fuori_giro$n, 0L)
  expect_null(spostato$fuori_giro$letture)

  # il tag fermo in deposito, non censito, è letto da giri di ogni comune
  deposito <- analizza_rfid(d[d$RFID == "RFD20241001301", ])$coerenza_comune
  expect_identical(deposito$fuori_comune$n, 0L)
  expect_gt(deposito$fuori_giro$n, 300)
  expect_identical(deposito$fuori_giro$letti, "CAMPOSAMPIERO")
  expect_gt(length(deposito$fuori_giro$dichiarati), 10)
  # le ultime cinque letture fuori giro, dalla più recente
  ultime <- deposito$fuori_giro$letture
  expect_equal(nrow(ultime), 5)
  expect_false(is.unsorted(rev(ultime$giorno_lettura)))
  expect_named(
    ultime,
    c("giorno_lettura", "matricola_veicolo", "comune_giro", "comune_lettura")
  )

  # un contenitore coerente non ha niente da segnalare
  coerente <- analizza_rfid(d[d$RFID == "RFD20250901001", ])$coerenza_comune
  expect_identical(coerente$fuori_comune$n, 0L)
  expect_identical(coerente$fuori_giro$n, 0L)
  # letture senza la coerenza del comune
  semplice <- analizza_rfid(letture_rfid("A", date_2025(3)))$coerenza_comune
  expect_identical(semplice$fuori_comune$n, 0L)
  expect_identical(semplice$fuori_giro$n, 0L)
})
