# Il dataset di esempio deve rispettare le specifiche del progetto.

test_that("dimensioni, periodo e mezzi", {
  d <- dati_esempio()
  expect_gte(nrow(d), 600)
  expect_equal(nrow(d), 6475)
  expect_equal(dplyr::n_distinct(d$RFID), 251)
  # i contenitori: gli altri RFID sono sacchetti
  bidoni <- unique(d$RFID[!rfid_sacchetto(d$RFID)])
  expect_length(bidoni, 239)
  # 2025 completo, 2024 parziale: serve a provare la selezione dell'anno
  expect_equal(
    as.Date(range(d$giorno_lettura)),
    as.Date(c("2024-10-01", "2025-12-31"))
  )
  expect_identical(anni_disponibili(archivio_esempio()), c(2025, 2024))

  ore <- as.numeric(format(d$giorno_lettura, "%H", tz = "UTC")) +
    as.numeric(format(d$giorno_lettura, "%M", tz = "UTC")) / 60
  expect_true(all(ore >= 6 & ore <= 19))

  mezzi <- dplyr::distinct(d, targa_veicolo, matricola_veicolo)
  expect_equal(nrow(mezzi), 5)
  expect_equal(anyDuplicated(mezzi$targa_veicolo), 0)
  expect_equal(anyDuplicated(mezzi$matricola_veicolo), 0)
})

test_that("coerenza tra stato a database, servizio, utenza, volume e raccolte", {
  d <- dati_esempio()
  non_presente <- d$presente_a_database == "Non Presente"
  # l'utenza viene dalla fornitura del contenitore: la hanno i contenitori
  # censiti, non i non censiti e non i sacchetti
  expect_true(all(is.na(d$id_utenza[non_presente])))
  expect_identical(
    is.na(d$id_utenza),
    non_presente | rfid_sacchetto(d$RFID)
  )
  # il servizio atteso dal giro c'è su ogni lettura; il servizio a database
  # solo sui contenitori censiti, non sui sacchetti
  expect_false(anyNA(d$servizio_atteso))
  con_contenitore <- !non_presente & !rfid_sacchetto(d$RFID)
  expect_identical(is.na(d$servizio_transponder), !con_contenitore)
  expect_equal(sum(con_contenitore), 5399)
  # volume e raccolte sono dati del contenitore: un sacchetto non li ha
  # nemmeno da censito
  senza_contenitore <- non_presente | rfid_sacchetto(d$RFID)
  for (campo in c("volume_previsto", "numero_raccolte_annue_previste")) {
    expect_true(all(is.na(d[[campo]][senza_contenitore])), info = campo)
    expect_false(anyNA(d[[campo]][!senza_contenitore]), info = campo)
  }
  expect_false(anyNA(d$latitudine))
  expect_false(anyNA(d$longitudine))
})

test_that("volume e raccolte previste sono realistici e univoci per RFID", {
  d <- dati_esempio()
  censiti <- d[
    d$presente_a_database == "Presente" & !rfid_sacchetto(d$RFID),
  ]
  expect_setequal(unique(censiti$volume_previsto), c(240, 770, 1100))
  expect_setequal(unique(censiti$numero_raccolte_annue_previste), c(13, 26))

  per_rfid <- dplyr::summarise(
    censiti,
    volumi = dplyr::n_distinct(volume_previsto),
    raccolte = dplyr::n_distinct(numero_raccolte_annue_previste),
    .by = "RFID"
  )
  expect_true(all(per_rfid$volumi == 1))
  expect_true(all(per_rfid$raccolte == 1))

  # i contenitori che hanno cambiato servizio conservano volume e raccolte iniziali
  stabili <- dplyr::filter(
    censiti,
    dplyr::n_distinct(servizio_atteso) == 1,
    .by = "RFID"
  )
  ultimi <- ultime_di(stabili)
  expect_true(all(
    ultimi$volume_previsto[
      ultimi$servizio_atteso == "PLASTICA E METALLI"
    ] ==
      770
  ))
  expect_true(all(
    ultimi$numero_raccolte_annue_previste[
      ultimi$servizio_atteso != "VETRO"
    ] ==
      26
  ))
  expect_setequal(
    unique(ultimi$numero_raccolte_annue_previste[
      ultimi$servizio_atteso == "VETRO"
    ]),
    c(13, 26)
  )
  quota_secco_1100 <- mean(
    ultimi$volume_previsto[ultimi$servizio_atteso == "SECCO"] == 1100
  )
  expect_equal(quota_secco_1100, 0.8, tolerance = 0.15)
})

test_that("distribuzioni vicine a quelle richieste", {
  # le quote riguardano i contenitori: i sacchetti hanno un test a parte
  d <- dati_esempio()
  d <- d[!rfid_sacchetto(d$RFID), ]
  ultimi <- ultime_di(d)

  expect_equal(
    mean(ultimi$presente_a_database == "Presente"),
    0.85,
    tolerance = 0.02
  )

  # le quote sono quelle dei servizi del database, cioè dei censiti
  quote <- prop.table(table(ultimi$servizio_transponder))
  attese <- c(
    SECCO = 0.40,
    CARTA = 0.27,
    VETRO = 0.11,
    UMIDO = 0.10,
    "PLASTICA E METALLI" = 0.07,
    "VERDE E RAMAGLIE" = 0.05
  )
  expect_setequal(names(quote), names(attese))
  expect_equal(
    as.numeric(quote[names(attese)]),
    unname(attese),
    tolerance = 0.1
  )

  # raccolta a cadenza fissa: un contenitore tipico ha una ventina di letture l'anno
  nel_2025 <- d[
    lubridate::year(d$giorno_lettura) == 2025 &
      d$presente_a_database == "Presente",
  ]
  letture <- as.integer(table(nel_2025$RFID))
  expect_gt(stats::median(letture), 15)
  expect_lt(stats::median(letture), 27)

  expect_equal(dplyr::n_distinct(d$id_utenza, na.rm = TRUE), 50)
  cambi_utenza <- tapply(d$id_utenza, d$RFID, function(x) {
    dplyr::n_distinct(x, na.rm = TRUE)
  })
  expect_gte(sum(cambi_utenza > 1), 5)
  expect_lte(sum(cambi_utenza > 1), 10)

})

test_that("ogni lettura ha il giro e il comune del mezzo che l'ha fatta", {
  esito <- carica_dataset(percorso_dataset_esempio())
  expect_length(esito$avvisi, 0)
  d <- esito$dati
  # una lettura esiste solo se un mezzo passa durante un giro: servizio
  # atteso e comune del giro ci sono sempre, anche per i censiti
  expect_false(anyNA(d$servizio_atteso))
  expect_false(anyNA(d$comune_giro))
  # nell'esempio nessuna lettura è fuori dai comuni serviti
  expect_identical(d$comune_lettura, d$comune_geografico)
  expect_true(all(d$comune_lettura %in% comuni_cantieri()$comune))
  # una lettura su cinque è fatta in un comune diverso da quello del giro:
  # quasi tutte sono dei due tag anomali
  expect_equal(sum(d$comune_lettura != d$comune_giro), 1218)
  expect_false(anyNA(d$comune_assegnato))
  expect_false(anyNA(d$cantiere))
  # la coerenza del comune: i censiti contro il comune del database, i non
  # censiti contro quello del giro. I sacchetti censiti, senza un comune a
  # database, hanno la coerenza ignota
  conteggi <- table(d$coerenza_comune, d$presente_a_database)
  expect_equal(
    as.integer(conteggi[unname(stati_coerenza_comune()), "Presente"]),
    c(5010L, 389L, 0L, 8L)
  )
  expect_equal(
    as.integer(conteggi[unname(stati_coerenza_comune()), "Non Presente"]),
    c(450L, 0L, 618L, 0L)
  )
  expect_true(all(
    d$coerenza_comune[rfid_sacchetto(d$RFID) & d$presente_a_database == "Presente"] ==
      "Coerenza ignota"
  ))
  # la coerenza scritta nel file è quella che l'app ricalcola
  grezzo <- utils::read.csv(percorso_dataset_esempio(), stringsAsFactors = FALSE)
  expect_identical(grezzo$coerenza_comune, d$coerenza_comune)

  # per i censiti il giro è quasi sempre quello del loro servizio
  censita <- !is.na(d$servizio_transponder)
  coerenti <- tipologia_servizio(d$servizio_transponder[censita]) ==
    tipologia_servizio(d$servizio_giro[censita])
  expect_gt(mean(coerenti), 0.95)
  expect_lt(mean(coerenti), 1)

  # tra i contenitori non censiti del 2025 ci sono stime nette e stime incerte
  nel_2025 <- filtra_periodo(d, periodo_anno(2025))
  stime <- servizio_prevalente_non_censiti(nel_2025)
  non_censiti <- unique(nel_2025$RFID[nel_2025$presente_a_database == "Non Presente"])
  expect_setequal(stime$RFID, non_censiti)
  # tre dei non censiti sono sacchetti: non hanno un servizio da stimare
  expect_length(non_censiti, 40)
  stime <- stime[!rfid_sacchetto(stime$RFID), ]
  expect_equal(nrow(stime), 37)
  expect_equal(sum(!is.na(stime$servizio_prevalente)), 18)
  expect_equal(sum(is.na(stime$servizio_prevalente)), 19)
  expect_true(all(stime$quota[is.na(stime$servizio_prevalente)] < 0.8))
  # il tag fermo in deposito è letto dai giri di tutti i mezzi: stima incerta
  expect_true(is.na(stime$servizio_prevalente[stime$RFID == "RFD20241001301"]))
})

test_that("i servizi attesi hanno i nomi del database e hanno tutti un'icona", {
  d <- dati_esempio()
  # il servizio atteso è il giro con il nome della sua tipologia: i giri che
  # raccolgono un rifiuto hanno il nome del servizio del database
  attesi <- c(
    "SECCO",
    "CARTA",
    "VETRO",
    "UMIDO",
    "PLASTICA E METALLI",
    "VERDE E RAMAGLIE",
    "PULIZIA TERRITORIO",
    "SERVIZI"
  )
  expect_setequal(unique(d$servizio_atteso), attesi)
  expect_identical(uniforma_servizio(d$servizio_atteso), d$servizio_atteso)
  expect_true(all(d$servizio_atteso %in% servizi_config()$servizio))
  # il giro sta in una colonna sua, con il nome del calendario: i giri sono
  # dodici, e il secco porta a porta è il più letto
  expect_equal(dplyr::n_distinct(d$servizio_giro), 12)
  expect_false(anyNA(d$servizio_giro))
  expect_equal(sum(d$servizio_giro == "SECCO PAP"), 2798)
  expect_true(all(
    c("CARTA CONT.STRADALI", "CARTA/CARTONE PAP", "UMIDO PAP", "VETRO PAP") %in%
      d$servizio_giro
  ))
  # fuori dai giri dei servizi è il giro, con il nome del database. Nei giri
  # dei servizi dell'esempio non passa nessun censito: resta il giro
  expect_identical(d$servizio_atteso, uniforma_servizio(d$servizio_giro))
  expect_equal(
    sum(d$servizio_giro %in% giri_servizio_dai_censiti()),
    52
  )
  # a database ci sono i sei servizi
  expect_setequal(
    unique(stats::na.omit(d$servizio_transponder)),
    c("SECCO", "CARTA", "VETRO", "UMIDO", "PLASTICA E METALLI", "VERDE E RAMAGLIE")
  )

  # il file ha le colonne del passo 3
  grezzo <- utils::read.csv(
    percorso_dataset_esempio(),
    colClasses = "character",
    na.strings = "NA"
  )
  expect_identical(
    names(grezzo),
    c(
      "giorno_lettura",
      "giorno",
      "targa_veicolo",
      "matricola_veicolo",
      "id_giro",
      "eseguito",
      "RFID",
      "presente_a_database",
      "servizio_transponder",
      "servizio_atteso",
      "coerenza_servizio",
      "servizio_giro",
      "volume",
      "frequenza",
      "numero_raccolte_annue",
      "codice_categoria",
      "comune_fornitura",
      "comune_giro",
      "comune_lettura",
      "coerenza_comune",
      "cantiere",
      "data_attivazione_contenitore",
      "data_cessazione_contenitore",
      "id_fornitura",
      "stato_fornitura",
      "id_utenza",
      "latitudine",
      "longitudine"
    )
  )
  # la coerenza del file è quella che l'app ricalcola al caricamento
  expect_identical(grezzo$coerenza_servizio, d$coerenza_servizio)
  expect_identical(
    c(
      sum(d$coerenza_servizio %in% "SI"),
      sum(d$coerenza_servizio %in% "NO"),
      sum(is.na(d$coerenza_servizio))
    ),
    c(5376L, 23L, 1076L)
  )
  # i non censiti e i sacchetti non hanno un servizio a database: la loro
  # coerenza manca
  expect_identical(is.na(d$coerenza_servizio), is.na(d$servizio_transponder))
  # le letture non coerenti sono di quattro censiti, letti in un giro del
  # secco: due lo sono in ogni lettura, il contenitore che ha cambiato
  # servizio e quello spostato in una sola
  non_coerenti <- d[d$coerenza_servizio %in% "NO", ]
  expect_true(all(non_coerenti$presente_a_database == "Presente"))
  expect_true(all(non_coerenti$servizio_giro == "SECCO PAP"))
  expect_identical(
    as.integer(table(non_coerenti$RFID)[c(
      "RFD20241007190",
      "RFD20241011121",
      "RFD20250901001",
      "RFD20250905100"
    )]),
    c(10L, 11L, 1L, 1L)
  )
  # il servizio atteso è quello che esce dalla regola del pacchetto
  expect_identical(
    servizio_atteso_dal_giro(dplyr::mutate(
      d,
      giorno = as.Date(giorno_lettura),
      id_giro = as.integer(grezzo$id_giro)
    )),
    d$servizio_atteso
  )
  # le colonne del contenitore che l'app non usa ci sono, per i censiti
  expect_identical(is.na(grezzo$frequenza), is.na(d$servizio_transponder))
  expect_true(all(grezzo$eseguito == "SI"))
  expect_identical(grezzo$giorno, substr(grezzo$giorno_lettura, 1, 10))
})
test_that("casi speciali presenti", {
  d <- dplyr::arrange(dati_esempio(), giorno_lettura)
  letture <- function(rfid) d[d$RFID == rfid, ]

  # righe di esempio della prima specifica
  expect_identical(
    format(letture("RFD20250901001")$giorno_lettura[1]),
    "2025-09-01 06:00:00"
  )
  expect_identical(
    format(letture("RFD20250901002")$giorno_lettura[1]),
    "2025-09-01 06:15:00"
  )
  expect_identical(
    format(letture("RFD20250901201")$giorno_lettura[1]),
    "2025-09-01 06:30:00"
  )

  cambio_servizio <- letture("RFD20250901001")
  expect_identical(
    cambio_servizio$servizio_transponder[1:3],
    c("CARTA", "SECCO", "CARTA")
  )
  expect_identical(
    format(cambio_servizio$giorno_lettura[1:3], "%d/%m"),
    c("01/09", "15/09", "28/09")
  )
  expect_true(all(cambio_servizio$servizio_transponder[-2] == "CARTA"))

  cambio_utenza <- letture("RFD20250901050")
  expect_identical(cambio_utenza$id_utenza[1], "UTZ001")
  expect_identical(format(cambio_utenza$giorno_lettura[1], "%d/%m"), "01/09")
  prima_nuova <- cambio_utenza[cambio_utenza$id_utenza == "UTZ025", ][1, ]
  expect_identical(format(prima_nuova$giorno_lettura, "%d/%m"), "20/09")

  con_stima <- letture("RFD20250915201")
  expect_equal(nrow(con_stima), 4)
  expect_true(all(con_stima$servizio_atteso == "SECCO"))

  # due letture in due giri di tipologie diverse: la stima resta incerta
  incerto <- letture("RFD20250920250")
  expect_equal(nrow(incerto), 2)
  expect_identical(incerto$servizio_atteso, c("SECCO", "CARTA"))
  expect_true(all(is.na(incerto$comune_fornitura)))
  expect_false(anyNA(incerto$comune_giro))

  spostato <- letture("RFD20250905100")
  expect_equal(spostato$latitudine[1:2], c(45.4744, 45.6488))
  expect_equal(spostato$longitudine[1:2], c(11.8449, 11.7836))
})

test_that("il dataset contiene alcuni sacchetti, censiti e non censiti", {
  d <- dati_esempio()
  sacchetto <- rfid_sacchetto(d$RFID)
  s <- d[sacchetto, ]
  # un sacchetto è monouso: dopo lo svuotamento non si legge più, quindi ha
  # una lettura sola
  expect_equal(nrow(s), 12)
  expect_equal(anyDuplicated(s$RFID), 0)
  ultimi <- ultime_di(s)
  expect_equal(nrow(ultimi), 12)
  expect_equal(sum(ultimi$presente_a_database == "Presente"), 8)
  expect_equal(sum(ultimi$presente_a_database == "Non Presente"), 4)
  # codice di 24 caratteri con il prefisso dei sacchetti: i contenitori ne
  # hanno un altro
  expect_true(all(nchar(s$RFID) == 24 & startsWith(s$RFID, "00BD")))
  expect_false(any(startsWith(d$RFID[!sacchetto], "00BD")))

  # a database un sacchetto ha solo la fornitura del suo gruppo, niente
  # utenza: il suo servizio atteso è il giro che lo ha letto, censito o no, e
  # il suo comune è quello in cui viene letto. La voce dei sacchetti gliela
  # dà l'app, dal codice
  censito <- s$presente_a_database == "Presente"
  expect_true(all(s$servizio_atteso == "SECCO"))
  expect_true(all(is.na(s$servizio_transponder)))
  expect_true(all(servizio_mostrato(s) == servizio_sacchetti()))
  expect_true(all(is.na(s$id_utenza)))
  expect_true(all(is.na(s$comune_fornitura)))
  # la fornitura sta nel file, che l'app non carica: si legge dal CSV
  grezzo <- utils::read.csv(percorso_dataset_esempio(), colClasses = "character")
  forniture <- grezzo$id_fornitura[rfid_sacchetto(grezzo$RFID)]
  censiti_grezzo <- grezzo$presente_a_database[rfid_sacchetto(grezzo$RFID)] == "Presente"
  expect_setequal(forniture[censiti_grezzo], sprintf("SAC%03d", 1:4))
  expect_true(all(is.na(forniture[!censiti_grezzo])))
  expect_false(any(forniture[censiti_grezzo] %in% grezzo$id_fornitura[!rfid_sacchetto(grezzo$RFID)]))
  expect_identical(s$comune_assegnato, s$comune_lettura)
  expect_identical(s$comune_lettura, s$comune_giro)
  expect_setequal(s$cantiere, cantieri())
  # letti dai giri del secco
  expect_true(all(s$servizio_atteso == "SECCO"))
  # due sono letti solo nel 2024, gli altri dieci nel 2025
  expect_equal(
    as.integer(table(lubridate::year(ultimi$giorno_lettura))),
    c(2, 10)
  )

  # nel 2025 icona, voce dei filtri e statistiche sono quelle dei sacchetti,
  # censiti o no
  nel_2025 <- aggiungi_servizio_icona(filtra_periodo(d, periodo_anno(2025)))
  expect_true(all(
    servizio_mostrato(nel_2025)[rfid_sacchetto(nel_2025$RFID)] ==
      servizio_sacchetti()
  ))
  stat <- calcola_statistiche(ultime_esempio(periodo = periodo_anno(2025)))
  expect_equal(stat$servizio_censiti[[servizio_sacchetti()]], 7)
  expect_equal(stat$servizio_stimato[[servizio_sacchetti()]], 3)
  # i contenitori non censiti restano 18 con la stima e 19 con la stima incerta
  expect_equal(sum(stat$servizio_stimato) - 3, 18)
  expect_equal(stat$stima_incerta, 19)

  # l'analisi dei cluster riguarda i contenitori: i sacchetti restano fuori
  analisi <- analisi_esempio()
  expect_false(any(rfid_sacchetto(analisi$RFID)))
  expect_equal(dplyr::n_distinct(analisi$RFID), 238)
})

test_that("il dataset contiene un esempio per ogni indicatore di cluster", {
  analisi <- analisi_esempio()
  indicatore <- function(rfid) {
    unique(analisi$indicatore_cluster[analisi$RFID == rfid])
  }

  expect_identical(indicatore("RFD20241001301"), "DEPOT_STUCK")
  expect_identical(indicatore("RFD20250203302"), "TRUCK_STOWAWAY")
  expect_identical(indicatore("RFD20241003303"), "SCATTERED_READS / GPS_NOISE")
  expect_identical(indicatore("RFD20250905100"), "RELOCATED_BIN")
  expect_identical(indicatore("RFD20250915201"), "GHOST_TAG")
  expect_identical(indicatore("RFD20250901001"), "VALID_TARGET")

  conteggi <- table(analisi$indicatore_cluster)
  expect_setequal(
    names(conteggi),
    setdiff(indicatori_config()$indicatore, "UNCATEGORIZED")
  )
  # la maggior parte dei contenitori è al suo posto
  expect_gt(conteggi[["VALID_TARGET"]] / dplyr::n_distinct(analisi$RFID), 0.7)
})

test_that("l'output di esempio incluso corrisponde all'analisi del dataset", {
  file <- app_sys("extdata", "cluster_analysis_2025-01-01_2025-12-31.csv")
  expect_true(file.exists(file))
  salvato <- utils::read.csv(file, stringsAsFactors = FALSE)
  analisi <- analisi_esempio()

  expect_identical(names(salvato), colonne_analisi_cluster())
  expect_equal(nrow(salvato), nrow(analisi))
  expect_identical(salvato$RFID, analisi$RFID)
  expect_identical(salvato$indicatore_cluster, analisi$indicatore_cluster)
  expect_equal(salvato$cluster_indice_fiducia, analisi$cluster_indice_fiducia)
})

test_that("la geografia segue la tabella dei comuni e le quote dei cantieri", {
  d <- dati_esempio()
  non_presente <- d$presente_a_database == "Non Presente"

  # comune del database: solo per i contenitori censiti, uno per RFID. Un
  # sacchetto non lo ha nemmeno da censito
  sacchetto <- rfid_sacchetto(d$RFID)
  expect_true(all(is.na(d$comune_fornitura[non_presente | sacchetto])))
  expect_false(anyNA(d$comune_fornitura[!non_presente & !sacchetto]))
  per_rfid <- tapply(
    d$comune_fornitura[!non_presente],
    d$RFID[!non_presente],
    dplyr::n_distinct
  )
  expect_true(all(per_rfid == 1))

  # tutti i comuni sono in tabella; il cantiere è quello del comune assegnato
  comuni <- stats::na.omit(c(d$comune_fornitura, d$comune_giro))
  expect_true(all(comuni %in% comuni_cantieri()$comune))
  # il comune di lettura è quello il cui confine contiene il punto: nel
  # dataset di esempio ogni lettura sta in un comune servito
  expect_false(anyNA(d$comune_geografico))
  confini <- split(poligoni_comuni(), poligoni_comuni()$comune)
  punti <- unique(d[, c("latitudine", "longitudine", "comune_lettura")])
  expect_true(all(vapply(
    seq_len(nrow(punti)),
    function(i) {
      dentro_poligoni(
        punti$longitudine[i],
        punti$latitudine[i],
        confini[[punti$comune_lettura[i]]]
      )
    },
    logical(1)
  )))
  expect_identical(d$comune_assegnato, d$comune_geografico)
  expect_identical(d$cantiere, cantiere_di_comune(d$comune_assegnato))
  expect_false(anyNA(d$cantiere))

  # quote dei cantieri tra i contenitori: 12%, 26%, 37% e 25%
  # comune e cantiere di ogni RFID: quelli del database per i censiti, il
  # comune in cui cade il maggior numero di letture per gli altri
  in_ordine <- d[order(d$giorno_lettura), ]
  rfid <- indice_gruppi(in_ordine$RFID)
  anagrafica <- geografia_per_gruppo(in_ordine, rfid, max(rfid))
  quote <- prop.table(table(anagrafica$cantiere))
  expect_equal(
    as.numeric(quote[c("ASIAGO", "BASSANO", "CAMPOSAMPIERO", "RUBANO")]),
    c(0.12, 0.26, 0.37, 0.25),
    tolerance = 0.1
  )
  # i giri del calendario lavorano in 32 comuni
  expect_equal(dplyr::n_distinct(anagrafica$comune, na.rm = TRUE), 32)

  # tutte le letture cadono nella zona servita
  zona <- riquadro_zona()
  expect_true(all(d$latitudine > zona$lat_min & d$latitudine < zona$lat_max))
  expect_true(all(d$longitudine > zona$lng_min & d$longitudine < zona$lng_max))

  # il contenitore spostato resta registrato a Limena, cantiere di Rubano, ma
  # dalla seconda lettura in poi viene letto a Cittadella
  spostato <- d[d$RFID == "RFD20250905100", ]
  expect_identical(unique(spostato$comune_fornitura), "LIMENA")
  expect_identical(spostato$comune_lettura[1:2], c("LIMENA", "CITTADELLA"))
  expect_identical(spostato$comune_giro[1:2], c("LIMENA", "CITTADELLA"))
  expect_identical(spostato$comune_geografico[1:2], c("LIMENA", "CITTADELLA"))
  # il cantiere di una lettura è quello del suo comune di lettura
  expect_identical(
    spostato$cantiere,
    c("RUBANO", rep("CAMPOSAMPIERO", nrow(spostato) - 1))
  )
  # fuori comune dopo lo spostamento; un censito non è mai fuori giro
  expect_identical(
    spostato$coerenza_comune,
    c("Coerente", rep("Fuori comune", nrow(spostato) - 1))
  )
  expect_identical(
    unique(d$comune_fornitura[d$RFID == "RFD20250901001"]),
    "BASSANO DEL GRAPPA"
  )
  # il tag rimasto sul mezzo viene letto durante giri di più comuni, sempre
  # lungo lo stesso percorso
  sul_mezzo <- d[d$RFID == "RFD20250203302", ]
  expect_gt(dplyr::n_distinct(sul_mezzo$comune_giro), 1)
  expect_setequal(
    sul_mezzo$comune_lettura,
    c("MAROSTICA", "BASSANO DEL GRAPPA", "NOVE")
  )
})
