test_that("i file estratti hanno sempre lo stesso nome", {
  file <- file_origine("origine")
  expect_named(
    file,
    c(
      "test_mezzi",
      "antenne_grezze",
      "contenitori",
      "tag_contenitori",
      "gruppi_sacchetti",
      "servizi_sacchetti",
      "calendario",
      "non_pianificati",
      "giri_aggiunti",
      "componenti",
      "manovre",
      "saponette_grezze"
    )
  )
  # i file grezzi delle letture hanno un nome diverso dai risultati della pipeline
  expect_identical(
    basename(unlist(file[c("antenne_grezze", "saponette_grezze")])),
    c("letture_antenne_grezze.csv", "letture_saponette_grezze.csv")
  )
  expect_identical(
    file$test_mezzi,
    file.path("origine", "AppSheet.ViewData.csv")
  )
  expect_error(leggi_tabella_origine("non_esiste.csv"), "File non trovato")
})

test_that("ogni tabella estratta si importa con i nomi e i tipi del progetto", {
  file <- scrivi_origine_prova(withr::local_tempdir())

  test <- importa_test_mezzi(file$test_mezzi)
  expect_named(
    test,
    c("targa_veicolo", "matricola_veicolo", "esito", "data_test")
  )
  expect_identical(test$data_test, as.Date(c("2026-07-31", "2026-07-31", NA)))
  # il trattino vale come mancante
  expect_true(is.na(test$esito[3]))

  letture <- importa_letture_antenne(file$antenne_grezze)
  expect_identical(
    names(letture),
    c(
      "data_ora_lettura",
      "data_lettura",
      "targa_veicolo",
      "RFID",
      "comune_lettura",
      "NumeroLetture",
      "latitudine",
      "longitudine",
      "cantiere"
    )
  )
  # fuori la posizione zero e la lettura senza comune
  expect_equal(nrow(letture), 9)
  # il comune è il pezzo dopo la prima virgola, in maiuscolo; il cantiere
  # viene dalla tabella, per nome esatto. La frazione e VENEZIA restano come
  # sono: il comune dalla posizione lo trova `correggi_comune_lettura()`
  expect_setequal(
    letture$comune_lettura,
    c("LIMENA", "TAGGI DI PROVA", "VENEZIA")
  )
  expect_true(all(
    letture$cantiere[letture$comune_lettura == "LIMENA"] == "RUBANO"
  ))
  expect_true(all(is.na(letture$cantiere[letture$comune_lettura != "LIMENA"])))
  # un RFID letto più volte nello stesso giorno: resta la prima lettura valida
  doppie <- importa_letture_antenne(scrivi_csv(c(
    "IndirizzoLocalizzazione;DataeOraPrimaLettura;RFID(EPC);Targa;Latitudine;Longitudine;NumeroLetture",
    "VIA ROMA 1, LIMENA;03/08/2026 10.00.00;123;AB123CD;45,4700;11,8400;1",
    "VIA ROMA 1, LIMENA;03/08/2026 08.00.00;123;AB123CD;45,4700;11,8400;1",
    # più presto, ma sporche: non contano
    "VIA ROMA 1, LIMENA;03/08/2026 07.00.00;123;AB123CD;0;0;1",
    "SENZA COMUNE;03/08/2026 07.30.00;123;AB123CD;45,4700;11,8400;1",
    "VIA ROMA 1, LIMENA;04/08/2026 08.00.00;123;AB123CD;45,4700;11,8400;1"
  )))
  expect_identical(format(doppie$data_ora_lettura, "%d %H"), c("03 08", "04 08"))
  # le letture escono in ordine di istante
  expect_false(is.unsorted(letture$data_ora_lettura))
  prima <- letture[letture$RFID == "123", ][1, ]
  expect_equal(prima$latitudine, 45.47)
  expect_identical(
    format(prima$data_ora_lettura, "%Y-%m-%d %H:%M:%S"),
    "2026-08-03 08:15:00"
  )
  expect_identical(prima$data_lettura, as.Date("2026-08-03"))

  contenitori <- importa_contenitori(file$contenitori)
  # fuori il contenitore senza codice
  expect_equal(nrow(contenitori), 1)
  expect_identical(contenitori$id_fornitura, "F1")
  expect_identical(contenitori$volume, 120)
  expect_identical(
    contenitori$data_cessazione_contenitore,
    as.Date("2099-12-31")
  )
  # i nomi dei comuni si correggono
  corretto <- importa_contenitori(scrivi_csv(c(
    readLines(file$contenitori)[1],
    "1,SECCO,120,Q,26,2020-01-01,2024-12-31,F9,CESSATA,U9,VALSTAGNA,31",
    "2,SECCO,120,Q,26,2020-01-01,,F8,ATTIVA,U8,LUSINA CONCO,31"
  )))
  expect_identical(corretto$comune_fornitura, c("VALBRENTA", "LUSIANA CONCO"))
  expect_identical(
    corretto$data_cessazione_contenitore[1],
    as.Date("2024-12-31")
  )

  # di un gruppo di sacchetti vale l'associazione più recente
  sacchetti <- importa_sacchetti(file$gruppi_sacchetti, file$servizi_sacchetti)
  expect_identical(sacchetti$id_fornitura, "S1")

  calendario <- importa_calendario(file$calendario)
  # fuori il turno senza mezzo; la matricola perde la lettera iniziale
  expect_equal(nrow(calendario), 6)
  expect_setequal(calendario$matricola_mezzo, c("1601", "1602"))
  expect_identical(calendario$comune_giro[6], "ROSÀ")
  expect_identical(calendario$eseguito, c("SI", "NO", "SI", "SI", "SI", "SI"))
  expect_s3_class(calendario$data_giro, "Date")
  # un file estratto con il nome vecchio della data vale lo stesso
  vecchio <- importa_calendario(scrivi_csv(c(
    "giorno,ora_inizio,ora_fine,matricola_mezzo,id_giro,eseguito,descrizione_servizio,comune_giro",
    "2026-08-03,06:00:00,12:00:00,X1601,10,SI,SECCO PAP,LIMENA"
  )))
  expect_identical(vecchio$data_giro, as.Date("2026-08-03"))
  # i giri non pianificati: stesse regole, senza orari
  non_pianificati <- importa_giri_non_pianificati(file$non_pianificati)
  expect_named(
    non_pianificati,
    c("data_giro", "matricola_mezzo", "descrizione_servizio", "comune_giro")
  )
  expect_identical(non_pianificati$matricola_mezzo, "1601")
  expect_identical(non_pianificati$data_giro, as.Date("2026-08-03"))
  expect_error(
    importa_giri_non_pianificati(scrivi_csv(c(
      "data_giro,matricola_mezzo",
      "2026-08-03,X1601"
    ))),
    "mancano le colonne: descrizione_servizio, comune_giro"
  )

  componenti <- importa_componenti(file$componenti)
  expect_identical(componenti$nucleo_residenti, c(2L, 3L))
  expect_identical(
    format(componenti$data_inizio_decorrenza[2], "%Y-%m-%d"),
    "2026-08-15"
  )
  expect_true(is.na(componenti$data_fine_decorrenza[2]))

  manovre <- importa_manovre(file$manovre)
  expect_named(
    manovre,
    c("id_fornitura", "data_inizio_dichiarazione", "data_fine_dichiarazione")
  )
  expect_identical(manovre$id_fornitura, "F1")
  expect_identical(manovre$data_fine_dichiarazione, as.Date("2099-12-31"))

  saponette <- importa_letture_saponette(file$saponette_grezze)
  # la riga ripetuta conta una volta, quella senza codice resta fuori; i
  # sacchetti restano, come per le letture con le antenne
  expect_named(
    saponette,
    c("data_ora_lettura", "data_lettura", "matricola_saponetta", "RFID")
  )
  # l'importazione normalizza i codici ma non li ripara e non li scarta
  expect_identical(
    saponette$RFID,
    c(
      "0000000123",
      "00BD00000000000000000001",
      "0000000123",
      "000000084C",
      "12345678901",
      "48455241AABB"
    )
  )
  expect_identical(
    saponette$matricola_saponetta,
    c("T1", "T1", "T3", "T4", "T5", "T5")
  )
  expect_identical(
    saponette$data_lettura,
    as.Date(c(
      "2025-03-01",
      "2025-03-02",
      "2026-08-21",
      "2026-08-22",
      "2026-08-23",
      "2026-08-24"
    ))
  )
  expect_identical(
    format(saponette$data_ora_lettura[1], "%Y-%m-%d %H:%M:%S"),
    "2025-03-01 07:00:00"
  )
  # il file di prima di ottobre 2026 aveva altri nomi: un errore chiaro
  expect_error(
    importa_letture_saponette(scrivi_csv(c(
      "data_lettura,codice_transponder,id_trasmissione",
      "2025-03-01 07:00:00,123,T1"
    ))),
    "mancano le colonne: data_ora_lettura, RFID, matricola_saponetta"
  )
  # anche le date italiane; un istante illeggibile si scarta, con un avviso
  expect_warning(
    italiane <- importa_letture_saponette(scrivi_csv(c(
      "data_ora_lettura,data_lettura,RFID,matricola_saponetta",
      "01/03/2025 07:00:00,01/03/2025,123,T1",
      "ieri,2025-03-01,124,T1"
    ))),
    "1 letture delle saponette hanno un istante illeggibile.*ieri"
  )
  expect_identical(italiane$RFID, "0000000123")
  expect_identical(italiane$data_lettura, as.Date("2025-03-01"))
  # il nome della colonna scritto con lo spazio vale come con il trattino basso
  con_spazio <- importa_letture_saponette(scrivi_csv(c(
    "data_ora_lettura,data_lettura,RFID,matricola saponetta",
    "2025-03-01 07:00:00,2025-03-01,123,T1"
  )))
  expect_identical(con_spazio$matricola_saponetta, "T1")
  # senza un giorno leggibile in data_lettura vale quello dell'istante
  senza_giorno <- importa_letture_saponette(scrivi_csv(c(
    "data_ora_lettura,data_lettura,RFID,matricola_saponetta",
    "2025-03-01 07:00:00,,123,T1"
  )))
  expect_identical(senza_giorno$data_lettura, as.Date("2025-03-01"))

  tag <- importa_tag_contenitori(file$tag_contenitori)
  # il primo valore non vuoto tra codice_transponder e codice_etichetta
  expect_identical(tag$codice_transponder, c("0000000128", "0000000999"))
  # basta uno dei due campi; senza nessuno è un errore
  solo_etichetta <- importa_tag_contenitori(scrivi_csv(c(
    "codice_etichetta,altro",
    "abc,1",
    ",2",
    "abc,3"
  )))
  expect_identical(solo_etichetta$codice_transponder, "0000000ABC")
  expect_error(
    importa_tag_contenitori(scrivi_csv(c("altro", "1"))),
    "manca sia codice_transponder sia codice_etichetta"
  )
})

test_that("dai file estratti si arriva al CSV dell'app, senza tabelle intermedie", {
  cartella <- withr::local_tempdir()
  file <- scrivi_origine_prova(file.path(cartella, "origine"))
  # il comune della frazione viene dalla posizione; due giri da controllare
  messaggi <- character(0)
  avvisi <- character(0)
  letture <- withCallingHandlers(
    prepara_letture_antenne(file),
    message = function(m) {
      messaggi <<- c(messaggi, conditionMessage(m))
      invokeRestart("muffleMessage")
    },
    warning = function(w) {
      avvisi <<- c(avvisi, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_match(
    messaggi,
    "per 1 letture.*TAGGI DI PROVA \u2192 LIMENA \\(1\\)",
    all = FALSE
  )
  # quante letture ha associato ogni passo
  expect_match(
    messaggi,
    "^Giro associato alle letture: 5 giro pianificato, nell'orario e nel comune",
    all = FALSE
  )
  expect_length(avvisi, 2)
  expect_match(
    avvisi[1],
    "^1 giri sono ancora non confermati dopo pi\u00f9 di 7 giorni"
  )
  expect_match(
    avvisi[2],
    "^1 giri confermati di mezzi con l'antenna non hanno nessuna lettura"
  )
  expect_identical(
    names(letture)[29:35],
    colonne_nucleo_manovra()
  )
  expect_equal(ncol(letture), 35)
  # censito tre volte, tre non censiti una; il sacchetto resta, con la
  # fornitura del suo gruppo
  expect_identical(
    sort(unique(letture$RFID)),
    c("0000000123", "0000000128", "0000000129", "0000000999", "00BD00000000000000000001")
  )
  # le letture scartate dalla pulizia stanno nel registro: posizione zero e
  # senza comune; quelle del mezzo non collaudato e prima del collaudo non
  # contano
  scartate_antenne <- letture_scartate(letture)
  expect_identical(
    sort(scartate_antenne$motivo),
    c("senza_posizione_o_comune", "senza_posizione_o_comune")
  )
  # il codice è già nel formato comune, per confrontarlo con quello delle saponette
  expect_identical(sort(scartate_antenne$RFID), c("0000000124", "0000000125"))
  expect_true(all(scartate_antenne$matricola_veicolo == "1601"))
  expect_true(all(c("data_ora_lettura", "data_lettura", "RFID") %in% names(scartate_antenne)))
  sacchetto <- letture[letture$RFID == "00BD00000000000000000001", ]
  expect_identical(sacchetto$presente_a_database, "PIA")
  expect_identical(sacchetto$id_fornitura, "S1")
  # la frazione era dentro LIMENA: il comune del giro, coerente
  frazione <- letture[letture$RFID == "0000000128", ]
  expect_identical(frazione$comune_lettura, "LIMENA")
  expect_identical(frazione$cantiere, "RUBANO")
  # la frazione (128) e il non censito di LIMENA (999) sono tra i tag
  # dell'azienda, ma senza fornitura: PIT; VENEZIA (129) non e' conosciuto: NP
  expect_identical(frazione$presente_a_database, "PIT")
  expect_true(all(letture$presente_a_database[letture$RFID == "0000000999"] == "PIT"))
  expect_true(all(letture$presente_a_database[letture$RFID == "0000000129"] == "NP"))
  expect_true(all(is.na(frazione$id_fornitura)))
  expect_identical(frazione$coerenza_comune, "Coerente")
  expect_true(is.na(frazione$distanza_comune_dichiarato))
  # VENEZIA non e' un comune servito: resta, fuori giro, lontano da LIMENA
  venezia <- letture[letture$RFID == "0000000129", ]
  expect_identical(venezia$comune_lettura, "VENEZIA")
  expect_identical(venezia$coerenza_comune, "Fuori giro")
  # fuori dal comune del pianificato ma nel suo orario: l'orario conta più
  # del comune, e vince il pianificato sul non pianificato
  expect_identical(venezia$servizio_giro, "SECCO PAP")
  expect_identical(venezia$id_giro, "10")
  expect_gt(venezia$distanza_comune_dichiarato, 25000)
  censite <- letture[letture$RFID == "0000000123", ]
  expect_true(all(censite$presente_a_database == "PIA"))
  expect_true(all(censite$id_fornitura == "F1"))
  expect_true(all(censite$servizio_giro == "SECCO PAP"))
  expect_true(all(censite$servizio_atteso == "SECCO"))
  # il nucleo cambia a metà agosto, il potenziamento comincia il 20
  expect_identical(censite$nucleo_residenti, c(2L, 3L, 3L))
  expect_identical(censite$potenziamento_secco, c(FALSE, FALSE, TRUE))
  # un non censito non ha fornitura: il potenziamento manca
  expect_true(is.na(letture$potenziamento_secco[letture$RFID == "0000000999"]))
  # l'ultimo giro del calendario era a ROSÀ, ma un censito si confronta con
  # il comune del database
  expect_true(all(censite$coerenza_comune == "Coerente"))
  expect_identical(
    scarta_codici_nulli(data.frame(
      RFID = c("0000000000123", "00BD1", "12")
    ))$RFID,
    c("00BD1", "12")
  )

  # CSV e archivio .duckdb, scritti come nella pipeline: l'app li carica
  output <- file.path(cartella, "output")
  csv <- scrivi_csv_output(letture, "letture_antenne.csv", output)
  percorsi <- c(
    csv,
    scrivi_archivio(csv, file.path(output, "letture_antenne.duckdb")),
    scrivi_csv_output(giri_da_controllare(letture), "controllo_giri.csv", output, na = "")
  )
  expect_identical(
    basename(percorsi),
    c("letture_antenne.csv", "letture_antenne.duckdb", "controllo_giri.csv")
  )
  # le letture con il comune del GPS sostituito: la frazione
  corretti <- comuni_corretti(letture)
  expect_identical(corretti$RFID, "0000000128")
  expect_identical(corretti$comune_gps, "TAGGI DI PROVA")
  expect_identical(corretti$comune_posizione, "LIMENA")
  # senza l'attributo i giri da controllare sono una tabella vuota
  expect_equal(nrow(giri_da_controllare(data.frame(data_lettura = as.Date(character(0))))), 0)
  # i giri da controllare: il giro ancora NO e quello confermato senza
  # letture; il giro confermato del mezzo senza antenna non conta
  giri <- utils::read.csv(percorsi[3], colClasses = "character")
  expect_identical(giri$data_giro, c("2026-08-05", "2026-08-10"))
  expect_identical(giri$id_giro, c("12", "10"))
  expect_identical(
    giri$problema,
    c(
      "non confermato dopo 7 giorni",
      "confermato, senza letture dell'antenna"
    )
  )
  caricato <- carica_archivio(percorsi[2])
  withr::defer(chiudi_archivio(caricato$archivio))
  expect_identical(caricato$archivio$n, nrow(letture))
  # il CSV ha tutte le colonne della pipeline; l'archivio .duckdb le sole
  # colonne dell'app
  dal_csv <- carica_dataset(percorsi[1], colonne = "tutte")$dati
  dal_duckdb <- carica_dataset(percorsi[2])$dati
  expect_named(dal_duckdb, colonne_app())
  expect_identical(dal_duckdb, dal_csv[colonne_app()])
  expect_true(all(
    names(letture) %in% c(names(dal_csv), "volume", "numero_raccolte_annue")
  ))
  expect_identical(dal_csv$potenziamento_secco, letture$potenziamento_secco)
  expect_identical(
    dal_csv$nucleo_residenti,
    as.integer(letture$nucleo_residenti)
  )
  expect_identical(dal_csv$id_fornitura, letture$id_fornitura)

  # le letture delle saponette: la stessa anagrafica, sacchetti compresi, con le
  # colonne in comune con quelle dell'app e la trasmissione
  messaggi_saponette <- character(0)
  saponette <- withCallingHandlers(
    prepara_letture_saponette(file),
    message = function(m) {
      messaggi_saponette <<- c(messaggi_saponette, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_named(saponette, colonne_letture_saponette())
  # il registro: una riga ripetuta, una senza RFID, un codice oltre 10 cifre
  scartate_saponette <- letture_scartate(saponette)
  expect_setequal(
    scartate_saponette$motivo,
    c("doppione", "senza_rfid", "codice_non_compatibile")
  )
  expect_identical(
    scartate_saponette$matricola_saponetta[
      scartate_saponette$motivo == "codice_non_compatibile"
    ],
    "T5"
  )
  expect_identical(
    scartate_saponette$RFID[scartate_saponette$motivo == "codice_non_compatibile"],
    "12345678901"
  )
  expect_true(all(
    setdiff(names(saponette), "matricola_saponetta") %in% names(letture)
  ))
  # il codice 84C era il 123 con i bit invertiti: riparato; quello di 11 cifre
  # e' scartato; quello HERA resta com'e'
  expect_match(messaggi_saponette, "non compatibile.*: 1\\.", all = FALSE)
  expect_match(messaggi_saponette, "riparati.*1 trasposizione", all = FALSE)
  expect_identical(
    saponette$RFID,
    c(
      "0000000123",
      "00BD00000000000000000001",
      "0000000123",
      "0000000123",
      "48455241AABB"
    )
  )
  # il sacchetto resta, con la fornitura del suo gruppo e senza servizio
  expect_identical(
    saponette$presente_a_database,
    c("PIA", "PIA", "PIA", "PIA", "NP")
  )
  expect_identical(saponette$id_fornitura, c("F1", "S1", "F1", "F1", NA))
  expect_identical(
    saponette$servizio_transponder,
    c("SECCO", NA, "SECCO", "SECCO", NA)
  )
  # nucleo e potenziamento in vigore nel giorno della lettura
  expect_identical(saponette$nucleo_residenti, c(2L, NA, 3L, 3L, NA))
  expect_identical(saponette$potenziamento_secco, c(FALSE, FALSE, TRUE, TRUE, NA))
  percorso <- scrivi_csv_output(saponette, "letture_saponette.csv", output)
  expect_identical(basename(percorso), "letture_saponette.csv")
  scritte <- utils::read.csv(percorso, colClasses = "character")
  expect_identical(names(scritte), colonne_letture_saponette())
  expect_identical(scritte$matricola_saponetta, c("T1", "T1", "T3", "T4", "T5"))
})

test_that("i giri aggiunti a mano hanno le colonne del calendario pianificato", {
  expect_identical(
    colonne_giri_aggiunti(),
    c(
      "data_giro", "ora_inizio", "ora_fine", "matricola_mezzo", "id_giro",
      "eseguito", "descrizione_servizio", "comune_giro"
    )
  )
  percorso <- withr::local_tempfile(fileext = ".csv")
  writeLines(
    c(
      "DATA_GIRO;ora_inizio;ora_fine;Matricola_Mezzo;id_giro;eseguito;descrizione_servizio;comune_giro",
      "20/10/2026;06:00;12:00:00;0933;;si;secco pap;ROSA'",
      ";;;;;;;",
      "2026-10-21;13:00:00;18:00;1799;G7;SI;Carta/Cartone PAP;Bassano del Grappa"
    ),
    percorso
  )
  giri <- importa_giri_aggiunti(percorso)
  expect_named(giri, colonne_giri_aggiunti())
  expect_identical(giri$data_giro, as.Date(c("2026-10-20", "2026-10-21")))
  # gli orari restano com'\u00e8, come nel calendario; la matricola non cambia
  expect_identical(giri$ora_inizio, c("06:00", "13:00:00"))
  expect_identical(giri$ora_fine, c("12:00:00", "18:00"))
  expect_identical(giri$matricola_mezzo, c("0933", "1799"))
  # servizio, comune ed eseguito in maiuscolo come nel calendario
  expect_identical(giri$descrizione_servizio, c("SECCO PAP", "CARTA/CARTONE PAP"))
  expect_identical(giri$comune_giro, c("ROS\u00c0", "BASSANO DEL GRAPPA"))
  expect_identical(giri$eseguito, c("SI", "SI"))
  expect_identical(giri$id_giro, c(NA, "G7"))
})

test_that("senza il file o senza righe non ci sono giri aggiunti", {
  expect_null(importa_giri_aggiunti("non_esiste.csv"))
  expect_null(importa_giri_aggiunti(NULL))
  vuoto <- withr::local_tempfile(fileext = ".csv")
  writeLines(paste(colonne_giri_aggiunti(), collapse = ","), vuoto)
  expect_null(importa_giri_aggiunti(vuoto))
})

test_that("il file dei giri aggiunti dice che cosa non va", {
  scrivi <- function(righe) {
    percorso <- withr::local_tempfile(fileext = ".csv", .local_envir = parent.frame())
    writeLines(righe, percorso)
    percorso
  }
  testata <- paste(colonne_giri_aggiunti(), collapse = ",")
  expect_error(
    importa_giri_aggiunti(scrivi(c("data_giro,matricola_mezzo", "2026-10-20,1"))),
    "ora_inizio, ora_fine"
  )
  # id_giro ed eseguito possono restare vuoti, il resto no
  expect_error(
    importa_giri_aggiunti(scrivi(c(testata, "2026-10-20,06:00,12:00,1,,,SECCO PAP,"))),
    "riga del file\\): 2"
  )
  expect_error(
    importa_giri_aggiunti(scrivi(c(testata, "ottobre,06:00,12:00,1,,,SECCO PAP,ROS\u00c0"))),
    "AAAA-MM-GG o GG/MM/AAAA"
  )
  expect_error(
    importa_giri_aggiunti(scrivi(c(testata, "2026-10-20,12:00,06:00,1,,,SECCO PAP,ROS\u00c0"))),
    "inizio prima della fine"
  )
  expect_error(
    importa_giri_aggiunti(scrivi(c(testata, "2026-10-20,6 di mattina,12:00,1,,,SECCO PAP,ROS\u00c0"))),
    "Orari non validi"
  )
  expect_error(
    importa_giri_aggiunti(scrivi(c(testata, "2026-10-20,06:00,12:00,1,,,SECCO PAAP,ROS\u00c0"))),
    "SECCO PAAP"
  )
  # senza id_giro ed eseguito si legge
  ok <- importa_giri_aggiunti(scrivi(c(testata, "2026-10-20,06:00,12:00,1,,,SECCO PAP,ROS\u00c0")))
  expect_identical(nrow(ok), 1L)
})

test_that("i giri aggiunti a mano entrano nel calendario pianificato della preparazione", {
  cartella <- withr::local_tempdir()
  file <- scrivi_origine_prova(file.path(cartella, "origine"))
  # un giro del mezzo 1601 a Venezia, che il calendario estratto non ha
  writeLines(
    c(
      paste(colonne_giri_aggiunti(), collapse = ","),
      "2026-08-03,09:20,10:00,1601,G99,SI,verde pap,Venezia"
    ),
    file$giri_aggiunti
  )
  messaggi <- character(0)
  letture <- withCallingHandlers(
    suppressWarnings(prepara_letture_antenne(file)),
    message = function(m) {
      messaggi <<- c(messaggi, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_match(messaggi, "Giri aggiunti a mano: 1 nuovi, 0", all = FALSE)
  # nel suo orario e nel suo comune vince il pianificato aggiunto, con il suo codice
  venezia <- letture[letture$RFID == "0000000129", ]
  expect_identical(venezia$servizio_giro, "VERDE PAP")
  expect_identical(venezia$comune_giro, "VENEZIA")
  expect_identical(venezia$id_giro, "G99")
  expect_identical(venezia$eseguito, "SI")
  # le altre letture restano al loro giro
  expect_true(all(letture$servizio_giro[letture$RFID == "0000000123"] == "SECCO PAP"))
})
