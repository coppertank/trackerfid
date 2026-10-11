# Letture storiche: lettura del file, conteggi per anno, anagrafica degli RFID.

# Letture storiche: una per data, alle 8 del mattino.
storiche <- function(rfid, date) {
  dplyr::tibble(
    RFID = rfid,
    giorno_lettura = as.POSIXct(paste(date, "08:00:00"), tz = "UTC")
  )
}

# n date distinte dentro un anno. Con `estremi` comprendono il 1° gennaio e il
# 31 dicembre, così l'anno risulta coperto per intero.
date_anno <- function(anno, n, estremi = FALSE) {
  if (estremi) {
    return(format(seq(
      as.Date(paste0(anno, "-01-01")),
      as.Date(paste0(anno, "-12-31")),
      length.out = n
    )))
  }
  format(as.Date(paste0(anno, "-03-01")) + 7 * (seq_len(n) - 1))
}

# Caso di prova, con il sistema precedente dal 2021 al 2024 e le antenne nel 2025.
#   A censito, 10 raccolte previste: 2, 4, 0, 6 letture storiche, 12 con le antenne
#   B censito, 20 raccolte previste: prima lettura nel 2022, poi 5, 10, 10 e 15
#   C non censito: 1 lettura nel 2021, 1 nel 2024, 3 con le antenne
#   D censito, letto solo dalle antenne: contenitore nuovo
#   E letto solo dal sistema precedente
storico_prova <- dplyr::bind_rows(
  storiche(
    "A",
    c(
      date_anno(2021, 2, estremi = TRUE),
      date_anno(2022, 4),
      date_anno(2024, 6, estremi = TRUE)
    )
  ),
  storiche(
    "B",
    c(date_anno(2022, 5), date_anno(2023, 10), date_anno(2024, 10))
  ),
  storiche("C", c("2021-05-10", "2024-05-10")),
  storiche("E", c("2021-02-01", "2021-06-01", "2022-04-01"))
)
letture_prova <- dplyr::bind_rows(
  letture_rfid("A", date_anno(2025, 12, estremi = TRUE), raccolte = 10),
  letture_rfid("B", date_anno(2025, 15), raccolte = 20, utenza = "U2"),
  letture_rfid(
    "C",
    date_anno(2025, 3),
    presente = "Non Presente",
    atteso = "SECCO PAP"
  ),
  letture_rfid("D", date_anno(2025, 8), raccolte = 10, utenza = "U3")
) |>
  dplyr::mutate(
    # A e D sono a Rubano, B a Cittadella, nel cantiere di Camposampiero. C non
    # è censito: il suo comune è quello del giro che lo legge, Limena.
    comune_da_database = dplyr::case_when(
      .data$RFID %in% c("A", "D") ~ "RUBANO",
      .data$RFID == "B" ~ "CITTADELLA"
    ),
    comune_lettura = dplyr::if_else(.data$RFID == "C", "LIMENA", NA_character_)
  )
unite_prova <- unisci_letture(letture_prova, storico_prova)
anagrafica_prova <- anagrafica_rfid(letture_prova)

test_that("il file delle letture storiche viene letto e validato", {
  esito <- carica_storico(scrivi_csv(c(
    "altro;Giorno_Lettura;rfid",
    "x;2023-05-01 08:30:00;R1",
    "x;02/05/2023 09:15;R2",
    "x;2023-05-03;0007"
  )))
  expect_identical(names(esito$dati), colonne_storico())
  expect_identical(esito$dati$RFID, c("R1", "R2", "0007"))
  expect_s3_class(esito$dati$giorno_lettura, "POSIXct")
  expect_identical(
    format(esito$dati$giorno_lettura, "%Y-%m-%d %H:%M"),
    c("2023-05-01 08:30", "2023-05-02 09:15", "2023-05-03 00:00")
  )
  expect_length(esito$avvisi, 0)

  con_scarti <- carica_storico(scrivi_csv(c(
    "RFID,giorno_lettura",
    "R1,2023-05-01 08:30:00",
    "R1,2023-05-01 08:30:00",
    ",2023-05-02 08:30:00",
    "R2,"
  )))
  expect_equal(nrow(con_scarti$dati), 1)
  expect_length(con_scarti$avvisi, 2)
  expect_match(con_scarti$avvisi[1], "2 righe scartate")
  expect_match(con_scarti$avvisi[2], "1 letture ripetute")
})

test_that("un file storico non valido viene rifiutato con un messaggio esplicito", {
  errore <- function(righe) {
    conditionMessage(expect_error(
      carica_storico(scrivi_csv(righe)),
      class = "errore_validazione"
    ))
  }
  expect_match(
    errore(c("codice,giorno_lettura", "R1,2023-05-01")),
    "Colonne mancanti.*RFID"
  )
  expect_match(
    errore(c("RFID,giorno_lettura", "R1,ieri", "R2,2023-05-01")),
    "giorno_lettura: 1 valori"
  )
  expect_match(
    errore(c("RFID,giorno_lettura", ",", "R1,")),
    "Nessuna riga utilizzabile"
  )
  expect_match(errore("RFID,giorno_lettura"), "non contiene righe")
})

test_that("le letture dei due sistemi si uniscono e si contano per anno", {
  expect_identical(names(unite_prova), c("RFID", "giorno_lettura", "fonte"))
  expect_equal(
    as.integer(table(unite_prova$fonte)[c("storico", "antenne")]),
    c(42L, 38L)
  )
  # senza letture storiche restano quelle con le antenne
  expect_identical(unique(unisci_letture(letture_prova)$fonte), "antenne")
  expect_equal(
    nrow(unisci_letture(letture_prova, storico_prova[0, ])),
    nrow(letture_prova)
  )

  conteggi <- conta_letture_annuali(unite_prova)
  di_a <- conteggi[conteggi$RFID == "A", ]
  expect_identical(di_a$anno, c(2021L, 2022L, 2024L, 2025L))
  expect_identical(di_a$fonte, c("storico", "storico", "storico", "antenne"))
  expect_identical(di_a$letture, c(2L, 4L, 6L, 12L))
})

test_that("la griglia riporta gli zeri solo dalla prima lettura dell'RFID in poi", {
  griglia <- griglia_annuale(unite_prova)
  di <- function(rfid) griglia[griglia$RFID == rfid, ]

  expect_identical(di("A")$anno, 2021:2025)
  expect_identical(di("A")$storico, c(2L, 4L, 0L, 6L, 0L))
  expect_identical(di("A")$antenne, c(0L, 0L, 0L, 0L, 12L))
  expect_identical(di("A")$letture, c(2L, 4L, 0L, 6L, 12L))
  # B compare dal 2022, D solo nel 2025: prima potevano non essere in servizio
  expect_identical(di("B")$anno, 2022:2025)
  expect_identical(di("D")$anno, 2025L)
  expect_true(all(di("B")$primo_anno == 2022L))
  # E non è più stato letto, ma gli anni arrivano alla fine dei dati
  expect_identical(di("E")$letture, c(2L, 1L, 0L, 0L, 0L))

  expect_setequal(griglia_annuale(unite_prova, c("A", "D"))$RFID, c("A", "D"))
  expect_equal(nrow(griglia_annuale(unite_prova, "ZZZ")), 0)
  # senza letture storiche la colonna resta, a zero
  expect_true(all(griglia_annuale(unisci_letture(letture_prova))$storico == 0L))
})

test_that("l'andamento di un RFID copre tutti gli anni fino alla fine dei dati", {
  expect_equal(
    andamento_rfid("A", letture_prova, storico_prova),
    dplyr::tibble(
      anno = 2021:2025,
      storico = c(2L, 4L, 0L, 6L, 0L),
      antenne = c(0L, 0L, 0L, 0L, 12L)
    )
  )
  # senza letture storiche resta il solo anno delle antenne
  expect_equal(
    andamento_rfid("A", letture_prova),
    dplyr::tibble(anno = 2025L, storico = 0L, antenne = 12L)
  )
  expect_equal(nrow(andamento_rfid("ZZZ", letture_prova, storico_prova)), 0)
  expect_equal(nrow(andamento_rfid(NULL, letture_prova, storico_prova)), 0)
  expect_equal(nrow(andamento_rfid("A", NULL, storico_prova)), 0)

  # sul dataset di esempio: due anni interi senza letture, poi le antenne
  esempio <- andamento_rfid("RFD20241004003", dati_esempio(), storico_esempio())
  expect_identical(esempio$anno, 2020:2025)
  expect_identical(esempio$storico, c(13L, 0L, 16L, 0L, 6L, 0L))
  expect_identical(esempio$antenne, c(0L, 0L, 0L, 0L, 6L, 25L))
})

test_that("la copertura distingue gli anni dei due sistemi e quelli incompleti", {
  copertura <- copertura_annuale(unite_prova)
  expect_identical(copertura$anno, 2021:2025)
  expect_identical(copertura$sistema, c(rep("storico", 4), "antenne"))
  expect_true(all(copertura$completo))
  expect_true(all(copertura$fattore == 1))
  expect_identical(copertura$giorni_antenne, c(0L, 0L, 0L, 0L, 365L))

  # sistema precedente da luglio 2023 a marzo 2024, antenne da aprile a settembre 2024
  parziale <- copertura_annuale(unisci_letture(
    letture_rfid("A", c("2024-04-01", "2024-09-30")),
    storiche("A", c("2023-07-01", "2024-03-31"))
  ))
  expect_identical(parziale$anno, 2023:2024)
  expect_identical(parziale$sistema, c("storico", "misto"))
  expect_identical(parziale$giorni_storico, c(184L, 91L))
  expect_identical(parziale$giorni_antenne, c(0L, 183L))
  expect_identical(parziale$giorni_coperti, c(184L, 274L))
  expect_identical(parziale$completo, c(FALSE, FALSE))
  expect_equal(parziale$fattore, c(365 / 184, 366 / 274))

  # qualche giorno scoperto a inizio o fine anno non rende l'anno incompleto
  quasi <- copertura_annuale(unisci_letture(
    letture_rfid("A", c("2024-01-03", "2024-12-28"))
  ))
  expect_true(quasi$completo)
  expect_equal(quasi$fattore, 1)
})

test_that("l'anagrafica ha una riga per RFID, con geografia e dati dei censiti", {
  expect_identical(
    names(anagrafica_prova),
    c(
      "RFID",
      "presente_a_database",
      "servizio_transponder",
      "tipologia",
      "cantiere",
      "comune",
      "numero_raccolte_annue_previste",
      "volume_previsto",
      "letture"
    )
  )
  expect_identical(anagrafica_prova$RFID, c("A", "B", "C", "D"))
  # la tipologia viene dal database per i censiti, dai giri per il non censito
  expect_identical(
    anagrafica_prova$tipologia,
    c("Carta", "Carta", "Secco", "Carta")
  )
  # il non censito prende il comune in cui viene letto
  expect_identical(
    anagrafica_prova$comune,
    c("RUBANO", "CITTADELLA", "LIMENA", "RUBANO")
  )
  expect_identical(
    anagrafica_prova$cantiere,
    c("RUBANO", "CAMPOSAMPIERO", "RUBANO", "RUBANO")
  )
  expect_equal(
    anagrafica_prova$numero_raccolte_annue_previste,
    c(10, 20, NA, 10)
  )
  expect_identical(
    anagrafica_prova$presente_a_database,
    c("Presente", "Presente", "Non Presente", "Presente")
  )
  expect_identical(anagrafica_prova$letture, c(12L, 15L, 3L, 8L))

  # un RFID tolto dal database perde le raccolte, non l'ultimo comune noto
  tolto <- dplyr::bind_rows(
    dplyr::mutate(
      letture_rfid("Z", "2025-03-01", raccolte = 26),
      comune_da_database = "RUBANO"
    ),
    letture_rfid("Z", "2025-06-01", presente = "Non Presente")
  )
  expect_identical(anagrafica_rfid(tolto)$comune, "RUBANO")
  expect_true(is.na(anagrafica_rfid(tolto)$numero_raccolte_annue_previste))

  # un non censito letto in più comuni prende quello in cui è letto più spesso
  girovago <- dplyr::mutate(
    letture_rfid("G", date_2025(5), presente = "Non Presente"),
    comune_lettura = c("NOVE", "MAROSTICA", "NOVE", NA, "MAROSTICA")
  )
  expect_identical(anagrafica_rfid(girovago)$comune, "MAROSTICA")
  expect_identical(anagrafica_rfid(girovago)$cantiere, "BASSANO")

  # un non censito letto da giri di rifiuti diversi non ha una tipologia
  incerto <- dplyr::bind_rows(
    letture_rfid("I", "2025-03-01", presente = "Non Presente", atteso = "SECCO PAP"),
    letture_rfid("I", "2025-03-08", presente = "Non Presente", atteso = "VETRO PAP"),
    letture_rfid("S", date_2025(5), presente = "Non Presente", atteso = c(rep("SECCO PAP", 4), "VETRO PAP"))
  )
  anagrafica_incerta <- anagrafica_rfid(incerto)
  expect_identical(
    anagrafica_incerta$tipologia[match(c("I", "S"), anagrafica_incerta$RFID)],
    c(NA, "Secco")
  )
  # il servizio atteso di un censito non conta: vale quello del database
  censito <- letture_rfid("K", date_2025(3), servizio = "UMIDO", atteso = "SECCO PAP")
  expect_identical(anagrafica_rfid(censito)$tipologia, "Umido")

  # le colonne facoltative possono mancare
  senza <- anagrafica_rfid(letture_test())
  expect_true(all(is.na(senza$comune)))
  expect_true(all(is.na(senza$cantiere)))
  expect_true(all(is.na(senza$numero_raccolte_annue_previste)))
  expect_equal(nrow(anagrafica_rfid(letture_test()[0, ])), 0)
})

test_that("gli RFID letti solo dal sistema precedente sono elencati a parte", {
  soli <- rfid_solo_storico(unite_prova)
  expect_identical(soli$RFID, "E")
  expect_equal(soli$letture, 3)
  expect_identical(soli$ultimo_anno, 2022L)
  expect_equal(nrow(rfid_solo_storico(unisci_letture(letture_prova))), 0)
})

test_that("le letture storiche si leggono anche da un CSV compresso", {
  compresso <- withr::local_tempfile(fileext = ".csv.gz")
  uscita <- gzfile(compresso, "wb")
  writeBin(
    readBin(
      percorso_storico_esempio(),
      "raw",
      file.size(percorso_storico_esempio())
    ),
    uscita
  )
  close(uscita)
  expect_identical(carica_storico(compresso)$dati, storico_esempio())
  # con il nome originale, come per un file caricato dal browser
  anonimo <- withr::local_tempfile(fileext = ".dat")
  file.copy(compresso, anonimo)
  expect_identical(
    carica_storico(anonimo, "storiche.csv.gz")$dati,
    storico_esempio()
  )
})
