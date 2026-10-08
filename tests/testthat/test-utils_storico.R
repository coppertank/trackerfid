# Letture storiche e confronto tra il sistema precedente e le antenne.

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
    comune = dplyr::case_when(
      .data$RFID %in% c("A", "D") ~ "X",
      .data$RFID == "B" ~ "Y"
    )
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
  expect_identical(esempio$storico, c(16L, 0L, 16L, 0L, 6L, 0L))
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

test_that("l'anagrafica ha una riga per RFID, con i dati dei soli censiti", {
  expect_identical(anagrafica_prova$RFID, c("A", "B", "C", "D"))
  expect_identical(anagrafica_prova$comune, c("X", "Y", NA, "X"))
  expect_equal(
    anagrafica_prova$numero_raccolte_annue_previste,
    c(10, 20, NA, 10)
  )
  expect_identical(
    anagrafica_prova$presente_a_database,
    c("Presente", "Presente", "Non Presente", "Presente")
  )

  # un RFID tolto dal database non conserva comune e raccolte
  tolto <- dplyr::bind_rows(
    dplyr::mutate(letture_rfid("Z", "2025-03-01", raccolte = 26), comune = "X"),
    letture_rfid("Z", "2025-06-01", presente = "Non Presente")
  )
  expect_true(is.na(anagrafica_rfid(tolto)$comune))
  expect_true(is.na(anagrafica_rfid(tolto)$numero_raccolte_annue_previste))

  # le colonne facoltative possono mancare
  senza <- anagrafica_rfid(letture_test())
  expect_true(all(is.na(senza$comune)))
  expect_true(all(is.na(senza$numero_raccolte_annue_previste)))
})

test_that("la tabella per anno ha una riga per RFID letto dalle antenne", {
  tabella <- tabella_letture_annuali(unite_prova, anagrafica_prova)
  expect_identical(
    names(tabella),
    c(
      "RFID",
      paste0("letture_", 2021:2025),
      "comune",
      "numero_raccolte_annue_previste"
    )
  )
  expect_identical(tabella$RFID, c("A", "B", "C", "D"))
  anni <- as.matrix(tabella[, paste0("letture_", 2021:2025)])
  dimnames(anni) <- NULL
  expect_equal(
    anni,
    rbind(
      c(2, 4, 0, 6, 12),
      c(NA, 5, 10, 10, 15),
      c(1, 0, 0, 1, 3),
      c(NA, NA, NA, NA, 8)
    )
  )
  expect_identical(tabella$comune, c("X", "Y", NA, "X"))
})

test_that("il confronto usa gli stessi RFID e gli anni in cui erano di sicuro in servizio", {
  confronto <- confronto_sistemi(unite_prova, anagrafica_prova)
  # D è nuovo, E non è stato letto dalle antenne: restano fuori
  expect_identical(confronto$RFID, c("A", "B", "C"))
  expect_equal(attr(confronto, "giorni_antenne"), 365)
  expect_identical(attr(confronto, "anni_storico"), 2021:2024)

  # l'anno della prima lettura non conta: A dal 2022, B dal 2023, C dal 2022
  expect_identical(confronto$anni_servizio, c(3L, 2L, 3L))
  expect_identical(confronto$anni_vuoti, c(1L, 0L, 2L))
  expect_equal(confronto$letture_annue_storico, c(10 / 3, 10, 1 / 3))
  expect_equal(confronto$letture_annue_antenne, c(12, 15, 3))
  # le letture utili non superano le raccolte previste: 12 letture, 10 previste
  expect_equal(confronto$letture_utili_antenne, c(10, 15, NA))
  expect_equal(confronto$tasso_storico, c(1 / 3, 0.5, NA))
  expect_equal(confronto$tasso_antenne, c(1, 0.75, NA))

  # le letture con le antenne sono riportate a un anno
  mezzo_anno <- letture_prova[
    letture_prova$giorno_lettura < as.POSIXct("2025-07-02", tz = "UTC"),
  ]
  mezzo_anno <- dplyr::bind_rows(mezzo_anno, letture_rfid("D", "2025-07-01"))
  parziale <- confronto_sistemi(
    unisci_letture(mezzo_anno, storico_prova),
    anagrafica_rfid(mezzo_anno)
  )
  expect_equal(attr(parziale, "giorni_antenne"), 182)
  n_a <- sum(mezzo_anno$RFID == "A")
  expect_equal(
    parziale$letture_annue_antenne[parziale$RFID == "A"],
    n_a * 365 / 182
  )

  # senza letture storiche non c'è niente da confrontare
  expect_equal(
    nrow(confronto_sistemi(unisci_letture(letture_prova), anagrafica_prova)),
    0
  )
})

test_that("il riepilogo misura anni vuoti, rapporto e letture recuperate", {
  confronto <- confronto_sistemi(unite_prova, anagrafica_prova)
  r <- riepilogo_confronto(confronto)
  expect_equal(nrow(r), 1)
  expect_equal(r$n_rfid, 3)
  expect_equal(r$rfid_con_anni_vuoti, 2)
  expect_equal(r$totale_anni_servizio, 8)
  expect_equal(r$totale_anni_vuoti, 3)
  expect_equal(r$quota_anni_vuoti, 3 / 8)
  expect_equal(r$media_storico, (10 / 3 + 10 + 1 / 3) / 3)
  expect_equal(r$media_antenne, 10)
  expect_equal(r$rapporto, 10 / ((10 / 3 + 10 + 1 / 3) / 3))
  # i tassi riguardano solo A e B, che hanno le raccolte previste: 30 in tutto
  expect_equal(r$n_con_raccolte, 2)
  expect_equal(r$raccolte_previste, 30)
  expect_equal(r$tasso_storico, (10 / 3 + 10) / 30)
  expect_equal(r$tasso_antenne, 25 / 30)
  expect_equal(r$letture_recuperate, 25 - (10 / 3 + 10))
  # contenitori da 240 litri
  expect_equal(r$litri_recuperati, 240 * (25 - (10 / 3 + 10)))

  per_comune <- riepilogo_confronto(confronto, "comune")
  expect_identical(per_comune$comune, c("X", "Y", NA))
  expect_equal(per_comune$tasso_antenne, c(1, 0.75, NaN))
  expect_equal(per_comune$quota_anni_vuoti, c(1 / 3, 0, 2 / 3))
})

test_that("le medie per anno contano un contenitore dall'anno dopo la prima lettura", {
  medie <- media_annuale(unite_prova, anagrafica_prova)
  expect_identical(medie$anno, 2022:2025)
  expect_identical(medie$sistema, c("storico", "storico", "storico", "antenne"))
  # 2022 solo A; 2023 e 2024 A e B; nel 2025 D è al primo anno e non conta
  expect_identical(medie$n_rfid, c(1L, 2L, 2L, 2L))
  expect_equal(medie$letture_medie, c(4, 5, 8, 13.5))
  expect_equal(medie$letture_utili_medie, c(4, 5, 8, 12.5))
  expect_equal(medie$raccolte_medie, c(10, 15, 15, 15))
  expect_equal(medie$tasso, c(0.4, 1 / 3, 8 / 15, 12.5 / 15))

  per_comune <- media_annuale(unite_prova, anagrafica_prova, "comune")
  expect_identical(per_comune$comune, c(rep("X", 4), rep("Y", 3)))
  expect_identical(per_comune$anno, c(2022:2025, 2023:2025))
  expect_equal(per_comune$letture_utili_medie, c(4, 0, 6, 10, 10, 10, 15))

  # un anno coperto a metà viene riportato a dodici mesi
  mezzo_anno <- letture_prova[
    letture_prova$giorno_lettura < as.POSIXct("2025-07-02", tz = "UTC"),
  ]
  mezzo_anno <- dplyr::bind_rows(mezzo_anno, letture_rfid("D", "2025-07-01"))
  parziali <- media_annuale(
    unisci_letture(mezzo_anno, storico_prova),
    anagrafica_rfid(mezzo_anno)
  )
  ultimo <- parziali[parziali$anno == 2025, ]
  expect_false(ultimo$completo)
  n_ab <- sum(mezzo_anno$RFID %in% c("A", "B"))
  expect_equal(ultimo$letture_medie, n_ab / 2 * 365 / 182)
})

test_that("media con intervallo di confidenza", {
  x <- c(2, 4, 6, 8)
  esito <- intervallo_media(c(x, NA))
  atteso <- stats::t.test(x)$conf.int
  expect_equal(esito[["media"]], 5)
  expect_equal(unname(esito[c("inferiore", "superiore")]), as.numeric(atteso))
  expect_true(all(is.na(intervallo_media(numeric(0)))))
  expect_equal(intervallo_media(3)[["media"]], 3)
  expect_true(is.na(intervallo_media(3)[["inferiore"]]))
})

test_that("gli RFID letti solo dal sistema precedente sono elencati a parte", {
  soli <- rfid_solo_storico(unite_prova)
  expect_identical(soli$RFID, "E")
  expect_equal(soli$letture, 3)
  expect_identical(soli$ultimo_anno, 2022L)
  expect_equal(nrow(rfid_solo_storico(unisci_letture(letture_prova))), 0)
})

test_that("sul dataset di esempio le antenne leggono molto di più del sistema precedente", {
  unite <- unisci_letture(dati_esempio(), storico_esempio())
  anagrafica <- anagrafica_rfid(dati_esempio())
  confronto <- confronto_sistemi(unite, anagrafica)
  r <- riepilogo_confronto(confronto)

  expect_identical(attr(confronto, "anni_storico"), 2020:2023)
  expect_equal(attr(confronto, "giorni_antenne"), 457)
  expect_equal(r$n_rfid, 162)
  # quasi un anno di servizio su cinque senza nemmeno una lettura
  expect_equal(r$totale_anni_servizio, 416)
  expect_equal(r$totale_anni_vuoti, 75)
  expect_equal(r$rfid_con_anni_vuoti, 61)
  # da un terzo a oltre l'80% delle raccolte previste
  expect_equal(r$tasso_storico, 0.33, tolerance = 0.03)
  expect_equal(r$tasso_antenne, 0.82, tolerance = 0.03)
  expect_gt(r$rapporto, 2)
  # un solo contenitore, esposto di rado, è letto meno di prima
  expect_equal(
    sum(confronto$letture_annue_antenne <= confronto$letture_annue_storico),
    1
  )

  # il sistema precedente leggeva in modo diverso da comune a comune, le antenne no
  per_comune <- riepilogo_confronto(
    confronto[!is.na(confronto$comune), ],
    "comune"
  )
  expect_identical(
    per_comune$comune[order(per_comune$tasso_storico)],
    c("COMUNE OVEST", "COMUNE SUD", "COMUNE EST", "COMUNE NORD")
  )
  expect_true(all(
    per_comune$tasso_antenne > 0.78 & per_comune$tasso_antenne < 0.86
  ))
  expect_true(all(per_comune$tasso_antenne - per_comune$tasso_storico > 0.3))
})
