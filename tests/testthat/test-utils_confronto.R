# Confronto tra il sistema precedente e le antenne sui contenitori letti dalle
# antenne, per il report sul beneficio delle antenne.

# Letture storiche: una per data, alle 8 del mattino.
storiche_il <- function(rfid, date) {
  dplyr::tibble(
    RFID = rfid,
    giorno_lettura = as.POSIXct(paste(date, "08:00:00"), tz = "UTC")
  )
}
ogni_14_giorni <- function(dal, n) format(as.Date(dal) + 14 * (seq_len(n) - 1))

# Caso di prova. Il sistema precedente parte dal 2024, le antenne dal primo
# luglio 2025: il periodo di riferimento va dal primo luglio 2024 al 30 giugno
# 2025. L'ultima lettura con le antenne è del 30 dicembre 2025.
#   A censito, secco, 26 raccolte: 2 letture storiche prima del periodo di
#     riferimento, 4 dentro, 1 dopo l'arrivo delle antenne. Con le antenne
#     13 letture, la prima il primo luglio
#   B censito, secco, 26 raccolte: mai letto dal sistema precedente. Con le
#     antenne 4 letture, la prima il primo ottobre
#   C non censito, letto dai giri del secco: 1 lettura storica nel periodo di
#     riferimento, 2 con le antenne
#   D censito, carta, 13 raccolte: 10 letture storiche nel periodo di
#     riferimento, una sola con le antenne, l'ultimo giorno
#   E letto solo dal sistema precedente
storico_coorte <- dplyr::bind_rows(
  storiche_il(
    "A",
    c(
      "2024-02-01",
      "2024-03-01",
      "2024-09-01",
      "2024-12-01",
      "2025-03-01",
      "2025-06-30",
      "2025-08-10"
    )
  ),
  storiche_il("C", "2025-01-15"),
  storiche_il("D", ogni_14_giorni("2024-07-01", 10)),
  storiche_il("E", c("2024-05-01", "2025-05-01"))
)
letture_coorte <- dplyr::bind_rows(
  letture_rfid("A", ogni_14_giorni("2025-07-01", 13), servizio = "SECCO"),
  letture_rfid("B", ogni_14_giorni("2025-10-01", 4), servizio = "SECCO"),
  letture_rfid(
    "C",
    c("2025-08-01", "2025-08-15"),
    presente = "Non Presente",
    atteso = "SECCO PAP"
  ),
  letture_rfid("D", "2025-12-30", servizio = "CARTA", raccolte = 13)
)
unite_coorte <- unisci_letture(letture_coorte, storico_coorte)
# L'anagrafica segue l'ordine della prima lettura con le antenne: qui la si
# riporta in ordine alfabetico, per leggere meglio i valori attesi.
anagrafica_coorte <- dplyr::arrange(anagrafica_rfid(letture_coorte), .data$RFID)
confronto_coorte <- confronto_sistemi(unite_coorte, anagrafica_coorte)

test_that("conta una lettura al giorno per contenitore e per sistema", {
  doppie <- dplyr::bind_rows(
    unite_coorte,
    # lo stesso contenitore riletto nello stesso giorno, dai due sistemi
    dplyr::tibble(
      RFID = c("A", "A", "A"),
      giorno_lettura = as.POSIXct(
        c("2025-07-01 07:00:00", "2025-07-01 15:00:00", "2024-09-01 19:00:00"),
        tz = "UTC"
      ),
      fonte = c("antenne", "antenne", "storico")
    )
  )
  uniche <- una_lettura_al_giorno(doppie)
  expect_equal(nrow(uniche), nrow(unite_coorte))
  expect_identical(names(uniche), names(unite_coorte))
  expect_false(is.unsorted(uniche$giorno_lettura))
  # resta la prima lettura del giorno
  primo_luglio <- uniche[
    uniche$RFID == "A" &
      lubridate::as_date(uniche$giorno_lettura) == as.Date("2025-07-01"),
  ]
  expect_equal(
    primo_luglio$giorno_lettura,
    as.POSIXct("2025-07-01 07:00:00", tz = "UTC")
  )
  # lo stesso giorno letto dai due sistemi resta in entrambi
  stesso_giorno <- dplyr::bind_rows(
    dplyr::mutate(storiche_il("A", "2025-07-01"), fonte = "storico"),
    dplyr::mutate(storiche_il("A", "2025-07-01"), fonte = "antenne"),
    dplyr::mutate(storiche_il("B", "2025-07-01"), fonte = "antenne")
  )
  expect_equal(nrow(una_lettura_al_giorno(stesso_giorno)), 3)
  expect_equal(nrow(una_lettura_al_giorno(unite_coorte[0, ])), 0)
})

test_that("il periodo di riferimento sono i dodici mesi prima delle antenne", {
  periodi <- periodi_confronto(unite_coorte)
  expect_identical(periodi$storico, as.Date(c("2024-02-01", "2025-08-10")))
  expect_identical(periodi$antenne, as.Date(c("2025-07-01", "2025-12-30")))
  expect_identical(periodi$riferimento, as.Date(c("2024-07-01", "2025-06-30")))
  expect_identical(periodi$giorni_riferimento, 365L)
  expect_identical(periodi$giorni_antenne, 183L)
  # dal primo febbraio al 30 giugno 2024
  expect_identical(periodi$giorni_prima, 151L)
  expect_true(periodi$sovrapposti)

  # letture storiche più corte di dodici mesi: il periodo parte dalla prima
  corto <- unisci_letture(
    letture_coorte,
    storico_coorte[storico_coorte$giorno_lettura >= as.POSIXct("2025-03-01", tz = "UTC"), ]
  )
  periodi_corti <- periodi_confronto(corto)
  expect_identical(
    periodi_corti$riferimento,
    as.Date(c("2025-03-01", "2025-06-30"))
  )
  expect_identical(periodi_corti$giorni_riferimento, 122L)
  expect_identical(periodi_corti$giorni_prima, 0L)
  # un periodo di riferimento più breve, a scelta
  expect_identical(periodi_confronto(unite_coorte, giorni = 30)$giorni_riferimento, 30L)

  # sistema precedente fermo prima delle antenne: nessuna sovrapposizione
  fermo <- unite_coorte[
    unite_coorte$fonte == "antenne" |
      unite_coorte$giorno_lettura < as.POSIXct("2025-07-01", tz = "UTC"),
  ]
  expect_false(periodi_confronto(fermo)$sovrapposti)

  # senza letture storiche non c'è un periodo di riferimento
  sole <- periodi_confronto(unisci_letture(letture_coorte))
  expect_identical(sole$giorni_riferimento, 0L)
  expect_identical(sole$giorni_prima, 0L)
  expect_false(sole$sovrapposti)
  # letture storiche che iniziano dopo le antenne
  dopo <- unisci_letture(letture_coorte, storiche_il("A", "2025-09-01"))
  expect_identical(periodi_confronto(dopo)$giorni_riferimento, 0L)
  expect_identical(periodi_confronto(dopo)$giorni_prima, 0L)
  # senza antenne il confronto non si può fare
  expect_error(
    periodi_confronto(unite_coorte[unite_coorte$fonte == "storico", ]),
    "antenne"
  )
})

test_that("le raccolte previste di un non censito vengono dai censiti dello stesso servizio", {
  riferimento <- raccolte_di_riferimento(anagrafica_coorte)
  expect_equal(riferimento$raccolte_previste, c(26, 26, 26, 13))
  expect_identical(riferimento$raccolte_stimate, c(FALSE, FALSE, TRUE, FALSE))

  # prima i censiti dello stesso comune, poi quelli dello stesso servizio
  anagrafica <- dplyr::tibble(
    RFID = c("a", "b", "c", "d", "e", "f", "g", "h"),
    presente_a_database = c(rep("Presente", 4), rep("Non Presente", 4)),
    tipologia = c("Secco", "Secco", "Secco", "Vetro", "Secco", "Secco", NA, "Umido"),
    comune = c("X", "X", "Y", "X", "Y", "Z", "X", "X"),
    numero_raccolte_annue_previste = c(26, 26, 52, 13, NA, NA, NA, NA)
  )
  esito <- raccolte_di_riferimento(anagrafica)
  # e: secco a Y, come c. f: secco in un comune senza censiti, il valore più
  # frequente del servizio. g: stima incerta. h: nessun censito dell'umido
  expect_equal(esito$raccolte_previste, c(26, 26, 52, 13, 52, 26, NA, NA))
  expect_identical(
    esito$raccolte_stimate,
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )
  # un non censito senza comune usa il servizio
  anagrafica$comune[5] <- NA
  expect_equal(raccolte_di_riferimento(anagrafica)$raccolte_previste[5], 26)
  # senza censiti non c'è niente da cui stimare
  soli <- anagrafica[5:8, ]
  expect_true(all(is.na(raccolte_di_riferimento(soli)$raccolte_previste)))
})

test_that("il confronto misura ogni sistema sui giorni in cui è stato osservato", {
  expect_identical(confronto_coorte$RFID, c("A", "B", "C", "D"))
  expect_identical(
    confronto_coorte$tipologia,
    c("Secco", "Secco", "Secco", "Carta")
  )
  # sistema precedente: le letture del periodo di riferimento
  expect_identical(confronto_coorte$letture_riferimento, c(4L, 0L, 1L, 10L))
  expect_equal(confronto_coorte$letture_annue_storico, c(4, 0, 1, 10))
  expect_identical(confronto_coorte$letture_prima, c(2L, 0L, 0L, 0L))
  expect_identical(confronto_coorte$letture_sovrapposte, c(1L, 0L, 0L, 0L))
  expect_identical(
    confronto_coorte$prima_lettura_storico,
    as.Date(c("2024-02-01", NA, "2025-01-15", "2024-07-01"))
  )
  # antenne: le letture dopo la prima, e i giorni passati da allora
  expect_identical(confronto_coorte$letture_antenne, c(13L, 4L, 2L, 1L))
  expect_identical(
    confronto_coorte$prima_lettura_antenne,
    as.Date(c("2025-07-01", "2025-10-01", "2025-08-01", "2025-12-30"))
  )
  expect_identical(confronto_coorte$giorni_dopo, c(182L, 90L, 151L, 0L))
  expect_identical(confronto_coorte$letture_dopo, c(12L, 3L, 1L, 0L))
  expect_equal(
    confronto_coorte$letture_annue_antenne,
    c(365 * 12 / 182, 365 * 3 / 90, 365 / 151, NA)
  )
  # raccolte previste nei giorni osservati e letture utili
  expect_equal(confronto_coorte$raccolte_previste, c(26, 26, 26, 13))
  expect_identical(
    confronto_coorte$raccolte_stimate,
    c(FALSE, FALSE, TRUE, FALSE)
  )
  expect_equal(confronto_coorte$previste_storico, c(26, 26, 26, 13))
  expect_equal(confronto_coorte$utili_storico, c(4, 0, 1, 10))
  expect_equal(
    confronto_coorte$previste_antenne,
    26 * c(182, 90, 151, 0) / 365
  )
  expect_equal(confronto_coorte$utili_antenne, c(12, 3, 1, 0))
  expect_identical(
    attr(confronto_coorte, "periodi"),
    periodi_confronto(unite_coorte)
  )

  # le letture utili non superano le raccolte previste nei giorni osservati
  fitto <- dplyr::bind_rows(
    letture_rfid("T", format(as.Date("2025-07-01") + 0:60), servizio = "SECCO"),
    letture_rfid("U", c("2025-07-01", "2025-12-30"), servizio = "SECCO")
  )
  unite_fitte <- unisci_letture(
    fitto,
    storiche_il("T", format(as.Date("2025-01-01") + 0:99))
  )
  esito <- confronto_sistemi(unite_fitte, anagrafica_rfid(fitto))
  # dal primo gennaio al 30 giugno 2025: 181 giorni di riferimento
  expect_equal(esito$letture_riferimento, c(100, 0))
  expect_equal(esito$letture_annue_storico[1], 100 * 365 / 181)
  expect_equal(esito$previste_storico[1], 26 * 181 / 365)
  expect_equal(esito$utili_storico[1], 26 * 181 / 365)
  expect_equal(esito$letture_dopo, c(60, 1))
  expect_equal(esito$utili_antenne, c(26 * 182 / 365, 1))

  # due letture nello stesso giorno contano due volte solo se non si passa da
  # una lettura al giorno
  doppie <- dplyr::bind_rows(
    unite_coorte,
    dplyr::tibble(
      RFID = "A",
      giorno_lettura = as.POSIXct("2025-07-15 18:00:00", tz = "UTC"),
      fonte = "antenne"
    )
  )
  expect_equal(confronto_sistemi(doppie, anagrafica_coorte)$letture_dopo[1], 13)
  expect_equal(
    confronto_sistemi(una_lettura_al_giorno(doppie), anagrafica_coorte)$letture_dopo[1],
    12
  )

  # la coorte è quella dell'anagrafica: i periodi restano quelli di tutti
  solo_b <- confronto_sistemi(
    unite_coorte,
    anagrafica_coorte[2, ],
    periodi_confronto(unite_coorte)
  )
  expect_identical(solo_b$giorni_dopo, 90L)
  expect_equal(nrow(confronto_sistemi(unite_coorte, anagrafica_coorte[0, ])), 0)
})

test_that("il riepilogo dà contenitori mai letti, ritmo e tasso di lettura", {
  r <- riepilogo_confronto(confronto_coorte)
  expect_equal(nrow(r), 1)
  expect_equal(r$n_rfid, 4)
  expect_equal(r$censiti, 3)
  expect_equal(r$mai_letti, 1)
  expect_equal(r$una_o_meno, 2)
  expect_equal(r$quota_mai_letti, 0.25)
  expect_equal(r$quota_una_o_meno, 0.5)
  # solo A era già stato letto prima del periodo di riferimento
  expect_equal(r$gia_in_servizio, 1)
  expect_equal(r$mai_letti_in_servizio, 0)
  expect_equal(r$una_o_meno_in_servizio, 0)
  expect_equal(r$media_storico, 3.75)
  expect_equal(r$mediana_storico, 2.5)
  # il ritmo delle antenne pesa ogni contenitore con i suoi giorni
  expect_equal(r$media_antenne, 365 * 16 / 423)
  expect_equal(r$rapporto, (365 * 16 / 423) / 3.75)
  expect_equal(r$n_con_raccolte, 4)
  expect_equal(r$raccolte_previste, 91)
  expect_equal(r$tasso_storico, 15 / 91)
  expect_equal(r$tasso_antenne, 16 / (26 * 423 / 365))
  expect_equal(
    r$letture_recuperate,
    (16 / (26 * 423 / 365) - 15 / 91) * 91
  )

  # per servizio: la carta ha un contenitore solo, letto l'ultimo giorno
  per_servizio <- riepilogo_confronto(confronto_coorte, "tipologia")
  expect_identical(per_servizio$tipologia, c("Carta", "Secco"))
  expect_equal(per_servizio$n_rfid, c(1, 3))
  expect_equal(per_servizio$tasso_storico, c(10 / 13, 5 / 78))
  expect_true(is.nan(per_servizio$tasso_antenne[1]))
  expect_equal(per_servizio$tasso_antenne[2], 16 / (26 * 423 / 365))
  expect_equal(per_servizio$mai_letti, c(0, 1))

  # i contenitori senza raccolte restano nei conteggi, non nel tasso
  incerto <- confronto_coorte
  incerto$raccolte_previste[3] <- NA
  incerto$previste_storico[3] <- NA
  incerto$utili_storico[3] <- NA
  incerto$previste_antenne[3] <- NA
  incerto$utili_antenne[3] <- NA
  senza <- riepilogo_confronto(incerto)
  expect_equal(senza$n_rfid, 4)
  expect_equal(senza$n_con_raccolte, 3)
  expect_equal(senza$una_o_meno, 2)
  expect_equal(senza$tasso_storico, 14 / 65)
})

test_that("l'intervallo jackknife di una media è quello della t di Student", {
  x <- c(3, 8, 1, 12, 7, 7, 2)
  intervallo <- intervallo_jackknife(
    list(x = x, n = rep(1, length(x))),
    function(s) s$x / s$n
  )
  margine <- stats::qt(0.975, length(x) - 1) * stats::sd(x) / sqrt(length(x))
  expect_equal(
    intervallo,
    c(stima = mean(x), inferiore = mean(x) - margine, superiore = mean(x) + margine)
  )
  # un livello più alto allarga l'intervallo
  largo <- intervallo_jackknife(
    list(x = x, n = rep(1, length(x))),
    function(s) s$x / s$n,
    livello = 0.99
  )
  expect_lt(largo[["inferiore"]], intervallo[["inferiore"]])

  # differenza tra due rapporti: la stima è quella sui totali
  somme <- confronto_coorte[c(
    "utili_antenne",
    "previste_antenne",
    "utili_storico",
    "previste_storico"
  )]
  punti <- intervallo_jackknife(somme, function(s) {
    s$utili_antenne / s$previste_antenne - s$utili_storico / s$previste_storico
  })
  r <- riepilogo_confronto(confronto_coorte)
  expect_equal(punti[["stima"]], r$tasso_antenne - r$tasso_storico)
  expect_lt(punti[["inferiore"]], punti[["stima"]])
  expect_gt(punti[["superiore"]], punti[["stima"]])

  # con un contenitore solo l'intervallo non si può calcolare
  uno <- intervallo_jackknife(list(x = 5, n = 1), function(s) s$x / s$n)
  expect_equal(uno[["stima"]], 5)
  expect_true(is.na(uno[["inferiore"]]))
  nessuno <- intervallo_jackknife(
    list(x = numeric(0), n = numeric(0)),
    function(s) s$x / s$n
  )
  expect_true(all(is.na(nessuno)))
})

test_that("le letture mese per mese riportano anche i mesi senza letture", {
  mensili <- letture_mensili(unite_coorte, c("A", "B"))
  storiche <- mensili[mensili$fonte == "storico", ]
  # A: da febbraio 2024 ad agosto 2025, diciannove mesi
  expect_equal(nrow(storiche), 19)
  expect_identical(
    range(storiche$mese),
    as.Date(c("2024-02-01", "2025-08-01"))
  )
  expect_equal(sum(storiche$letture), 7)
  expect_equal(storiche$letture[storiche$mese == as.Date("2024-04-01")], 0)
  expect_equal(storiche$contenitori[storiche$mese == as.Date("2024-04-01")], 0)
  con_antenne <- mensili[mensili$fonte == "antenne", ]
  expect_identical(
    range(con_antenne$mese),
    as.Date(c("2025-07-01", "2025-12-01"))
  )
  expect_equal(sum(con_antenne$letture), 17)
  # a ottobre le antenne leggono sia A sia B
  expect_equal(
    con_antenne$contenitori[con_antenne$mese == as.Date("2025-10-01")],
    2
  )
  # tutti gli RFID, se non se ne indicano
  expect_equal(sum(letture_mensili(unite_coorte)$letture), nrow(unite_coorte))
  expect_equal(nrow(letture_mensili(unite_coorte, "nessuno")), 0)
})

test_that("la copertura settimanale conta mezzi, contenitori e contenitori nuovi", {
  letture <- dplyr::bind_rows(
    # lunedì 7 e mercoledì 9 luglio 2025: stessa settimana
    letture_rfid("A", c("2025-07-07", "2025-07-09", "2025-07-21")),
    letture_rfid("B", c("2025-07-09", "2025-07-22")),
    letture_rfid("C", "2025-07-27")
  )
  letture$matricola_veicolo[letture$RFID == "B"] <- "VEH002"
  settimane <- copertura_settimanale(letture)
  expect_identical(
    settimane$settimana,
    as.Date(c("2025-07-07", "2025-07-21"))
  )
  expect_equal(settimane$mezzi, c(2, 2))
  expect_equal(settimane$contenitori, c(2, 3))
  expect_equal(settimane$nuovi, c(2, 1))
  expect_equal(settimane$letture, c(3, 3))
  expect_equal(settimane$cumulati, c(2, 3))
})

test_that("i codici gemelli differiscono per un solo carattere", {
  codici <- c("00A1", "00A3", "00B1", "0A01", "00A7", "00A1", NA, "00A", "GZ1", "HZ1", "RX1", "RX2")
  coppie <- codici_gemelli(codici)
  chiavi <- paste(coppie$codice_a, coppie$codice_b)
  expect_setequal(
    chiavi,
    c("00A1 00A3", "00A1 00A7", "00A3 00A7", "00A1 00B1", "GZ1 HZ1", "RX1 RX2")
  )
  riga <- function(a, b) coppie[coppie$codice_a == a & coppie$codice_b == b, ]
  # 1 e 3 differiscono per un bit, 1 e 7 per due
  expect_identical(riga("00A1", "00A3")$posizione, 4L)
  expect_identical(riga("00A1", "00A3")$carattere_a, "1")
  expect_identical(riga("00A1", "00A3")$carattere_b, "3")
  expect_true(riga("00A1", "00A3")$un_bit)
  expect_false(riga("00A1", "00A7")$un_bit)
  expect_true(riga("00A3", "00A7")$un_bit)
  # A e B sono cifre esadecimali: 1010 e 1011
  expect_identical(riga("00A1", "00B1")$posizione, 3L)
  expect_true(riga("00A1", "00B1")$un_bit)
  # G e H non sono cifre esadecimali: non si può dire
  expect_true(is.na(riga("GZ1", "HZ1")$un_bit))
  # 1 e 2 differiscono per due bit
  expect_false(riga("RX1", "RX2")$un_bit)

  # stesso risultato del confronto di ogni codice con ogni altro
  set.seed(11)
  cifre <- c(0:9, LETTERS[1:6])
  casuali <- unique(replicate(
    400,
    paste(sample(cifre, 4, replace = TRUE, prob = c(8, rep(1, 15))), collapse = "")
  ))
  attese <- utils::combn(casuali, 2)
  diversi <- rowSums(
    do.call(rbind, strsplit(attese[1, ], "")) !=
      do.call(rbind, strsplit(attese[2, ], ""))
  )
  attese <- attese[, diversi == 1, drop = FALSE]
  trovate <- codici_gemelli(casuali)
  expect_gt(nrow(trovate), 50)
  expect_setequal(
    paste(trovate$codice_a, trovate$codice_b),
    paste(pmin(attese[1, ], attese[2, ]), pmax(attese[1, ], attese[2, ]))
  )
  expect_false(anyNA(trovate$un_bit))

  # niente coppie: tabella vuota con le stesse colonne
  expect_equal(nrow(codici_gemelli(c("0001", "0102"))), 0)
  expect_identical(names(codici_gemelli("0001")), names(coppie))
  expect_equal(nrow(codici_gemelli(character(0))), 0)
})

test_that("un gemello è sospetto se non compare mai insieme al codice giusto", {
  # 00000A1A01 è censito. Il sistema precedente lo legge il lunedì, a
  # settimane alterne, e nelle altre settimane legge il codice 00000A1A03.
  lunedi <- format(as.Date("2025-01-06") + 7 * 0:19)
  giusto <- lunedi[c(TRUE, FALSE)]
  sbagliato <- lunedi[c(FALSE, TRUE)]
  storico <- dplyr::bind_rows(
    storiche_il("00000A1A01", giusto),
    storiche_il("00000A1A03", sbagliato),
    # un vicino di casa: codice quasi uguale, letto negli stessi giorni
    storiche_il("00000B2B02", giusto),
    storiche_il("00000B2B03", giusto),
    # codice quasi uguale, ma di un altro giro: letto il giovedì
    storiche_il("00000C3C01", giusto),
    storiche_il("00000C3C03", format(as.Date(sbagliato) + 3)),
    # due bit di differenza
    storiche_il("00000D4D01", giusto),
    storiche_il("00000D4D07", sbagliato),
    # due censiti: due contenitori veri
    storiche_il("00000E5E01", giusto),
    storiche_il("00000E5E03", sbagliato)
  )
  censiti <- c("00000A1A01", "00000B2B02", "00000C3C01", "00000D4D01", "00000E5E01", "00000E5E03")
  letture <- dplyr::bind_rows(lapply(censiti, function(codice) {
    letture_rfid(codice, c("2025-07-07", "2025-07-21"), servizio = "SECCO")
  }))
  unite <- una_lettura_al_giorno(unisci_letture(letture, storico))
  coppie <- codici_gemelli(unique(unite$RFID))
  gemelli <- gemelli_sospetti(coppie, unite, letture)

  # le coppie con un solo censito: il censito è il codice, l'altro il gemello
  expect_setequal(
    paste(gemelli$codice, gemelli$gemello),
    c(
      "00000A1A01 00000A1A03",
      "00000B2B02 00000B2B03",
      "00000C3C01 00000C3C03",
      "00000D4D01 00000D4D07"
    )
  )
  riga <- function(codice) gemelli[gemelli$codice == codice, ]
  a <- riga("00000A1A01")
  expect_true(a$sospetto)
  # le coppie sospette stanno in cima
  expect_identical(gemelli$codice[1], "00000A1A01")
  expect_identical(a$gemello_letto_da, "sistema precedente")
  expect_equal(a$letture_codice, 12)
  expect_equal(a$letture_gemello, 10)
  expect_equal(a$quota_gemello, 10 / 22)
  expect_equal(a$letture_codice_storico, 10)
  expect_equal(a$letture_gemello_storico, 10)
  expect_equal(a$quota_gemello_storico, 0.5)
  expect_equal(a$giorni_in_comune, 0)
  expect_equal(a$quota_stessi_giorni, 1)
  expect_true(is.na(a$distanza_m))
  expect_true(a$un_bit)
  # letti negli stessi giorni: due contenitori
  expect_equal(riga("00000B2B02")$giorni_in_comune, 10)
  expect_false(riga("00000B2B02")$sospetto)
  # un altro giorno della settimana: un altro giro
  expect_equal(riga("00000C3C01")$giorni_in_comune, 0)
  expect_equal(riga("00000C3C01")$quota_stessi_giorni, 0)
  expect_false(riga("00000C3C01")$sospetto)
  # due bit di differenza
  expect_false(riga("00000D4D01")$un_bit)
  expect_false(riga("00000D4D01")$sospetto)

  # con le antenne conta anche la posizione: il gemello letto nello stesso
  # punto resta sospetto, quello letto lontano no
  vicino <- letture_rfid(
    "00000A1A03",
    "2025-08-04",
    presente = "Non Presente",
    atteso = "SECCO PAP"
  )
  con_vicino <- dplyr::bind_rows(letture, vicino)
  unite_vicino <- una_lettura_al_giorno(unisci_letture(con_vicino, storico))
  esito <- gemelli_sospetti(coppie, unite_vicino, con_vicino)
  a <- esito[esito$codice == "00000A1A01", ]
  expect_identical(a$gemello_letto_da, "entrambi")
  expect_equal(a$distanza_m, 0)
  expect_true(a$sospetto)
  lontano <- vicino
  lontano$latitudine <- lontano$latitudine + gradi_lat(5000)
  con_lontano <- dplyr::bind_rows(letture, lontano)
  esito <- gemelli_sospetti(
    coppie,
    una_lettura_al_giorno(unisci_letture(con_lontano, storico)),
    con_lontano
  )
  a <- esito[esito$codice == "00000A1A01", ]
  expect_equal(a$distanza_m, 5000, tolerance = 0.01)
  expect_false(a$sospetto)
  # la distanza massima si può cambiare
  esito <- gemelli_sospetti(
    coppie,
    una_lettura_al_giorno(unisci_letture(con_lontano, storico)),
    con_lontano,
    distanza_massima_m = 6000
  )
  expect_true(esito$sospetto[esito$codice == "00000A1A01"])

  # nessuna coppia con un solo censito
  nessuna <- gemelli_sospetti(
    coppie[coppie$codice_a == "00000E5E01", ],
    unite,
    letture
  )
  expect_equal(nrow(nessuna), 0)
  expect_identical(names(nessuna), names(gemelli))
})

test_that("i contenitori fuori comune sono quelli censiti letti lontano dal loro comune", {
  quando <- function(date) as.POSIXct(paste(date, "08:00:00"), tz = "UTC")
  letture <- dplyr::tibble(
    giorno_lettura = quando(c(
      # X: cinque letture, tre fuori comune, l'ultima di nuovo a casa
      "2025-01-01", "2025-01-08", "2025-01-15", "2025-01-22", "2025-01-29",
      # Y: sempre fuori, in un punto fuori dai comuni serviti
      "2025-02-01", "2025-02-08",
      # Z: mai fuori
      "2025-03-01",
      # N: non censito
      "2025-03-02"
    )),
    RFID = c(rep("X", 5), "Y", "Y", "Z", "N"),
    presente_a_database = c(rep("Presente", 8), "Non Presente"),
    comune_da_database = c(rep("LIMENA", 5), "ROANA", "ROANA", "LIMENA", NA),
    comune_geografico = c(
      "LIMENA", "CITTADELLA", "CITTADELLA", "TOMBOLO", "LIMENA",
      NA, NA, "LIMENA", "CITTADELLA"
    ),
    fuori_comune = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE)
  )
  fuori <- rfid_fuori_comune(letture)
  expect_identical(fuori$RFID, c("X", "Y"))
  expect_identical(fuori$comune_da_database, c("LIMENA", "ROANA"))
  expect_identical(fuori$cantiere, c("RUBANO", "ASIAGO"))
  # il comune in cui cade il maggior numero delle letture fuori comune
  expect_identical(fuori$letto_a, c("CITTADELLA", NA))
  expect_equal(fuori$letture, c(5, 2))
  expect_equal(fuori$letture_fuori, c(3, 2))
  expect_equal(fuori$quota_fuori, c(0.6, 1))
  expect_identical(fuori$prima_fuori, as.Date(c("2025-01-08", "2025-02-01")))
  expect_identical(fuori$ultima_fuori, as.Date(c("2025-01-22", "2025-02-08")))
  expect_identical(fuori$stabile, c(FALSE, TRUE))

  # nessuna lettura fuori comune, o colonna assente
  expect_equal(nrow(rfid_fuori_comune(letture[letture$RFID == "Z", ])), 0)
  expect_identical(
    names(rfid_fuori_comune(letture[letture$RFID == "Z", ])),
    names(fuori)
  )
  expect_equal(nrow(rfid_fuori_comune(letture_test())), 0)
})

test_that("sul dataset di esempio il sistema precedente leggeva una parte dei contenitori", {
  letture <- senza_sacchetti(dati_esempio())
  storico <- senza_sacchetti(storico_esempio())
  unite <- una_lettura_al_giorno(unisci_letture(letture, storico))
  periodi <- periodi_confronto(unite)
  expect_identical(periodi$riferimento, as.Date(c("2023-10-02", "2024-09-30")))
  expect_identical(periodi$giorni_riferimento, 365L)
  expect_identical(periodi$giorni_antenne, 457L)
  expect_false(periodi$sovrapposti)

  anagrafica <- anagrafica_rfid(letture)
  confronto <- confronto_sistemi(unite, anagrafica, periodi)
  expect_equal(nrow(confronto), 239)
  expect_false(any(rfid_sacchetto(confronto$RFID)))
  # il servizio manca solo ai non censiti con la stima incerta
  expect_equal(sum(is.na(confronto$tipologia)), 20)
  expect_true(all(
    confronto$presente_a_database[is.na(confronto$tipologia)] == "Non Presente"
  ))
  expect_equal(sum(confronto$raccolte_stimate), 17)

  # i contenitori del secco: uno su quattro mai letto nei dodici mesi prima
  secco <- confronto[confronto$tipologia %in% "Secco", ]
  r <- riepilogo_confronto(secco)
  expect_equal(r$n_rfid, 84)
  expect_equal(r$censiti, 79)
  expect_equal(r$mai_letti, 21)
  expect_equal(r$una_o_meno, 26)
  expect_equal(r$gia_in_servizio, 65)
  expect_equal(r$una_o_meno_in_servizio, 7)
  expect_equal(r$n_con_raccolte, 84)
  # da meno di un quarto a oltre l'80% delle raccolte previste
  expect_equal(r$tasso_storico, 0.23, tolerance = 0.03)
  expect_equal(r$tasso_antenne, 0.82, tolerance = 0.03)
  punti <- intervallo_jackknife(
    secco[c("utili_antenne", "previste_antenne", "utili_storico", "previste_storico")],
    function(s) {
      s$utili_antenne / s$previste_antenne - s$utili_storico / s$previste_storico
    }
  )
  expect_gt(punti[["inferiore"]], 0.5)
  expect_lt(punti[["superiore"]], 0.7)

  # le antenne leggono di più in ogni servizio e in ogni cantiere
  per_servizio <- riepilogo_confronto(
    confronto[!is.na(confronto$tipologia), ],
    "tipologia"
  )
  expect_equal(nrow(per_servizio), 6)
  expect_true(all(per_servizio$tasso_antenne - per_servizio$tasso_storico > 0.4))
  per_cantiere <- riepilogo_confronto(secco, "cantiere")
  expect_identical(per_cantiere$cantiere, cantieri())
  expect_true(all(per_cantiere$tasso_antenne > per_cantiere$tasso_storico))

  # il codice letto male: metà delle letture storiche sotto un altro codice
  coppie <- codici_gemelli(unique(unite$RFID))
  gemelli <- gemelli_sospetti(coppie, unite, letture)
  sospetti <- gemelli[gemelli$sospetto, ]
  expect_identical(sospetti$codice, "RFD20241003061")
  expect_identical(sospetti$gemello, "RFD20241003063")
  expect_identical(sospetti$gemello_letto_da, "sistema precedente")
  expect_equal(sospetti$letture_gemello, 37)
  expect_equal(sospetti$quota_gemello_storico, 0.49, tolerance = 0.02)
  expect_equal(sospetti$giorni_in_comune, 0)

  # i due contenitori letti lontano dal comune del database
  fuori <- rfid_fuori_comune(letture)
  expect_identical(fuori$RFID, c("RFD20250203302", "RFD20250905100"))
  expect_identical(fuori$stabile, c(FALSE, TRUE))
  expect_identical(fuori$letto_a[2], "CITTADELLA")
})
