# Confronto tra il sistema precedente e le antenne sui contenitori letti da
# tutti e due i sistemi, per il report sul beneficio delle antenne.

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
  # tutte le letture del sistema precedente: prima, dentro e dopo
  expect_identical(confronto_coorte$letture_storico, c(7L, 0L, 1L, 10L))
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

test_that("la coorte è fatta dei contenitori letti da tutti e due i sistemi", {
  # B è letto solo dalle antenne: resta fuori
  coorte <- coorte_dei_due_sistemi(confronto_coorte)
  expect_identical(coorte$RFID, c("A", "C", "D"))
  expect_identical(names(coorte), names(confronto_coorte))
  expect_equal(nrow(coorte_dei_due_sistemi(confronto_coorte[0, ])), 0)
  r <- riepilogo_confronto(coorte)
  expect_equal(r$n_rfid, 3)
  expect_equal(r$mai_letti, 0)
  expect_equal(r$una_o_meno, 1)
  expect_equal(r$solo_dopo, 0)
  expect_equal(r$tasso_storico, 15 / 65)
  # A ha una lettura precedente dopo l'arrivo delle antenne, ma non solo quella
  expect_equal(riepilogo_confronto(confronto_coorte)$solo_dopo, 0)

  # F: le antenne lo leggono da agosto 2025, il sistema precedente solo dopo.
  # Entra nella coorte, e nel periodo di riferimento risulta senza letture.
  letture_f <- dplyr::bind_rows(
    letture_coorte,
    letture_rfid("F", c("2025-08-05", "2025-08-19"), servizio = "SECCO")
  )
  storico_f <- dplyr::bind_rows(
    storico_coorte,
    storiche_il("F", c("2025-09-02", "2025-09-16"))
  )
  unite_f <- unisci_letture(letture_f, storico_f)
  expect_identical(
    periodi_confronto(unite_f)$riferimento,
    periodi_confronto(unite_coorte)$riferimento
  )
  confronto_f <- confronto_sistemi(unite_f, anagrafica_rfid(letture_f))
  f <- confronto_f[confronto_f$RFID == "F", ]
  expect_identical(f$letture_storico, 2L)
  expect_identical(f$letture_sovrapposte, 2L)
  expect_identical(f$letture_riferimento, 0L)
  expect_identical(f$letture_prima, 0L)
  coorte_f <- coorte_dei_due_sistemi(confronto_f)
  expect_setequal(coorte_f$RFID, c("A", "C", "D", "F"))
  r_f <- riepilogo_confronto(coorte_f)
  expect_equal(r_f$solo_dopo, 1)
  expect_equal(r_f$mai_letti, 1)
  expect_equal(r_f$gia_in_servizio, 1)
  expect_equal(r_f$mai_letti_in_servizio, 0)
  # per gruppo: F è del secco
  per_servizio <- riepilogo_confronto(coorte_f, "tipologia")
  expect_identical(per_servizio$tipologia, c("Carta", "Secco"))
  expect_equal(per_servizio$solo_dopo, c(0, 1))
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

  # con i codici di interesse restano le sole coppie che ne contengono uno
  scelti <- casuali[seq(1, length(casuali), by = 7)]
  ristrette <- codici_gemelli(casuali, tra = scelti)
  attese_ristrette <- trovate[
    trovate$codice_a %in% scelti | trovate$codice_b %in% scelti,
  ]
  expect_gt(nrow(ristrette), 10)
  expect_lt(nrow(ristrette), nrow(trovate))
  expect_identical(ristrette, attese_ristrette)
  expect_true(all(ristrette$codice_a < ristrette$codice_b))
  expect_identical(codici_gemelli(casuali, tra = casuali), trovate)
  expect_equal(nrow(codici_gemelli(casuali, tra = "ZZZZ")), 0)
  solo_a3 <- codici_gemelli(codici, tra = "00A3")
  expect_setequal(
    paste(solo_a3$codice_a, solo_a3$codice_b),
    c("00A1 00A3", "00A3 00A7")
  )

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

test_that("un escluso può avere le letture precedenti sotto un codice gemello", {
  # Le antenne leggono di lunedì, dal 7 luglio 2025. Il sistema precedente
  # leggeva di lunedì fino a giugno, e continua anche dopo.
  lunedi_prima <- format(as.Date("2025-01-06") + 7 * 0:9)
  lunedi_antenne <- format(as.Date("2025-07-07") + 14 * 0:3)
  lunedi_sfalsati <- format(as.Date("2025-07-14") + 14 * 0:3)
  storico <- dplyr::bind_rows(
    # A: letto solo dalle antenne, con un gemello a un bit di distanza
    storiche_il("00000A1A03", lunedi_prima),
    # B: gemello a due bit di distanza
    storiche_il("00000B2B07", lunedi_prima),
    # C: gemello letto di giovedì, da un altro giro
    storiche_il("00000C3C03", format(as.Date(lunedi_prima) + 3)),
    # D: gemello letto negli stessi giorni delle antenne, quindi un altro
    # contenitore dello stesso giro
    storiche_il("00000D4D03", c(lunedi_prima, lunedi_antenne)),
    # E: letto da tutti e due i sistemi, con un vicino dal codice quasi uguale
    # che le antenne non hanno ancora letto
    storiche_il("00000E5E01", lunedi_prima),
    storiche_il("00000E5E03", c(lunedi_prima, lunedi_sfalsati)),
    # F: letto da tutti e due i sistemi, senza gemelli
    storiche_il("00000F6F01", lunedi_prima)
  )
  antenne <- c(
    "00000A1A01",
    "00000B2B01",
    "00000C3C01",
    "00000D4D01",
    "00000E5E01",
    "00000F6F01",
    # senza gemelli
    "0000017101",
    # due codici quasi uguali letti tutti e due dalle antenne
    "00000A9901",
    "00000A9903"
  )
  letture <- dplyr::bind_rows(lapply(antenne, function(codice) {
    letture_rfid(codice, lunedi_antenne, servizio = "SECCO")
  }))
  unite <- una_lettura_al_giorno(unisci_letture(letture, storico))
  coppie <- codici_gemelli(unique(unite$RFID), tra = antenne)
  gemelli <- gemelli_tra_sistemi(coppie, unite)

  # un codice delle antenne e uno del solo sistema precedente
  expect_setequal(
    paste(gemelli$codice, gemelli$gemello),
    c(
      "00000A1A01 00000A1A03",
      "00000B2B01 00000B2B07",
      "00000C3C01 00000C3C03",
      "00000D4D01 00000D4D03",
      "00000E5E01 00000E5E03"
    )
  )
  riga <- function(codice) gemelli[gemelli$codice == codice, ]
  a <- riga("00000A1A01")
  expect_false(a$codice_in_storico)
  expect_equal(a$letture_codice, 4)
  expect_equal(a$letture_gemello, 10)
  expect_equal(a$giorni_in_comune, 0)
  expect_equal(a$quota_stessi_giorni, 1)
  expect_true(a$compatibile)
  expect_true(a$sospetto)
  # due bit: compatibile con i giorni, ma non sospetto
  expect_true(riga("00000B2B01")$compatibile)
  expect_false(riga("00000B2B01")$sospetto)
  # un altro giorno della settimana
  expect_equal(riga("00000C3C01")$quota_stessi_giorni, 0)
  expect_false(riga("00000C3C01")$compatibile)
  # letti negli stessi giorni dai due sistemi: due contenitori
  expect_equal(riga("00000D4D01")$giorni_in_comune, 4)
  expect_false(riga("00000D4D01")$compatibile)
  # per il codice contano i soli giorni delle antenne, anche se ha letture
  # precedenti sue: i lunedì di prima, in comune con il gemello, non pesano
  e <- riga("00000E5E01")
  expect_true(e$codice_in_storico)
  expect_equal(e$letture_codice, 4)
  expect_equal(e$letture_gemello, 14)
  expect_equal(e$giorni_in_comune, 0)
  expect_true(e$sospetto)
  # prima le coppie sospette, poi le compatibili
  expect_true(all(gemelli$sospetto[1:2]))
  expect_identical(gemelli$codice[3], "00000B2B01")

  # la soglia sui giorni della settimana si può cambiare
  misto <- dplyr::bind_rows(
    storico,
    storiche_il("00000A1A03", format(as.Date(lunedi_prima[1:5]) + 2))
  )
  unite_miste <- una_lettura_al_giorno(unisci_letture(letture, misto))
  expect_false(
    gemelli_tra_sistemi(coppie, unite_miste)$compatibile[
      gemelli_tra_sistemi(coppie, unite_miste)$codice == "00000A1A01"
    ]
  )
  larga <- gemelli_tra_sistemi(coppie, unite_miste, quota_giorni = 0.6)
  expect_true(larga$compatibile[larga$codice == "00000A1A01"])

  # a confronto con la coorte: 5 esclusi, 2 contenitori letti dai due sistemi
  esclusi <- antenne[c(1:4, 7)]
  coorte <- antenne[5:6]
  riepilogo <- riepilogo_gemelli(gemelli, esclusi, coorte)
  expect_identical(riepilogo$indizio, c("gemello", "compatibile", "sospetto"))
  expect_equal(riepilogo$esclusi, c(4, 2, 1))
  expect_equal(riepilogo$quota_esclusi, c(0.8, 0.4, 0.2))
  expect_equal(riepilogo$coorte, c(1, 1, 1))
  expect_equal(riepilogo$quota_coorte, c(0.5, 0.5, 0.5))
  expect_equal(riepilogo$per_caso, c(2.5, 2.5, 2.5))
  # solo la parte oltre la quota della coorte, e mai meno di zero: con metà
  # della coorte che ha un gemello, l'80% degli esclusi vuol dire che il 60%
  # ha le letture precedenti sotto il gemello
  expect_equal(riepilogo$in_piu, c(3, 0, 0))
  # un codice con più gemelli conta una volta
  doppio <- dplyr::bind_rows(gemelli, gemelli[gemelli$codice == "00000A1A01", ])
  expect_identical(riepilogo_gemelli(doppio, esclusi, coorte), riepilogo)
  # senza gemelli nella coorte tutti gli esclusi con un gemello sono in più
  senza <- riepilogo_gemelli(gemelli, esclusi, "00000F6F01")
  expect_equal(senza$in_piu, c(4, 2, 1))
  # se ogni contenitore della coorte ha un gemello la stima non si può fare
  tutti <- riepilogo_gemelli(gemelli, esclusi, "00000E5E01")
  expect_equal(tutti$quota_coorte, c(1, 1, 1))
  expect_true(all(is.na(tutti$in_piu)))
  # senza esclusi non c'è niente da stimare
  expect_true(all(is.na(riepilogo_gemelli(gemelli, character(0), coorte)$in_piu)))

  # nessuna coppia tra i due sistemi
  nessuna <- gemelli_tra_sistemi(coppie[0, ], unite)
  expect_equal(nrow(nessuna), 0)
  expect_identical(names(nessuna), names(gemelli))
  expect_equal(riepilogo_gemelli(nessuna, esclusi, coorte)$esclusi, c(0, 0, 0))
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

  # la coorte: i contenitori letti da tutti e due i sistemi
  coorte <- coorte_dei_due_sistemi(confronto)
  expect_equal(nrow(coorte), 184)
  expect_equal(sum(coorte$presente_a_database == "Presente"), 170)
  secco_coorte <- coorte[coorte$tipologia %in% "Secco", ]
  r_coorte <- riepilogo_confronto(secco_coorte)
  expect_equal(r_coorte$n_rfid, 65)
  expect_equal(r_coorte$censiti, 64)
  expect_equal(r_coorte$mai_letti, 2)
  expect_equal(r_coorte$una_o_meno, 7)
  # nell'esempio i due sistemi non lavorano mai insieme
  expect_equal(r_coorte$gia_in_servizio, 65)
  expect_equal(r_coorte$solo_dopo, 0)
  expect_equal(r_coorte$tasso_storico, 0.30, tolerance = 0.03)
  expect_equal(r_coorte$tasso_antenne, 0.82, tolerance = 0.03)
  per_cantiere_coorte <- riepilogo_confronto(secco_coorte, "cantiere")
  expect_equal(per_cantiere_coorte$n_rfid, c(7, 7, 42, 9))
  expect_true(all(
    per_cantiere_coorte$tasso_antenne > per_cantiere_coorte$tasso_storico
  ))

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
  coppie <- codici_gemelli(unique(unite$RFID), tra = confronto$RFID)
  gemelli <- gemelli_sospetti(coppie, unite, letture)
  sospetti <- gemelli[gemelli$sospetto, ]
  expect_identical(sospetti$codice, "RFD20241003061")
  expect_identical(sospetti$gemello, "RFD20241003063")
  expect_identical(sospetti$gemello_letto_da, "sistema precedente")
  expect_equal(sospetti$letture_gemello, 37)
  expect_equal(sospetti$quota_gemello_storico, 0.49, tolerance = 0.02)
  expect_equal(sospetti$giorni_in_comune, 0)
  # nessuno dei 55 esclusi ha un gemello tra i codici del sistema precedente
  tra_sistemi <- gemelli_tra_sistemi(coppie, unite)
  expect_identical(tra_sistemi$codice, "RFD20241003061")
  esclusi <- setdiff(confronto$RFID, coorte$RFID)
  expect_length(esclusi, 55)
  per_indizio <- riepilogo_gemelli(tra_sistemi, esclusi, coorte$RFID)
  expect_equal(per_indizio$esclusi, c(0, 0, 0))
  expect_equal(per_indizio$coorte, c(1, 1, 1))
  expect_equal(per_indizio$in_piu, c(0, 0, 0))

  # i due contenitori letti lontano dal comune del database
  fuori <- rfid_fuori_comune(letture)
  expect_identical(fuori$RFID, c("RFD20250203302", "RFD20250905100"))
  expect_identical(fuori$stabile, c(FALSE, TRUE))
  expect_identical(fuori$letto_a[2], "CITTADELLA")
})
