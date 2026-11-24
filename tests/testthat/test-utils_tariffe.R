test_that("la tabella delle tariffe è quella dell'azienda", {
  tariffe <- tariffe_secco()
  expect_identical(tariffe$componenti, 0:6)
  expect_identical(tariffe$litri_inclusi, c(240, 720, 960, 1200, 1440, 1680, 1680))
  expect_identical(tariffe$litri_minimi, c(NA, 240, 360, 480, 600, 720, 840))
  expect_identical(tariffe$litri_premialita, c(NA, 480, 600, 720, 840, 960, 1200))
  expect_identical(litri_potenziamento(), 6240)
  penalita <- tariffe_contenitori()
  expect_identical(penalita$volume, c(120, 60, 40, 30))
  expect_identical(penalita$eccedenza, c(10, 5, 3.34, 2.5))
  # il premio non dipende dal contenitore: 5 euro ogni 120 litri (25/11/2026)
  expect_false("premio" %in% names(penalita))
  expect_equal(premio_al_litro(), 5 / 120)
  expect_equal(premio_al_litro(), 0.0416667, tolerance = 1e-6)
})

test_that("la tariffa al litro dell'eccedenza viene dal contenitore, o dal carrellato se il volume non c'è", {
  al_litro <- tariffa_al_litro(c(120, 60, 40, 30, 240, NA))
  expect_equal(al_litro$eccedenza[1:2], c(10 / 120, 5 / 60))
  expect_equal(al_litro$eccedenza[3], 3.34 / 40)
  expect_equal(al_litro$eccedenza[5:6], rep(10 / 120, 2))
  expect_named(al_litro, "eccedenza")
})

test_that("la fascia dei componenti arrotonda la media e si ferma a 6", {
  expect_identical(
    fascia_componenti(c(0, 0.4, 0.5, 1.49, 2.5, 5.6, 6, 9, NA)),
    c(0L, 0L, 1L, 1L, 3L, 6L, 6L, 6L, NA)
  )
})

test_that("premio ed eccedenza seguono gli esempi della tariffa, per un anno intero", {
  conferimenti <- function(litri, componenti = 1, potenziamento = FALSE, fattore = 1, volume = 120) {
    economia_forniture(dplyr::tibble(
      litri = litri,
      componenti_medi = componenti,
      potenziamento = potenziamento,
      fattore = fattore,
      volume_contenitore = volume
    ))
  }
  # un componente: massimo 480, minimo 240, premio (480 - litri) x 5/120
  premiati <- conferimenti(c(480, 360, 300, 240, 120, 0))
  expect_identical(
    as.character(premiati$classe),
    rep("Premialit\u00e0", 6)
  )
  # al massimo il premio e' zero, al minimo (e sotto) e' 10 euro
  expect_equal(premiati$premio_eur, c(0, 5, 7.5, 10, 10, 10))
  # sotto il minimo i litri addebitati restano il minimo
  expect_equal(premiati$litri_addebitati, c(480, 360, 300, 240, 240, 240))
  expect_equal(premiati$litri_sotto_massimo, c(0, 120, 180, 240, 240, 240))
  expect_equal(premiati$litri_minimi, rep(240, 6))
  expect_equal(premiati$litri_premialita, rep(480, 6))
  # oltre il massimo per la premialita' e fino ai litri inclusi: ordinaria
  ordinaria <- conferimenti(c(481, 720))
  expect_identical(as.character(ordinaria$classe), c("Ordinaria", "Ordinaria"))
  expect_equal(ordinaria$importo_eur, c(0, 0))
  # oltre i litri inclusi: eccedente, 10 euro ogni 120 litri
  ecceduta <- conferimenti(c(721, 960, 1200))
  expect_identical(as.character(ecceduta$classe), rep("Eccedente", 3))
  expect_equal(ecceduta$litri_eccedenti, c(1, 240, 480))
  expect_equal(ecceduta$eccedenza_eur, c(1, 240, 480) * 10 / 120)
  expect_equal(ecceduta$importo_eur, ecceduta$eccedenza_eur)
  # il premio massimo e' 10 euro per ogni fascia, tranne 6 o piu' componenti (840-1200): 15 euro
  massimi <- conferimenti(rep(0, 6), componenti = 1:6)
  expect_equal(massimi$premio_eur, c(10, 10, 10, 10, 10, 15))
  sei <- conferimenti(c(1200, 840, 100), componenti = 9)
  expect_identical(sei$fascia, c(6L, 6L, 6L))
  expect_equal(sei$premio_eur, c(0, 15, 15))
  # due componenti (minimo 360, massimo 600): (600 - 450) x 5/120
  due <- conferimenti(c(600, 450, 100), componenti = 2)
  expect_equal(due$premio_eur, c(0, 6.25, 10))
  # il premio non dipende dal volume del contenitore, l'eccedenza si'
  bidoncino <- conferimenti(c(480, 360, 100), volume = 40)
  expect_equal(bidoncino$premio_eur, c(0, 5, 10))
  eccedente <- conferimenti(c(840, 840), volume = c(120, 40))
  expect_equal(eccedente$eccedenza_eur, c(120 * 10 / 120, 120 * 3.34 / 40))
})

test_that("casa vuota e potenziamento non hanno minimo né premio", {
  vuota <- economia_forniture(dplyr::tibble(
    litri = c(0, 240, 360),
    componenti_medi = 0,
    potenziamento = FALSE,
    fattore = 1
  ))
  expect_identical(vuota$regime, rep("casa vuota", 3))
  expect_identical(as.character(vuota$classe), c("Ordinaria", "Ordinaria", "Eccedente"))
  expect_equal(vuota$premio_eur, c(0, 0, 0))
  expect_true(all(is.na(vuota$litri_minimi)))
  expect_equal(vuota$litri_addebitati, c(0, 240, 360))
  expect_equal(vuota$eccedenza_eur, c(0, 0, 120 * 10 / 120))

  # il potenziamento ha la precedenza sulla fascia
  potenziata <- economia_forniture(dplyr::tibble(
    litri = c(100, 6240, 6360),
    componenti_medi = 3,
    potenziamento = TRUE,
    fattore = 1
  ))
  expect_identical(potenziata$regime, rep("potenziamento", 3))
  # il potenziamento non ha premio e non paga eccedenza, nemmeno oltre i 6240 litri
  expect_identical(as.character(potenziata$classe), rep("Ordinaria", 3))
  expect_equal(potenziata$premio_eur, c(0, 0, 0))
  expect_equal(potenziata$litri_limite, rep(6240, 3))
  expect_equal(potenziata$eccedenza_eur, c(0, 0, 0))
  expect_equal(potenziata$litri_eccedenti, c(0, 0, 0))
  expect_equal(potenziata$importo_eur, c(0, 0, 0))
})

test_that("in un periodo corto i litri si riportano a un anno e si confrontano con le soglie annue", {
  # un semestre: 240 litri in mezzo anno sono 480 litri all'anno
  semestre <- economia_forniture(dplyr::tibble(
    litri = c(240, 180, 120, 361),
    componenti_medi = 1,
    potenziamento = FALSE,
    fattore = 0.5
  ))
  expect_equal(semestre$litri, c(240, 180, 120, 361))
  expect_equal(semestre$litri_annui, c(480, 360, 240, 722))
  # le soglie restano quelle annue
  expect_equal(semestre$litri_limite, rep(720, 4))
  expect_equal(semestre$litri_premialita, rep(480, 4))
  expect_equal(semestre$litri_minimi, rep(240, 4))
  # 480, 360 e 240 litri annui: premio di 0, 5 e 10 euro (annui, come negli esempi della tariffa)
  expect_equal(semestre$premio_eur[1:3], c(0, 5, 10))
  expect_identical(as.character(semestre$classe[4]), "Eccedente")
  expect_equal(semestre$eccedenza_eur[4], 2 * 10 / 120)
  # la classe e' quella che si avrebbe scalando le soglie al periodo
  scalate <- economia_forniture(
    dplyr::tibble(litri = c(240, 180, 120, 361), componenti_medi = 1, potenziamento = FALSE, fattore = 0.5),
    annualizza = FALSE
  )
  expect_identical(as.character(scalate$classe), as.character(semestre$classe))
  expect_equal(scalate$litri_limite, rep(360, 4))
  expect_equal(scalate$litri_premialita, rep(240, 4))
  expect_equal(scalate$litri_minimi, rep(120, 4))
  expect_equal(scalate$premio_eur[1:3], c(0, 2.5, 5))
  expect_equal(scalate$importo_eur, semestre$importo_eur * 0.5)
  # un anno intero: nessuna differenza
  intero <- economia_forniture(dplyr::tibble(
    litri = 360, componenti_medi = 1, potenziamento = FALSE, fattore = 1
  ))
  expect_equal(intero$litri_annui, 360)
  expect_equal(intero$premio_eur, 5)
  # senza giorni di attivita' la classe e i litri annui mancano
  vuota <- economia_forniture(dplyr::tibble(
    litri = 100, componenti_medi = 1, potenziamento = FALSE, fattore = c(0)
  ))
  expect_true(is.na(vuota$litri_annui))
  expect_true(is.na(vuota$classe))
  expect_true(is.na(economia_forniture(dplyr::tibble(
    litri = 100, componenti_medi = 1, potenziamento = FALSE, fattore = NA_real_
  ))$classe))
})

test_that("le fasce di eccedenza e le quote di premio hanno etichette leggibili", {
  fasce <- fasce_eccedenza(c(0, 10, 120, 121, 240, 500, NA))
  expect_identical(
    as.character(fasce),
    c(NA, "fino a 120 lt", "fino a 120 lt", "da 120 a 240 lt", "da 120 a 240 lt", "oltre 480 lt", NA)
  )
  expect_identical(
    levels(fasce),
    c("fino a 120 lt", "da 120 a 240 lt", "da 240 a 480 lt", "oltre 480 lt")
  )
  quote <- quote_premio(c(0, 5, 7, 15, 20))
  expect_identical(
    as.character(quote),
    c(NA, "fino a 5 €", "da 5 a 10 €", "da 10 a 15 €", "oltre 15 €")
  )
  expect_identical(
    levels(fasce_eccedenza(1, limiti = c(60, 120))),
    c("fino a 60 lt", "da 60 a 120 lt", "oltre 120 lt")
  )
})

test_that("la soglia di fatturazione di un gruppo e' quella della sua fascia", {
  soglie <- soglia_fatturazione_del_gruppo(c(
    "Casa vuota", "Potenziamento", "1 componente", "2 componenti", "3 componenti",
    "4 componenti", "5 componenti", "6 o pi\u00f9", "Componenti non noti",
    "Condomini", "Domestiche (totale)"
  ))
  expect_equal(soglie, c(240, NA, 720, 960, 1200, 1440, 1680, 1680, NA, NA, NA))
  # accetta anche un fattore, come `gruppo` delle forniture
  expect_equal(soglia_fatturazione_del_gruppo(factor("2 componenti", gruppi_forniture())), 960)
})

test_that("limite, minimo e massimo si ridimensionano ai giorni di attivita' nell'anno", {
  # una fornitura attiva meta' anno, in un periodo di un anno intero (attiva meta' del periodo)
  f <- function(litri, quota_attivita = 0.5, quota_anno = 0.5, fattore = 1, componenti_medi = 1) {
    economia_forniture(dplyr::tibble(
      litri = litri,
      componenti_medi = componenti_medi,
      potenziamento = FALSE,
      fattore = fattore,
      quota_attivita = quota_attivita,
      quota_anno = quota_anno
    ))
  }
  r <- f(c(0, 120, 180, 240, 241))
  # soglie dimezzate: minimo 120, massimo 240; i litri attesi sono quelli dei suoi giorni di attivita'
  expect_equal(r$litri_minimi, rep(120, 5))
  expect_equal(r$litri_premialita, rep(240, 5))
  expect_equal(r$litri_annui, c(0, 120, 180, 240, 241))
  # il premio massimo e' dimezzato (5 euro), poi (240 - litri) x 5/120
  expect_equal(r$premio_eur, c(5, 5, 2.5, 0, 0))
  expect_identical(
    as.character(r$classe),
    c("Premialit\u00e0", "Premialit\u00e0", "Premialit\u00e0", "Premialit\u00e0", "Ordinaria")
  )
  # un mese su dodici e dieci mesi su dodici: il premio massimo (10 euro) segue l'attivita' nell'anno
  mesi <- f(0, 1 / 12, 1 / 12)
  expect_equal(mesi$premio_eur, 10 / 12)
  dieci <- f(0, 10 / 12, 10 / 12)
  expect_equal(dieci$premio_eur, 10 * 10 / 12)
  # con 6 o piu' componenti il premio massimo e' 15 euro
  sei <- f(0, componenti_medi = 6)
  expect_equal(sei$premio_eur, 7.5)
  expect_equal(sei$litri_minimi, 420)
  expect_equal(sei$litri_premialita, 600)
  # anche il limite dell'eccedenza si dimezza (720 -> 360): 800 litri attesi sono 440 sopra
  eccedente <- f(c(300, 360, 361, 800))
  expect_equal(eccedente$litri_limite, rep(360, 4))
  expect_identical(as.character(eccedente$classe), c("Ordinaria", "Ordinaria", "Eccedente", "Eccedente"))
  expect_equal(eccedente$litri_eccedenti, c(0, 0, 1, 440))
  expect_equal(eccedente$eccedenza_eur, c(0, 0, 1, 440) * 10 / 120)
  expect_equal(eccedente$premio_eur, rep(0, 4))
  # la classe dipende solo dal ritmo: stessi litri al giorno di attivita', stessa classe di un anno intero
  intero_anno <- f(c(600, 720, 721, 1600), quota_attivita = 1, quota_anno = 1)
  expect_identical(as.character(intero_anno$classe), c("Ordinaria", "Ordinaria", "Eccedente", "Eccedente"))

  # attivata il 1 aprile, periodo dal 1 gennaio al 30 giugno: 91 giorni su 181 di periodo, 275 giorni su 365 nell'anno
  periodo <- 181 / 365
  attiva_nel_periodo <- 91 / 181
  nell_anno <- 275 / 365
  parziale <- f(100, attiva_nel_periodo, nell_anno, periodo)
  # litri al giorno di attivita' x giorni di attivita' dell'anno
  expect_equal(parziale$litri_annui, 100 * 275 / 91)
  expect_equal(parziale$litri_minimi, 240 * nell_anno)
  expect_equal(parziale$litri_premialita, 480 * nell_anno)
  expect_equal(parziale$premio_eur, (480 * nell_anno - 100 * 275 / 91) * 5 / 120)
  # un anno intero attivo per tutto il periodo e' la stima di prima, litri / fattore
  intera <- f(100, 1, 1, periodo)
  expect_equal(intera$litri_annui, 100 / periodo)
  expect_equal(intera$litri_premialita, 480)
  # cessata il 31 marzo (90 giorni): attiva tutto il periodo di attivita', soglie ridotte a 90/365
  cessata <- f(60, 90 / 365, 90 / 365, 1)
  expect_equal(cessata$litri_annui, 60)
  expect_equal(cessata$litri_premialita, 480 * 90 / 365)
  # senza le colonne la fornitura e' attiva tutto il periodo e tutto l'anno
  senza <- economia_forniture(dplyr::tibble(
    litri = c(0, 360), componenti_medi = 1, potenziamento = FALSE, fattore = 1
  ))
  expect_equal(senza$premio_eur, c(10, 5))
  expect_equal(senza$quota_attivita, c(1, 1))
  expect_equal(senza$quota_anno, c(1, 1))
  # una quota fuori da 0-1 si riporta nei limiti, una mancante vale 1
  fuori <- f(c(0, 0), quota_attivita = c(1.5, NA), quota_anno = c(1.5, NA))
  expect_equal(fuori$premio_eur, c(10, 10))
  # senza giorni di attivita' nel periodo i litri attesi e la classe mancano
  nessun_giorno <- f(10, quota_attivita = 0, quota_anno = 0.5)
  expect_true(is.na(nessun_giorno$litri_annui))
  expect_true(is.na(nessun_giorno$classe))
  # senza annualizzazione (giri) niente giorni di attivita': si scalano le soglie al periodo
  campione <- economia_forniture(
    dplyr::tibble(
      litri = 60, componenti_medi = 1, potenziamento = FALSE, fattore = 0.25,
      quota_attivita = 0.5, quota_anno = 0.5
    ),
    annualizza = FALSE
  )
  expect_equal(campione$litri_annui, 60)
  expect_equal(campione$litri_premialita, 480 * 0.25)
  expect_equal(campione$litri_minimi, 240 * 0.25)
  expect_equal(campione$premio_eur, (120 - 60) * 5 / 120)
  expect_equal(c(campione$quota_attivita, campione$quota_anno), c(1, 1))
  # casa vuota e potenziamento restano senza premio; anche il loro limite segue l'attivita'
  senza_premio <- economia_forniture(dplyr::tibble(
    litri = 0,
    componenti_medi = c(0, 3),
    potenziamento = c(FALSE, TRUE),
    fattore = 1,
    quota_attivita = 0.5,
    quota_anno = 0.5
  ))
  expect_equal(senza_premio$premio_eur, c(0, 0))
  expect_equal(senza_premio$litri_limite, c(120, 3120))
  # una casa vuota attiva meta' anno paga l'eccedenza oltre 120 litri attesi
  vuota <- f(c(120, 180), componenti_medi = 0)
  expect_identical(as.character(vuota$classe), c("Ordinaria", "Eccedente"))
  expect_equal(vuota$eccedenza_eur, c(0, 60 * 10 / 120))
})

test_that("i litri attesi a fine anno dipendono dai giorni di attivita'", {
  # attiva tutto: litri / fattore
  expect_equal(riporta_ad_anno(100, 0.5), 200)
  # attiva meta' del periodo e meta' dell'anno: litri per giorno di attivita' x giorni di attivita' dell'anno
  expect_equal(riporta_ad_anno(100, 0.5, 0.5, 0.25), 100 * 0.25 / (0.5 * 0.5))
  expect_equal(riporta_ad_anno(c(10, 20), 1, c(1, 0.5), c(1, 0.5)), c(10, 20))
  # senza giorni nel periodo o senza fattore: mancante
  expect_true(is.na(riporta_ad_anno(10, 0)))
  expect_true(is.na(riporta_ad_anno(10, 1, 0)))
  expect_true(is.na(riporta_ad_anno(10, NA_real_)))
})
