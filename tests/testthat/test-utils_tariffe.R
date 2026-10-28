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
  expect_identical(penalita$premio, c(5, 2.5, 1.67, 1.25))
})

test_that("la tariffa al litro viene dal contenitore, o dal carrellato se il volume non c'è", {
  al_litro <- tariffa_al_litro(c(120, 60, 40, 30, 240, NA))
  expect_equal(al_litro$eccedenza[1:2], c(10 / 120, 5 / 60))
  expect_equal(al_litro$premio[3], 1.67 / 40)
  expect_equal(al_litro$eccedenza[5:6], rep(10 / 120, 2))
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
  # un componente: massimo 480, minimo 240, 5 euro ogni 120 litri in meno
  premiati <- conferimenti(c(480, 360, 300, 240, 120, 0))
  expect_identical(
    as.character(premiati$classe),
    rep("Premialità", 6)
  )
  expect_equal(premiati$premio_eur, c(5, 10, 12.5, 15, 15, 15))
  # sotto il minimo i litri addebitati restano il minimo
  expect_equal(premiati$litri_addebitati, c(480, 360, 300, 240, 240, 240))
  expect_equal(premiati$litri_sotto_massimo, c(0, 120, 180, 240, 240, 240))
  # oltre il massimo per la premialità e fino ai litri inclusi: ordinaria
  ordinaria <- conferimenti(c(481, 720))
  expect_identical(as.character(ordinaria$classe), c("Ordinaria", "Ordinaria"))
  expect_equal(ordinaria$importo_eur, c(0, 0))
  # oltre i litri inclusi: eccedente, 10 euro ogni 120 litri
  ecceduta <- conferimenti(c(721, 960, 1200))
  expect_identical(as.character(ecceduta$classe), rep("Eccedente", 3))
  expect_equal(ecceduta$litri_eccedenti, c(1, 240, 480))
  expect_equal(ecceduta$eccedenza_eur, c(1, 240, 480) * 10 / 120)
  expect_equal(ecceduta$importo_eur, ecceduta$eccedenza_eur)
  # 6 o più componenti: massimo 1200, minimo 840, quindi premio fino a 20 euro
  sei <- conferimenti(c(1200, 840), componenti = 9)
  expect_identical(sei$fascia, c(6L, 6L))
  expect_equal(sei$premio_eur, c(5, 20))
  # con un contenitore da 40 litri il premio per conferimento è 1,67 euro
  bidoncino <- conferimenti(480, volume = 40)
  expect_equal(bidoncino$premio_eur, 1.67)
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
  expect_identical(as.character(potenziata$classe), c("Ordinaria", "Ordinaria", "Eccedente"))
  expect_equal(potenziata$premio_eur, c(0, 0, 0))
  expect_equal(potenziata$litri_limite, rep(6240, 3))
  expect_equal(potenziata$eccedenza_eur[3], 10)
})

test_that("in un periodo corto le soglie e il premio si riducono in proporzione", {
  # un semestre: la metà dei litri, e il premio di un conferimento si dimezza
  semestre <- economia_forniture(dplyr::tibble(
    litri = c(240, 180, 120, 361),
    componenti_medi = 1,
    potenziamento = FALSE,
    fattore = 0.5
  ))
  expect_equal(semestre$litri_limite, rep(360, 4))
  expect_equal(semestre$litri_premialita, rep(240, 4))
  expect_equal(semestre$litri_minimi, rep(120, 4))
  # a metà anno 240 litri valgono come 480 in un anno: premio di 5 euro, per metà periodo
  expect_equal(semestre$premio_eur[1:3], c(2.5, 5, 7.5))
  expect_identical(as.character(semestre$classe[4]), "Eccedente")
  expect_equal(semestre$eccedenza_eur[4], 1 * 10 / 120)
  # senza giorni di attività la classe manca
  expect_true(is.na(economia_forniture(dplyr::tibble(
    litri = 100,
    componenti_medi = 1,
    potenziamento = FALSE,
    fattore = NA_real_
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
