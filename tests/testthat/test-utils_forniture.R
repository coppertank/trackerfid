contenitori_forniture_prova <- function() {
  contenitore <- function(codice, fornitura, categoria, volume = 120, da = "2020-01-01", a = "2099-12-31", raccolte = 26, servizio = "SECCO") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = servizio,
      volume = volume,
      numero_raccolte_annue = raccolte,
      data_attivazione_contenitore = as.Date(da),
      data_cessazione_contenitore = as.Date(a),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = categoria
    )
  }
  dplyr::bind_rows(
    contenitore("A1", "F1", "31"),
    # due contenitori, uno da 40 litri; cessata a metà anno
    contenitore("A2", "F2", "31", a = "2025-06-30"),
    contenitore("A3", "F2", "31", volume = 40, a = "2025-06-30"),
    contenitore("A4", "F3", "34", volume = 240),
    contenitore("A5", "F4", "44", volume = 240),
    contenitore("A6", "F5", "31"),
    contenitore("A7", "F6", "32"),
    # cessato prima del periodo, e di un altro servizio
    contenitore("A8", "F7", "31", a = "2024-12-31"),
    contenitore("A9", "F8", "31", servizio = "CARTA")
  )
}

componenti_forniture_prova <- function() {
  riga <- function(fornitura, da, a, residenti, sottocategoria = "D") {
    dplyr::tibble(
      id_fornitura = fornitura,
      data_inizio_decorrenza = as.POSIXct(da, tz = ""),
      data_fine_decorrenza = as.POSIXct(a, tz = ""),
      nucleo_residenti = residenti,
      sottocategoria = sottocategoria
    )
  }
  dplyr::bind_rows(
    # la fine di una riga è l'inizio della seguente: il giorno non si conta due volte
    riga("F1", "2020-01-01", "2025-07-01", 2L),
    riga("F1", "2025-07-01", NA, 4L),
    riga("F2", "2020-01-01", NA, 1L),
    riga("F3", "2020-01-01", NA, 0L, "D"),
    riga("F4", "2020-01-01", NA, 0L, "ND"),
    riga("F5", "2020-01-01", NA, 3L),
    # senza inizio la riga non vale (come nella pipeline); F6 resta senza componenti
    riga("F6", NA, NA, 5L),
    riga("F7", "2020-01-01", NA, 2L),
    riga("F8", "2020-01-01", NA, 2L)
  )
}

manovre_forniture_prova <- function() {
  dplyr::tibble(
    id_fornitura = c("F5", "F5", "F1", "F2"),
    data_inizio_dichiarazione = as.Date(c("2025-04-01", "2025-06-01", "2025-03-01", "2025-01-01")),
    data_fine_dichiarazione = as.Date(c(NA, "2025-08-31", "2025-02-01", "2025-02-15"))
  )
}

anagrafica_prova <- function(dal = "2025-01-01", al = "2025-12-31") {
  anagrafica_forniture(
    contenitori_forniture_prova(),
    componenti_forniture_prova(),
    manovre_forniture_prova(),
    dal,
    al
  )
}

test_that("il tipo di fornitura ha la precedenza delle domestiche", {
  expect_identical(
    tipo_fornitura(
      c("31", "32", "34", "44", "31", NA, "99"),
      c("D", "ND", "ND", "ND", "ND", "ND", "D")
    ),
    c("domestica", "domestica", "condominio", "non domestica", "domestica", "non domestica", "altra")
  )
  expect_identical(tipo_fornitura(31, "d"), "domestica")
})

test_that("i giorni dell'anno sono 366 solo per un periodo dentro un anno bisestile", {
  expect_identical(giorni_dell_anno("2024-01-01", "2024-06-30"), 366)
  expect_identical(giorni_dell_anno("2025-01-01", "2025-12-31"), 365)
  expect_identical(giorni_dell_anno("2024-07-01", "2025-06-30"), 365)
})

test_that("i giorni coperti da intervalli sovrapposti si contano una volta", {
  coperti <- giorni_coperti(dplyr::tibble(
    id_fornitura = c("a", "a", "a", "b"),
    da = as.Date(c("2025-01-01", "2025-01-05", "2025-02-01", "2025-03-01")),
    a = as.Date(c("2025-01-10", "2025-01-20", "2025-02-02", "2025-03-01"))
  ))
  # 1-20 gennaio e 1-2 febbraio; un giorno
  expect_identical(coperti$giorni, c(22, 1))
})

test_that("l'anagrafica tiene le forniture attive nel periodo, con i loro contenitori", {
  anagrafica <- anagrafica_prova()
  # F7 e' cessata prima del periodo
  expect_setequal(anagrafica$id_fornitura, c("F1", "F2", "F3", "F4", "F5", "F6", "F8"))
  f2 <- anagrafica[anagrafica$id_fornitura == "F2", ]
  expect_identical(f2$contenitori, 2L)
  expect_identical(f2$volume_totale, 160)
  expect_identical(f2$volume_contenitore, 120)
  expect_identical(f2$servizio, "SECCO")
  # attiva dal 1 gennaio al 30 giugno
  expect_identical(f2$giorni_attivi, 181)
  # il fattore e' quello del periodo, non dell'attivita' (F2 e' attiva solo sei mesi)
  expect_equal(f2$fattore, 1)
  # due contenitori da 26 conferimenti massimi l'anno: il massimo della fornitura e' la somma
  expect_identical(f2$conferimenti_massimi_annui, 52)
  expect_identical(f2$litri_massimi_annui, 26 * 120 + 26 * 40)
  # il periodo riduce l'attività: dal 1 aprile, 275 giorni
  parziale <- anagrafica_prova("2025-04-01", "2025-12-31")
  expect_identical(parziale$giorni_attivi[parziale$id_fornitura == "F1"], 275)
  expect_false("F2" %in% parziale$id_fornitura[parziale$giorni_attivi > 100])
})

test_that("i componenti sono la media pesata sul tempo, senza contare due volte il giorno del cambio", {
  anagrafica <- anagrafica_prova()
  f1 <- anagrafica[anagrafica$id_fornitura == "F1", ]
  # 181 giorni con 2 residenti (fino al 30 giugno) e 184 con 4
  expect_equal(f1$componenti_medi, (181 * 2 + 184 * 4) / 365)
  expect_identical(f1$giorni_con_componenti, 365)
  # la riga senza inizio non vale: nessun componente
  f6 <- anagrafica[anagrafica$id_fornitura == "F6", ]
  expect_true(is.na(f6$componenti_medi))
  expect_identical(as.character(f6$gruppo), "Componenti non noti")
  # in un periodo prima del cambio la media e' quella della riga
  prima <- anagrafica_prova("2025-01-01", "2025-06-30")
  expect_equal(prima$componenti_medi[prima$id_fornitura == "F1"], 2)
})

test_that("il potenziamento vale se una manovra e' in vigore per almeno meta' dei giorni", {
  anagrafica <- anagrafica_prova()
  potenziamento <- stats::setNames(anagrafica$potenziamento, anagrafica$id_fornitura)
  # F5: dal 1 aprile, 275 giorni; la seconda dichiarazione e' dentro la prima
  expect_true(potenziamento[["F5"]])
  # F1: la dichiarazione finisce prima di cominciare (annullata); F2: 46 giorni su 181
  expect_false(potenziamento[["F1"]])
  expect_false(potenziamento[["F2"]])
  expect_identical(as.character(anagrafica$gruppo[anagrafica$id_fornitura == "F5"]), "Potenziamento")
})

test_that("i gruppi dividono domestiche, condomini e non domestiche", {
  anagrafica <- anagrafica_prova()
  gruppo <- stats::setNames(as.character(anagrafica$gruppo), anagrafica$id_fornitura)
  expect_identical(gruppo[["F1"]], "3 componenti")
  expect_identical(gruppo[["F2"]], "1 componente")
  expect_identical(gruppo[["F3"]], "Condomini")
  expect_identical(gruppo[["F4"]], "Non domestiche")
  expect_identical(gruppo[["F5"]], "Potenziamento")
  expect_identical(
    as.character(gruppo_fornitura(rep("domestica", 4), c(0, 0.4, 5.6, 9), FALSE)),
    c("Casa vuota", "Casa vuota", "6 o più", "6 o più")
  )
  expect_identical(levels(anagrafica$gruppo), gruppi_forniture())
})

test_that("i conteggi di forniture e contenitori sommano le domestiche e le altre", {
  conteggi <- conta_forniture(anagrafica_prova())
  forniture <- stats::setNames(conteggi$forniture, conteggi$gruppo)
  contenitori <- stats::setNames(conteggi$contenitori, conteggi$gruppo)
  expect_identical(forniture[["3 componenti"]], 1L)
  expect_identical(forniture[["Condomini"]], 1L)
  expect_identical(contenitori[["1 componente"]], 2L)
  # domestiche: F1, F2, F5, F6, F8; tutte: più il condominio e la non domestica
  expect_identical(forniture[["Domestiche (totale)"]], 5L)
  expect_identical(forniture[["Tutte le forniture"]], 7L)
  expect_identical(forniture[["Tutte le forniture"]], sum(forniture[gruppi_forniture()]))
  expect_equal(conta_non_domestiche(anagrafica_prova())$categoria, "44")
})

test_that("l'anagrafica si legge dai file estratti, con la categoria del contenitore", {
  file <- scrivi_origine_prova(withr::local_tempdir())
  anagrafica <- leggi_anagrafica_forniture(file, "2026-01-01", "2026-12-31")
  expect_identical(anagrafica$id_fornitura, "F1")
  expect_identical(anagrafica$categoria, "31")
  expect_identical(anagrafica$tipo, "domestica")
  # nel 2026 i residenti sono 2 fino al 14 agosto e 3 dopo; la manovra dal 20 agosto
  expect_false(anagrafica$potenziamento)
  expect_equal(anagrafica$componenti_medi, (226 * 2 + 139 * 3) / 365)
})

test_that("la fascia di componenti del report si sceglie con un numero o un nome", {
  expect_null(gruppo_da_filtro(""))
  expect_null(gruppo_da_filtro(NULL))
  expect_null(gruppo_da_filtro(NA))
  expect_identical(gruppo_da_filtro("0"), "Casa vuota")
  expect_identical(gruppo_da_filtro("1"), "1 componente")
  expect_identical(gruppo_da_filtro(3), "3 componenti")
  expect_identical(gruppo_da_filtro("6+"), "6 o pi\u00f9")
  expect_identical(gruppo_da_filtro("6"), "6 o pi\u00f9")
  expect_identical(gruppo_da_filtro("Potenziamento"), "Potenziamento")
  expect_error(gruppo_da_filtro("7"), "non riconosciuto")
  expect_true(all(
    vapply(c("0", 1:5, "6+", "potenziamento"), gruppo_da_filtro, "") %in% gruppi_forniture()
  ))
})

test_that("senza manovre o composizioni che toccano le forniture i conteggi restano vuoti", {
  attivita <- dplyr::tibble(
    id_fornitura = "F1",
    inizio = as.Date("2025-01-01"),
    fine = as.Date("2025-12-31")
  )
  nessuna <- componenti_medi_pesati(attivita, componenti_forniture_prova()[0, ])
  expect_identical(nrow(nessuna), 0L)
  altrove <- componenti_medi_pesati(attivita, dplyr::filter(componenti_forniture_prova(), id_fornitura == "F2"))
  expect_identical(nrow(altrove), 0L)
  expect_identical(
    nrow(giorni_coperti(dplyr::tibble(id_fornitura = character(0), da = as.Date(character(0)), a = as.Date(character(0))))),
    0L
  )
  # manovre di forniture che non sono attive: nessuna fornitura con il potenziamento
  anagrafica <- anagrafica_forniture(
    contenitori_forniture_prova(),
    componenti_forniture_prova(),
    dplyr::tibble(
      id_fornitura = "ALTRA",
      data_inizio_dichiarazione = as.Date("2025-01-01"),
      data_fine_dichiarazione = as.Date(NA)
    ),
    "2025-01-01",
    "2025-12-31"
  )
  expect_false(any(anagrafica$potenziamento))
  # e senza nessun contenitore attivo l'anagrafica e' vuota
  vuota <- anagrafica_forniture(
    contenitori_forniture_prova(),
    componenti_forniture_prova(),
    manovre_forniture_prova(),
    "2100-01-01",
    "2100-12-31"
  )
  expect_identical(nrow(vuota), 0L)
})

test_that("senza la categoria dei contenitori vale quella della composizione dei nuclei", {
  contenitori <- contenitori_forniture_prova()
  contenitori$categoria_contenitore <- NA_character_
  componenti <- componenti_forniture_prova()
  componenti$categoria <- ifelse(componenti$id_fornitura == "F3", "34", "31")
  anagrafica <- anagrafica_forniture(
    contenitori,
    componenti,
    manovre_forniture_prova(),
    "2025-01-01",
    "2025-12-31"
  )
  tipo <- stats::setNames(anagrafica$tipo, anagrafica$id_fornitura)
  expect_identical(tipo[["F1"]], "domestica")
  expect_identical(tipo[["F3"]], "condominio")
  # la categoria del contenitore, se c'e', ha la precedenza
  contenitori$categoria_contenitore <- "32"
  con_contenitore <- anagrafica_forniture(contenitori, componenti, manovre_forniture_prova(), "2025-01-01", "2025-12-31")
  expect_true(all(con_contenitore$categoria == "32"))
})

test_that("manca codice_categoria nel file dei contenitori: avviso e categoria vuota", {
  percorso <- scrivi_csv(c(
    "codice_transponder,descrizione_rifiuto,volume,frequenza,numero_raccolte_annue,data_attivazione_contenitore,data_cessazione_contenitore,id_servizio,stato_servizio,id_utenza,comune_servizio,contenitore_gratuito",
    "123,SECCO,120,Q,26,2020-01-01,,F1,ATTIVA,U1,LIMENA,N"
  ))
  expect_warning(
    contenitori <- importa_contenitori(percorso, categoria = TRUE),
    "manca `codice_categoria`"
  )
  expect_true(is.na(contenitori$categoria_contenitore))
  expect_no_warning(importa_contenitori(percorso))
})

test_that("con un servizio si contano solo i contenitori di quel servizio, anche dentro la stessa fornitura", {
  contenitori <- dplyr::bind_rows(
    contenitori_forniture_prova(),
    # una fornitura del secco con anche un contenitore della carta, e viceversa
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "M1", id_fornitura = "FM", servizio_transponder = "SECCO", volume = 120),
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "M2", id_fornitura = "FM", servizio_transponder = " carta ", volume = 240)
  )
  tutti <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31"
  )
  secco <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31",
    servizio = "secco"
  )
  # senza servizio la fornitura mista conta tutti e due i contenitori, con il servizio solo quello del secco
  expect_identical(tutti$contenitori[tutti$id_fornitura == "FM"], 2L)
  expect_identical(secco$contenitori[secco$id_fornitura == "FM"], 1L)
  expect_identical(secco$volume_totale[secco$id_fornitura == "FM"], 120)
  # le forniture della carta non ci sono; le altre del secco restano come prima
  expect_false("F8" %in% secco$id_fornitura)
  expect_true(all(secco$servizio == "SECCO"))
  gli_stessi <- setdiff(secco$id_fornitura, "FM")
  expect_identical(
    secco$contenitori[match(gli_stessi, secco$id_fornitura)],
    tutti$contenitori[match(gli_stessi, tutti$id_fornitura)]
  )
  # i conteggi del report vengono dalla stessa tabella
  expect_identical(
    conta_forniture(secco)$contenitori[conta_forniture(secco)$gruppo == "Tutte le forniture"],
    sum(secco$contenitori)
  )
})

test_that("il massimo annuo di conferimenti e' la somma di quello di ogni contenitore", {
  contenitore <- function(codice, fornitura, raccolte, volume = 120) {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = "SECCO",
      volume = volume,
      numero_raccolte_annue = raccolte,
      data_attivazione_contenitore = as.Date("2020-01-01"),
      data_cessazione_contenitore = as.Date("2099-12-31"),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = "31"
    )
  }
  contenitori <- dplyr::bind_rows(
    # un bidone quindicinale (26) e uno settimanale (52) nella stessa fornitura
    contenitore("B1", "G1", 26),
    contenitore("B2", "G1", 52, volume = 240),
    # lo stesso bidone con due righe di associazione conta una volta
    contenitore("B3", "G2", 52),
    contenitore("B3", "G2", 52),
    # massimo annuo non noto per un bidone su due: conta quello noto
    contenitore("B4", "G3", 26),
    contenitore("B5", "G3", NA),
    # massimo annuo non noto per tutti
    contenitore("B6", "G4", NA)
  )
  anagrafica <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31"
  )
  riga <- function(f) anagrafica[anagrafica$id_fornitura == f, ]
  expect_identical(riga("G1")$conferimenti_massimi_annui, 78)
  expect_identical(riga("G1")$litri_massimi_annui, 26 * 120 + 52 * 240)
  expect_identical(riga("G2")$conferimenti_massimi_annui, 52)
  expect_identical(riga("G3")$conferimenti_massimi_annui, 26)
  expect_true(is.na(riga("G4")$conferimenti_massimi_annui))
  expect_true(is.na(riga("G4")$litri_massimi_annui))
  expect_false("raccolte_annue" %in% names(anagrafica))
})

test_that("forniture, contenitori e volumi si contano per gruppo e per servizio", {
  contenitori <- dplyr::bind_rows(
    contenitori_forniture_prova(),
    # una fornitura domestica con un bidone del secco (120 L) e uno della carta (240 L)
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "M1", id_fornitura = "FM", volume = 120),
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "M2", id_fornitura = "FM", servizio_transponder = "CARTA", volume = 240)
  )
  per_servizio <- anagrafica_forniture_per_servizio(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31"
  )
  expect_setequal(unique(per_servizio$servizio), c("SECCO", "CARTA"))
  # la fornitura mista compare una volta per servizio, con i suoi contenitori
  mista <- per_servizio[per_servizio$id_fornitura == "FM", ]
  expect_setequal(mista$servizio, c("SECCO", "CARTA"))
  expect_identical(mista$contenitori[mista$servizio == "CARTA"], 1L)
  expect_identical(mista$volume_totale[mista$servizio == "CARTA"], 240)

  tabella <- conta_forniture_per_servizio(per_servizio)
  tutte <- tabella[tabella$gruppo == "Tutte le forniture", ]
  riga <- function(servizio) tutte[tutte$servizio == servizio, ]
  # le forniture del totale sono distinte (la mista conta una volta), i contenitori e i volumi si sommano
  expect_identical(riga("Tutti i servizi")$forniture, dplyr::n_distinct(per_servizio$id_fornitura))
  expect_identical(riga("Tutti i servizi")$contenitori, sum(per_servizio$contenitori))
  expect_equal(riga("Tutti i servizi")$volume_totale, sum(per_servizio$volume_totale))
  expect_equal(riga("Tutti i servizi")$volume_medio, sum(per_servizio$volume_totale) / sum(per_servizio$contenitori))
  # senza il campo nessun contenitore e' gratuito
  expect_identical(riga("Tutti i servizi")$contenitori_gratuiti, 0L)
  expect_identical(riga("Tutti i servizi")$contenitori_non_gratuiti, riga("Tutti i servizi")$contenitori)
  expect_identical(riga("CARTA")$contenitori, 2L)
  expect_equal(riga("CARTA")$volume_totale, 120 + 240)
  expect_equal(riga("CARTA")$volume_medio, 180)
  # per servizio i contenitori sommano al totale
  expect_identical(
    riga("SECCO")$contenitori + riga("CARTA")$contenitori,
    riga("Tutti i servizi")$contenitori
  )
  # il secco e' il primo servizio di ogni gruppo
  uno <- tabella[tabella$gruppo == "1 componente", ]
  expect_identical(uno$servizio[1], "Tutti i servizi")
  expect_identical(uno$servizio[2], "SECCO")
  # i gruppi senza forniture non compaiono
  expect_false("Potenziamento" %in% tabella$gruppo[tabella$forniture == 0])
  expect_true(all(tabella$forniture > 0))
})

test_that("i bidoni di una fornitura si elencano per servizio e periodo", {
  contenitori <- dplyr::bind_rows(
    contenitori_forniture_prova(),
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "M1", id_fornitura = "F2", volume = 240),
    # lo stesso bidone con due righe conta una volta; un bidone della carta non entra
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "M2", id_fornitura = "F1", servizio_transponder = "CARTA")
  )
  bidoni <- bidoni_delle_forniture(contenitori, c("F1", "F2", "F7"), "2025-01-01", "2025-12-31")
  riga <- function(f) bidoni$bidoni[bidoni$id_fornitura == f]
  expect_identical(riga("F1"), "A1 (120 L)")
  # F2 ha A2 (120 L), A3 (40 L) e M1 (240 L)
  expect_identical(riga("F2"), "A2 (120 L), A3 (40 L), M1 (240 L)")
  # F7 e' cessata prima del periodo: nessun bidone
  expect_false("F7" %in% bidoni$id_fornitura)
  # senza servizio c'e' anche la carta
  tutti <- bidoni_delle_forniture(contenitori, "F1", "2025-01-01", "2025-12-31", servizio = NULL)
  expect_identical(tutti$bidoni, "A1 (120 L), M2 (120 L)")
})

test_that("il fattore dipende solo dal periodo, non da quando la fornitura e' stata attivata", {
  contenitori <- dplyr::bind_rows(
    contenitori_forniture_prova(),
    # attivata a fine anno
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "N1", id_fornitura = "FN", data_attivazione_contenitore = as.Date("2025-12-20"))
  )
  anagrafica <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31"
  )
  expect_equal(anagrafica$fattore, rep(1, nrow(anagrafica)))
  # i giorni di attivita' restano, ma non entrano nel fattore
  expect_equal(anagrafica$giorni_attivi[anagrafica$id_fornitura == "FN"], 12)
  semestre <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-06-30"
  )
  expect_equal(semestre$fattore, rep(181 / 365, nrow(semestre)))
  # un periodo di due anni vale due anni (litri annualizzati = media annua)
  due_anni <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2024-01-01", "2025-12-31"
  )
  expect_equal(unique(due_anni$fattore), 731 / 365)
})

test_that("i contenitori gratuiti si contano a parte, e un codice nullo non e' un bidone", {
  contenitore <- function(codice, fornitura, gratuito, volume = 120) {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = "SECCO",
      volume = volume,
      numero_raccolte_annue = 26,
      contenitore_gratuito = gratuito,
      data_attivazione_contenitore = as.Date("2020-01-01"),
      data_cessazione_contenitore = as.Date("2099-12-31"),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = "31"
    )
  }
  contenitori <- dplyr::bind_rows(
    # un bidone a pagamento (120 L) e uno gratuito (240 L)
    contenitore("B1", "G1", "N"),
    contenitore("B2", "G1", "S", volume = 240),
    # tutti gratuiti
    contenitore("B3", "G2", "S", volume = 60),
    # nessun gratuito
    contenitore("B4", "G3", "N", volume = 40),
    # un codice di soli zeri: la fornitura non ha un bidone
    contenitore("0000000000", "G4", "N")
  )
  anagrafica <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31"
  )
  riga <- function(f) anagrafica[anagrafica$id_fornitura == f, ]
  expect_identical(riga("G1")$contenitori, 2L)
  expect_identical(riga("G1")$contenitori_gratuiti, 1L)
  expect_identical(riga("G1")$contenitori_non_gratuiti, 1L)
  # il volume del contenitore a pagamento, non quello del gratuito
  expect_identical(riga("G1")$volume_contenitore_non_gratuito, 120)
  expect_identical(riga("G2")$contenitori_non_gratuiti, 0L)
  expect_true(is.na(riga("G2")$volume_contenitore_non_gratuito))
  expect_identical(riga("G3")$contenitori_gratuiti, 0L)
  expect_false("G4" %in% anagrafica$id_fornitura)
  # senza il campo (file vecchi) nessun contenitore e' gratuito
  senza_campo <- anagrafica_forniture(
    dplyr::select(contenitori, -"contenitore_gratuito"),
    componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31"
  )
  expect_equal(sum(senza_campo$contenitori_gratuiti), 0)
})

test_that("la tabella per servizio conta i contenitori gratuiti e non gratuiti", {
  contenitore <- function(codice, fornitura, gratuito) {
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = codice, id_fornitura = fornitura, contenitore_gratuito = gratuito)
  }
  contenitori <- dplyr::bind_rows(
    contenitore("B1", "G1", "N"),
    contenitore("B2", "G1", "S"),
    contenitore("B3", "G2", "S")
  )
  per_servizio <- anagrafica_forniture_per_servizio(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31"
  )
  tutte <- conta_forniture_per_servizio(per_servizio)
  tutte <- tutte[tutte$gruppo == "Tutte le forniture" & tutte$servizio == "Tutti i servizi", ]
  expect_identical(tutte$contenitori, 3L)
  expect_identical(tutte$contenitori_gratuiti, 2L)
  expect_identical(tutte$contenitori_non_gratuiti, 1L)
})

test_that("un bidone gratuito si riconosce nell'elenco dei bidoni di una fornitura", {
  contenitori <- dplyr::bind_rows(
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "B1", id_fornitura = "G1", contenitore_gratuito = "N"),
    dplyr::mutate(contenitori_forniture_prova()[1, ], codice_transponder = "B2", id_fornitura = "G1", contenitore_gratuito = "S", volume = 240)
  )
  bidoni <- bidoni_delle_forniture(contenitori, "G1", "2025-01-01", "2025-12-31")
  expect_identical(bidoni$bidoni, "B1 (120 L), B2 (240 L, gratuito)")
})

test_that("la propensione parte dai soli bidoni e forniture letti nel periodo", {
  contenitore <- function(codice, fornitura, volume = 120, raccolte = 26, gratuito = "N", servizio = "SECCO") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = servizio,
      volume = volume,
      numero_raccolte_annue = raccolte,
      contenitore_gratuito = gratuito,
      data_attivazione_contenitore = as.Date("2020-01-01"),
      data_cessazione_contenitore = as.Date("2099-12-31"),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = "31"
    )
  }
  contenitori <- dplyr::bind_rows(
    # G1: due bidoni, ne e' letto uno solo (120 L, 26 conferimenti)
    contenitore("B1", "G1"),
    contenitore("B2", "G1", volume = 240, raccolte = 52),
    # G2: letti tutti e due, uno gratuito, uno senza massimo annuo
    contenitore("B3", "G2", volume = 60, gratuito = "S"),
    contenitore("B4", "G2", volume = 120, raccolte = NA_real_),
    # G3: mai letto nel periodo
    contenitore("B5", "G3"),
    # G4: letto, ma e' della carta
    contenitore("B6", "G4", servizio = "CARTA")
  )
  anagrafica <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31", "SECCO"
  )
  letture <- function(rfid, fornitura, stato = "PIA") {
    dplyr::tibble(RFID = rfid, id_fornitura = fornitura, stato = factor(stato, stati_lettura()))
  }
  unita <- dplyr::bind_rows(
    letture("B1", "G1"), letture("B1", "G1"),
    letture("B3", "G2"), letture("B4", "G2"),
    letture("B6", "G4"),
    # una lettura non associata non fa di un bidone un bidone letto
    letture("B5", "G3", "PIT")
  )
  letti <- anagrafica_dei_bidoni_letti(anagrafica, contenitori, unita, "2025-01-01", "2025-12-31")
  # restano G1 e G2: G3 non e' mai letto, G4 e' un altro servizio
  expect_setequal(letti$id_fornitura, c("G1", "G2"))
  g1 <- letti[letti$id_fornitura == "G1", ]
  # di G1 conta solo B1: un contenitore, 120 L, 26 conferimenti, 3.120 litri massimi
  expect_identical(g1$contenitori, 1L)
  expect_equal(g1$volume_totale, 120)
  expect_equal(g1$conferimenti_massimi_annui, 26)
  expect_equal(g1$litri_massimi_annui, 26 * 120)
  # di G2 contano entrambi: uno gratuito, e il massimo e' quello del solo bidone che lo ha
  g2 <- letti[letti$id_fornitura == "G2", ]
  expect_identical(g2$contenitori, 2L)
  expect_identical(g2$contenitori_gratuiti, 1L)
  expect_identical(g2$contenitori_non_gratuiti, 1L)
  expect_identical(g2$contenitori_con_massimo, 1L)
  expect_equal(g2$conferimenti_massimi_annui, 26)
  expect_equal(g2$litri_massimi_annui, 26 * 60)
  # gruppo, tipo e fattore restano quelli dell'anagrafica
  expect_identical(g1$gruppo, anagrafica$gruppo[anagrafica$id_fornitura == "G1"])
  # un periodo senza letture non lascia niente
  vuota <- anagrafica_dei_bidoni_letti(anagrafica, contenitori, unita[0, ], "2025-01-01", "2025-12-31")
  expect_identical(nrow(vuota), 0L)
})

test_that("la base della propensione sono i bidoni attivi almeno un giorno nel periodo, letti o no", {
  contenitore <- function(codice, fornitura, da = "2020-01-01", a = "2099-12-31", volume = 120, raccolte = 26, gratuito = "N", servizio = "SECCO") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = servizio,
      volume = volume,
      numero_raccolte_annue = raccolte,
      contenitore_gratuito = gratuito,
      data_attivazione_contenitore = as.Date(da),
      data_cessazione_contenitore = as.Date(a),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = "31"
    )
  }
  # periodo: anno 2025
  contenitori <- dplyr::bind_rows(
    # G1: un bidone attivo dal 2020 e uno attivato a meta' anno: contano tutti e due
    contenitore("B1", "G1"),
    contenitore("B2", "G1", da = "2025-06-01", volume = 240, raccolte = 52),
    # G2: un bidone mai letto (conta lo stesso)
    contenitore("B3", "G2"),
    # G3: cessato a marzo: attivo in parte, conta
    contenitore("B4", "G3", a = "2025-03-31"),
    # G4: attivato a marzo e letto: conta, anche se non era attivo il primo giorno
    contenitore("B5", "G4", da = "2025-03-01"),
    # G5: della carta
    contenitore("B6", "G5", servizio = "CARTA"),
    # G6: cessato prima del periodo
    contenitore("B7", "G6", a = "2024-12-31"),
    # G7: un bidone gratuito, e uno senza massimo annuo
    contenitore("B8", "G7", volume = 60, gratuito = "S"),
    contenitore("B9", "G7", raccolte = NA_real_),
    # G8: un codice di soli zeri non e' un bidone
    contenitore("0000000000", "G8"),
    # G9: attivato l'ultimo giorno del periodo: conta
    contenitore("B10", "G9", da = "2025-12-31"),
    # G10: attivato il giorno dopo la fine del periodo: non conta
    contenitore("B11", "G10", da = "2026-01-01")
  )
  anagrafica <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31", "SECCO"
  )
  base <- anagrafica_dei_bidoni_attivi(anagrafica, contenitori, "2025-01-01", "2025-12-31")
  # G1, G2, G3, G4, G7, G9: G5 e' un altro servizio, G6 era cessato, G8 non ha bidoni, G10 nasce dopo
  expect_setequal(base$id_fornitura, c("G1", "G2", "G3", "G4", "G7", "G9"))
  # di G1 contano tutti e due i bidoni, anche quello attivato a meta' anno: 360 L, 78 conferimenti
  g1 <- base[base$id_fornitura == "G1", ]
  expect_identical(g1$contenitori, 2L)
  expect_equal(g1$volume_totale, 120 + 240)
  expect_equal(g1$conferimenti_massimi_annui, 26 + 52)
  expect_equal(g1$litri_massimi_annui, 26 * 120 + 52 * 240)
  # con le letture non c'entra: un bidone mai letto resta (G2), cessato nel periodo anche (G3)
  expect_identical(base$contenitori[base$id_fornitura %in% c("G2", "G3")], c(1L, 1L))
  # G7: uno gratuito, e il massimo e' quello del solo bidone che lo ha
  g7 <- base[base$id_fornitura == "G7", ]
  expect_identical(g7$contenitori, 2L)
  expect_identical(g7$contenitori_gratuiti, 1L)
  expect_identical(g7$contenitori_con_massimo, 1L)
  expect_equal(g7$litri_massimi_annui, 26 * 60)
  # gruppo, tipo e fattore restano quelli dell'anagrafica
  expect_identical(g1$gruppo, anagrafica$gruppo[anagrafica$id_fornitura == "G1"])
  expect_equal(g1$fattore, anagrafica$fattore[anagrafica$id_fornitura == "G1"])
  # la base e' quella dell'anagrafica: stesse forniture e stessi contenitori
  expect_setequal(base$id_fornitura, anagrafica$id_fornitura)
  expect_identical(base$contenitori[order(base$id_fornitura)], anagrafica$contenitori[order(anagrafica$id_fornitura)])
  # un periodo piu' corto: a giugno G3 (cessato a marzo) e G9 (attivato a dicembre) escono
  a_giugno <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-06-01", "2025-06-30", "SECCO"
  )
  base_giugno <- anagrafica_dei_bidoni_attivi(a_giugno, contenitori, "2025-06-01", "2025-06-30")
  expect_false(any(c("G3", "G9", "G10") %in% base_giugno$id_fornitura))
  expect_true(all(c("G1", "G2", "G4", "G7") %in% base_giugno$id_fornitura))
  # senza contenitori attivi non resta niente
  expect_identical(nrow(anagrafica_dei_bidoni_attivi(anagrafica, contenitori, "1999-01-01", "1999-12-31")), 0L)
})

test_that("una fornitura con almeno una lettura nel periodo e' sempre nella base della propensione", {
  contenitore <- function(codice, fornitura, da, a = "2099-12-31") {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = "SECCO",
      volume = 120,
      numero_raccolte_annue = 26,
      contenitore_gratuito = "N",
      data_attivazione_contenitore = as.Date(da),
      data_cessazione_contenitore = as.Date(a),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = "31"
    )
  }
  contenitori <- dplyr::bind_rows(
    contenitore("L1", "H1", "2020-01-01"),
    # attivato e letto a meta' periodo
    contenitore("L2", "H2", "2025-09-15"),
    # cessato a meta' periodo e letto prima
    contenitore("L3", "H3", "2020-01-01", "2025-02-10")
  )
  anagrafica <- anagrafica_forniture(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-01-01", "2025-12-31", "SECCO"
  )
  letture <- function(rfid, fornitura) {
    dplyr::tibble(RFID = rfid, id_fornitura = fornitura, stato = factor("PIA", stati_lettura()))
  }
  unita <- dplyr::bind_rows(letture("L1", "H1"), letture("L2", "H2"), letture("L3", "H3"))
  base <- anagrafica_dei_bidoni_attivi(anagrafica, contenitori, "2025-01-01", "2025-12-31")
  letti <- anagrafica_dei_bidoni_letti(anagrafica, contenitori, unita, "2025-01-01", "2025-12-31")
  # tutte le forniture lette sono nella base
  expect_true(all(letti$id_fornitura %in% base$id_fornitura))
  expect_setequal(base$id_fornitura, c("H1", "H2", "H3"))
})

test_that("la tabella di forniture e contenitori conta i bidoni attivi anche solo in parte nel periodo", {
  contenitore <- function(codice, fornitura, da, a, servizio = "SECCO", volume = 120) {
    dplyr::tibble(
      codice_transponder = codice,
      servizio_transponder = servizio,
      volume = volume,
      numero_raccolte_annue = 26,
      data_attivazione_contenitore = as.Date(da),
      data_cessazione_contenitore = as.Date(a),
      id_fornitura = fornitura,
      id_utenza = paste0("U", fornitura),
      categoria_contenitore = "31"
    )
  }
  # periodo: 1 marzo - 30 giugno 2025
  contenitori <- dplyr::bind_rows(
    # interi nel periodo, o a cavallo di uno dei due estremi: contano
    contenitore("C1", "H1", "2020-01-01", "2099-12-31"),
    contenitore("C2", "H2", "2025-04-10", "2099-12-31"), # attivato nel periodo
    contenitore("C3", "H3", "2020-01-01", "2025-04-10"), # cessato nel periodo
    contenitore("C4", "H4", "2025-06-30", "2099-12-31"), # attivo solo l'ultimo giorno
    contenitore("C5", "H5", "2020-01-01", "2025-03-01"), # attivo solo il primo giorno
    # fuori dal periodo: non contano
    contenitore("C6", "H6", "2025-07-01", "2099-12-31"), # attivato il giorno dopo
    contenitore("C7", "H7", "2020-01-01", "2025-02-28"), # cessato il giorno prima
    # una fornitura con un bidone attivo e uno no: conta solo il primo
    contenitore("C8", "H8", "2020-01-01", "2099-12-31", volume = 120),
    contenitore("C9", "H8", "2025-09-01", "2099-12-31", volume = 240),
    # un bidone della carta attivo in parte: la fornitura compare nel suo servizio
    contenitore("C10", "H1", "2025-05-01", "2025-05-31", servizio = "CARTA", volume = 240)
  )
  per_servizio <- anagrafica_forniture_per_servizio(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-03-01", "2025-06-30"
  )
  secco <- per_servizio[per_servizio$servizio == "SECCO", ]
  expect_setequal(secco$id_fornitura, c("H1", "H2", "H3", "H4", "H5", "H8"))
  expect_false(any(c("H6", "H7") %in% per_servizio$id_fornitura))
  # H8: del secondo bidone (attivato a settembre) non si conta niente
  h8 <- secco[secco$id_fornitura == "H8", ]
  expect_identical(h8$contenitori, 1L)
  expect_equal(h8$volume_totale, 120)
  # la tabella somma i soli bidoni attivi: 6 del secco e 1 della carta
  tabella <- conta_forniture_per_servizio(per_servizio)
  tutte <- tabella[tabella$gruppo == "Tutte le forniture", ]
  riga <- function(servizio) tutte[tutte$servizio == servizio, ]
  expect_identical(riga("SECCO")$forniture, 6L)
  expect_identical(riga("SECCO")$contenitori, 6L)
  expect_equal(riga("CARTA")$volume_totale, 240)
  # H1 ha il secco e la carta: nel totale e' una fornitura sola, con due bidoni
  expect_identical(riga("Tutti i servizi")$forniture, 6L)
  expect_identical(riga("Tutti i servizi")$contenitori, 7L)
  # un periodo di un solo giorno (30 giugno) tiene i bidoni attivi quel giorno
  un_giorno <- anagrafica_forniture_per_servizio(
    contenitori, componenti_forniture_prova(), manovre_forniture_prova(), "2025-06-30", "2025-06-30"
  )
  expect_setequal(un_giorno$id_fornitura, c("H1", "H2", "H4", "H8"))
})
