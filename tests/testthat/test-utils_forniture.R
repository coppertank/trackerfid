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
  expect_equal(f2$fattore, 181 / 365)
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
    "codice_transponder,descrizione_rifiuto,volume,frequenza,numero_raccolte_annue,data_attivazione_contenitore,data_cessazione_contenitore,id_servizio,stato_servizio,id_utenza,comune_servizio",
    "123,SECCO,120,Q,26,2020-01-01,,F1,ATTIVA,U1,LIMENA"
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

