# Letture sintetiche di una via: un giro del secco passa ogni 14 giorni, per
# venti volte, e legge i bidoni esposti. Ogni bidone ha il suo schema di
# esposizione: `esposto(passaggio)` dice se esce. I passaggi in
# `senza_antenna` li fa un mezzo senza antenna: non lasciano letture.
letture_via <- function(
  bidoni = list(),
  senza_antenna = integer(0),
  passaggi = 20,
  primo_giorno = as.Date("2025-01-07")
) {
  m_lat <- 1 / 111320
  m_lon <- 1 / (111320 * cos(45.7 * pi / 180))
  righe <- list()
  for (nome in names(bidoni)) {
    b <- bidoni[[nome]]
    for (j in seq_len(passaggi)) {
      if (j %in% senza_antenna || !b$esposto(j)) next
      righe[[length(righe) + 1]] <- data.frame(
        giorno_lettura = as.POSIXct(primo_giorno + 14 * (j - 1), tz = "UTC") + 8 * 3600 + b$posizione * 10,
        RFID = nome,
        presente_a_database = if (is.null(b$fornitura)) "Non Presente" else "Presente",
        servizio_transponder = if (is.null(b$fornitura)) NA_character_ else "SECCO",
        servizio_atteso = b$giro %||% "SECCO",
        id_fornitura = b$fornitura %||% NA_character_,
        volume_previsto = if (is.null(b$fornitura)) NA_real_ else 120,
        numero_raccolte_annue_previste = if (is.null(b$fornitura)) NA_real_ else 26,
        nucleo_residenti = b$nucleo %||% NA_integer_,
        tipo_categoria = if (is.null(b$fornitura)) NA_character_ else b$tipo %||% "DOMESTICA",
        manovra_sociale = b$manovra %||% NA_character_,
        latitudine = 45.7 + (b$scarto_m %||% 0) * m_lat,
        longitudine = 11.7 + b$posizione * 15 * m_lon,
        matricola_veicolo = "VEH001",
        id_utenza = if (is.null(b$fornitura)) NA_character_ else paste0("U", b$fornitura),
        stato_fornitura = if (is.null(b$fornitura)) NA_character_ else "APERTO",
        data_attivazione_contenitore = as.Date(NA),
        data_cessazione_contenitore = as.Date(NA),
        nucleo_non_residenti = NA_integer_,
        nucleo_totale = NA_integer_,
        categoria = NA_character_,
        sottocategoria = NA_character_,
        comune_fornitura = "PROVA",
        comune_lettura = "PROVA",
        stringsAsFactors = FALSE
      )
    }
  }
  letture <- do.call(rbind, righe)
  letture[order(letture$giorno_lettura), , drop = FALSE]
}

# Una via di dieci bidoni censiti che escono a passaggi alterni, più quelli
# del caso in prova. I vicini hanno cinque componenti: 13 conferimenti l'anno
# contro 14 consentiti, entro il limite.
via <- function(altri = list(), ...) {
  vicini <- stats::setNames(lapply(1:10, function(i) {
    list(
      posizione = i,
      fornitura = sprintf("V%02d", i),
      nucleo = 5L,
      esposto = function(j) (j + i) %% 2 == 0
    )
  }), sprintf("VICINO%02d", 1:10))
  letture_via(c(vicini, altri), ...)
}

test_that("il limite dipende dai residenti, o dal potenziamento", {
  expect_identical(litri_conferibili()$litri, c(720, 960, 1200, 1440, 1680))
  expect_identical(litri_potenziamento(), 6240)
  expect_identical(
    litri_consentiti(c(0, 1, 2, 3, 4, 5, 9, NA)),
    c(720, 720, 960, 1200, 1440, 1680, 1680, NA)
  )
  # il potenziamento sostituisce il limite del nucleo, anche senza nucleo
  expect_identical(litri_consentiti(c(1, 5, NA), c(TRUE, TRUE, TRUE)), rep(6240, 3))
  expect_identical(litri_consentiti(2, NA), 960)
})

test_that("solo tre manovre danno il potenziamento", {
  expect_identical(
    ha_potenziamento(c(
      "MANOVRA SOCIALE NEONATI",
      " manovra  sociale anziani ",
      "MANOVRA SOCIALE DISABILI",
      "MANOVRA SOCIALE ALTRO",
      NA
    )),
    c(TRUE, TRUE, TRUE, FALSE, FALSE)
  )
})

test_that("le forniture domestiche si riconoscono dalla tipologia", {
  expect_identical(
    fornitura_domestica(c("DOMESTICA", "Domestico", "UD", "NON DOMESTICA", "und", "ALTRO", NA)),
    c(TRUE, TRUE, TRUE, FALSE, FALSE, NA, NA)
  )
})

test_that("i parametri hanno valori predefiniti e si cambiano per nome", {
  parametri <- parametri_conferimenti()
  expect_identical(parametri$occasioni_minime, 4)
  expect_identical(parametri$peso_prior, 4)
  expect_identical(parametri_conferimenti(peso_prior = 8)$peso_prior, 8)
  expect_error(parametri_conferimenti(sconosciuto = 1), "Parametri sconosciuti: sconosciuto")
})

test_that("il tasso va verso la media con poche occasioni, verso la quota con molte", {
  stima <- stima_tasso(k = c(0, 2, 90), n = c(0, 2, 100), media = 0.4, peso = 4)
  expect_equal(stima$tasso[1], 0.4)
  # due su due non vuol dire sempre
  expect_equal(stima$tasso[2], (2 + 1.6) / 6)
  expect_equal(stima$tasso[3], 91.6 / 104)
  expect_true(all(stima$basso < stima$tasso & stima$tasso < stima$alto))
  # più occasioni, intervallo più stretto
  ampiezza <- stima$alto - stima$basso
  expect_lt(ampiezza[3], ampiezza[2])
})

test_that("fasce di litri e nucleo minimo seguono la tabella della tariffa", {
  litri <- c(500, 720, 721, 1680, 1681, 6240, 6241, NA)
  expect_identical(
    as.character(fascia_litri(litri)),
    c("fino a 720", "fino a 720", "721-960", "1.441-1.680", "1.681-6.240", "1.681-6.240", "oltre 6.240", NA)
  )
  expect_identical(
    nucleo_minimo_compatibile(litri),
    c("1", "1", "2", "5 o più", "Solo con potenziamento", "Solo con potenziamento", "Oltre il potenziamento", NA)
  )
})

test_that("le occasioni sono i giorni dei vicini sullo stesso percorso, dentro la finestra", {
  # tre contenitori: 1 è l'unità, 2 è sul suo percorso, 3 su un'altra via
  # raccolta in altri giorni
  unita <- data.frame(contenitore = 1L, inizio = 11L, fine = 40L)
  giorni_unita <- data.frame(u = 1L, giorno = c(10L, 24L))
  evidenza <- data.frame(
    c = c(1L, 1L, 2L, 2L, 2L, 3L, 3L),
    giorno = c(10L, 24L, 10L, 24L, 38L, 17L, 31L)
  )
  vicini <- data.frame(da = c(1L, 1L, 2L, 3L), a = c(2L, 3L, 1L, 1L), distanza = 1)
  esito <- occasioni_conferimento(unita, giorni_unita, evidenza, vicini, 3L)
  # 24 e 38: il 10 è prima della finestra, 17 e 31 sono dell'altra via
  expect_identical(esito$occasioni, 2L)
  expect_identical(esito$vicini, 1L)
  expect_identical(
    occasioni_conferimento(unita[0, ], giorni_unita[0, ], evidenza, vicini, 3L),
    list(occasioni = integer(0), vicini = integer(0))
  )
})

test_that("una fornitura che espone sempre supera il limite, una che espone poco no", {
  letture <- via(list(
    SEMPRE = list(posizione = 11, fornitura = "F1", nucleo = 1L, esposto = function(j) TRUE),
    RARO = list(posizione = 12, fornitura = "F2", nucleo = 2L, esposto = function(j) j %% 5 == 1)
  ))
  analisi <- analisi_conferimenti(letture, dal = "2025-01-01", al = "2025-12-31")
  expect_identical(analisi$periodo, as.Date(c("2025-01-07", "2025-12-31")))
  f <- analisi$forniture
  sempre <- f[f$id_fornitura == "F1", ]
  raro <- f[f$id_fornitura == "F2", ]
  # dopo la prima lettura restano 19 passaggi, tutti con il bidone esposto
  expect_identical(sempre$metodo, "passaggi")
  expect_equal(sempre$occasioni, 19)
  expect_equal(sempre$esposizioni, 19)
  expect_gt(sempre$conferimenti_annui, 23)
  expect_equal(sempre$conferimenti_consentiti, 6)
  expect_equal(sempre$conferimenti_eccedenti, sempre$conferimenti_annui - 6)
  expect_identical(sempre$classe, "Oltre il limite")
  # esce una volta su cinque: circa 5 conferimenti, il limite di due
  # componenti con un carrellato è 8
  expect_equal(raro$esposizioni, 3)
  expect_lt(raro$conferimenti_annui, 8)
  expect_identical(raro$classe, "Entro il limite")
  # la tabella va dalla più urgente
  expect_identical(f$id_fornitura[1], "F1")
  expect_identical(elenco_da_contattare(analisi)$id_fornitura, "F1")
})

test_that("i passaggi senza antenna non cambiano la stima, il conto per giorni sì", {
  bidone <- list(SEMPRE = list(posizione = 11, fornitura = "F1", nucleo = 1L, esposto = function(j) TRUE))
  tutti <- analisi_conferimenti(via(bidone), dal = "2025-01-01", al = "2025-12-31")$forniture
  meta <- analisi_conferimenti(
    via(bidone, senza_antenna = seq(2, 20, by = 2)),
    dal = "2025-01-01",
    al = "2025-12-31"
  )$forniture
  a <- tutti[tutti$id_fornitura == "F1", ]
  b <- meta[meta$id_fornitura == "F1", ]
  expect_equal(b$occasioni, 9)
  expect_equal(b$esposizioni, 9)
  # il tasso resta pieno: cambia solo l'incertezza
  expect_gt(b$tasso_osservato, 0.99)
  expect_lt(abs(b$conferimenti_annui - a$conferimenti_annui), 3)
  expect_lt(b$conferimenti_per_giorni, 0.6 * a$conferimenti_per_giorni)
})

test_that("potenziamento, non domestiche, nucleo mancante e dati insufficienti", {
  letture <- via(list(
    POTENZIATO = list(posizione = 11, fornitura = "F1", nucleo = 1L, manovra = "MANOVRA SOCIALE ANZIANI", esposto = function(j) TRUE),
    NEGOZIO = list(posizione = 12, fornitura = "F2", tipo = "NON DOMESTICA", esposto = function(j) TRUE),
    SENZA = list(posizione = 13, fornitura = "F3", esposto = function(j) TRUE),
    TARDI = list(posizione = 14, fornitura = "F4", nucleo = 1L, esposto = function(j) j >= 18)
  ))
  f <- analisi_conferimenti(letture, dal = "2025-01-01", al = "2025-12-31")$forniture
  classe <- stats::setNames(f$classe, f$id_fornitura)
  # 26 conferimenti contro 52 consentiti
  expect_identical(classe[["F1"]], "Entro il limite")
  expect_equal(f$litri_consentiti[f$id_fornitura == "F1"], 6240)
  expect_true(f$potenziamento[f$id_fornitura == "F1"])
  expect_identical(classe[["F2"]], "Non domestica")
  expect_identical(classe[["F3"]], "Nucleo mancante")
  # due occasioni dopo la prima lettura
  expect_identical(classe[["F4"]], "Dati insufficienti")
  expect_true(all(f$classe %in% classi_conferimenti()))
})

test_that("senza vicini le occasioni vengono dalle raccolte previste", {
  letture <- letture_via(list(
    SOLO = list(posizione = 1, fornitura = "F1", nucleo = 1L, esposto = function(j) j %% 2 == 1)
  ))
  analisi <- analisi_conferimenti(letture, dal = "2025-01-01", al = "2025-12-31")
  f <- analisi$forniture
  expect_identical(f$metodo, "raccolte previste")
  # dal giorno dopo la prima lettura al 31 dicembre, 26 raccolte l'anno
  giorni <- as.integer(as.Date("2025-12-31") - as.Date("2025-01-07"))
  expect_equal(f$occasioni, round(26 * giorni / 365))
  expect_equal(f$esposizioni, 9)
})

test_that("letture lontane, tag anomali, sacchetti e giri di altri rifiuti non contano", {
  letture <- via(list(
    SEMPRE = list(posizione = 11, fornitura = "F1", nucleo = 1L, esposto = function(j) TRUE),
    CARTA = list(posizione = 12, fornitura = "F2", nucleo = 1L, giro = "CARTA", esposto = function(j) TRUE)
  ))
  # una lettura del bidone a un chilometro, sul mezzo
  lontana <- letture[letture$RFID == "SEMPRE", ][1, ]
  lontana$giorno_lettura <- lontana$giorno_lettura + 3600
  lontana$latitudine <- lontana$latitudine + 0.01
  # un tag letto ogni giorno, e un sacchetto
  deposito <- letture[rep(1, 300), ]
  deposito$RFID <- "DEPOSITO"
  deposito$id_fornitura <- "F9"
  deposito$giorno_lettura <- as.POSIXct("2025-01-01 05:00:00", tz = "UTC") + 86400 * (0:299)
  sacchetto <- letture[1, ]
  sacchetto$RFID <- "00BD00000000000000000001"
  analisi <- analisi_conferimenti(
    rbind(letture, lontana, deposito, sacchetto),
    dal = "2025-01-01",
    al = "2025-12-31"
  )
  r <- analisi$riepilogo
  expect_identical(r$sacchetti, 1L)
  expect_identical(r$lontane, 1L)
  expect_identical(r$tag_anomali, 1L)
  expect_identical(r$letture_tag_anomali, 300L)
  expect_identical(r$fuori_dai_giri_secco, 20L)
  f <- analisi$forniture
  expect_false(any(c("F2", "F9") %in% f$id_fornitura))
  expect_equal(f$esposizioni[f$id_fornitura == "F1"], 19)
})

test_that("un non censito ha nucleo minimo, quota di potenziamento e candidato", {
  letture <- via(list(
    # il bidone censito smette di essere letto dopo il passaggio 8 e un
    # non censito, nello stesso punto, comincia dal 9
    VECCHIO = list(posizione = 11, fornitura = "F1", nucleo = 2L, manovra = "MANOVRA SOCIALE NEONATI", esposto = function(j) j <= 8),
    NUOVO = list(posizione = 11, scarto_m = 5, esposto = function(j) j >= 9)
  ))
  analisi <- analisi_conferimenti(letture, dal = "2025-01-01", al = "2025-12-31")
  nc <- analisi$non_censiti
  expect_identical(nc$RFID, "NUOVO")
  expect_identical(nc$metodo, "passaggi")
  expect_equal(nc$occasioni, 11)
  expect_equal(nc$esposizioni, 11)
  # raccolte e volume dai censiti vicini
  expect_identical(nc$origine_ipotesi, "censiti vicini")
  expect_equal(nc$raccolte_annue_ipotizzate, 26)
  expect_equal(nc$volume_ipotizzato, 120)
  expect_identical(nc$nucleo_minimo, "Solo con potenziamento")
  expect_identical(
    nc$quota_potenziamento,
    analisi$manovra_per_fascia$quota[match(nc$fascia_litri, analisi$manovra_per_fascia$fascia)]
  )
  expect_identical(nc$fornitura_candidata, "F1")
  expect_identical(nc$RFID_candidato, "VECCHIO")
  expect_identical(nc$manovra_candidata, "MANOVRA SOCIALE NEONATI")
  expect_lt(nc$distanza_candidato_m, 10)
  # la quota per fascia conta le forniture domestiche stimate
  m <- analisi$manovra_per_fascia
  expect_identical(levels(m$fascia), levels(fascia_litri(numeric(0))))
  expect_equal(sum(m$forniture), sum(analisi$forniture$classe %in% classi_conferimenti()[1:4]))
  expect_equal(m$quota, (m$con_potenziamento + 0.5) / (m$forniture + 1))
})

test_that("il CSV del passo 3 si legge con le colonne della fornitura", {
  letture <- leggi_letture_conferimenti(percorso_dataset_esempio())
  expect_named(
    letture,
    c(colonne_conferimenti()$obbligatorie, colonne_conferimenti()$facoltative)
  )
  expect_equal(nrow(letture), nrow(dati_esempio()))
  expect_s3_class(letture$giorno_lettura, "POSIXct")
  expect_equal(letture$giorno_lettura, dati_esempio()$giorno_lettura)
  expect_type(letture$nucleo_residenti, "integer")
  expect_s3_class(letture$data_attivazione_contenitore, "Date")
  expect_true(all(stats::na.omit(letture$manovra_sociale) %in% manovre_potenziamento()))

  # un file senza le colonne della fornitura va rigenerato
  vecchio <- withr::local_tempfile(fileext = ".csv")
  grezzo <- utils::read.csv(percorso_dataset_esempio(), colClasses = "character")
  utils::write.csv(grezzo[1:28], vecchio, row.names = FALSE)
  expect_error(
    leggi_letture_conferimenti(vecchio),
    "Mancano le colonne: nucleo_residenti, tipo_categoria, manovra_sociale"
  )
  expect_error(leggi_letture_conferimenti("non_esiste.csv"), "File non trovato")
})

test_that("l'analisi del dataset di esempio separa domestiche e non domestiche", {
  letture <- leggi_letture_conferimenti(percorso_dataset_esempio())
  analisi <- analisi_conferimenti(letture, dal = "2025-01-01", al = "2025-12-31")
  expect_identical(analisi$periodo, as.Date(c("2025-01-01", "2025-12-31")))
  expect_identical(analisi$riepilogo$sacchetti, 10L)
  f <- analisi$forniture
  expect_false(anyDuplicated(f$id_fornitura) > 0)
  # le non domestiche dell'esempio sono quelle con il codice di categoria UND
  esempio <- utils::read.csv(percorso_dataset_esempio(), colClasses = "character")
  und <- unique(esempio$id_fornitura[esempio$codice_categoria %in% "UND"])
  expect_gt(length(intersect(und, f$id_fornitura)), 0)
  expect_true(all(f$classe[f$id_fornitura %in% und] == "Non domestica"))
  expect_true(all(f$classe[!f$id_fornitura %in% und] != "Non domestica"))
  # la fornitura che ottiene la manovra a giugno ha il potenziamento
  expect_true(all(f$potenziamento == ha_potenziamento(f$manovra_sociale)))
  expect_named(f, names(forniture_vuote()))
  expect_true(all(analisi$non_censiti$RFID %in% esempio$RFID[esempio$presente_a_database == "Non Presente"]))
})

test_that("i risultati si scrivono in tre CSV", {
  analisi <- analisi_conferimenti(
    via(list(SEMPRE = list(posizione = 11, fornitura = "F1", nucleo = 1L, esposto = function(j) TRUE))),
    dal = "2025-01-01",
    al = "2025-12-31"
  )
  cartella <- withr::local_tempdir()
  percorsi <- esporta_analisi_conferimenti(analisi, cartella)
  expect_identical(names(percorsi), c("forniture", "contattare", "non_censiti"))
  expect_true(all(file.exists(percorsi)))
  expect_match(basename(percorsi[["contattare"]]), "conferimenti_da_contattare_2025-01-07_2025-12-31.csv")
  contattare <- utils::read.csv2(percorsi[["contattare"]], colClasses = "character")
  expect_identical(contattare$id_fornitura, "F1")
})

test_that("date e ore si leggono nel formato del passo 3 e negli altri", {
  x <- c("2025-09-01 08:30:15", "01/09/2025 08:30:15", "2025-09-01", NA, "non una data")
  attese <- as.POSIXct(c("2025-09-01 08:30:15", "2025-09-01 08:30:15", "2025-09-01 00:00:00", NA, NA), tz = "UTC")
  expect_equal(leggi_data_ora(x), attese)
})
