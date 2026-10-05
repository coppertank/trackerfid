test_that("l'analisi da file usa come predefinito l'anno più recente", {
  analisi <- genera_analisi_cluster(percorso_dataset_esempio())
  expect_identical(names(analisi), colonne_analisi_cluster())
  expect_true(all(analisi$analisi_dal == as.Date("2025-01-01")))
  expect_true(all(analisi$analisi_al == as.Date("2025-12-31")))
  expect_identical(analisi$indicatore_cluster, analisi_esempio()$indicatore_cluster)

  # con una sola data il periodo si completa con l'anno dell'altra
  solo_inizio <- genera_analisi_cluster(percorso_dataset_esempio(), data_inizio = as.Date("2024-11-01"))
  expect_true(all(solo_inizio$analisi_al == as.Date("2024-12-31")))
  solo_fine <- genera_analisi_cluster(percorso_dataset_esempio(), data_fine = "2025-06-30")
  expect_true(all(solo_fine$analisi_dal == as.Date("2025-01-01")))
  expect_true(all(solo_fine$globale_ultima_lettura < as.POSIXct("2025-07-01", tz = "UTC")))
})

test_that("gli avvisi di validazione vengono mostrati e gli errori interrompono", {
  senza_facoltative <- scrivi_csv(c(
    intestazione_csv,
    "2025-09-15 08:30:00,AB123CD,VEH001,R1,Presente,CARTA,NA,UTZ001,41.9,12.5"
  ))
  expect_message(
    analisi <- genera_analisi_cluster(senza_facoltative),
    "Colonne facoltative assenti"
  )
  expect_equal(nrow(analisi), 1)
  expect_error(
    genera_analisi_cluster(scrivi_csv(c("a,b", "1,2"))),
    class = "errore_validazione"
  )
})

test_that("il salvataggio usa il nome con le date del periodo", {
  cartella <- withr::local_tempdir()
  analisi <- genera_analisi_cluster(
    percorso_dataset_esempio(),
    data_inizio = as.Date("2025-03-15"), data_fine = as.Date("2025-09-30")
  )
  file <- scrivi_analisi_cluster(analisi, file.path(cartella, "output"))
  expect_identical(basename(file), "cluster_analysis_2025-03-15_2025-09-30.csv")
  riletto <- utils::read.csv(file, stringsAsFactors = FALSE)
  expect_equal(nrow(riletto), nrow(analisi))
  expect_identical(names(riletto), colonne_analisi_cluster())

  vuota <- genera_analisi_cluster(percorso_dataset_esempio(), "2030-01-01", "2030-12-31")
  expect_error(scrivi_analisi_cluster(vuota, cartella), "nessuna lettura nel periodo")
})

test_that("lo script eseguibile produce il file dalla riga di comando", {
  skip_on_cran()
  script <- app_sys("scripts", "generate_cluster_analysis.R")
  expect_true(file.exists(script))
  cartella <- withr::local_tempdir()
  uscita <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c(
      shQuote(script), shQuote(percorso_dataset_esempio()),
      "2025-01-01", "2025-12-31", shQuote(cartella)
    ),
    stdout = TRUE, stderr = TRUE
  ))
  file <- file.path(cartella, "cluster_analysis_2025-01-01_2025-12-31.csv")
  expect_true(file.exists(file), info = paste(uscita, collapse = "\n"))
  expect_equal(nrow(utils::read.csv(file)), nrow(analisi_esempio()))
})
