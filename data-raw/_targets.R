# Pipeline della preparazione dei dati reali, con {targets}.
#
# Dai file estratti dal passo 1 (data-raw/origine/) scrive in data-raw/output/:
# letture_app.csv, letture_app.duckdb e controllo_giri.csv per l'app;
# letture_storiche.csv, le letture delle saponette con la stessa anagrafica.
# I report non fanno parte della pipeline: si compilano a mano con
# data-raw/compila_report.R.
# I due dataset si leggono anche con targets::tar_load(letture_app) e
# targets::tar_load(letture_storiche); le tabelle intermedie non si salvano.
#
# Dalla radice del progetto:
#   targets::tar_make()                      tutto quello che è da rifare
#   targets::tar_make(names = duckdb_letture_app)   solo i file dell'app
#   targets::tar_outdated()                  che cosa è da rifare
#   targets::tar_visnetwork()                la catena dei passi
# Il passo 1, data-raw/01_estrai_tabelle.R, si collega al database aziendale
# e va eseguito a mano.

library(targets)

# Le funzioni del pacchetto, dai sorgenti: cambiarne una rifà i target che la
# usano.
pkgload::load_all(".", quiet = TRUE)
tar_option_set(imports = "trackerfid")

# Nomi delle colonne del file delle letture storiche: codice, istante e
# trasmissione.
colonne_storiche <- list(
  rfid = "codice_transponder",
  data_ora = "data_lettura",
  trasmissione = "id_trasmissione"
)

list(
  # --- File di partenza: la pipeline ne segue il contenuto ------------------
  # Antenne: test dei mezzi, letture e calendari. Il file dei giri non
  # pianificati può mancare: si seguono quelli che ci sono.
  tar_target(
    file_antenne,
    file_origine()[c(
      "test_mezzi",
      "letture",
      "calendario",
      "non_pianificati"
    )] |>
      unlist() |>
      purrr::keep(file.exists),
    format = "file"
  ),
  # Anagrafica, comune ai due dataset: contenitori, sacchetti, nuclei, manovre.
  tar_target(
    file_anagrafica,
    unlist(file_origine()[c(
      "contenitori",
      "gruppi_sacchetti",
      "servizi_sacchetti",
      "componenti",
      "manovre"
    )]),
    format = "file"
  ),
  tar_target(file_storiche, file_origine()$storiche, format = "file"),

  # --- Letture per l'app: dataset, CSV, archivio .duckdb, giri da controllare
  tar_target(
    letture_app,
    {
      file_antenne
      file_anagrafica
      prepara_letture_app(file_origine())
    }
  ),
  tar_target(
    csv_letture_app,
    scrivi_csv_output(letture_app, "letture_app.csv"),
    format = "file"
  ),
  # L'archivio dell'app: lo stesso CSV, validato come lo valida l'app.
  tar_target(
    duckdb_letture_app,
    scrivi_archivio(csv_letture_app, "data-raw/output/letture_app.duckdb"),
    format = "file"
  ),
  tar_target(
    csv_controllo_giri,
    scrivi_csv_output(giri_da_controllare(letture_app), "controllo_giri.csv", na = ""),
    format = "file"
  ),

  # Le letture il cui comune del GPS è stato sostituito da quello della posizione.
  tar_target(
    csv_comuni_corretti,
    scrivi_csv_output(comuni_corretti(letture_app), "comuni_corretti.csv", na = ""),
    format = "file"
  ),

  # --- Letture storiche, con la stessa anagrafica -----------------------------
  tar_target(
    letture_storiche,
    {
      file_storiche
      file_anagrafica
      prepara_letture_storiche(
        file_origine(),
        colonna_rfid = colonne_storiche$rfid,
        colonna_data_ora = colonne_storiche$data_ora,
        colonna_trasmissione = colonne_storiche$trasmissione
      )
    }
  ),
  tar_target(
    csv_letture_storiche,
    scrivi_csv_output(letture_storiche, "letture_storiche.csv"),
    format = "file"
  )
)
