# Pipeline della preparazione dei dati reali, con {targets}.
#
# Dai file estratti dal passo 1 (data-raw/origine/) scrive in data-raw/output/:
# letture_antenne.csv, letture_antenne.duckdb e controllo_giri.csv per l'app;
# letture_saponette.csv, le letture delle saponette con la stessa anagrafica;
# letture_scartate_antenne.csv e letture_scartate_saponette.csv, le letture
# scartate dalla pulizia, con il motivo.
# I report non fanno parte della pipeline: si compilano a mano con
# data-raw/compila_report.R.
# I due dataset si leggono anche con targets::tar_load(letture_antenne) e
# targets::tar_load(letture_saponette); le tabelle intermedie non si salvano.
#
# Dalla radice del progetto:
#   targets::tar_make()                      tutto quello che è da rifare
#   targets::tar_make(names = duckdb_letture_antenne)   solo i file dell'app
#   targets::tar_outdated()                  che cosa è da rifare
#   targets::tar_visnetwork()                la catena dei passi
# Il passo 1, data-raw/01_estrai_tabelle.R, si collega al database aziendale
# e va eseguito a mano.

library(targets)

# Le funzioni del pacchetto, dai sorgenti: cambiarne una rifà i target che la
# usano.
pkgload::load_all(".", quiet = TRUE)
tar_option_set(imports = "trackerfid")

list(
  # --- File di partenza: la pipeline ne segue il contenuto ------------------
  # Antenne: test dei mezzi, letture e calendari. I file dei giri non
  # pianificati e dei giri aggiunti a mano possono mancare: si seguono quelli
  # che ci sono.
  tar_target(
    file_antenne,
    file_origine()[c(
      "test_mezzi",
      "antenne_grezze",
      "calendario",
      "non_pianificati",
      "giri_aggiunti"
    )] |>
      unlist() |>
      purrr::keep(file.exists),
    format = "file"
  ),
  # Anagrafica, comune ai due dataset: contenitori, tag, sacchetti, nuclei, manovre.
  tar_target(
    file_anagrafica,
    unlist(file_origine()[c(
      "contenitori",
      "tag_contenitori",
      "gruppi_sacchetti",
      "servizi_sacchetti",
      "componenti",
      "manovre"
    )]),
    format = "file"
  ),
  tar_target(
    file_saponette,
    file_origine()$saponette_grezze,
    format = "file"
  ),

  # --- Letture per l'app: dataset, CSV, archivio .duckdb, giri da controllare
  tar_target(
    letture_antenne,
    {
      file_antenne
      file_anagrafica
      prepara_letture_antenne(file_origine())
    }
  ),
  tar_target(
    csv_letture_antenne,
    scrivi_csv_output(letture_antenne, "letture_antenne.csv"),
    format = "file"
  ),
  # L'archivio dell'app: lo stesso CSV, validato come lo valida l'app.
  tar_target(
    duckdb_letture_antenne,
    scrivi_archivio(csv_letture_antenne, "data-raw/output/letture_antenne.duckdb"),
    format = "file"
  ),
  tar_target(
    csv_controllo_giri,
    scrivi_csv_output(giri_da_controllare(letture_antenne), "controllo_giri.csv", na = ""),
    format = "file"
  ),

  # Le letture scartate dalla pulizia, con il motivo: servono al report degli
  # indicatori per contare quante letture ha scartato ogni sistema.
  tar_target(
    csv_scartate_antenne,
    scrivi_csv_output(
      letture_scartate(letture_antenne),
      "letture_scartate_antenne.csv",
      na = ""
    ),
    format = "file"
  ),

  # Le letture il cui comune del GPS è stato sostituito da quello della posizione.
  tar_target(
    csv_comuni_corretti,
    scrivi_csv_output(comuni_corretti(letture_antenne), "comuni_corretti.csv", na = ""),
    format = "file"
  ),

  # --- Letture delle saponette, con la stessa anagrafica -----------------------------
  tar_target(
    letture_saponette,
    {
      file_saponette
      file_anagrafica
      prepara_letture_saponette(file_origine())
    }
  ),
  tar_target(
    csv_letture_saponette,
    scrivi_csv_output(letture_saponette, "letture_saponette.csv"),
    format = "file"
  ),
  tar_target(
    csv_scartate_saponette,
    scrivi_csv_output(
      letture_scartate(letture_saponette),
      "letture_scartate_saponette.csv",
      na = ""
    ),
    format = "file"
  )
)
