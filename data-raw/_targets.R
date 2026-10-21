# Pipeline della preparazione dei dati reali, con {targets}.
#
# Dai file estratti dal passo 1 (data-raw/origine/) scrive in data-raw/output/:
# letture_app.csv, letture_app.duckdb e controllo_giri.csv per l'app;
# letture_storiche.csv, le letture delle saponette con la stessa anagrafica;
# il report sui conferimenti del secco.
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
  giorno = "giorno",
  trasmissione = "id_trasmissione"
)

# Periodo del report sui conferimenti, "AAAA-MM-GG". Vuoti: dal primo gennaio
# dell'anno dell'ultima lettura all'ultima lettura.
periodo_report <- list(dal = "", al = "")

# Compila il report sui conferimenti e ne restituisce l'HTML e i tre CSV. I
# percorsi diventano assoluti prima di render(), che si sposta in docs/.
compila_report_conferimenti <- function(
  rmd,
  letture,
  periodo,
  cartella = "data-raw/output"
) {
  cartella <- normalizePath(cartella)
  nome <- "conferimenti_secco.html"
  rmarkdown::render(
    rmd,
    params = list(
      letture = normalizePath(letture),
      dal = periodo$dal,
      al = periodo$al,
      cartella_csv = cartella
    ),
    output_file = nome,
    output_dir = cartella,
    quiet = TRUE
  )
  c(
    file.path(cartella, nome),
    list.files(cartella, pattern = "^conferimenti_.*\\.csv$", full.names = TRUE)
  )
}

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
  tar_target(file_report, "docs/conferimenti_secco.Rmd", format = "file"),

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

  # --- Letture storiche, con la stessa anagrafica -----------------------------
  tar_target(
    letture_storiche,
    {
      file_storiche
      file_anagrafica
      prepara_letture_storiche(
        file_origine(),
        colonna_rfid = colonne_storiche$rfid,
        colonna_giorno = colonne_storiche$giorno,
        colonna_trasmissione = colonne_storiche$trasmissione
      )
    }
  ),
  tar_target(
    csv_letture_storiche,
    scrivi_csv_output(letture_storiche, "letture_storiche.csv"),
    format = "file"
  ),

  # --- Report sui conferimenti del secco ------------------------------------
  tar_target(
    report_conferimenti,
    compila_report_conferimenti(file_report, csv_letture_app, periodo_report),
    format = "file"
  )
)
