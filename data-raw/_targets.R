# Pipeline della preparazione dei dati reali, con {targets}.
#
# Dai file estratti dal passo 1 in data-raw/origine/ scrive in
# data-raw/output/:
#   - letture_app.csv e letture_app.duckdb, da caricare nell'app;
#   - letture_storiche.csv, le letture prima delle antenne, per i report;
#   - il report sui conferimenti del secco, con i suoi tre CSV.
#
# Si esegue dalla radice del progetto:
#   targets::tar_make()                          tutto quello che è da rifare
#   targets::tar_make(names = letture_app)       solo il CSV dell'app
#   targets::tar_outdated()                      che cosa è da rifare
#   targets::tar_visnetwork()                    la catena dei passi
# Il file _targets.yaml, nella radice, dice a {targets} che questo script e
# il suo archivio stanno in data-raw/.
#
# Il passo 1, data-raw/01_estrai_tabelle.R, resta fuori: si collega al
# database aziendale e va eseguito a mano. La pipeline vede da sola quando un
# file di data-raw/origine/ cambia.
#
# Si salvano solo i file finali, non le tabelle intermedie: ogni target
# scrive un file e restituisce il suo percorso. Cambiare una regola rifà
# l'intera preparazione delle letture, che legge di nuovo i file estratti.

library(targets)

# Le funzioni del pacchetto, dai sorgenti. Con `imports` {targets} le
# considera parte della pipeline: cambiarne una rifà i target che la usano.
pkgload::load_all(".", quiet = TRUE)
tar_option_set(imports = "trackerfid")

# Nomi delle due colonne del file delle letture storiche: vedi
# `importa_letture_storiche()`.
colonne_storiche <- list(colonna_rfid = "codice_transponder", colonna_data = "data_lettura")

# Periodo del report sui conferimenti, "AAAA-MM-GG". Vuoti: dal primo gennaio
# dell'anno dell'ultima lettura all'ultima lettura.
periodo_report <- list(dal = "", al = "")

# Compila il report sui conferimenti e ne restituisce i file: l'HTML e i tre
# CSV. I percorsi sono assoluti perché il report si compila dalla sua
# cartella, docs/.
compila_report_conferimenti <- function(rmd, letture, periodo, cartella = "data-raw/output") {
  # Prima di render(), che valuta i parametri dopo essersi spostato in docs/.
  cartella <- normalizePath(cartella)
  parametri <- list(
    letture = normalizePath(letture),
    dal = periodo$dal,
    al = periodo$al,
    cartella_csv = cartella
  )
  nome <- "conferimenti_secco.html"
  rmarkdown::render(
    rmd,
    params = parametri,
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
  # --- File estratti dal passo 1: la pipeline ne segue il contenuto --------
  tar_target(
    file_estratti,
    unlist(file_origine()[setdiff(names(file_origine()), "storiche")]),
    format = "file"
  ),
  tar_target(file_storiche, file_origine()$storiche, format = "file"),
  tar_target(file_report, "docs/conferimenti_secco.Rmd", format = "file"),

  # --- Letture per l'app: CSV e archivio .duckdb ---------------------------
  # Il target dipende dai file estratti: quando cambiano, si rifà.
  tar_target(
    letture_app,
    {
      file_estratti
      scrivi_letture_app(prepara_letture_app(file_origine()))
    },
    format = "file"
  ),

  # --- Letture storiche, per i report ---------------------------------------
  tar_target(
    letture_storiche,
    do.call(
      scrivi_letture_storiche,
      c(list(file = list(storiche = file_storiche)), colonne_storiche)
    ),
    format = "file"
  ),

  # --- Report sui conferimenti del secco ------------------------------------
  tar_target(
    report_conferimenti,
    compila_report_conferimenti(file_report, letture_app[1], periodo_report),
    format = "file"
  )
)
