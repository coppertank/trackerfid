# Passo 4, facoltativo: prepara le letture storiche, cioè quelle fatte prima
# dell'installazione delle antenne sui mezzi, per i report di confronto con le
# letture delle antenne. L'app non le usa.
#
# Legge il file delle letture storiche in data-raw/origine/ e scrive
# data-raw/output/letture_storiche.csv, con due colonne, RFID e
# giorno_lettura, e i codici RFID nello stesso formato del passo 3.
# Eseguire dalla radice del progetto.
#
# Da adattare al file di origine: il nome del file e i nomi delle due colonne.

devtools::load_all()

file_origine <- "data-raw/origine/dv_cnt_letture.csv"
colonna_rfid <- "codice_transponder"
colonna_data <- "data_lettura"


origine <- read_csv_auto(file_origine)

letture_storiche <- dplyr::tibble(
  RFID = origine[[colonna_rfid]],
  giorno_lettura = origine[[colonna_data]]
) |>
  dplyr::filter(!is.na(RFID), !is.na(giorno_lettura)) |>
  # Stesso formato dei codici usato per le letture con le antenne: senza
  # questo passaggio gli RFID dei due file non si ritrovano.
  formatta_rfid() |>
  # Stessa esclusione del passo 3.
  dplyr::filter(!stringr::str_starts(RFID, "00BD")) |>
  dplyr::distinct()


write.csv(
  letture_storiche,
  "data-raw/output/letture_storiche.csv",
  row.names = FALSE
)


# Controllo: se pochi RFID delle antenne compaiono nello storico, è probabile
# che i codici dei due file abbiano formati diversi.
if (file.exists("data-raw/output/letture_app.csv")) {
  rfid_antenne <- unique(read_csv_auto("data-raw/output/letture_app.csv")$RFID)
  message(sprintf(
    "RFID delle antenne presenti anche nello storico: %d su %d",
    sum(rfid_antenne %in% letture_storiche$RFID),
    length(rfid_antenne)
  ))
}
