# Passo 4, facoltativo: prepara le letture storiche, cioè quelle fatte prima
# dell'installazione delle antenne sui mezzi.
#
# Legge data-raw/origine/letture_storiche.csv e scrive
# data-raw/output/letture_storiche.csv, con le due colonne che l'app e il
# report docs/beneficio_antenne.Rmd si aspettano: RFID e giorno_lettura.
# Eseguire dalla radice del progetto.
#
# Da adattare al file di origine: il nome del file e i nomi delle due colonne.

devtools::load_all()

file_origine <- "data-raw/origine/letture_storiche.csv"
colonna_rfid <- "RFID"
colonna_data <- "giorno_lettura"


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
