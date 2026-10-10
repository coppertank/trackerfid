# Passo 3 di 3: crea il CSV delle letture da caricare nell'app.
#
# Usa le funzioni di R/fct_preparazione_letture.R e le tabelle caricate dal
# passo 2. Scrive data-raw/output/letture_app.csv, con le colonne che l'app
# si aspetta. Eseguire dalla radice del progetto.

devtools::load_all()
source("data-raw/02_importa_tabelle.R")


letture_valide <- formatta_rfid(filtra_letture_post_test(letture, test_mezzi))


servizi_associati <- associa_servizio(
  letture_valide,
  associazione_contenitori,
  tag_contenitori,
  associazione_sacchetti
)


# `colonna_comune` e' il nome della colonna del calendario che riporta il
# comune in cui lavora il giro: da li' viene `comune_lettura`. Se nel calendario
# la colonna ha un altro nome va indicato qui; se non c'e', `comune_lettura`
# manca e il cantiere viene dal solo comune del database.
servizio_atteso <- associa_servizio_atteso_da_calendario(
  servizi_associati,
  calendario_servizio_rifiuti,
  colonna_comune = "comune"
)


letture_app <- servizio_atteso |>
  dplyr::filter(
    !stringr::str_starts(RFID, "00BD")
  )

write.csv(letture_app, "data-raw/output/letture_app.csv", row.names = FALSE)

# Con milioni di letture conviene anche il file compresso: occupa circa un
# decimo e l'app lo legge allo stesso modo. `dateTimeAs = "write.csv"` scrive
# l'ora come la scrive write.csv(), senza convertirla in UTC.
# data.table::fwrite(
#   letture_app,
#   "data-raw/output/letture_app.csv.gz",
#   dateTimeAs = "write.csv"
# )
