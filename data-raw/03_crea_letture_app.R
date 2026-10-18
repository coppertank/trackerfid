# Passo 3: crea il CSV delle letture da caricare nell'app.
#
# Usa le funzioni di R/fct_preparazione_letture.R e le tabelle caricate dal
# passo 2. Scrive data-raw/output/letture_app.csv, con le colonne che l'app
# si aspetta, e data-raw/output/letture_app.duckdb, lo stesso file già
# validato: l'app lo apre in un istante. Eseguire dalla radice del progetto.

devtools::load_all()
source("data-raw/02_importa_tabelle.R")


letture_valide <- formatta_rfid(filtra_letture_post_test(letture, test_mezzi))


servizi_associati <- associa_servizio(
  letture_valide,
  associazione_contenitori,
  associazione_sacchetti
)


servizio_atteso <- associa_servizio_atteso_da_calendario(
  servizi_associati,
  calendario_servizio_rifiuti
)

# Nucleo familiare, categoria e manovra sociale della fornitura in vigore al
# momento della lettura: sette colonne in coda, che l'app non usa e che
# servono all'analisi dei conferimenti del secco (passo 5). Se la tabella
# delle manovre ha più colonne con valori "MANOVRA SOCIALE ...", indicare
# quella giusta con colonna_manovra = "nome".
con_nucleo <- associa_nucleo_e_manovra(
  servizio_atteso,
  associazione_componenti,
  manovra_sociale
)

# RIMUOVE I SACCHETTI ("00BD") ED EVENTUALI RFID NON VALIDI ("0000000000")
letture_app <- con_nucleo |>
  dplyr::filter(
    !stringr::str_starts(RFID, "00BD"),
    !stringr::str_starts(RFID, "0000000000")
  )


write.csv(letture_app, "data-raw/output/letture_app.csv", row.names = FALSE)

# L'archivio DuckDB: il CSV appena scritto, letto e validato come lo legge
# l'app, con gli avvisi della validazione. Caricato nell'app, o indicato in
# run_app(letture = ), si apre senza rileggere né validare le letture.
scrivi_archivio(
  "data-raw/output/letture_app.csv",
  "data-raw/output/letture_app.duckdb"
)

# Con milioni di letture conviene anche il file compresso: occupa circa un
# decimo e l'app lo legge allo stesso modo. dateTimeAs = "write.csv" scrive
# l'ora come la scrive write.csv(), senza convertirla in UTC.
# data.table::fwrite(
#   letture_app,
#   "data-raw/output/letture_app.csv.gz",
#   dateTimeAs = "write.csv"
# )
