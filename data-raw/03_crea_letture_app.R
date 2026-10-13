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


# Dal calendario vengono il giro, `servizio_giro`, e il comune del giro,
# `comune_giro`. Il servizio atteso, `servizio_atteso`, e' quello del
# contenitore a database; dove manca e' il giro, con il nome del database:
# "CARTA/CARTONE PAP" e "CARTA CONT.STRADALI" diventano "CARTA".
# `origine_servizio` dice quale dei due vale: "Database" o "Calendario". Il
# servizio del database non ha piu' una colonna sua. Il comune di lettura,
# `comune_lettura`, viene dalla posizione di ogni lettura, non dal
# calendario: fuori dai comuni serviti vale "FUORI SERVIZIO AZIENDA".
servizio_atteso <- associa_servizio_atteso_da_calendario(
  servizi_associati,
  calendario_servizio_rifiuti
)

# Il calendario dice che cosa un mezzo doveva fare, non che cosa ha fatto: se
# un mezzo ne sostituisce un altro, il servizio del calendario e' sbagliato.
# Per i non censiti il servizio atteso viene quindi dai censiti letti dallo
# stesso mezzo nei 30 minuti prima e dopo: se sono almeno 5 e almeno l'80% ha
# lo stesso servizio vale quello, con origine "Censiti", altrimenti resta il
# calendario.
# `servizio_giro` resta il giro del calendario, con il suo nome: "SECCO PAP",
# non "SECCO". `coerenza` dice se i servizi della lettura concordano:
# "Coerente" o "Non coerente".
servizio_atteso <- associa_servizio_atteso_dai_censiti(
  servizio_atteso,
  minuti = 30,
  minimo_censiti = 5,
  soglia = 0.8
)


letture_app <- servizio_atteso |>
  dplyr::filter(
    !stringr::str_starts(RFID, "00BD"),
    !stringr::str_starts(RFID, "0000000000")
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
