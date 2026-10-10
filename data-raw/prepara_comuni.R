# Aggiorna i confini dei comuni serviti:
#   inst/extdata/comuni_confini.geojson    un confine per ogni comune della tabella
#
# La tabella dei comuni, inst/extdata/comuni_cantieri.csv, e' compilata a mano:
# una riga per comune, con il cantiere e le coordinate del centro abitato. Per
# aggiungere un comune si scrive la sua riga, poi si riesegue questo script.
#
# I confini vengono dall'ultimo rilascio della raccolta guglielmo/geojson-italy,
# che ripubblica quelli dell'ISTAT con licenza CC-BY 4.0. Li scarica e li
# sceglie la funzione scarica_confini_comuni(), in R/fct_confini_comuni.R. La
# funzione si ferma, senza scrivere niente, se anche un solo comune della
# tabella non trova il suo confine o se il centro abitato cade fuori dal
# confine trovato: i nomi si confrontano senza badare a maiuscole, accenti e
# apostrofi.
#
# Serve la connessione a internet. Eseguire dalla radice del pacchetto:
#   Rscript data-raw/prepara_comuni.R
#
# Dopo aver cambiato i confini va rigenerato anche il dataset di esempio, che
# mette i contenitori dentro il confine del loro comune:
#   Rscript data-raw/genera_sample_dataset.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))

comuni <- comuni_cantieri()
senza_centro <- comuni$comune[is.na(comuni$latitudine) | is.na(comuni$longitudine)]
if (length(senza_centro) > 0) {
  stop(
    "Mancano le coordinate del centro abitato di: ",
    paste(senza_centro, collapse = ", "),
    ". Vanno scritte nella tabella dei comuni."
  )
}

scarica_confini_comuni()
