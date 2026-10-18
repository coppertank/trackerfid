# Aggiorna i confini dei comuni serviti:
#   inst/extdata/comuni_confini.geojson    un confine per ogni comune della tabella
#
# La tabella dei comuni, inst/extdata/comuni_cantieri.csv, e' compilata a mano:
# una riga per comune, con il cantiere e le coordinate del centro abitato. Per
# aggiungere un comune si scrive la sua riga, poi si riesegue questo script.
#
# I confini vengono dallo strato dei comuni del geoportale della Regione del
# Veneto (licenza IODL 2.0), scaricato come shapefile nella cartella
# data-raw/rvccomuni/. La richiesta usata per scaricarlo sta in
# data-raw/rvccomuni/wfsrequest.txt. Quando la Regione pubblica confini nuovi
# basta sostituire il contenuto della cartella, con un solo file .shp, e
# rieseguire lo script: il nome del file .shp non conta.
#
# Le funzioni stanno in R/fct_preparazione_comuni.R. Lo script si ferma, senza
# scrivere niente, se anche un solo comune della tabella non trova il suo
# confine o se il centro abitato cade fuori dal confine trovato: i nomi si
# confrontano senza badare a maiuscole, accenti e apostrofi, quindi ROSA' della
# Regione trova ROSÀ della tabella.
#
# Serve il pacchetto sf. Eseguire dalla radice del pacchetto:
#   Rscript data-raw/prepara_comuni.R
#
# Dopo aver cambiato i confini va rigenerato anche il dataset di esempio, che
# mette i contenitori dentro il confine del loro comune, e con lui l'analisi
# dei cluster di esempio:
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

prepara_confini_comuni(cartella = file.path("data-raw", "rvccomuni"))
