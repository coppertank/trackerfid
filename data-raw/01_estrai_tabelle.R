# Passo 1 di 3: estrae le tabelle dal database aziendale.
#
# Scrive un CSV per tabella in data-raw/origine/. Quei file sono la copia
# grezza dei dati: non vanno modificati a mano e non entrano nel repository.
# Eseguire dalla radice del progetto. Servono i pacchetti odbc e DBI e la
# sorgente dati ODBC "DenodoODBC2" configurata sul computer.

library(odbc)
library(DBI)
library(tidyverse)


con <- dbConnect(
  odbc::odbc(),
  dsn = "DenodoODBC2"
)


tabelle <- c(
  "iv_estrazione_giri_servizi_rifiuti_filtered",
  "dv_cnt_associazione_contenitori",
  "dv_cnt_letture",
  "dv_cnt_tag_contenitori",
  "dv_cnt_gruppi_sacchetti",
  "dv_cnt_gruppi_sacchetti_servizi"
)


for (target_label in tabelle) {
  # Applica il filtro se la tabella è "dv_cnt_letture", altrimenti query standard
  if (target_label == "dv_cnt_letture") {
    query_sql <- sprintf(
      'SELECT * FROM "%s" WHERE data_lettura > \'2026-01-01\'',
      target_label
    )
  } else {
    query_sql <- sprintf('SELECT * FROM "%s"', target_label)
  }

  df_temp <- dbGetQuery(con, query_sql)

  output_path <- paste0("data-raw/origine/", target_label, ".csv")

  write.csv(
    df_temp,
    file = output_path,
    row.names = FALSE
  )

  assign(target_label, df_temp, envir = .GlobalEnv)
  rm(df_temp)
}


dbDisconnect(con)
