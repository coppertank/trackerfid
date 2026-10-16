# Passo 2 di 3: carica le tabelle ripulite.
#
# Ogni sezione legge una tabella già ripulita da data-raw/intermedi/. Il codice
# commentato sotto ogni lettura la ricostruisce dai file grezzi di
# data-raw/origine/ e la salva di nuovo: va eseguito dopo una nuova estrazione.
# Eseguire dalla radice del progetto.

library(tidyverse)


# TEST MEZZI -------------------------------------------------------------

test_mezzi <- readRDS(file = "data-raw/intermedi/test_mezzi.rds")


# test_mezzi <- read_delim(
#   "data-raw/origine/AppSheet.ViewData.2026-10-13.csv",
#   delim = ';',
#   col_types = cols(.default = col_character()),
#   na = c("", "NA", "-")
# ) |>
#   mutate(
#     `ULTIMA DATA VERBALE SEZIONE B` = str_replace_all(
#       `ULTIMA DATA VERBALE SEZIONE B`,
#       "\\.",
#       ":"
#     ),
#     data_test = as_date(ymd_hm(`ULTIMA DATA VERBALE SEZIONE B`))
#   ) |>
#   rename(
#     esito = `ESITO TEST SEZIONE B`,
#     targa_veicolo = "TARGA_VEICOLO",
#     matricola_veicolo = "MATRICOLA_VEICOLO"
#   ) |>
#   select(targa_veicolo, matricola_veicolo, esito, data_test)

# saveRDS(test_mezzi, file = "data-raw/intermedi/test_mezzi.rds")

# COMUNI - CANTIERI ------------------------------------------------------

# comuni_cantieri <- read_csv("inst/extdata/comuni_cantieri.csv", col_types = cols()) |>
#   select(comune, cantiere)

# LETTURE ----------------------------------------------------------------

letture <- readRDS(file = "data-raw/intermedi/letture.rds")


# letture <- read_delim(
#   "data-raw/origine/letture_antenne.csv",
#   delim = ';',
#   col_types = cols(.default = col_character())
# ) |>
#   separate(
#     col = IndirizzoLocalizzazione,
#     into = c("indirizzo_lettura", "comune_lettura"),
#     sep = ",\\s*",
#     remove = FALSE
#   ) |> # remove = FALSE conserva la colonna originale
#   mutate(
#     giorno = as_date(dmy_hms(DataeOraPrimaLettura)),
#     DataeOraPrimaLettura = as.POSIXct(
#       DataeOraPrimaLettura,
#       format = "%d/%m/%Y %H.%M.%S"
#     ),
#     Latitudine = as.numeric(stringr::str_replace_all(
#       .data$Latitudine,
#       ",",
#       "."
#     )),
#     Longitudine = as.numeric(stringr::str_replace_all(
#       .data$Longitudine,
#       ",",
#       "."
#     )),
#     comune_lettura = toupper(comune_lettura)
#   ) |>
#   rename(
#     giorno_lettura = DataeOraPrimaLettura,
#     RFID = "RFID(EPC)",
#     targa_veicolo = Targa,
#     latitudine = Latitudine,
#     longitudine = Longitudine
#   ) |>
#   select(-c(IndirizzoLocalizzazione, indirizzo_lettura)) |>
#   filter(latitudine != 0, comune_lettura != "") |>
#   left_join(comuni_cantieri, by = c("comune_lettura" = "comune"))

# saveRDS(letture, file = "data-raw/intermedi/letture.rds")

# ASSOCIAZIONE CONTENITORI -----------------------------------------------

associazione_contenitori <- readRDS(
  file = "data-raw/intermedi/associazione_contenitori.rds"
)


# associazione_contenitori <- read.csv(
#   "data-raw/origine/dv_cnt_associazione_contenitori.csv",
#   stringsAsFactors = FALSE,
#   na.strings = c("", "NA", "NaN", "null", "NULL")
# ) |>
#   filter(!is.na(codice_transponder)) |>
#   select(
#     codice_transponder,
#     descrizione_rifiuto,
#     volume,
#     frequenza,
#     numero_raccolte_annue,
#     data_attivazione_contenitore,
#     data_cessazione_contenitore,
#     stato_servizio,
#     id_utenza,
#     comune_servizio,
#     codice_categoria
#   ) |>
#   mutate(
#     data_attivazione_contenitore = as_date(data_attivazione_contenitore),
#     data_cessazione_contenitore = if_else(
#       is.na(data_cessazione_contenitore),
#       as_date("2099-12-31"),
#       as_date(data_cessazione_contenitore)
#     ),
#     comune_servizio = case_when(
#       comune_servizio == "LUSINA CONCO" ~ "LUSIANA CONCO",
#       comune_servizio == "VALSTAGNA" ~ "VALBRENTA",
#       TRUE ~ comune_servizio # Mantiene il valore originale per tutti gli altri
#     ),
#     codice_categoria = as.factor(codice_categoria)
#   ) |>
#   rename(servizio_transponder = descrizione_rifiuto)

# saveRDS(associazione_contenitori, file = "data-raw/intermedi/associazione_contenitori.rds")

# TAG CONTENITORI --------------------------------------------------------

tag_contenitori <- readRDS(
  file = "data-raw/intermedi/tag_contenitori.rds"
)


# tag_contenitori <- read.csv(
#   "data-raw/origine/dv_cnt_tag_contenitori.csv",
#   stringsAsFactors = FALSE,
#   na.strings = c("", "NA", "NaN", "null", "NULL")
# ) |>
#   mutate(codice_transponder = coalesce(codice_transponder, codice_etichetta)) |>
#   select(codice_transponder)

# saveRDS(tag_contenitori, file = "data-raw/intermedi/tag_contenitori.rds")

# ASSOCIAZIONE SACCHETTI -------------------------------------------------

associazione_sacchetti <- readRDS(
  file = "data-raw/intermedi/associazione_sacchetti.rds"
)


# gruppi_sacchetti <- read.csv(
#   "data-raw/origine/dv_cnt_gruppi_sacchetti.csv",
#   stringsAsFactors = FALSE,
#   na.strings = c("", "NA", "NaN", "null", "NULL")
# ) |>
#   select(codice_uhf, codice_gruppo_sacchetti)

# associazione_gruppi_sacchetti <- read.csv(
#   "data-raw/origine/dv_cnt_gruppi_sacchetti_servizi.csv",
#   stringsAsFactors = FALSE,
#   na.strings = c("", "NA", "NaN", "null", "NULL")
# ) |>
#   mutate(id_servizio = as.character(id_servizio)) |>
#   select(codice_gruppo_sacchetti, id_servizio, ultima_variazione) |>
#   mutate(ultima_variazione = as.POSIXct(ultima_variazione)) |>
#   group_by(codice_gruppo_sacchetti) |>
#   # Prende la riga con la data maggiore per ciascun 'codice_gruppo'
#   # with_ties = FALSE evita duplicati se ci sono due record con la stessa identica data/ora
#   slice_max(order_by = ultima_variazione, n = 1, with_ties = FALSE) |>
#   ungroup()

# associazione_sacchetti <- gruppi_sacchetti |>
#   left_join(
#     associazione_gruppi_sacchetti,
#     by = "codice_gruppo_sacchetti"
#   ) |>
#   select(
#     codice_uhf,
#     id_servizio
#   )
# # 1. Salvare il dataframe in un file RDS
# saveRDS(associazione_sacchetti, file = "data-raw/intermedi/associazione_sacchetti.rds")

# CALENDARIO SERVIZIO RIFIUTI --------------------------------------------

calendario_servizio_rifiuti <- readRDS(
  file = "data-raw/intermedi/calendario_servizio_rifiuti.rds"
)

# calendario_servizio_rifiuti <- read_delim(
#   "data-raw/origine/iv_estrazione_giri_servizi_rifiuti_filtered.csv",
#   col_types = cols(.default = col_character()),
#   na = c("", "NA", "NaN", "null", "NULL")
# ) |>
#   filter(!is.na(matricola_mezzo)) |>
#   mutate(
#     # togli le lettere iniziali dai valori della matricola del mezzo
#     matricola_mezzo = str_remove(matricola_mezzo, "^[A-Za-z]"),
#     giorno = as_date(giorno),
#     comune = dplyr::case_when(
#       comune == "ROSA'" ~ "ROSÀ",
#       .default = comune
#     )
#   ) |>
#   rename(comune_giro = comune)

# saveRDS(calendario_servizio_rifiuti, file = "data-raw/intermedi/calendario_servizio_rifiuti.rds")
