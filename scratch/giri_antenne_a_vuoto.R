# SCRATCH: script usa e getta, la cartella si puo' eliminare quando non serve piu'.
#
# Giri pianificati dei mezzi con l'antenna che sono andati "a vuoto": ogni giorno
# in cui un mezzo aveva un servizio da compiere (calendario dei soli giri
# PIANIFICATI) si guarda se dal file delle letture delle sole ANTENNE risulta che
# quel mezzo ha letto almeno un codice RFID. I giorni con zero letture sono quelli
# in cui l'antenna non ha letto o trasmesso niente.
#
# Dalla radice del progetto:
#   source("scratch/giri_antenne_a_vuoto.R")
#
# Parametri (si possono definire prima di eseguire lo script, altrimenti valgono
# i predefiniti):
#   cartella_origine   dove stanno i file estratti          ("data-raw/origine")
#   cartella_output    dove scrivere i CSV                  ("data-raw/output")
#   dal, al            primo e ultimo giorno da guardare     (NULL: tutti i giorni)
#
# Che cosa fa, nell'ordine:
#  1. mezzi con l'antenna = quelli del file dei test (AppSheet.ViewData.csv,
#     colonna MATRICOLA_VEICOLO), con targa, esito e data del collaudo;
#  2. calendario dei giri pianificati: matricola_mezzo pulita (via la lettera
#     iniziale, `X1601` diventa `1601`; poi via gli zeri iniziali e gli spazi,
#     maiuscole) e solo i mezzi del punto 1;
#  3. letture delle antenne: il file GREZZO (nessuna lettura scartata, quindi
#     anche quelle senza posizione, doppioni compresi), assegnate al mezzo
#     tramite la targa del file dei test;
#  4. per ogni mezzo e giorno con un giro: servizi, comuni, quante letture ha fatto
#     l'antenna quel giorno e quanti RFID distinti.
#
# Attenzione al collaudo: un'antenna non legge prima di essere collaudata. La
# colonna `antenna_attiva` e' VERO solo per un test POSITIVO e un giorno dopo la
# data del test (come fa la pipeline, `filtra_letture_dopo_il_collaudo()`): i giri
# a vuoto veri sono quelli con `a_vuoto` e `antenna_attiva` insieme.
#
# Scrive in cartella_output (fuori da git, sono dati aziendali):
#   giri_antenne_per_giorno.csv    tutti i giorni con un giro, con le letture
#   giri_antenne_a_vuoto.csv       solo i giorni a vuoto con l'antenna attiva
#   giri_antenne_riepilogo.csv     un riepilogo per mezzo

if (!exists("cartella_origine")) cartella_origine <- "data-raw/origine"
if (!exists("cartella_output")) cartella_output <- "data-raw/output"
if (!exists("dal")) dal <- NULL
if (!exists("al")) al <- NULL

if (!file.exists("DESCRIPTION")) {
  stop("Esegui dalla radice del progetto trackerfid.", call. = FALSE)
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))
library(dplyr)

file <- file_origine(cartella_origine)
mancanti <- unlist(file[c("test_mezzi", "calendario", "antenne_grezze")])
mancanti <- mancanti[!file.exists(mancanti)]
if (length(mancanti) > 0) {
  stop("File non trovati: ", paste(mancanti, collapse = ", "), call. = FALSE)
}

# ---- Funzioni di pulizia -------------------------------------------------------
# Matricola: senza lettera iniziale, zeri iniziali e spazi, in maiuscolo. Le due
# tabelle si confrontano solo dopo questa pulizia.
pulisci_matricola <- function(x) {
  chiave_matricola(stringr::str_remove(trimws(as.character(x)), "^[A-Za-z]"))
}
# Targa: solo lettere e cifre, in maiuscolo (il file dei test e quello delle letture
# possono scriverla con spazi o trattini diversi).
pulisci_targa <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
}
# Prima data di un vettore, mancante se non ce n'e' (senza avvisi).
prima_data <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) > 0) min(x) else as.Date(NA)
}
# Ora (HH:MM) del primo o dell'ultimo istante, mancante se non ce n'e'.
ora_del <- function(x, quale = min) {
  x <- x[!is.na(x)]
  if (length(x) > 0) format(quale(x), "%H:%M") else NA_character_
}

# ---- 1. Mezzi con l'antenna: il file dei test -----------------------------------
test <- importa_test_mezzi(file$test_mezzi) |>
  mutate(
    matricola_mezzo = pulisci_matricola(matricola_veicolo),
    targa = pulisci_targa(targa_veicolo)
  ) |>
  filter(!is.na(matricola_mezzo), nzchar(matricola_mezzo))

# Un mezzo puo' avere piu' verbali: e' "positivo" se ne ha uno positivo, e il suo
# collaudo e' il primo test positivo.
mezzi <- test |>
  summarise(
    targhe = paste(sort(unique(targa)), collapse = ", "),
    esito = if (any(esito %in% "POSITIVO")) {
      "POSITIVO"
    } else {
      na_if(paste(sort(unique(na.omit(esito))), collapse = ", "), "")
    },
    data_collaudo = prima_data(data_test[esito %in% "POSITIVO"]),
    .by = matricola_mezzo
  )
message("Mezzi nel file dei test: ", nrow(mezzi), " (", sum(mezzi$esito %in% "POSITIVO"), " con test positivo)")

# La targa deve indicare un solo mezzo, altrimenti le letture si duplicano.
targhe <- test |> distinct(targa, matricola_mezzo)
if (anyDuplicated(targhe$targa) > 0) {
  warning(
    "Queste targhe sono associate a piu' di una matricola nel file dei test: ",
    paste(unique(targhe$targa[duplicated(targhe$targa)]), collapse = ", "),
    call. = FALSE
  )
}

# ---- 2. Calendario dei soli giri pianificati ------------------------------------
calendario <- importa_calendario(file$calendario) |>
  mutate(
    matricola_mezzo = pulisci_matricola(matricola_mezzo),
    data_giro = as.Date(data_giro)
  ) |>
  filter(!is.na(data_giro), !is.na(matricola_mezzo), nzchar(matricola_mezzo))
if (!is.null(dal)) calendario <- filter(calendario, data_giro >= as.Date(dal))
if (!is.null(al)) calendario <- filter(calendario, data_giro <= as.Date(al))
message("Giri pianificati nel periodo: ", nrow(calendario), " di ", n_distinct(calendario$matricola_mezzo), " mezzi")
calendario <- filter(calendario, matricola_mezzo %in% mezzi$matricola_mezzo)
message("Giri pianificati dei mezzi con l'antenna: ", nrow(calendario), " di ", n_distinct(calendario$matricola_mezzo), " mezzi")

senza_giri <- setdiff(mezzi$matricola_mezzo, calendario$matricola_mezzo)
if (length(senza_giri) > 0) {
  message(
    "Mezzi del file dei test senza nessun giro pianificato (controllare la matricola): ",
    paste(sort(senza_giri), collapse = ", ")
  )
}

# Un riga per mezzo e giorno: i servizi si elencano tutti, anche se sono piu' di uno.
giorni <- calendario |>
  summarise(
    servizi = paste(sort(unique(descrizione_servizio)), collapse = " | "),
    comuni = paste(sort(unique(na.omit(comune_giro))), collapse = " | "),
    giri = n_distinct(id_giro, descrizione_servizio, comune_giro),
    eseguito = paste(sort(unique(na.omit(eseguito))), collapse = "/"),
    .by = c(matricola_mezzo, data_giro)
  )

# ---- 3. Letture delle antenne (file grezzo) -------------------------------------
letture <- importa_letture_antenne(file$antenne_grezze) |>
  mutate(targa = pulisci_targa(targa_veicolo))
message("Righe del file delle antenne: ", nrow(letture))
if (any(is.na(letture$data_lettura))) {
  warning(
    sum(is.na(letture$data_lettura)),
    " righe delle antenne hanno una data illeggibile e non si possono assegnare a un giorno.",
    call. = FALSE
  )
}
senza_mezzo <- !letture$targa %in% targhe$targa
message("Righe di targhe che non sono nel file dei test (non contano): ", sum(senza_mezzo))
letture <- letture |>
  filter(!senza_mezzo, !is.na(data_lettura)) |>
  inner_join(targhe, by = "targa", relationship = "many-to-many")
if (!is.null(dal)) letture <- filter(letture, data_lettura >= as.Date(dal))
if (!is.null(al)) letture <- filter(letture, data_lettura <= as.Date(al))

letture_per_giorno <- letture |>
  summarise(
    letture = n(),
    rfid_distinti = n_distinct(RFID),
    prima_lettura = ora_del(data_ora_lettura, min),
    ultima_lettura = ora_del(data_ora_lettura, max),
    .by = c(matricola_mezzo, data_lettura)
  )

# ---- 4. Giorni con un giro e letture dell'antenna ---------------------------------
risultato <- giorni |>
  left_join(letture_per_giorno, by = c("matricola_mezzo", "data_giro" = "data_lettura")) |>
  left_join(mezzi, by = "matricola_mezzo") |>
  mutate(
    letture = coalesce(letture, 0L),
    rfid_distinti = coalesce(rfid_distinti, 0L),
    a_vuoto = letture == 0,
    antenna_attiva = (esito %in% "POSITIVO") & ((data_giro > data_collaudo) %in% TRUE)
  ) |>
  arrange(matricola_mezzo, data_giro) |>
  select(
    matricola_mezzo, data_giro, servizi, comuni, giri, eseguito,
    letture, rfid_distinti, prima_lettura, ultima_lettura,
    a_vuoto, antenna_attiva, esito, data_collaudo, targhe
  )

a_vuoto_attivi <- filter(risultato, a_vuoto, antenna_attiva)

riepilogo <- risultato |>
  summarise(
    giorni_con_un_giro = n(),
    giorni_con_letture = sum(!a_vuoto),
    giorni_a_vuoto = sum(a_vuoto),
    giorni_a_vuoto_con_antenna_attiva = sum(a_vuoto & antenna_attiva),
    giorni_con_antenna_attiva = sum(antenna_attiva),
    primo_giro = min(data_giro),
    ultimo_giro = max(data_giro),
    esito = first(esito),
    data_collaudo = first(data_collaudo),
    .by = matricola_mezzo
  ) |>
  arrange(desc(giorni_a_vuoto_con_antenna_attiva), matricola_mezzo)

# ---- Risultati --------------------------------------------------------------------
cat("\n=== Riepilogo per mezzo ===\n")
print(as.data.frame(riepilogo), row.names = FALSE)
cat(
  "\n=== Giorni con un giro pianificato e nessuna lettura dell'antenna, con l'antenna attiva: ",
  nrow(a_vuoto_attivi), " ===\n",
  sep = ""
)
if (nrow(a_vuoto_attivi) > 0) {
  print(
    as.data.frame(select(a_vuoto_attivi, matricola_mezzo, data_giro, servizi, comuni, eseguito)),
    row.names = FALSE
  )
}
nonattivi <- sum(risultato$a_vuoto & !risultato$antenna_attiva)
if (nonattivi > 0) {
  cat(
    "\nAltri ", nonattivi, " giorni senza letture sono di mezzi senza test positivo o prima del collaudo: ",
    "non contano come giri a vuoto (colonna antenna_attiva).\n",
    sep = ""
  )
}

# CSV con il separatore ; e la virgola decimale, come gli altri elenchi del progetto.
scrivi <- function(x, nome) {
  x <- x |>
    mutate(across(where(is.logical), ~ if_else(.x, "SI", "NO")))
  dir.create(cartella_output, recursive = TRUE, showWarnings = FALSE)
  percorso <- file.path(cartella_output, nome)
  utils::write.table(x, percorso, sep = ";", dec = ",", row.names = FALSE, na = "")
  message("Scritto ", percorso, " (", nrow(x), " righe)")
  invisible(percorso)
}
scrivi(risultato, "giri_antenne_per_giorno.csv")
scrivi(a_vuoto_attivi, "giri_antenne_a_vuoto.csv")
scrivi(riepilogo, "giri_antenne_riepilogo.csv")
