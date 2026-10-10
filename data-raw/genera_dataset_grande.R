# Genera un dataset sintetico di grandi dimensioni, per misurare le prestazioni
# dell'app senza usare dati aziendali.
#
# Scrive nella cartella indicata:
#   letture_prova.csv            letture con le antenne, nel formato dell'app
#   letture_prova.csv.gz         le stesse letture, compresse
#   letture_prova.parquet        le stesse letture in Parquet (serve nanoparquet)
#   letture_storiche_prova.csv   letture del sistema precedente
#
# Uso, dalla radice del pacchetto:
#   Rscript data-raw/genera_dataset_grande.R <cartella> [letture] [rfid] [storiche]
#
# Predefiniti: 2.000.000 di letture in un anno, 100.000 RFID, 3.000.000 di
# letture storiche. I file occupano qualche centinaio di megabyte: la cartella
# non deve stare sotto git. I dati sono casuali e non hanno casi particolari:
# servono a misurare i tempi, non a provare le funzioni.
#
# Come nei dati reali, ogni lettura ha il giro del mezzo che l'ha fatta e il
# comune di quel giro: `servizio_atteso` e `comune_lettura` non mancano mai.

argomenti <- commandArgs(TRUE)
if (length(argomenti) < 1) {
  stop("Indica la cartella in cui scrivere i file.")
}
cartella <- argomenti[1]
numero <- function(i, predefinito) {
  if (length(argomenti) >= i) as.numeric(argomenti[i]) else predefinito
}
n_letture <- numero(2, 2e6)
n_rfid <- numero(3, 1e5)
n_storiche <- numero(4, 3e6)
dir.create(cartella, recursive = TRUE, showWarnings = FALSE)

library(data.table)
set.seed(20261009)

comuni <- utils::read.csv("inst/extdata/comuni_cantieri.csv", stringsAsFactors = FALSE)
quote_cantieri <- c(ASIAGO = 0.12, BASSANO = 0.26, CAMPOSAMPIERO = 0.37, RUBANO = 0.25)
servizi <- c("SECCO", "CARTA", "VETRO", "UMIDO", "PLASTICA E METALLI", "VERDE E RAMAGLIE")
giri <- c("SECCO PAP", "CARTA/CARTONE PAP", "VETRO PAP", "UMIDO PAP", "PLASTICA PAP", "VERDE PAP")
volumi <- c(1100, 240, 1100, 240, 770, 240)

# ---- Contenitori ---------------------------------------------------------------

# Peso di ogni comune: la quota del suo cantiere divisa tra i suoi comuni, con
# peso quadruplo per quello che da' il nome al cantiere.
peso <- ifelse(startsWith(comuni$comune, comuni$cantiere), 4, 1)
peso <- peso / tapply(peso, comuni$cantiere, sum)[comuni$cantiere] * quote_cantieri[comuni$cantiere]
i_comune <- sample(nrow(comuni), n_rfid, replace = TRUE, prob = peso)
i_servizio <- sample(6, n_rfid, replace = TRUE, prob = c(0.40, 0.27, 0.11, 0.10, 0.07, 0.05))
bidoni <- data.table(
  RFID = sprintf("%024d", seq_len(n_rfid)),
  censito = runif(n_rfid) < 0.85,
  i_servizio = i_servizio,
  comune = comuni$comune[i_comune],
  cantiere = comuni$cantiere[i_comune],
  # attorno al centro abitato, entro un paio di chilometri
  lat = comuni$latitudine[i_comune] + rnorm(n_rfid, 0, 0.007),
  lon = comuni$longitudine[i_comune] + rnorm(n_rfid, 0, 0.010),
  utenza = sprintf("UTZ%07d", sample(max(1, round(n_rfid * 0.6)), n_rfid, replace = TRUE)),
  raccolte = ifelse(i_servizio == 3 & runif(n_rfid) < 0.5, 13, 26)
)
# Quota delle letture fatte da un giro diverso da quello del contenitore. Per
# un non censito su sette i giri si mescolano: la stima del servizio resta
# incerta, e sulla mappa il marker ha il punto di domanda.
bidoni[, rumore := fifelse(!censito & runif(n_rfid) < 0.15, 0.40, 0.03)]

# ---- Letture con le antenne: un anno ---------------------------------------------

# Venti mezzi per cantiere.
n_mezzi <- 20 * length(quote_cantieri)
mezzi <- sprintf("VEH%03d", seq_len(n_mezzi))
targhe <- sprintf("AB%03dCD", seq_len(n_mezzi))
k <- sample(n_rfid, n_letture, replace = TRUE)
i_mezzo <- (match(bidoni$cantiere[k], names(quote_cantieri)) - 1) * 20 + sample(20, n_letture, replace = TRUE)
istante <- as.POSIXct("2025-01-01", tz = "UTC") +
  floor(runif(n_letture, 0, 365)) * 86400 + runif(n_letture, 6, 19) * 3600
# Il giro di ogni lettura: quasi sempre quello del servizio del contenitore.
i_giro <- bidoni$i_servizio[k]
altro_giro <- which(runif(n_letture) < bidoni$rumore[k])
i_giro[altro_giro] <- sample(6, length(altro_giro), replace = TRUE)
censito <- bidoni$censito[k]
letture <- data.table(
  giorno_lettura = format(istante, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  targa_veicolo = targhe[i_mezzo],
  matricola_veicolo = mezzi[i_mezzo],
  RFID = bidoni$RFID[k],
  presente_a_database = fifelse(censito, "Presente", "Non Presente"),
  servizio_transponder = fifelse(censito, servizi[bidoni$i_servizio[k]], NA_character_),
  servizio_atteso = giri[i_giro],
  id_utenza = fifelse(censito, bidoni$utenza[k], NA_character_),
  latitudine = round(bidoni$lat[k] + rnorm(n_letture, 0, 0.0001), 5),
  longitudine = round(bidoni$lon[k] + rnorm(n_letture, 0, 0.0001), 5),
  comune_da_database = fifelse(censito, bidoni$comune[k], NA_character_),
  comune_lettura = bidoni$comune[k],
  cantiere = bidoni$cantiere[k],
  volume_previsto = fifelse(censito, volumi[bidoni$i_servizio[k]], NA_real_),
  numero_raccolte_annue_previste = fifelse(censito, bidoni$raccolte[k], NA_real_)
)
setorder(letture, giorno_lettura)

file_letture <- file.path(cartella, "letture_prova.csv")
fwrite(letture, file_letture, na = "NA")
fwrite(letture, paste0(file_letture, ".gz"), na = "NA")
if (requireNamespace("nanoparquet", quietly = TRUE)) {
  nanoparquet::write_parquet(as.data.frame(letture), file.path(cartella, "letture_prova.parquet"))
}

# ---- Letture storiche: dal 2020 a tutto il 2024 ----------------------------------

# Otto contenitori su dieci esistevano gia'. Il sistema precedente ne leggeva
# alcuni molto meno di altri, e ogni tanto saltava un anno intero.
con_storia <- which(runif(n_rfid) < 0.8)
k <- con_storia[sample(length(con_storia), n_storiche, replace = TRUE)]
anno <- sample(2020:2024, n_storiche, replace = TRUE)
istante <- as.POSIXct(sprintf("%d-01-01", anno), tz = "UTC") +
  floor(runif(n_storiche, 0, 365)) * 86400 + runif(n_storiche, 6, 19) * 3600
# Un anno su sei senza letture, diverso per ogni contenitore.
anno_vuoto <- (k * 7 + anno) %% 6 == 0
storiche <- data.table(
  RFID = bidoni$RFID[k],
  giorno_lettura = format(istante, "%Y-%m-%d %H:%M:%S", tz = "UTC")
)[!anno_vuoto]
setorder(storiche, giorno_lettura)
storiche <- unique(storiche)
file_storiche <- file.path(cartella, "letture_storiche_prova.csv")
fwrite(storiche, file_storiche)

for (file in list.files(cartella, pattern = "_prova", full.names = TRUE)) {
  message(sprintf("%-32s %7.0f MB", basename(file), file.size(file) / 1024^2))
}
message(sprintf(
  "%s letture di %s RFID, %s letture storiche.",
  format(nrow(letture), big.mark = ".", decimal.mark = ","),
  format(n_rfid, big.mark = ".", decimal.mark = ",", scientific = FALSE),
  format(nrow(storiche), big.mark = ".", decimal.mark = ",")
))
