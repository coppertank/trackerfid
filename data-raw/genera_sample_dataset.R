# Genera il dataset di esempio `inst/extdata/sample_rfid_dataset.csv`.
#
# Lo script e' riproducibile (seed fisso) e usa solo R base.
# Eseguire dalla radice del pacchetto:
#   Rscript data-raw/genera_sample_dataset.R

set.seed(20250901)

# ---- Parametri generali -----------------------------------------------------

giorni_periodo <- seq(as.Date("2025-09-01"), as.Date("2025-09-30"), by = "day")

veicoli <- data.frame(
  matricola = c("VEH001", "VEH002", "VEH003", "VEH004", "VEH005"),
  targa = c("AB123CD", "EF456GH", "IL789MN", "OP012QR", "ST345UV"),
  stringsAsFactors = FALSE
)

# Calendario dei mezzi: giorni della settimana (1 = lunedi' ... 7 = domenica),
# fascia oraria [da, a) e servizio svolto nel turno.
turni <- list(
  list(m = "VEH001", giorni = c(1, 3, 5), da = 6, a = 13, s = "SECCO"),
  list(m = "VEH001", giorni = c(2, 4, 6), da = 6, a = 13, s = "CARTA"),
  list(m = "VEH001", giorni = 7, da = 7, a = 12, s = "CARTA"),
  list(m = "VEH002", giorni = c(1, 3, 5), da = 7, a = 14, s = "CARTA"),
  list(m = "VEH002", giorni = c(2, 4, 6), da = 7, a = 14, s = "VETRO"),
  list(m = "VEH003", giorni = 1:6, da = 6, a = 12, s = "UMIDO"),
  list(m = "VEH003", giorni = c(2, 5), da = 13, a = 18, s = "PLASTICA"),
  list(m = "VEH004", giorni = 1:6, da = 12, a = 19, s = "SECCO"),
  list(m = "VEH005", giorni = c(1, 3, 5), da = 13, a = 19, s = "VETRO")
)

servizi <- c("SECCO", "CARTA", "VETRO", "UMIDO", "PLASTICA")
quote_servizi <- c(SECCO = 0.45, CARTA = 0.30, VETRO = 0.12, UMIDO = 0.10, PLASTICA = 0.03)

n_presenti <- 200
n_non_presenti <- 36

# ---- Funzioni di supporto ---------------------------------------------------

giorno_settimana <- function(data) (as.POSIXlt(data)$wday + 6) %% 7 + 1

istante <- function(data, ore_decimali) {
  as.POSIXct(paste(format(data), "00:00:00"), tz = "UTC") + round(ore_decimali * 3600)
}

# Servizio previsto dal calendario per un mezzo in un dato istante (NA se il
# mezzo non ha un turno in quel giorno/orario: stima non determinabile).
servizio_da_calendario <- function(matricola, quando) {
  gs <- giorno_settimana(as.Date(quando, tz = "UTC"))
  ora <- as.numeric(format(quando, "%H", tz = "UTC")) +
    as.numeric(format(quando, "%M", tz = "UTC")) / 60
  for (t in turni) {
    if (t$m == matricola && gs %in% t$giorni && ora >= t$da && ora < t$a) {
      return(t$s)
    }
  }
  NA_character_
}

# Tutte le coppie (giorno, turno) in cui viene svolto un servizio.
uscite_servizio <- function(servizio) {
  out <- list()
  for (i in seq_along(turni)) {
    t <- turni[[i]]
    if (t$s != servizio) next
    giorni <- giorni_periodo[giorno_settimana(giorni_periodo) %in% t$giorni]
    out[[length(out) + 1]] <- data.frame(data = giorni, turno = i)
  }
  do.call(rbind, out)
}

# Orario realistico dentro un turno: piu' letture a inizio giro.
orario_turno <- function(t) t$da + (t$a - t$da) * stats::rbeta(1, 2, 3.2)

# Estrae n letture (in giorni distinti) per un servizio.
letture_servizio <- function(servizio, n, giorni_ammessi = giorni_periodo) {
  uscite <- uscite_servizio(servizio)
  uscite <- uscite[uscite$data %in% giorni_ammessi, , drop = FALSE]
  giorni <- unique(uscite$data)
  n <- min(n, length(giorni))
  scelti <- sort(giorni[sample.int(length(giorni), n)])
  righe <- lapply(scelti, function(g) {
    candidati <- uscite$turno[uscite$data == g]
    t <- turni[[candidati[sample.int(length(candidati), 1)]]]
    data.frame(
      giorno_lettura = istante(g, orario_turno(t)),
      matricola_veicolo = t$m,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, righe)
}

# Lettura "fuori turno": il calendario non permette di stimare il servizio.
lettura_fuori_turno <- function(giorni_ammessi = giorni_periodo) {
  repeat {
    g <- giorni_ammessi[sample.int(length(giorni_ammessi), 1)]
    m <- sample(veicoli$matricola, 1)
    quando <- istante(g, stats::runif(1, 6, 19))
    if (is.na(servizio_da_calendario(m, quando))) {
      return(data.frame(giorno_lettura = quando, matricola_veicolo = m, stringsAsFactors = FALSE))
    }
  }
}

# ---- Posizioni: quartieri (cluster urbani) + punti periferici ----------------

zone <- data.frame(
  lat = c(41.880, 41.900, 41.926, 41.928, 41.914, 41.870, 41.862),
  lng = c(12.438, 12.424, 12.436, 12.404, 12.374, 12.398, 12.436),
  sd = c(0.0050, 0.0055, 0.0045, 0.0050, 0.0060, 0.0055, 0.0045),
  peso = c(0.20, 0.18, 0.15, 0.14, 0.11, 0.12, 0.10)
)

posizione_casuale <- function() {
  if (stats::runif(1) < 0.15) {
    # zona periferica: distribuzione uniforme sull'intera area
    lat <- stats::runif(1, 41.85, 41.95)
    lng <- stats::runif(1, 12.35, 12.45)
  } else {
    z <- zone[sample.int(nrow(zone), 1, prob = zone$peso), ]
    lat <- stats::rnorm(1, z$lat, z$sd)
    lng <- stats::rnorm(1, z$lng, z$sd)
  }
  c(lat = min(max(lat, 41.85), 41.95), lng = min(max(lng, 12.35), 12.45))
}

# ---- Anagrafica dei contenitori ---------------------------------------------

# Numero di letture per contenitore: 60% 1-2, 30% 3-5, 10% 6-15.
n_totale <- n_presenti + n_non_presenti
n_classi <- c(round(0.6 * n_totale), round(0.3 * n_totale))
n_classi <- c(n_classi, n_totale - sum(n_classi))

estrai_n_letture <- function(classe) {
  switch(classe,
    sample(1:2, 1),
    sample(3:5, 1),
    sample(6:15, 1)
  )
}

bidoni <- data.frame(
  seq = c(1:n_presenti, 201, 201:234, 250),
  presente = c(rep(TRUE, n_presenti), rep(FALSE, n_non_presenti)),
  stringsAsFactors = FALSE
)
bidoni$chiave <- seq_len(nrow(bidoni))

# Casi speciali richiesti dalla specifica (identificati per posizione).
k_cambio_servizio <- which(bidoni$presente & bidoni$seq == 1)
k_esempio_secco <- which(bidoni$presente & bidoni$seq == 2)
k_cambio_utenza <- which(bidoni$presente & bidoni$seq == 50)
k_spostato <- which(bidoni$presente & bidoni$seq == 100)
k_np_esempio <- which(!bidoni$presente)[1] # RFD20250901201
k_np_stima <- which(!bidoni$presente)[2] # RFD20250915201
k_np_senza_stima <- which(!bidoni$presente & bidoni$seq == 250) # RFD20250920250
speciali <- c(k_cambio_servizio, k_cambio_utenza, k_spostato, k_np_stima, k_np_senza_stima)

# Classi di numerosita': gli speciali hanno un numero di letture fissato.
bidoni$classe <- NA_integer_
bidoni$classe[c(k_cambio_servizio, k_cambio_utenza, k_np_stima)] <- 2L
bidoni$classe[c(k_spostato, k_np_senza_stima)] <- 1L
residui <- n_classi - tabulate(bidoni$classe, nbins = 3)
liberi <- which(is.na(bidoni$classe))
bidoni$classe[liberi] <- sample(rep(1:3, times = residui))
bidoni$n_letture <- vapply(bidoni$classe, estrai_n_letture, numeric(1))

# Servizio dei contenitori: quote esatte sui censiti.
conteggi <- round(quote_servizi * n_presenti)
bidoni$servizio <- NA_character_
bidoni$servizio[c(k_cambio_servizio, k_spostato)] <- "CARTA"
bidoni$servizio[c(k_esempio_secco, k_cambio_utenza)] <- "SECCO"
assegnati <- table(factor(bidoni$servizio[bidoni$presente], levels = servizi))
da_assegnare <- which(bidoni$presente & is.na(bidoni$servizio))
bidoni$servizio[da_assegnare] <- sample(rep(servizi, times = conteggi - as.integer(assegnati)))
# Per i non censiti il servizio "reale" e' ignoto all'azienda: serve solo a
# simulare letture plausibili.
np <- which(!bidoni$presente)
bidoni$servizio[np] <- sample(servizi, length(np), replace = TRUE, prob = quote_servizi)
bidoni$servizio[c(k_np_esempio, k_np_stima)] <- "SECCO"

# Utenze: 50 utenze, alcune con molti contenitori e altre con uno solo.
utenze <- sprintf("UTZ%03d", 1:50)
bidoni$utenza <- NA_character_
bidoni$utenza[c(k_cambio_servizio, k_cambio_utenza)] <- "UTZ001"
bidoni$utenza[k_esempio_secco] <- "UTZ002"
da_assegnare <- which(bidoni$presente & is.na(bidoni$utenza))
pool <- c(
  utenze[3:50],
  sample(utenze[1:35], length(da_assegnare) - 48, replace = TRUE, prob = 1 / (1:35)^0.8)
)
bidoni$utenza[da_assegnare] <- sample(pool)

# Posizione "di casa" di ogni contenitore.
pos <- t(vapply(seq_len(nrow(bidoni)), function(i) posizione_casuale(), numeric(2)))
bidoni$lat <- pos[, "lat"]
bidoni$lng <- pos[, "lng"]

# ---- Generazione delle letture ----------------------------------------------

rumore_gps <- function(n) stats::rnorm(n, 0, 0.00015)

genera_letture <- function(b) {
  if (b$chiave == k_cambio_servizio) {
    out <- data.frame(
      giorno_lettura = as.POSIXct(
        c("2025-09-01 06:00:00", "2025-09-15 08:42:17", "2025-09-28 09:21:44"),
        tz = "UTC"
      ),
      matricola_veicolo = "VEH001",
      servizio_transponder = c("CARTA", "SECCO", "CARTA"),
      stringsAsFactors = FALSE
    )
  } else if (b$chiave == k_cambio_utenza) {
    out <- data.frame(
      giorno_lettura = as.POSIXct(
        c("2025-09-01 12:48:05", "2025-09-08 13:10:31", "2025-09-20 12:52:49", "2025-09-27 13:05:12"),
        tz = "UTC"
      ),
      matricola_veicolo = "VEH004",
      id_utenza = c("UTZ001", "UTZ001", "UTZ025", "UTZ025"),
      stringsAsFactors = FALSE
    )
  } else if (b$chiave == k_spostato) {
    out <- data.frame(
      giorno_lettura = as.POSIXct(c("2025-09-05 08:14:26", "2025-09-19 10:37:53"), tz = "UTC"),
      matricola_veicolo = "VEH002",
      latitudine = c(41.85, 42.00),
      longitudine = c(12.35, 12.50),
      stringsAsFactors = FALSE
    )
  } else if (b$chiave == k_np_stima) {
    out <- data.frame(
      giorno_lettura = as.POSIXct(
        c("2025-09-15 07:26:40", "2025-09-17 07:31:08", "2025-09-22 14:12:55", "2025-09-26 07:19:33"),
        tz = "UTC"
      ),
      matricola_veicolo = c("VEH001", "VEH001", "VEH004", "VEH001"),
      stringsAsFactors = FALSE
    )
  } else if (b$chiave == k_np_senza_stima) {
    # VEH005 non ha turni di sabato: stima non determinabile.
    out <- data.frame(
      giorno_lettura = as.POSIXct(c("2025-09-20 15:44:02", "2025-09-27 16:08:37"), tz = "UTC"),
      matricola_veicolo = "VEH005",
      stringsAsFactors = FALSE
    )
  } else if (b$chiave %in% c(k_esempio_secco, k_np_esempio)) {
    # Righe di esempio della specifica come prima lettura, poi letture casuali.
    prima <- data.frame(
      giorno_lettura = as.POSIXct(
        if (b$chiave == k_esempio_secco) "2025-09-01 06:15:00" else "2025-09-01 06:30:00",
        tz = "UTC"
      ),
      matricola_veicolo = "VEH001",
      stringsAsFactors = FALSE
    )
    altre <- if (b$n_letture > 1) {
      letture_servizio(b$servizio, b$n_letture - 1, giorni_periodo[-1])
    }
    out <- rbind(prima, altre)
  } else if (b$presente) {
    out <- letture_servizio(b$servizio, b$n_letture)
  } else {
    # Non censito: letture per lo piu' nel giro del proprio servizio, a volte
    # nel giro di un altro servizio o fuori turno (stima non determinabile).
    misto <- b$n_letture >= 3 && stats::runif(1) < 0.45
    righe <- lapply(seq_len(b$n_letture), function(i) {
      u <- stats::runif(1)
      if (u < 0.14) {
        lettura_fuori_turno()
      } else if (misto && u < 0.50) {
        letture_servizio(sample(setdiff(servizi, b$servizio), 1), 1)
      } else {
        letture_servizio(b$servizio, 1)
      }
    })
    out <- do.call(rbind, righe)
    out <- out[!duplicated(as.Date(out$giorno_lettura, tz = "UTC")), , drop = FALSE]
  }

  out <- out[order(out$giorno_lettura), , drop = FALSE]
  n <- nrow(out)
  if (is.null(out$servizio_transponder)) {
    out$servizio_transponder <- if (b$presente) b$servizio else NA_character_
  }
  if (is.null(out$id_utenza)) {
    out$id_utenza <- if (b$presente) b$utenza else NA_character_
  }
  if (is.null(out$latitudine)) {
    out$latitudine <- b$lat + rumore_gps(n)
    out$longitudine <- b$lng + rumore_gps(n)
  }
  out$chiave <- b$chiave
  out$presente <- b$presente
  out
}

letture <- do.call(rbind, lapply(seq_len(nrow(bidoni)), function(i) genera_letture(bidoni[i, ])))

# Le tre righe di esempio della specifica devono aprire il file: le letture
# generate nel primo giorno partono dopo le 06:30.
soglia <- as.POSIXct("2025-09-01 06:30:00", tz = "UTC")
righe_esempio <- letture$giorno_lettura %in% as.POSIXct(
  c("2025-09-01 06:00:00", "2025-09-01 06:15:00", "2025-09-01 06:30:00"),
  tz = "UTC"
)
anticipate <- letture$giorno_lettura <= soglia & !righe_esempio
letture$giorno_lettura[anticipate] <- letture$giorno_lettura[anticipate] + 45 * 60

# Coordinate esatte delle righe di esempio (centro di Roma).
coord_esempio <- list(
  list(k = k_cambio_servizio, lat = 41.9028, lng = 12.4964),
  list(k = k_esempio_secco, lat = 41.9035, lng = 12.4971),
  list(k = k_np_esempio, lat = 41.9042, lng = 12.4978)
)
for (ce in coord_esempio) {
  righe <- which(letture$chiave == ce$k)
  letture$latitudine[righe] <- ce$lat + c(0, rumore_gps(length(righe) - 1))
  letture$longitudine[righe] <- ce$lng + c(0, rumore_gps(length(righe) - 1))
}

# ---- Variazioni nel tempo ----------------------------------------------------

n_per_bidone <- table(letture$chiave)
candidati <- function(min_letture) {
  k <- as.integer(names(n_per_bidone)[n_per_bidone >= min_letture])
  k <- k[bidoni$presente[k]]
  setdiff(k, c(speciali, k_esempio_secco))
}

# Indici delle letture successive al "punto di svolta" di un contenitore.
dopo_svolta <- function(k) {
  righe <- which(letture$chiave == k)
  righe <- righe[order(letture$giorno_lettura[righe])]
  righe[seq(ceiling(length(righe) / 2) + 1, length(righe))]
}

# Altri 6 contenitori che cambiano utenza (7 in totale con il caso speciale).
k_utenza <- sample(candidati(3), 6)
for (k in k_utenza) {
  nuova <- sample(setdiff(utenze, bidoni$utenza[k]), 1)
  letture$id_utenza[dopo_svolta(k)] <- nuova
}

# Altri 2 contenitori che cambiano servizio in modo definitivo.
k_servizio <- sample(setdiff(candidati(4), k_utenza), 2)
for (k in k_servizio) {
  letture$servizio_transponder[dopo_svolta(k)] <- sample(setdiff(servizi, bidoni$servizio[k]), 1)
}

# 8 contenitori spostati di qualche centinaio di metri.
k_mossi <- sample(setdiff(candidati(2), c(k_utenza, k_servizio)), 8)
for (k in k_mossi) {
  righe <- dopo_svolta(k)
  letture$latitudine[righe] <- letture$latitudine[righe] + stats::runif(1, 0.003, 0.009) * sample(c(-1, 1), 1)
  letture$longitudine[righe] <- letture$longitudine[righe] + stats::runif(1, 0.003, 0.009) * sample(c(-1, 1), 1)
}

# ---- Campi derivati -----------------------------------------------------------

letture$targa_veicolo <- veicoli$targa[match(letture$matricola_veicolo, veicoli$matricola)]
letture$presente_a_database <- ifelse(letture$presente, "Presente", "Non Presente")

# Servizio atteso: solo per i non censiti, dal calendario del mezzo.
letture$servizio_atteso <- NA_character_
for (i in which(!letture$presente)) {
  letture$servizio_atteso[i] <- servizio_da_calendario(
    letture$matricola_veicolo[i], letture$giorno_lettura[i]
  )
}

# Codice RFID: "RFD" + data della prima lettura + progressivo del contenitore.
prima_lettura <- tapply(letture$giorno_lettura, letture$chiave, min)
prima_lettura <- as.POSIXct(prima_lettura, origin = "1970-01-01", tz = "UTC")
bidoni$RFID <- sprintf(
  "RFD%s%03d",
  format(prima_lettura[as.character(bidoni$chiave)], "%Y%m%d", tz = "UTC"),
  bidoni$seq
)
letture$RFID <- bidoni$RFID[letture$chiave]

letture <- letture[order(letture$giorno_lettura, letture$RFID), ]
finale <- data.frame(
  giorno_lettura = format(letture$giorno_lettura, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  targa_veicolo = letture$targa_veicolo,
  matricola_veicolo = letture$matricola_veicolo,
  RFID = letture$RFID,
  presente_a_database = letture$presente_a_database,
  servizio_transponder = letture$servizio_transponder,
  servizio_atteso = letture$servizio_atteso,
  id_utenza = letture$id_utenza,
  latitudine = sprintf("%.4f", letture$latitudine),
  longitudine = sprintf("%.4f", letture$longitudine),
  stringsAsFactors = FALSE
)

# ---- Controlli di conformita' alla specifica ---------------------------------

per_rfid <- split(finale, finale$RFID)
stopifnot(
  nrow(finale) >= 600,
  length(per_rfid) >= 200, length(per_rfid) <= 250,
  !anyDuplicated(bidoni$RFID),
  min(finale$giorno_lettura) == "2025-09-01 06:00:00",
  max(finale$giorno_lettura) < "2025-09-30 19:00:01",
  all(substr(finale$giorno_lettura, 12, 19) >= "06:00:00"),
  all(substr(finale$giorno_lettura, 12, 19) <= "19:00:00"),
  finale$RFID[1:3] == c("RFD20250901001", "RFD20250901002", "RFD20250901201"),
  identical(per_rfid$RFD20250901001$servizio_transponder, c("CARTA", "SECCO", "CARTA")),
  identical(substr(per_rfid$RFD20250901001$giorno_lettura, 1, 10), c("2025-09-01", "2025-09-15", "2025-09-28")),
  identical(per_rfid$RFD20250901050$id_utenza, c("UTZ001", "UTZ001", "UTZ025", "UTZ025")),
  identical(per_rfid$RFD20250915201$servizio_atteso, rep("SECCO", 4)),
  nrow(per_rfid$RFD20250920250) == 2, all(is.na(per_rfid$RFD20250920250$servizio_atteso)),
  identical(per_rfid$RFD20250905100$latitudine, c("41.8500", "42.0000")),
  identical(per_rfid$RFD20250905100$longitudine, c("12.3500", "12.5000")),
  all(is.na(finale$servizio_transponder) == (finale$presente_a_database == "Non Presente")),
  all(is.na(finale$id_utenza) == (finale$presente_a_database == "Non Presente")),
  all(is.na(finale$servizio_atteso[finale$presente_a_database == "Presente"])),
  nrow(unique(finale[, c("targa_veicolo", "matricola_veicolo")])) == 5,
  length(unique(stats::na.omit(finale$id_utenza))) == 50,
  !anyNA(finale$latitudine), !anyNA(finale$longitudine)
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  finale,
  "inst/extdata/sample_rfid_dataset.csv",
  row.names = FALSE, quote = FALSE, na = "NA", fileEncoding = "UTF-8"
)

message(sprintf(
  "Scritte %d letture di %d RFID in inst/extdata/sample_rfid_dataset.csv",
  nrow(finale), length(per_rfid)
))
