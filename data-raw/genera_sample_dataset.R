# Genera il dataset di esempio `inst/extdata/sample_rfid_dataset.csv`.
#
# Il dataset copre 15 mesi (ottobre 2024 - dicembre 2025): l'anno 2025 e'
# completo, il 2024 parziale, cosi' si puo' provare la selezione dell'anno.
# Ogni contenitore viene letto a cadenza fissa dal giro del proprio servizio,
# come accade con un calendario di raccolta reale. Sono inclusi esempi per
# tutti gli indicatori dell'analisi dei cluster spaziali.
#
# Lo script e' riproducibile (seed fisso) e usa solo R base.
# Eseguire dalla radice del pacchetto:
#   Rscript data-raw/genera_sample_dataset.R

set.seed(20250901)

# ---- Parametri generali -----------------------------------------------------

inizio_dati <- as.Date("2024-10-01")
fine_dati <- as.Date("2025-12-31")
giorni_periodo <- seq(inizio_dati, fine_dati, by = "day")

veicoli <- data.frame(
  matricola = c("VEH001", "VEH002", "VEH003", "VEH004", "VEH005"),
  targa = c("AB123CD", "EF456GH", "IL789MN", "OP012QR", "ST345UV"),
  stringsAsFactors = FALSE
)

# Calendario dei mezzi: giorni della settimana (1 = lunedi' ... 7 = domenica),
# fascia oraria [da, a), nome del giro (diventa il `servizio_atteso`; PAP =
# porta a porta) e tipologia di contenitore raccolta nel giro (`s`, NA per i
# giri che non raccolgono una tipologia precisa).
turno <- function(m, giorni, da, a, giro, s = NA_character_) {
  list(m = m, giorni = giorni, da = da, a = a, giro = giro, s = s)
}
turni <- list(
  turno("VEH001", c(1, 3, 5), 6, 13, "SECCO PAP", "SECCO"),
  turno("VEH001", c(2, 4, 6), 6, 13, "CARTA/CARTONE PAP", "CARTA"),
  turno("VEH001", 7, 7, 12, "CARTA CONT.STRADALI", "CARTA"),
  turno("VEH002", c(1, 3, 5), 7, 14, "CARTA CONT.STRADALI", "CARTA"),
  turno("VEH002", c(2, 4, 6), 7, 14, "VETRO PAP", "VETRO"),
  turno("VEH003", 1:6, 6, 12, "UMIDO PAP", "UMIDO"),
  turno("VEH003", c(1, 4), 13, 18, "UMIDO CONT.STRADALI", "UMIDO"),
  turno("VEH003", c(2, 5), 13, 18, "PLASTICA PAP", "PLASTICA E METALLI"),
  turno("VEH003", c(3, 6), 13, 18, "VERDE PAP", "VERDE E RAMAGLIE"),
  turno("VEH004", 1:6, 12, 19, "SECCO PAP", "SECCO"),
  turno("VEH005", c(1, 3), 6, 12, "ASSISTENTE SERVIZI"),
  turno("VEH005", c(2, 4), 6, 12, "PULIZIA TERRIT."),
  turno("VEH005", 6, 6, 12, "SERVIZI MERCATI"),
  turno("VEH005", c(1, 3, 5), 13, 19, "VETRO PAP", "VETRO"),
  turno("VEH005", c(2, 4), 13, 19, "PLAST.CONT.STRADALI", "PLASTICA E METALLI")
)

# Tipologie di `servizio_transponder` e loro quota tra i contenitori censiti.
servizi <- c("SECCO", "CARTA", "VETRO", "UMIDO", "PLASTICA E METALLI", "VERDE E RAMAGLIE")
quote_servizi <- c(0.40, 0.27, 0.11, 0.10, 0.07, 0.05)
names(quote_servizi) <- servizi

# Volumi dei contenitori (litri) e relative probabilita' per tipologia.
volumi <- list(
  "SECCO" = list(v = c(1100, 240), p = c(0.8, 0.2)),
  "CARTA" = list(v = c(240, 1100), p = c(0.5, 0.5)),
  "VETRO" = list(v = c(1100, 770), p = c(0.9, 0.1)),
  "UMIDO" = list(v = c(240, 770), p = c(0.7, 0.3)),
  "PLASTICA E METALLI" = list(v = 770, p = 1),
  "VERDE E RAMAGLIE" = list(v = c(240, 1100), p = c(0.7, 0.3))
)

n_presenti <- 200
n_non_presenti <- 36

# ---- Funzioni di supporto ---------------------------------------------------

giorno_settimana <- function(data) (as.POSIXlt(data)$wday + 6) %% 7 + 1

istante <- function(data, ore_decimali) {
  as.POSIXct(paste(format(data), "00:00:00"), tz = "UTC") + round(ore_decimali * 3600)
}

estrai <- function(x, n = 1, ...) x[sample.int(length(x), n, ...)]

# Giro previsto dal calendario per un mezzo in un dato istante (NA se il
# mezzo non ha un turno in quel giorno/orario: stima non determinabile).
servizio_da_calendario <- function(matricola, quando) {
  gs <- giorno_settimana(as.Date(quando, tz = "UTC"))
  ora <- as.numeric(format(quando, "%H", tz = "UTC")) +
    as.numeric(format(quando, "%M", tz = "UTC")) / 60
  for (t in turni) {
    if (t$m == matricola && gs %in% t$giorni && ora >= t$da && ora < t$a) {
      return(t$giro)
    }
  }
  NA_character_
}

# Indici dei turni che raccolgono una tipologia.
turni_servizio <- function(servizio) {
  which(vapply(turni, function(t) !is.na(t$s) && t$s == servizio, logical(1)))
}

# Orario tipico dentro un turno: piu' letture a inizio giro.
orario_turno <- function(t) t$da + (t$a - t$da) * stats::rbeta(1, 2, 3.2)

# Date di raccolta a cadenza fissa: stesso giorno della settimana ogni
# `cadenza` giorni, a partire da `dal`, con uno sfasamento in settimane.
date_raccolta <- function(giorno, cadenza, dal, al = fine_dati, sfasamento = 0) {
  candidati <- giorni_periodo[
    giorni_periodo >= dal & giorni_periodo <= al & giorno_settimana(giorni_periodo) == giorno
  ]
  if (length(candidati) == 0) {
    return(candidati)
  }
  prima <- candidati[1] + 7 * sfasamento
  if (prima > al) candidati[1] else seq(prima, al, by = cadenza)
}

# Letture di un contenitore secondo il proprio calendario: il contenitore non
# viene esposto (e quindi letto) a ogni passaggio.
letture_calendario <- function(b, dal = b$attivo_dal, al = fine_dati) {
  t <- turni[[b$turno]]
  date <- date_raccolta(b$giorno, b$cadenza, dal, al, b$sfasamento)
  tenute <- date[stats::runif(length(date)) < b$esposizione]
  if (length(tenute) == 0) tenute <- date[1]
  ore <- b$ora_tipica + stats::rnorm(length(tenute), 0, 0.25)
  ore <- pmin(pmax(ore, t$da), t$a - 0.02)
  data.frame(
    giorno_lettura = istante(tenute, ore),
    matricola_veicolo = t$m,
    stringsAsFactors = FALSE
  )
}

# Lettura in un giorno e orario casuali di un turno.
lettura_turno <- function(t, dal = inizio_dati, al = fine_dati) {
  giorni <- giorni_periodo[
    giorni_periodo >= dal & giorni_periodo <= al & giorno_settimana(giorni_periodo) %in% t$giorni
  ]
  data.frame(
    giorno_lettura = istante(estrai(giorni), orario_turno(t)),
    matricola_veicolo = t$m,
    stringsAsFactors = FALSE
  )
}

# Lettura "fuori turno": il calendario non permette di stimare il servizio.
lettura_fuori_turno <- function(dal = inizio_dati, al = fine_dati) {
  giorni <- giorni_periodo[giorni_periodo >= dal & giorni_periodo <= al]
  repeat {
    m <- estrai(veicoli$matricola)
    quando <- istante(estrai(giorni), stats::runif(1, 6, 19))
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

# Errore tipico del GPS di bordo: una decina di metri.
rumore_gps <- function(n, sd = 0.0001) stats::rnorm(n, 0, sd)

# ---- Anagrafica dei contenitori ---------------------------------------------

bidoni <- data.frame(
  seq = c(1:n_presenti, 201, 201:234, 250, 301:303),
  presente = c(rep(TRUE, n_presenti), rep(FALSE, n_non_presenti), FALSE, TRUE, TRUE),
  stringsAsFactors = FALSE
)
bidoni$chiave <- seq_len(nrow(bidoni))

# Casi speciali (identificati per posizione).
k_cambio_servizio <- which(bidoni$presente & bidoni$seq == 1)
k_esempio_secco <- which(bidoni$presente & bidoni$seq == 2)
k_cambio_utenza <- which(bidoni$presente & bidoni$seq == 50)
k_spostato <- which(bidoni$presente & bidoni$seq == 100)
k_np_esempio <- which(!bidoni$presente)[1] # RFD20250901201
k_np_stima <- which(!bidoni$presente)[2] # RFD20250915201
k_np_senza_stima <- which(bidoni$seq == 250) # RFD20250920250
# Esempi per gli indicatori dell'analisi dei cluster.
k_deposito <- which(bidoni$seq == 301) # tag fermo in deposito
k_camion <- which(bidoni$seq == 302) # tag rimasto sul camion
k_sparso <- which(bidoni$seq == 303) # letture sparse / GPS rumoroso
speciali <- c(
  k_cambio_servizio, k_esempio_secco, k_cambio_utenza, k_spostato,
  k_np_esempio, k_np_stima, k_np_senza_stima, k_deposito, k_camion, k_sparso
)
censiti <- which(bidoni$presente)
non_censiti <- which(!bidoni$presente)

# Servizio dei contenitori: quote esatte sui primi 200 censiti.
conteggi <- round(quote_servizi * n_presenti)
bidoni$servizio <- NA_character_
bidoni$servizio[c(k_cambio_servizio, k_spostato)] <- "CARTA"
bidoni$servizio[c(k_esempio_secco, k_cambio_utenza, k_camion)] <- "SECCO"
bidoni$servizio[k_sparso] <- "UMIDO"
primi <- which(bidoni$presente & bidoni$seq <= n_presenti)
assegnati <- table(factor(bidoni$servizio[primi], levels = servizi))
da_assegnare <- primi[is.na(bidoni$servizio[primi])]
bidoni$servizio[da_assegnare] <- sample(rep(servizi, times = conteggi - as.integer(assegnati)))
# Per i non censiti il servizio "reale" e' ignoto all'azienda: serve solo a
# simulare letture plausibili. Tipologie bilanciate, cosi' compaiono tutti i giri.
bidoni$servizio[non_censiti] <- sample(rep_len(servizi, length(non_censiti)))
bidoni$servizio[c(k_np_esempio, k_np_stima)] <- "SECCO"

# Utenze: 50 utenze, alcune con molti contenitori e altre con uno solo.
utenze <- sprintf("UTZ%03d", 1:50)
bidoni$utenza <- NA_character_
bidoni$utenza[c(k_cambio_servizio, k_cambio_utenza)] <- "UTZ001"
bidoni$utenza[k_esempio_secco] <- "UTZ002"
da_assegnare <- censiti[is.na(bidoni$utenza[censiti])]
pool <- c(
  utenze[3:50],
  sample(utenze[1:35], length(da_assegnare) - 48, replace = TRUE, prob = 1 / (1:35)^0.8)
)
bidoni$utenza[da_assegnare] <- sample(pool)

# Volume e raccolte annue previste (solo per i censiti).
bidoni$volume <- NA_integer_
bidoni$raccolte <- NA_integer_
for (k in censiti) {
  vol <- volumi[[bidoni$servizio[k]]]
  bidoni$volume[k] <- vol$v[sample.int(length(vol$v), 1, prob = vol$p)]
  # bisettimanale; il vetro puo' essere mensile
  bidoni$raccolte[k] <- if (bidoni$servizio[k] == "VETRO") estrai(c(13L, 26L)) else 26L
}

# Calendario di ogni contenitore: turno, giorno, cadenza, orario tipico.
bidoni$turno <- NA_integer_
bidoni$giorno <- NA_integer_
bidoni$ora_tipica <- NA_real_
for (k in seq_len(nrow(bidoni))) {
  indice <- estrai(turni_servizio(bidoni$servizio[k]))
  bidoni$turno[k] <- indice
  bidoni$giorno[k] <- estrai(turni[[indice]]$giorni)
  bidoni$ora_tipica[k] <- orario_turno(turni[[indice]])
}
bidoni$cadenza <- ifelse(!is.na(bidoni$raccolte) & bidoni$raccolte == 13L, 28L, 14L)
bidoni$sfasamento <- vapply(bidoni$cadenza, function(cd) sample.int(cd / 7, 1) - 1, numeric(1))
# Quota di passaggi in cui il contenitore e' esposto e viene letto.
bidoni$esposizione <- stats::runif(nrow(bidoni), 0.75, 0.98)
# Data di attivazione: quasi tutti presenti dall'inizio, alcuni arrivano nel 2025.
bidoni$attivo_dal <- inizio_dati
tardivi <- sample(setdiff(seq_len(nrow(bidoni)), speciali), 34)
bidoni$attivo_dal[tardivi] <- estrai(
  seq(as.Date("2025-01-15"), as.Date("2025-10-15"), by = "day"), length(tardivi)
)

# Contenitori censiti ma quasi mai esposti: poche letture in un anno.
k_rari <- sample(setdiff(censiti, c(speciali, tardivi)), 14)
bidoni$esposizione[k_rari] <- stats::runif(length(k_rari), 0.03, 0.10)

# Non censiti: meta' raccolti con regolarita', meta' letti solo sporadicamente.
np_ordinari <- setdiff(non_censiti, speciali)
np_regolari <- sample(np_ordinari, 17)
np_sporadici <- setdiff(np_ordinari, np_regolari)
# Quota di letture avvenute in un giro diverso dal proprio (stima meno netta).
bidoni$rumore <- 0
bidoni$rumore[np_regolari] <- sample(rep_len(c(0.04, 0.04, 0.08, 0.35, 0.50), length(np_regolari)))

# Posizione "di casa" di ogni contenitore.
pos <- t(vapply(seq_len(nrow(bidoni)), function(i) posizione_casuale(), numeric(2)))
bidoni$lat <- pos[, "lat"]
bidoni$lng <- pos[, "lng"]

# Casi con calendario fissato: prime letture scritte a mano, poi il calendario.
imposta <- function(k, turno, giorno, ora, dal, sfasamento = 0, esposizione = 0.95) {
  bidoni$turno[k] <<- turno
  bidoni$giorno[k] <<- giorno
  bidoni$ora_tipica[k] <<- ora
  bidoni$attivo_dal[k] <<- as.Date(dal)
  bidoni$sfasamento[k] <<- sfasamento
  bidoni$esposizione[k] <<- esposizione
  bidoni$cadenza[k] <<- 14L
}
imposta(k_cambio_servizio, turno = 2, giorno = 2, ora = 8.6, dal = "2025-10-07")
imposta(k_esempio_secco, turno = 1, giorno = 1, ora = 6.3, dal = "2025-09-15")
imposta(k_cambio_utenza, turno = 10, giorno = 6, ora = 13.0, dal = "2025-10-11")
imposta(k_spostato, turno = 4, giorno = 5, ora = 10.5, dal = "2025-10-03")
imposta(k_np_esempio, turno = 1, giorno = 1, ora = 6.5, dal = "2025-09-15")
bidoni$raccolte[c(k_cambio_servizio, k_esempio_secco, k_cambio_utenza, k_spostato)] <- 26L

# ---- Generazione delle letture ----------------------------------------------

# Percorso del mezzo su cui e' rimasto un tag: le letture si distribuiscono
# lungo tutto il giro.
percorso <- data.frame(
  lat = c(41.866, 41.872, 41.882, 41.900),
  lng = c(12.395, 12.418, 12.440, 12.426)
)
punto_sul_percorso <- function(n) {
  tratti <- nrow(percorso) - 1
  lunghezze <- sqrt(diff(percorso$lat)^2 + (diff(percorso$lng) * cos(41.88 * pi / 180))^2)
  tratto <- sample.int(tratti, n, replace = TRUE, prob = lunghezze)
  f <- stats::runif(n)
  data.frame(
    latitudine = percorso$lat[tratto] + f * diff(percorso$lat)[tratto] + rumore_gps(n),
    longitudine = percorso$lng[tratto] + f * diff(percorso$lng)[tratto] + rumore_gps(n)
  )
}

manuali <- function(quando, matricola, ...) {
  data.frame(
    giorno_lettura = as.POSIXct(quando, tz = "UTC"),
    matricola_veicolo = matricola,
    ...,
    stringsAsFactors = FALSE
  )
}
aggiungi <- function(prime, altre, ...) {
  extra <- list(...)
  for (campo in names(extra)) altre[[campo]] <- extra[[campo]]
  for (campo in setdiff(names(prime), names(altre))) altre[[campo]] <- NA
  rbind(prime, altre[, names(prime), drop = FALSE])
}

turno_garantito <- 0

genera_letture <- function(b) {
  if (b$chiave == k_cambio_servizio) {
    # CARTA -> SECCO -> CARTA, poi resta CARTA.
    out <- aggiungi(
      manuali(
        c("2025-09-01 06:00:00", "2025-09-15 08:42:17", "2025-09-28 09:21:44"), "VEH001",
        servizio_transponder = c("CARTA", "SECCO", "CARTA")
      ),
      letture_calendario(b),
      servizio_transponder = "CARTA"
    )
  } else if (b$chiave == k_cambio_utenza) {
    # Passa da UTZ001 a UTZ025 il 20 settembre.
    out <- aggiungi(
      manuali(
        c("2025-09-01 12:48:05", "2025-09-08 13:10:31", "2025-09-20 12:52:49", "2025-09-27 13:05:12"),
        "VEH004",
        id_utenza = c("UTZ001", "UTZ001", "UTZ025", "UTZ025")
      ),
      letture_calendario(b),
      id_utenza = "UTZ025"
    )
  } else if (b$chiave == k_spostato) {
    # Prima lettura in un punto, dalla seconda in poi a oltre 20 km di distanza.
    altre <- letture_calendario(b)
    out <- aggiungi(
      manuali(
        c("2025-09-05 08:14:26", "2025-09-19 10:37:53"), "VEH002",
        latitudine = c(41.85, 42.00), longitudine = c(12.35, 12.50)
      ),
      altre,
      latitudine = 42.00 + rumore_gps(nrow(altre)),
      longitudine = 12.50 + rumore_gps(nrow(altre))
    )
  } else if (b$chiave == k_np_stima) {
    out <- manuali(
      c("2025-09-15 07:26:40", "2025-09-17 07:31:08", "2025-09-22 14:12:55", "2025-09-26 07:19:33"),
      c("VEH001", "VEH001", "VEH004", "VEH001")
    )
  } else if (b$chiave == k_np_senza_stima) {
    # VEH005 non ha turni il sabato pomeriggio: stima non determinabile.
    out <- manuali(c("2025-09-20 15:44:02", "2025-09-27 16:08:37"), "VEH005")
  } else if (b$chiave %in% c(k_esempio_secco, k_np_esempio)) {
    prima <- if (b$chiave == k_esempio_secco) "2025-09-01 06:15:00" else "2025-09-01 06:30:00"
    out <- aggiungi(manuali(prima, "VEH001"), letture_calendario(b))
  } else if (b$chiave == k_deposito) {
    # Tag fermo vicino al lettore del deposito: letto ogni giorno lavorativo
    # dai mezzi in uscita o al rientro.
    feriali <- giorni_periodo[giorno_settimana(giorni_periodo) <= 6]
    giorni <- rep(feriali, times = sample(1:2, length(feriali), replace = TRUE, prob = c(0.65, 0.35)))
    uscita <- stats::runif(length(giorni)) < 0.7
    ore <- ifelse(uscita, stats::runif(length(giorni), 6.0, 6.4), stats::runif(length(giorni), 18.5, 18.98))
    out <- data.frame(
      giorno_lettura = istante(giorni, ore),
      matricola_veicolo = sample(veicoli$matricola, length(giorni), replace = TRUE),
      latitudine = 41.9050 + rumore_gps(length(giorni), 0.00004),
      longitudine = 12.3620 + rumore_gps(length(giorni), 0.00004),
      stringsAsFactors = FALSE
    )
  } else if (b$chiave == k_camion) {
    # Tag rimasto nel cassone di VEH004: due letture per giorno lavorativo,
    # ogni volta in un punto diverso del giro.
    feriali <- giorni_periodo[giorni_periodo >= as.Date("2025-02-03") & giorno_settimana(giorni_periodo) <= 6]
    giorni <- rep(feriali, each = 2)
    out <- cbind(
      data.frame(
        giorno_lettura = istante(giorni, stats::runif(length(giorni), 12, 18.98)),
        matricola_veicolo = "VEH004",
        stringsAsFactors = FALSE
      ),
      punto_sul_percorso(length(giorni))
    )
  } else if (b$chiave == k_sparso) {
    # GPS molto impreciso: ogni lettura cade a centinaia di metri dalle altre.
    out <- letture_calendario(b)
    out$latitudine <- b$lat + rumore_gps(nrow(out), 0.0030)
    out$longitudine <- b$lng + rumore_gps(nrow(out), 0.0030)
  } else if (b$presente) {
    out <- letture_calendario(b)
  } else {
    # Non censito. La prima lettura segue a rotazione tutti i turni del
    # calendario, cosi' nel dataset compare ogni giro almeno una volta.
    turno_garantito <<- turno_garantito %% length(turni) + 1
    prima <- lettura_turno(turni[[turno_garantito]], b$attivo_dal)
    if (b$chiave %in% np_regolari) {
      altre <- letture_calendario(b)
      altrove <- which(stats::runif(nrow(altre)) < b$rumore)
      for (i in altrove) {
        altre[i, ] <- if (stats::runif(1) < 0.25) {
          lettura_fuori_turno(b$attivo_dal)
        } else {
          lettura_turno(turni[[sample.int(length(turni), 1)]], b$attivo_dal)
        }
      }
    } else {
      n <- sample(0:3, 1)
      altre <- do.call(rbind, lapply(seq_len(n), function(i) {
        if (stats::runif(1) < 0.2) {
          lettura_fuori_turno(b$attivo_dal)
        } else {
          lettura_turno(turni[[b$turno]], b$attivo_dal)
        }
      }))
    }
    out <- rbind(prima, altre)
  }

  out <- out[order(out$giorno_lettura), , drop = FALSE]
  out <- out[!duplicated(out$giorno_lettura), , drop = FALSE]
  n <- nrow(out)
  completa <- function(campo, valore) {
    if (is.null(out[[campo]])) out[[campo]] <<- valore
    mancanti <- is.na(out[[campo]])
    out[[campo]][mancanti] <<- rep_len(valore, n)[mancanti]
  }
  completa("servizio_transponder", if (b$presente) b$servizio else NA_character_)
  completa("id_utenza", if (b$presente) b$utenza else NA_character_)
  completa("latitudine", b$lat + rumore_gps(n))
  completa("longitudine", b$lng + rumore_gps(n))
  out$chiave <- b$chiave
  out$presente <- b$presente
  out[, c(
    "giorno_lettura", "matricola_veicolo", "servizio_transponder", "id_utenza",
    "latitudine", "longitudine", "chiave", "presente"
  )]
}

letture <- do.call(rbind, lapply(seq_len(nrow(bidoni)), function(i) genera_letture(bidoni[i, ])))
rownames(letture) <- NULL

# Coordinate esatte delle righe di esempio della specifica (centro di Roma).
coord_esempio <- list(
  list(k = k_cambio_servizio, lat = 41.9028, lng = 12.4964),
  list(k = k_esempio_secco, lat = 41.9035, lng = 12.4971),
  list(k = k_np_esempio, lat = 41.9042, lng = 12.4978)
)
for (ce in coord_esempio) {
  righe <- which(letture$chiave == ce$k)
  righe <- righe[order(letture$giorno_lettura[righe])]
  letture$latitudine[righe] <- ce$lat + c(0, rumore_gps(length(righe) - 1))
  letture$longitudine[righe] <- ce$lng + c(0, rumore_gps(length(righe) - 1))
}

# ---- Variazioni nel corso del 2025 -------------------------------------------

n_per_bidone <- table(letture$chiave)
candidati <- function(min_letture) {
  k <- as.integer(names(n_per_bidone)[n_per_bidone >= min_letture])
  setdiff(k[bidoni$presente[k]], c(speciali, tardivi, k_rari))
}
giorni_svolta <- seq(as.Date("2025-03-01"), as.Date("2025-10-15"), by = "day")

# Indici delle letture di un contenitore successive a una data casuale del 2025.
dopo_svolta <- function(k) {
  svolta <- estrai(giorni_svolta)
  which(letture$chiave == k & as.Date(letture$giorno_lettura, tz = "UTC") >= svolta)
}

# Altri 6 contenitori che cambiano utenza (7 in totale con il caso speciale).
k_utenza <- sample(candidati(12), 6)
for (k in k_utenza) {
  letture$id_utenza[dopo_svolta(k)] <- estrai(setdiff(utenze, bidoni$utenza[k]))
}

# Altri 2 contenitori che cambiano servizio in modo definitivo.
k_servizio <- sample(setdiff(candidati(12), k_utenza), 2)
for (k in k_servizio) {
  letture$servizio_transponder[dopo_svolta(k)] <- estrai(setdiff(servizi, bidoni$servizio[k]))
}

# 8 contenitori spostati di qualche centinaio di metri.
k_mossi <- sample(setdiff(candidati(12), c(k_utenza, k_servizio)), 8)
for (k in k_mossi) {
  righe <- dopo_svolta(k)
  letture$latitudine[righe] <- letture$latitudine[righe] + stats::runif(1, 0.003, 0.009) * estrai(c(-1, 1))
  letture$longitudine[righe] <- letture$longitudine[righe] + stats::runif(1, 0.003, 0.009) * estrai(c(-1, 1))
}

# ---- Campi derivati -----------------------------------------------------------

letture$targa_veicolo <- veicoli$targa[match(letture$matricola_veicolo, veicoli$matricola)]
letture$presente_a_database <- ifelse(letture$presente, "Presente", "Non Presente")
letture$volume_previsto <- bidoni$volume[letture$chiave]
letture$numero_raccolte_annue_previste <- bidoni$raccolte[letture$chiave]

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
  volume_previsto = letture$volume_previsto,
  numero_raccolte_annue_previste = letture$numero_raccolte_annue_previste,
  stringsAsFactors = FALSE
)

# ---- Controlli di conformita' -------------------------------------------------

per_rfid <- split(finale, finale$RFID)
non_presente <- finale$presente_a_database == "Non Presente"
nel_2025 <- substr(finale$giorno_lettura, 1, 4) == "2025"
stopifnot(
  nrow(finale) >= 600,
  length(per_rfid) >= 200, length(per_rfid) <= 250,
  !anyDuplicated(bidoni$RFID),
  setequal(unique(substr(finale$giorno_lettura, 1, 4)), c("2024", "2025")),
  all(substr(finale$giorno_lettura, 12, 19) >= "06:00:00"),
  all(substr(finale$giorno_lettura, 12, 19) <= "19:00:00"),
  # righe di esempio e casi speciali della prima specifica
  finale$giorno_lettura[finale$RFID == "RFD20250901001"][1] == "2025-09-01 06:00:00",
  finale$giorno_lettura[finale$RFID == "RFD20250901002"][1] == "2025-09-01 06:15:00",
  finale$giorno_lettura[finale$RFID == "RFD20250901201"][1] == "2025-09-01 06:30:00",
  identical(per_rfid$RFD20250901001$servizio_transponder[1:4], c("CARTA", "SECCO", "CARTA", "CARTA")),
  identical(substr(per_rfid$RFD20250901001$giorno_lettura[1:3], 1, 10), c("2025-09-01", "2025-09-15", "2025-09-28")),
  identical(per_rfid$RFD20250901050$id_utenza[1:4], c("UTZ001", "UTZ001", "UTZ025", "UTZ025")),
  all(per_rfid$RFD20250901050$id_utenza[-(1:2)] == "UTZ025"),
  identical(per_rfid$RFD20250915201$servizio_atteso, rep("SECCO PAP", 4)),
  nrow(per_rfid$RFD20250920250) == 2, all(is.na(per_rfid$RFD20250920250$servizio_atteso)),
  identical(per_rfid$RFD20250905100$latitudine[1:2], c("41.8500", "42.0000")),
  identical(per_rfid$RFD20250905100$longitudine[1:2], c("12.3500", "12.5000")),
  # coerenza dei campi
  all(is.na(finale$servizio_transponder) == non_presente),
  all(is.na(finale$id_utenza) == non_presente),
  all(is.na(finale$volume_previsto) == non_presente),
  all(is.na(finale$numero_raccolte_annue_previste) == non_presente),
  all(is.na(finale$servizio_atteso[!non_presente])),
  all(finale$servizio_transponder %in% c(servizi, NA)),
  all(finale$servizio_atteso %in% c(vapply(turni, function(t) t$giro, ""), NA)),
  length(unique(stats::na.omit(finale$servizio_atteso))) == 12,
  all(finale$volume_previsto %in% c(240, 770, 1100, NA)),
  all(finale$numero_raccolte_annue_previste %in% c(13, 26, NA)),
  all(vapply(per_rfid, function(x) length(unique(x$volume_previsto)) == 1, logical(1))),
  nrow(unique(finale[, c("targa_veicolo", "matricola_veicolo")])) == 5,
  length(unique(stats::na.omit(finale$id_utenza))) == 50,
  !anyNA(finale$latitudine), !anyNA(finale$longitudine),
  # esempi per l'analisi dei cluster: oltre 300 letture nel 2025
  sum(nel_2025 & finale$RFID == bidoni$RFID[k_deposito]) > 300,
  sum(nel_2025 & finale$RFID == bidoni$RFID[k_camion]) > 300
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
message(sprintf(
  "Esempi cluster: deposito %s, camion %s, letture sparse %s",
  bidoni$RFID[k_deposito], bidoni$RFID[k_camion], bidoni$RFID[k_sparso]
))
