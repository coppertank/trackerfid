# Genera inst/extdata/sample_rfid_dataset.csv: letture simulate con le
# antenne, ottobre 2024 - dicembre 2025, con le 35 colonne e i formati della
# pipeline dei dati reali. Ogni contenitore è letto a cadenza fissa dal giro
# del suo servizio, nei comuni veri del territorio, dentro il confine; ci
# sono esempi per tutti gli indicatori dei cluster e alcuni sacchetti. Seme
# fisso. Casi e numeri sono descritti in CLAUDE.md, e i test li citano.
# Dalla radice del pacchetto: Rscript data-raw/genera_sample_dataset.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))
set.seed(20250901)

# ---- Parametri generali -----------------------------------------------------

inizio_dati <- as.Date("2024-10-01")
fine_dati <- as.Date("2025-12-31")
giorni_periodo <- seq(inizio_dati, fine_dati, by = "day")

veicoli <- data.frame(
  matricola = c("1601", "1602", "1603", "1604", "1605"),
  targa = c("AB123CD", "EF456GH", "IL789MN", "OP012QR", "ST345UV"),
  stringsAsFactors = FALSE
)

# Calendario dei mezzi: giorni (1 = lunedì), fascia oraria [da, a), giro
# (`servizio_giro`; PAP = porta a porta), tipologia raccolta (`s`, NA se nessuna).
turno <- function(m, giorni, da, a, giro, s = NA_character_) {
  list(m = m, giorni = giorni, da = da, a = a, giro = giro, s = s)
}
turni <- list(
  turno("1601", c(1, 3, 5), 6, 13, "SECCO PAP", "SECCO"),
  turno("1601", c(2, 4, 6), 6, 13, "CARTA/CARTONE PAP", "CARTA"),
  turno("1601", 7, 7, 12, "CARTA CONT.STRADALI", "CARTA"),
  turno("1602", c(1, 3, 5), 7, 14, "CARTA CONT.STRADALI", "CARTA"),
  turno("1602", c(2, 4, 6), 7, 14, "VETRO PAP", "VETRO"),
  turno("1603", 1:6, 6, 12, "UMIDO PAP", "UMIDO"),
  turno("1603", c(1, 4), 13, 18, "UMIDO CONT.STRADALI", "UMIDO"),
  turno("1603", c(2, 5), 13, 18, "PLASTICA PAP", "PLASTICA E METALLI"),
  turno("1603", c(3, 6), 13, 18, "VERDE PAP", "VERDE E RAMAGLIE"),
  turno("1604", 1:6, 12, 19, "SECCO PAP", "SECCO"),
  turno("1605", c(1, 3), 6, 12, "SERV.MANIFESTAZIONE"),
  turno("1605", c(2, 4), 6, 12, "PULIZIA TERRIT."),
  turno("1605", 6, 6, 12, "SERVIZI MERCATI"),
  turno("1605", c(1, 3, 5), 13, 19, "VETRO PAP", "VETRO"),
  turno("1605", c(2, 4), 13, 19, "PLAST.CONT.STRADALI", "PLASTICA E METALLI")
)

# Servizi del database e loro quota tra i contenitori censiti.
servizi <- c(
  "SECCO",
  "CARTA",
  "VETRO",
  "UMIDO",
  "PLASTICA E METALLI",
  "VERDE E RAMAGLIE"
)
quote_servizi <- c(0.40, 0.27, 0.11, 0.10, 0.07, 0.05)
names(quote_servizi) <- servizi

# Volumi estratti solo per tenere ferma la sequenza casuale: il volume del
# file lo sceglie l'anagrafica, alla fine.
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
  as.POSIXct(paste(format(data), "00:00:00"), tz = "UTC") +
    round(ore_decimali * 3600)
}

estrai <- function(x, n = 1, ...) x[sample.int(length(x), n, ...)]

# Giro in corso per un mezzo in un istante, NA fuori orario: serve solo alle
# letture fuori orario.
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

# Turni di un mezzo in un giorno della settimana.
turni_del_giorno <- function(matricola, gs) {
  Filter(function(t) t$m == matricola && gs %in% t$giorni, turni)
}

# Giro del mezzo al momento di una lettura: il turno in cui cade, o il più
# vicino del giorno (come sui dati reali).
giro_del_mezzo <- function(matricola, quando) {
  gs <- giorno_settimana(as.Date(quando, tz = "UTC"))
  ora <- as.numeric(format(quando, "%H", tz = "UTC")) +
    as.numeric(format(quando, "%M", tz = "UTC")) / 60
  del_giorno <- turni_del_giorno(matricola, gs)
  stopifnot(length(del_giorno) > 0)
  distanza <- vapply(
    del_giorno,
    function(t) {
      if (ora >= t$da && ora < t$a) 0 else min(abs(ora - t$da), abs(ora - t$a))
    },
    numeric(1)
  )
  del_giorno[[which.min(distanza)]]$giro
}

# Posizione nel calendario del turno di `giro_del_mezzo()`.
indice_turno_del_mezzo <- function(matricola, quando) {
  gs <- giorno_settimana(as.Date(quando, tz = "UTC"))
  ora <- as.numeric(format(quando, "%H", tz = "UTC")) +
    as.numeric(format(quando, "%M", tz = "UTC")) / 60
  del_giorno <- which(vapply(
    turni,
    function(t) t$m == matricola && gs %in% t$giorni,
    logical(1)
  ))
  stopifnot(length(del_giorno) > 0)
  distanza <- vapply(
    turni[del_giorno],
    function(t) {
      if (ora >= t$da && ora < t$a) 0 else min(abs(ora - t$da), abs(ora - t$a))
    },
    numeric(1)
  )
  del_giorno[which.min(distanza)]
}

# Indici dei turni che raccolgono una tipologia.
turni_servizio <- function(servizio) {
  which(vapply(turni, function(t) !is.na(t$s) && t$s == servizio, logical(1)))
}

# Orario tipico dentro un turno: piu' letture a inizio giro.
orario_turno <- function(t) t$da + (t$a - t$da) * stats::rbeta(1, 2, 3.2)

# Date di raccolta a cadenza fissa: stesso giorno della settimana ogni
# `cadenza` giorni, a partire da `dal`, con uno sfasamento in settimane.
date_raccolta <- function(
  giorno,
  cadenza,
  dal,
  al = fine_dati,
  sfasamento = 0
) {
  candidati <- giorni_periodo[
    giorni_periodo >= dal &
      giorni_periodo <= al &
      giorno_settimana(giorni_periodo) == giorno
  ]
  if (length(candidati) == 0) {
    return(candidati)
  }
  prima <- candidati[1] + 7 * sfasamento
  if (prima > al) candidati[1] else seq(prima, al, by = cadenza)
}

# Letture di un contenitore dal suo calendario; non è esposto a ogni passaggio.
letture_calendario <- function(b, dal = b$attivo_dal, al = fine_dati) {
  t <- turni[[b$turno]]
  date <- date_raccolta(b$giorno, b$cadenza, dal, al, b$sfasamento)
  tenute <- date[stats::runif(length(date)) < b$esposizione]
  if (length(tenute) == 0) {
    tenute <- date[1]
  }
  ore <- b$ora_tipica + stats::rnorm(length(tenute), 0, 0.25)
  ore <- pmin(pmax(ore, t$da), t$a - 0.02)
  data.frame(
    data_ora_lettura = istante(tenute, ore),
    matricola_veicolo = t$m,
    stringsAsFactors = FALSE
  )
}

# Lettura in un giorno e orario casuali di un turno.
lettura_turno <- function(t, dal = inizio_dati, al = fine_dati) {
  giorni <- giorni_periodo[
    giorni_periodo >= dal &
      giorni_periodo <= al &
      giorno_settimana(giorni_periodo) %in% t$giorni
  ]
  data.frame(
    data_ora_lettura = istante(estrai(giorni), orario_turno(t)),
    matricola_veicolo = t$m,
    stringsAsFactors = FALSE
  )
}

# Lettura fuori orario: il giro è il più vicino della giornata.
lettura_fuori_turno <- function(dal = inizio_dati, al = fine_dati) {
  giorni <- giorni_periodo[giorni_periodo >= dal & giorni_periodo <= al]
  repeat {
    m <- estrai(veicoli$matricola)
    quando <- istante(estrai(giorni), stats::runif(1, 6, 19))
    if (is.na(servizio_da_calendario(m, quando))) {
      return(data.frame(
        data_ora_lettura = quando,
        matricola_veicolo = m,
        stringsAsFactors = FALSE
      ))
    }
  }
}

# ---- Posizioni: scarti attorno a centri astratti ------------------------------
# Servono solo a estrarre lo scarto dal centro abitato, poi riportato
# attorno al centro del comune assegnato.

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
  presente = c(
    rep(TRUE, n_presenti),
    rep(FALSE, n_non_presenti),
    FALSE,
    TRUE,
    TRUE
  ),
  stringsAsFactors = FALSE
)
bidoni$chiave <- seq_len(nrow(bidoni))

# Casi speciali (identificati per posizione).
k_cambio_servizio <- which(bidoni$presente & bidoni$seq == 1)
k_esempio_secco <- which(bidoni$presente & bidoni$seq == 2)
k_cambio_utenza <- which(bidoni$presente & bidoni$seq == 50)
k_spostato <- which(bidoni$presente & bidoni$seq == 100)
k_np_esempio <- which(!bidoni$presente)[1] # D250901201
k_np_stima <- which(!bidoni$presente)[2] # D250915201
k_np_incerto <- which(bidoni$seq == 250) # D250920250
# Esempi per gli indicatori dell'analisi dei cluster.
k_deposito <- which(bidoni$seq == 301) # tag fermo in deposito
k_camion <- which(bidoni$seq == 302) # tag rimasto sul camion
k_sparso <- which(bidoni$seq == 303) # letture sparse / GPS rumoroso
speciali <- c(
  k_cambio_servizio,
  k_esempio_secco,
  k_cambio_utenza,
  k_spostato,
  k_np_esempio,
  k_np_stima,
  k_np_incerto,
  k_deposito,
  k_camion,
  k_sparso
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
bidoni$servizio[da_assegnare] <- sample(rep(
  servizi,
  times = conteggi - as.integer(assegnati)
))
# Il servizio "reale" dei non censiti serve solo a simulare letture plausibili.
bidoni$servizio[non_censiti] <- sample(rep_len(servizi, length(non_censiti)))
bidoni$servizio[c(k_np_esempio, k_np_stima)] <- "SECCO"

# Utenze: 50 utenze, alcune con molti contenitori e altre con uno solo.
utenze <- sprintf("58590%03d", 1:50)
bidoni$utenza <- NA_character_
bidoni$utenza[c(k_cambio_servizio, k_cambio_utenza)] <- "58590001"
bidoni$utenza[k_esempio_secco] <- "58590002"
da_assegnare <- censiti[is.na(bidoni$utenza[censiti])]
pool <- c(
  utenze[3:50],
  sample(
    utenze[1:35],
    length(da_assegnare) - 48,
    replace = TRUE,
    prob = 1 / (1:35)^0.8
  )
)
bidoni$utenza[da_assegnare] <- sample(pool)

# Volume e raccolte annue previste (solo per i censiti).
bidoni$volume <- NA_integer_
bidoni$raccolte <- NA_integer_
for (k in censiti) {
  vol <- volumi[[bidoni$servizio[k]]]
  bidoni$volume[k] <- vol$v[sample.int(length(vol$v), 1, prob = vol$p)]
  # bisettimanale; il vetro puo' essere mensile
  bidoni$raccolte[k] <- if (bidoni$servizio[k] == "VETRO") {
    estrai(c(13L, 26L))
  } else {
    26L
  }
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
bidoni$cadenza <- ifelse(
  !is.na(bidoni$raccolte) & bidoni$raccolte == 13L,
  28L,
  14L
)
bidoni$sfasamento <- vapply(
  bidoni$cadenza,
  function(cd) sample.int(cd / 7, 1) - 1,
  numeric(1)
)
# Quota di passaggi in cui il contenitore e' esposto e viene letto.
bidoni$esposizione <- stats::runif(nrow(bidoni), 0.75, 0.98)
# Data di attivazione: quasi tutti presenti dall'inizio, alcuni arrivano nel 2025.
bidoni$attivo_dal <- inizio_dati
tardivi <- sample(setdiff(seq_len(nrow(bidoni)), speciali), 34)
bidoni$attivo_dal[tardivi] <- estrai(
  seq(as.Date("2025-01-15"), as.Date("2025-10-15"), by = "day"),
  length(tardivi)
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
bidoni$rumore[np_regolari] <- sample(rep_len(
  c(0.04, 0.04, 0.08, 0.35, 0.50),
  length(np_regolari)
))

# Posizione "di casa" di ogni contenitore, per ora come scarto astratto.
pos <- t(vapply(
  seq_len(nrow(bidoni)),
  function(i) posizione_casuale(),
  numeric(2)
))
bidoni$lat <- pos[, "lat"]
bidoni$lng <- pos[, "lng"]

# Casi con calendario fissato: prime letture scritte a mano, poi il calendario.
imposta <- function(
  k,
  turno,
  giorno,
  ora,
  dal,
  sfasamento = 0,
  esposizione = 0.95
) {
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
bidoni$raccolte[c(
  k_cambio_servizio,
  k_esempio_secco,
  k_cambio_utenza,
  k_spostato
)] <- 26L

# ---- Geografia: comuni e cantieri ---------------------------------------------
# Un giro è un turno in un giorno della settimana, con il suo comune; ogni
# contenitore sta nel comune del suo giro. Semi propri: cambiare la
# geografia non cambia date, mezzi e servizi.

comuni <- utils::read.csv(
  "inst/extdata/comuni_cantieri.csv",
  stringsAsFactors = FALSE
)
# Confini dei comuni: i vertici di ogni comune, pronti per il controllo.
confini <- split(poligoni_comuni(), poligoni_comuni()$comune)
stopifnot(setequal(names(confini), comuni$comune))
quote_cantieri <- c(
  ASIAGO = 0.12,
  BASSANO = 0.26,
  CAMPOSAMPIERO = 0.37,
  RUBANO = 0.25
)
stopifnot(setequal(names(quote_cantieri), unique(comuni$cantiere)))

# Valuta un'espressione con un seme proprio, poi ripristina la sequenza casuale.
con_seme <- function(seme, espressione) {
  stato <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", stato, envir = globalenv()))
  set.seed(seme)
  espressione
}

centro_comune <- function(comune) {
  riga <- match(comune, comuni$comune)
  stopifnot(!is.na(riga))
  c(lat = comuni$latitudine[riga], lng = comuni$longitudine[riga])
}

# Il punto e' dentro il confine del comune?
nel_comune <- function(lat, lng, comune) {
  dentro_poligoni(lng, lat, confini[[comune]])
}

# Il comune di un punto lo dà comune_del_punto() del pacchetto.

# Attorno al punto, a circa 50 m, solo comuni serviti: GPS e arrotondamento
# non devono portare letture fuori dal territorio. Vicino al confine tra due
# comuni serviti invece può stare.
nel_territorio <- function(lat, lng, margine = 0.0005) {
  !anyNA(comune_del_punto(
    lat + c(-1, 1, 0, 0) * margine,
    lng + c(0, 0, -1, 1) * margine
  ))
}

# Giri del calendario: una riga per turno e giorno della settimana.
giri <- do.call(
  rbind,
  lapply(seq_along(turni), function(indice) {
    data.frame(turno = indice, giorno = turni[[indice]]$giorni)
  })
)
giro_di <- function(turno, giorno) {
  match(paste(turno, giorno), paste(giri$turno, giri$giorno))
}
bidoni$giro <- giro_di(bidoni$turno, bidoni$giorno)
stopifnot(!anyNA(bidoni$giro))
giri$bidoni <- tabulate(bidoni$giro, nrow(giri))

# Giri con il comune fissato, per i casi speciali.
giri$comune <- NA_character_
# - I contenitori delle righe di esempio stanno a Bassano del Grappa: li
#   leggono il lunedi' il turno 1, il martedi' il turno 2, la domenica il 3.
giri$comune[giro_di(c(1, 2, 3), c(1, 2, 7))] <- "BASSANO DEL GRAPPA"
# - Il contenitore con il cambio di utenza e' letto dal turno 10 il sabato e,
#   nelle prime letture, il lunedi': stesso comune.
giri$comune[giro_di(c(10, 10), c(1, 6))] <- "CAMPOSAMPIERO"
# - Il contenitore spostato: la prima lettura e' del giro del venerdi' del
#   turno 1, a Limena; dopo lo spostamento lo legge a Cittadella il giro del
#   venerdi' del turno 4.
giri$comune[giro_di(1, 5)] <- "LIMENA"
giri$comune[giro_di(4, 5)] <- "CITTADELLA"
fissati <- !is.na(giri$comune)
giri$cantiere <- comuni$cantiere[match(giri$comune, comuni$comune)]

# Cantiere degli altri giri: dai più carichi, al cantiere più lontano dalla quota.
obiettivo <- quote_cantieri * nrow(bidoni)
assegnati <- vapply(
  names(quote_cantieri),
  function(cantiere) {
    sum(giri$bidoni[fissati & giri$cantiere %in% cantiere])
  },
  numeric(1)
)
for (g in setdiff(order(-giri$bidoni, seq_len(nrow(giri))), which(fissati))) {
  scelto <- names(which.max(obiettivo - assegnati))
  giri$cantiere[g] <- scelto
  assegnati[scelto] <- assegnati[scelto] + giri$bidoni[g]
}

# Comune degli altri giri: a sorte nel cantiere, senza ripetizioni finché si
# può, con peso quadruplo per il comune che dà il nome al cantiere.
giri$comune[!fissati] <- con_seme(2027, {
  comune <- rep(NA_character_, nrow(giri))
  for (cantiere in names(quote_cantieri)) {
    quali <- which(!fissati & giri$cantiere == cantiere)
    scelta <- setdiff(
      comuni$comune[comuni$cantiere == cantiere],
      giri$comune[fissati]
    )
    ordine <- scelta[sample.int(
      length(scelta),
      prob = ifelse(startsWith(scelta, cantiere), 4, 1)
    )]
    comune[quali] <- rep_len(ordine, length(quali))
  }
  comune[!fissati]
})

# Giro di zona: i contenitori stanno a Pove del Grappa, nel file il comune
# del giro è MASSICCIO DEL GRAPPA, fuori tabella.
giro_di_zona <- which(giri$comune == "POVE DEL GRAPPA")
stopifnot(length(giro_di_zona) == 1)
giri$nome_calendario <- giri$comune
giri$nome_calendario[giro_di_zona] <- "MASSICCIO DEL GRAPPA"
stopifnot(!anyNA(giri$comune), !anyNA(giri$cantiere))

# Ogni contenitore sta nel comune del proprio giro.
bidoni$comune <- giri$comune[bidoni$giro]
# Il contenitore spostato è registrato a Limena, il tag sul mezzo gira a
# Marostica, il deposito è a Camposampiero.
bidoni$comune[k_spostato] <- "LIMENA"
bidoni$comune[k_camion] <- "MAROSTICA"
bidoni$comune[k_deposito] <- "CAMPOSAMPIERO"
bidoni$cantiere <- comuni$cantiere[match(bidoni$comune, comuni$comune)]

# Scarto dal centro astratto, fino a 1,5 km, riportato attorno al centro del
# comune; dimezzato finché il punto sta nel comune e lontano dal confine del
# territorio.
for (i in seq_len(nrow(bidoni))) {
  zona <- which.min((zone$lat - bidoni$lat[i])^2 + (zone$lng - bidoni$lng[i])^2)
  scarto <- c(bidoni$lat[i] - zone$lat[zona], bidoni$lng[i] - zone$lng[zona])
  scarto <- scarto * min(1, 0.015 / sqrt(sum(scarto^2)))
  centro <- centro_comune(bidoni$comune[i])
  while (
    !(nel_comune(
      centro[["lat"]] + scarto[1],
      centro[["lng"]] + scarto[2],
      bidoni$comune[i]
    ) &&
      nel_territorio(centro[["lat"]] + scarto[1], centro[["lng"]] + scarto[2]))
  ) {
    scarto <- scarto / 2
  }
  bidoni$lat[i] <- centro[["lat"]] + scarto[1]
  bidoni$lng[i] <- centro[["lng"]] + scarto[2]
}

# Luoghi dei casi speciali, con quattro decimali come nel file.
# Il contenitore spostato: registrato a Limena, poi portato a Cittadella.
spostato_da <- round(centro_comune("LIMENA"), 4)
spostato_a <- round(centro_comune("CITTADELLA"), 4)
# Il deposito dei mezzi, a Camposampiero.
deposito <- round(centro_comune("CAMPOSAMPIERO") + c(-0.006, 0.008), 4)
stopifnot(nel_comune(deposito[["lat"]], deposito[["lng"]], "CAMPOSAMPIERO"))

# ---- Generazione delle letture ----------------------------------------------

# Il tag sul mezzo: una scia di meno di 6 km da Marostica a Nove e verso Bassano.
verso_bassano <- centro_comune("NOVE") +
  0.45 * (centro_comune("BASSANO DEL GRAPPA") - centro_comune("NOVE"))
tappe <- cbind(centro_comune("MAROSTICA"), centro_comune("NOVE"), verso_bassano)
percorso <- data.frame(lat = tappe["lat", ], lng = tappe["lng", ])
punto_sul_percorso <- function(n) {
  tratti <- nrow(percorso) - 1
  lunghezze <- sqrt(
    diff(percorso$lat)^2 + (diff(percorso$lng) * cos(45.75 * pi / 180))^2
  )
  tratto <- sample.int(tratti, n, replace = TRUE, prob = lunghezze)
  f <- stats::runif(n)
  data.frame(
    latitudine = percorso$lat[tratto] +
      f * diff(percorso$lat)[tratto] +
      rumore_gps(n),
    longitudine = percorso$lng[tratto] +
      f * diff(percorso$lng)[tratto] +
      rumore_gps(n)
  )
}

manuali <- function(quando, matricola, ...) {
  data.frame(
    data_ora_lettura = as.POSIXct(quando, tz = "UTC"),
    matricola_veicolo = matricola,
    ...,
    stringsAsFactors = FALSE
  )
}
aggiungi <- function(prime, altre, ...) {
  extra <- list(...)
  for (campo in names(extra)) {
    altre[[campo]] <- extra[[campo]]
  }
  for (campo in setdiff(names(prime), names(altre))) {
    altre[[campo]] <- NA
  }
  rbind(prime, altre[, names(prime), drop = FALSE])
}

turno_garantito <- 0

genera_letture <- function(b) {
  if (b$chiave == k_cambio_servizio) {
    # CARTA -> SECCO -> CARTA, poi resta CARTA.
    out <- aggiungi(
      manuali(
        c("2025-09-01 06:00:00", "2025-09-15 08:42:17", "2025-09-28 09:21:44"),
        "1601",
        servizio_database = c("CARTA", "SECCO", "CARTA")
      ),
      letture_calendario(b),
      servizio_database = "CARTA"
    )
  } else if (b$chiave == k_cambio_utenza) {
    # Passa da 58590001 a 58590025 il 20 settembre.
    out <- aggiungi(
      manuali(
        c(
          "2025-09-01 12:48:05",
          "2025-09-08 13:10:31",
          "2025-09-20 12:52:49",
          "2025-09-27 13:05:12"
        ),
        "1604",
        id_utenza = c("58590001", "58590001", "58590025", "58590025")
      ),
      letture_calendario(b),
      id_utenza = "58590025"
    )
  } else if (b$chiave == k_spostato) {
    # Prima lettura a Limena, dalla seconda in poi a Cittadella: circa 20 km.
    altre <- letture_calendario(b)
    out <- aggiungi(
      manuali(
        c("2025-09-05 08:14:26", "2025-09-19 10:37:53"),
        c("1601", "1602"),
        latitudine = c(spostato_da[["lat"]], spostato_a[["lat"]]),
        longitudine = c(spostato_da[["lng"]], spostato_a[["lng"]])
      ),
      altre,
      latitudine = spostato_a[["lat"]] + rumore_gps(nrow(altre)),
      longitudine = spostato_a[["lng"]] + rumore_gps(nrow(altre))
    )
  } else if (b$chiave == k_np_stima) {
    out <- manuali(
      c(
        "2025-09-15 07:26:40",
        "2025-09-17 07:31:08",
        "2025-09-22 14:12:55",
        "2025-09-26 07:19:33"
      ),
      c("1601", "1601", "1604", "1601")
    )
  } else if (b$chiave == k_np_incerto) {
    # Letto un sabato dal giro del secco e il sabato dopo dal mezzo della
    # carta: due tipologie alla pari, la stima resta incerta.
    out <- manuali(
      c("2025-09-20 15:44:02", "2025-09-27 16:08:37"),
      c("1604", "1601")
    )
  } else if (b$chiave %in% c(k_esempio_secco, k_np_esempio)) {
    prima <- if (b$chiave == k_esempio_secco) {
      "2025-09-01 06:15:00"
    } else {
      "2025-09-01 06:30:00"
    }
    out <- aggiungi(manuali(prima, "1601"), letture_calendario(b))
  } else if (b$chiave == k_deposito) {
    # Tag fermo vicino al lettore del deposito: letto ogni giorno lavorativo
    # dai mezzi in uscita o al rientro.
    feriali <- giorni_periodo[giorno_settimana(giorni_periodo) <= 6]
    giorni <- rep(
      feriali,
      times = sample(1:2, length(feriali), replace = TRUE, prob = c(0.65, 0.35))
    )
    uscita <- stats::runif(length(giorni)) < 0.7
    ore <- ifelse(
      uscita,
      stats::runif(length(giorni), 6.0, 6.4),
      stats::runif(length(giorni), 18.5, 18.98)
    )
    out <- data.frame(
      data_ora_lettura = istante(giorni, ore),
      matricola_veicolo = sample(
        veicoli$matricola,
        length(giorni),
        replace = TRUE
      ),
      latitudine = deposito[["lat"]] + rumore_gps(length(giorni), 0.00004),
      longitudine = deposito[["lng"]] + rumore_gps(length(giorni), 0.00004),
      stringsAsFactors = FALSE
    )
  } else if (b$chiave == k_camion) {
    # Tag rimasto nel cassone di 1604: due letture per giorno lavorativo,
    # ogni volta in un punto diverso del giro.
    feriali <- giorni_periodo[
      giorni_periodo >= as.Date("2025-02-03") &
        giorno_settimana(giorni_periodo) <= 6
    ]
    giorni <- rep(feriali, each = 2)
    out <- cbind(
      data.frame(
        data_ora_lettura = istante(
          giorni,
          stats::runif(length(giorni), 12, 18.98)
        ),
        matricola_veicolo = "1604",
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
      altre <- do.call(
        rbind,
        lapply(seq_len(n), function(i) {
          if (stats::runif(1) < 0.2) {
            lettura_fuori_turno(b$attivo_dal)
          } else {
            lettura_turno(turni[[b$turno]], b$attivo_dal)
          }
        })
      )
    }
    out <- rbind(prima, altre)
  }

  out <- out[order(out$data_ora_lettura), , drop = FALSE]
  out <- out[!duplicated(out$data_ora_lettura), , drop = FALSE]
  n <- nrow(out)
  completa <- function(campo, valore) {
    if (is.null(out[[campo]])) {
      out[[campo]] <<- valore
    }
    mancanti <- is.na(out[[campo]])
    out[[campo]][mancanti] <<- rep_len(valore, n)[mancanti]
  }
  completa("servizio_database", if (b$presente) b$servizio else NA_character_)
  completa("id_utenza", if (b$presente) b$utenza else NA_character_)
  completa("latitudine", b$lat + rumore_gps(n))
  completa("longitudine", b$lng + rumore_gps(n))
  out$chiave <- b$chiave
  out$presente <- b$presente
  out[, c(
    "data_ora_lettura",
    "matricola_veicolo",
    "servizio_database",
    "id_utenza",
    "latitudine",
    "longitudine",
    "chiave",
    "presente"
  )]
}

letture <- do.call(
  rbind,
  lapply(seq_len(nrow(bidoni)), function(i) genera_letture(bidoni[i, ]))
)
rownames(letture) <- NULL

# Coordinate esatte delle righe di esempio della specifica: tre contenitori
# vicini, nel centro di Bassano del Grappa.
bassano <- round(centro_comune("BASSANO DEL GRAPPA"), 4)
coord_esempio <- list(
  list(k = k_cambio_servizio, lat = bassano[["lat"]], lng = bassano[["lng"]]),
  list(
    k = k_esempio_secco,
    lat = bassano[["lat"]] + 0.0007,
    lng = bassano[["lng"]] + 0.0007
  ),
  list(
    k = k_np_esempio,
    lat = bassano[["lat"]] + 0.0014,
    lng = bassano[["lng"]] + 0.0014
  )
)
for (ce in coord_esempio) {
  righe <- which(letture$chiave == ce$k)
  righe <- righe[order(letture$data_ora_lettura[righe])]
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
  which(
    letture$chiave == k & as.Date(letture$data_ora_lettura, tz = "UTC") >= svolta
  )
}

# Altri 6 contenitori che cambiano utenza (7 in totale con il caso speciale).
k_utenza <- sample(candidati(12), 6)
for (k in k_utenza) {
  letture$id_utenza[dopo_svolta(k)] <- estrai(setdiff(utenze, bidoni$utenza[k]))
}

# Altri 2 contenitori che cambiano servizio in modo definitivo.
k_servizio <- sample(setdiff(candidati(12), k_utenza), 2)
for (k in k_servizio) {
  letture$servizio_database[dopo_svolta(k)] <- estrai(setdiff(
    servizi,
    bidoni$servizio[k]
  ))
}

# 8 contenitori spostati di qualche centinaio di metri.
k_mossi <- sample(setdiff(candidati(12), c(k_utenza, k_servizio)), 8)
for (k in k_mossi) {
  righe <- dopo_svolta(k)
  letture$latitudine[righe] <- letture$latitudine[righe] +
    stats::runif(1, 0.003, 0.009) * estrai(c(-1, 1))
  letture$longitudine[righe] <- letture$longitudine[righe] +
    stats::runif(1, 0.003, 0.009) * estrai(c(-1, 1))
}

# ---- Sacchetti ----------------------------------------------------------------
# Codice di 24 caratteri con prefisso 00BD, solo la fornitura del gruppo a
# database, una lettura in un giro del secco. Semi propri.

sacchetti <- data.frame(
  # progressivo nel codice: da 1 i censiti, da 101 i non censiti
  n = c(1:8, 101:104),
  fornitura = c(
    "200000001",
    "200000001",
    "200000001",
    "200000002",
    "200000002",
    "200000003",
    "200000003",
    "200000004",
    rep(NA_character_, 4)
  ),
  # i due giri "SECCO PAP" del calendario
  turno = c(1, 1, 1, 10, 10, 10, 10, 1, 1, 10, 10, 1),
  # giorno della raccolta, cioe' dell'unica lettura
  giorno = as.Date(c(
    "2024-11-18",
    "2025-03-17",
    "2025-06-09",
    "2025-04-08",
    "2025-09-16",
    "2025-05-22",
    "2025-10-30",
    "2025-07-11",
    "2024-10-23",
    "2025-08-05",
    "2025-11-13",
    "2025-12-05"
  )),
  stringsAsFactors = FALSE
)
sacchetti$RFID <- sprintf("00BD%020d", sacchetti$n)
# Un sacchetto sta nel comune del giro che lo raccoglie.
sacchetti$comune <- giri$comune[giro_di(
  sacchetti$turno,
  giorno_settimana(sacchetti$giorno)
)]
# I sacchetti di una fornitura vengono lasciati davanti alla stessa porta, in
# giorni diversi.
sacchetti$porta <- ifelse(
  is.na(sacchetti$fornitura),
  sacchetti$RFID,
  sacchetti$fornitura
)
stopifnot(
  all(nchar(sacchetti$RFID) == 24),
  all(vapply(turni[sacchetti$turno], function(t) t$giro, "") == "SECCO PAP"),
  all(mapply(
    function(g, t) g %in% turni[[t]]$giorni,
    giorno_settimana(sacchetti$giorno),
    sacchetti$turno
  )),
  all(sacchetti$giorno >= inizio_dati & sacchetti$giorno <= fine_dati)
)

# Posizione di ogni porta, dentro il confine del comune, e ora in cui il mezzo
# ci passa.
porte <- unique(sacchetti[, c("porta", "comune", "turno")])
stopifnot(!anyDuplicated(porte$porta))
porte <- con_seme(2028, {
  for (i in seq_len(nrow(porte))) {
    centro <- centro_comune(porte$comune[i])
    scarto <- stats::rnorm(2, 0, 0.004)
    while (
      !(nel_comune(
        centro[["lat"]] + scarto[1],
        centro[["lng"]] + scarto[2],
        porte$comune[i]
      ) &&
        nel_territorio(
          centro[["lat"]] + scarto[1],
          centro[["lng"]] + scarto[2]
        ))
    ) {
      scarto <- scarto / 2
    }
    porte$lat[i] <- centro[["lat"]] + scarto[1]
    porte$lng[i] <- centro[["lng"]] + scarto[2]
    porte$ora[i] <- orario_turno(turni[[porte$turno[i]]])
  }
  porte
})

letture_sacchetti <- con_seme(
  2029,
  do.call(
    rbind,
    lapply(seq_len(nrow(sacchetti)), function(i) {
      s <- sacchetti[i, ]
      p <- porte[match(s$porta, porte$porta), ]
      t <- turni[[s$turno]]
      ora <- p$ora + stats::rnorm(1, 0, 0.25)
      data.frame(
        data_ora_lettura = istante(s$giorno, min(max(ora, t$da), t$a - 0.02)),
        matricola_veicolo = t$m,
        servizio_database = NA_character_,
        id_utenza = NA_character_,
        id_fornitura = s$fornitura,
        latitudine = p$lat + rumore_gps(1),
        longitudine = p$lng + rumore_gps(1),
        chiave = NA_integer_,
        presente = !is.na(s$fornitura),
        RFID = s$RFID,
        stringsAsFactors = FALSE
      )
    })
  )
)

# Le letture dei sacchetti portano già il loro codice.
letture$RFID <- NA_character_
letture$id_fornitura <- NA_character_
letture <- rbind(letture, letture_sacchetti[, names(letture)])
rownames(letture) <- NULL

# ---- Campi derivati: da qui niente numeri casuali -----------------------------

# La domenica lavora un solo mezzo: le letture date a un mezzo fermo passano
# al primo in servizio.
data_ora_lettura_gs <- giorno_settimana(as.Date(
  letture$data_ora_lettura,
  tz = "UTC"
))
fermo <- which(
  !mapply(
    function(m, gs) length(turni_del_giorno(m, gs)) > 0,
    letture$matricola_veicolo,
    data_ora_lettura_gs
  )
)
for (i in fermo) {
  letture$matricola_veicolo[i] <- Find(
    function(m) length(turni_del_giorno(m, data_ora_lettura_gs[i])) > 0,
    veicoli$matricola
  )
}

letture$targa_veicolo <- veicoli$targa[match(
  letture$matricola_veicolo,
  veicoli$matricola
)]
letture$presente_a_database <- ifelse(
  letture$presente,
  "PIA",
  "NP"
)
letture$volume_previsto <- bidoni$volume[letture$chiave]
letture$numero_raccolte_annue_previste <- bidoni$raccolte[letture$chiave]

# Giro del mezzo al momento della lettura, con il nome del calendario.
letture$servizio_atteso <- vapply(
  seq_len(nrow(letture)),
  function(i) {
    giro_del_mezzo(letture$matricola_veicolo[i], letture$data_ora_lettura[i])
  },
  character(1)
)

# Comune della fornitura (solo censiti, non cambia se il bidone è spostato),
# del giro, di lettura (dal punto), coerenza e cantiere con le funzioni
# dell'app.
letture$comune_fornitura <- ifelse(
  letture$presente,
  bidoni$comune[letture$chiave],
  NA_character_
)
turno_lettura <- vapply(
  seq_len(nrow(letture)),
  function(i) {
    indice_turno_del_mezzo(
      letture$matricola_veicolo[i],
      letture$data_ora_lettura[i]
    )
  },
  numeric(1)
)
# Il giro è una riga della tabella `giri`; il suo numero dà `id_giro`.
letture$id_giro <- giro_di(
  turno_lettura,
  giorno_settimana(as.Date(letture$data_ora_lettura, tz = "UTC"))
)
letture$comune_giro <- giri$nome_calendario[letture$id_giro]
# Giro sbagliato: il 24/9/2025 il calendario dava al turno 1 il giro del
# venerdì, a Limena, ma il mezzo ha fatto quello del mercoledì.
giro_sbagliato <- list(giorno = as.Date("2025-09-24"), turno = 1, previsto = 5)
del_giro_sbagliato <- turno_lettura == giro_sbagliato$turno &
  as.Date(letture$data_ora_lettura, tz = "UTC") == giro_sbagliato$giorno
letture$id_giro[del_giro_sbagliato] <- giro_di(
  giro_sbagliato$turno,
  giro_sbagliato$previsto
)
letture$comune_giro[del_giro_sbagliato] <- giri$nome_calendario[
  letture$id_giro[del_giro_sbagliato]
]
stopifnot(
  giorno_settimana(giro_sbagliato$giorno) == 3,
  sum(del_giro_sbagliato) >= 8,
  giri$comune[giro_di(giro_sbagliato$turno, giro_sbagliato$previsto)] ==
    "LIMENA",
  giri$comune[giro_di(giro_sbagliato$turno, 3)] != "LIMENA"
)
# Comune di lettura dalle coordinate del file; distanza con la funzione della
# pipeline, cantiere e coerenza con quella dell'app.
geografia <- data.frame(
  presente_a_database = letture$presente_a_database,
  comune_fornitura = letture$comune_fornitura,
  comune_giro = letture$comune_giro,
  latitudine = as.numeric(sprintf("%.4f", letture$latitudine)),
  longitudine = as.numeric(sprintf("%.4f", letture$longitudine)),
  stringsAsFactors = FALSE
)
geografia$comune_lettura <- comune_del_punto(
  geografia$latitudine,
  geografia$longitudine
)
geografia$distanza_comune_dichiarato <- distanza_comune_dichiarato(geografia)
con_geografia <- aggiungi_geografia(geografia)
letture$distanza_comune_dichiarato <- geografia$distanza_comune_dichiarato
letture$comune_lettura <- con_geografia$comune_lettura
letture$coerenza_comune <- con_geografia$coerenza_comune
letture$cantiere <- con_geografia$cantiere

# RFID: "D" + AAMMGG della prima lettura + progressivo: D250901001.
prima_lettura <- tapply(letture$data_ora_lettura, letture$chiave, min)
prima_lettura <- as.POSIXct(prima_lettura, origin = "1970-01-01", tz = "UTC")
bidoni$RFID <- sprintf(
  "D%s%03d",
  format(prima_lettura[as.character(bidoni$chiave)], "%y%m%d", tz = "UTC"),
  bidoni$seq
)
dei_bidoni <- !is.na(letture$chiave)
letture$RFID[dei_bidoni] <- bidoni$RFID[letture$chiave[dei_bidoni]]

letture <- letture[order(letture$data_ora_lettura, letture$RFID), ]

# Una fornitura per contenitore, utenza e servizio, numerate per prima lettura.
del_contenitore <- letture$presente & !is.na(letture$chiave)
chiave_fornitura <- paste(
  letture$RFID,
  letture$id_utenza,
  letture$servizio_database,
  sep = "|"
)
forniture <- unique(chiave_fornitura[del_contenitore])
letture$id_fornitura[del_contenitore] <- sprintf(
  "11600%04d",
  match(chiave_fornitura[del_contenitore], forniture)
)
# Servizio atteso e coerenza con le regole della pipeline.
giorno <- as.Date(letture$data_ora_lettura, tz = "UTC")
letture$servizio_giro <- letture$servizio_atteso
letture$servizio_atteso <- servizio_atteso_dal_giro(data.frame(
  data_lettura = giorno,
  id_giro = letture$id_giro,
  servizio_giro = letture$servizio_giro,
  servizio_transponder = letture$servizio_database,
  stringsAsFactors = FALSE
))
letture$servizio_transponder <- letture$servizio_database
letture$coerenza_servizio <- coerenza_servizio(letture)

# Anagrafica che l'app non usa, nei formati aziendali, con semi a parte.
con_contenitore <- !is.na(letture$servizio_database)
letture$frequenza <- ifelse(
  con_contenitore,
  c("26" = "QUINDICINALE", "13" = "MENSILE")[
    as.character(letture$numero_raccolte_annue_previste)
  ],
  NA_character_
)
# Utenze domestiche e non domestiche: una su cinque e' non domestica.
numero_utenza <- suppressWarnings(as.integer(substr(letture$id_utenza, 6, 8)))
domestica_utenza <- ifelse(con_contenitore, numero_utenza %% 5 != 0, NA)

# Volume: domestiche 40 (umido, vetro), 120 (carta, plastica, verde), 40-120
# (secco); non domestiche 1100, salvo l'umido. Vale quello della prima fornitura.
volume_contenitore <- con_seme(2032, {
  righe <- which(con_contenitore)
  prima <- righe[!duplicated(letture$RFID[righe])]
  servizio <- letture$servizio_database[prima]
  secco_domestico <- sample(
    c(120, 60, 40),
    length(prima),
    replace = TRUE,
    prob = c(0.7, 0.2, 0.1)
  )
  volume <- ifelse(
    domestica_utenza[prima],
    ifelse(
      servizio %in% c("UMIDO", "VETRO"),
      40,
      ifelse(servizio == "SECCO", secco_domestico, 120)
    ),
    ifelse(servizio == "UMIDO", 120, 1100)
  )
  stats::setNames(volume, letture$RFID[prima])
})
letture$volume_previsto <- ifelse(
  con_contenitore,
  unname(volume_contenitore[letture$RFID]),
  NA_real_
)

# Date di ogni associazione contenitore-fornitura: la prima da un mese prima
# della prima lettura, le altre dalla loro prima lettura; l'ultima è aperta
# (2099-12-31), le altre finiscono all'ultima lettura.
righe <- which(con_contenitore)
chiave_associazione <- paste(letture$RFID, letture$id_fornitura, sep = "|")
estremi <- function(funzione) {
  valori <- tapply(
    as.numeric(giorno[righe]),
    chiave_associazione[righe],
    funzione
  )
  stats::setNames(
    as.Date(as.numeric(valori), origin = "1970-01-01"),
    names(valori)
  )
}
inizio_associazione <- estremi(min)
fine_associazione <- estremi(max)[names(inizio_associazione)]
rfid_associazione <- sub("[|].*$", "", names(inizio_associazione))
prima_associazione <- inizio_associazione ==
  as.Date(
    stats::ave(as.numeric(inizio_associazione), rfid_associazione, FUN = min),
    origin = "1970-01-01"
  )
ultima_associazione <- fine_associazione ==
  as.Date(
    stats::ave(as.numeric(fine_associazione), rfid_associazione, FUN = max),
    origin = "1970-01-01"
  )
a <- match(chiave_associazione, names(inizio_associazione))
letture$data_attivazione_contenitore <- ifelse(
  con_contenitore,
  format(
    ifelse(prima_associazione, inizio_associazione - 30, inizio_associazione)[
      a
    ] +
      as.Date("1970-01-01")
  ),
  NA_character_
)
letture$data_cessazione_contenitore <- ifelse(
  con_contenitore,
  ifelse(ultima_associazione, "2099-12-31", format(fine_associazione))[a],
  NA_character_
)
letture$stato_fornitura <- ifelse(
  con_contenitore,
  ifelse(ultima_associazione, "ATTIVA", "CESSATA")[a],
  NA_character_
)

# Nucleo e categoria (come associa_componenti()) e potenziamento (come
# associa_potenziamento()), con un seme a parte. Domestica: 31, IA_31, D;
# non domestica: altra categoria, IA_50, ND, nucleo 0. Qualche domestica
# senza decorrenza. Una domestica del secco cambia nucleo il 1/7/2025,
# un'altra ha il potenziamento dal 1/6/2025.
forniture_nucleo <- con_seme(2031, {
  elenco <- sort(unique(stats::na.omit(letture$id_fornitura[con_contenitore])))
  domestica <- domestica_utenza[con_contenitore][
    match(elenco, letture$id_fornitura[con_contenitore])
  ]
  n <- length(elenco)
  residenti <- sample(
    0:6,
    n,
    replace = TRUE,
    prob = c(0.04, 0.28, 0.28, 0.18, 0.14, 0.05, 0.03)
  )
  non_residenti <- ifelse(
    residenti == 0,
    sample(1:3, n, replace = TRUE),
    ifelse(stats::runif(n) < 0.1, sample(1:2, n, replace = TRUE), 0L)
  )
  manovra <- ifelse(
    domestica & stats::runif(n) < 0.15,
    sample(
      c(
        "MANOVRA SOCIALE NEONATI",
        "MANOVRA SOCIALE ANZIANI",
        "MANOVRA SOCIALE DISABILI"
      ),
      n,
      replace = TRUE
    ),
    NA_character_
  )
  categoria_non_domestica <- sample(c("44", "68", "71"), n, replace = TRUE)
  senza_componenti <- domestica & stats::runif(n) < 0.03
  componente <- function(valore) ifelse(senza_componenti, NA, valore)
  data.frame(
    id_fornitura = elenco,
    nucleo_residenti = componente(ifelse(domestica, residenti, 0L)),
    nucleo_non_residenti = componente(ifelse(domestica, non_residenti, 0L)),
    tipo_categoria = componente(ifelse(domestica, "IA_31", "IA_50")),
    categoria = componente(ifelse(domestica, "31", categoria_non_domestica)),
    sottocategoria = componente(ifelse(domestica, "D", "ND")),
    manovra_sociale = manovra,
    stringsAsFactors = FALSE
  )
})
riga_fornitura <- match(letture$id_fornitura, forniture_nucleo$id_fornitura)
for (campo in c(
  "nucleo_residenti",
  "nucleo_non_residenti",
  "tipo_categoria",
  "categoria",
  "sottocategoria",
  "manovra_sociale"
)) {
  letture[[campo]] <- forniture_nucleo[[campo]][riga_fornitura]
}
# Le due forniture del secco che cambiano: le prime domestiche senza manovra
# lette prima e dopo l'estate 2025.
secco_2025 <- con_contenitore &
  letture$servizio_database %in% "SECCO" &
  format(giorno, "%Y") == "2025" &
  letture$sottocategoria %in% "D" &
  is.na(letture$manovra_sociale)
prima_estate <- unique(letture$id_fornitura[
  secco_2025 & giorno < as.Date("2025-06-01")
])
dopo_estate <- unique(letture$id_fornitura[
  secco_2025 & giorno >= as.Date("2025-07-01")
])
che_cambiano <- utils::head(sort(intersect(prima_estate, dopo_estate)), 2)
cambia_nucleo <- letture$id_fornitura %in%
  che_cambiano[1] &
  giorno >= as.Date("2025-07-01")
letture$nucleo_residenti[cambia_nucleo] <- letture$nucleo_residenti[
  cambia_nucleo
] +
  1L
nuova_manovra <- letture$id_fornitura %in%
  che_cambiano[2] &
  giorno >= as.Date("2025-06-01")
letture$manovra_sociale[nuova_manovra] <- "MANOVRA SOCIALE NEONATI"
letture$potenziamento_secco <- ifelse(
  is.na(letture$id_fornitura),
  NA,
  !is.na(letture$manovra_sociale)
)
letture$nucleo_totale <- letture$nucleo_residenti + letture$nucleo_non_residenti

# Conferma dei giri: NO negli ultimi sette giorni, a volte mancante, uguale
# per un giro in un giorno. id_giro = 1800 + riga di `giri`.
giro_giorno <- paste(letture$id_giro, format(giorno))
elenco_giri <- unique(giro_giorno)
eseguito_giro <- con_seme(2033, {
  giorno_giro <- as.Date(sub("^[0-9]+ ", "", elenco_giri))
  esito <- ifelse(giorno_giro > fine_dati - 7, "NO", "SI")
  esito[esito == "SI" & stats::runif(length(esito)) < 0.02] <- NA
  esito
})
letture$eseguito <- eseguito_giro[match(giro_giorno, elenco_giri)]

finale <- data.frame(
  data_ora_lettura = format(
    letture$data_ora_lettura,
    "%Y-%m-%d %H:%M:%S",
    tz = "UTC"
  ),
  data_lettura = format(giorno),
  targa_veicolo = letture$targa_veicolo,
  matricola_veicolo = letture$matricola_veicolo,
  id_giro = 1800L + letture$id_giro,
  eseguito = letture$eseguito,
  RFID = letture$RFID,
  presente_a_database = letture$presente_a_database,
  servizio_transponder = letture$servizio_transponder,
  servizio_atteso = letture$servizio_atteso,
  coerenza_servizio = letture$coerenza_servizio,
  servizio_giro = letture$servizio_giro,
  volume = letture$volume_previsto,
  frequenza = letture$frequenza,
  numero_raccolte_annue = letture$numero_raccolte_annue_previste,
  comune_fornitura = letture$comune_fornitura,
  comune_giro = letture$comune_giro,
  comune_lettura = letture$comune_lettura,
  coerenza_comune = letture$coerenza_comune,
  distanza_comune_dichiarato = letture$distanza_comune_dichiarato,
  cantiere = letture$cantiere,
  data_attivazione_contenitore = letture$data_attivazione_contenitore,
  data_cessazione_contenitore = letture$data_cessazione_contenitore,
  id_fornitura = letture$id_fornitura,
  stato_fornitura = letture$stato_fornitura,
  id_utenza = letture$id_utenza,
  latitudine = sprintf("%.4f", letture$latitudine),
  longitudine = sprintf("%.4f", letture$longitudine),
  nucleo_residenti = letture$nucleo_residenti,
  nucleo_non_residenti = letture$nucleo_non_residenti,
  nucleo_totale = letture$nucleo_totale,
  tipo_categoria = letture$tipo_categoria,
  categoria = letture$categoria,
  sottocategoria = letture$sottocategoria,
  potenziamento_secco = letture$potenziamento_secco,
  stringsAsFactors = FALSE
)

# ---- Controlli di conformita' -------------------------------------------------

per_rfid <- split(finale, finale$RFID)
non_presente <- finale$presente_a_database == "NP"
nel_2025 <- substr(finale$data_ora_lettura, 1, 4) == "2025"
# Le righe dei sacchetti, riconosciute dal codice con la regola dell'app.
sacchetto <- rfid_sacchetto(finale$RFID)
n_bidoni <- sum(!rfid_sacchetto(names(per_rfid)))
# Stima del servizio dei non censiti nel 2025, con la regola dell'app: netta
# se una tipologia raggiunge l'80% delle letture, altrimenti incerta.
stime_2025 <- servizio_prevalente_non_censiti(finale[nel_2025, ])
stopifnot(
  nrow(finale) >= 600,
  n_bidoni >= 200,
  n_bidoni <= 250,
  !anyDuplicated(bidoni$RFID),
  !any(rfid_sacchetto(bidoni$RFID)),
  # sacchetti: riconosciuti tutti dal codice, letti una volta da un giro del
  # secco nel loro comune, censiti con la sola fornitura
  setequal(unique(finale$RFID[sacchetto]), sacchetti$RFID),
  # monouso: una lettura per sacchetto
  sum(sacchetto) == nrow(sacchetti),
  !anyDuplicated(finale$RFID[sacchetto]),
  all(finale$servizio_atteso[sacchetto] == "SECCO"),
  all(
    finale$comune_giro[sacchetto] ==
      sacchetti$comune[match(finale$RFID[sacchetto], sacchetti$RFID)]
  ),
  all(finale$comune_lettura[sacchetto] == finale$comune_giro[sacchetto]),
  identical(
    sort(unique(finale$RFID[sacchetto & !non_presente])),
    sacchetti$RFID[!is.na(sacchetti$fornitura)]
  ),
  setequal(
    stats::na.omit(finale$id_fornitura[sacchetto]),
    stats::na.omit(sacchetti$fornitura)
  ),
  all(is.na(finale$id_utenza[sacchetto])),
  setequal(unique(substr(finale$data_ora_lettura, 1, 4)), c("2024", "2025")),
  all(substr(finale$data_ora_lettura, 12, 19) >= "06:00:00"),
  all(substr(finale$data_ora_lettura, 12, 19) <= "19:00:00"),
  # righe di esempio e casi speciali della prima specifica
  finale$data_ora_lettura[finale$RFID == "D250901001"][1] ==
    "2025-09-01 06:00:00",
  finale$data_ora_lettura[finale$RFID == "D250901002"][1] ==
    "2025-09-01 06:15:00",
  finale$data_ora_lettura[finale$RFID == "D250901201"][1] ==
    "2025-09-01 06:30:00",
  identical(
    per_rfid$D250901001$servizio_transponder[1:4],
    c("CARTA", "SECCO", "CARTA", "CARTA")
  ),
  identical(
    substr(per_rfid$D250901001$data_ora_lettura[1:3], 1, 10),
    c("2025-09-01", "2025-09-15", "2025-09-28")
  ),
  identical(
    per_rfid$D250901050$id_utenza[1:4],
    c("58590001", "58590001", "58590025", "58590025")
  ),
  all(per_rfid$D250901050$id_utenza[-(1:2)] == "58590025"),
  identical(per_rfid$D250915201$servizio_atteso, rep("SECCO", 4)),
  # il non censito con la stima incerta: due letture, due tipologie
  identical(per_rfid$D250920250$servizio_atteso, c("SECCO", "CARTA")),
  is.na(stime_2025$servizio_prevalente[stime_2025$RFID == "D250920250"]),
  identical(
    stime_2025$servizio_prevalente[stime_2025$RFID == "D250915201"],
    "SECCO"
  ),
  identical(
    per_rfid$D250905100$latitudine[1:2],
    sprintf("%.4f", c(spostato_da[["lat"]], spostato_a[["lat"]]))
  ),
  identical(
    per_rfid$D250905100$longitudine[1:2],
    sprintf("%.4f", c(spostato_da[["lng"]], spostato_a[["lng"]]))
  ),
  # coerenza dei campi: i dati del contenitore e l'utenza mancano ai non
  # censiti e ai sacchetti, la fornitura solo ai non censiti
  all(is.na(finale$servizio_transponder) == (non_presente | sacchetto)),
  all(is.na(finale$id_utenza) == (non_presente | sacchetto)),
  all(is.na(finale$id_fornitura) == non_presente),
  all(is.na(finale$volume) == (non_presente | sacchetto)),
  all(is.na(finale$numero_raccolte_annue) == (non_presente | sacchetto)),
  all(is.na(finale$frequenza) == (non_presente | sacchetto)),
  all(is.na(finale$stato_fornitura) == (non_presente | sacchetto)),
  all(finale$servizio_transponder %in% c(servizi, NA)),
  # ogni lettura ha il suo giro; la conferma e' la stessa per tutte le
  # letture di un giro in un giorno, NO negli ultimi sette giorni, a volte
  # mancante
  !anyNA(finale$id_giro),
  all(finale$id_giro > 1800),
  all(finale$eseguito %in% c("SI", "NO", NA)),
  all(tapply(
    paste(finale$eseguito),
    paste(finale$id_giro, finale$data_lettura),
    function(x) length(unique(x)) == 1
  )),
  all(finale$eseguito[finale$data_lettura > format(fine_dati - 7)] == "NO"),
  !any(finale$eseguito[finale$data_lettura <= format(fine_dati - 7)] %in% "NO"),
  any(is.na(finale$eseguito)),
  identical(finale$data_lettura, substr(finale$data_ora_lettura, 1, 10)),
  # ogni lettura ha il giro del mezzo che l'ha fatta, con il nome del
  # calendario: i giri sono dodici
  !anyNA(finale$servizio_giro),
  identical(finale$servizio_giro, letture$servizio_giro),
  length(unique(finale$servizio_giro)) == 12,
  # il servizio atteso ha i nomi del database: e' il giro, oppure, nei giri
  # dei servizi, il servizio dei censiti letti nel giro
  !anyNA(finale$servizio_atteso),
  all(servizi %in% finale$servizio_atteso),
  all((finale$servizio_atteso == uniforma_servizio(finale$servizio_giro))[
    !finale$servizio_giro %in% giri_servizio_dai_censiti()
  ]),
  all(finale$servizio_atteso %in% c(servizi_config()$servizio)),
  # la coerenza e' quella che l'app ricalcola al caricamento: manca per i
  # non censiti e per i sacchetti, che non hanno un servizio a database
  all(finale$coerenza_servizio %in% c("SI", "NO", NA)),
  identical(finale$coerenza_servizio, coerenza_servizio(finale)),
  all(is.na(finale$coerenza_servizio) == (non_presente | sacchetto)),
  # tra i non censiti ci sono stime nette e stime incerte
  setequal(stime_2025$RFID, unique(finale$RFID[nel_2025 & non_presente])),
  sum(!is.na(stime_2025$servizio_prevalente)) >= 10,
  sum(is.na(stime_2025$servizio_prevalente)) >= 5,
  all(finale$volume %in% c(40, 60, 120, 1100, NA)),
  all(finale$numero_raccolte_annue %in% c(13, 26, NA)),
  all(vapply(per_rfid, function(x) length(unique(x$volume)) == 1, logical(1))),
  # geografia: comuni della tabella, un comune del database per contenitore
  # censito, comune del giro e comune di lettura per ogni lettura
  all(is.na(finale$comune_fornitura) == (non_presente | sacchetto)),
  all(
    stats::na.omit(c(finale$comune_fornitura, finale$comune_giro)) %in%
      c(comuni$comune, "MASSICCIO DEL GRAPPA")
  ),
  # il giro di zona: per i non censiti la coerenza e' ignota
  any(finale$comune_giro == "MASSICCIO DEL GRAPPA" & non_presente),
  all(
    finale$coerenza_comune[
      finale$comune_giro == "MASSICCIO DEL GRAPPA" & non_presente
    ] ==
      "Coerenza ignota"
  ),
  # la distanza dal comune dichiarato c'e' solo per le letture fatte in un
  # altro comune; quelle entro la tolleranza sono coerenti
  all(
    !is.na(finale$distanza_comune_dichiarato[
      finale$coerenza_comune %in% c("Fuori comune", "Fuori giro")
    ])
  ),
  all(
    finale$distanza_comune_dichiarato[
      finale$coerenza_comune %in% c("Fuori comune", "Fuori giro")
    ] >
      tolleranza_confine_m()
  ),
  any(
    finale$coerenza_comune == "Coerente" &
      !is.na(finale$distanza_comune_dichiarato)
  ),
  all(is.na(finale$distanza_comune_dichiarato[
    finale$comune_lettura ==
      ifelse(non_presente, finale$comune_giro, finale$comune_fornitura)
  ])),
  all(vapply(
    per_rfid,
    function(x) length(unique(x$comune_fornitura)) == 1,
    logical(1)
  )),
  !anyNA(finale$comune_giro),
  # il comune di lettura viene dalla posizione: ogni lettura cade in un
  # comune servito, e ha il suo cantiere
  !anyNA(finale$comune_lettura),
  all(finale$comune_lettura %in% comuni$comune),
  !anyNA(finale$cantiere),
  # la coerenza del comune e' quella che l'app ricalcola al caricamento; un
  # sacchetto censito non ha un comune a database, e la sua e' ignota
  identical(finale$coerenza_comune, coerenza_comune(finale)),
  all(finale$coerenza_comune[sacchetto & !non_presente] == "Coerenza ignota"),
  !any(finale$coerenza_comune[!non_presente] == "Fuori giro"),
  !any(finale$coerenza_comune[non_presente] == "Fuori comune"),
  setequal(unique(finale$cantiere), names(quote_cantieri)),
  max(abs(
    prop.table(table(bidoni$cantiere))[names(quote_cantieri)] - quote_cantieri
  )) <
    0.05,
  # il contenitore spostato e' registrato a Limena e letto a Cittadella
  all(per_rfid$D250905100$comune_fornitura == "LIMENA"),
  identical(per_rfid$D250905100$comune_lettura[1:2], c("LIMENA", "CITTADELLA")),
  identical(
    per_rfid$D250905100$coerenza_comune[1:2],
    c("Coerente", "Fuori comune")
  ),
  identical(per_rfid$D250905100$comune_giro[1:2], c("LIMENA", "CITTADELLA")),
  # il cantiere segue la posizione: Limena e' di Rubano, Cittadella di Camposampiero
  identical(per_rfid$D250905100$cantiere[1:2], c("RUBANO", "CAMPOSAMPIERO")),
  # tutte le letture cadono nella zona servita
  all(
    letture$latitudine > riquadro_zona()$lat_min &
      letture$latitudine < riquadro_zona()$lat_max
  ),
  all(
    letture$longitudine > riquadro_zona()$lng_min &
      letture$longitudine < riquadro_zona()$lng_max
  ),
  nrow(unique(finale[, c("targa_veicolo", "matricola_veicolo")])) == 5,
  # fornitura: aperta o cessata; le date del contenitore comprendono le sue
  # letture, e l'associazione aperta finisce il 2099-12-31
  all(finale$stato_fornitura %in% c("ATTIVA", "CESSATA", NA)),
  any(finale$stato_fornitura %in% "CESSATA"),
  all(
    is.na(finale$data_cessazione_contenitore) == is.na(finale$stato_fornitura)
  ),
  all(
    (finale$data_cessazione_contenitore == "2099-12-31") ==
      (finale$stato_fornitura == "ATTIVA"),
    na.rm = TRUE
  ),
  all(finale$data_lettura >= finale$data_attivazione_contenitore, na.rm = TRUE),
  all(finale$data_lettura <= finale$data_cessazione_contenitore, na.rm = TRUE),
  length(unique(stats::na.omit(finale$id_utenza[!sacchetto]))) == 50,
  !anyNA(finale$latitudine),
  !anyNA(finale$longitudine),
  # nucleo e categoria: solo per le forniture dei contenitori; il nucleo solo
  # per quelle domestiche; il potenziamento per ogni fornitura, vero solo per
  # domestiche
  ncol(finale) == 35,
  identical(names(finale)[29:35], colonne_nucleo_manovra()),
  all(is.na(finale$tipo_categoria[non_presente | sacchetto])),
  any(is.na(finale$tipo_categoria) & !non_presente & !sacchetto),
  all(is.na(finale$sottocategoria) == is.na(finale$tipo_categoria)),
  all(is.na(finale$nucleo_residenti) == is.na(finale$tipo_categoria)),
  all(finale$sottocategoria %in% c("D", "ND", NA)),
  all(
    (finale$sottocategoria == "D") == (finale$tipo_categoria == "IA_31"),
    na.rm = TRUE
  ),
  all(
    (finale$sottocategoria == "D") == (finale$categoria == "31"),
    na.rm = TRUE
  ),
  all(finale$nucleo_totale[finale$sottocategoria %in% "ND"] == 0),
  all(
    finale$nucleo_totale ==
      finale$nucleo_residenti + finale$nucleo_non_residenti,
    na.rm = TRUE
  ),
  all(is.na(finale$potenziamento_secco) == non_presente),
  !any(
    finale$potenziamento_secco[finale$sottocategoria %in% "ND" | sacchetto],
    na.rm = TRUE
  ),
  any(finale$potenziamento_secco, na.rm = TRUE),
  length(che_cambiano) == 2,
  # esempi per l'analisi dei cluster: oltre 300 letture nel 2025
  sum(nel_2025 & finale$RFID == bidoni$RFID[k_deposito]) > 300,
  sum(nel_2025 & finale$RFID == bidoni$RFID[k_camion]) > 300
)

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(
  finale,
  "inst/extdata/sample_rfid_dataset.csv",
  row.names = FALSE,
  quote = FALSE,
  na = "NA",
  fileEncoding = "UTF-8"
)

message(sprintf(
  "Scritte %d letture di %d RFID in inst/extdata/sample_rfid_dataset.csv",
  nrow(finale),
  length(per_rfid)
))
message(sprintf(
  "Di cui %d letture di %d sacchetti, %d censiti e %d non censiti",
  sum(sacchetto),
  nrow(sacchetti),
  sum(!is.na(sacchetti$fornitura)),
  sum(is.na(sacchetti$fornitura))
))
message(sprintf(
  "Esempi cluster: deposito %s, camion %s, letture sparse %s",
  bidoni$RFID[k_deposito],
  bidoni$RFID[k_camion],
  bidoni$RFID[k_sparso]
))
