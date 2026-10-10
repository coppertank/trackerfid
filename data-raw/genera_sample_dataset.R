# Genera i due dataset di esempio:
#   inst/extdata/sample_rfid_dataset.csv        letture con le antenne
#   inst/extdata/sample_letture_storiche.csv    letture del sistema precedente
#
# Le letture con le antenne coprono 15 mesi (ottobre 2024 - dicembre 2025):
# l'anno 2025 e' completo, il 2024 parziale, cosi' si puo' provare la selezione
# dell'anno. Ogni contenitore viene letto a cadenza fissa dal giro del proprio
# servizio, come accade con un calendario di raccolta reale. Sono inclusi
# esempi per tutti gli indicatori dell'analisi dei cluster spaziali.
#
# Come nei dati reali, ogni lettura e' fatta da un mezzo durante un giro: per
# questo ogni riga ha il servizio atteso e il comune di lettura, cioe' il
# servizio e il comune del giro. Per un contenitore non censito sono le sole
# informazioni disponibili.
#
# Oltre ai contenitori ci sono alcuni sacchetti, censiti e non censiti: hanno
# il codice di 24 caratteri con il prefisso 00BD e nessuna tipologia di
# rifiuto. Servono a provare la tipologia "Sacchetti" dell'app.
#
# I contenitori stanno nei comuni veri del territorio servito, elencati in
# inst/extdata/comuni_cantieri.csv, attorno al centro abitato e dentro il
# confine comunale. Le quote dei cantieri sono fissate qui sotto.
#
# Le letture storiche vanno da gennaio 2020 a settembre 2024 e hanno solo due
# colonne, RFID e data e ora. Sono generate in coda allo script: cosi' le
# letture con le antenne non dipendono da loro.
#
# Lo script e' riproducibile (seed fisso). Usa R base e, per i confini dei
# comuni, le funzioni geografiche del pacchetto.
# Eseguire dalla radice del pacchetto:
#   Rscript data-raw/genera_sample_dataset.R

suppressMessages(pkgload::load_all(".", quiet = TRUE))
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

# Giro in corso per un mezzo in un dato istante: NA se l'istante cade fuori
# dall'orario dei suoi turni. Serve solo a costruire le letture fuori orario:
# il giro scritto nel file e' quello di `giro_del_mezzo()`.
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

# Giro che il mezzo stava facendo al momento di una lettura: il turno in cui
# cade l'istante, altrimenti il turno piu' vicino dello stesso mezzo in quel
# giorno. E' la regola che sui dati reali applica
# `associa_servizio_atteso_da_calendario()`. Il mezzo deve avere almeno un
# turno in quel giorno.
giro_del_mezzo <- function(matricola, quando) {
  gs <- giorno_settimana(as.Date(quando, tz = "UTC"))
  ora <- as.numeric(format(quando, "%H", tz = "UTC")) +
    as.numeric(format(quando, "%M", tz = "UTC")) / 60
  del_giorno <- turni_del_giorno(matricola, gs)
  stopifnot(length(del_giorno) > 0)
  distanza <- vapply(del_giorno, function(t) {
    if (ora >= t$da && ora < t$a) 0 else min(abs(ora - t$da), abs(ora - t$a))
  }, numeric(1))
  del_giorno[[which.min(distanza)]]$giro
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

# Lettura fuori dall'orario dei turni: il mezzo passa prima dell'inizio o dopo
# la fine del giro. Il suo giro e' quello piu' vicino nella giornata.
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

# ---- Posizioni: scarti attorno a centri astratti ------------------------------
#
# Le coordinate di questa parte non sono luoghi veri: servono solo a estrarre,
# per ogni contenitore, uno scarto dal centro abitato. Piu' sotto lo scarto
# viene riportato attorno al centro del comune assegnato.

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
k_np_incerto <- which(bidoni$seq == 250) # RFD20250920250
# Esempi per gli indicatori dell'analisi dei cluster.
k_deposito <- which(bidoni$seq == 301) # tag fermo in deposito
k_camion <- which(bidoni$seq == 302) # tag rimasto sul camion
k_sparso <- which(bidoni$seq == 303) # letture sparse / GPS rumoroso
speciali <- c(
  k_cambio_servizio, k_esempio_secco, k_cambio_utenza, k_spostato,
  k_np_esempio, k_np_stima, k_np_incerto, k_deposito, k_camion, k_sparso
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

# Posizione "di casa" di ogni contenitore, per ora come scarto astratto.
pos <- t(vapply(seq_len(nrow(bidoni)), function(i) posizione_casuale(), numeric(2)))
bidoni$lat <- pos[, "lat"]
bidoni$lng <- pos[, "lng"]

# ---- Geografia: comuni e cantieri del territorio servito ----------------------
#
# Ogni contenitore riceve un comune vero. Le estrazioni di questa parte hanno
# un seme proprio e non toccano la sequenza casuale principale: cambiare la
# geografia non cambia date, mezzi e servizi delle letture.

comuni <- utils::read.csv("inst/extdata/comuni_cantieri.csv", stringsAsFactors = FALSE)
# Confini dei comuni: i vertici di ogni comune, pronti per il controllo.
confini <- split(poligoni_comuni(), poligoni_comuni()$comune)
stopifnot(setequal(names(confini), comuni$comune))
quote_cantieri <- c(ASIAGO = 0.12, BASSANO = 0.26, CAMPOSAMPIERO = 0.37, RUBANO = 0.25)
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

# Comune che contiene un punto. Per un punto fuori dalla zona servita, il
# comune con il centro piu' vicino.
comune_del_punto <- function(lat, lng) {
  vicini <- order((comuni$latitudine - lat)^2 + ((comuni$longitudine - lng) * cos(45.6 * pi / 180))^2)
  for (i in utils::head(vicini, 8)) {
    if (nel_comune(lat, lng, comuni$comune[i])) {
      return(comuni$comune[i])
    }
  }
  comuni$comune[vicini[1]]
}

# Cantiere: quote esatte tra i contenitori. Comune: a sorte dentro il cantiere,
# con peso quadruplo per il comune che da' il nome al cantiere.
conteggi_cantieri <- round(quote_cantieri * nrow(bidoni))
piu_grande <- which.max(conteggi_cantieri)
conteggi_cantieri[piu_grande] <- conteggi_cantieri[piu_grande] + nrow(bidoni) - sum(conteggi_cantieri)
bidoni$cantiere <- con_seme(2026, sample(rep(names(quote_cantieri), conteggi_cantieri)))
bidoni$comune <- con_seme(2027, vapply(bidoni$cantiere, function(cantiere) {
  scelta <- comuni$comune[comuni$cantiere == cantiere]
  scelta[sample.int(length(scelta), 1, prob = ifelse(startsWith(scelta, cantiere), 4, 1))]
}, character(1), USE.NAMES = FALSE))

# Casi speciali con il comune fissato.
bidoni$comune[c(k_cambio_servizio, k_esempio_secco, k_np_esempio)] <- "BASSANO DEL GRAPPA"
bidoni$comune[k_spostato] <- "LIMENA"
bidoni$comune[k_camion] <- "MAROSTICA"
bidoni$comune[k_deposito] <- "CAMPOSAMPIERO"
bidoni$cantiere <- comuni$cantiere[match(bidoni$comune, comuni$comune)]

# Lo scarto dal centro astratto piu' vicino, limitato a 1,5 km circa, viene
# riportato attorno al centro del comune. Se porta il contenitore fuori dal
# confine comunale si dimezza, finche' il punto rientra.
for (i in seq_len(nrow(bidoni))) {
  zona <- which.min((zone$lat - bidoni$lat[i])^2 + (zone$lng - bidoni$lng[i])^2)
  scarto <- c(bidoni$lat[i] - zone$lat[zona], bidoni$lng[i] - zone$lng[zona])
  scarto <- scarto * min(1, 0.015 / sqrt(sum(scarto^2)))
  centro <- centro_comune(bidoni$comune[i])
  while (!nel_comune(centro[["lat"]] + scarto[1], centro[["lng"]] + scarto[2], bidoni$comune[i])) {
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
# lungo tutto il giro. Meno di 6 km: da Marostica a Nove, poi verso Bassano
# fino a poco meno di meta' strada. Cosi' le letture formano un'unica scia.
verso_bassano <- centro_comune("NOVE") + 0.45 * (centro_comune("BASSANO DEL GRAPPA") - centro_comune("NOVE"))
tappe <- cbind(centro_comune("MAROSTICA"), centro_comune("NOVE"), verso_bassano)
percorso <- data.frame(lat = tappe["lat", ], lng = tappe["lng", ])
punto_sul_percorso <- function(n) {
  tratti <- nrow(percorso) - 1
  lunghezze <- sqrt(diff(percorso$lat)^2 + (diff(percorso$lng) * cos(45.75 * pi / 180))^2)
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
    # Prima lettura a Limena, dalla seconda in poi a Cittadella: circa 20 km.
    altre <- letture_calendario(b)
    out <- aggiungi(
      manuali(
        c("2025-09-05 08:14:26", "2025-09-19 10:37:53"), "VEH002",
        latitudine = c(spostato_da[["lat"]], spostato_a[["lat"]]),
        longitudine = c(spostato_da[["lng"]], spostato_a[["lng"]])
      ),
      altre,
      latitudine = spostato_a[["lat"]] + rumore_gps(nrow(altre)),
      longitudine = spostato_a[["lng"]] + rumore_gps(nrow(altre))
    )
  } else if (b$chiave == k_np_stima) {
    out <- manuali(
      c("2025-09-15 07:26:40", "2025-09-17 07:31:08", "2025-09-22 14:12:55", "2025-09-26 07:19:33"),
      c("VEH001", "VEH001", "VEH004", "VEH001")
    )
  } else if (b$chiave == k_np_incerto) {
    # Letto un sabato dal giro del secco e il sabato dopo dal mezzo della
    # carta: due tipologie alla pari, la stima resta incerta.
    out <- manuali(c("2025-09-20 15:44:02", "2025-09-27 16:08:37"), c("VEH004", "VEH001"))
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
      latitudine = deposito[["lat"]] + rumore_gps(length(giorni), 0.00004),
      longitudine = deposito[["lng"]] + rumore_gps(length(giorni), 0.00004),
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

# Coordinate esatte delle righe di esempio della specifica: tre contenitori
# vicini, nel centro di Bassano del Grappa.
bassano <- round(centro_comune("BASSANO DEL GRAPPA"), 4)
coord_esempio <- list(
  list(k = k_cambio_servizio, lat = bassano[["lat"]], lng = bassano[["lng"]]),
  list(k = k_esempio_secco, lat = bassano[["lat"]] + 0.0007, lng = bassano[["lng"]] + 0.0007),
  list(k = k_np_esempio, lat = bassano[["lat"]] + 0.0014, lng = bassano[["lng"]] + 0.0014)
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

# ---- Sacchetti ----------------------------------------------------------------
#
# I mezzi leggono anche i sacchetti, che portano un tag a perdere. Si
# riconoscono dal codice: 24 caratteri, con il prefisso 00BD. A database un
# sacchetto ha solo l'utenza a cui e' stato consegnato: niente tipologia di
# rifiuto, volume, raccolte previste o comune. Un sacchetto e' monouso: il
# mezzo lo legge quando lo raccoglie, durante un giro del secco, e dopo lo
# svuotamento non puo' piu' essere letto. Ha quindi una sola lettura.
#
# Le estrazioni di questa parte hanno semi propri e non toccano la sequenza
# casuale principale: i sacchetti non cambiano le letture dei contenitori ne'
# le letture storiche.

sacchetti <- data.frame(
  # progressivo nel codice: da 1 i censiti, da 101 i non censiti
  n = c(1:8, 101:104),
  utenza = c(
    "SAC001", "SAC001", "SAC001", "SAC002", "SAC002", "SAC003", "SAC003", "SAC004",
    rep(NA_character_, 4)
  ),
  comune = c(
    rep("BASSANO DEL GRAPPA", 3), rep("CAMPOSAMPIERO", 2), rep("RUBANO", 2), "ASIAGO",
    "MAROSTICA", "CITTADELLA", "SELVAZZANO DENTRO", "GALLIO"
  ),
  # i due giri "SECCO PAP" del calendario
  turno = c(1, 1, 1, 10, 10, 10, 10, 1, 1, 10, 10, 1),
  # giorno della raccolta, cioe' dell'unica lettura
  giorno = as.Date(c(
    "2024-11-18", "2025-03-17", "2025-06-09", "2025-04-08", "2025-09-16", "2025-05-22",
    "2025-10-30", "2025-07-11", "2024-10-23", "2025-08-05", "2025-11-13", "2025-12-05"
  )),
  stringsAsFactors = FALSE
)
sacchetti$RFID <- sprintf("00BD%020d", sacchetti$n)
# I sacchetti di un'utenza vengono lasciati davanti alla stessa porta, in
# giorni diversi.
sacchetti$porta <- ifelse(is.na(sacchetti$utenza), sacchetti$RFID, sacchetti$utenza)
stopifnot(
  all(nchar(sacchetti$RFID) == 24),
  all(vapply(turni[sacchetti$turno], function(t) t$giro, "") == "SECCO PAP"),
  all(mapply(function(g, t) g %in% turni[[t]]$giorni, giorno_settimana(sacchetti$giorno), sacchetti$turno)),
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
    while (!nel_comune(centro[["lat"]] + scarto[1], centro[["lng"]] + scarto[2], porte$comune[i])) {
      scarto <- scarto / 2
    }
    porte$lat[i] <- centro[["lat"]] + scarto[1]
    porte$lng[i] <- centro[["lng"]] + scarto[2]
    porte$ora[i] <- orario_turno(turni[[porte$turno[i]]])
  }
  porte
})

letture_sacchetti <- con_seme(2029, do.call(rbind, lapply(seq_len(nrow(sacchetti)), function(i) {
  s <- sacchetti[i, ]
  p <- porte[match(s$porta, porte$porta), ]
  t <- turni[[s$turno]]
  ora <- p$ora + stats::rnorm(1, 0, 0.25)
  data.frame(
    giorno_lettura = istante(s$giorno, min(max(ora, t$da), t$a - 0.02)),
    matricola_veicolo = t$m,
    servizio_transponder = NA_character_,
    id_utenza = s$utenza,
    latitudine = p$lat + rumore_gps(1),
    longitudine = p$lng + rumore_gps(1),
    chiave = NA_integer_,
    presente = !is.na(s$utenza),
    RFID = s$RFID,
    stringsAsFactors = FALSE
  )
})))

# Le letture dei sacchetti si aggiungono a quelle dei contenitori: non hanno
# la chiave di un contenitore e portano gia' il proprio codice.
letture$RFID <- NA_character_
letture <- rbind(letture, letture_sacchetti[, names(letture)])
rownames(letture) <- NULL

# ---- Campi derivati -----------------------------------------------------------
#
# Da qui alle letture storiche non si usano numeri casuali: una modifica a
# questa parte non cambia date, posizioni e servizi delle letture.

# Una lettura esiste solo se un mezzo passa durante un giro. La domenica nel
# calendario lavora un solo mezzo: le poche letture attribuite a sorte a un
# mezzo che quel giorno e' fermo passano al primo mezzo in servizio.
giorno_lettura_gs <- giorno_settimana(as.Date(letture$giorno_lettura, tz = "UTC"))
fermo <- which(!mapply(
  function(m, gs) length(turni_del_giorno(m, gs)) > 0,
  letture$matricola_veicolo, giorno_lettura_gs
))
for (i in fermo) {
  letture$matricola_veicolo[i] <- Find(
    function(m) length(turni_del_giorno(m, giorno_lettura_gs[i])) > 0,
    veicoli$matricola
  )
}

letture$targa_veicolo <- veicoli$targa[match(letture$matricola_veicolo, veicoli$matricola)]
letture$presente_a_database <- ifelse(letture$presente, "Presente", "Non Presente")
letture$volume_previsto <- bidoni$volume[letture$chiave]
letture$numero_raccolte_annue_previste <- bidoni$raccolte[letture$chiave]

# Servizio atteso: il giro che il mezzo stava facendo al momento della
# lettura. C'e' per ogni lettura, come nei dati reali; l'app lo usa per
# stimare il servizio dei contenitori non censiti.
letture$servizio_atteso <- vapply(seq_len(nrow(letture)), function(i) {
  giro_del_mezzo(letture$matricola_veicolo[i], letture$giorno_lettura[i])
}, character(1))

# Colonne geografiche.
# - comune_da_database: comune dell'anagrafica dei contenitori. Lo hanno solo
#   i contenitori censiti e non cambia se il contenitore viene spostato. Un
#   sacchetto non lo ha nemmeno da censito.
# - comune_lettura: comune del giro che ha fatto la lettura, cioe' quello in
#   cui il mezzo si trovava. C'e' per ogni lettura: per un non censito e'
#   l'unico comune noto.
# - cantiere: quello del comune del database, se c'e', altrimenti quello del
#   comune della lettura.
letture$comune_da_database <- ifelse(letture$presente, bidoni$comune[letture$chiave], NA_character_)
letture$comune_lettura <- mapply(comune_del_punto, letture$latitudine, letture$longitudine)
comune_assegnato <- ifelse(
  is.na(letture$comune_da_database), letture$comune_lettura, letture$comune_da_database
)
letture$cantiere <- comuni$cantiere[match(comune_assegnato, comuni$comune)]

# Codice RFID: "RFD" + data della prima lettura con le antenne + progressivo
# del contenitore.
prima_lettura <- tapply(letture$giorno_lettura, letture$chiave, min)
prima_lettura <- as.POSIXct(prima_lettura, origin = "1970-01-01", tz = "UTC")
bidoni$RFID <- sprintf(
  "RFD%s%03d",
  format(prima_lettura[as.character(bidoni$chiave)], "%Y%m%d", tz = "UTC"),
  bidoni$seq
)
dei_bidoni <- !is.na(letture$chiave)
letture$RFID[dei_bidoni] <- bidoni$RFID[letture$chiave[dei_bidoni]]

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
  comune_da_database = letture$comune_da_database,
  comune_lettura = letture$comune_lettura,
  cantiere = letture$cantiere,
  volume_previsto = letture$volume_previsto,
  numero_raccolte_annue_previste = letture$numero_raccolte_annue_previste,
  stringsAsFactors = FALSE
)

# ---- Controlli di conformita' -------------------------------------------------

per_rfid <- split(finale, finale$RFID)
non_presente <- finale$presente_a_database == "Non Presente"
nel_2025 <- substr(finale$giorno_lettura, 1, 4) == "2025"
# Le righe dei sacchetti, riconosciute dal codice con la regola dell'app.
sacchetto <- rfid_sacchetto(finale$RFID)
n_bidoni <- sum(!rfid_sacchetto(names(per_rfid)))
# Stima del servizio dei non censiti nel 2025, con la regola dell'app: netta
# se una tipologia raggiunge l'80% delle letture, altrimenti incerta.
stime_2025 <- servizio_prevalente_non_censiti(finale[nel_2025, ])
stopifnot(
  nrow(finale) >= 600,
  n_bidoni >= 200, n_bidoni <= 250,
  !anyDuplicated(bidoni$RFID),
  !any(rfid_sacchetto(bidoni$RFID)),
  # sacchetti: riconosciuti tutti dal codice, letti una volta da un giro del
  # secco nel loro comune, censiti con la sola utenza
  setequal(unique(finale$RFID[sacchetto]), sacchetti$RFID),
  # monouso: una lettura per sacchetto
  sum(sacchetto) == nrow(sacchetti),
  !anyDuplicated(finale$RFID[sacchetto]),
  all(finale$servizio_atteso[sacchetto] == "SECCO PAP"),
  all(finale$comune_lettura[sacchetto] == sacchetti$comune[match(finale$RFID[sacchetto], sacchetti$RFID)]),
  identical(
    sort(unique(finale$RFID[sacchetto & !non_presente])),
    sacchetti$RFID[!is.na(sacchetti$utenza)]
  ),
  setequal(stats::na.omit(finale$id_utenza[sacchetto]), stats::na.omit(sacchetti$utenza)),
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
  # il non censito con la stima incerta: due letture, due tipologie
  identical(per_rfid$RFD20250920250$servizio_atteso, c("SECCO PAP", "CARTA/CARTONE PAP")),
  is.na(stime_2025$servizio_prevalente[stime_2025$RFID == "RFD20250920250"]),
  identical(stime_2025$servizio_prevalente[stime_2025$RFID == "RFD20250915201"], "SECCO PAP"),
  identical(
    per_rfid$RFD20250905100$latitudine[1:2],
    sprintf("%.4f", c(spostato_da[["lat"]], spostato_a[["lat"]]))
  ),
  identical(
    per_rfid$RFD20250905100$longitudine[1:2],
    sprintf("%.4f", c(spostato_da[["lng"]], spostato_a[["lng"]]))
  ),
  # coerenza dei campi: i dati del contenitore mancano ai non censiti e ai
  # sacchetti, l'utenza solo ai non censiti
  all(is.na(finale$servizio_transponder) == (non_presente | sacchetto)),
  all(is.na(finale$id_utenza) == non_presente),
  all(is.na(finale$volume_previsto) == (non_presente | sacchetto)),
  all(is.na(finale$numero_raccolte_annue_previste) == (non_presente | sacchetto)),
  all(finale$servizio_transponder %in% c(servizi, NA)),
  # ogni lettura ha il giro del mezzo che l'ha fatta
  !anyNA(finale$servizio_atteso),
  all(finale$servizio_atteso %in% vapply(turni, function(t) t$giro, "")),
  length(unique(finale$servizio_atteso)) == 12,
  # tra i non censiti ci sono stime nette e stime incerte
  setequal(stime_2025$RFID, unique(finale$RFID[nel_2025 & non_presente])),
  sum(!is.na(stime_2025$servizio_prevalente)) >= 10,
  sum(is.na(stime_2025$servizio_prevalente)) >= 5,
  all(finale$volume_previsto %in% c(240, 770, 1100, NA)),
  all(finale$numero_raccolte_annue_previste %in% c(13, 26, NA)),
  all(vapply(per_rfid, function(x) length(unique(x$volume_previsto)) == 1, logical(1))),
  # geografia: comuni della tabella, un comune del database per contenitore
  # censito, comune di lettura e cantiere per ogni lettura
  all(is.na(finale$comune_da_database) == (non_presente | sacchetto)),
  all(stats::na.omit(c(finale$comune_da_database, finale$comune_lettura)) %in% comuni$comune),
  all(vapply(per_rfid, function(x) length(unique(x$comune_da_database)) == 1, logical(1))),
  !anyNA(finale$comune_lettura),
  !anyNA(finale$cantiere),
  setequal(unique(finale$cantiere), names(quote_cantieri)),
  max(abs(prop.table(table(bidoni$cantiere))[names(quote_cantieri)] - quote_cantieri)) < 0.03,
  # il contenitore spostato e' registrato a Limena e letto a Cittadella
  all(per_rfid$RFD20250905100$comune_da_database == "LIMENA"),
  identical(per_rfid$RFD20250905100$comune_lettura[1:2], c("LIMENA", "CITTADELLA")),
  all(per_rfid$RFD20250905100$cantiere == "RUBANO"),
  # tutte le letture cadono nella zona servita
  all(letture$latitudine > riquadro_zona()$lat_min & letture$latitudine < riquadro_zona()$lat_max),
  all(letture$longitudine > riquadro_zona()$lng_min & letture$longitudine < riquadro_zona()$lng_max),
  nrow(unique(finale[, c("targa_veicolo", "matricola_veicolo")])) == 5,
  length(unique(stats::na.omit(finale$id_utenza[!sacchetto]))) == 50,
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
  "Di cui %d letture di %d sacchetti, %d censiti e %d non censiti",
  sum(sacchetto), nrow(sacchetti), sum(!is.na(sacchetti$utenza)), sum(is.na(sacchetti$utenza))
))
message(sprintf(
  "Esempi cluster: deposito %s, camion %s, letture sparse %s",
  bidoni$RFID[k_deposito], bidoni$RFID[k_camion], bidoni$RFID[k_sparso]
))

# ---- Letture storiche: il sistema precedente alle antenne --------------------
#
# Prima delle antenne una lettura registrava solo il codice e l'istante. Il
# sistema perdeva molti passaggi, in misura diversa da cantiere a cantiere, da
# comune a comune e da un anno all'altro, e capitava che un contenitore
# restasse un anno intero senza letture. Questa parte usa numeri casuali: va
# lasciata in coda allo script.

inizio_storico <- as.Date("2020-01-01")
fine_storico <- inizio_dati - 1
giorni_storico <- seq(inizio_storico, fine_storico, by = "day")
anni_storico <- 2020:2024

# Quota dei passaggi registrati dal sistema precedente, per cantiere e anno.
cattura <- rbind(
  BASSANO = c(0.62, 0.60, 0.56, 0.58, 0.55),
  CAMPOSAMPIERO = c(0.50, 0.44, 0.47, 0.42, 0.45),
  RUBANO = c(0.36, 0.33, 0.38, 0.30, 0.34),
  ASIAGO = c(0.28, 0.45, 0.22, 0.30, 0.26)
)
colnames(cattura) <- anni_storico
# Dentro un cantiere ogni comune registra un po' piu' o un po' meno della media.
comuni$fattore_cattura <- stats::runif(nrow(comuni), 0.7, 1.3)
# Probabilita' che un contenitore resti un anno intero senza letture.
quota_anni_vuoti <- c(BASSANO = 0.06, CAMPOSAMPIERO = 0.14, RUBANO = 0.22, ASIAGO = 0.30)

# Istanti delle letture storiche di un contenitore: i passaggi seguono la
# stessa cadenza usata con le antenne, ma ne viene registrata solo una parte.
# `vuoti` elenca gli anni senza letture; se manca vengono estratti a sorte.
letture_storiche_bidone <- function(giorno, cadenza, sfasamento, ora, esposizione, cantiere,
                                    dal, al = fine_storico, vuoti = NULL, fattore = 1) {
  passaggi <- giorni_storico[
    giorni_storico >= dal & giorni_storico <= al & giorno_settimana(giorni_storico) == giorno
  ]
  if (length(passaggi) <= sfasamento) {
    return(NULL)
  }
  passaggi <- passaggi[seq(1 + sfasamento, length(passaggi), by = cadenza / 7)]
  anno <- as.integer(format(passaggi, "%Y"))
  if (is.null(vuoti)) {
    vuoti <- anni_storico[stats::runif(length(anni_storico)) < quota_anni_vuoti[[cantiere]]]
  }
  quota <- pmin(esposizione * fattore * cattura[cantiere, as.character(anno)], 0.95)
  registrata <- stats::runif(length(passaggi)) < quota
  registrata <- registrata & !anno %in% vuoti
  if (!any(registrata)) {
    return(NULL)
  }
  ore <- pmin(pmax(ora + stats::rnorm(sum(registrata), 0, 0.4), 6), 18.98)
  istante(passaggi[registrata], ore)
}

# Contenitori gia' in servizio prima delle antenne: i censiti letti fin da
# ottobre 2024 e una parte dei non censiti. Gli altri RFID del dataset con le
# antenne non hanno storia: contenitori nuovi, oppure mai letti prima.
con_storia <- c(
  setdiff(censiti, c(speciali, tardivi)),
  estrai(setdiff(np_regolari, tardivi), 9),
  estrai(setdiff(np_sporadici, tardivi), 5)
)
# Sette su dieci erano in servizio da prima del 2020, gli altri sono stati
# consegnati tra il 2020 e il 2023.
bidoni$consegna <- inizio_storico
k_recenti <- estrai(con_storia, round(0.3 * length(con_storia)))
bidoni$consegna[k_recenti] <- estrai(
  seq(as.Date("2020-03-01"), as.Date("2023-12-31"), by = "day"), length(k_recenti)
)

# Contenitore usato come esempio nei documenti: in servizio da prima del 2020,
# letto con regolarita' dalle antenne, senza nessuna lettura nel 2021 e nel 2023.
k_vetrina <- min(which(
  bidoni$presente & bidoni$raccolte == 26L &
    !bidoni$chiave %in% c(speciali, tardivi, k_rari, k_recenti, k_utenza, k_servizio, k_mossi)
))

storiche <- lapply(con_storia, function(k) {
  b <- bidoni[k, ]
  # Un non censito letto di rado dalle antenne era letto di rado anche prima.
  esposizione <- if (k %in% np_sporadici) 0.08 else b$esposizione
  quando <- letture_storiche_bidone(
    b$giorno, b$cadenza, b$sfasamento, b$ora_tipica, esposizione, b$cantiere,
    dal = b$consegna, vuoti = if (k == k_vetrina) c(2021, 2023),
    fattore = comuni$fattore_cattura[match(b$comune, comuni$comune)]
  )
  if (is.null(quando)) {
    return(NULL)
  }
  data.frame(RFID = b$RFID, giorno_lettura = quando, stringsAsFactors = FALSE)
})

# RFID letti solo dal sistema precedente. I primi 30 sono contenitori ritirati
# prima delle antenne, gli altri erano ancora in servizio a settembre 2024 e le
# antenne non li hanno ancora letti.
n_solo_storico <- 45
n_ritirati <- 30
solo_storico <- lapply(seq_len(n_solo_storico), function(i) {
  dal <- if (stats::runif(1) < 0.7) {
    inizio_storico
  } else {
    estrai(seq(as.Date("2020-03-01"), as.Date("2022-12-31"), by = "day"))
  }
  al <- if (i <= n_ritirati) {
    estrai(seq(dal + 200, as.Date("2024-03-31"), by = "day"))
  } else {
    fine_storico
  }
  quando <- letture_storiche_bidone(
    giorno = sample.int(6, 1), cadenza = 14, sfasamento = sample.int(2, 1) - 1,
    ora = stats::runif(1, 6.5, 17), esposizione = stats::runif(1, 0.75, 0.98),
    cantiere = estrai(names(quote_cantieri), 1, prob = quote_cantieri),
    dal = dal, al = al
  )
  if (is.null(quando)) {
    return(NULL)
  }
  data.frame(
    RFID = sprintf("RFD%s%03d", format(min(quando), "%Y%m%d", tz = "UTC"), 400L + i),
    giorno_lettura = quando,
    stringsAsFactors = FALSE
  )
})

storico <- do.call(rbind, c(storiche, solo_storico))
storico <- storico[order(storico$giorno_lettura, storico$RFID), ]
storico_finale <- data.frame(
  RFID = storico$RFID,
  giorno_lettura = format(storico$giorno_lettura, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
  stringsAsFactors = FALSE
)

rfid_antenne <- unique(finale$RFID)
rfid_storico <- unique(storico_finale$RFID)
anni_vetrina <- unique(substr(
  storico_finale$giorno_lettura[storico_finale$RFID == bidoni$RFID[k_vetrina]], 1, 4
))
stopifnot(
  # tutte precedenti alla prima lettura con le antenne
  all(substr(storico_finale$giorno_lettura, 1, 10) < format(inizio_dati)),
  setequal(unique(substr(storico_finale$giorno_lettura, 1, 4)), as.character(anni_storico)),
  !anyDuplicated(storico_finale),
  !anyNA(storico_finale),
  # RFID letti da entrambi i sistemi, solo dalle antenne, solo in passato
  length(intersect(rfid_storico, rfid_antenne)) >= 150,
  length(setdiff(rfid_antenne, rfid_storico)) >= 30,
  length(setdiff(rfid_storico, rfid_antenne)) >= 30,
  setequal(anni_vetrina, c("2020", "2022", "2024"))
)

utils::write.csv(
  storico_finale,
  "inst/extdata/sample_letture_storiche.csv",
  row.names = FALSE, quote = FALSE, fileEncoding = "UTF-8"
)

message(sprintf(
  "Scritte %d letture storiche di %d RFID in inst/extdata/sample_letture_storiche.csv",
  nrow(storico_finale), length(rfid_storico)
))
message(sprintf(
  "RFID in comune %d, solo antenne %d, solo storico %d. Esempio con anni vuoti: %s",
  length(intersect(rfid_storico, rfid_antenne)), length(setdiff(rfid_antenne, rfid_storico)),
  length(setdiff(rfid_storico, rfid_antenne)), bidoni$RFID[k_vetrina]
))
