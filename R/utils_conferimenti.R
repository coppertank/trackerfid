# Conferimenti del secco, l'unico rifiuto fatturato: quanto conferisce ogni
# fornitura in un anno rispetto al limite della tariffa. Le antenne sono solo
# su una parte dei mezzi: per ogni fornitura si misura la quota di passaggi
# di un mezzo con l'antenna in cui il bidone era esposto, e la si riporta
# alle raccolte di un anno. Metodo e ragioni nel report
# `docs/conferimenti_secco.Rmd`; l'app non usa queste funzioni.

# --- Tariffa ----------------------------------------------------------------

#' Litri annui per componenti residenti (fornitura domestica)
#' Contesto: solo il secco è fatturato; il limite dipende dal nucleo.
#' Risultato: 720, 960, 1200, 1440 e 1680 litri; da cinque residenti in su non
#' cresce.
#' @noRd
litri_conferibili <- function() {
  data.frame(
    componenti = 1:5,
    litri = c(720, 960, 1200, 1440, 1680)
  )
}

#' Litri annui con il potenziamento del secco, al posto del limite del nucleo
#' Contesto: la manovra sociale (`NEONATI`, `ANZIANI`, `DISABILI`) in vigore.
#' Risultato: 6240 litri.
#' @noRd
litri_potenziamento <- function() {
  6240
}

#' Litri annui consentiti a una fornitura
#' Funzionamento: con il potenziamento `litri_potenziamento()`; altrimenti dal
#' nucleo (zero residenti come uno, cinque o più come cinque); senza nucleo,
#' mancante.
#' Risultato: un vettore di litri.
#' @noRd
litri_consentiti <- function(nucleo_residenti, potenziamento = FALSE) {
  tabella <- litri_conferibili()
  componenti <- pmin(
    pmax(suppressWarnings(as.integer(nucleo_residenti)), 1L),
    max(tabella$componenti)
  )
  litri <- tabella$litri[match(componenti, tabella$componenti)]
  potenziamento <- rep_len(potenziamento %in% TRUE, length(litri))
  litri[potenziamento] <- litri_potenziamento()
  litri
}

#' Fornitura domestica dalla sottocategoria: `D` vero, `ND` falso, altro mancante
#' @noRd
fornitura_domestica <- function(sottocategoria) {
  valori <- toupper(stringr::str_squish(as.character(sottocategoria)))
  esito <- rep(NA, length(valori))
  esito[valori %in% "D"] <- TRUE
  esito[valori %in% "ND"] <- FALSE
  esito
}

# --- Parametri ----------------------------------------------------------------

#' Parametri dell'analisi dei conferimenti
#'
#' `raggio_vicini_m`, `vicini_massimi`, `vicini_minimi` (sotto, occasioni
#' dalle raccolte previste); `distanza_anomala_m` dalla posizione abituale;
#' `quota_giorni_anomala` per i tag fermi; `peso_prior` della media nel
#' tasso; `occasioni_minime` per contattare; `livello_intervallo`;
#' `soglie_probabilita` delle classi; `raggio_sostituzione_m`.
#' @param ... Valori da cambiare.
#' @noRd
parametri_conferimenti <- function(...) {
  base <- list(
    raggio_vicini_m = 150,
    vicini_massimi = 10,
    vicini_minimi = 3,
    distanza_anomala_m = 300,
    quota_giorni_anomala = 0.3,
    peso_prior = 4,
    occasioni_minime = 4,
    livello_intervallo = 0.8,
    soglie_probabilita = c(oltre = 0.8, probabile = 0.5, vicino = 0.2),
    raggio_sostituzione_m = 30
  )
  modifiche <- list(...)
  ignoti <- setdiff(names(modifiche), names(base))
  if (length(ignoti) > 0) {
    stop(
      "Parametri sconosciuti: ",
      paste(ignoti, collapse = ", "),
      call. = FALSE
    )
  }
  utils::modifyList(base, modifiche)
}

#' Classi delle forniture, dalla più urgente: probabilità di superare il limite, poi i casi indecidibili
#' @noRd
classi_conferimenti <- function() {
  c(
    "Oltre il limite",
    "Probabile superamento",
    "Vicino al limite",
    "Entro il limite",
    "Dati insufficienti",
    "Nucleo mancante",
    "Non attiva",
    "Non domestica",
    "Categoria sconosciuta"
  )
}

# --- Posizioni, vicini e occasioni -------------------------------------------------

#' Posizione abituale di ogni contenitore, letture lontane e tag anomali
#' Contesto: un tag fermo in deposito o sul mezzo non descrive un contenitore in
#' strada e falserebbe il conteggio.
#' Funzionamento: posizione: mediana delle letture nei giri del secco. Lontana:
#' oltre `distanza_anomala_m`. Tag anomalo: letto in almeno cinque giorni e in più
#' di `quota_giorni_anomala` dei giorni tra prima e ultima lettura.
#' @return Lista: `contenitori` (`RFID`, `lat`, `lon`, `giorni`,
#'   `tag_anomalo`), `indice` e `lontana` per lettura.
#' @noRd
posizioni_contenitori <- function(rfid, lat, lon, giorno, parametri) {
  indice <- indice_gruppi(rfid)
  n <- max(c(indice, 0L))
  if (n == 0) {
    return(list(
      contenitori = data.frame(
        RFID = character(0),
        lat = numeric(0),
        lon = numeric(0),
        giorni = integer(0),
        tag_anomalo = logical(0)
      ),
      indice = integer(0),
      lontana = logical(0)
    ))
  }
  lat_casa <- quantile_per_gruppo(lat, indice, 0.5, n)
  lon_casa <- quantile_per_gruppo(lon, indice, 0.5, n)
  distanza <- distanza_haversine_m(lat, lon, lat_casa[indice], lon_casa[indice])
  giorni <- n_distinti_per_gruppo(giorno, indice, n)
  estremi <- estremi_per_gruppo(giorno, indice, n)
  arco <- estremi$massimo - estremi$minimo + 1
  list(
    contenitori = data.frame(
      RFID = rfid[!duplicated(indice)],
      lat = lat_casa,
      lon = lon_casa,
      giorni = giorni,
      tag_anomalo = giorni > parametri$quota_giorni_anomala * arco &
        giorni >= 5,
      stringsAsFactors = FALSE
    ),
    indice = indice,
    lontana = distanza > parametri$distanza_anomala_m
  )
}

#' Fino a `massimo` contenitori entro `raggio_m` da ognuno, dal più vicino
#' @return Data frame con `da`, `a`, `distanza`.
#' @noRd
vicini_contenitori <- function(lat, lon, raggio_m, massimo) {
  if (length(lat) < 2) {
    return(data.frame(da = integer(0), a = integer(0), distanza = numeric(0)))
  }
  xy <- proietta_metri(lat, lon)
  vicini <- dbscan::frNN(xy, eps = raggio_m, sort = TRUE)
  quanti <- pmin(lengths(vicini$id), massimo)
  da <- rep(seq_along(quanti), quanti)
  presi <- function(elenco) {
    unlist(
      lapply(seq_along(elenco), function(i) {
        utils::head(elenco[[i]], quanti[i])
      }),
      use.names = FALSE
    )
  }
  data.frame(
    da = da,
    a = as.integer(presi(vicini$id)),
    distanza = as.numeric(presi(vicini$dist))
  )
}

#' Occasioni di conferimento di ogni unità
#'
#' Contesto: il denominatore del tasso. Solo i giorni in cui un mezzo con
#' l'antenna è passato davvero possono mostrare un conferimento.
#' Funzionamento: un'occasione è un giorno in cui un mezzo con l'antenna, in un
#' giro del secco, è passato: lo dice la lettura del contenitore o di un vicino
#' sullo stesso percorso, cioè letto almeno una volta lo stesso giorno del
#' contenitore. Con meno di `vicini_minimi` vicini le occasioni vengono dalle
#' raccolte previste e il tasso esce basso.
#' Risultato: le occasioni e i vicini di ogni unità.
#' @param unita `contenitore`, `inizio`, `fine` (giorni interi).
#' @param giorni_unita `u`, `giorno`: giorni di lettura di ogni unità.
#' @param evidenza `c`, `giorno`: giorni di lettura di ogni contenitore.
#' @return Lista con `occasioni` e `vicini` per unità.
#' @noRd
occasioni_conferimento <- function(
  unita,
  giorni_unita,
  evidenza,
  vicini,
  n_contenitori
) {
  quante <- nrow(unita)
  if (quante == 0) {
    return(list(occasioni = integer(0), vicini = integer(0)))
  }
  passo <- 100000
  # Elenco ordinato per gruppo: inizio e lunghezza di ogni gruppo.
  per_gruppo <- function(gruppo, n) {
    ordine <- order(gruppo, method = "radix")
    conta <- tabulate(gruppo, n)
    list(ordine = ordine, conta = conta, inizio = cumsum(conta) - conta + 1L)
  }
  chiave_evidenza <- evidenza$c * passo + evidenza$giorno

  # 1. Coppie unità-vicino.
  gv <- per_gruppo(vicini$da, n_contenitori)
  vicini_a <- vicini$a[gv$ordine]
  quanti <- gv$conta[unita$contenitore]
  coppia_u <- rep(seq_len(quante), quanti)
  coppia_v <- vicini_a[sequence(quanti, from = gv$inizio[unita$contenitore])]

  # 2. Restano i vicini letti almeno una volta nello stesso giorno.
  gu <- per_gruppo(giorni_unita$u, quante)
  giorno_u <- giorni_unita$giorno[gu$ordine]
  ripetizioni <- gu$conta[coppia_u]
  coppia <- rep(seq_along(coppia_u), ripetizioni)
  giorni <- giorno_u[sequence(ripetizioni, from = gu$inizio[coppia_u])]
  insieme <- (coppia_v[coppia] * passo + giorni) %in% chiave_evidenza
  tiene <- tabulate(coppia[insieme], length(coppia_u)) > 0
  coppia_u <- coppia_u[tiene]
  coppia_v <- coppia_v[tiene]

  # 3. Giorni di passaggio: quelli dei vicini rimasti e quelli dell'unità,
  # dentro la finestra, contati una volta.
  ge <- per_gruppo(evidenza$c, n_contenitori)
  giorno_e <- evidenza$giorno[ge$ordine]
  ripetizioni <- ge$conta[coppia_v]
  pu <- c(rep(coppia_u, ripetizioni), giorni_unita$u)
  pg <- c(
    giorno_e[sequence(ripetizioni, from = ge$inizio[coppia_v])],
    giorni_unita$giorno
  )
  dentro <- pg >= unita$inizio[pu] & pg <= unita$fine[pu]
  chiavi <- unique(pu[dentro] * passo + pg[dentro])
  list(
    occasioni = tabulate(as.integer(chiavi %/% passo), quante),
    vicini = tabulate(coppia_u, quante)
  )
}

# --- Stima --------------------------------------------------------------------

#' Tasso di esposizione: stima Beta, avvicinata alla media con il peso di `peso` occasioni
#' Contesto: un contenitore con poche occasioni darebbe tassi estremi; la media
#' del gruppo li stabilizza.
#' Risultato: tasso con intervallo di credibilità.
#' @param k,n Esposizioni e occasioni.
#' @return Data frame con `tasso`, `basso`, `alto`, `alfa`, `beta`.
#' @noRd
stima_tasso <- function(k, n, media, peso, livello = 0.8) {
  alfa <- k + media * peso
  beta <- pmax(n - k, 0) + (1 - media) * peso
  coda <- (1 - livello) / 2
  data.frame(
    tasso = alfa / (alfa + beta),
    basso = stats::qbeta(coda, alfa, beta),
    alto = stats::qbeta(1 - coda, alfa, beta),
    alfa = alfa,
    beta = beta
  )
}

#' Fascia di litri annui della tariffa, come fattore ordinato
#' @noRd
fascia_litri <- function(litri) {
  limiti <- c(litri_conferibili()$litri, litri_potenziamento())
  etichette <- c(
    sprintf("fino a %s", formatta_numero(limiti[1])),
    sprintf(
      "%s-%s",
      formatta_numero(limiti[-length(limiti)] + 1),
      formatta_numero(limiti[-1])
    ),
    sprintf("oltre %s", formatta_numero(limiti[length(limiti)]))
  )
  cut(
    litri,
    c(-Inf, limiti, Inf),
    labels = etichette,
    right = TRUE,
    ordered_result = TRUE
  )
}

#' Nucleo più piccolo il cui limite copre i litri: "1"..."5 o più", o con potenziamento
#' @noRd
nucleo_minimo_compatibile <- function(litri) {
  tabella <- litri_conferibili()
  nomi <- as.character(tabella$componenti)
  nomi[length(nomi)] <- paste(nomi[length(nomi)], "o pi\u00f9")
  posizione <- findInterval(litri, c(-Inf, tabella$litri), left.open = TRUE)
  esito <- nomi[posizione]
  esito[litri > max(tabella$litri)] <- "Solo con potenziamento"
  esito[litri > litri_potenziamento()] <- "Oltre il potenziamento"
  esito[is.na(litri)] <- NA_character_
  esito
}

# --- Analisi --------------------------------------------------------------------

#' Analisi dei conferimenti del secco
#'
#' Contesto: il secco è l'unico rifiuto fatturato (di norma 26 raccolte l'anno);
#' interessa quante forniture superano il limite di litri e se i non censiti
#' potrebbero avere la manovra sociale. Le antenne leggono i contenitori ma non
#' vedono lo svuotamento: si stima da quante volte il contenitore è stato letto
#' quando il mezzo è passato.
#' Funzionamento: 1. letture del periodo nei giri del secco, senza sacchetti,
#' letture lontane e tag anomali; 2. per ogni unità (contenitore censito in una
#' fornitura, o non censito stimato del secco) esposizioni e occasioni; 3. tasso
#' di esposizione riportato a un anno; 4. per fornitura, probabilità di superare
#' il limite; 5. per non censito, nucleo minimo, probabilità del potenziamento e
#' contenitore forse sostituito.
#' Risultato: una lista con le tabelle per fornitura e per non censito, il
#' riepilogo e i parametri; la usa il report `docs/conferimenti_secco.qmd`.
#' @param letture Letture con tutte le colonne della pipeline:
#'   `carica_dataset(percorso, colonne = "tutte")`.
#' @param dal,al Periodo; senza, dal primo gennaio dell'anno dell'ultima
#'   lettura. Non prima del primo giorno con letture.
#' @return Lista con `forniture`, `non_censiti`, `unita`,
#'   `manovra_per_fascia`, `riepilogo`, `periodo`, `tasso_medio` (attributi
#'   `unita` e `tutte`) e `parametri`.
#' @noRd
analisi_conferimenti <- function(
  letture,
  dal = NULL,
  al = NULL,
  parametri = parametri_conferimenti()
) {
  usate <- c(
    "data_ora_lettura",
    "RFID",
    "presente_a_database",
    "servizio_transponder",
    "servizio_atteso",
    "latitudine",
    "longitudine",
    "id_utenza",
    "volume_previsto",
    "numero_raccolte_annue_previste",
    "comune_fornitura",
    "comune_lettura",
    "id_fornitura",
    "stato_fornitura",
    "data_attivazione_contenitore",
    "data_cessazione_contenitore",
    "nucleo_residenti",
    "tipo_categoria",
    "categoria",
    "sottocategoria",
    "potenziamento_secco"
  )
  mancanti <- setdiff(usate, names(letture))
  if (length(mancanti) > 0) {
    stop(
      "Mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      ". Le letture vanno lette con carica_dataset(percorso, colonne = \"tutte\").",
      call. = FALSE
    )
  }
  censite <- letture$presente_a_database == "PIA" &
    !rfid_sacchetto(letture$RFID)
  if (any(censite) && all(is.na(letture$id_fornitura[censite]))) {
    stop(
      "Le letture dei censiti non hanno la fornitura: il file va rigenerato con la pipeline di preparazione dei dati.",
      call. = FALSE
    )
  }
  giorno <- as.integer(as.Date(letture$data_ora_lettura))
  al <- as.integer(as.Date(al %||% as.Date(max(giorno), origin = "1970-01-01")))
  dal <- as.integer(as.Date(
    dal %||%
      as.Date(sprintf(
        "%s-01-01",
        format(as.Date(al, origin = "1970-01-01"), "%Y")
      ))
  ))
  nel_periodo <- giorno >= dal & giorno <= al
  riepilogo <- list(letture = sum(nel_periodo))
  sacchetto <- rfid_sacchetto(letture$RFID)
  riepilogo$sacchetti <- sum(nel_periodo & sacchetto)
  letture <- letture[nel_periodo & !sacchetto, , drop = FALSE]
  giorno <- giorno[nel_periodo & !sacchetto]
  if (nrow(letture) == 0) {
    stop("Nessuna lettura di contenitori nel periodo.", call. = FALSE)
  }
  # Prima del primo giorno con letture le antenne non c'erano.
  dal <- max(dal, min(giorno))

  # 1. Letture nei giri del secco.
  secco <- servizio_secco(letture$servizio_atteso)
  riepilogo$fuori_dai_giri_secco <- sum(!secco)
  ls <- letture[secco, , drop = FALSE]
  gs <- giorno[secco]
  posizioni <- posizioni_contenitori(
    ls$RFID,
    ls$latitudine,
    ls$longitudine,
    gs,
    parametri
  )
  contenitori <- posizioni$contenitori
  ci <- posizioni$indice
  valida <- !posizioni$lontana & !contenitori$tag_anomalo[ci]
  riepilogo$letture_secco <- nrow(ls)
  riepilogo$lontane <- sum(posizioni$lontana)
  riepilogo$tag_anomali <- sum(contenitori$tag_anomalo)
  riepilogo$letture_tag_anomali <- sum(contenitori$tag_anomalo[ci])
  evidenza <- unique(data.frame(c = ci[valida], giorno = gs[valida]))
  vicini <- vicini_contenitori(
    contenitori$lat,
    contenitori$lon,
    parametri$raggio_vicini_m,
    parametri$vicini_massimi
  )
  vicini <- vicini[
    vicini$da != vicini$a & !contenitori$tag_anomalo[vicini$a],
    ,
    drop = FALSE
  ]

  # 2. Unità: contenitore censito del secco in una fornitura, o non censito
  # stimato del secco.
  censita <- valida &
    ls$presente_a_database == "PIA" &
    servizio_secco(ls$servizio_transponder) &
    !is.na(ls$id_fornitura)
  stime <- servizio_prevalente_non_censiti(letture)
  rfid_nc <- stime$RFID[servizio_secco(stime$servizio_prevalente)]
  non_censita <- valida &
    ls$presente_a_database != "PIA" &
    ls$RFID %in% rfid_nc
  riepilogo$contenitori_censiti <- length(unique(ls$RFID[censita]))
  riepilogo$non_censiti <- length(unique(ls$RFID[non_censita]))

  chiave <- ifelse(
    censita,
    paste(ls$RFID, ls$id_fornitura, sep = "\r"),
    ls$RFID
  )
  presa <- censita | non_censita
  ordine <- which(presa)[order(
    gs[presa],
    ls$data_ora_lettura[presa],
    method = "radix"
  )]
  u <- indice_gruppi(chiave[ordine])
  quante <- max(c(u, 0L))
  r <- ls[ordine, , drop = FALSE]
  gu <- gs[ordine]
  primo <- !duplicated(u)
  unita <- data.frame(
    unita = chiave[ordine][primo],
    tipo = ifelse(censita[ordine][primo], "censito", "non censito"),
    RFID = r$RFID[primo],
    contenitore = ci[ordine][primo],
    id_fornitura = ultimo_valido_per_gruppo(r$id_fornitura, u, quante),
    id_utenza = ultimo_valido_per_gruppo(r$id_utenza, u, quante),
    stato_fornitura = ultimo_valido_per_gruppo(r$stato_fornitura, u, quante),
    comune_fornitura = ultimo_valido_per_gruppo(r$comune_fornitura, u, quante),
    comune_lettura = piu_frequente_per_gruppo(r$comune_lettura, u, quante),
    volume = ultimo_valido_per_gruppo(r$volume_previsto, u, quante),
    raccolte = ultimo_valido_per_gruppo(
      r$numero_raccolte_annue_previste,
      u,
      quante
    ),
    nucleo_residenti = ultimo_valido_per_gruppo(r$nucleo_residenti, u, quante),
    tipo_categoria = ultimo_valido_per_gruppo(r$tipo_categoria, u, quante),
    categoria = ultimo_valido_per_gruppo(r$categoria, u, quante),
    sottocategoria = ultimo_valido_per_gruppo(r$sottocategoria, u, quante),
    # Il potenziamento dell'ultima lettura: falso se la manovra è finita.
    potenziamento = ultimo_per_gruppo(r$potenziamento_secco, u, quante),
    attivazione = estremi_per_gruppo(
      as.integer(r$data_attivazione_contenitore),
      u,
      quante
    )$minimo,
    cessazione = estremi_per_gruppo(
      as.integer(r$data_cessazione_contenitore),
      u,
      quante
    )$massimo,
    prima_lettura = primo_per_gruppo(gu, u, quante),
    ultima_lettura = ultimo_per_gruppo(gu, u, quante),
    stringsAsFactors = FALSE
  )
  # Finestra dal giorno dopo la prima lettura: contare la prima gonfierebbe il
  # tasso, perché un bidone compare nei dati solo se è stato letto.
  unita$inizio <- pmax(
    dal,
    dplyr::coalesce(unita$attivazione, dal),
    unita$prima_lettura + 1L
  )
  unita$fine <- pmin(al, dplyr::coalesce(unita$cessazione, al))
  giorni_unita <- unique(data.frame(u = u, giorno = gu))
  dentro <- giorni_unita$giorno >= unita$inizio[giorni_unita$u] &
    giorni_unita$giorno <= unita$fine[giorni_unita$u]
  unita$esposizioni <- tabulate(giorni_unita$u[dentro], quante)
  occasioni <- occasioni_conferimento(
    unita,
    giorni_unita,
    evidenza,
    vicini,
    nrow(contenitori)
  )
  unita$vicini <- occasioni$vicini
  unita$giorni_finestra <- pmax(unita$fine - unita$inizio + 1L, 0L)

  # Per un non censito: dai censiti vicini, poi dal comune, poi da tutti.
  unita <- completa_non_censiti(unita, vicini, contenitori)

  # Senza vicini sul percorso, occasioni dalle raccolte previste: stima prudente.
  unita$metodo <- ifelse(
    unita$vicini >= parametri$vicini_minimi,
    "passaggi",
    "raccolte previste"
  )
  dalle_raccolte <- round(unita$raccolte * unita$giorni_finestra / 365)
  unita$occasioni <- ifelse(
    unita$metodo == "passaggi",
    occasioni$occasioni,
    pmax(unita$esposizioni, dplyr::coalesce(dalle_raccolte, 0))
  )
  unita$occasioni <- pmax(unita$occasioni, unita$esposizioni)

  # 3. Tasso medio: le forniture domestiche censite con occasioni contate.
  base <- unita$tipo == "censito" &
    unita$metodo == "passaggi" &
    unita$occasioni >= parametri$occasioni_minime &
    fornitura_domestica(unita$sottocategoria) %in% TRUE
  base_ridotta <- !any(base)
  if (base_ridotta) {
    base <- unita$occasioni > 0
  }
  tasso_medio <- if (any(base)) {
    sum(unita$esposizioni[base]) / sum(unita$occasioni[base])
  } else {
    0.5
  }
  tasso_medio <- min(max(tasso_medio, 0.01), 0.99)

  forniture <- stima_forniture(
    unita[unita$tipo == "censito", , drop = FALSE],
    tasso_medio,
    al,
    parametri
  )
  manovra <- manovra_per_fascia(forniture)
  non_censiti <- stima_non_censiti(
    unita[unita$tipo == "non censito", , drop = FALSE],
    unita[unita$tipo == "censito", , drop = FALSE],
    contenitori,
    tasso_medio,
    manovra,
    parametri
  )

  list(
    forniture = forniture,
    non_censiti = non_censiti,
    unita = unita,
    manovra_per_fascia = manovra,
    riepilogo = riepilogo,
    periodo = as.Date(c(dal, al), origin = "1970-01-01"),
    tasso_medio = structure(
      tasso_medio,
      unita = sum(base),
      tutte = base_ridotta
    ),
    parametri = parametri
  )
}

#' Il servizio è del secco? Per tipologia: `SECCO PAP` lo è
#' @noRd
servizio_secco <- function(servizio) {
  uniforma_servizio(servizio) %in% "SECCO"
}

#' Raccolte e volume dei non censiti: i più frequenti tra i censiti vicini, poi nel comune, poi tra tutti
#'
#' `origine_ipotesi` dice da dove vengono.
#' @noRd
completa_non_censiti <- function(unita, vicini, contenitori) {
  censite <- unita$tipo == "censito"
  unita$origine_ipotesi <- ifelse(censite, "anagrafica", NA_character_)
  if (!any(!censite)) {
    return(unita)
  }
  # Raccolte e volume di ogni contenitore censito.
  per_contenitore <- function(campo) {
    valori <- rep(NA_real_, nrow(contenitori))
    valori[unita$contenitore[censite]] <- unita[[campo]][censite]
    valori
  }
  moda <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) NA_real_ else as.numeric(names(which.max(table(x))))
  }
  for (campo in c("raccolte", "volume")) {
    del_contenitore <- per_contenitore(campo)
    # dai vicini
    coppie <- vicini[!is.na(del_contenitore[vicini$a]), , drop = FALSE]
    dai_vicini <- rep(NA_real_, nrow(contenitori))
    if (nrow(coppie) > 0) {
      dai_vicini <- piu_frequente_per_gruppo(
        del_contenitore[coppie$a],
        coppie$da,
        nrow(contenitori)
      )
    }
    # dal comune
    comuni <- unita$comune_lettura
    nel_comune <- tapply(unita[[campo]][censite], comuni[censite], moda)
    dal_comune <- unname(nel_comune[comuni])
    generale <- moda(unita[[campo]][censite])
    nc <- which(!censite)
    valore <- dai_vicini[unita$contenitore[nc]]
    origine <- ifelse(!is.na(valore), "censiti vicini", NA_character_)
    usa_comune <- is.na(valore) & !is.na(dal_comune[nc])
    valore[usa_comune] <- dal_comune[nc][usa_comune]
    origine[usa_comune] <- "comune"
    usa_tutti <- is.na(valore)
    valore[usa_tutti] <- generale
    origine[usa_tutti] <- "tutti i censiti"
    unita[[campo]][nc] <- valore
    if (campo == "raccolte") {
      unita$origine_ipotesi[nc] <- origine
    }
  }
  unita
}

#' Conferimenti stimati di ogni fornitura censita
#' Contesto: il limite vale per fornitura, non per contenitore.
#' Funzionamento: somma esposizioni e occasioni dei suoi contenitori del secco e
#' riporta il tasso alle raccolte annue dei contenitori attivi alla fine del
#' periodo.
#' @param al Ultimo giorno, come intero.
#' @return Una riga per fornitura.
#' @noRd
stima_forniture <- function(unita, tasso_medio, al, parametri) {
  if (nrow(unita) == 0) {
    return(forniture_vuote())
  }
  f <- indice_gruppi(unita$id_fornitura)
  quante <- max(f)
  # Per ultima lettura: valgono gli attributi più recenti.
  ordine <- order(f, unita$ultima_lettura, method = "radix")
  unita <- unita[ordine, , drop = FALSE]
  f <- f[ordine]
  attiva <- unita$fine >= al
  attive <- tabulate(f[attiva], quante)
  # Volume e raccolte dei contenitori attivi; senza, di tutti.
  scelte <- attiva | !(attive[f] > 0)
  con_volume <- scelte & !is.na(unita$volume)
  volume <- somma_per_gruppo(ifelse(con_volume, unita$volume, 0), f, quante) /
    tabulate(f[con_volume], quante)
  volume[!is.finite(volume)] <- NA
  raccolte <- estremi_per_gruppo(
    ifelse(scelte & !is.na(unita$raccolte), unita$raccolte, -Inf),
    f,
    quante
  )$massimo
  raccolte[!is.finite(raccolte)] <- NA
  moda_raccolte <- {
    validi <- unita$raccolte[!is.na(unita$raccolte)]
    if (length(validi) == 0) 26 else as.numeric(names(which.max(table(validi))))
  }
  raccolte_ipotizzate <- is.na(raccolte)
  raccolte[raccolte_ipotizzate] <- moda_raccolte

  esposizioni <- somma_per_gruppo(unita$esposizioni, f, quante)
  occasioni <- somma_per_gruppo(unita$occasioni, f, quante)
  giorni <- estremi_per_gruppo(unita$fine, f, quante)$massimo -
    estremi_per_gruppo(unita$inizio, f, quante)$minimo +
    1
  metodo <- ifelse(
    tabulate(f[unita$metodo != "passaggi"], quante) > 0,
    "raccolte previste",
    "passaggi"
  )
  contenitori_attivi <- pmax(attive, 1L)

  stima <- stima_tasso(
    esposizioni,
    occasioni,
    tasso_medio,
    parametri$peso_prior,
    parametri$livello_intervallo
  )
  annui <- raccolte * contenitori_attivi
  nucleo <- ultimo_valido_per_gruppo(unita$nucleo_residenti, f, quante)
  potenziamento <- ultimo_per_gruppo(unita$potenziamento, f, quante) %in% TRUE
  litri <- litri_consentiti(nucleo, potenziamento)
  consentiti <- litri / volume
  conferimenti <- stima$tasso * annui
  quota_consentita <- consentiti / annui
  probabilita <- ifelse(
    quota_consentita >= 1,
    0,
    1 - stats::pbeta(pmin(quota_consentita, 1), stima$alfa, stima$beta)
  )
  stato <- ultimo_valido_per_gruppo(unita$stato_fornitura, f, quante)

  forniture <- data.frame(
    id_fornitura = unita$id_fornitura[!duplicated(f)],
    id_utenza = ultimo_valido_per_gruppo(unita$id_utenza, f, quante),
    RFID = vapply(
      split(unita$RFID, f),
      function(x) paste(unique(rev(x)), collapse = ", "),
      ""
    ),
    comune_fornitura = ultimo_valido_per_gruppo(
      unita$comune_fornitura,
      f,
      quante
    ),
    tipo_categoria = ultimo_valido_per_gruppo(unita$tipo_categoria, f, quante),
    categoria = ultimo_valido_per_gruppo(unita$categoria, f, quante),
    sottocategoria = ultimo_valido_per_gruppo(unita$sottocategoria, f, quante),
    stato_fornitura = stato,
    nucleo_residenti = nucleo,
    potenziamento = potenziamento,
    contenitori_attivi = attive,
    volume = volume,
    raccolte_annue = raccolte,
    raccolte_ipotizzate = raccolte_ipotizzate,
    giorni_osservati = giorni,
    metodo = metodo,
    occasioni = occasioni,
    esposizioni = esposizioni,
    tasso_osservato = ifelse(occasioni > 0, esposizioni / occasioni, NA),
    tasso = stima$tasso,
    conferimenti_annui = conferimenti,
    conferimenti_annui_basso = stima$basso * annui,
    conferimenti_annui_alto = stima$alto * annui,
    conferimenti_per_giorni = esposizioni * 365 / pmax(giorni, 1),
    litri_consentiti = litri,
    conferimenti_consentiti = consentiti,
    conferimenti_eccedenti = pmax(conferimenti - consentiti, 0),
    litri_annui = conferimenti * volume,
    probabilita_oltre = probabilita,
    stringsAsFactors = FALSE
  )
  rownames(forniture) <- NULL
  forniture$classe <- classe_fornitura(forniture, parametri)
  forniture[
    order(
      match(forniture$classe, classi_conferimenti()),
      -forniture$conferimenti_eccedenti,
      -forniture$probabilita_oltre,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
}

#' Classe di ogni fornitura: prima i casi indecidibili, poi la probabilità di superare il limite
#'
#' Non attiva: `CESSATA`, `ANNULLATA`, o senza contenitori attivi.
#' @noRd
classe_fornitura <- function(forniture, parametri) {
  soglie <- parametri$soglie_probabilita
  domestica <- fornitura_domestica(forniture$sottocategoria)
  dplyr::case_when(
    is.na(domestica) ~ "Categoria sconosciuta",
    !domestica ~ "Non domestica",
    forniture$contenitori_attivi == 0 |
      toupper(dplyr::coalesce(forniture$stato_fornitura, "")) %in%
        c("CESSATA", "CESSATO", "ANNULLATA", "ANNULLATO") ~ "Non attiva",
    is.na(forniture$litri_consentiti) |
      is.na(forniture$volume) ~ "Nucleo mancante",
    forniture$occasioni < parametri$occasioni_minime ~ "Dati insufficienti",
    forniture$probabilita_oltre >= soglie[["oltre"]] ~ "Oltre il limite",
    forniture$probabilita_oltre >=
      soglie[["probabile"]] ~ "Probabile superamento",
    forniture$probabilita_oltre >= soglie[["vicino"]] ~ "Vicino al limite",
    .default = "Entro il limite"
  )
}

#' Tabella delle forniture senza righe
#' @noRd
forniture_vuote <- function() {
  data.frame(
    id_fornitura = character(0),
    id_utenza = character(0),
    RFID = character(0),
    comune_fornitura = character(0),
    tipo_categoria = character(0),
    categoria = character(0),
    sottocategoria = character(0),
    stato_fornitura = character(0),
    nucleo_residenti = integer(0),
    potenziamento = logical(0),
    contenitori_attivi = integer(0),
    volume = numeric(0),
    raccolte_annue = numeric(0),
    raccolte_ipotizzate = logical(0),
    giorni_osservati = numeric(0),
    metodo = character(0),
    occasioni = numeric(0),
    esposizioni = numeric(0),
    tasso_osservato = numeric(0),
    tasso = numeric(0),
    conferimenti_annui = numeric(0),
    conferimenti_annui_basso = numeric(0),
    conferimenti_annui_alto = numeric(0),
    conferimenti_per_giorni = numeric(0),
    litri_consentiti = numeric(0),
    conferimenti_consentiti = numeric(0),
    conferimenti_eccedenti = numeric(0),
    litri_annui = numeric(0),
    probabilita_oltre = numeric(0),
    classe = character(0),
    stringsAsFactors = FALSE
  )
}

#' Quota di forniture domestiche stimate con il potenziamento, per fascia di litri
#'
#' Quota avvicinata a un mezzo: (con potenziamento + 0,5) / (forniture + 1).
#' @noRd
manovra_per_fascia <- function(forniture) {
  stimate <- forniture$classe %in% classi_conferimenti()[1:4]
  fascia <- fascia_litri(forniture$litri_annui[stimate])
  livelli <- levels(fascia_litri(numeric(0)))
  totale <- as.vector(table(factor(fascia, levels = livelli)))
  con <- as.vector(table(factor(
    fascia[forniture$potenziamento[stimate]],
    levels = livelli
  )))
  data.frame(
    fascia = factor(livelli, levels = livelli, ordered = TRUE),
    forniture = totale,
    con_potenziamento = con,
    quota = (con + 0.5) / (totale + 1),
    stringsAsFactors = FALSE
  )
}

#' Conferimenti stimati dei non censiti
#' Contesto: un bidone non a database potrebbe appartenere a una fornitura con
#' la manovra.
#' Funzionamento: come per le forniture, con raccolte e volume da
#' `completa_non_censiti()`; in più nucleo minimo, quota del potenziamento nella
#' fascia e candidato sostituito (`candidati_sostituzione()`).
#' @return Una riga per non censito.
#' @noRd
stima_non_censiti <- function(
  unita,
  censite,
  contenitori,
  tasso_medio,
  manovra,
  parametri
) {
  stima <- stima_tasso(
    unita$esposizioni,
    unita$occasioni,
    tasso_medio,
    parametri$peso_prior,
    parametri$livello_intervallo
  )
  conferimenti <- stima$tasso * unita$raccolte
  litri <- conferimenti * unita$volume
  fascia <- fascia_litri(litri)
  sostituzione <- candidati_sostituzione(
    unita,
    censite,
    contenitori,
    parametri$raggio_sostituzione_m
  )
  esito <- data.frame(
    RFID = unita$RFID,
    comune_lettura = unita$comune_lettura,
    latitudine = contenitori$lat[unita$contenitore],
    longitudine = contenitori$lon[unita$contenitore],
    data_prima_lettura = as.Date(unita$prima_lettura, origin = "1970-01-01"),
    data_ultima_lettura = as.Date(unita$ultima_lettura, origin = "1970-01-01"),
    metodo = unita$metodo,
    occasioni = unita$occasioni,
    esposizioni = unita$esposizioni,
    tasso_osservato = ifelse(
      unita$occasioni > 0,
      unita$esposizioni / unita$occasioni,
      NA
    ),
    tasso = stima$tasso,
    raccolte_annue_ipotizzate = unita$raccolte,
    volume_ipotizzato = unita$volume,
    origine_ipotesi = unita$origine_ipotesi,
    conferimenti_annui = conferimenti,
    conferimenti_annui_basso = stima$basso * unita$raccolte,
    conferimenti_annui_alto = stima$alto * unita$raccolte,
    litri_annui = litri,
    fascia_litri = as.character(fascia),
    nucleo_minimo = nucleo_minimo_compatibile(litri),
    quota_potenziamento = manovra$quota[match(
      as.character(fascia),
      as.character(manovra$fascia)
    )],
    dati_sufficienti = unita$occasioni >= parametri$occasioni_minime,
    fornitura_candidata = sostituzione$id_fornitura,
    utenza_candidata = sostituzione$id_utenza,
    RFID_candidato = sostituzione$RFID,
    distanza_candidato_m = sostituzione$distanza,
    data_ultima_lettura_candidato = sostituzione$ultima_lettura,
    potenziamento_candidato = sostituzione$potenziamento,
    stringsAsFactors = FALSE
  )
  esito[
    order(!esito$dati_sufficienti, -esito$litri_annui, method = "radix"),
    ,
    drop = FALSE
  ]
}

#' Contenitore censito che un non censito potrebbe aver sostituito
#' Contesto: un bidone nuovo al posto di uno censito spiega molti non censiti.
#' Funzionamento: il più vicino entro `raggio_m`, del secco, letto l'ultima volta
#' prima della prima lettura del non censito.
#' @noRd
candidati_sostituzione <- function(unita, censite, contenitori, raggio_m) {
  vuoto <- rep(NA_character_, nrow(unita))
  esito <- data.frame(
    id_fornitura = vuoto,
    id_utenza = vuoto,
    RFID = vuoto,
    distanza = rep(NA_real_, nrow(unita)),
    ultima_lettura = as.Date(vuoto),
    potenziamento = rep(NA, nrow(unita)),
    stringsAsFactors = FALSE
  )
  if (nrow(unita) == 0 || nrow(censite) == 0) {
    return(esito)
  }
  xy <- proietta_metri(
    c(contenitori$lat[unita$contenitore], contenitori$lat[censite$contenitore]),
    c(contenitori$lon[unita$contenitore], contenitori$lon[censite$contenitore])
  )
  nc <- seq_len(nrow(unita))
  trovati <- dbscan::frNN(
    xy[-nc, , drop = FALSE],
    eps = raggio_m,
    query = xy[nc, , drop = FALSE],
    sort = TRUE
  )
  quanti <- lengths(trovati$id)
  q <- rep(nc, quanti)
  candidato <- unlist(trovati$id, use.names = FALSE)
  distanza <- unlist(trovati$dist, use.names = FALSE)
  prima <- censite$ultima_lettura[candidato] < unita$prima_lettura[q] &
    censite$RFID[candidato] != unita$RFID[q]
  q <- q[prima]
  candidato <- candidato[prima]
  distanza <- distanza[prima]
  # Il primo rimasto di ogni non censito è il più vicino.
  primo <- !duplicated(q)
  q <- q[primo]
  candidato <- candidato[primo]
  esito$id_fornitura[q] <- censite$id_fornitura[candidato]
  esito$id_utenza[q] <- censite$id_utenza[candidato]
  esito$RFID[q] <- censite$RFID[candidato]
  esito$distanza[q] <- distanza[primo]
  esito$ultima_lettura[q] <- as.Date(
    censite$ultima_lettura[candidato],
    origin = "1970-01-01"
  )
  esito$potenziamento[q] <- censite$potenziamento[candidato] %in% TRUE
  esito
}

# --- Elenchi ------------------------------------------------------------------

#' Forniture domestiche attive da contattare: oltre il limite o probabile superamento
#' @noRd
elenco_da_contattare <- function(analisi) {
  forniture <- analisi$forniture
  scelte <- forniture$classe %in% classi_conferimenti()[1:2]
  elenco <- forniture[
    scelte,
    c(
      "id_fornitura",
      "id_utenza",
      "RFID",
      "comune_fornitura",
      "nucleo_residenti",
      "potenziamento",
      "volume",
      "raccolte_annue",
      "occasioni",
      "esposizioni",
      "conferimenti_annui",
      "conferimenti_annui_basso",
      "conferimenti_annui_alto",
      "conferimenti_consentiti",
      "conferimenti_eccedenti",
      "probabilita_oltre",
      "classe"
    ),
    drop = FALSE
  ]
  elenco[
    order(
      -elenco$conferimenti_eccedenti,
      -elenco$probabilita_oltre,
      method = "radix"
    ),
    ,
    drop = FALSE
  ]
}

#' Scrive tre CSV (forniture, da contattare, non censiti) per Excel in italiano
#' Funzionamento: punto e virgola, virgola decimale, due decimali.
#' @return I percorsi, invisibili.
#' @noRd
esporta_analisi_conferimenti <- function(analisi, cartella) {
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  periodo <- paste(format(analisi$periodo, "%Y-%m-%d"), collapse = "_")
  file <- c(
    forniture = sprintf("conferimenti_forniture_%s.csv", periodo),
    contattare = sprintf("conferimenti_da_contattare_%s.csv", periodo),
    non_censiti = sprintf("conferimenti_non_censiti_%s.csv", periodo)
  )
  percorsi <- file.path(cartella, file)
  tabelle <- list(
    analisi$forniture,
    elenco_da_contattare(analisi),
    analisi$non_censiti
  )
  # Due decimali bastano; le coordinate ne tengono sei.
  arrotonda <- function(tabella) {
    for (campo in names(tabella)) {
      if (is.double(tabella[[campo]]) && !inherits(tabella[[campo]], "Date")) {
        cifre <- if (campo %in% c("latitudine", "longitudine")) 6 else 2
        tabella[[campo]] <- round(tabella[[campo]], cifre)
      }
    }
    tabella
  }
  for (i in seq_along(tabelle)) {
    utils::write.csv2(
      arrotonda(tabelle[[i]]),
      percorsi[i],
      row.names = FALSE,
      na = ""
    )
  }
  invisible(stats::setNames(percorsi, names(file)))
}
