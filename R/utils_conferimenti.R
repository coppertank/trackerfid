# Conferimenti del secco: quanto conferisce ogni fornitura in un anno, a
# confronto con il limite della tariffa.
#
# Il secco è l'unico rifiuto che l'azienda fattura. Ogni fornitura domestica
# può conferire in un anno un certo numero di litri, secondo i componenti
# residenti del nucleo, oppure 6240 litri se ha diritto al potenziamento del
# secco; i conferimenti oltre il limite si pagano a parte. Le letture delle
# antenne dicono quando un contenitore è stato svuotato, ma non tutte: le
# antenne ci sono da poco e solo su una parte dei mezzi. Per questo non si
# contano le letture e basta: si misura, per ogni fornitura, quante volte ha
# esposto il bidone sulle volte in cui un mezzo con l'antenna è passato
# davanti, e si riporta quella frequenza alle raccolte di un anno.
#
# Le regole e le ragioni delle scelte sono spiegate nel report
# `docs/conferimenti_secco.Rmd`. Le funzioni non usano reattività: l'app le
# potrà richiamare così come sono.

# --- Tariffa ----------------------------------------------------------------

#' Litri di secco conferibili in un anno, per componenti del nucleo
#'
#' Tabella della tariffa: vale per le forniture domestiche, secondo i
#' componenti residenti. Da cinque componenti in su il limite non cresce più.
#'
#' @return Data frame con `componenti` e `litri`.
#' @noRd
litri_conferibili <- function() {
  data.frame(
    componenti = 1:5,
    litri = c(720, 960, 1200, 1440, 1680)
  )
}

#' Litri di secco conferibili in un anno con il potenziamento del secco
#'
#' Sostituisce il limite del nucleo, qualunque sia il numero dei componenti.
#' @noRd
litri_potenziamento <- function() {
  6240
}

#' Manovre sociali che danno diritto al potenziamento del secco
#'
#' Sono i valori del campo `manovra_sociale` del CSV del passo 3. Una
#' fornitura con una di queste manovre in vigore può conferire fino a
#' `litri_potenziamento()` litri.
#' @noRd
manovre_potenziamento <- function() {
  c(
    "MANOVRA SOCIALE NEONATI",
    "MANOVRA SOCIALE ANZIANI",
    "MANOVRA SOCIALE DISABILI"
  )
}

#' Dice se una manovra sociale dà il potenziamento del secco
#' @noRd
ha_potenziamento <- function(manovra) {
  toupper(stringr::str_squish(as.character(manovra))) %in% manovre_potenziamento()
}

#' Litri di secco che una fornitura può conferire in un anno
#'
#' Con il potenziamento valgono `litri_potenziamento()` litri. Altrimenti il
#' limite dipende dai componenti residenti: un nucleo di zero residenti vale
#' come uno, uno di cinque o più come cinque. Senza il nucleo, e senza
#' potenziamento, il limite manca.
#'
#' @param nucleo_residenti Componenti residenti del nucleo.
#' @param potenziamento `TRUE` se la fornitura ha il potenziamento.
#' @return I litri, uno per elemento.
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

#' Dice se una fornitura è domestica, dalla sua tipologia
#'
#' `TRUE` per le tipologie che iniziano per `DOMESTIC` e per il codice `UD`;
#' `FALSE` per quelle che contengono `NON DOMESTIC` e per il codice `UND`;
#' mancante per le altre, che l'analisi non sa classificare. Il report
#' elenca le tipologie trovate e come sono state lette.
#'
#' @param tipo_categoria Tipologia della fornitura.
#' @noRd
fornitura_domestica <- function(tipo_categoria) {
  valori <- toupper(stringr::str_squish(as.character(tipo_categoria)))
  esito <- rep(NA, length(valori))
  non_domestica <- grepl("NON DOMESTIC", valori, fixed = TRUE) |
    valori %in% c("UND", "NON DOMESTICA", "NON DOMESTICO")
  domestica <- !non_domestica &
    (startsWith(valori, "DOMESTIC") | valori %in% "UD")
  esito[domestica %in% TRUE] <- TRUE
  esito[non_domestica %in% TRUE] <- FALSE
  esito
}

# --- Parametri ----------------------------------------------------------------

#' Parametri dell'analisi dei conferimenti
#'
#' - `raggio_vicini_m`: distanza massima, in metri, tra un contenitore e i
#'   vicini le cui letture dicono che il mezzo è passato.
#' - `vicini_massimi`: vicini considerati, i più vicini.
#' - `vicini_minimi`: vicini che servono perché i passaggi si possano
#'   contare; con meno, le occasioni si stimano dalle raccolte previste.
#' - `distanza_anomala_m`: una lettura più lontana di così dalla posizione
#'   abituale del contenitore non conta.
#' - `quota_giorni_anomala`: un contenitore letto in una quota maggiore dei
#'   giorni tra la sua prima e la sua ultima lettura non è un bidone esposto
#'   per la raccolta, ma un tag fermo in deposito o su un mezzo.
#' - `peso_prior`: peso, in occasioni, della frequenza media nella stima del
#'   tasso di esposizione.
#' - `occasioni_minime`: occasioni osservate sotto le quali la stima non
#'   basta per contattare una fornitura.
#' - `livello_intervallo`: probabilità dell'intervallo di stima.
#' - `soglie_probabilita`: probabilità di superare il limite oltre cui una
#'   fornitura è «Oltre il limite», «Probabile superamento», «Vicino al
#'   limite».
#' - `raggio_sostituzione_m`: distanza massima tra un non censito e un
#'   contenitore censito che potrebbe aver sostituito.
#'
#' @param ... Valori da cambiare.
#' @return Lista dei parametri.
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
    stop("Parametri sconosciuti: ", paste(ignoti, collapse = ", "), call. = FALSE)
  }
  utils::modifyList(base, modifiche)
}

#' Classi delle forniture, dalla più urgente
#'
#' Le prime quattro dicono quanto è probabile che la fornitura superi il
#' limite; le altre perché non si può dire.
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

# --- Lettura del file ------------------------------------------------------------

#' Colonne del CSV del passo 3 che servono all'analisi dei conferimenti
#'
#' Le obbligatorie sono quelle senza le quali l'analisi non ha senso; le
#' altre, se mancano, restano vuote.
#' @noRd
colonne_conferimenti <- function() {
  list(
    obbligatorie = c(
      "giorno_lettura",
      "RFID",
      "presente_a_database",
      "servizio_transponder",
      "servizio_atteso",
      "id_fornitura",
      "volume_previsto",
      "numero_raccolte_annue_previste",
      "nucleo_residenti",
      "tipo_categoria",
      "manovra_sociale",
      "latitudine",
      "longitudine"
    ),
    facoltative = c(
      "matricola_veicolo",
      "id_utenza",
      "stato_fornitura",
      "data_attivazione_contenitore",
      "data_cessazione_contenitore",
      "nucleo_non_residenti",
      "nucleo_totale",
      "categoria",
      "sottocategoria",
      "comune_fornitura",
      "comune_lettura"
    )
  )
}

#' Legge il CSV del passo 3 per l'analisi dei conferimenti
#'
#' L'app non tiene le colonne della fornitura che servono qui, come il nucleo
#' e la manovra sociale: per questo l'analisi legge il CSV, non l'archivio
#' `.duckdb`. Legge solo le colonne di `colonne_conferimenti()`, tutte come
#' testo, poi converte date, ore e numeri. I nomi alternativi di
#' `alias_colonne()` valgono anche qui.
#'
#' @param percorso Percorso del CSV, anche compresso con gzip.
#' @return Data frame delle letture.
#' @noRd
leggi_letture_conferimenti <- function(percorso) {
  if (!file.exists(percorso)) {
    stop("File non trovato: ", percorso, call. = FALSE)
  }
  separatore <- if (grepl("\\.gz$", percorso)) "auto" else rileva_separatore(percorso)
  intestazione <- names(data.table::fread(
    percorso,
    sep = separatore,
    nrows = 0,
    showProgress = FALSE
  ))
  # Nomi del file portati a quelli dell'analisi, senza badare alle maiuscole.
  nomi <- intestazione
  alias <- alias_colonne()
  da_alias <- tolower(nomi) %in% names(alias)
  nomi[da_alias] <- alias[tolower(nomi[da_alias])]
  colonne <- colonne_conferimenti()
  volute <- c(colonne$obbligatorie, colonne$facoltative)
  posizione <- match(tolower(volute), tolower(nomi))
  # Con due nomi per la stessa colonna vale il primo trovato.
  mancanti <- colonne$obbligatorie[is.na(posizione[seq_along(colonne$obbligatorie)])]
  if (length(mancanti) > 0) {
    stop(
      "Mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      ". Il file va rigenerato con il passo 3 della preparazione dei dati.",
      call. = FALSE
    )
  }
  presenti <- !is.na(posizione)
  letture <- data.table::fread(
    percorso,
    sep = separatore,
    select = posizione[presenti],
    colClasses = "character",
    na.strings = c("", "NA"),
    encoding = "UTF-8",
    strip.white = TRUE,
    showProgress = FALSE,
    data.table = FALSE
  )
  names(letture) <- volute[presenti]
  for (campo in volute[!presenti]) {
    letture[[campo]] <- NA_character_
  }
  letture <- letture[volute]

  numero <- function(x) suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
  # Le date sono poche, ripetute su milioni di righe: si convertono una volta.
  in_data <- function(x) {
    valori <- unique(x)
    date <- suppressWarnings(as.Date(substr(valori, 1, 10), optional = TRUE))
    date[match(x, valori)]
  }
  letture$giorno_lettura <- leggi_data_ora(letture$giorno_lettura)
  for (campo in c("latitudine", "longitudine", "volume_previsto", "numero_raccolte_annue_previste")) {
    letture[[campo]] <- numero(letture[[campo]])
  }
  for (campo in c("nucleo_residenti", "nucleo_non_residenti", "nucleo_totale")) {
    letture[[campo]] <- as.integer(numero(letture[[campo]]))
  }
  for (campo in c("data_attivazione_contenitore", "data_cessazione_contenitore")) {
    letture[[campo]] <- in_data(letture[[campo]])
  }
  for (campo in c("servizio_transponder", "servizio_atteso", "tipo_categoria", "manovra_sociale")) {
    letture[[campo]] <- toupper(letture[[campo]])
  }
  scartate <- is.na(letture$giorno_lettura) | is.na(letture$RFID) |
    is.na(letture$latitudine) | is.na(letture$longitudine)
  structure(letture[!scartate, , drop = FALSE], scartate = sum(scartate))
}

#' Date e ore del CSV, in UTC
#'
#' Il formato del passo 3, `AAAA-MM-GG HH:MM:SS`, si legge con un percorso
#' veloce; gli altri formati di `formati_data_ora()`, come le date italiane,
#' con `lubridate::parse_date_time()`, più lento.
#' @noRd
leggi_data_ora <- function(x) {
  esito <- lubridate::fast_strptime(x, "%Y-%m-%d %H:%M:%OS", tz = "UTC", lt = FALSE)
  altri <- which(is.na(esito) & !is.na(x))
  if (length(altri) > 0) {
    esito[altri] <- suppressWarnings(lubridate::parse_date_time(
      x[altri],
      orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY"),
      tz = "UTC",
      exact = FALSE
    ))
  }
  esito
}

# --- Posizioni, vicini e occasioni -------------------------------------------------

#' Posizione abituale di ogni contenitore e letture anomale
#'
#' La posizione abituale è la mediana delle letture del contenitore nei giri
#' del secco: poche letture lontane, di un mezzo che lo porta via o di un GPS
#' impreciso, non la spostano. Una lettura più lontana di
#' `distanza_anomala_m` è anomala. Un contenitore letto in almeno cinque
#' giorni, e in più di `quota_giorni_anomala` dei giorni tra la sua prima e
#' la sua ultima lettura, è un tag anomalo: un giro passa al più una o due
#' volte alla settimana.
#'
#' @param rfid,lat,lon,giorno Una riga per lettura nei giri del secco.
#' @param parametri Vedi `parametri_conferimenti()`.
#' @return Lista: `contenitori`, data frame con `RFID`, `lat`, `lon`,
#'   `giorni` e `tag_anomalo`; `indice`, il contenitore di ogni lettura;
#'   `lontana`, se ogni lettura è anomala.
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
      tag_anomalo = giorni > parametri$quota_giorni_anomala * arco & giorni >= 5,
      stringsAsFactors = FALSE
    ),
    indice = indice,
    lontana = distanza > parametri$distanza_anomala_m
  )
}

#' Vicini di ogni contenitore
#'
#' Per ogni contenitore, fino a `massimo` altri contenitori entro `raggio_m`
#' metri dalla sua posizione abituale, dal più vicino.
#'
#' @param lat,lon Posizioni abituali.
#' @param raggio_m Distanza massima.
#' @param massimo Vicini al massimo.
#' @return Data frame con `da`, `a` e `distanza`: indici delle posizioni.
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
    unlist(lapply(seq_along(elenco), function(i) utils::head(elenco[[i]], quanti[i])), use.names = FALSE)
  }
  data.frame(
    da = da,
    a = as.integer(presi(vicini$id)),
    distanza = as.numeric(presi(vicini$dist))
  )
}

#' Occasioni di conferimento di ogni unità
#'
#' Un'occasione è un giorno in cui un mezzo con l'antenna, in un giro del
#' secco, è passato davanti al contenitore: lo dice la lettura, quel giorno,
#' del contenitore stesso o di uno dei suoi vicini. Contano solo i vicini
#' letti almeno una volta nello stesso giorno del contenitore: sono sul suo
#' stesso percorso. Un vicino su un'altra strada, raccolta in un altro
#' giorno, aggiungerebbe giorni in cui il mezzo non passava.
#'
#' @param unita Data frame con `contenitore`, l'indice del contenitore, e
#'   `inizio` e `fine`, i giorni della finestra, come interi.
#' @param giorni_unita Data frame con `u`, l'indice dell'unità, e `giorno`:
#'   i giorni in cui l'unità è stata letta, anche fuori dalla finestra.
#' @param evidenza Data frame con `c`, l'indice del contenitore, e `giorno`:
#'   i giorni in cui ogni contenitore è stato letto in un giro del secco.
#' @param vicini Risultato di `vicini_contenitori()`.
#' @param n_contenitori Numero dei contenitori.
#' @return Lista con `occasioni`, i giorni di passaggio nella finestra, e
#'   `vicini`, i vicini sullo stesso percorso: uno per unità.
#' @noRd
occasioni_conferimento <- function(unita, giorni_unita, evidenza, vicini, n_contenitori) {
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

#' Tasso di esposizione: stima e intervallo
#'
#' Il tasso è la quota di occasioni in cui il bidone è stato esposto. Con
#' poche occasioni la quota osservata oscilla molto: due esposizioni su due
#' non vogliono dire che il bidone esce sempre. La stima avvicina allora la
#' quota alla media `media` di tutte le forniture, con il peso di `peso`
#' occasioni: è la media di una distribuzione Beta di parametri `alfa` e
#' `beta`. Con molte occasioni il peso della media scompare.
#'
#' @param k Esposizioni osservate.
#' @param n Occasioni osservate.
#' @param media Tasso medio.
#' @param peso Peso della media, in occasioni.
#' @param livello Probabilità dell'intervallo.
#' @return Data frame con `tasso`, `basso`, `alto`, `alfa` e `beta`.
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

#' Fasce di litri annui, dalla tabella della tariffa
#'
#' Un limite per fascia: fino a 720 litri basta un componente, fino a 1680 un
#' nucleo di cinque, fino a 6240 il potenziamento.
#'
#' @param litri Litri annui stimati.
#' @return Fattore ordinato.
#' @noRd
fascia_litri <- function(litri) {
  limiti <- c(litri_conferibili()$litri, litri_potenziamento())
  etichette <- c(
    sprintf("fino a %s", formatta_numero(limiti[1])),
    sprintf("%s-%s", formatta_numero(limiti[-length(limiti)] + 1), formatta_numero(limiti[-1])),
    sprintf("oltre %s", formatta_numero(limiti[length(limiti)]))
  )
  cut(litri, c(-Inf, limiti, Inf), labels = etichette, right = TRUE, ordered_result = TRUE)
}

#' Nucleo più piccolo il cui limite copre i litri stimati
#'
#' @param litri Litri annui stimati.
#' @return Testo: `"1"`, ..., `"5 o più"`, `"Solo con potenziamento"`
#'   oppure `"Oltre il potenziamento"`.
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
#' I passaggi, spiegati nel report `docs/conferimenti_secco.Rmd`:
#' 1. tiene le letture del periodo nei giri del secco, senza i sacchetti, e
#'    toglie le letture lontane dalla posizione abituale e i tag anomali;
#' 2. per ogni contenitore censito del secco, in ogni sua fornitura, e per
#'    ogni non censito stimato del secco conta i giorni in cui è stato letto,
#'    le esposizioni, e le occasioni, vedi `occasioni_conferimento()`;
#' 3. stima il tasso di esposizione e lo riporta alle raccolte di un anno;
#' 4. per ogni fornitura confronta i conferimenti stimati con quelli
#'    consentiti e calcola la probabilità di superare il limite;
#' 5. per ogni non censito dice quale nucleo basterebbe e quanto è probabile
#'    che abbia il potenziamento, dalle forniture censite con conferimenti
#'    simili, e cerca il contenitore censito che potrebbe aver sostituito.
#'
#' @param letture Letture, come le restituisce `leggi_letture_conferimenti()`.
#' @param dal,al Primo e ultimo giorno del periodo. Senza, dal primo giorno
#'   dell'anno dell'ultima lettura all'ultima lettura. Il periodo comincia
#'   comunque dal primo giorno con letture.
#' @param parametri Vedi `parametri_conferimenti()`.
#' @return Lista con `forniture`, `non_censiti`, `unita`,
#'   `manovra_per_fascia`, `riepilogo`, `periodo`, `tasso_medio` e
#'   `parametri`. `tasso_medio` ha due attributi: `unita`, quante unità lo
#'   determinano, e `tutte`, `TRUE` se nessuna fornitura domestica aveva
#'   abbastanza occasioni contate dai vicini e la media è su tutte le unità.
#' @noRd
analisi_conferimenti <- function(
  letture,
  dal = NULL,
  al = NULL,
  parametri = parametri_conferimenti()
) {
  giorno <- as.integer(as.Date(letture$giorno_lettura))
  al <- as.integer(as.Date(al %||% as.Date(max(giorno), origin = "1970-01-01")))
  dal <- as.integer(as.Date(
    dal %||% as.Date(sprintf("%s-01-01", format(as.Date(al, origin = "1970-01-01"), "%Y")))
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
  # Il periodo comincia dal primo giorno con letture: prima le antenne non
  # c'erano.
  dal <- max(dal, min(giorno))

  # 1. Letture nei giri del secco.
  secco <- servizio_secco(letture$servizio_atteso)
  riepilogo$fuori_dai_giri_secco <- sum(!secco)
  ls <- letture[secco, , drop = FALSE]
  gs <- giorno[secco]
  posizioni <- posizioni_contenitori(ls$RFID, ls$latitudine, ls$longitudine, gs, parametri)
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
  vicini <- vicini[vicini$da != vicini$a & !contenitori$tag_anomalo[vicini$a], , drop = FALSE]

  # 2. Unità: un contenitore censito del secco in una sua fornitura, oppure un
  # non censito il cui servizio stimato è il secco.
  censita <- valida & ls$presente_a_database == "Presente" &
    servizio_secco(ls$servizio_transponder) & !is.na(ls$id_fornitura)
  stime <- servizio_prevalente_non_censiti(letture)
  rfid_nc <- stime$RFID[servizio_secco(stime$servizio_prevalente)]
  non_censita <- valida & ls$presente_a_database == "Non Presente" & ls$RFID %in% rfid_nc
  riepilogo$contenitori_censiti <- length(unique(ls$RFID[censita]))
  riepilogo$non_censiti <- length(unique(ls$RFID[non_censita]))

  chiave <- ifelse(censita, paste(ls$RFID, ls$id_fornitura, sep = "\r"), ls$RFID)
  presa <- censita | non_censita
  ordine <- which(presa)[order(gs[presa], ls$giorno_lettura[presa], method = "radix")]
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
    raccolte = ultimo_valido_per_gruppo(r$numero_raccolte_annue_previste, u, quante),
    nucleo_residenti = ultimo_valido_per_gruppo(r$nucleo_residenti, u, quante),
    tipo_categoria = ultimo_valido_per_gruppo(r$tipo_categoria, u, quante),
    categoria = ultimo_valido_per_gruppo(r$categoria, u, quante),
    sottocategoria = ultimo_valido_per_gruppo(r$sottocategoria, u, quante),
    # La manovra dell'ultima lettura: vuota se la manovra è finita.
    manovra_sociale = ultimo_per_gruppo(r$manovra_sociale, u, quante),
    attivazione = estremi_per_gruppo(as.integer(r$data_attivazione_contenitore), u, quante)$minimo,
    cessazione = estremi_per_gruppo(as.integer(r$data_cessazione_contenitore), u, quante)$massimo,
    prima_lettura = primo_per_gruppo(gu, u, quante),
    ultima_lettura = ultimo_per_gruppo(gu, u, quante),
    stringsAsFactors = FALSE
  )
  # Finestra: dal giorno dopo la prima lettura, e non prima dell'attivazione
  # del contenitore nella fornitura. Un bidone compare nei dati solo se è
  # stato letto almeno una volta: contare quella prima lettura, e le
  # occasioni prima, gonfierebbe il tasso, tanto più quanto meno occasioni ci
  # sono. Dopo la prima lettura ogni occasione è una prova a sé.
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
  occasioni <- occasioni_conferimento(unita, giorni_unita, evidenza, vicini, nrow(contenitori))
  unita$vicini <- occasioni$vicini
  unita$giorni_finestra <- pmax(unita$fine - unita$inizio + 1L, 0L)

  # Raccolte e volume: per un non censito quelli più frequenti tra i censiti
  # vicini, poi nel suo comune, poi tra tutti.
  unita <- completa_non_censiti(unita, vicini, contenitori)

  # Senza vicini sul percorso le occasioni si stimano dalle raccolte previste,
  # come se tutti i mezzi avessero l'antenna: la stima del tasso è prudente.
  unita$metodo <- ifelse(unita$vicini >= parametri$vicini_minimi, "passaggi", "raccolte previste")
  dalle_raccolte <- round(unita$raccolte * unita$giorni_finestra / 365)
  unita$occasioni <- ifelse(
    unita$metodo == "passaggi",
    occasioni$occasioni,
    pmax(unita$esposizioni, dplyr::coalesce(dalle_raccolte, 0))
  )
  unita$occasioni <- pmax(unita$occasioni, unita$esposizioni)

  # 3. Tasso medio: le forniture domestiche censite con occasioni contate.
  base <- unita$tipo == "censito" & unita$metodo == "passaggi" &
    unita$occasioni >= parametri$occasioni_minime &
    fornitura_domestica(unita$tipo_categoria) %in% TRUE
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

  forniture <- stima_forniture(unita[unita$tipo == "censito", , drop = FALSE], tasso_medio, al, parametri)
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
    tasso_medio = structure(tasso_medio, unita = sum(base), tutte = base_ridotta),
    parametri = parametri
  )
}

#' Dice se un servizio è del secco
#'
#' Confronta la tipologia, come `coerenza_servizio()`: `SECCO PAP` è secco.
#' @noRd
servizio_secco <- function(servizio) {
  uniforma_servizio(servizio) %in% "SECCO"
}

#' Raccolte e volume dei non censiti, dai censiti vicini
#'
#' Di un non censito non si conoscono le raccolte previste né il volume. Si
#' prendono quelli più frequenti tra i contenitori censiti vicini: nella
#' stessa via i bidoni sono di solito dello stesso tipo e raccolti con la
#' stessa frequenza. Senza censiti vicini valgono quelli più frequenti nel
#' suo comune di lettura, poi tra tutti i censiti. `origine_ipotesi` dice da
#' dove vengono.
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
      dai_vicini <- piu_frequente_per_gruppo(del_contenitore[coppie$a], coppie$da, nrow(contenitori))
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

#' Stima dei conferimenti di ogni fornitura censita
#'
#' Somma esposizioni e occasioni dei contenitori del secco della fornitura,
#' stima il tasso e lo riporta alle raccolte di un anno, per i contenitori
#' attivi alla fine del periodo: un contenitore sostituito non conta due
#' volte, due contenitori attivi insieme sì.
#'
#' @param unita Unità censite di `analisi_conferimenti()`.
#' @param tasso_medio Tasso medio, vedi `stima_tasso()`.
#' @param al Ultimo giorno del periodo, come intero.
#' @param parametri Vedi `parametri_conferimenti()`.
#' @return Una riga per fornitura.
#' @noRd
stima_forniture <- function(unita, tasso_medio, al, parametri) {
  if (nrow(unita) == 0) {
    return(forniture_vuote())
  }
  f <- indice_gruppi(unita$id_fornitura)
  quante <- max(f)
  # Le unità in ordine di ultima lettura: gli attributi sono quelli più recenti.
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
    estremi_per_gruppo(unita$inizio, f, quante)$minimo + 1
  metodo <- ifelse(
    tabulate(f[unita$metodo != "passaggi"], quante) > 0,
    "raccolte previste",
    "passaggi"
  )
  contenitori_attivi <- pmax(attive, 1L)

  stima <- stima_tasso(esposizioni, occasioni, tasso_medio, parametri$peso_prior, parametri$livello_intervallo)
  annui <- raccolte * contenitori_attivi
  nucleo <- ultimo_valido_per_gruppo(unita$nucleo_residenti, f, quante)
  manovra <- ultimo_per_gruppo(unita$manovra_sociale, f, quante)
  potenziamento <- ha_potenziamento(manovra)
  litri <- litri_consentiti(nucleo, potenziamento)
  consentiti <- litri / volume
  conferimenti <- stima$tasso * annui
  quota_consentita <- consentiti / annui
  probabilita <- ifelse(
    quota_consentita >= 1,
    0,
    1 - stats::pbeta(pmin(quota_consentita, 1), stima$alfa, stima$beta)
  )
  tipo <- ultimo_valido_per_gruppo(unita$tipo_categoria, f, quante)
  stato <- ultimo_valido_per_gruppo(unita$stato_fornitura, f, quante)

  forniture <- data.frame(
    id_fornitura = unita$id_fornitura[!duplicated(f)],
    id_utenza = ultimo_valido_per_gruppo(unita$id_utenza, f, quante),
    RFID = vapply(split(unita$RFID, f), function(x) paste(unique(rev(x)), collapse = ", "), ""),
    comune_fornitura = ultimo_valido_per_gruppo(unita$comune_fornitura, f, quante),
    tipo_categoria = tipo,
    categoria = ultimo_valido_per_gruppo(unita$categoria, f, quante),
    sottocategoria = ultimo_valido_per_gruppo(unita$sottocategoria, f, quante),
    stato_fornitura = stato,
    nucleo_residenti = nucleo,
    manovra_sociale = manovra,
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
  forniture[order(
    match(forniture$classe, classi_conferimenti()),
    -forniture$conferimenti_eccedenti,
    -forniture$probabilita_oltre,
    method = "radix"
  ), , drop = FALSE]
}

#' Classe di ogni fornitura
#'
#' Prima le ragioni per cui non si può dire niente, poi la probabilità di
#' superare il limite. Vedi `classi_conferimenti()`.
#' @noRd
classe_fornitura <- function(forniture, parametri) {
  soglie <- parametri$soglie_probabilita
  domestica <- fornitura_domestica(forniture$tipo_categoria)
  dplyr::case_when(
    is.na(domestica) ~ "Categoria sconosciuta",
    !domestica ~ "Non domestica",
    forniture$contenitori_attivi == 0 |
      toupper(dplyr::coalesce(forniture$stato_fornitura, "")) %in% c("CESSATO", "CESSATA") ~ "Non attiva",
    is.na(forniture$litri_consentiti) | is.na(forniture$volume) ~ "Nucleo mancante",
    forniture$occasioni < parametri$occasioni_minime ~ "Dati insufficienti",
    forniture$probabilita_oltre >= soglie[["oltre"]] ~ "Oltre il limite",
    forniture$probabilita_oltre >= soglie[["probabile"]] ~ "Probabile superamento",
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
    manovra_sociale = character(0),
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

#' Quota di forniture con il potenziamento, per fascia di litri annui
#'
#' Tra le forniture domestiche censite con una stima, cioè non in «Dati
#' insufficienti», quante hanno il potenziamento in ogni fascia di
#' `fascia_litri()`. La quota è avvicinata a un mezzo quando le forniture
#' sono poche: (con potenziamento + 0,5) / (forniture + 1).
#'
#' @param forniture Risultato di `stima_forniture()`.
#' @return Data frame con `fascia`, `forniture`, `con_potenziamento` e
#'   `quota`.
#' @noRd
manovra_per_fascia <- function(forniture) {
  stimate <- forniture$classe %in% classi_conferimenti()[1:4]
  fascia <- fascia_litri(forniture$litri_annui[stimate])
  livelli <- levels(fascia_litri(numeric(0)))
  totale <- as.vector(table(factor(fascia, levels = livelli)))
  con <- as.vector(table(factor(fascia[forniture$potenziamento[stimate]], levels = livelli)))
  data.frame(
    fascia = factor(livelli, levels = livelli, ordered = TRUE),
    forniture = totale,
    con_potenziamento = con,
    quota = (con + 0.5) / (totale + 1),
    stringsAsFactors = FALSE
  )
}

#' Stima dei conferimenti dei non censiti
#'
#' Come per una fornitura, ma con raccolte e volume presi dai censiti vicini:
#' vedi `completa_non_censiti()`. Dai litri annui stimati vengono il nucleo
#' più piccolo che li coprirebbe e la quota di forniture censite con il
#' potenziamento nella stessa fascia di litri. Il contenitore censito più
#' vicino, entro `raggio_sostituzione_m`, letto l'ultima volta prima della
#' prima lettura del non censito è il candidato che potrebbe aver
#' sostituito.
#'
#' @param unita Unità non censite di `analisi_conferimenti()`.
#' @param censite Unità censite.
#' @param contenitori Contenitori di `posizioni_contenitori()`.
#' @param tasso_medio Tasso medio.
#' @param manovra Risultato di `manovra_per_fascia()`.
#' @param parametri Vedi `parametri_conferimenti()`.
#' @return Una riga per non censito.
#' @noRd
stima_non_censiti <- function(unita, censite, contenitori, tasso_medio, manovra, parametri) {
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
  sostituzione <- candidati_sostituzione(unita, censite, contenitori, parametri$raggio_sostituzione_m)
  esito <- data.frame(
    RFID = unita$RFID,
    comune_lettura = unita$comune_lettura,
    latitudine = contenitori$lat[unita$contenitore],
    longitudine = contenitori$lon[unita$contenitore],
    prima_lettura = as.Date(unita$prima_lettura, origin = "1970-01-01"),
    ultima_lettura = as.Date(unita$ultima_lettura, origin = "1970-01-01"),
    metodo = unita$metodo,
    occasioni = unita$occasioni,
    esposizioni = unita$esposizioni,
    tasso_osservato = ifelse(unita$occasioni > 0, unita$esposizioni / unita$occasioni, NA),
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
    quota_potenziamento = manovra$quota[match(as.character(fascia), as.character(manovra$fascia))],
    dati_sufficienti = unita$occasioni >= parametri$occasioni_minime,
    fornitura_candidata = sostituzione$id_fornitura,
    utenza_candidata = sostituzione$id_utenza,
    RFID_candidato = sostituzione$RFID,
    distanza_candidato_m = sostituzione$distanza,
    ultima_lettura_candidato = sostituzione$ultima_lettura,
    manovra_candidata = sostituzione$manovra_sociale,
    stringsAsFactors = FALSE
  )
  esito[order(!esito$dati_sufficienti, -esito$litri_annui, method = "radix"), , drop = FALSE]
}

#' Contenitore censito che un non censito potrebbe aver sostituito
#'
#' Un bidone rotto o perso viene sostituito con uno nuovo, che a volte non
#' viene legato alla fornitura: il vecchio smette di essere letto e il nuovo
#' comincia, nello stesso punto. Per ogni non censito si cercano i
#' contenitori censiti del secco entro `raggio_m` dalla sua posizione,
#' letti l'ultima volta prima della sua prima lettura, e si tiene il più
#' vicino.
#'
#' @return Data frame, una riga per unità non censita, con `id_fornitura`,
#'   `id_utenza`, `RFID`, `distanza`, `ultima_lettura` e `manovra_sociale`
#'   del candidato, vuoti se non ce n'è.
#' @noRd
candidati_sostituzione <- function(unita, censite, contenitori, raggio_m) {
  vuoto <- rep(NA_character_, nrow(unita))
  esito <- data.frame(
    id_fornitura = vuoto,
    id_utenza = vuoto,
    RFID = vuoto,
    distanza = rep(NA_real_, nrow(unita)),
    ultima_lettura = as.Date(vuoto),
    manovra_sociale = vuoto,
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
  esito$ultima_lettura[q] <- as.Date(censite$ultima_lettura[candidato], origin = "1970-01-01")
  esito$manovra_sociale[q] <- censite$manovra_sociale[candidato]
  esito
}

# --- Elenchi ------------------------------------------------------------------

#' Forniture da contattare
#'
#' Le forniture domestiche attive «Oltre il limite» o con un «Probabile
#' superamento», dalla più alta stima dei conferimenti eccedenti, con le sole
#' colonne che servono a chi le contatta.
#'
#' @param analisi Risultato di `analisi_conferimenti()`.
#' @return Data frame.
#' @noRd
elenco_da_contattare <- function(analisi) {
  forniture <- analisi$forniture
  scelte <- forniture$classe %in% classi_conferimenti()[1:2]
  elenco <- forniture[scelte, c(
    "id_fornitura",
    "id_utenza",
    "RFID",
    "comune_fornitura",
    "nucleo_residenti",
    "manovra_sociale",
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
  ), drop = FALSE]
  elenco[order(-elenco$conferimenti_eccedenti, -elenco$probabilita_oltre, method = "radix"), , drop = FALSE]
}

#' Scrive i risultati dell'analisi dei conferimenti in CSV
#'
#' Tre file, con il periodo nel nome: tutte le forniture, le forniture da
#' contattare e i non censiti. Separatore punto e virgola e virgola
#' decimale, per aprirli con Excel in italiano; numeri con due decimali.
#'
#' @param analisi Risultato di `analisi_conferimenti()`.
#' @param cartella Cartella in cui scriverli.
#' @return I percorsi dei file, invisibili.
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
  tabelle <- list(analisi$forniture, elenco_da_contattare(analisi), analisi$non_censiti)
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
    utils::write.csv2(arrotonda(tabelle[[i]]), percorsi[i], row.names = FALSE, na = "")
  }
  invisible(stats::setNames(percorsi, names(file)))
}
