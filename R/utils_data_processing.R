# Schema del file, servizi, coerenze, statistiche e storia di un RFID.

#' Campi obbligatori del CSV
#' @noRd
colonne_obbligatorie <- function() {
  c(
    "giorno_lettura",
    "targa_veicolo",
    "matricola_veicolo",
    "RFID",
    "presente_a_database",
    "servizio_atteso",
    "id_utenza",
    "latitudine",
    "longitudine"
  )
}

#' Campi facoltativi del CSV: quantità per i cluster, servizi e geografia
#' @noRd
colonne_facoltative <- function() {
  c(colonne_quantita(), colonne_geografiche())
}

#' Colonna del servizio del contenitore a database, vuota per non censiti e sacchetti
#' @noRd
colonne_contenitore <- function() "servizio_transponder"

#' Nomi alternativi delle colonne del file
#'
#' `volume` e `numero_raccolte_annue` della pipeline; `comune_da_database`
#' (fino al 17/10/2026) e `comune` dei file vecchi. Valgono solo se manca la
#' colonna con il nome dell'app.
#' @return Vettore con nome: nome del file -> nome dell'app.
#' @noRd
alias_colonne <- function() {
  c(
    volume = "volume_previsto",
    numero_raccolte_annue = "numero_raccolte_annue_previste",
    comune_da_database = "comune_fornitura",
    comune = "comune_fornitura"
  )
}

#' Colonna del giro con il nome del calendario (`SECCO PAP`); se manca vale `servizio_atteso`
#' @noRd
colonne_calendario <- function() "servizio_giro"

#' Colonna della conferma del giro: `SI`, `NO` (non ancora) o vuota
#'
#' La mostra la tabella dei giri; nessun conto la usa.
#' @noRd
colonna_conferma <- function() "eseguito"

#' Colonna dei metri dal comune dichiarato, scritta dalla pipeline
#'
#' Solo per le letture in un altro comune; zero dentro il suo confine.
#' @noRd
colonna_distanza <- function() "distanza_comune_dichiarato"

#' Metri oltre il confine del comune dichiarato entro cui una lettura è coerente
#'
#' Portata dell'antenna più errore del GPS.
#' @noRd
tolleranza_confine_m <- function() 100

#' Campi facoltativi numerici del CSV
#' @noRd
colonne_quantita <- function() {
  c("volume_previsto", "numero_raccolte_annue_previste")
}

#' Campi geografici del CSV: comune della fornitura, del giro, di lettura e cantiere
#' @noRd
colonne_geografiche <- function() {
  c("comune_fornitura", "comune_giro", "comune_lettura", "cantiere")
}

#' Campi calcolati dalla validazione, anche se il file li contiene già
#'
#' Vedi `aggiungi_geografia()` e `coerenza_servizio()`.
#' @noRd
colonne_derivate <- function() {
  c(
    "comune_assegnato",
    "comune_geografico",
    "coerenza_comune",
    "coerenza_servizio"
  )
}

#' Colonne della pipeline che l'app non usa, con il tipo SQL
#'
#' Le tiene `colonne = "tutte"` (analisi dei conferimenti). Mancanti: vuote
#' senza avvisi; non convertibili: vuote con un avviso.
#' @return Vettore con nome: `VARCHAR`, `DATE`, `INTEGER` o `BOOLEAN`.
#' @noRd
tipi_colonne_preparazione <- function() {
  c(
    giorno = "DATE",
    id_giro = "VARCHAR",
    frequenza = "VARCHAR",
    data_attivazione_contenitore = "DATE",
    data_cessazione_contenitore = "DATE",
    id_fornitura = "VARCHAR",
    stato_fornitura = "VARCHAR",
    nucleo_residenti = "INTEGER",
    nucleo_non_residenti = "INTEGER",
    nucleo_totale = "INTEGER",
    tipo_categoria = "VARCHAR",
    categoria = "VARCHAR",
    sottocategoria = "VARCHAR",
    potenziamento_secco = "BOOLEAN"
  )
}

#' Nomi delle colonne di `tipi_colonne_preparazione()`
#' @noRd
colonne_preparazione <- function() names(tipi_colonne_preparazione())

#' Colonne del dataset validato che l'app porta in R
#' @noRd
colonne_app <- function() {
  c(
    colonne_obbligatorie(),
    colonne_contenitore(),
    colonne_facoltative(),
    colonne_calendario(),
    colonna_conferma(),
    colonna_distanza(),
    colonne_derivate()
  )
}

#' Tutte le colonne del dataset validato: quelle dell'app, poi quelle della pipeline
#' @noRd
colonne_dataset <- function() {
  c(colonne_app(), colonne_preparazione())
}

#' Valori ammessi per `presente_a_database`
#' @noRd
stati_database <- function() c("Presente", "Non Presente")

#' Valori di `coerenza_comune`, con i nomi usati dai filtri
#' @noRd
stati_coerenza_comune <- function() {
  c(
    coerente = "Coerente",
    fuori_comune = "Fuori comune",
    fuori_giro = "Fuori giro",
    ignota = "Coerenza ignota"
  )
}

#' Coerenza tra comune di lettura e comune dichiarato
#'
#' Il dichiarato è quello del database per un censito (diverso: `Fuori
#' comune`) e quello del giro per un non censito (diverso: `Fuori giro`).
#' Coerente se coincidono, o se la lettura è entro `tolleranza_confine_m()`
#' dal dichiarato. Ignota se manca uno dei due comuni, o se il giro di un non
#' censito è una zona fuori tabella. Nomi confrontati con `chiave_comune()`.
#' Stessa regola in SQL in `valida_archivio()`.
#' @param data Letture; una colonna che manca vale come vuota.
#' @return Un valore di `stati_coerenza_comune()` per lettura.
#' @noRd
coerenza_comune <- function(data) {
  n <- nrow(data)
  chiave <- function(nome) {
    valori <- data[[nome]]
    if (is.null(valori)) {
      return(rep(NA_character_, n))
    }
    per_valori_distinti(as.character(valori), function(nomi) {
      chiavi <- chiave_comune(nomi)
      chiavi[!is.na(chiavi) & !nzchar(chiavi)] <- NA_character_
      chiavi
    })
  }
  lettura <- chiave("comune_lettura")
  censito <- data[["presente_a_database"]] %in% "Presente"
  giro <- chiave("comune_giro")
  dichiarato <- giro
  dichiarato[censito] <- chiave("comune_fornitura")[censito]
  distanza <- suppressWarnings(as.numeric(
    data[[colonna_distanza()]] %||% rep(NA_real_, n)
  ))
  # Un giro di zona, fuori tabella: niente confronto.
  giro_fuori_tabella <- !censito &
    !is.na(giro) &
    is.na(indice_comune(as.character(data[["comune_giro"]] %||% rep(NA, n))))
  stati <- stati_coerenza_comune()
  esito <- rep(stati[["ignota"]], n)
  noto <- !is.na(lettura) & !is.na(dichiarato) & !giro_fuori_tabella
  uguale <- noto &
    (lettura == dichiarato |
      (!is.na(distanza) & distanza <= tolleranza_confine_m()))
  esito[uguale] <- stati[["coerente"]]
  esito[noto & !uguale & censito] <- stati[["fuori_comune"]]
  esito[noto & !uguale & !censito] <- stati[["fuori_giro"]]
  unname(esito)
}

#' Letture fuori comune e fuori giro, dalla colonna `coerenza_comune`
#' @return Lista di vettori logici `fuori_comune` e `fuori_giro`.
#' @noRd
incongruenze_comune <- function(data) {
  stati <- stati_coerenza_comune()
  valori <- data[["coerenza_comune"]] %||% rep(NA_character_, nrow(data))
  list(
    fuori_comune = valori %in% stati[["fuori_comune"]],
    fuori_giro = valori %in% stati[["fuori_giro"]]
  )
}

#' Valori di `coerenza_servizio`, `SI` e `NO`, con i nomi usati dai filtri
#' @noRd
stati_coerenza <- function() {
  c(coerente = "SI", non_coerente = "NO")
}

#' Parole con cui l'app mostra `SI` e `NO` della coerenza del servizio
#' @noRd
etichette_coerenza <- function() {
  c(coerente = "Coerente", non_coerente = "Non coerente")
}

#' Coerenza tra servizio a database e servizio atteso dal giro
#'
#' `SI` se sono della stessa tipologia (`tipologia_servizio()`), altrimenti
#' `NO`; mancante se manca uno dei due, come per i non censiti. Un nome
#' sconosciuto fa tipologia a sé. Stessa regola in SQL in `valida_archivio()`.
#' @param data Letture; una colonna che manca vale come vuota.
#' @noRd
coerenza_servizio <- function(data) {
  n <- nrow(data)
  tipologia_di <- function(nome) {
    valori <- data[[nome]]
    if (is.null(valori)) {
      return(rep(NA_character_, n))
    }
    per_valori_distinti(as.character(valori), function(nomi) {
      puliti <- stringr::str_to_upper(stringr::str_trim(nomi))
      puliti[!is.na(puliti) & !nzchar(puliti)] <- NA_character_
      tipologia_servizio(puliti)
    })
  }
  del_contenitore <- tipologia_di("servizio_transponder")
  atteso <- tipologia_di("servizio_atteso")
  stati <- stati_coerenza()
  esito <- rep(NA_character_, n)
  noti <- !is.na(del_contenitore) & !is.na(atteso)
  esito[noti] <- ifelse(
    del_contenitore[noti] == atteso[noti],
    stati[["coerente"]],
    stati[["non_coerente"]]
  )
  esito
}

#' Servizio dato ai sacchetti, che non hanno una tipologia di rifiuto
#' @noRd
servizio_sacchetti <- function() "SACCHETTI"

#' Il codice è di un sacchetto? 24 caratteri con il prefisso `00BD`
#'
#' Stessa regola in SQL in `sql_sacchetto()`. Un codice mancante non lo è.
#' @noRd
rfid_sacchetto <- function(rfid) {
  rfid <- as.character(rfid)
  esito <- nchar(rfid) %in% 24L
  lunghi <- which(esito)
  esito[lunghi] <- toupper(substr(rfid[lunghi], 1, 4)) == "00BD"
  esito
}

#' Toglie le letture dei sacchetti, per le analisi sui contenitori
#'
#' Senza sacchetti restituisce `data` senza copiarlo.
#' @noRd
senza_sacchetti <- function(data) {
  sacchetto <- rfid_sacchetto(data$RFID)
  if (!any(sacchetto)) {
    return(data)
  }
  data[!sacchetto, , drop = FALSE]
}

#' Percorso del dataset di esempio incluso nel pacchetto
#' @noRd
percorso_dataset_esempio <- function() {
  app_sys("extdata", "sample_rfid_dataset.csv")
}

#' Segnala un errore di validazione con uno o più messaggi espliciti
#' @noRd
errore_validazione <- function(messaggi) {
  rlang::abort(
    paste(messaggi, collapse = "\n"),
    class = "errore_validazione",
    messaggi = messaggi
  )
}

#' Formati di file accettati: estensione e descrizione
#' @noRd
formati_file <- function() {
  c(
    csv = "CSV",
    txt = "CSV",
    gz = "CSV compresso con gzip",
    parquet = "Parquet",
    duckdb = "Archivio DuckDB"
  )
}

#' Prime righe del file con un problema, per i messaggi
#' @noRd
elenco_righe <- function(indici, massimo = 5) {
  # +1: la prima riga del file è l'intestazione
  righe <- utils::head(indici, massimo) + 1
  testo <- paste(righe, collapse = ", ")
  if (length(indici) > massimo) paste0(testo, ", \u2026") else testo
}

# File letti da percorso: restano in memoria per tutte le sessioni del processo.
memoria_file <- new.env(parent = emptyenv())

#' Legge un file una volta per processo, condiviso da tutte le sessioni
#'
#' Se il file cambia sul disco viene riletto.
#' @param lettore Di norma `carica_archivio()`.
#' @noRd
leggi_una_volta <- function(lettore, path) {
  if (!file.exists(path)) {
    errore_validazione(paste0("File non trovato: ", path, "."))
  }
  info <- file.info(path)
  chiave <- normalizePath(path)
  firma <- paste(info$size, as.numeric(info$mtime))
  voce <- memoria_file[[chiave]]
  if (is.null(voce) || !identical(voce$firma, firma)) {
    voce <- list(firma = firma, esito = lettore(path))
    memoria_file[[chiave]] <- voce
  }
  voce$esito
}

#' Ordina i servizi: prima i noti, nell'ordine canonico, poi gli altri
#' @noRd
ordina_servizi <- function(servizi) {
  servizi <- as.character(unique(servizi[!is.na(servizi)]))
  noti <- servizi_config()$servizio
  as.character(c(intersect(noti, servizi), sort(setdiff(servizi, noti))))
}

#' Letture di un periodo, estremi inclusi; senza copia se c'è tutto
#' @param periodo Due date, o `NULL`.
#' @noRd
filtra_periodo <- function(data, periodo = NULL) {
  if (is.null(periodo)) {
    return(data)
  }
  periodo <- lubridate::as_date(periodo)
  inizio <- as.POSIXct(format(periodo[1]), tz = "UTC")
  fine <- as.POSIXct(format(periodo[2] + 1), tz = "UTC")
  nel_periodo <- data$giorno_lettura >= inizio & data$giorno_lettura < fine
  if (all(nel_periodo)) {
    return(data)
  }
  data[nel_periodo, , drop = FALSE]
}

#' Periodo di un anno solare
#' @noRd
periodo_anno <- function(anno) {
  c(lubridate::make_date(anno, 1, 1), lubridate::make_date(anno, 12, 31))
}

#' Descrizione breve di un periodo: "Anno 2025" oppure le due date
#' @noRd
etichetta_periodo <- function(periodo) {
  if (is.null(periodo)) {
    return(NULL)
  }
  periodo <- lubridate::as_date(periodo)
  anno <- lubridate::year(periodo[1])
  if (identical(periodo, periodo_anno(anno))) {
    paste("Anno", anno)
  } else {
    paste(format(periodo, "%d/%m/%Y"), collapse = " - ")
  }
}

#' Servizio stimato dei non censiti: regola dell'80%, riferimento per `tabella_stima()`
#'
#' Sulle letture "Non Presente" di ogni RFID: se la tipologia più frequente
#' del servizio atteso raggiunge `soglia`, il giro più letto di quella
#' tipologia; altrimenti `NA`.
#' @return Tibble con `RFID`, `quota`, `servizio_prevalente`, solo per gli
#'   RFID con almeno una stima.
#' @noRd
servizio_prevalente_non_censiti <- function(data, soglia = 0.8) {
  stimate <- data$presente_a_database == "Non Presente" &
    !is.na(data$servizio_atteso)
  conteggi <- dplyr::count(
    dplyr::tibble(
      RFID = data$RFID[stimate],
      servizio_atteso = data$servizio_atteso[stimate]
    ),
    .data$RFID,
    .data$servizio_atteso,
    name = "n"
  )
  if (nrow(conteggi) == 0) {
    return(dplyr::tibble(
      RFID = character(0),
      quota = numeric(0),
      servizio_prevalente = character(0)
    ))
  }
  tipologia <- tipologia_servizio(conteggi$servizio_atteso)
  rfid <- indice_gruppi(conteggi$RFID)
  totale <- somma_per_gruppo(conteggi$n, rfid)[rfid]
  coppia <- indice_gruppi(paste(rfid, tipologia, sep = "\r"))
  n_tipologia <- somma_per_gruppo(conteggi$n, coppia)[coppia]

  # Per ogni RFID la tipologia più letta e, dentro, il giro più letto.
  ordine <- order(
    rfid,
    -n_tipologia,
    tipologia,
    -conteggi$n,
    conteggi$servizio_atteso,
    method = "radix"
  )
  primo <- ordine[!duplicated(rfid[ordine])]
  quota <- n_tipologia[primo] / totale[primo]
  dplyr::tibble(
    RFID = conteggi$RFID[primo],
    quota = quota,
    servizio_prevalente = dplyr::if_else(
      quota >= soglia,
      conteggi$servizio_atteso[primo],
      NA_character_
    )
  )
}

#' Aggiunge `servizio_icona`, regola di riferimento per `sql_con_icona()`
#'
#' Censito: `servizio_transponder`. Non censito: il servizio stimato
#' dell'RFID, `NA` (punto di domanda) sotto soglia. Sacchetto: il servizio
#' dei sacchetti. Dà icona, voce dei filtri e statistiche.
#' @param riferimento Letture su cui stimare il servizio dei non censiti.
#' @noRd
aggiungi_servizio_icona <- function(
  letture,
  riferimento = letture,
  soglia = 0.8
) {
  prevalenti <- servizio_prevalente_non_censiti(riferimento, soglia)
  stimato <- prevalenti$servizio_prevalente[match(
    letture$RFID,
    prevalenti$RFID
  )]
  censita <- letture$presente_a_database == "Presente"
  del_contenitore <- letture[["servizio_transponder"]] %||%
    rep(NA_character_, nrow(letture))
  letture$servizio_icona <- ifelse(
    censita,
    as.character(del_contenitore),
    stimato
  )
  letture$servizio_icona[
    per_valori_distinti(as.character(letture$RFID), rfid_sacchetto)
  ] <- servizio_sacchetti()
  letture
}

#' Servizio con cui una lettura compare su marker, filtri e statistiche
#'
#' La colonna `servizio_icona`; se manca la calcola sulle letture ricevute,
#' che per una riga per RFID dà una stima diversa: va aggiunta prima.
#' @noRd
servizio_mostrato <- function(data) {
  if (is.null(data[["servizio_icona"]])) {
    data <- aggiungi_servizio_icona(data)
  }
  data$servizio_icona
}

#' Voce che nei filtri e nelle statistiche indica le letture senza cantiere
#' @noRd
senza_cantiere <- function() "Non assegnato"

#' Cantieri delle letture nell'ordine dei filtri: tabella, altri, "Non assegnato"
#' @noRd
ordina_cantieri <- function(valori) {
  presenti <- as.character(unique(valori[!is.na(valori)]))
  noti <- cantieri()
  c(
    intersect(noti, presenti),
    sort(setdiff(presenti, noti)),
    if (anyNA(valori)) senza_cantiere()
  )
}

#' Conteggi per servizio, in ordine canonico
#' @noRd
conta_servizi <- function(valori) {
  valori <- valori[!is.na(valori)]
  ordine <- ordina_servizi(valori)
  conteggi <- table(factor(valori, levels = ordine))
  stats::setNames(as.integer(conteggi), ordine)
}

#' Conteggi per cantiere, nell'ordine dei filtri; i mancanti sotto `senza_cantiere()`
#' @noRd
conta_cantieri <- function(valori) {
  if (is.null(valori) || all(is.na(valori))) {
    return(stats::setNames(integer(0), character(0)))
  }
  ordine <- ordina_cantieri(valori)
  valori[is.na(valori)] <- senza_cantiere()
  conteggi <- table(factor(valori, levels = ordine))
  stats::setNames(as.integer(conteggi), ordine)
}

#' Statistiche della barra laterale, da una riga per RFID con `servizio_icona`
#'
#' @param letture_cantieri Letture per cantiere, da `conteggi_cantieri()`.
#' @return Lista: totale, per stato, `servizio_censiti`, `servizio_stimato`,
#'   `stima_incerta`, `giri`, `non_coerenti`, `rfid_cantieri`,
#'   `letture_cantieri`, incongruenze del comune.
#' @noRd
calcola_statistiche <- function(data, letture_cantieri = NULL) {
  censiti <- data$presente_a_database == "Presente"
  servizio <- servizio_mostrato(data)
  incongruenze <- incongruenze_comune(data)
  list(
    totale = nrow(data),
    presente = sum(censiti),
    non_presente = sum(!censiti),
    servizio_censiti = conta_servizi(servizio[censiti]),
    servizio_stimato = conta_servizi(servizio[!censiti]),
    stima_incerta = sum(!censiti & is.na(servizio)),
    giri = conta_servizi(data[["servizio_giro"]]),
    non_coerenti = sum(
      data[["coerenza_servizio"]] %in% stati_coerenza()[["non_coerente"]]
    ),
    fuori_comune = sum(incongruenze$fuori_comune),
    fuori_giro = sum(incongruenze$fuori_giro),
    coerenza_comune_ignota = sum(
      data[["coerenza_comune"]] %in% stati_coerenza_comune()[["ignota"]]
    ),
    rfid_cantieri = conta_cantieri(data[["cantiere"]]),
    letture_cantieri = letture_cantieri %||%
      stats::setNames(integer(0), character(0))
  )
}

#' Codici distinti da un testo, separati da virgola, punto e virgola o a capo
#' @noRd
analizza_input_ricerca <- function(testo) {
  if (is.null(testo) || length(testo) == 0 || is.na(testo)) {
    return(character(0))
  }
  codici <- stringr::str_trim(stringr::str_split(testo, "[,;\\n\\r]+")[[1]])
  unique(codici[codici != ""])
}

#' Confronto tra codici senza badare alle maiuscole
#' @noRd
codice_in <- function(valori, codici) {
  codici <- stringr::str_to_upper(codici)
  per_valori_distinti(valori, function(distinti) {
    !is.na(distinti) & stringr::str_to_upper(distinti) %in% codici
  })
}

#' Riquadro delle letture di un RFID: `lat_min`, `lat_max`, `lng_min`, `lng_max`
#' @noRd
calcola_bbox <- function(data) {
  list(
    lat_min = min(data$latitudine, na.rm = TRUE),
    lat_max = max(data$latitudine, na.rm = TRUE),
    lng_min = min(data$longitudine, na.rm = TRUE),
    lng_max = max(data$longitudine, na.rm = TRUE)
  )
}

#' Cambi di valore di un campo nelle letture di un RFID
#'
#' Una riga per cambio, con la prima lettura del nuovo valore.
#' @return Tibble con `giorno_lettura`, `valore`, `attuale`.
#' @noRd
cronologia_transizioni <- function(letture, campo) {
  x <- letture |>
    dplyr::filter(!is.na(.data[[campo]])) |>
    dplyr::arrange(.data$giorno_lettura)
  valori <- x[[campo]]
  cambio <- c(TRUE, valori[-1] != valori[-length(valori)])[seq_along(valori)]
  out <- dplyr::tibble(
    giorno_lettura = x$giorno_lettura[cambio],
    valore = valori[cambio]
  )
  dplyr::mutate(out, attuale = dplyr::row_number() == dplyr::n())
}

#' Stima del servizio di un non censito, dalle sue letture "Non Presente"
#' @return Tibble con `servizio`, `n`, `pct`, dal più frequente.
#' @noRd
stima_servizio_atteso <- function(letture) {
  letture |>
    dplyr::filter(
      .data$presente_a_database == "Non Presente",
      !is.na(.data$servizio_atteso)
    ) |>
    dplyr::count(servizio = .data$servizio_atteso, name = "n") |>
    dplyr::mutate(pct = 100 * .data$n / sum(.data$n)) |>
    dplyr::arrange(dplyr::desc(.data$n), .data$servizio)
}

#' Storia di un RFID per il dettaglio del bidone
#'
#' Casi: `censito_stabile`, `cambio_servizio`, `non_censito_con_stima`,
#' `non_censito_senza_stima`; lo stato è quello dell'ultima lettura.
#' @noRd
analizza_rfid <- function(letture) {
  letture <- dplyr::arrange(letture, .data$giorno_lettura)
  ultima <- letture[nrow(letture), ]
  censito <- identical(ultima$presente_a_database, "Presente")

  # Cambi di servizio a database: il servizio atteso cambia con il mezzo.
  servizi <- cronologia_transizioni(
    dplyr::tibble(
      giorno_lettura = letture$giorno_lettura,
      servizio = letture[["servizio_transponder"]] %||%
        rep(NA_character_, nrow(letture))
    ),
    "servizio"
  )
  utenze <- cronologia_transizioni(letture, "id_utenza")
  stima <- stima_servizio_atteso(letture)

  caso <- if (censito) {
    if (nrow(servizi) > 1) "cambio_servizio" else "censito_stabile"
  } else {
    if (nrow(stima) > 0) "non_censito_con_stima" else "non_censito_senza_stima"
  }

  list(
    rfid = ultima$RFID,
    caso = caso,
    censito = censito,
    sacchetto = rfid_sacchetto(ultima$RFID),
    n_letture = nrow(letture),
    prima_lettura = min(letture$giorno_lettura),
    ultima_lettura = ultima$giorno_lettura,
    servizio_attuale = if (nrow(servizi) > 0) {
      servizi$valore[nrow(servizi)]
    } else {
      NA_character_
    },
    cronologia_servizio = servizi,
    stima = stima,
    cambio_utenza = nrow(utenze) > 1,
    cronologia_utenza = utenze,
    coerenza_comune = coerenza_comune_rfid(letture),
    coerenza = coerenza_rfid(letture)
  )
}

#' Letture di un RFID fuori comune e fuori giro
#'
#' @return Lista con `fuori_comune` e `fuori_giro`: `n`, `dichiarati` e
#'   `letti` dal più frequente; `fuori_giro` anche le ultime cinque letture.
#' @noRd
coerenza_comune_rfid <- function(letture) {
  incongruenze <- incongruenze_comune(letture)
  per_frequenza <- function(valori) {
    valori <- as.character(valori)
    valori[is.na(valori)] <- "comune sconosciuto"
    names(sort(table(valori), decreasing = TRUE))
  }
  riepilogo <- function(quali, colonna) {
    esito <- list(n = sum(quali))
    if (esito$n > 0) {
      esito$dichiarati <- per_frequenza(letture[[colonna]][quali])
      esito$letti <- per_frequenza(letture[["comune_lettura"]][quali])
    }
    esito
  }
  fuori_giro <- riepilogo(incongruenze$fuori_giro, "comune_giro")
  if (fuori_giro$n > 0) {
    ultime <- utils::tail(letture[incongruenze$fuori_giro, , drop = FALSE], 5)
    fuori_giro$letture <- ultime[
      rev(seq_len(nrow(ultime))),
      intersect(
        c(
          "giorno_lettura",
          "matricola_veicolo",
          "comune_giro",
          "comune_lettura"
        ),
        names(ultime)
      ),
      drop = FALSE
    ]
  }
  list(
    fuori_comune = riepilogo(incongruenze$fuori_comune, "comune_fornitura"),
    fuori_giro = fuori_giro
  )
}

#' Letture di un RFID con il servizio non coerente
#'
#' @return Lista con `n` e le cinque `letture` più recenti.
#' @noRd
coerenza_rfid <- function(letture) {
  non_coerenti <- letture[["coerenza_servizio"]] %in%
    stati_coerenza()[["non_coerente"]]
  esito <- list(n = sum(non_coerenti))
  if (esito$n > 0) {
    ultime <- utils::tail(letture[non_coerenti, , drop = FALSE], 5)
    esito$letture <- ultime[
      rev(seq_len(nrow(ultime))),
      intersect(
        c(
          "giorno_lettura",
          "matricola_veicolo",
          "servizio_transponder",
          "servizio_atteso",
          "servizio_giro"
        ),
        names(ultime)
      ),
      drop = FALSE
    ]
  }
  esito
}
