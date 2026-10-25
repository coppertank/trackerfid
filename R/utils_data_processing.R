# Schema del file, servizi, coerenze, statistiche e storia di un RFID.

#' Campi obbligatori del CSV delle letture delle antenne
#'
#' Contesto: l'app carica il CSV prodotto dalla pipeline (`letture_antenne.csv`).
#' Senza questi campi non può funzionare e `valida_archivio()` rifiuta il file.
#' Risultato: i nomi dei nove campi obbligatori (istante, mezzo, RFID, stato a
#' database, utenza, posizione e servizio atteso).
#' @noRd
colonne_obbligatorie <- function() {
  c(
    "data_ora_lettura",
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
#'
#' Contesto: i campi che arricchiscono le schede (servizio del database,
#' comuni, cantiere, conferma del giro, distanza dal comune dichiarato...). Se
#' mancano, le funzioni che li usano si adattano.
#' Risultato: i nomi dei campi facoltativi.
#' @noRd
colonne_facoltative <- function() {
  c(colonne_quantita(), colonne_geografiche())
}

#' Colonna del servizio del contenitore a database, vuota per non censiti e sacchetti
#' Contesto: `servizio_transponder` è il servizio del bidone secondo il database
#' (secco, carta...), diverso dal servizio atteso, che viene dal giro.
#' @noRd
colonne_contenitore <- function() "servizio_transponder"

#' Nomi alternativi delle colonne del file
#'
#' Contesto: i nomi delle colonne sono cambiati nel tempo (e la pipeline
#' usa nomi suoi). I file vecchi devono poter ancora essere caricati.
#' Funzionamento: associa il nome vecchio al nome attuale: `volume` e
#' `numero_raccolte_annue` della pipeline diventano `volume_previsto` e
#' `numero_raccolte_annue_previste`; `comune_da_database` e `comune`
#' diventano `comune_fornitura`; `giorno_lettura` e `giorno` diventano
#' `data_ora_lettura` e `data_lettura`. Valgono solo se manca la colonna con il
#' nome attuale.
#' Risultato: un vettore con nome: nome del file, valore nome dell'app.
#' @return Vettore con nome: nome del file -> nome dell'app.
#' @noRd
alias_colonne <- function() {
  c(
    volume = "volume_previsto",
    numero_raccolte_annue = "numero_raccolte_annue_previste",
    comune_da_database = "comune_fornitura",
    comune = "comune_fornitura",
    giorno_lettura = "data_ora_lettura",
    giorno = "data_lettura"
  )
}

#' Colonna del giro con il nome del calendario (`SECCO PAP`); se manca vale `servizio_atteso`
#' Contesto: è il nome del giro senza conversioni: serve a tenere separato il
#' secco porta a porta dagli altri giri del secco.
#' @noRd
colonne_calendario <- function() "servizio_giro"

#' Colonna della conferma del giro: `SI`, `NO` (non ancora) o vuota
#' Contesto: l'azienda conferma i giri dopo averli eseguiti. La mostra la tabella
#' dei giri; nessun conto la usa. La pipeline segnala i giri `NO` dopo una
#' settimana (`controllo_giri()`).
#' @noRd
colonna_conferma <- function() "eseguito"

#' Colonna dei metri dal comune dichiarato, scritta dalla pipeline
#'
#' Contesto: la pipeline calcola i confini; l'app non li usa per decidere. Per
#' le letture in un comune diverso da quello dichiarato scrive la distanza dal
#' confine, e `coerenza_comune()` la confronta con la tolleranza.
#' Zero dentro il confine.
#' @noRd
colonna_distanza <- function() "distanza_comune_dichiarato"

#' Metri oltre il confine del comune dichiarato entro cui una lettura è coerente
#'
#' Contesto: una lettura a pochi metri dal confine del suo comune non è un
#' bidone spostato: la portata dell'antenna più l'errore del GPS giustificano la
#' differenza. Soglia: 100 metri. Se cambia, vanno riscritti il CSV, il
#' `.duckdb` e il dataset di esempio.
#' @noRd
tolleranza_confine_m <- function() 100

#' Campi facoltativi numerici del CSV
#' Contesto: volume e raccolte annue previste, e distanza dal comune
#' dichiarato; si convertono in numeri in validazione e un valore non
#' numerico ferma il caricamento indicando le righe.
#' @noRd
colonne_quantita <- function() {
  c("volume_previsto", "numero_raccolte_annue_previste")
}

#' Campi geografici del CSV: comune della fornitura, del giro, di lettura e cantiere
#' Contesto: l'app non ricava i comuni dalle coordinate: li legge dal file, già
#' corretti dalla pipeline (`correggi_comune_lettura()`).
#' @noRd
colonne_geografiche <- function() {
  c("comune_fornitura", "comune_giro", "comune_lettura", "cantiere")
}

#' Campi calcolati dalla validazione, anche se il file li contiene già
#' Contesto: le coerenze, il comune assegnato e il cantiere si ricalcolano
#' sempre, così l'app non dipende da come li ha scritti chi ha preparato il file.
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
#' Contesto: il CSV della pipeline ha 35 colonne; l'app ne usa 21. Le altre
#' servono all'analisi dei conferimenti (nucleo, categoria, potenziamento) e ai
#' report, che le leggono con `colonne = "tutte"`.
#' Funzionamento: associa a ogni colonna il tipo (`VARCHAR`, `DATE`, `INTEGER`,
#' `BOOLEAN`). Le mancanti restano vuote senza avvisi; quelle non convertibili
#' restano vuote con un avviso.
#' Risultato: un vettore con nome: il tipo SQL.
#' @return Vettore con nome: `VARCHAR`, `DATE`, `INTEGER` o `BOOLEAN`.
#' @noRd
tipi_colonne_preparazione <- function() {
  c(
    data_lettura = "DATE",
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
#' Contesto: sono quelle che servono a mappa, filtri, popup e analisi dei
#' cluster; le altre restano nel `.duckdb`. È anche lo schema della tabella
#' `letture` dell'archivio (`versione_archivio()` va aumentata se cambia).
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
#' Contesto: `Presente` = censito (il bidone ha una fornitura), `Non Presente` =
#' non censito.
#' @noRd
stati_database <- function() c("Presente", "Non Presente")

#' Valori di `coerenza_comune`, con i nomi usati dai filtri
#' Risultato: `Coerente`, `Fuori comune`, `Fuori giro`, `Coerenza ignota`.
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
#' Contesto: dice se un bidone è stato letto dove ci si aspettava. Per un censito
#' si confronta con il comune del database; per un non censito, che non ha un
#' comune dichiarato, con il comune del giro in cui è stato letto (un mezzo
#' fuori calendario dà `Fuori giro`).
#' Funzionamento: i nomi si confrontano con `chiave_comune()`. Coerente se
#' coincidono, o se la lettura è entro `tolleranza_confine_m()` dal dichiarato.
#' `Fuori comune` per un censito diverso dal suo comune, `Fuori giro` per un non
#' censito diverso dal giro. `Coerenza ignota` se manca uno dei due comuni, o se
#' il giro di un non censito è una zona fuori tabella. Un censito non è mai fuori
#' giro. Stessa regola in SQL in `valida_archivio()`.
#' Risultato: un valore di `stati_coerenza_comune()` per lettura.
#' @param data Letture; una colonna che manca vale come vuota.
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
#' Contesto: statistiche, popup e dettaglio del bidone hanno bisogno dei
#' vettori logici; la colonna è calcolata una volta.
#' Risultato: lista di vettori logici `fuori_comune` e `fuori_giro`. Una lettura
#' non è mai entrambi.
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
#' Risultato: «Coerente» e «Non coerente».
#' @noRd
etichette_coerenza <- function() {
  c(coerente = "Coerente", non_coerente = "Non coerente")
}

#' Coerenza tra servizio a database e servizio atteso dal giro
#'
#' Contesto: un bidone del secco letto in un giro della carta è un'anomalia
#' (bidone spostato, mezzo fuori calendario, tag sul mezzo). È il confronto che
#' fa emergere i casi da guardare.
#' Funzionamento: `SI` se i due servizi sono della stessa tipologia
#' (`tipologie_servizio()`), `NO` altrimenti; mancante se manca uno dei due (ogni
#' non censito, che non ha un servizio a database). Un nome sconosciuto fa
#' tipologia a sé. Stessa regola in SQL in `valida_archivio()`.
#' Risultato: `SI`, `NO` o `NA` per lettura.
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
#' Contesto: `SACCHETTI` non è un valore del file: lo assegna l'app a icone,
#' filtri e statistiche. Nei dati reali la pipeline scarta i sacchetti.
#' @noRd
servizio_sacchetti <- function() "SACCHETTI"

#' Il codice è di un sacchetto? 24 caratteri con il prefisso `00BD`
#' Contesto: i bidoni hanno codici di 10 caratteri. Un sacchetto è monouso (una
#' lettura sola) e non ha un servizio. Stessa regola in SQL in `sql_sacchetto()`.
#' Risultato: un vettore logico; un codice mancante non è un sacchetto.
#' @noRd
rfid_sacchetto <- function(rfid) {
  rfid <- as.character(rfid)
  esito <- nchar(rfid) %in% 24L
  lunghi <- which(esito)
  esito[lunghi] <- toupper(substr(rfid[lunghi], 1, 4)) == "00BD"
  esito
}

#' Toglie le letture dei sacchetti, per le analisi sui contenitori
#' Contesto: cluster e conferimenti riguardano i bidoni: un sacchetto è letto
#' una volta sola e non ha un servizio.
#' Risultato: `data` senza i sacchetti, senza copiarlo se non ce ne sono.
#' @noRd
senza_sacchetti <- function(data) {
  sacchetto <- rfid_sacchetto(data$RFID)
  if (!any(sacchetto)) {
    return(data)
  }
  data[!sacchetto, , drop = FALSE]
}

#' Percorso del dataset di esempio incluso nel pacchetto
#' Contesto: `inst/extdata/sample_rfid_dataset.csv`, simulato (non contiene dati
#' aziendali), serve a `run_app(demo = TRUE)`, ai test e alla documentazione.
#' @noRd
percorso_dataset_esempio <- function() {
  app_sys("extdata", "sample_rfid_dataset.csv")
}

#' Segnala un errore di validazione con uno o più messaggi espliciti
#' Contesto: i problemi di un file caricato devono arrivare all'utente come
#' elenco leggibile, non come un errore tecnico.
#' Risultato: ferma l'esecuzione con una condizione di classe propria, che
#' `mod_caricamento` mostra come avviso.
#' @noRd
errore_validazione <- function(messaggi) {
  rlang::abort(
    paste(messaggi, collapse = "\n"),
    class = "errore_validazione",
    messaggi = messaggi
  )
}

#' Formati di file accettati: estensione e descrizione
#' Risultato: CSV, CSV gzip, Parquet e il `.duckdb` della pipeline.
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
#' Risultato: un testo con i numeri delle prime `massimo` righe, e «...» se ce
#' ne sono altre.
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
#' Contesto: con `run_app(letture = "percorso")` ogni utente apre lo stesso file;
#' rileggerlo e rivalidarlo per ciascuno spreca tempo e memoria.
#' Funzionamento: tiene l'archivio in memoria; se il file cambia sul disco lo
#' rilegge.
#' @param lettore Di norma `carica_archivio()`.
#' @param path Percorso del file.
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
#' Contesto: ordine unico per filtri, legenda e statistiche.
#' @noRd
ordina_servizi <- function(servizi) {
  servizi <- as.character(unique(servizi[!is.na(servizi)]))
  noti <- servizi_config()$servizio
  as.character(c(intersect(noti, servizi), sort(setdiff(servizi, noti))))
}

#' Letture di un periodo, estremi inclusi; senza copia se c'è tutto
#' Contesto: il periodo di analisi sceglie le letture da mostrare.
#' @param data Letture con `data_ora_lettura`.
#' @param periodo Due date, o `NULL` (tutto).
#' @noRd
filtra_periodo <- function(data, periodo = NULL) {
  if (is.null(periodo)) {
    return(data)
  }
  periodo <- lubridate::as_date(periodo)
  inizio <- as.POSIXct(format(periodo[1]), tz = "UTC")
  fine <- as.POSIXct(format(periodo[2] + 1), tz = "UTC")
  nel_periodo <- data$data_ora_lettura >= inizio & data$data_ora_lettura < fine
  if (all(nel_periodo)) {
    return(data)
  }
  data[nel_periodo, , drop = FALSE]
}

#' Periodo di un anno solare
#' Risultato: dal 1 gennaio al 31 dicembre, come due date.
#' @noRd
periodo_anno <- function(anno) {
  c(lubridate::make_date(anno, 1, 1), lubridate::make_date(anno, 12, 31))
}

#' Descrizione breve di un periodo: "Anno 2025" oppure le due date
#' Contesto: compare nel titolo della dashboard.
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
#' Contesto: un bidone non censito non ha un servizio a database; lo si stima
#' da dove è stato letto. È la regola di riferimento: l'app usa la versione SQL
#' (`tabella_stima()`), questa serve ai test.
#' Funzionamento: sulle letture `Non Presente` di ogni RFID, se la tipologia più
#' frequente del servizio atteso raggiunge `soglia` (80%) il servizio è il giro più
#' letto di quella tipologia; altrimenti `NA` (stima incerta, punto di domanda).
#' Risultato: `RFID`, `quota` e `servizio_prevalente`, solo per gli RFID con
#' almeno una stima.
#' @return Tibble con `RFID`, `quota`, `servizio_prevalente`.
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
#' Contesto: icona, voce dei filtri e statistiche devono dire sempre lo stesso
#' servizio per un bidone. `servizio_icona` è quel servizio.
#' Funzionamento: censito: `servizio_transponder`; non censito: il servizio
#' stimato dell'RFID (`NA` sotto soglia, punto di domanda); sacchetto:
#' `SACCHETTI`.
#' Risultato: le letture con la colonna `servizio_icona`.
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
#' Contesto: la colonna `servizio_icona`; se manca la calcola sulle letture
#' ricevute, che per una riga per RFID dà una stima diversa: va aggiunta prima.
#' @noRd
servizio_mostrato <- function(data) {
  if (is.null(data[["servizio_icona"]])) {
    data <- aggiungi_servizio_icona(data)
  }
  data$servizio_icona
}

#' Voce che nei filtri e nelle statistiche indica le letture senza cantiere
#' Risultato: «Non assegnato».
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
#' Contesto: le statistiche della barra laterale mostrano quanti bidoni per servizio.
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
#' Contesto: i riquadri a sinistra riassumono i bidoni visibili sulla mappa:
#' quanti, quanti censiti, per servizio, con stima incerta, per cantiere, non
#' coerenti, fuori comune e fuori giro. Vanno calcolate su una riga per RFID,
#' con `servizio_icona` già calcolato sul periodo intero.
#' Risultato: una lista: totale, per stato, `servizio_censiti`,
#' `servizio_stimato`, `stima_incerta`, `giri`, `non_coerenti`, `rfid_cantieri`,
#' `letture_cantieri` e le incongruenze del comune.
#' @param data Una riga per RFID, con `servizio_icona`.
#' @param letture_cantieri Letture per cantiere, da `conteggi_cantieri()`.
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
#' Contesto: i campi di ricerca (RFID, utenza) accettano elenchi incollati.
#' Risultato: i codici puliti e senza doppioni.
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
#' Contesto: serve a inquadrare la mappa sugli spostamenti di un bidone.
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
#' Contesto: il dettaglio del bidone mostra se il servizio o l'utenza sono
#' cambiati nel tempo.
#' Risultato: una riga per cambio, con la prima lettura del nuovo valore
#' (`data_ora_lettura`, `valore`, `attuale`).
#' @return Tibble con `data_ora_lettura`, `valore`, `attuale`.
#' @noRd
cronologia_transizioni <- function(letture, campo) {
  x <- letture |>
    dplyr::filter(!is.na(.data[[campo]])) |>
    dplyr::arrange(.data$data_ora_lettura)
  valori <- x[[campo]]
  cambio <- c(TRUE, valori[-1] != valori[-length(valori)])[seq_along(valori)]
  out <- dplyr::tibble(
    data_ora_lettura = x$data_ora_lettura[cambio],
    valore = valori[cambio]
  )
  dplyr::mutate(out, attuale = dplyr::row_number() == dplyr::n())
}

#' Stima del servizio di un non censito, dalle sue letture "Non Presente"
#' Contesto: un non censito non ha un servizio a database; si guarda in quali
#' giri è stato letto.
#' Risultato: `servizio`, `n` e `pct`, dal più frequente.
#' @return Tibble con `servizio`, `n`, `pct`.
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
#' Contesto: il pannello del bidone dice che cosa gli è successo: servizio
#' stabile o cambiato, stima del servizio di un non censito, cambio di utenza,
#' fuori comune e fuori giro, letture non coerenti.
#' Funzionamento: classifica il bidone in uno di quattro casi:
#' `censito_stabile`, `cambio_servizio`, `non_censito_con_stima`,
#' `non_censito_senza_stima`; lo stato è quello dell'ultima lettura.
#' Risultato: una lista con il caso e i dati dei blocchi del pannello.
#' @param letture Tutte le letture del periodo di un RFID.
#' @noRd
analizza_rfid <- function(letture) {
  letture <- dplyr::arrange(letture, .data$data_ora_lettura)
  ultima <- letture[nrow(letture), ]
  censito <- identical(ultima$presente_a_database, "Presente")

  # Cambi di servizio a database: il servizio atteso cambia con il mezzo.
  servizi <- cronologia_transizioni(
    dplyr::tibble(
      data_ora_lettura = letture$data_ora_lettura,
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
    prima_lettura = min(letture$data_ora_lettura),
    ultima_lettura = ultima$data_ora_lettura,
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
#' Contesto: parte del dettaglio del bidone.
#' Risultato: una lista con `fuori_comune` e `fuori_giro`: `n`, `dichiarati` e
#' `letti` dal più frequente; `fuori_giro` anche le ultime cinque letture.
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
          "data_ora_lettura",
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
#' Contesto: parte del dettaglio del bidone.
#' Risultato: una lista con `n` e le cinque `letture` più recenti.
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
          "data_ora_lettura",
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
