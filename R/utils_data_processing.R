# Funzioni di utilità per lettura, validazione ed elaborazione delle letture RFID.

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

#' Campi facoltativi del CSV
#'
#' Le due quantità servono all'analisi dei cluster. I campi geografici
#' servono a filtri, popup e ricerca dei giri.
#' @noRd
colonne_facoltative <- function() {
  c(colonne_quantita(), colonne_geografiche())
}

#' Campo facoltativo del CSV con il servizio del contenitore a database
#'
#' `servizio_transponder` è il servizio del contenitore secondo il database,
#' vuoto per i non censiti e per i sacchetti. `servizio_atteso` è invece il
#' servizio che il giro del mezzo lasciava aspettare, con il nome del
#' database, vedi `associa_servizio_atteso_da_calendario()`. Il confronto tra
#' i due è la coerenza, vedi `coerenza_servizio()`.
#' @noRd
colonne_contenitore <- function() "servizio_transponder"

#' Nomi alternativi delle colonne del file
#'
#' La pipeline di preparazione dei dati scrive `volume` e
#' `numero_raccolte_annue`, come l'anagrafica dei contenitori; l'app li chiama
#' come la tabella dell'analisi dei cluster. Il comune della fornitura si
#' chiamava `comune_da_database` nei file preparati fino al 17 ottobre 2026, e
#' `comune` in quelli ancora più vecchi: valgono come `comune_fornitura`. Un
#' nome alternativo vale solo se il file non ha già la colonna con il nome
#' dell'app.
#'
#' @return Vettore con nome: i nomi sono quelli del file, i valori quelli
#'   dell'app.
#' @noRd
alias_colonne <- function() {
  c(
    volume = "volume_previsto",
    numero_raccolte_annue = "numero_raccolte_annue_previste",
    comune_da_database = "comune_fornitura",
    comune = "comune_fornitura"
  )
}

#' Campo facoltativo del CSV con il servizio del giro secondo il calendario
#'
#' `servizio_giro` è il giro che il calendario prevedeva, con il nome del
#' calendario: `SECCO PAP`, non `SECCO`. Serve al filtro sul giro, alla
#' coerenza dei servizi e alla ricerca dei giri. In un file che non lo riporta
#' vale `servizio_atteso`.
#' @noRd
colonne_calendario <- function() "servizio_giro"

#' Conferma del giro
#'
#' `eseguito` dice se il giro del calendario è stato confermato: `SI`, `NO`
#' se non ancora, e a volte manca. `NO` non vuol dire che il giro non è stato
#' fatto: di solito diventa `SI` nei giorni seguenti. La tabella dei giri la
#' mostra; nessun conto la usa, perché una lettura dice già che il mezzo è
#' passato. Facoltativa, senza avviso se manca.
#' @noRd
colonna_conferma <- function() "eseguito"

#' Distanza della lettura dal comune dichiarato
#'
#' `distanza_comune_dichiarato` la scrive la preparazione dei dati, vedi
#' `distanza_comune_dichiarato()`: in metri, zero se il punto cade dentro il
#' comune dichiarato, solo per le letture fatte in un altro comune. Una
#' lettura appena oltre il confine, entro `tolleranza_confine_m()`, è
#' coerente: vedi `coerenza_comune()`. Facoltativa, senza avviso se manca: un
#' file senza la colonna confronta i soli nomi.
#' @noRd
colonna_distanza <- function() "distanza_comune_dichiarato"

#' Distanza dal confine entro cui una lettura in un altro comune è coerente
#'
#' In metri. Il mezzo legge un bidone dall'altra parte della strada, il GPS
#' sbaglia di qualche decina di metri e il sistema di localizzazione può usare
#' confini un po' diversi da quelli della Regione: vicino al confine il comune
#' di lettura è spesso quello accanto. Un bidone davvero spostato di solito è
#' molto più lontano.
#' @noRd
tolleranza_confine_m <- function() 100

#' Campi facoltativi numerici del CSV
#' @noRd
colonne_quantita <- function() {
  c("volume_previsto", "numero_raccolte_annue_previste")
}

#' Campi facoltativi geografici del CSV
#'
#' `comune_fornitura` è il comune della fornitura a cui è legato il
#' contenitore, secondo il database: vuoto per i non censiti.
#' `comune_giro` è il comune in cui il calendario mandava il mezzo che ha
#' fatto la lettura. `comune_lettura` è il comune in cui è stata fatta la
#' lettura, come lo dà il sistema di localizzazione dei mezzi. `cantiere` è il
#' cantiere del comune assegnato, vedi `colonne_derivate()`.
#' @noRd
colonne_geografiche <- function() {
  c("comune_fornitura", "comune_giro", "comune_lettura", "cantiere")
}

#' Campi calcolati dalla validazione
#'
#' Li compila `aggiungi_geografia()`, dal comune di lettura e dai comuni
#' dichiarati. `comune_geografico` è il comune di lettura, mancante se non è
#' tra i comuni serviti: lo usano i conti. `comune_assegnato` è il comune
#' geografico oppure, fuori dai comuni serviti, quello del database: è il
#' comune usato da filtri, bolle e statistiche. `coerenza_comune` confronta il
#' comune di lettura con quello dichiarato, vedi `coerenza_comune()`.
#'
#' `coerenza_servizio` la compila `coerenza_servizio()`, dai servizi della
#' lettura. Le due coerenze vengono ricalcolate anche se il file le contiene
#' già.
#' @noRd
colonne_derivate <- function() {
  c(
    "comune_assegnato",
    "comune_geografico",
    "coerenza_comune",
    "coerenza_servizio"
  )
}

#' Colonne del file della pipeline che l'app non usa ancora, con il tipo
#'
#' Giro, contenitore, fornitura, nucleo, categoria e potenziamento del secco.
#' La validazione le tiene, convertite nel tipo SQL indicato: il dataset è
#' uno solo, con tutte le colonne, e le usa l'analisi dei conferimenti. Un
#' file che non le ha le lascia vuote, senza avvisi; un valore che non si
#' converte resta vuoto, con un avviso.
#'
#' @return Vettore con nome: per ogni colonna `VARCHAR`, `DATE`, `INTEGER` o
#'   `BOOLEAN`.
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

#' Colonne del dataset validato che l'app usa
#'
#' Sono quelle che filtri, mappe, ricerche, dettaglio e analisi dei cluster
#' portano in R: le altre colonne del dataset restano nell'archivio.
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

#' Tutte le colonne del dataset validato, nell'ordine in cui compaiono
#'
#' Quelle che l'app usa, poi quelle della pipeline che non usa ancora.
#' @noRd
colonne_dataset <- function() {
  c(colonne_app(), colonne_preparazione())
}

#' Valori ammessi per `presente_a_database`
#' @noRd
stati_database <- function() c("Presente", "Non Presente")

#' Valori della colonna `coerenza_comune`
#'
#' Sono anche le voci del filtro sulla coerenza del comune. La coerenza dei
#' servizi è un'altra cosa, vedi `stati_coerenza()`.
#'
#' @return Vettore con i valori: i nomi sono quelli usati dai filtri.
#' @noRd
stati_coerenza_comune <- function() {
  c(
    coerente = "Coerente",
    fuori_comune = "Fuori comune",
    fuori_giro = "Fuori giro",
    ignota = "Coerenza ignota"
  )
}

#' Coerenza tra il comune di lettura e il comune dichiarato
#'
#' Il comune di lettura è quello esatto in cui è stata fatta la lettura, e il
#' più aggiornato. Lo si confronta con il comune dichiarato:
#'
#' - per un censito il comune del database: se differisce, la lettura è
#'   `Fuori comune`, di solito un bidone spostato;
#' - per un non censito il comune del giro: se differisce, la lettura è
#'   `Fuori giro`, di solito un mezzo che non faceva il giro del calendario.
#'
#' Se i due comuni coincidono la lettura è `Coerente`. È coerente anche una
#' lettura fatta in un altro comune ma entro `tolleranza_confine_m()` metri
#' dal comune dichiarato, secondo `distanza_comune_dichiarato`: vicino al
#' confine il sistema di localizzazione dà spesso il comune accanto. Se manca
#' il comune dichiarato, o il comune di lettura, la coerenza è `Coerenza
#' ignota`: ad esempio per un sacchetto censito, che a database non ha un
#' comune. È ignota anche quella di un non censito il cui comune del giro non
#' è un comune servito, come una zona del calendario: non c'è un confine con
#' cui confrontare il comune di lettura. I nomi si confrontano senza badare a
#' maiuscole, accenti e apostrofi, vedi `chiave_comune()`.
#'
#' La usano la preparazione dei dati e la validazione dell'app: il CSV
#' riporta gli stessi valori che l'app ricalcola al caricamento.
#'
#' @param data Letture con `presente_a_database`, `comune_lettura`,
#'   `comune_fornitura`, `comune_giro` e, se c'è,
#'   `distanza_comune_dichiarato`. Una colonna che manca vale come vuota.
#' @return Vettore di testo, un valore di `stati_coerenza_comune()` per
#'   lettura.
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
  # Un giro in una zona, non in un comune servito: niente confronto.
  giro_fuori_tabella <- !censito & !is.na(giro) &
    is.na(indice_comune(as.character(data[["comune_giro"]] %||% rep(NA, n))))
  stati <- stati_coerenza_comune()
  esito <- rep(stati[["ignota"]], n)
  noto <- !is.na(lettura) & !is.na(dichiarato) & !giro_fuori_tabella
  uguale <- noto & (lettura == dichiarato |
    (!is.na(distanza) & distanza <= tolleranza_confine_m()))
  esito[uguale] <- stati[["coerente"]]
  esito[noto & !uguale & censito] <- stati[["fuori_comune"]]
  esito[noto & !uguale & !censito] <- stati[["fuori_giro"]]
  unname(esito)
}

#' Letture fuori comune e fuori giro
#'
#' @param data Letture. Senza la colonna `coerenza_comune` nessuna lettura è
#'   fuori comune o fuori giro.
#' @return Lista con i vettori logici `fuori_comune` e `fuori_giro`, un valore
#'   per lettura.
#' @noRd
incongruenze_comune <- function(data) {
  stati <- stati_coerenza_comune()
  valori <- data[["coerenza_comune"]] %||% rep(NA_character_, nrow(data))
  list(
    fuori_comune = valori %in% stati[["fuori_comune"]],
    fuori_giro = valori %in% stati[["fuori_giro"]]
  )
}

#' Valori della colonna `coerenza_servizio`
#'
#' Sono quelli che scrive la preparazione dei dati: `SI` e `NO`. Filtri,
#' popup e statistiche li mostrano con le parole di `etichette_coerenza()`.
#'
#' @return Vettore con i valori: i nomi sono quelli usati dai filtri.
#' @noRd
stati_coerenza <- function() {
  c(coerente = "SI", non_coerente = "NO")
}

#' Parole con cui l'app mostra i valori di `coerenza_servizio`
#'
#' @return Vettore con le etichette, con gli stessi nomi di `stati_coerenza()`.
#' @noRd
etichette_coerenza <- function() {
  c(coerente = "Coerente", non_coerente = "Non coerente")
}

#' Coerenza tra il servizio del contenitore e quello atteso dal giro
#'
#' Una lettura è coerente se il servizio del contenitore a database,
#' `servizio_transponder`, e il servizio atteso dal giro, `servizio_atteso`,
#' sono della stessa tipologia, vedi `tipologia_servizio()`. Un contenitore
#' della carta letto in un giro del secco non è coerente: è stato letto in
#' mezzo a contenitori di un altro rifiuto, oppure il mezzo non stava facendo
#' il giro del calendario.
#'
#' Un non censito non ha un servizio a database: la sua coerenza manca. Manca
#' anche se manca il servizio atteso, ad esempio perché il mezzo quel giorno
#' non aveva giri. Un nome che `tipologie_servizio()` non conosce fa tipologia
#' a sé: un censito letto in un giro che non raccoglie rifiuti, come
#' `PULIZIA TERRIT.`, non è coerente.
#'
#' La usano la preparazione dei dati e la validazione dell'app: il CSV riporta
#' gli stessi valori che l'app ricalcola al caricamento.
#'
#' @param data Letture con `servizio_transponder` e `servizio_atteso`. Una
#'   colonna che manca vale come vuota.
#' @return Vettore di testo, un valore per lettura: quelli di
#'   `stati_coerenza()`, oppure mancante.
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

#' Servizio assegnato ai sacchetti
#'
#' I sacchetti non hanno una tipologia di rifiuto a database. L'app li
#' riconosce dal codice, vedi `rfid_sacchetto()`, e dà loro questo servizio,
#' che in `tipologie_servizio()` ha una tipologia e un'icona sue.
#' @noRd
servizio_sacchetti <- function() "SACCHETTI"

#' Il codice RFID è quello di un sacchetto?
#'
#' I codici dei sacchetti hanno 24 caratteri e iniziano per `00BD`. Quelli dei
#' bidoni ne hanno 10.
#'
#' @param rfid Vettore di codici RFID.
#' @return Vettore logico. Un codice mancante non è un sacchetto.
#' @noRd
rfid_sacchetto <- function(rfid) {
  rfid <- as.character(rfid)
  esito <- nchar(rfid) %in% 24L
  # Il prefisso si guarda solo nei codici lunghi: quelli dei bidoni sono la
  # gran parte e si scartano già dalla lunghezza.
  lunghi <- which(esito)
  esito[lunghi] <- toupper(substr(rfid[lunghi], 1, 4)) == "00BD"
  esito
}

#' Letture dei soli contenitori, senza i sacchetti
#'
#' Un sacchetto è monouso: viene letto una volta sola, quando il mezzo lo
#' raccoglie. Si vede sulla mappa e nelle ricerche, ma non entra nell'analisi
#' dei cluster, che riguarda i contenitori.
#'
#' @param data Data frame con la colonna `RFID`.
#' @return Le righe che non sono di un sacchetto. Se non ce ne sono, `data`
#'   così com'è, senza copie.
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

#' Formati di file accettati per le letture
#'
#' Il file `.duckdb` è l'archivio scritto da `scrivi_archivio()`.
#'
#' @return Vettore con nome: l'estensione e la descrizione del formato.
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

#' Elenca le prime righe del file che presentano un problema
#' @noRd
elenco_righe <- function(indici, massimo = 5) {
  # +1: la prima riga del file è l'intestazione
  righe <- utils::head(indici, massimo) + 1
  testo <- paste(righe, collapse = ", ")
  if (length(indici) > massimo) paste0(testo, ", \u2026") else testo
}

# File letti da percorso: restano in memoria per tutte le sessioni del processo.
memoria_file <- new.env(parent = emptyenv())

#' Legge un file una sola volta per processo di R
#'
#' Serve ai file indicati all'avvio dell'app: tutte le sessioni usano lo
#' stesso dataset, senza rileggerlo e senza tenerne una copia a testa. Se il
#' file cambia sul disco viene riletto, e la versione precedente lascia la
#' memoria.
#'
#' @param lettore Funzione che legge e valida il file, di norma
#'   `carica_archivio()`.
#' @param path Percorso del file.
#' @return Il risultato di `lettore(path)`.
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

#' Ordina i servizi: prima quelli noti (ordine canonico), poi gli altri
#' @noRd
ordina_servizi <- function(servizi) {
  servizi <- as.character(unique(servizi[!is.na(servizi)]))
  noti <- servizi_config()$servizio
  as.character(c(intersect(noti, servizi), sort(setdiff(servizi, noti))))
}

#' Filtra le letture per periodo (estremi inclusi, a giornata intera)
#'
#' Se tutte le letture cadono nel periodo restituisce il dataset ricevuto,
#' senza copiarlo: con milioni di letture la copia costa tempo e memoria.
#'
#' @param data Dataframe con letture.
#' @param periodo Vettore di due date (inizio, fine) oppure `NULL`.
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

#' Periodo corrispondente a un anno solare (1 gennaio - 31 dicembre)
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

#' Servizio atteso prevalente dei contenitori non censiti
#'
#' Per ogni RFID considera le letture "Non Presente" con `servizio_atteso`
#' valorizzato. La quota si calcola per tipologia (i giri che condividono
#' l'icona, ad esempio "CARTA CONT.STRADALI" e "CARTA/CARTONE PAP", contano
#' insieme): se la tipologia più frequente raggiunge la soglia, il servizio
#' prevalente è il giro più letto di quella tipologia, altrimenti è `NA`.
#'
#' @param data Letture da analizzare.
#' @param soglia Quota minima (0-1) perché la stima sia considerata affidabile.
#' @return Tibble con `RFID`, `quota` della tipologia più frequente e
#'   `servizio_prevalente` (`NA` sotto soglia). Contiene solo gli RFID con
#'   almeno una stima, in ordine di RFID.
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

  # Per ogni RFID: la tipologia più letta e, al suo interno, il giro più letto.
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

#' Aggiunge alle letture il servizio da rappresentare con l'icona
#'
#' La colonna `servizio_icona` vale `servizio_transponder` per le letture
#' censite: è il servizio del contenitore a database. Per quelle non censite
#' vale il servizio stimato dell'RFID, cioè il servizio atteso prevalente
#' tra le sue letture, quello dei giri che lo hanno letto: manca se nessuna
#' tipologia raggiunge la soglia, e in quel caso il marker mostra il punto di
#' domanda. Un non censito sta così sotto una voce sola.
#'
#' Un sacchetto si riconosce dal codice e non ha una tipologia di rifiuto: ha
#' sempre il servizio dei sacchetti, censito o no.
#'
#' La stessa colonna dà la voce con cui una lettura compare nel filtro sul
#' servizio e nelle statistiche: icone, filtri e conteggi coincidono.
#'
#' @param letture Letture da completare.
#' @param riferimento Letture su cui calcolare il servizio prevalente.
#' @param soglia Quota minima del servizio atteso prevalente.
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
#' È la colonna `servizio_icona`, vedi `aggiungi_servizio_icona()`: per un
#' censito il servizio a database della lettura, per un non censito il servizio
#' stimato dell'RFID, per un sacchetto il servizio dei sacchetti. Se la colonna
#' manca viene calcolata sulle letture ricevute: chi passa solo una parte
#' delle letture, ad esempio una riga per RFID, deve quindi averla aggiunta
#' prima, su tutte le letture del periodo.
#'
#' @param data Letture.
#' @return Vettore con un servizio per lettura, mancante dove il marker mostra
#'   il punto di domanda.
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

#' Cantieri presenti in un insieme di letture, nell'ordine dei filtri
#'
#' Prima i cantieri della tabella dei comuni, poi gli altri in ordine
#' alfabetico. In fondo la voce delle letture senza cantiere, se ce ne sono.
#'
#' @param valori Colonna `cantiere` delle letture.
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

#' Conta le occorrenze di un servizio, in ordine canonico
#' @noRd
conta_servizi <- function(valori) {
  valori <- valori[!is.na(valori)]
  ordine <- ordina_servizi(valori)
  conteggi <- table(factor(valori, levels = ordine))
  stats::setNames(as.integer(conteggi), ordine)
}

#' Conta le occorrenze di ogni cantiere, nell'ordine dei filtri
#'
#' @param valori Colonna `cantiere`; i mancanti contano sotto la voce
#'   `senza_cantiere()`.
#' @return Vettore intero con nome. Vuoto se nessuna lettura ha un cantiere.
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

#' Statistiche del dataset filtrato (una riga per RFID)
#'
#' I servizi seguono gli stessi criteri dei filtri, vedi
#' `servizio_mostrato()`: per i censiti il servizio del contenitore a
#' database, per i non censiti il servizio stimato. I non censiti senza un
#' servizio stimato sono contati a parte.
#'
#' @param data Dataframe deduplicato per RFID, con la colonna
#'   `servizio_icona`: vedi `servizio_mostrato()`.
#' @param letture_cantieri Letture per cantiere tra tutte quelle che superano
#'   i filtri, vedi `conteggi_cantieri()`. Facoltative.
#' @return Lista con totale, conteggi per stato database, per servizio e per
#'   cantiere: `servizio_censiti` conta i censiti per servizio a database,
#'   `servizio_stimato` i non censiti con un servizio stimato,
#'   `stima_incerta` quelli senza;
#'   `giri` conta gli RFID per giro del calendario; `non_coerenti` quelli con
#'   i servizi che non concordano. `rfid_cantieri` conta gli RFID,
#'   `letture_cantieri` le letture.
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

#' Estrae i codici da un testo di ricerca
#'
#' @param testo Testo con codici separati da virgola, punto e virgola o a capo.
#' @return Vettore di codici univoci, senza spazi superflui.
#' @noRd
analizza_input_ricerca <- function(testo) {
  if (is.null(testo) || length(testo) == 0 || is.na(testo)) {
    return(character(0))
  }
  codici <- stringr::str_trim(stringr::str_split(testo, "[,;\\n\\r]+")[[1]])
  unique(codici[codici != ""])
}

#' Confronto tra codici che ignora maiuscole e minuscole
#'
#' Il confronto avviene sui valori distinti: un codice compare in molte
#' letture, e portarle tutte in maiuscolo costa di più.
#' @noRd
codice_in <- function(valori, codici) {
  codici <- stringr::str_to_upper(codici)
  per_valori_distinti(valori, function(distinti) {
    !is.na(distinti) & stringr::str_to_upper(distinti) %in% codici
  })
}

#' Calcola bounding box di tutte le letture di un RFID
#'
#' @param data Dataframe con letture di un solo RFID
#' @return List con lat_min, lat_max, lng_min, lng_max
#' @noRd
calcola_bbox <- function(data) {
  list(
    lat_min = min(data$latitudine, na.rm = TRUE),
    lat_max = max(data$latitudine, na.rm = TRUE),
    lng_min = min(data$longitudine, na.rm = TRUE),
    lng_max = max(data$longitudine, na.rm = TRUE)
  )
}

#' Cronologia delle transizioni di un campo per un singolo RFID
#'
#' Letture consecutive con lo stesso valore vengono accorpate: resta una riga
#' per ogni cambio, con la data della prima lettura in cui il valore compare.
#'
#' @param letture Letture di un solo RFID.
#' @param campo Nome del campo, ad esempio `"id_utenza"`.
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

#' Stima del servizio di un contenitore non censito
#'
#' Contano le sole letture "Non Presente", come per l'icona del marker: nei
#' dati reali il servizio atteso c'è anche sulle letture dei censiti.
#'
#' @param letture Letture di un solo RFID.
#' @return Tibble con `servizio`, `n`, `pct` (0-100), in ordine decrescente.
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

#' Analizza la storia di un RFID per il pannello di dettaglio
#'
#' Lo stato (censito o no) è quello dell'ultima lettura. I casi possibili sono:
#' `"censito_stabile"`, `"cambio_servizio"`, `"non_censito_con_stima"`,
#' `"non_censito_senza_stima"`.
#'
#' @param letture Tutte le letture di un solo RFID.
#' @return Lista con il caso rilevato e i dati per descriverlo.
#' @noRd
analizza_rfid <- function(letture) {
  letture <- dplyr::arrange(letture, .data$giorno_lettura)
  ultima <- letture[nrow(letture), ]
  censito <- identical(ultima$presente_a_database, "Presente")

  # I cambi di servizio sono quelli del database: il servizio atteso viene
  # dai giri, e cambia con il mezzo che legge il contenitore.
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

#' Letture di un RFID fuori dai comuni dichiarati
#'
#' Vedi `coerenza_comune()`: il comune di lettura e quello dichiarato non
#' coincidono.
#'
#' @param letture Letture dell'RFID, in ordine di tempo.
#' @return Lista con `fuori_comune` e `fuori_giro`. Ognuna ha `n`, il numero
#'   di letture, e, se ce ne sono, `dichiarati`, i comuni dichiarati di quelle
#'   letture, e `letti`, i comuni di lettura: tutti e due dal più frequente.
#'   `fuori_giro` ha anche `letture`, le ultime cinque, con data, mezzo,
#'   comune del giro e comune di lettura.
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

#' Letture di un RFID con i servizi che non concordano
#'
#' Vedi `coerenza_servizio()`: il servizio a database e quello atteso dal
#' giro non sono dello stesso rifiuto.
#'
#' @param letture Letture di un RFID, in ordine di tempo, con la colonna
#'   `coerenza_servizio`. Senza la colonna nessuna lettura risulta non
#'   coerente.
#' @return Lista con `n`, il numero delle letture non coerenti, e `letture`,
#'   le cinque più recenti, dalla più recente: giorno, mezzo, servizio a
#'   database, servizio atteso e giro.
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
