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
#' Il passo 3 della preparazione dei dati scrive `volume` e
#' `numero_raccolte_annue`, come l'anagrafica dei contenitori; l'app li chiama
#' come la tabella dell'analisi dei cluster. La colonna `comune` dei file meno
#' recenti vale come `comune_da_database`. Un nome alternativo vale solo se il
#' file non ha già la colonna con il nome dell'app.
#'
#' @return Vettore con nome: i nomi sono quelli del file, i valori quelli
#'   dell'app.
#' @noRd
alias_colonne <- function() {
  c(
    volume = "volume_previsto",
    numero_raccolte_annue = "numero_raccolte_annue_previste",
    comune = "comune_da_database"
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

#' Campi facoltativi numerici del CSV
#' @noRd
colonne_quantita <- function() {
  c("volume_previsto", "numero_raccolte_annue_previste")
}

#' Campi facoltativi geografici del CSV
#'
#' `comune_da_database` è il comune dell'anagrafica, vuoto per i non censiti.
#' `comune_giro` è il comune in cui il calendario mandava il mezzo che ha
#' fatto la lettura. `comune_lettura` è il comune in cui è stata fatta la
#' lettura, come lo dà il sistema di localizzazione dei mezzi. `cantiere` è il
#' cantiere del comune assegnato, vedi `colonne_derivate()`.
#' @noRd
colonne_geografiche <- function() {
  c("comune_da_database", "comune_giro", "comune_lettura", "cantiere")
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

#' Tutte le colonne del dataset validato, nell'ordine in cui compaiono
#' @noRd
colonne_dataset <- function() {
  c(
    colonne_obbligatorie(),
    colonne_contenitore(),
    colonne_facoltative(),
    colonne_calendario(),
    colonne_derivate()
  )
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
#' Se i due comuni coincidono la lettura è `Coerente`. Se manca il comune
#' dichiarato, o il comune di lettura, la coerenza è `Coerenza ignota`: ad
#' esempio per un sacchetto censito, che a database non ha un comune. I nomi
#' si confrontano senza badare a maiuscole, accenti e apostrofi, vedi
#' `chiave_comune()`.
#'
#' La usano la preparazione dei dati e la validazione dell'app: il CSV
#' riporta gli stessi valori che l'app ricalcola al caricamento.
#'
#' @param data Letture con `presente_a_database`, `comune_lettura`,
#'   `comune_da_database` e `comune_giro`. Una colonna che manca vale come
#'   vuota.
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
  dichiarato <- chiave("comune_giro")
  dichiarato[censito] <- chiave("comune_da_database")[censito]
  stati <- stati_coerenza_comune()
  esito <- rep(stati[["ignota"]], n)
  noto <- !is.na(lettura) & !is.na(dichiarato)
  uguale <- noto & lettura == dichiarato
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
#' dei cluster né nel confronto con le letture storiche, che riguardano i
#' contenitori.
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

#' Rileva il separatore del CSV (virgola o punto e virgola)
#'
#' @param path Percorso del file.
#' @return `","` oppure `";"`.
#' @noRd
rileva_separatore <- function(path) {
  intestazione <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(intestazione) == 0 || !nzchar(stringr::str_trim(intestazione))) {
    errore_validazione("Il file \u00e8 vuoto.")
  }
  n_virgole <- stringr::str_count(intestazione, stringr::fixed(","))
  n_punti_virgola <- stringr::str_count(intestazione, stringr::fixed(";"))
  if (n_punti_virgola > n_virgole) ";" else ","
}

#' Legge un CSV rilevando automaticamente il separatore
#'
#' Tutte le colonne vengono lette come testo: la conversione dei tipi è
#' demandata a `validate_dataset()`, così codici come RFID e utenze non
#' perdono eventuali zeri iniziali.
#'
#' @param path Percorso del file CSV.
#' @return Un data frame di sole colonne `character`.
#' @noRd
read_csv_auto <- function(path) {
  separatore <- rileva_separatore(path)
  tryCatch(
    data.table::fread(
      path,
      sep = separatore,
      header = TRUE,
      colClasses = "character",
      na.strings = c("NA", ""),
      encoding = "UTF-8",
      strip.white = TRUE,
      showProgress = FALSE,
      data.table = FALSE
    ),
    error = function(e) {
      errore_validazione(paste0(
        "Impossibile leggere il file come CSV: ",
        conditionMessage(e)
      ))
    }
  )
}

#' Formati di file accettati per le letture
#'
#' @return Vettore con nome: l'estensione e la descrizione del formato.
#' @noRd
formati_file <- function() {
  c(
    csv = "CSV",
    txt = "CSV",
    gz = "CSV compresso con gzip",
    parquet = "Parquet"
  )
}

#' Decomprime un file gzip in un file temporaneo
#'
#' Legge e scrive a blocchi, così la memoria usata non dipende dalla
#' dimensione del file.
#'
#' @param path Percorso del file compresso.
#' @return Percorso del file decompresso, da eliminare dopo l'uso.
#' @noRd
decomprimi_gz <- function(path) {
  destinazione <- tempfile(fileext = ".csv")
  ingresso <- gzfile(path, open = "rb")
  uscita <- file(destinazione, open = "wb")
  on.exit({
    close(ingresso)
    close(uscita)
  })
  repeat {
    blocco <- readBin(ingresso, what = "raw", n = 32 * 1024^2)
    if (length(blocco) == 0) {
      break
    }
    writeBin(blocco, uscita)
  }
  destinazione
}

#' Legge un file di letture nel formato indicato dalla sua estensione
#'
#' Accetta CSV, CSV compresso con gzip (`.csv.gz`) e Parquet. Il Parquet
#' arriva con le colonne già tipizzate: la validazione le accetta così.
#'
#' @param path Percorso del file.
#' @param nome Nome originale del file: dà l'estensione quando `path` è il
#'   percorso temporaneo di un file caricato.
#' @return Un data frame.
#' @noRd
leggi_tabella <- function(path, nome = basename(path)) {
  estensione <- tolower(tools::file_ext(nome))
  if (estensione == "gz") {
    decompresso <- tryCatch(
      decomprimi_gz(path),
      error = function(e) {
        errore_validazione(paste0(
          "Impossibile decomprimere il file: ",
          conditionMessage(e)
        ))
      }
    )
    on.exit(unlink(decompresso))
    return(read_csv_auto(decompresso))
  }
  if (estensione == "parquet") {
    if (!requireNamespace("nanoparquet", quietly = TRUE)) {
      errore_validazione(
        "Per leggere un file Parquet serve il pacchetto nanoparquet: install.packages(\"nanoparquet\")."
      )
    }
    return(tryCatch(
      as.data.frame(nanoparquet::read_parquet(path)),
      error = function(e) {
        errore_validazione(paste0(
          "Impossibile leggere il file come Parquet: ",
          conditionMessage(e)
        ))
      }
    ))
  }
  read_csv_auto(path)
}

#' Converte un vettore in data e ora (POSIXct, UTC)
#'
#' Il formato atteso è `YYYY-MM-DD HH:MM:SS`; sono accettate anche le varianti
#' senza secondi, con data italiana (`DD/MM/YYYY`) o con la sola data. Una
#' colonna già di tipo data conserva l'ora che mostra, senza conversioni di
#' fuso.
#' @noRd
converti_data_ora <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(lubridate::force_tz(x, "UTC"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(format(x), tz = "UTC"))
  }
  lubridate::parse_date_time(
    x,
    orders = c("Ymd HMS", "Ymd HM", "dmY HMS", "dmY HM", "Ymd", "dmY"),
    tz = "UTC",
    quiet = TRUE
  )
}

#' Converte un vettore in numerico (accetta la virgola decimale)
#' @noRd
converti_numero <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  # La sostituzione della virgola serve solo ai file che la usano.
  if (any(stringr::str_detect(x, stringr::fixed(",")), na.rm = TRUE)) {
    x <- stringr::str_replace(x, stringr::fixed(","), ".")
  }
  suppressWarnings(as.numeric(x))
}

#' Toglie gli spazi superflui e rende mancanti i testi vuoti
#'
#' Il risultato è quello di `stringr::str_squish()` su ogni cella, con i testi
#' vuoti resi mancanti. Su milioni di righe però pulire ogni cella costa
#' secondi, e quasi nessuna ne ha bisogno. Una colonna con pochi valori
#' ripetuti, come un servizio o un comune, viene pulita sui soli valori
#' distinti. In una colonna con valori quasi tutti diversi, come una data o
#' una coordinata, vengono pulite le sole celle trovate da
#' `celle_da_pulire()`. Le colonne non testuali, come quelle di un file
#' Parquet, restano come sono.
#' @noRd
pulisci_testo <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  if (!is.character(x)) {
    return(x)
  }
  pulisci <- function(valori) dplyr::na_if(stringr::str_squish(valori), "")
  campione <- utils::head(x, 5000)
  if (length(unique(campione)) <= length(campione) / 2) {
    return(per_valori_distinti(x, pulisci))
  }
  da_pulire <- celle_da_pulire(x)
  x[da_pulire] <- pulisci(x[da_pulire])
  x
}

#' Posizioni delle celle che `stringr::str_squish()` modificherebbe, più le vuote
#'
#' Una cella va pulita se è vuota, se inizia o finisce con uno spazio, se ha
#' due spazi di seguito, o se contiene uno spazio che non è quello semplice:
#' una tabulazione, un a capo, uno spazio non separabile. Ogni controllo è una
#' ricerca di testo fisso o di un solo carattere: su due milioni di celle
#' costa un decimo di secondo, contro il secondo di un'espressione regolare
#' con più alternative.
#'
#' @param x Vettore di testo.
#' @return Vettore di posizioni. Le celle mancanti non compaiono.
#' @noRd
celle_da_pulire <- function(x) {
  which(
    !nzchar(x) |
      startsWith(x, " ") |
      endsWith(x, " ") |
      stringr::str_detect(x, stringr::fixed("  ")) |
      stringr::str_detect(x, "[^\\S\\x20]")
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

#' Valida il dataset e converte i campi nei tipi corretti
#'
#' Controlla la presenza dei campi obbligatori, converte `giorno_lettura` in
#' POSIXct e le coordinate in numerico, normalizza i valori testuali.
#' I valori non interpretabili generano un errore esplicito; le righe con campi
#' essenziali vuoti vengono scartate e segnalate tra gli avvisi.
#'
#' I campi facoltativi numerici (`volume_previsto`,
#' `numero_raccolte_annue_previste`) vengono convertiti se presenti. I nomi dei
#' comuni sono riportati alla grafia della tabella dei comuni. Se un campo
#' facoltativo manca, la colonna è creata vuota e la mancanza è segnalata tra
#' gli avvisi. Alcune colonne possono avere un altro nome, vedi
#' `alias_colonne()`. Le colonne che l'app non usa vengono ignorate.
#'
#' Comune di lettura, comune assegnato e cantiere sono completati da
#' `aggiungi_geografia()`; la coerenza dei servizi da `coerenza_servizio()`.
#'
#' @param data Data frame letto da `leggi_tabella()`.
#' @return Lista con `dati` (tibble validato) e `avvisi` (vettore di testo).
#' @noRd
validate_dataset <- function(data) {
  attese <- colonne_obbligatorie()
  nomi <- stringr::str_trim(names(data))
  # Nomi alternativi: valgono se il file non ha la colonna con il nome dell'app.
  alias <- alias_colonne()
  for (alternativo in names(alias)) {
    if (!alias[[alternativo]] %in% tolower(nomi)) {
      nomi[tolower(nomi) == alternativo] <- alias[[alternativo]]
    }
  }
  posizione <- match(tolower(attese), tolower(nomi))
  mancanti <- attese[is.na(posizione)]
  if (length(mancanti) > 0) {
    errore_validazione(sprintf(
      "Colonne mancanti nel file: %s. Il CSV deve contenere tutti i %d campi obbligatori.",
      paste(mancanti, collapse = ", "),
      length(attese)
    ))
  }

  facoltative <- colonne_facoltative()
  posizione_facoltative <- match(tolower(facoltative), tolower(nomi))
  originale <- data
  data <- dplyr::as_tibble(data[, posizione, drop = FALSE])
  names(data) <- attese
  for (i in seq_along(facoltative)) {
    data[[facoltative[i]]] <- if (is.na(posizione_facoltative[i])) {
      rep(NA_character_, nrow(data))
    } else {
      originale[[posizione_facoltative[i]]]
    }
  }
  # Il servizio del contenitore a database, vuoto per i non censiti.
  posizione_contenitore <- match(colonne_contenitore(), tolower(nomi))
  con_contenitore <- !is.na(posizione_contenitore)
  data[[colonne_contenitore()]] <- if (con_contenitore) {
    originale[[posizione_contenitore]]
  } else {
    rep(NA_character_, nrow(data))
  }
  # I file preparati tra il 13 e il 15 ottobre 2026 non hanno
  # `servizio_transponder`: il servizio del database stava nel servizio
  # atteso, con `origine_servizio` uguale a "Database". Resta in una colonna
  # di passaggio, che il risultato non riporta.
  posizione_origine <- match("origine_servizio", tolower(nomi))
  con_origine <- !con_contenitore && !is.na(posizione_origine)
  if (con_origine) {
    data$origine_servizio <- originale[[posizione_origine]]
  }
  # Il servizio del calendario: nei file che non lo riportano è quello atteso.
  posizione_giro <- match(colonne_calendario(), tolower(nomi))
  data[[colonne_calendario()]] <- if (is.na(posizione_giro)) {
    data$servizio_atteso
  } else {
    originale[[posizione_giro]]
  }
  rm(originale)
  if (nrow(data) == 0) {
    errore_validazione("Il file non contiene righe di dati.")
  }

  for (campo in names(data)) {
    data[[campo]] <- pulisci_testo(data[[campo]])
  }
  # Codici e nomi sono sempre testo, anche se il file li riporta come numeri.
  testuali <- setdiff(
    names(data),
    c("giorno_lettura", "latitudine", "longitudine", colonne_quantita())
  )
  for (campo in testuali) {
    if (!is.character(data[[campo]])) {
      data[[campo]] <- as.character(data[[campo]])
    }
  }

  errori <- character(0)
  avvisi <- character(0)

  # --- Conversione dei tipi -------------------------------------------------
  giorno <- converti_data_ora(data$giorno_lettura)
  non_validi <- which(!is.na(data$giorno_lettura) & is.na(giorno))
  if (length(non_validi) > 0) {
    errori <- c(
      errori,
      sprintf(
        "giorno_lettura: %d valori non riconosciuti come data e ora (formato atteso YYYY-MM-DD HH:MM:SS). Righe del file: %s.",
        length(non_validi),
        elenco_righe(non_validi)
      )
    )
  }

  coordinate <- list(latitudine = c(-90, 90), longitudine = c(-180, 180))
  numeri <- purrr::imap(coordinate, function(limiti, campo) {
    valori <- converti_numero(data[[campo]])
    non_numerici <- which(!is.na(data[[campo]]) & is.na(valori))
    fuori_scala <- which(
      !is.na(valori) & (valori < limiti[1] | valori > limiti[2])
    )
    list(
      valori = valori,
      non_numerici = non_numerici,
      fuori_scala = fuori_scala
    )
  })
  for (campo in names(numeri)) {
    esito <- numeri[[campo]]
    if (length(esito$non_numerici) > 0) {
      errori <- c(
        errori,
        sprintf(
          "%s: %d valori non numerici. Righe del file: %s.",
          campo,
          length(esito$non_numerici),
          elenco_righe(esito$non_numerici)
        )
      )
    }
    if (length(esito$fuori_scala) > 0) {
      errori <- c(
        errori,
        sprintf(
          "%s: %d valori fuori dall'intervallo ammesso (%s .. %s). Righe del file: %s.",
          campo,
          length(esito$fuori_scala),
          coordinate[[campo]][1],
          coordinate[[campo]][2],
          elenco_righe(esito$fuori_scala)
        )
      )
    }
  }

  stato <- per_valori_distinti(data$presente_a_database, function(valori) {
    stati_database()[match(tolower(valori), tolower(stati_database()))]
  })
  non_validi <- which(!is.na(data$presente_a_database) & is.na(stato))
  if (length(non_validi) > 0) {
    errori <- c(
      errori,
      sprintf(
        "presente_a_database: valori non ammessi (%s). Sono accettati solo \"Presente\" e \"Non Presente\". Righe del file: %s.",
        paste(
          utils::head(unique(data$presente_a_database[non_validi]), 3),
          collapse = ", "
        ),
        elenco_righe(non_validi)
      )
    )
  }

  quantita <- purrr::map(
    stats::setNames(colonne_quantita(), colonne_quantita()),
    function(campo) {
      valori <- converti_numero(data[[campo]])
      list(
        valori = valori,
        non_validi = which(!is.na(data[[campo]]) & (is.na(valori) | valori < 0))
      )
    }
  )
  for (campo in colonne_quantita()) {
    non_validi <- quantita[[campo]]$non_validi
    if (length(non_validi) > 0) {
      errori <- c(
        errori,
        sprintf(
          "%s: %d valori non numerici o negativi. Righe del file: %s.",
          campo,
          length(non_validi),
          elenco_righe(non_validi)
        )
      )
    }
  }

  if (length(errori) > 0) {
    errore_validazione(errori)
  }

  assenti <- facoltative[is.na(posizione_facoltative)]
  if (length(assenti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Colonne facoltative assenti: %s. Le informazioni che ne dipendono resteranno vuote.",
        paste(assenti, collapse = ", ")
      )
    )
  }

  maiuscolo <- function(x) per_valori_distinti(x, stringr::str_to_upper)
  data$volume_previsto <- quantita$volume_previsto$valori
  data$numero_raccolte_annue_previste <- quantita$numero_raccolte_annue_previste$valori
  data$giorno_lettura <- giorno
  data$presente_a_database <- stato
  data$servizio_transponder <- maiuscolo(data$servizio_transponder)
  data$servizio_atteso <- maiuscolo(data$servizio_atteso)
  data$servizio_giro <- maiuscolo(data$servizio_giro)
  # Un file con l'origine del servizio: il servizio del database torna nella
  # sua colonna, e il servizio atteso torna quello del giro.
  if (con_origine) {
    dal_database <- which(
      stato %in% "Presente" & toupper(data$origine_servizio) %in% "DATABASE"
    )
    data$servizio_transponder[dal_database] <- data$servizio_atteso[
      dal_database
    ]
    data$servizio_atteso[dal_database] <- uniforma_servizio(
      data$servizio_giro[dal_database]
    )
  }
  data$coerenza_servizio <- coerenza_servizio(data)
  data$comune_da_database <- normalizza_comune(data$comune_da_database)
  data$comune_giro <- normalizza_comune(data$comune_giro)
  data$comune_lettura <- normalizza_comune(data$comune_lettura)
  data$cantiere <- maiuscolo(data$cantiere)
  data$latitudine <- numeri$latitudine$valori
  data$longitudine <- numeri$longitudine$valori

  # --- Righe con campi essenziali vuoti --------------------------------------
  essenziali <- c(
    "giorno_lettura",
    "RFID",
    "presente_a_database",
    "latitudine",
    "longitudine"
  )
  complete <- stats::complete.cases(data[, essenziali])
  n_scartate <- sum(!complete)
  if (n_scartate == nrow(data)) {
    errore_validazione(sprintf(
      "Nessuna riga utilizzabile: tutte le righe hanno almeno un campo essenziale vuoto (%s).",
      paste(essenziali, collapse = ", ")
    ))
  }
  if (n_scartate > 0) {
    data <- data[complete, , drop = FALSE]
    avvisi <- c(
      avvisi,
      sprintf(
        "%d righe scartate perch\u00e9 prive di un campo essenziale (%s).",
        n_scartate,
        paste(essenziali, collapse = ", ")
      )
    )
  }

  # --- Controlli di coerenza (non bloccanti) ----------------------------------
  incoerenti <- sum(
    data$presente_a_database == "Non Presente" &
      (!is.na(data$servizio_transponder) | !is.na(data$id_utenza))
  )
  if (incoerenti > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "%d letture \"Non Presente\" riportano comunque servizio_transponder o id_utenza.",
        incoerenti
      )
    )
  }
  coppie <- dplyr::distinct(data, .data$targa_veicolo, .data$matricola_veicolo)
  if (
    anyDuplicated(coppie$targa_veicolo) > 0 ||
      anyDuplicated(coppie$matricola_veicolo) > 0
  ) {
    avvisi <- c(
      avvisi,
      "La relazione tra targa_veicolo e matricola_veicolo non \u00e8 univoca (1:1)."
    )
  }

  # --- Comune assegnato e cantiere ----------------------------------------------
  data <- aggiungi_geografia(data)
  # Un comune di lettura del file che la tabella non contiene: la lettura
  # conta come fatta fuori dai comuni serviti.
  letti <- unique(data$comune_lettura)
  letti_ignoti <- sort(letti[!is.na(letti) & is.na(cantiere_di_comune(letti))])
  if (length(letti_ignoti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Comuni di lettura assenti dalla tabella dei comuni: %s%s. Le letture fatte l\u00ec contano come fatte fuori dai comuni serviti.",
        paste(utils::head(letti_ignoti, 5), collapse = ", "),
        if (length(letti_ignoti) > 5) ", \u2026" else ""
      )
    )
  }
  comuni <- unique(data$comune_assegnato)
  ignoti <- sort(comuni[!is.na(comuni) & is.na(cantiere_di_comune(comuni))])
  if (length(ignoti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Comuni assenti dalla tabella dei comuni: %s%s. Il loro cantiere \u00e8 quello indicato nel file, se c'\u00e8.",
        paste(utils::head(ignoti, 5), collapse = ", "),
        if (length(ignoti) > 5) ", \u2026" else ""
      )
    )
  }

  # Un comune del giro con un nome che la tabella non conosce: forse scritto
  # male, e allora non coincide con il comune di lettura.
  dei_giri <- unique(data$comune_giro)
  giri_ignoti <- sort(
    dei_giri[!is.na(dei_giri) & is.na(cantiere_di_comune(dei_giri))]
  )
  if (length(giri_ignoti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Comuni dei giri assenti dalla tabella dei comuni: %s%s. Se il nome \u00e8 sbagliato, i non censiti letti in quei giri risultano fuori giro.",
        paste(utils::head(giri_ignoti, 5), collapse = ", "),
        if (length(giri_ignoti) > 5) ", \u2026" else ""
      )
    )
  }

  list(dati = data[, colonne_dataset()], avvisi = avvisi)
}

#' Legge e valida un file di letture RFID
#'
#' @param path Percorso del file: CSV, CSV compresso con gzip o Parquet.
#' @param nome Nome originale del file, vedi `leggi_tabella()`.
#' @return Lista con `dati` e `avvisi`, vedi `validate_dataset()`.
#' @noRd
carica_dataset <- function(path, nome = basename(path)) {
  validate_dataset(leggi_tabella(path, nome))
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
#' @param lettore Funzione che legge e valida il file: `carica_dataset()` o
#'   `carica_storico()`.
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

#' Deduplica RFID mantenendo solo l'ultima lettura
#'
#' Le righe del risultato vanno dalla lettura più recente alla meno recente.
#' Con `righe` la scelta avviene tra le sole letture indicate, senza creare
#' prima una copia filtrata del dataset: su milioni di letture è la parte più
#' costosa di ogni cambio di filtro.
#'
#' @param data Dataframe con letture
#' @param righe Vettore logico con le letture da considerare. Tutte, se manca.
#' @return Dataframe deduplicato (un RFID = una riga)
#' @noRd
deduplica_ultimo_rfid <- function(data, righe = NULL) {
  indici <- if (is.null(righe)) seq_len(nrow(data)) else which(righe)
  # A parità di istante resta l'ordine delle righe.
  recenti <- order(
    data$giorno_lettura[indici],
    decreasing = TRUE,
    method = "radix"
  )
  indici <- indici[recenti]
  data[indici[!duplicated(data$RFID[indici])], , drop = FALSE]
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

#' Anni presenti nel dataset, dal più recente
#' @noRd
anni_disponibili <- function(data) {
  sort(unique(lubridate::year(data$giorno_lettura)), decreasing = TRUE)
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

#' Letture che superano i filtri laterali
#'
#' I servizi si filtrano per esclusione. Una lettura passa se il suo servizio
#' non è tra quelli deselezionati: per un censito è il servizio del
#' contenitore a database, per un non censito il servizio stimato dell'RFID,
#' vedi `servizio_mostrato()`. Gli RFID non censiti con la stima incerta, quelli
#' che sulla mappa hanno il punto di domanda, passano solo con
#' `includi_stima_incerta`. Lo stato a database si sceglie a parte, e vale
#' per tutti gli elenchi.
#'
#' Il giro è `servizio_giro`, con il nome del calendario: `SECCO PAP` è una
#' voce diversa da `SECCO CONT.STRADALI`. Vale per ogni lettura, di un censito
#' o no. Una lettura senza giro non è mai tra quelle deselezionate.
#'
#' La coerenza è la colonna `coerenza_servizio`: servizio a database e
#' servizio atteso dal giro dello stesso rifiuto, vedi `coerenza_servizio()`.
#' Una lettura di cui non si può dire, come quella di un non censito, sta
#' con quelle coerenti.
#'
#' La coerenza del comune è la colonna `coerenza_comune`, con le voci di
#' `stati_coerenza_comune()`: passano le letture la cui voce non è tra quelle
#' deselezionate.
#'
#' Anche i cantieri si filtrano per esclusione; le letture senza cantiere
#' corrispondono alla voce `senza_cantiere()`. I comuni si scelgono: passano
#' le letture il cui comune assegnato è tra quelli indicati, anche se
#' appartengono a cantieri diversi.
#'
#' @param data Dataframe con letture. Per il filtro sul servizio serve la
#'   colonna `servizio_icona`: vedi `servizio_mostrato()`.
#' @param stato_db Valori di `presente_a_database` da mantenere.
#' @param servizi_esclusi Servizi deselezionati.
#' @param includi_stima_incerta Mantiene le letture degli RFID non censiti
#'   senza un servizio stimato.
#' @param giri_esclusi Giri deselezionati, con il nome del calendario.
#' @param coerenza_escluse Voci di `stati_coerenza()` deselezionate.
#' @param cantieri_esclusi Cantieri deselezionati.
#' @param comuni Comuni da mantenere. Tutti, se il vettore è vuoto.
#' @param coerenza_comune_escluse Voci di `stati_coerenza_comune()`
#'   deselezionate.
#' @return Vettore logico, un valore per lettura.
#' @noRd
maschera_filtri <- function(
  data,
  stato_db = stati_database(),
  servizi_esclusi = character(0),
  includi_stima_incerta = TRUE,
  giri_esclusi = character(0),
  coerenza_escluse = character(0),
  cantieri_esclusi = character(0),
  comuni = character(0),
  coerenza_comune_escluse = character(0)
) {
  # Ogni filtro passa su tutte le letture solo se è attivo: con milioni di
  # righe un confronto in meno si sente. Un servizio mancante non è mai tra
  # quelli deselezionati.
  servizi_esclusi <- servizi_esclusi[!is.na(servizi_esclusi)]
  giri_esclusi <- giri_esclusi[!is.na(giri_esclusi)]
  tieni <- data$presente_a_database %in% stato_db
  senza_incerti <- !isTRUE(includi_stima_incerta)
  if (length(servizi_esclusi) > 0 || senza_incerti) {
    servizio <- servizio_mostrato(data)
    if (length(servizi_esclusi) > 0) {
      tieni <- tieni & !(servizio %in% servizi_esclusi)
    }
    if (senza_incerti) {
      tieni <- tieni &
        !(data$presente_a_database == "Non Presente" & is.na(servizio))
    }
  }
  if (length(giri_esclusi) > 0 && "servizio_giro" %in% names(data)) {
    tieni <- tieni & !(data$servizio_giro %in% giri_esclusi)
  }
  # Coerenza dei servizi: le letture di cui non si può dire stanno con quelle
  # coerenti.
  stati <- stati_coerenza()
  coerenza_escluse <- intersect(coerenza_escluse, names(stati))
  if (length(coerenza_escluse) > 0) {
    non_coerente <- if (is.null(data[["coerenza_servizio"]])) {
      rep(FALSE, nrow(data))
    } else {
      data$coerenza_servizio %in% stati[["non_coerente"]]
    }
    if ("coerente" %in% coerenza_escluse) {
      tieni <- tieni & non_coerente
    }
    if ("non_coerente" %in% coerenza_escluse) {
      tieni <- tieni & !non_coerente
    }
  }

  # I filtri geografici valgono solo per i dataset che hanno le colonne.
  if (length(cantieri_esclusi) > 0 && "cantiere" %in% names(data)) {
    cantiere <- data$cantiere
    cantiere[is.na(cantiere)] <- senza_cantiere()
    tieni <- tieni & !(cantiere %in% cantieri_esclusi)
  }
  comuni <- comuni[!is.na(comuni) & nzchar(comuni)]
  if (length(comuni) > 0 && "comune_assegnato" %in% names(data)) {
    tieni <- tieni & data$comune_assegnato %in% comuni
  }
  # Coerenza del comune: comune di lettura contro comune dichiarato.
  voci <- stati_coerenza_comune()
  coerenza_comune_escluse <- intersect(coerenza_comune_escluse, names(voci))
  if (
    length(coerenza_comune_escluse) > 0 && "coerenza_comune" %in% names(data)
  ) {
    tieni <- tieni & !(data$coerenza_comune %in% voci[coerenza_comune_escluse])
  }
  tieni
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
#' @param cantieri_letture Colonna `cantiere` di tutte le letture che
#'   superano i filtri, per contare le letture di ogni cantiere. Facoltativa.
#' @return Lista con totale, conteggi per stato database, per servizio e per
#'   cantiere: `servizio_censiti` conta i censiti per servizio a database,
#'   `servizio_stimato` i non censiti con un servizio stimato,
#'   `stima_incerta` quelli senza;
#'   `giri` conta gli RFID per giro del calendario; `non_coerenti` quelli con
#'   i servizi che non concordano. `rfid_cantieri` conta gli RFID,
#'   `letture_cantieri` le letture.
#' @noRd
calcola_statistiche <- function(data, cantieri_letture = NULL) {
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
    letture_cantieri = conta_cantieri(cantieri_letture)
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

#' Tutte le letture degli RFID cercati
#'
#' @param data Dataframe con letture.
#' @param rfid_list Vettore di RFID cercati.
#' @return Tutte le letture trovate, ordinate per RFID e data decrescente, con
#'   la colonna `is_ultimo` che marca la lettura più recente di ogni RFID.
#' @noRd
cerca_per_rfid <- function(data, rfid_list) {
  data |>
    dplyr::filter(codice_in(.data$RFID, rfid_list)) |>
    dplyr::arrange(.data$RFID, dplyr::desc(.data$giorno_lettura)) |>
    dplyr::group_by(.data$RFID) |>
    dplyr::mutate(is_ultimo = dplyr::row_number() == 1L) |>
    dplyr::ungroup()
}

#' Filtra RFID dove l'ultima lettura ha una delle utenze cercate
#'
#' Un contenitore passato da un'utenza a un'altra compare solo cercando
#' l'utenza attuale.
#'
#' @param data Dataframe
#' @param utenze Vettore di id utenza ricercati
#' @return Dataframe filtrato (ultima lettura di ogni RFID)
#' @noRd
filtra_ultimo_per_utenza <- function(data, utenze) {
  data |>
    deduplica_ultimo_rfid() |>
    dplyr::filter(codice_in(.data$id_utenza, utenze)) |>
    dplyr::arrange(.data$id_utenza, .data$RFID)
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
    fuori_comune = riepilogo(incongruenze$fuori_comune, "comune_da_database"),
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
