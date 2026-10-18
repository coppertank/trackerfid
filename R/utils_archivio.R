# Archivio delle letture in DuckDB: lettura del file, validazione, file .duckdb.
#
# Le letture non stanno in un data frame di R ma in una tabella di DuckDB, in
# memoria. La validazione è scritta in SQL: con milioni di righe è molto più
# veloce di quella in R e non tiene in memoria due copie delle letture. Le
# funzioni di R che lavorano su poche righe, come il dettaglio di un bidone o
# il riepilogo dei giri, ricevono i data frame estratti dall'archivio: vedi
# `R/utils_selezione.R`.

#' Versione dello schema dell'archivio
#'
#' Va aumentata quando cambiano le colonne della tabella delle letture: un
#' file `.duckdb` con un'altra versione va rigenerato.
#' @noRd
versione_archivio <- function() 2L

#' Apre un database DuckDB in memoria per un archivio
#'
#' Il database ha già la tabella `servizi_tipologie`, che dà la tipologia di
#' ogni servizio noto: la usano coerenza dei servizi e stima del servizio dei
#' non censiti. Un nome che la tabella non conosce fa tipologia a sé, come in
#' `tipologia_servizio()`.
#'
#' Le estensioni di DuckDB restano spente: l'app non ne usa, e così non
#' tenta mai di scaricarne dalla rete. Lettura di CSV, CSV compressi e
#' Parquet sono già nel pacchetto.
#'
#' @return Connessione DBI.
#' @noRd
connessione_duckdb <- function() {
  con <- DBI::dbConnect(
    duckdb::duckdb(shared_home = FALSE, allow_extensions = FALSE)
  )
  # Come `tipologia_servizio()`: per un nome ripetuto vale la prima tipologia.
  config <- servizi_config()
  config <- config[!duplicated(config$servizio), , drop = FALSE]
  DBI::dbWriteTable(
    con,
    "servizi_tipologie",
    data.frame(
      servizio = config$servizio,
      tipologia = config$tipologia,
      stringsAsFactors = FALSE
    )
  )
  con
}

#' Crea l'oggetto archivio attorno a una connessione
#'
#' La connessione si chiude quando l'archivio non serve più: alla chiamata di
#' `chiudi_archivio()` oppure, per un archivio dimenticato, quando R libera la
#' memoria.
#'
#' @param con Connessione DuckDB.
#' @param tabella Nome della tabella delle letture, eventualmente con lo
#'   schema: `"letture"` oppure `"archivio_file.letture"`.
#' @param origine Nome del file da cui vengono le letture.
#' @param condiviso `TRUE` per un archivio usato da più sessioni, che
#'   `chiudi_archivio()` non chiude.
#' @return Lista di classe `archivio_letture` con `con`, `tabella`,
#'   `origine`, `condiviso` e il riepilogo: `n`, `n_rfid`, `primo` e `ultimo`
#'   istante.
#' @noRd
nuovo_archivio <- function(con, tabella, origine = NULL, condiviso = FALSE) {
  risorsa <- new.env(parent = emptyenv())
  risorsa$con <- con
  reg.finalizer(
    risorsa,
    function(e) {
      if (!is.null(e$con) && DBI::dbIsValid(e$con)) {
        try(DBI::dbDisconnect(e$con, shutdown = TRUE), silent = TRUE)
      }
    },
    onexit = TRUE
  )
  riepilogo <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT count(*) AS n, count(DISTINCT RFID) AS n_rfid, min(giorno_lettura) AS primo, max(giorno_lettura) AS ultimo FROM %s",
      tabella
    )
  )
  structure(
    list(
      con = con,
      risorsa = risorsa,
      tabella = tabella,
      origine = origine,
      condiviso = condiviso,
      n = as.integer(riepilogo$n),
      n_rfid = as.integer(riepilogo$n_rfid),
      primo = riepilogo$primo,
      ultimo = riepilogo$ultimo
    ),
    class = "archivio_letture"
  )
}

#' Chiude la connessione di un archivio
#'
#' Un archivio condiviso tra le sessioni resta aperto.
#' @noRd
chiudi_archivio <- function(archivio) {
  if (is.null(archivio) || isTRUE(archivio$condiviso)) {
    return(invisible(NULL))
  }
  con <- archivio$risorsa$con
  if (!is.null(con) && DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
  archivio$risorsa$con <- NULL
  invisible(NULL)
}

#' Esegue una query e restituisce un tibble
#' @noRd
interroga <- function(archivio, sql) {
  dplyr::as_tibble(DBI::dbGetQuery(archivio$con, sql))
}

#' Testi tra apici per una query SQL
#'
#' @param con Connessione.
#' @param valori Vettore di testo.
#' @return Testo con i valori separati da virgole, pronto per `IN (...)`.
#' @noRd
sql_elenco <- function(con, valori) {
  paste(DBI::dbQuoteString(con, as.character(valori)), collapse = ", ")
}

#' Colonne della tabella delle letture, nell'ordine in cui sono salvate
#'
#' Sono quelle del dataset validato, più `riga`, la posizione della lettura
#' nel file: decide quale lettura resta tra due dello stesso istante.
#' @noRd
colonne_archivio <- function() c(colonne_dataset(), "riga")

#' Legge e valida un file di letture in un archivio DuckDB
#'
#' Accetta CSV, CSV compresso con gzip (`.csv.gz`), Parquet e il file
#' `.duckdb` scritto da `scrivi_archivio()`. Un CSV, compresso o no, e un
#' Parquet vengono letti in DuckDB e validati in SQL, con le stesse regole e
#' gli stessi messaggi della validazione descritta in `colonne_dataset()`:
#' vedi `valida_archivio()`. Un file `.duckdb` è già validato e si apre senza
#' rileggere niente: vedi `apri_file_archivio()`.
#'
#' @param path Percorso del file.
#' @param nome Nome originale del file: dà l'estensione quando `path` è il
#'   percorso temporaneo di un file caricato dal browser.
#' @return Lista con `archivio`, vedi `nuovo_archivio()`, e `avvisi`.
#' @noRd
carica_archivio <- function(path, nome = basename(path)) {
  estensione <- tolower(tools::file_ext(nome))
  if (estensione == "duckdb") {
    return(apri_file_archivio(path, nome))
  }
  con <- connessione_duckdb()
  riuscito <- FALSE
  on.exit(if (!riuscito) DBI::dbDisconnect(con, shutdown = TRUE))
  leggi_grezze(con, path, estensione)
  avvisi <- valida_archivio(con, csv = estensione != "parquet")
  riuscito <- TRUE
  list(archivio = nuovo_archivio(con, "letture", origine = nome), avvisi = avvisi)
}

#' Legge e valida un file di letture e ne restituisce un data frame
#'
#' È `carica_archivio()` seguita dall'estrazione di tutte le letture: serve a
#' script e test, che lavorano su poche migliaia di righe. L'app usa
#' l'archivio senza estrarlo.
#'
#' @inheritParams carica_archivio
#' @return Lista con `dati`, le letture validate in un tibble con le colonne
#'   di `colonne_dataset()`, e `avvisi`.
#' @noRd
carica_dataset <- function(path, nome = basename(path)) {
  esito <- carica_archivio(path, nome)
  on.exit(chiudi_archivio(esito$archivio))
  list(dati = letture_archivio(esito$archivio), avvisi = esito$avvisi)
}

#' Tutte le letture di un archivio, o quelle che soddisfano una condizione
#'
#' @param archivio Archivio delle letture.
#' @param condizione Condizione SQL, oppure `NULL` per tutte le letture.
#' @return Tibble con le colonne di `colonne_dataset()`, nell'ordine del file.
#' @noRd
letture_archivio <- function(archivio, condizione = NULL) {
  interroga(
    archivio,
    sprintf(
      "SELECT %s FROM %s%s ORDER BY riga",
      paste(colonne_dataset(), collapse = ", "),
      archivio$tabella,
      if (is.null(condizione)) "" else paste(" WHERE", condizione)
    )
  )
}

# --- Lettura del file ---------------------------------------------------------

#' Carica il file nella tabella `grezze`, senza conversioni
#'
#' Un CSV si legge tutto come testo, con il separatore più frequente nella
#' riga di intestazione, come `rileva_separatore()`: codici come RFID e utenze
#' non perdono gli zeri iniziali. Un Parquet arriva con i tipi del file.
#' @noRd
leggi_grezze <- function(con, path, estensione) {
  if (!file.exists(path)) {
    errore_validazione(paste0("File non trovato: ", path, "."))
  }
  if (estensione == "parquet") {
    sql <- sprintf(
      "CREATE TABLE grezze AS SELECT * FROM read_parquet(%s)",
      DBI::dbQuoteString(con, path)
    )
    formato <- "Parquet"
  } else {
    # La compressione si riconosce dai primi due byte, non dall'estensione:
    # un CSV non compresso chiamato .gz si legge lo stesso.
    compresso <- identical(
      readBin(path, "raw", n = 2),
      as.raw(c(0x1f, 0x8b))
    )
    intestazione <- tryCatch(
      {
        connessione <- if (compresso) gzfile(path) else file(path)
        on.exit(close(connessione))
        readLines(connessione, n = 1, warn = FALSE, encoding = "UTF-8")
      },
      error = function(e) {
        errore_validazione(paste0(
          "Impossibile decomprimere il file: ",
          conditionMessage(e)
        ))
      }
    )
    if (length(intestazione) == 0 || !nzchar(stringr::str_trim(intestazione))) {
      errore_validazione("Il file \u00e8 vuoto.")
    }
    virgole <- stringr::str_count(intestazione, stringr::fixed(","))
    punti_virgola <- stringr::str_count(intestazione, stringr::fixed(";"))
    sql <- sprintf(
      paste0(
        "CREATE TABLE grezze AS SELECT * FROM read_csv(%s, delim = %s, ",
        "header = true, all_varchar = true, nullstr = ['NA', ''], ",
        "compression = '%s', null_padding = true)"
      ),
      DBI::dbQuoteString(con, path),
      DBI::dbQuoteString(con, if (punti_virgola > virgole) ";" else ","),
      if (compresso) "gzip" else "none"
    )
    formato <- "CSV"
  }
  tryCatch(
    DBI::dbExecute(con, sql),
    error = function(e) {
      errore_validazione(paste0(
        "Impossibile leggere il file come ",
        formato,
        ": ",
        conditionMessage(e)
      ))
    }
  )
  invisible(NULL)
}

#' Espressione SQL che pulisce un campo di testo
#'
#' Come `stringr::str_squish()`: spazi ai margini tolti, spazi interni ridotti
#' a uno, testo vuoto mancante. In un CSV anche il testo `NA` è mancante.
#'
#' @param colonna Espressione SQL del campo, già di tipo testo.
#' @param csv `TRUE` per un campo letto da un CSV.
#' @noRd
sql_pulito <- function(colonna, csv = TRUE) {
  pulito <- sprintf(
    "nullif(regexp_replace(regexp_replace(%s, '[\\s\\x{00A0}]+', ' ', 'g'), '^ | $', '', 'g'), '')",
    colonna
  )
  if (csv) sprintf("nullif(%s, 'NA')", pulito) else pulito
}

#' Formati di data e ora accettati per `giorno_lettura`
#'
#' Anno-mese-giorno o giorno/mese/anno,
#' con ore, minuti ed eventuali secondi, oppure la sola data.
#' @noRd
formati_data_ora <- function() {
  c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M:%S.%f",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%d.%m.%Y %H:%M:%S",
    "%d.%m.%Y %H:%M",
    "%Y-%m-%d",
    "%Y/%m/%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%d.%m.%Y"
  )
}

# --- Validazione --------------------------------------------------------------

#' Valida la tabella `grezze` e scrive la tabella `letture`
#'
#' Applica in SQL le regole della validazione delle letture:
#'
#' - le colonne obbligatorie devono esserci, con i nomi alternativi di
#'   `alias_colonne()`; le facoltative mancanti diventano vuote, con un
#'   avviso;
#' - i testi sono puliti come da `sql_pulito()`; servizi e cantiere passano
#'   in maiuscolo, i comuni nella grafia della tabella dei comuni;
#' - data e ora, coordinate e quantità non interpretabili, e valori di
#'   `presente_a_database` diversi da `Presente` e `Non Presente`, fermano il
#'   caricamento con un messaggio che indica le righe del file;
#' - le righe senza un campo essenziale vengono scartate, con un avviso;
#' - comune geografico, comune assegnato, cantiere e coerenza del comune
#'   seguono `aggiungi_geografia()`, la coerenza dei servizi
#'   `coerenza_servizio()`;
#' - un file con `origine_servizio` e senza `servizio_transponder`, preparato
#'   tra il 13 e il 15 ottobre 2026, riporta il servizio del database nella
#'   sua colonna.
#'
#' @param con Connessione con la tabella `grezze`.
#' @param csv `TRUE` se le letture vengono da un CSV.
#' @return Gli avvisi, un vettore di testo.
#' @noRd
valida_archivio <- function(con, csv = TRUE) {
  descrizione <- DBI::dbGetQuery(con, "DESCRIBE grezze")
  nomi <- stringr::str_trim(descrizione$column_name)
  tipi_file <- stats::setNames(toupper(descrizione$column_type), descrizione$column_name)
  # Nomi alternativi: valgono se il file non ha la colonna con il nome dell'app.
  nomi_app <- nomi
  alias <- alias_colonne()
  for (alternativo in names(alias)) {
    if (!alias[[alternativo]] %in% tolower(nomi_app)) {
      nomi_app[tolower(nomi_app) == alternativo] <- alias[[alternativo]]
    }
  }
  # Colonna del file corrispondente a un campo dell'app, oppure NA.
  colonna_di <- function(campo) {
    i <- match(tolower(campo), tolower(nomi_app))
    if (is.na(i)) NA_character_ else descrizione$column_name[i]
  }
  attese <- colonne_obbligatorie()
  mancanti <- attese[is.na(vapply(attese, colonna_di, character(1)))]
  if (length(mancanti) > 0) {
    errore_validazione(sprintf(
      "Colonne mancanti nel file: %s. Il CSV deve contenere tutti i %d campi obbligatori.",
      paste(mancanti, collapse = ", "),
      length(attese)
    ))
  }
  n_righe <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM grezze")$n
  if (n_righe == 0) {
    errore_validazione("Il file non contiene righe di dati.")
  }

  identificatore <- function(colonna) as.character(DBI::dbQuoteIdentifier(con, colonna))
  # Testo pulito di un campo, oppure NULL se il file non ha la colonna.
  testo <- function(campo) {
    colonna <- colonna_di(campo)
    if (is.na(colonna)) {
      return("CAST(NULL AS VARCHAR)")
    }
    tipo <- tipi_file[[colonna]]
    valore <- if (tipo == "VARCHAR") {
      identificatore(colonna)
    } else {
      sprintf("CAST(%s AS VARCHAR)", identificatore(colonna))
    }
    sql_pulito(valore, csv = csv && tipo == "VARCHAR")
  }
  # Un campo numerico: il testo con la virgola decimale, o il numero del file.
  numero <- function(campo) {
    colonna <- colonna_di(campo)
    if (is.na(colonna)) {
      return(c(testo = "CAST(NULL AS VARCHAR)", valore = "CAST(NULL AS DOUBLE)"))
    }
    if (tipi_file[[colonna]] != "VARCHAR") {
      return(c(
        testo = "CAST(NULL AS VARCHAR)",
        valore = sprintf("TRY_CAST(%s AS DOUBLE)", identificatore(colonna))
      ))
    }
    pulito <- testo(campo)
    c(
      testo = pulito,
      valore = sprintf(
        "TRY_CAST(regexp_replace(%s, ',', '.') AS DOUBLE)",
        pulito
      )
    )
  }
  data_ora <- local({
    colonna <- colonna_di("giorno_lettura")
    tipo <- tipi_file[[colonna]]
    if (tipo == "VARCHAR") {
      pulito <- testo("giorno_lettura")
      c(
        testo = pulito,
        valore = sprintf(
          "try_strptime(%s, [%s])",
          pulito,
          sql_elenco(con, formati_data_ora())
        )
      )
    } else {
      c(
        testo = "CAST(NULL AS VARCHAR)",
        valore = sprintf("TRY_CAST(%s AS TIMESTAMP)", identificatore(colonna))
      )
    }
  })
  latitudine <- numero("latitudine")
  longitudine <- numero("longitudine")
  volume <- numero("volume_previsto")
  raccolte <- numero("numero_raccolte_annue_previste")
  con_contenitore <- !is.na(colonna_di(colonne_contenitore()))
  con_origine <- !con_contenitore && !is.na(colonna_di("origine_servizio"))
  con_giro <- !is.na(colonna_di(colonne_calendario()))

  # Tabella di lavoro: testi puliti, valori convertiti e, per i campi che
  # possono non essere interpretabili, il testo di partenza.
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TEMP TABLE lavoro AS SELECT rowid + 1 AS riga, ",
      data_ora[["testo"]], " AS t_giorno, ",
      data_ora[["valore"]], " AS giorno_lettura, ",
      testo("targa_veicolo"), " AS targa_veicolo, ",
      testo("matricola_veicolo"), " AS matricola_veicolo, ",
      testo("RFID"), " AS RFID, ",
      testo("presente_a_database"), " AS t_presente, ",
      "upper(", testo("servizio_atteso"), ") AS servizio_atteso, ",
      "upper(", testo(colonne_contenitore()), ") AS servizio_transponder, ",
      "upper(", if (con_giro) testo(colonne_calendario()) else testo("servizio_atteso"), ") AS servizio_giro, ",
      if (con_origine) testo("origine_servizio") else "CAST(NULL AS VARCHAR)", " AS origine_servizio, ",
      testo("id_utenza"), " AS id_utenza, ",
      latitudine[["testo"]], " AS t_latitudine, ",
      latitudine[["valore"]], " AS latitudine, ",
      longitudine[["testo"]], " AS t_longitudine, ",
      longitudine[["valore"]], " AS longitudine, ",
      volume[["testo"]], " AS t_volume, ",
      volume[["valore"]], " AS volume_previsto, ",
      raccolte[["testo"]], " AS t_raccolte, ",
      raccolte[["valore"]], " AS numero_raccolte_annue_previste, ",
      testo("comune_fornitura"), " AS comune_fornitura, ",
      testo("comune_giro"), " AS comune_giro, ",
      testo("comune_lettura"), " AS comune_lettura, ",
      "upper(", testo("cantiere"), ") AS cantiere ",
      "FROM grezze"
    )
  )
  DBI::dbExecute(con, "DROP TABLE grezze")

  # --- Valori non interpretabili ---------------------------------------------
  # Le prime righe del file con un problema, per il messaggio.
  righe_con <- function(condizione) {
    trovate <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT riga FROM lavoro WHERE %s ORDER BY riga LIMIT 6",
        condizione
      )
    )$riga
    list(
      n = DBI::dbGetQuery(
        con,
        sprintf("SELECT count(*) AS n FROM lavoro WHERE %s", condizione)
      )$n,
      righe = elenco_righe(trovate)
    )
  }
  errori <- character(0)
  problema <- righe_con("t_giorno IS NOT NULL AND giorno_lettura IS NULL")
  if (problema$n > 0) {
    errori <- c(
      errori,
      sprintf(
        "giorno_lettura: %d valori non riconosciuti come data e ora (formato atteso YYYY-MM-DD HH:MM:SS). Righe del file: %s.",
        problema$n,
        problema$righe
      )
    )
  }
  coordinate <- list(latitudine = c(-90, 90), longitudine = c(-180, 180))
  for (campo in names(coordinate)) {
    limiti <- coordinate[[campo]]
    non_numerici <- righe_con(sprintf(
      "t_%s IS NOT NULL AND %s IS NULL",
      campo,
      campo
    ))
    if (non_numerici$n > 0) {
      errori <- c(
        errori,
        sprintf(
          "%s: %d valori non numerici. Righe del file: %s.",
          campo,
          non_numerici$n,
          non_numerici$righe
        )
      )
    }
    fuori_scala <- righe_con(sprintf(
      "%s < %s OR %s > %s",
      campo,
      limiti[1],
      campo,
      limiti[2]
    ))
    if (fuori_scala$n > 0) {
      errori <- c(
        errori,
        sprintf(
          "%s: %d valori fuori dall'intervallo ammesso (%s .. %s). Righe del file: %s.",
          campo,
          fuori_scala$n,
          limiti[1],
          limiti[2],
          fuori_scala$righe
        )
      )
    }
  }
  presente <- sprintf(
    "CASE lower(t_presente) WHEN %s THEN %s WHEN %s THEN %s END",
    DBI::dbQuoteString(con, tolower(stati_database()[1])),
    DBI::dbQuoteString(con, stati_database()[1]),
    DBI::dbQuoteString(con, tolower(stati_database()[2])),
    DBI::dbQuoteString(con, stati_database()[2])
  )
  non_ammessi <- sprintf("t_presente IS NOT NULL AND %s IS NULL", presente)
  problema <- righe_con(non_ammessi)
  if (problema$n > 0) {
    valori <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT t_presente FROM lavoro WHERE %s GROUP BY t_presente ORDER BY min(riga) LIMIT 3",
        non_ammessi
      )
    )$t_presente
    errori <- c(
      errori,
      sprintf(
        "presente_a_database: valori non ammessi (%s). Sono accettati solo \"Presente\" e \"Non Presente\". Righe del file: %s.",
        paste(valori, collapse = ", "),
        problema$righe
      )
    )
  }
  quantita <- c(
    volume_previsto = "t_volume",
    numero_raccolte_annue_previste = "t_raccolte"
  )
  for (campo in names(quantita)) {
    problema <- righe_con(sprintf(
      "(%s IS NOT NULL AND %s IS NULL) OR %s < 0",
      quantita[[campo]],
      campo,
      campo
    ))
    if (problema$n > 0) {
      errori <- c(
        errori,
        sprintf(
          "%s: %d valori non numerici o negativi. Righe del file: %s.",
          campo,
          problema$n,
          problema$righe
        )
      )
    }
  }
  if (length(errori) > 0) {
    errore_validazione(errori)
  }

  avvisi <- character(0)
  facoltative <- colonne_facoltative()
  assenti <- facoltative[is.na(vapply(facoltative, colonna_di, character(1)))]
  if (length(assenti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Colonne facoltative assenti: %s. Le informazioni che ne dipendono resteranno vuote.",
        paste(assenti, collapse = ", ")
      )
    )
  }

  # --- Comuni: grafia della tabella, chiave di confronto, cantiere ------------
  distinti <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT DISTINCT valore FROM (",
      "SELECT comune_fornitura AS valore FROM lavoro",
      "UNION ALL SELECT comune_giro FROM lavoro",
      "UNION ALL SELECT comune_lettura FROM lavoro) WHERE valore IS NOT NULL"
    )
  )$valore
  normalizzati <- normalizza_comune(distinti)
  tabella_comuni <- comuni_cantieri()$comune
  chiavi <- chiave_comune(normalizzati)
  chiavi[!is.na(chiavi) & !nzchar(chiavi)] <- NA_character_
  duckdb::duckdb_register(
    con,
    "mappa_comuni",
    data.frame(
      valore = as.character(distinti),
      comune = as.character(normalizzati),
      chiave = as.character(chiavi),
      servito = as.character(tabella_comuni[match(normalizzati, tabella_comuni)]),
      cantiere = as.character(cantiere_di_comune(normalizzati)),
      stringsAsFactors = FALSE
    )
  )
  on.exit(duckdb::duckdb_unregister(con, "mappa_comuni"), add = TRUE)
  # Il servizio uniforme di un giro, per i file con l'origine del servizio.
  giri <- DBI::dbGetQuery(
    con,
    "SELECT DISTINCT servizio_giro FROM lavoro WHERE servizio_giro IS NOT NULL"
  )$servizio_giro
  duckdb::duckdb_register(
    con,
    "mappa_giri",
    data.frame(
      servizio_giro = as.character(giri),
      uniforme = as.character(uniforma_servizio(giri)),
      stringsAsFactors = FALSE
    )
  )
  on.exit(duckdb::duckdb_unregister(con, "mappa_giri"), add = TRUE)

  # --- Tabella delle letture ---------------------------------------------------
  # Un file con l'origine del servizio: il servizio del database torna nella
  # sua colonna, e il servizio atteso torna quello del giro.
  dal_database <- if (con_origine) {
    sprintf(
      "(%s = 'Presente' AND upper(l.origine_servizio) = 'DATABASE')",
      presente
    )
  } else {
    "FALSE"
  }
  stati <- stati_coerenza()
  stati_comune <- stati_coerenza_comune()
  q <- function(x) DBI::dbQuoteString(con, x)
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE letture AS ",
      "WITH base AS (SELECT l.riga, l.giorno_lettura, l.targa_veicolo, l.matricola_veicolo, l.RFID, ",
      presente, " AS presente_a_database, ",
      "CASE WHEN ", dal_database, " THEN g.uniforme ELSE l.servizio_atteso END AS servizio_atteso, ",
      "l.id_utenza, l.latitudine, l.longitudine, ",
      "CASE WHEN ", dal_database, " THEN l.servizio_atteso ELSE l.servizio_transponder END AS servizio_transponder, ",
      "l.volume_previsto, l.numero_raccolte_annue_previste, ",
      "cd.comune AS comune_fornitura, cg.comune AS comune_giro, cl.comune AS comune_lettura, ",
      "l.cantiere AS cantiere_file, l.servizio_giro, ",
      "cl.servito AS comune_geografico, cd.chiave AS chiave_database, cg.chiave AS chiave_giro, cl.chiave AS chiave_lettura ",
      "FROM lavoro l ",
      "LEFT JOIN mappa_comuni cd ON cd.valore = l.comune_fornitura ",
      "LEFT JOIN mappa_comuni cg ON cg.valore = l.comune_giro ",
      "LEFT JOIN mappa_comuni cl ON cl.valore = l.comune_lettura ",
      "LEFT JOIN mappa_giri g ON g.servizio_giro = l.servizio_giro), ",
      "completa AS (SELECT *, coalesce(comune_geografico, comune_fornitura) AS comune_assegnato, ",
      "CASE WHEN presente_a_database = 'Presente' THEN chiave_database ELSE chiave_giro END AS chiave_dichiarata ",
      "FROM base WHERE giorno_lettura IS NOT NULL AND RFID IS NOT NULL AND presente_a_database IS NOT NULL ",
      "AND latitudine IS NOT NULL AND longitudine IS NOT NULL) ",
      "SELECT c.giorno_lettura, c.targa_veicolo, c.matricola_veicolo, c.RFID, c.presente_a_database, ",
      "c.servizio_atteso, c.id_utenza, c.latitudine, c.longitudine, c.servizio_transponder, ",
      "c.volume_previsto, c.numero_raccolte_annue_previste, c.comune_fornitura, c.comune_giro, ",
      "c.comune_lettura, coalesce(ca.cantiere, c.cantiere_file) AS cantiere, c.servizio_giro, ",
      "c.comune_assegnato, c.comune_geografico, ",
      "CASE WHEN c.chiave_lettura IS NULL OR c.chiave_dichiarata IS NULL THEN ", q(stati_comune[["ignota"]]), " ",
      "WHEN c.chiave_lettura = c.chiave_dichiarata THEN ", q(stati_comune[["coerente"]]), " ",
      "WHEN c.presente_a_database = 'Presente' THEN ", q(stati_comune[["fuori_comune"]]), " ",
      "ELSE ", q(stati_comune[["fuori_giro"]]), " END AS coerenza_comune, ",
      "CASE WHEN c.servizio_transponder IS NULL OR c.servizio_atteso IS NULL THEN NULL ",
      "WHEN coalesce(tt.tipologia, c.servizio_transponder) = coalesce(ta.tipologia, c.servizio_atteso) THEN ", q(stati[["coerente"]]), " ",
      "ELSE ", q(stati[["non_coerente"]]), " END AS coerenza_servizio, ",
      "c.riga ",
      "FROM completa c ",
      "LEFT JOIN (SELECT DISTINCT comune, cantiere FROM mappa_comuni) ca ON ca.comune = c.comune_assegnato ",
      "LEFT JOIN servizi_tipologie tt ON tt.servizio = c.servizio_transponder ",
      "LEFT JOIN servizi_tipologie ta ON ta.servizio = c.servizio_atteso ",
      "ORDER BY c.riga"
    )
  )
  DBI::dbExecute(con, "DROP TABLE lavoro")

  # --- Righe con campi essenziali vuoti ----------------------------------------
  essenziali <- c(
    "giorno_lettura",
    "RFID",
    "presente_a_database",
    "latitudine",
    "longitudine"
  )
  n_valide <- DBI::dbGetQuery(con, "SELECT count(*) AS n FROM letture")$n
  if (n_valide == 0) {
    errore_validazione(sprintf(
      "Nessuna riga utilizzabile: tutte le righe hanno almeno un campo essenziale vuoto (%s).",
      paste(essenziali, collapse = ", ")
    ))
  }
  if (n_valide < n_righe) {
    avvisi <- c(
      avvisi,
      sprintf(
        "%d righe scartate perch\u00e9 prive di un campo essenziale (%s).",
        n_righe - n_valide,
        paste(essenziali, collapse = ", ")
      )
    )
  }

  # --- Controlli di coerenza (non bloccanti) -----------------------------------
  incoerenti <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT count(*) AS n FROM letture WHERE presente_a_database = 'Non Presente'",
      "AND (servizio_transponder IS NOT NULL OR id_utenza IS NOT NULL)"
    )
  )$n
  if (incoerenti > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "%d letture \"Non Presente\" riportano comunque servizio_transponder o id_utenza.",
        incoerenti
      )
    )
  }
  # Una targa con più matricole, o una matricola con più targhe. Il valore
  # mancante conta come un valore.
  mezzi <- DBI::dbGetQuery(
    con,
    paste(
      "SELECT count(*) AS coppie,",
      "count(DISTINCT coalesce(targa_veicolo, chr(0))) AS targhe,",
      "count(DISTINCT coalesce(matricola_veicolo, chr(0))) AS matricole",
      "FROM (SELECT DISTINCT targa_veicolo, matricola_veicolo FROM letture)"
    )
  )
  if (mezzi$coppie > mezzi$targhe || mezzi$coppie > mezzi$matricole) {
    avvisi <- c(
      avvisi,
      "La relazione tra targa_veicolo e matricola_veicolo non \u00e8 univoca (1:1)."
    )
  }

  # --- Comuni che la tabella non contiene ---------------------------------------
  ignoti <- function(colonna) {
    valori <- DBI::dbGetQuery(
      con,
      sprintf(
        "SELECT DISTINCT %s AS valore FROM letture WHERE %s IS NOT NULL",
        colonna,
        colonna
      )
    )$valore
    sort(valori[is.na(cantiere_di_comune(valori))])
  }
  elenco <- function(valori) {
    paste0(
      paste(utils::head(valori, 5), collapse = ", "),
      if (length(valori) > 5) ", \u2026" else ""
    )
  }
  letti_ignoti <- ignoti("comune_lettura")
  if (length(letti_ignoti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Comuni di lettura assenti dalla tabella dei comuni: %s. Le letture fatte l\u00ec contano come fatte fuori dai comuni serviti.",
        elenco(letti_ignoti)
      )
    )
  }
  assegnati_ignoti <- ignoti("comune_assegnato")
  if (length(assegnati_ignoti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Comuni assenti dalla tabella dei comuni: %s. Il loro cantiere \u00e8 quello indicato nel file, se c'\u00e8.",
        elenco(assegnati_ignoti)
      )
    )
  }
  # Un comune del giro con un nome che la tabella non conosce: forse scritto
  # male, e allora non coincide con il comune di lettura.
  giri_ignoti <- ignoti("comune_giro")
  if (length(giri_ignoti) > 0) {
    avvisi <- c(
      avvisi,
      sprintf(
        "Comuni dei giri assenti dalla tabella dei comuni: %s. Se il nome \u00e8 sbagliato, i non censiti letti in quei giri risultano fuori giro.",
        elenco(giri_ignoti)
      )
    )
  }
  avvisi
}

# --- File .duckdb ---------------------------------------------------------------

#' Scrive un archivio in un file `.duckdb`
#'
#' Il file contiene la tabella `letture`, già validata, e la tabella
#' `informazioni`, con la versione dello schema, il file d'origine e gli
#' avvisi della validazione. L'app lo apre con `carica_archivio()` senza
#' rileggere né validare niente. Un file con lo stesso nome viene sostituito.
#'
#' @param archivio Archivio da scrivere, oppure il percorso di un file di
#'   letture da leggere con `carica_archivio()`.
#' @param percorso Percorso del file `.duckdb` da scrivere.
#' @param avvisi Avvisi della validazione, da mostrare quando l'app apre il
#'   file. Con un percorso come `archivio` vengono da `carica_archivio()`.
#' @return Il percorso, invisibile.
#' @noRd
scrivi_archivio <- function(archivio, percorso, avvisi = character(0)) {
  if (is.character(archivio)) {
    esito <- carica_archivio(archivio)
    on.exit(chiudi_archivio(esito$archivio), add = TRUE)
    archivio <- esito$archivio
    avvisi <- esito$avvisi
  }
  if (file.exists(percorso)) {
    unlink(percorso)
  }
  con <- archivio$con
  DBI::dbExecute(
    con,
    sprintf("ATTACH %s AS destinazione", DBI::dbQuoteString(con, percorso))
  )
  on.exit(DBI::dbExecute(con, "DETACH destinazione"), add = TRUE, after = FALSE)
  DBI::dbExecute(
    con,
    sprintf(
      "CREATE TABLE destinazione.letture AS SELECT %s FROM %s ORDER BY riga",
      paste(colonne_archivio(), collapse = ", "),
      archivio$tabella
    )
  )
  informazioni <- data.frame(
    chiave = c("versione", "origine", "avvisi", "scritto"),
    valore = c(
      as.character(versione_archivio()),
      archivio$origine %||% "",
      as.character(jsonlite::toJSON(as.character(avvisi))),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ),
    stringsAsFactors = FALSE
  )
  duckdb::duckdb_register(con, "informazioni_nuove", informazioni)
  on.exit(duckdb::duckdb_unregister(con, "informazioni_nuove"), add = TRUE)
  DBI::dbExecute(
    con,
    "CREATE TABLE destinazione.informazioni AS SELECT * FROM informazioni_nuove"
  )
  invisible(percorso)
}

#' Apre un file `.duckdb` scritto da `scrivi_archivio()`
#'
#' Il file viene collegato in sola lettura a un database in memoria: le
#' letture non vengono copiate.
#'
#' @param path Percorso del file.
#' @param nome Nome del file, per l'origine delle letture.
#' @return Lista con `archivio` e `avvisi`, come `carica_archivio()`.
#' @noRd
apri_file_archivio <- function(path, nome = basename(path)) {
  if (!file.exists(path)) {
    errore_validazione(paste0("File non trovato: ", path, "."))
  }
  con <- connessione_duckdb()
  riuscito <- FALSE
  on.exit(if (!riuscito) DBI::dbDisconnect(con, shutdown = TRUE))
  tryCatch(
    DBI::dbExecute(
      con,
      sprintf(
        "ATTACH %s AS archivio_file (READ_ONLY)",
        DBI::dbQuoteString(con, path)
      )
    ),
    error = function(e) {
      errore_validazione(paste0(
        "Impossibile aprire il file come archivio DuckDB: ",
        conditionMessage(e)
      ))
    }
  )
  tabelle <- DBI::dbGetQuery(
    con,
    "SELECT table_name FROM information_schema.tables WHERE table_catalog = 'archivio_file'"
  )$table_name
  if (!all(c("letture", "informazioni") %in% tabelle)) {
    errore_validazione(
      "Il file .duckdb non contiene un archivio di letture: va scritto con scrivi_archivio() o con il passo 3 della preparazione dei dati."
    )
  }
  informazioni <- DBI::dbGetQuery(
    con,
    "SELECT chiave, valore FROM archivio_file.informazioni"
  )
  valore <- function(chiave) informazioni$valore[match(chiave, informazioni$chiave)]
  if (!identical(valore("versione"), as.character(versione_archivio()))) {
    errore_validazione(sprintf(
      "Il file .duckdb \u00e8 stato scritto con lo schema %s, l'app usa lo schema %s: va rigenerato con il passo 3 della preparazione dei dati.",
      valore("versione") %||% "sconosciuto",
      versione_archivio()
    ))
  }
  colonne <- DBI::dbGetQuery(
    con,
    "SELECT column_name FROM information_schema.columns WHERE table_catalog = 'archivio_file' AND table_name = 'letture'"
  )$column_name
  if (!all(colonne_archivio() %in% colonne)) {
    errore_validazione(sprintf(
      "Il file .duckdb non ha le colonne attese: mancano %s.",
      paste(setdiff(colonne_archivio(), colonne), collapse = ", ")
    ))
  }
  avvisi <- tryCatch(
    as.character(jsonlite::fromJSON(valore("avvisi") %||% "[]")),
    error = function(e) character(0)
  )
  riuscito <- TRUE
  list(
    archivio = nuovo_archivio(con, "archivio_file.letture", origine = nome),
    avvisi = avvisi
  )
}
