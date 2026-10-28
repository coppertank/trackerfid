# Interrogazioni sull'archivio: un periodo è l'archivio più una condizione
# SQL, senza copie. Filtri, ricerche, giri e dettaglio portano in R solo le
# righe che servono.

#' Selezione delle letture di un periodo
#'
#' Contesto: ogni scheda lavora su un periodo (all'avvio l'anno più recente).
#' La selezione non copia le letture: è l'archivio più la condizione SQL del
#' periodo.
#' Funzionamento: conta letture e RFID del periodo; due date incluse, o `NULL`
#' per tutte.
#' Risultato: una lista di classe `selezione_letture`.
#' @return Lista di classe `selezione_letture`: `archivio`, `periodo`, `n`,
#'   `n_rfid`.
#' @noRd
seleziona_periodo <- function(archivio, periodo = NULL) {
  selezione <- structure(
    list(
      archivio = archivio,
      periodo = if (!is.null(periodo)) lubridate::as_date(periodo)
    ),
    class = "selezione_letture"
  )
  conteggio <- interroga(
    archivio,
    sprintf(
      "SELECT count(*) AS n, count(DISTINCT RFID) AS n_rfid FROM %s WHERE %s",
      archivio$tabella,
      sql_periodo(selezione)
    )
  )
  selezione$n <- as.integer(conteggio$n)
  selezione$n_rfid <- as.integer(conteggio$n_rfid)
  selezione$stima <- tabella_stima(selezione)
  selezione
}

#' Condizione SQL del periodo di una selezione
#' Contesto: da inserire nel `WHERE` delle query sulle letture; `TRUE` se non
#' c'è un periodo.
#' @noRd
sql_periodo <- function(selezione) {
  periodo <- selezione$periodo
  if (is.null(periodo)) {
    return("TRUE")
  }
  sprintf(
    "data_ora_lettura >= TIMESTAMP '%s 00:00:00' AND data_ora_lettura < TIMESTAMP '%s 00:00:00'",
    format(periodo[1]),
    format(periodo[2] + 1)
  )
}

#' Condizione SQL che riconosce i sacchetti, come `rfid_sacchetto()`
#' Contesto: 24 caratteri con prefisso `00BD`. Va scritta nei due posti.
#' @noRd
sql_sacchetto <- function(colonna = "RFID") {
  sprintf(
    "(length(%s) = 24 AND upper(substr(%s, 1, 4)) = '00BD')",
    colonna,
    colonna
  )
}

#' Tabella temporanea del servizio stimato dei non censiti nel periodo
#'
#' Contesto: un non censito non ha un servizio a database; icona, filtri e
#' statistiche usano quello stimato. Ricalcolarlo a ogni filtro rallentava, quindi
#' si calcola una volta per periodo.
#' Funzionamento: sulle letture non censite (`PIT` o `NP`) del periodo, se la tipologia
#' prevalente raggiunge `soglia` (80%) il servizio è il giro più letto di quella
#' tipologia; altrimenti `NA` (stima incerta). Regola di
#' `servizio_prevalente_non_censiti()`. La tabella resta finché l'archivio è
#' aperto ed è condivisa tra le sessioni.
#' Risultato: il nome della tabella, con `RFID` e `stimato`.
#' @return Il nome della tabella, con `RFID` e `stimato`.
#' @noRd
tabella_stima <- function(selezione, soglia = 0.8) {
  periodo <- selezione$periodo
  nome <- if (is.null(periodo)) {
    "stima_tutto"
  } else {
    paste0(
      "stima_",
      format(periodo[1], "%Y%m%d"),
      "_",
      format(periodo[2], "%Y%m%d")
    )
  }
  DBI::dbExecute(
    selezione$archivio$con,
    sprintf(
      paste(
        "CREATE TEMP TABLE IF NOT EXISTS %s AS",
        "WITH conteggi AS (SELECT p.RFID, p.servizio_atteso AS servizio, coalesce(t.tipologia, p.servizio_atteso) AS tipologia, count(*) AS n",
        "FROM %s p LEFT JOIN servizi_tipologie t ON t.servizio = p.servizio_atteso",
        "WHERE %s AND p.presente_a_database <> 'PIA' AND p.servizio_atteso IS NOT NULL GROUP BY ALL),",
        "quote AS (SELECT *, sum(n) OVER (PARTITION BY RFID) AS totale, sum(n) OVER (PARTITION BY RFID, tipologia) AS n_tipologia FROM conteggi)",
        "SELECT RFID, CASE WHEN n_tipologia / totale >= %s THEN servizio END AS stimato FROM quote",
        "QUALIFY row_number() OVER (PARTITION BY RFID ORDER BY n_tipologia DESC, tipologia, n DESC, servizio) = 1"
      ),
      nome,
      selezione$archivio$tabella,
      sql_periodo(selezione),
      format(soglia, scientific = FALSE)
    )
  )
  nome
}

#' Sottoquery delle letture del periodo con `servizio_icona`
#' Contesto: unisce le letture alla tabella della stima, così filtri, icona e
#' statistiche dicono lo stesso servizio. Regola di riferimento in R:
#' `aggiungi_servizio_icona()`.
#' Risultato: una sottoquery SQL da usare in `FROM`.
#' @noRd
sql_con_icona <- function(selezione) {
  sprintf(
    paste(
      "(SELECT p.*, CASE WHEN %s THEN %s WHEN p.presente_a_database = 'PIA' THEN p.servizio_transponder ELSE s.stimato END AS servizio_icona",
      "FROM %s p LEFT JOIN %s s ON s.RFID = p.RFID WHERE %s)"
    ),
    sql_sacchetto("p.RFID"),
    DBI::dbQuoteString(selezione$archivio$con, servizio_sacchetti()),
    selezione$archivio$tabella,
    selezione$stima,
    sql_periodo(selezione)
  )
}

#' SQL delle righe dell'ultima lettura di ogni RFID che soddisfa una condizione
#' Contesto: la mappa principale mostra un marker per bidone, nell'ultima lettura.
#' Funzionamento: `arg_max()` con `GROUP BY`, più veloce di una funzione finestra;
#' a parità di istante vale l'ultima riga del file.
#' Risultato: una query SQL, una riga per RFID.
#' @noRd
sql_ultime <- function(selezione, condizione = "TRUE") {
  sprintf(
    "(SELECT arg_max(riga, {'g': data_ora_lettura, 'r': -riga}) AS riga FROM %s WHERE %s GROUP BY RFID)",
    sql_con_icona(selezione),
    condizione
  )
}

#' Colonne restituite dalle interrogazioni sulle letture
#' Contesto: porta in R solo ciò che serve alle mappe e ai popup; con `icona`
#' anche `servizio_icona`.
#' @noRd
colonne_risultato <- function(icona = TRUE) {
  paste(c(colonne_app(), if (icona) "servizio_icona"), collapse = ", ")
}

#' Condizione SQL dei filtri della mappa principale, sulle letture di `sql_con_icona()`
#'
#' Contesto: i filtri sopra la mappa vanno applicati a tutte le letture del
#' periodo, prima di tenere l'ultima per RFID: un bidone letto due volte in
#' servizi diversi compare in base all'ultima lettura che passa i filtri.
#' Funzionamento: servizi, giri, cantieri e coerenze per esclusione (un mancante
#' non è mai escluso), comuni e stato a database per scelta. I non censiti con la
#' stima incerta passano con `includi_stima_incerta`; una coerenza del servizio
#' mancante sta con le coerenti.
#' Risultato: un testo SQL per il `WHERE`.
#' @param comuni Comuni da tenere; vuoto: tutti.
#' @noRd
condizione_filtri <- function(
  con,
  stato_db = stati_database(),
  servizi_esclusi = character(0),
  includi_stima_incerta = TRUE,
  giri_esclusi = character(0),
  coerenza_escluse = character(0),
  cantieri_esclusi = character(0),
  comuni = character(0),
  coerenza_comune_escluse = character(0)
) {
  # `NOT coalesce(x IN (...), FALSE)`: un valore mancante non è escluso.
  escluso <- function(colonna, valori) {
    valori <- valori[!is.na(valori)]
    if (length(valori) == 0) {
      return(NULL)
    }
    sprintf("NOT coalesce(%s IN (%s), FALSE)", colonna, sql_elenco(con, valori))
  }
  condizioni <- c(
    if (length(stato_db) == 0) {
      "FALSE"
    } else {
      sprintf("presente_a_database IN (%s)", sql_elenco(con, stato_db))
    },
    escluso("servizio_icona", servizi_esclusi),
    if (!isTRUE(includi_stima_incerta)) {
      "NOT (presente_a_database <> 'PIA' AND servizio_icona IS NULL)"
    },
    escluso("servizio_giro", giri_esclusi)
  )
  stati <- stati_coerenza()
  coerenza_escluse <- intersect(coerenza_escluse, names(stati))
  non_coerente <- sprintf(
    "coalesce(coerenza_servizio = %s, FALSE)",
    DBI::dbQuoteString(con, stati[["non_coerente"]])
  )
  if ("coerente" %in% coerenza_escluse) {
    condizioni <- c(condizioni, non_coerente)
  }
  if ("non_coerente" %in% coerenza_escluse) {
    condizioni <- c(condizioni, paste("NOT", non_coerente))
  }
  if (length(cantieri_esclusi) > 0) {
    condizioni <- c(
      condizioni,
      sprintf(
        "coalesce(cantiere, %s) NOT IN (%s)",
        DBI::dbQuoteString(con, senza_cantiere()),
        sql_elenco(con, cantieri_esclusi)
      )
    )
  }
  comuni <- comuni[!is.na(comuni) & nzchar(comuni)]
  if (length(comuni) > 0) {
    condizioni <- c(
      condizioni,
      sprintf("comune_assegnato IN (%s)", sql_elenco(con, comuni))
    )
  }
  voci <- stati_coerenza_comune()
  coerenza_comune_escluse <- intersect(coerenza_comune_escluse, names(voci))
  condizioni <- c(
    condizioni,
    escluso("coerenza_comune", voci[coerenza_comune_escluse])
  )
  paste0("(", condizioni, ")", collapse = " AND ")
}

#' Ultima lettura di ogni RFID tra quelle che superano i filtri
#' Contesto: i dati della mappa principale.
#' Funzionamento: prima i filtri su tutte le letture del periodo, poi l'ultima per
#' RFID (`sql_ultime()`).
#' Risultato: un tibble con una riga per RFID e la colonna `riga`.
#' @param ... Filtri di `condizione_filtri()`.
#' @return Una riga per RFID, con la colonna `riga`.
#' @noRd
ultime_letture <- function(selezione, ..., colonne = NULL) {
  colonne <- unique(c(colonne %||% c(colonne_app(), "servizio_icona"), "riga"))
  interroga(
    selezione$archivio,
    sprintf(
      "SELECT %s FROM %s l SEMI JOIN %s u ON u.riga = l.riga ORDER BY data_ora_lettura DESC, riga",
      paste(colonne, collapse = ", "),
      sql_con_icona(selezione),
      sql_ultime(selezione, condizione_filtri(selezione$archivio$con, ...))
    )
  )
}

#' Numero delle letture del periodo che superano i filtri
#' Contesto: l'intestazione della mappa dice «N letture di M RFID».
#' @inheritParams ultime_letture
#' @noRd
conta_letture <- function(selezione, ...) {
  as.integer(
    interroga(
      selezione$archivio,
      sprintf(
        "SELECT count(*) AS n FROM %s WHERE %s",
        sql_con_icona(selezione),
        condizione_filtri(selezione$archivio$con, ...)
      )
    )$n
  )
}

#' Letture per cantiere che superano i filtri, nell'ordine di `ordina_cantieri()`
#' Contesto: statistiche della barra laterale.
#' @inheritParams ultime_letture
#' @noRd
conteggi_cantieri <- function(selezione, ...) {
  conteggi <- interroga(
    selezione$archivio,
    sprintf(
      "SELECT cantiere, count(*) AS n FROM %s WHERE %s GROUP BY cantiere",
      sql_con_icona(selezione),
      condizione_filtri(selezione$archivio$con, ...)
    )
  )
  if (nrow(conteggi) == 0 || all(is.na(conteggi$cantiere))) {
    return(stats::setNames(integer(0), character(0)))
  }
  ordine <- ordina_cantieri(conteggi$cantiere)
  nomi <- dplyr::coalesce(conteggi$cantiere, senza_cantiere())
  stats::setNames(as.integer(conteggi$n[match(ordine, nomi)]), ordine)
}

#' Voci dei filtri presenti nel periodo
#' Contesto: i menu mostrano solo servizi, giri, cantieri e comuni che compaiono
#' nelle letture del periodo.
#' Risultato: una lista con le voci e `con_geografia` (se c'è almeno un cantiere).
#' @return Lista con `servizi`, `giri`, `cantieri`, `comuni` (con
#'   `cantiere` e `comune_assegnato`) e `con_geografia`.
#' @noRd
scelte_periodo <- function(selezione) {
  servizi <- interroga(
    selezione$archivio,
    sprintf("SELECT DISTINCT servizio_icona FROM %s", sql_con_icona(selezione))
  )$servizio_icona
  periodo <- sprintf(
    "FROM %s WHERE %s",
    selezione$archivio$tabella,
    sql_periodo(selezione)
  )
  giri <- interroga(
    selezione$archivio,
    paste("SELECT DISTINCT servizio_giro", periodo)
  )$servizio_giro
  comuni <- interroga(
    selezione$archivio,
    paste("SELECT DISTINCT cantiere, comune_assegnato", periodo)
  )
  list(
    servizi = ordina_servizi(servizi),
    giri = ordina_servizi(giri),
    cantieri = ordina_cantieri(comuni$cantiere),
    comuni = comuni[!is.na(comuni$comune_assegnato), , drop = FALSE],
    con_geografia = any(!is.na(comuni$cantiere))
  )
}

#' Riquadro delle letture dei cantieri e comuni scelti: due punti, o zero righe
#' Contesto: la mappa si sposta sull'area scelta nei filtri.
#' @noRd
area_filtri <- function(
  selezione,
  cantieri_esclusi = character(0),
  comuni = character(0)
) {
  condizione <- condizione_filtri(
    selezione$archivio$con,
    cantieri_esclusi = cantieri_esclusi,
    comuni = comuni
  )
  angoli <- interroga(
    selezione$archivio,
    sprintf(
      paste(
        "SELECT min(latitudine) AS lat_min, max(latitudine) AS lat_max,",
        "min(longitudine) AS lng_min, max(longitudine) AS lng_max",
        "FROM %s WHERE %s AND %s"
      ),
      selezione$archivio$tabella,
      sql_periodo(selezione),
      condizione
    )
  )
  if (is.na(angoli$lat_min)) {
    return(dplyr::tibble(latitudine = numeric(0), longitudine = numeric(0)))
  }
  dplyr::tibble(
    latitudine = c(angoli$lat_min, angoli$lat_max),
    longitudine = c(angoli$lng_min, angoli$lng_max)
  )
}

#' Codici di ricerca in maiuscolo, pronti per un confronto SQL
#' @noRd
sql_codici <- function(con, codici) {
  sql_elenco(con, unique(stringr::str_to_upper(codici)))
}

#' Letture degli RFID cercati, senza badare alle maiuscole
#' Contesto: scheda «Ricerca RFID».
#' Risultato: per RFID e data decrescente, con `servizio_icona` e `is_ultimo`.
#' @return Per RFID e data decrescente, con `servizio_icona` e `is_ultimo`.
#' @noRd
cerca_per_rfid <- function(selezione, codici) {
  interroga(
    selezione$archivio,
    sprintf(
      paste(
        "SELECT %s, row_number() OVER (PARTITION BY RFID ORDER BY data_ora_lettura DESC, riga) = 1 AS is_ultimo",
        "FROM %s WHERE upper(RFID) IN (%s)",
        "ORDER BY RFID, data_ora_lettura DESC, riga"
      ),
      colonne_risultato(),
      sql_con_icona(selezione),
      sql_codici(selezione$archivio$con, codici)
    )
  )
}

#' Ultima lettura dei bidoni la cui utenza attuale è tra quelle cercate
#' Contesto: scheda «Ricerca Utenza»; un bidone che ha cambiato utenza compare
#' solo per l'utenza dell'ultima lettura.
#' @noRd
filtra_ultimo_per_utenza <- function(selezione, utenze) {
  interroga(
    selezione$archivio,
    sprintf(
      paste(
        "SELECT %s FROM %s l SEMI JOIN %s u ON u.riga = l.riga",
        "WHERE upper(id_utenza) IN (%s) ORDER BY id_utenza, RFID"
      ),
      colonne_risultato(),
      sql_con_icona(selezione),
      sql_ultime(selezione),
      sql_codici(selezione$archivio$con, utenze)
    )
  )
}

#' Matricole dei mezzi, con etichette "matricola (targa)" per l'elenco
#' Contesto: scheda «Ricerca Giro».
#' @noRd
mezzi_presenti <- function(selezione) {
  if (is.null(selezione) || selezione$n == 0) {
    return(character(0))
  }
  mezzi <- interroga(
    selezione$archivio,
    sprintf(
      paste(
        "SELECT matricola_veicolo, arg_min(targa_veicolo, riga) AS targa_veicolo",
        "FROM %s WHERE %s GROUP BY matricola_veicolo ORDER BY matricola_veicolo"
      ),
      selezione$archivio$tabella,
      sql_periodo(selezione)
    )
  )
  stats::setNames(
    as.character(mezzi$matricola_veicolo),
    paste(mezzi$matricola_veicolo, "\u00b7", mezzi$targa_veicolo)
  )
}

#' Primo e ultimo giorno con letture, o `NULL`
#' @noRd
giorni_letture <- function(selezione) {
  if (is.null(selezione) || selezione$n == 0) {
    return(NULL)
  }
  estremi <- interroga(
    selezione$archivio,
    sprintf(
      "SELECT min(data_ora_lettura)::DATE AS primo, max(data_ora_lettura)::DATE AS ultimo FROM %s WHERE %s",
      selezione$archivio$tabella,
      sql_periodo(selezione)
    )
  )
  c(estremi$primo, estremi$ultimo)
}

#' Letture dei mezzi in un intervallo di giorni, per la ricerca dei giri
#' Contesto: la tabella dei giri e la mappa di «Ricerca Giro» partono da qui.
#' @param mezzo Matricola; tutti se vuota.
#' @param solo Tra `"fuori_giro"` e `"fuori_comune"`: tiene le letture con
#'   almeno una delle due; vuoto: tutte.
#' @noRd
cerca_giri <- function(selezione, mezzo = NULL, dal, al, solo = character(0)) {
  con <- selezione$archivio$con
  dal <- lubridate::as_date(dal)
  al <- lubridate::as_date(al)
  condizioni <- sprintf(
    "data_ora_lettura >= TIMESTAMP '%s 00:00:00' AND data_ora_lettura < TIMESTAMP '%s 00:00:00'",
    format(dal),
    format(al + 1)
  )
  if (!is.null(mezzo) && length(mezzo) == 1 && !is.na(mezzo) && nzchar(mezzo)) {
    condizioni <- c(
      condizioni,
      sprintf("matricola_veicolo = %s", DBI::dbQuoteString(con, mezzo))
    )
  }
  voci <- stati_coerenza_comune()[intersect(
    solo,
    c("fuori_giro", "fuori_comune")
  )]
  if (length(voci) > 0) {
    condizioni <- c(
      condizioni,
      sprintf("coerenza_comune IN (%s)", sql_elenco(con, voci))
    )
  }
  trovate <- interroga(
    selezione$archivio,
    sprintf(
      "SELECT %s FROM %s WHERE %s ORDER BY data_ora_lettura, RFID, riga",
      colonne_risultato(),
      sql_con_icona(selezione),
      paste0("(", condizioni, ")", collapse = " AND ")
    )
  )
  trovate$data_lettura <- lubridate::as_date(trovate$data_ora_lettura)
  trovate
}

#' Letture di un RFID, in ordine di tempo
#' Contesto: dettaglio del bidone.
#' @param origine Selezione (letture del periodo) o archivio (tutte).
#' @noRd
letture_del_rfid <- function(origine, rfid) {
  if (inherits(origine, "selezione_letture")) {
    archivio <- origine$archivio
    periodo <- sql_periodo(origine)
  } else {
    archivio <- origine
    periodo <- "TRUE"
  }
  interroga(
    archivio,
    sprintf(
      "SELECT %s FROM %s WHERE RFID = %s AND %s ORDER BY data_ora_lettura, riga",
      colonne_risultato(icona = FALSE),
      archivio$tabella,
      DBI::dbQuoteString(archivio$con, rfid),
      periodo
    )
  )
}

#' Tutte le letture di una selezione, per l'analisi dei cluster
#' Contesto: l'unico caso in cui l'app porta in R il periodo intero (al click di
#' «Genera Analisi»).
#' @noRd
letture_periodo <- function(selezione) {
  letture_archivio(selezione$archivio, sql_periodo(selezione), colonne_app())
}

#' Anni presenti nelle letture di un archivio, dal più recente
#' @noRd
anni_disponibili <- function(archivio) {
  if (is.null(archivio) || archivio$n == 0) {
    return(numeric(0))
  }
  as.numeric(
    interroga(
      archivio,
      sprintf(
        "SELECT DISTINCT year(data_ora_lettura) AS anno FROM %s ORDER BY anno DESC",
        archivio$tabella
      )
    )$anno
  )
}
