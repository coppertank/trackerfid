# Selezioni dell'archivio: le letture di un periodo, filtrate, cercate.
#
# Il periodo di analisi non copia le letture: è una selezione dell'archivio,
# cioè l'archivio più la condizione SQL sul periodo. Filtri, ricerche, giri e
# dettaglio interrogano DuckDB e ricevono in R solo le righe che servono:
# l'ultima lettura di ogni bidone, le letture degli RFID cercati, quelle di un
# giorno. Le regole sono quelle descritte nelle funzioni in R che lavorano sui
# data frame: `aggiungi_servizio_icona()`, `deduplica_ultimo_rfid()` e le
# altre citate qui sotto.

#' Seleziona le letture di un periodo
#'
#' @param archivio Archivio delle letture, vedi `nuovo_archivio()`.
#' @param periodo Vettore di due date, estremi inclusi, oppure `NULL` per
#'   tutte le letture.
#' @return Lista di classe `selezione_letture` con `archivio`, `periodo`,
#'   `n` (letture) e `n_rfid`.
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
#' @noRd
sql_periodo <- function(selezione) {
  periodo <- selezione$periodo
  if (is.null(periodo)) {
    return("TRUE")
  }
  sprintf(
    "giorno_lettura >= TIMESTAMP '%s 00:00:00' AND giorno_lettura < TIMESTAMP '%s 00:00:00'",
    format(periodo[1]),
    format(periodo[2] + 1)
  )
}

#' Condizione SQL che riconosce i sacchetti, come `rfid_sacchetto()`
#' @noRd
sql_sacchetto <- function(colonna = "RFID") {
  sprintf("(length(%s) = 24 AND upper(substr(%s, 1, 4)) = '00BD')", colonna, colonna)
}

#' Tabella del servizio stimato dei non censiti in un periodo
#'
#' La stima segue `servizio_prevalente_non_censiti()`: per ogni RFID le sue
#' letture "Non Presente" del periodo con un servizio atteso; la tipologia più
#' letta, se raggiunge la soglia, e al suo interno il servizio atteso più
#' letto. Gli RFID sotto la soglia hanno la stima mancante.
#'
#' La tabella si calcola una volta per periodo e resta nel database finché
#' l'archivio è aperto: la usano tutte le interrogazioni del periodo, e tutte
#' le sessioni che guardano lo stesso periodo dello stesso archivio.
#'
#' @param selezione Selezione delle letture.
#' @param soglia Quota minima della tipologia più letta.
#' @return Il nome della tabella, con `RFID` e `stimato`.
#' @noRd
tabella_stima <- function(selezione, soglia = 0.8) {
  periodo <- selezione$periodo
  nome <- if (is.null(periodo)) {
    "stima_tutto"
  } else {
    paste0("stima_", format(periodo[1], "%Y%m%d"), "_", format(periodo[2], "%Y%m%d"))
  }
  DBI::dbExecute(
    selezione$archivio$con,
    sprintf(
      paste(
        "CREATE TEMP TABLE IF NOT EXISTS %s AS",
        "WITH conteggi AS (SELECT p.RFID, p.servizio_atteso AS servizio, coalesce(t.tipologia, p.servizio_atteso) AS tipologia, count(*) AS n",
        "FROM %s p LEFT JOIN servizi_tipologie t ON t.servizio = p.servizio_atteso",
        "WHERE %s AND p.presente_a_database = 'Non Presente' AND p.servizio_atteso IS NOT NULL GROUP BY ALL),",
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

#' Letture di una selezione con il servizio dell'icona
#'
#' Una sottoquery con le letture del periodo e la colonna `servizio_icona`,
#' calcolata come `aggiungi_servizio_icona()`: per un censito il servizio a
#' database, per un non censito il servizio stimato dalle sue letture del
#' periodo, vedi `tabella_stima()`, per un sacchetto il servizio dei
#' sacchetti.
#'
#' @param selezione Selezione delle letture.
#' @return Testo SQL, da usare tra parentesi in un `FROM`.
#' @noRd
sql_con_icona <- function(selezione) {
  sprintf(
    paste(
      "(SELECT p.*, CASE WHEN %s THEN %s WHEN p.presente_a_database = 'Presente' THEN p.servizio_transponder ELSE s.stimato END AS servizio_icona",
      "FROM %s p LEFT JOIN %s s ON s.RFID = p.RFID WHERE %s)"
    ),
    sql_sacchetto("p.RFID"),
    DBI::dbQuoteString(selezione$archivio$con, servizio_sacchetti()),
    selezione$archivio$tabella,
    selezione$stima,
    sql_periodo(selezione)
  )
}

#' Righe dell'ultima lettura di ogni RFID tra quelle che soddisfano una condizione
#'
#' A parità di istante resta la lettura che viene prima nel file, come in
#' `deduplica_ultimo_rfid()`. `arg_max()` è molto più veloce di una funzione
#' finestra su milioni di letture.
#'
#' @param selezione Selezione delle letture.
#' @param condizione Condizione SQL sulle letture di `sql_con_icona()`.
#' @return Testo SQL che dà la colonna `riga`.
#' @noRd
sql_ultime <- function(selezione, condizione = "TRUE") {
  sprintf(
    "(SELECT arg_max(riga, {'g': giorno_lettura, 'r': -riga}) AS riga FROM %s WHERE %s GROUP BY RFID)",
    sql_con_icona(selezione),
    condizione
  )
}

#' Colonne restituite dalle interrogazioni sulle letture
#' @noRd
colonne_risultato <- function(icona = TRUE) {
  paste(c(colonne_dataset(), if (icona) "servizio_icona"), collapse = ", ")
}

#' Condizione SQL dei filtri della mappa principale
#'
#' Le regole sono quelle dei filtri laterali: i servizi, i giri, i cantieri e
#' le voci delle due coerenze si filtrano per esclusione, i comuni per scelta,
#' lo stato a database per scelta. Un valore mancante non è mai tra quelli
#' esclusi. Gli RFID non censiti con la stima incerta passano solo con
#' `includi_stima_incerta`. Una lettura di cui la coerenza del servizio non si
#' può dire sta con quelle coerenti; le letture senza cantiere corrispondono
#' alla voce `senza_cantiere()`; passano le letture il cui comune assegnato è
#' tra quelli scelti.
#'
#' La condizione va applicata alle letture di `sql_con_icona()`, che hanno il
#' servizio dell'icona.
#'
#' @param con Connessione, per le virgolette dei testi.
#' @param stato_db Valori di `presente_a_database` da mantenere.
#' @param servizi_esclusi Servizi deselezionati.
#' @param includi_stima_incerta Mantiene i non censiti senza servizio stimato.
#' @param giri_esclusi Giri deselezionati, con il nome del calendario.
#' @param coerenza_escluse Voci di `stati_coerenza()` deselezionate.
#' @param cantieri_esclusi Cantieri deselezionati.
#' @param comuni Comuni da mantenere. Tutti, se il vettore è vuoto.
#' @param coerenza_comune_escluse Voci di `stati_coerenza_comune()`
#'   deselezionate.
#' @return Testo SQL.
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
      "NOT (presente_a_database = 'Non Presente' AND servizio_icona IS NULL)"
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
#'
#' Come `deduplica_ultimo_rfid()` dopo i filtri: prima i filtri su tutte le
#' letture del periodo, poi l'ultima per RFID. A parità di istante resta la
#' lettura che viene prima nel file.
#'
#' @param selezione Selezione delle letture.
#' @param ... Filtri, vedi `condizione_filtri()`.
#' @param colonne Colonne da restituire. Tutte quelle di `colonne_dataset()`
#'   e `servizio_icona`, se manca.
#' @return Tibble con una riga per RFID, dalla lettura più recente. Ha sempre
#'   anche la colonna `riga`, la posizione della lettura nel file.
#' @noRd
ultime_letture <- function(selezione, ..., colonne = NULL) {
  colonne <- unique(c(colonne %||% c(colonne_dataset(), "servizio_icona"), "riga"))
  interroga(
    selezione$archivio,
    sprintf(
      "SELECT %s FROM %s l SEMI JOIN %s u ON u.riga = l.riga ORDER BY giorno_lettura DESC, riga",
      paste(colonne, collapse = ", "),
      sql_con_icona(selezione),
      sql_ultime(selezione, condizione_filtri(selezione$archivio$con, ...))
    )
  )
}

#' Numero delle letture che superano i filtri
#'
#' Le letture del periodo, non le ultime: è il numero che l'intestazione della
#' mappa principale riporta accanto agli RFID.
#'
#' @inheritParams ultime_letture
#' @return Un intero.
#' @noRd
conta_letture <- function(selezione, ...) {
  as.integer(interroga(
    selezione$archivio,
    sprintf(
      "SELECT count(*) AS n FROM %s WHERE %s",
      sql_con_icona(selezione),
      condizione_filtri(selezione$archivio$con, ...)
    )
  )$n)
}

#' Letture per cantiere tra quelle che superano i filtri
#'
#' @inheritParams ultime_letture
#' @return Vettore intero con nome, nell'ordine di `ordina_cantieri()`: le
#'   letture senza cantiere stanno sotto `senza_cantiere()`. Vuoto se nessuna
#'   lettura ha un cantiere.
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

#' Valori presenti nel periodo, per le voci dei filtri
#'
#' @param selezione Selezione delle letture.
#' @return Lista con `servizi` (servizi dell'icona, in ordine di
#'   `ordina_servizi()`), `giri` (giri del calendario), `cantieri` (in ordine
#'   di `ordina_cantieri()`), `comuni` (data frame con `cantiere` e
#'   `comune_assegnato`) e `con_geografia` (`TRUE` se almeno una lettura ha
#'   un cantiere).
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

#' Riquadro delle letture dei cantieri e dei comuni scelti
#'
#' È l'area su cui la mappa principale si inquadra: cambia con cantieri e
#' comuni, non con gli altri filtri.
#'
#' @param selezione Selezione delle letture.
#' @param cantieri_esclusi,comuni Vedi `condizione_filtri()`.
#' @return Data frame con due punti, gli angoli del riquadro, con
#'   `latitudine` e `longitudine`; zero righe senza letture.
#' @noRd
area_filtri <- function(selezione, cantieri_esclusi = character(0), comuni = character(0)) {
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

#' Tutte le letture degli RFID cercati
#'
#' Il confronto dei codici non bada a maiuscole e minuscole, come
#' `codice_in()`.
#'
#' @param selezione Selezione delle letture.
#' @param codici RFID cercati.
#' @return Tibble con le letture trovate, ordinate per RFID e data
#'   decrescente, con `servizio_icona` e `is_ultimo`, che marca la lettura
#'   più recente di ogni RFID.
#' @noRd
cerca_per_rfid <- function(selezione, codici) {
  interroga(
    selezione$archivio,
    sprintf(
      paste(
        "SELECT %s, row_number() OVER (PARTITION BY RFID ORDER BY giorno_lettura DESC, riga) = 1 AS is_ultimo",
        "FROM %s WHERE upper(RFID) IN (%s)",
        "ORDER BY RFID, giorno_lettura DESC, riga"
      ),
      colonne_risultato(),
      sql_con_icona(selezione),
      sql_codici(selezione$archivio$con, codici)
    )
  )
}

#' Ultima lettura dei bidoni la cui utenza attuale è tra quelle cercate
#'
#' Un contenitore passato da un'utenza a un'altra compare solo cercando
#' l'utenza attuale: conta l'utenza dell'ultima lettura del periodo.
#'
#' @param selezione Selezione delle letture.
#' @param utenze Utenze cercate.
#' @return Tibble con l'ultima lettura di ogni RFID trovato, ordinato per
#'   utenza e RFID.
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

#' Mezzi presenti nelle letture di una selezione
#'
#' @param selezione Selezione delle letture, oppure `NULL`.
#' @return Vettore delle matricole, in ordine: i nomi sono le etichette per
#'   l'elenco, con matricola e targa. Per ogni matricola vale la targa della
#'   prima lettura del file.
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

#' Primo e ultimo giorno con letture in una selezione
#'
#' @param selezione Selezione delle letture, oppure `NULL`.
#' @return Vettore di due date, `NULL` senza letture.
#' @noRd
giorni_letture <- function(selezione) {
  if (is.null(selezione) || selezione$n == 0) {
    return(NULL)
  }
  estremi <- interroga(
    selezione$archivio,
    sprintf(
      "SELECT min(giorno_lettura)::DATE AS primo, max(giorno_lettura)::DATE AS ultimo FROM %s WHERE %s",
      selezione$archivio$tabella,
      sql_periodo(selezione)
    )
  )
  c(estremi$primo, estremi$ultimo)
}

#' Letture dei mezzi in un intervallo di giorni
#'
#' @param selezione Selezione delle letture.
#' @param mezzo Matricola del mezzo. Tutti i mezzi, se manca o è vuota.
#' @param dal,al Primo e ultimo giorno, inclusi.
#' @param solo Voci tra `"fuori_giro"` e `"fuori_comune"`: restano le sole
#'   letture con almeno una delle incongruenze indicate, vedi
#'   `incongruenze_comune()`. Tutte, se è vuoto.
#' @return Tibble con le letture trovate, in ordine di tempo, con
#'   `servizio_icona` e la colonna `giorno`.
#' @noRd
cerca_giri <- function(selezione, mezzo = NULL, dal, al, solo = character(0)) {
  con <- selezione$archivio$con
  dal <- lubridate::as_date(dal)
  al <- lubridate::as_date(al)
  condizioni <- sprintf(
    "giorno_lettura >= TIMESTAMP '%s 00:00:00' AND giorno_lettura < TIMESTAMP '%s 00:00:00'",
    format(dal),
    format(al + 1)
  )
  if (!is.null(mezzo) && length(mezzo) == 1 && !is.na(mezzo) && nzchar(mezzo)) {
    condizioni <- c(
      condizioni,
      sprintf("matricola_veicolo = %s", DBI::dbQuoteString(con, mezzo))
    )
  }
  voci <- stati_coerenza_comune()[intersect(solo, c("fuori_giro", "fuori_comune"))]
  if (length(voci) > 0) {
    condizioni <- c(
      condizioni,
      sprintf("coerenza_comune IN (%s)", sql_elenco(con, voci))
    )
  }
  trovate <- interroga(
    selezione$archivio,
    sprintf(
      "SELECT %s FROM %s WHERE %s ORDER BY giorno_lettura, RFID, riga",
      colonne_risultato(),
      sql_con_icona(selezione),
      paste0("(", condizioni, ")", collapse = " AND ")
    )
  )
  trovate$giorno <- lubridate::as_date(trovate$giorno_lettura)
  trovate
}

#' Letture di un RFID
#'
#' @param origine Selezione delle letture, per le sole letture del periodo,
#'   oppure archivio, per tutte.
#' @param rfid Codice RFID, esatto.
#' @return Tibble con le letture, in ordine di tempo.
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
      "SELECT %s FROM %s WHERE RFID = %s AND %s ORDER BY giorno_lettura, riga",
      colonne_risultato(icona = FALSE),
      archivio$tabella,
      DBI::dbQuoteString(archivio$con, rfid),
      periodo
    )
  )
}

#' Letture di una selezione, per l'analisi dei cluster
#'
#' @param selezione Selezione delle letture.
#' @return Tibble con le letture del periodo, nell'ordine del file.
#' @noRd
letture_periodo <- function(selezione) {
  letture_archivio(selezione$archivio, sql_periodo(selezione))
}

#' Anni presenti nelle letture di un archivio, dal più recente
#' @noRd
anni_disponibili <- function(archivio) {
  if (is.null(archivio) || archivio$n == 0) {
    return(numeric(0))
  }
  as.numeric(interroga(
    archivio,
    sprintf(
      "SELECT DISTINCT year(giorno_lettura) AS anno FROM %s ORDER BY anno DESC",
      archivio$tabella
    )
  )$anno)
}
