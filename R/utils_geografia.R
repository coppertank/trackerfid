# Geografia del territorio servito: comuni, cantieri e confini.
#
# La tabella dei comuni sta in `inst/extdata/comuni_cantieri.csv`: è l'unico
# elenco di comuni e cantieri. I confini stanno in
# `inst/extdata/comuni_confini.geojson`, un comune per ogni riga della
# tabella: li scarica `scarica_confini_comuni()`, in
# `R/fct_confini_comuni.R`.

# Tabelle lette una sola volta per sessione di R.
memoria_geografia <- new.env(parent = emptyenv())

#' Comuni serviti, con cantiere e coordinate del centro abitato
#'
#' @return Tibble con `comune`, `cantiere`, `latitudine`, `longitudine`.
#' @noRd
comuni_cantieri <- function() {
  if (is.null(memoria_geografia$comuni)) {
    tabella <- utils::read.csv(
      app_sys("extdata", "comuni_cantieri.csv"),
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
    memoria_geografia$comuni <- dplyr::as_tibble(tabella)
  }
  memoria_geografia$comuni
}

#' Cantieri, nell'ordine in cui compaiono in filtri, tabelle e grafici
#' @noRd
cantieri <- function() sort(unique(comuni_cantieri()$cantiere))

#' Percorso del file con i confini dei comuni serviti
#' @noRd
percorso_confini_comuni <- function() {
  app_sys("extdata", "comuni_confini.geojson")
}

#' Vertici di una geometria GeoJSON
#'
#' Un comune può avere più poligoni, ad esempio un'isola amministrativa, e
#' ogni poligono può avere dei fori: il primo anello è il contorno, gli altri
#' sono fori.
#'
#' @param geometria Geometria GeoJSON di tipo `Polygon` o `MultiPolygon`, letta
#'   con `jsonlite::fromJSON(simplifyVector = FALSE)`.
#' @return Tibble con una riga per vertice: `poligono`, `anello`, `punto`,
#'   `longitudine`, `latitudine`.
#' @noRd
anelli_geometria <- function(geometria) {
  poligoni <- switch(
    geometria$type %||% "",
    Polygon = list(geometria$coordinates),
    MultiPolygon = geometria$coordinates,
    stop("Geometria non prevista nei confini dei comuni: ", geometria$type)
  )
  parti <- list()
  for (p in seq_along(poligoni)) {
    for (a in seq_along(poligoni[[p]])) {
      anello <- poligoni[[p]][[a]]
      # Ogni punto è una coppia longitudine, latitudine; a volte segue la quota.
      valori <- unlist(anello, use.names = FALSE)
      n <- length(anello)
      passo <- length(valori) / n
      parti[[length(parti) + 1]] <- dplyr::tibble(
        poligono = p,
        anello = a,
        punto = seq_len(n),
        longitudine = valori[seq(1, by = passo, length.out = n)],
        latitudine = valori[seq(2, by = passo, length.out = n)]
      )
    }
  }
  dplyr::bind_rows(parti)
}

#' Vertici dei confini di un file GeoJSON di comuni
#'
#' @param geojson `FeatureCollection` in cui ogni comune ha le proprietà
#'   `comune` e `cantiere`, come quella scritta da `scarica_confini_comuni()`.
#' @return Tibble con una riga per vertice: `comune`, `cantiere`, `poligono`
#'   (identificativo unico di ogni poligono), `anello`, `punto`,
#'   `longitudine`, `latitudine`.
#' @noRd
anelli_geojson <- function(geojson) {
  parti <- lapply(geojson$features, function(elemento) {
    anelli <- anelli_geometria(elemento$geometry)
    comune <- as.character(elemento$properties$comune %||% NA)
    dplyr::tibble(
      comune = comune,
      cantiere = as.character(elemento$properties$cantiere %||% NA),
      poligono = paste(comune, anelli$poligono),
      anello = anelli$anello,
      punto = anelli$punto,
      longitudine = anelli$longitudine,
      latitudine = anelli$latitudine
    )
  })
  dplyr::bind_rows(parti)
}

#' Confini dei comuni serviti
#'
#' Fonte: ISTAT, dalla raccolta `guglielmo/geojson-italy`, licenza CC-BY 4.0.
#' Per disegnarli con `ggplot2::geom_polygon()` servono `group = poligono` e
#' `subgroup = anello`.
#'
#' @return Tibble con una riga per vertice, vedi `anelli_geojson()`. Gli
#'   attributi `origine` e `versione` dicono da dove vengono i confini.
#' @noRd
poligoni_comuni <- function() {
  if (is.null(memoria_geografia$poligoni)) {
    geojson <- jsonlite::fromJSON(
      percorso_confini_comuni(),
      simplifyVector = FALSE
    )
    memoria_geografia$poligoni <- structure(
      anelli_geojson(geojson),
      origine = geojson$origine,
      versione = geojson$versione
    )
  }
  memoria_geografia$poligoni
}

#' Il punto è dentro i poligoni?
#'
#' Conta i lati attraversati da una semiretta che parte dal punto: un numero
#' dispari vuol dire dentro. Vale anche con più poligoni e con i fori, perché
#' un punto dentro un foro attraversa due contorni.
#'
#' @param lng,lat Coordinate di un punto.
#' @param poligoni Vertici di uno o più poligoni, con `poligono`, `anello`,
#'   `longitudine` e `latitudine`: i vertici di ogni anello stanno di seguito.
#' @return `TRUE` oppure `FALSE`.
#' @noRd
dentro_poligoni <- function(lng, lat, poligoni) {
  x <- poligoni$longitudine
  y <- poligoni$latitudine
  if (length(x) == 0) {
    return(FALSE)
  }
  anello <- paste(poligoni$poligono, poligoni$anello)
  # Il vertice che precede il primo di un anello è l'ultimo dello stesso anello.
  precedente <- seq_along(x) - 1L
  precedente[!duplicated(anello)] <- which(!duplicated(anello, fromLast = TRUE))
  attraversa <- ((y > lat) != (y[precedente] > lat)) &
    (lng < (x[precedente] - x) * (lat - y) / (y[precedente] - y) + x)
  sum(attraversa) %% 2 == 1
}

#' Riquadro che contiene tutta la zona servita
#'
#' @return Lista con `lat_min`, `lat_max`, `lng_min`, `lng_max`.
#' @noRd
riquadro_zona <- function() {
  if (is.null(memoria_geografia$zona)) {
    poligoni <- poligoni_comuni()
    memoria_geografia$zona <- list(
      lat_min = min(poligoni$latitudine),
      lat_max = max(poligoni$latitudine),
      lng_min = min(poligoni$longitudine),
      lng_max = max(poligoni$longitudine)
    )
  }
  memoria_geografia$zona
}

#' Applica una funzione ai soli valori distinti di un vettore
#'
#' I campi di testo delle letture hanno pochi valori ripetuti milioni di
#' volte: lavorare sui valori distinti evita di ripetere lo stesso calcolo.
#'
#' @param x Vettore.
#' @param f Funzione vettoriale, che restituisce un valore per ogni elemento.
#' @noRd
per_valori_distinti <- function(x, f) {
  distinti <- unique(x)
  f(distinti)[match(x, distinti)]
}

#' Chiave di confronto tra nomi di comune
#'
#' Maiuscolo, senza accenti, apostrofi e punteggiatura: `"Rosà"` e `"ROSA'"`
#' hanno la stessa chiave.
#' @noRd
chiave_comune <- function(x) {
  x <- stringr::str_to_upper(as.character(x))
  x <- chartr(
    "\u00c0\u00c1\u00c8\u00c9\u00cc\u00cd\u00d2\u00d3\u00d9\u00da",
    "AAEEIIOOUU",
    x
  )
  stringr::str_squish(stringr::str_replace_all(x, "[^A-Z0-9 ]", " "))
}

#' Riporta i nomi dei comuni alla grafia della tabella dei comuni
#'
#' Un comune che non è in tabella resta com'è, in maiuscolo.
#'
#' @param x Nomi di comune.
#' @return Vettore di testo della stessa lunghezza.
#' @noRd
normalizza_comune <- function(x) {
  per_valori_distinti(as.character(x), function(nomi) {
    tabella <- comuni_cantieri()
    noti <- tabella$comune[match(
      chiave_comune(nomi),
      chiave_comune(tabella$comune)
    )]
    dplyr::coalesce(noti, stringr::str_to_upper(nomi))
  })
}

#' Cantiere a cui appartiene un comune
#'
#' @param comune Nomi di comune, in qualsiasi grafia.
#' @return Il cantiere, oppure `NA` se il comune non è in tabella.
#' @noRd
cantiere_di_comune <- function(comune) {
  per_valori_distinti(as.character(comune), function(nomi) {
    tabella <- comuni_cantieri()
    tabella$cantiere[match(chiave_comune(nomi), chiave_comune(tabella$comune))]
  })
}

#' Completa le colonne geografiche delle letture
#'
#' Il comune assegnato a una lettura è quello del database, se c'è, altrimenti
#' quello del giro che l'ha letta. Il cantiere è quello del comune assegnato,
#' secondo la tabella dei comuni. Per un comune che non è in tabella resta il
#' cantiere indicato nel file.
#'
#' @param letture Letture con `comune_da_database`, `comune_lettura` e
#'   `cantiere`. Le colonne assenti sono trattate come vuote.
#' @return Le letture con `comune_assegnato` e `cantiere` compilati.
#' @noRd
aggiungi_geografia <- function(letture) {
  for (campo in colonne_geografiche()) {
    if (!campo %in% names(letture)) letture[[campo]] <- NA_character_
  }
  letture$comune_assegnato <- dplyr::coalesce(
    letture$comune_da_database,
    letture$comune_lettura
  )
  letture$cantiere <- dplyr::coalesce(
    cantiere_di_comune(letture$comune_assegnato),
    letture$cantiere
  )
  letture
}

#' Comune e cantiere di ogni RFID
#'
#' Un RFID può avere letture in più comuni. Il suo comune è l'ultimo indicato
#' dal database; se il database non lo conosce, è il comune in cui è stato
#' letto più spesso. Il cantiere è quello del comune. Per un comune che non è
#' nella tabella dei comuni vale il cantiere scritto nel file sulle letture di
#' quel comune; per un RFID senza comune, quello di una sua lettura qualsiasi.
#'
#' @param letture Letture validate, in ordine di tempo dentro ogni RFID.
#' @param gruppo Gruppo di ogni lettura, vedi `indice_gruppi()`.
#' @param n_gruppi Numero dei gruppi.
#' @return Tibble con `comune` e `cantiere`, una riga per gruppo.
#' @noRd
geografia_per_gruppo <- function(letture, gruppo, n_gruppi = max(gruppo)) {
  letture <- aggiungi_geografia(letture)
  comune <- ultimo_valido_per_gruppo(
    letture$comune_da_database,
    gruppo,
    n_gruppi
  )
  # Il comune di lettura serve solo agli RFID che il database non conosce.
  di_lettura <- as.character(letture$comune_lettura)
  di_lettura[!is.na(comune)[gruppo]] <- NA_character_
  letto <- piu_frequente_per_gruppo(di_lettura, gruppo, n_gruppi)
  comune <- dplyr::coalesce(comune, letto)
  del_file <- as.character(letture$cantiere)
  altro_comune <- !is.na(comune)[gruppo] &
    (is.na(letture$comune_assegnato) |
      letture$comune_assegnato != comune[gruppo])
  del_file[altro_comune] <- NA_character_
  dplyr::tibble(
    comune = comune,
    cantiere = dplyr::coalesce(
      cantiere_di_comune(comune),
      ultimo_valido_per_gruppo(del_file, gruppo, n_gruppi)
    )
  )
}
