# Geografia del territorio servito: comuni, cantieri e confini.
#
# La tabella dei comuni sta in `inst/extdata/comuni_cantieri.csv`: è l'unico
# elenco di comuni e cantieri. I confini stanno in
# `inst/extdata/comuni_confini.geojson`, un comune per ogni riga della
# tabella: li prepara `prepara_confini_comuni()`, in
# `R/fct_preparazione_comuni.R`, dagli shapefile della Regione del Veneto.

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

#' Confini dei comuni, pronti per il disegno
#'
#' Servono al contorno dei comuni dei giri e all'area da inquadrare nella
#' ricerca dei giri, vedi `aggiungi_comuni_giro()` e `area_giri()`.
#'
#' @return Lista con un elemento per comune, nell'ordine della tabella dei
#'   comuni. Ogni elemento ha i vertici `x` e `y`, il numero dell'anello di
#'   ogni vertice, `anello`, e il riquadro `x_min`, `x_max`, `y_min`, `y_max`.
#'   Un comune senza confine ha `NULL`.
#' @noRd
lati_comuni <- function() {
  if (is.null(memoria_geografia$lati)) {
    poligoni <- poligoni_comuni()
    per_comune <- split(poligoni, poligoni$comune)
    memoria_geografia$lati <- lapply(
      comuni_cantieri()$comune,
      function(comune) {
        confine <- per_comune[[comune]]
        if (is.null(confine)) {
          return(NULL)
        }
        x <- confine$longitudine
        y <- confine$latitudine
        list(
          x = x,
          y = y,
          anello = indice_gruppi(paste(confine$poligono, confine$anello)),
          x_min = min(x),
          x_max = max(x),
          y_min = min(y),
          y_max = max(y)
        )
      }
    )
  }
  memoria_geografia$lati
}

#' Riga della tabella dei comuni di ogni nome
#'
#' @param comune Nomi di comune, in qualsiasi grafia.
#' @return Vettore di interi, `NA` per un comune che non è in tabella.
#' @noRd
indice_comune <- function(comune) {
  per_valori_distinti(as.character(comune), function(nomi) {
    match(chiave_comune(nomi), chiave_comune(comuni_cantieri()$comune))
  })
}

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
#'   `comune` e `cantiere`, come quella scritta da `prepara_confini_comuni()`.
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
#' Fonte: Regione del Veneto, strato dei comuni del geoportale, licenza IODL
#' 2.0. I confini non sono semplificati.
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

#' Baricentro e superficie di uno o più poligoni
#'
#' È il baricentro dell'area, non la media dei vertici: ogni poligono pesa
#' per la sua superficie, e i fori si sottraggono, qualunque sia il verso in
#' cui sono scritti i vertici. I conti sono fatti in gradi: dentro un comune
#' la lunghezza di un grado di longitudine cambia di pochissimo.
#'
#' @param poligoni Vertici di uno o più poligoni, con `poligono`, `anello`,
#'   `longitudine` e `latitudine`: i vertici di ogni anello stanno di seguito,
#'   il primo anello di un poligono è il contorno, gli altri sono fori.
#' @return Vettore con `longitudine`, `latitudine` e `area`, in gradi
#'   quadrati. Senza superficie le coordinate mancano e l'area è zero.
#' @noRd
baricentro_poligoni <- function(poligoni) {
  vuoto <- c(longitudine = NA_real_, latitudine = NA_real_, area = 0)
  if (nrow(poligoni) == 0) {
    return(vuoto)
  }
  # Coordinate riferite al loro punto medio: i prodotti restano piccoli e i
  # conti non perdono cifre.
  origine_x <- mean(poligoni$longitudine)
  origine_y <- mean(poligoni$latitudine)
  x <- poligoni$longitudine - origine_x
  y <- poligoni$latitudine - origine_y
  anello <- paste(poligoni$poligono, poligoni$anello)
  # Il vertice che segue l'ultimo di un anello è il primo dello stesso anello.
  successivo <- seq_along(x) + 1L
  successivo[!duplicated(anello, fromLast = TRUE)] <- which(!duplicated(anello))
  incrocio <- x * y[successivo] - x[successivo] * y
  gruppo <- indice_gruppi(anello)
  n <- max(gruppo)
  area <- somma_per_gruppo(incrocio, gruppo, n) / 2
  centro_x <- somma_per_gruppo((x + x[successivo]) * incrocio, gruppo, n) /
    (6 * area)
  centro_y <- somma_per_gruppo((y + y[successivo]) * incrocio, gruppo, n) /
    (6 * area)
  # Un contorno aggiunge superficie, un foro la toglie.
  foro <- primo_per_gruppo(poligoni$anello, gruppo, n) > 1
  peso <- abs(area) * ifelse(foro, -1, 1)
  utili <- is.finite(centro_x) & is.finite(centro_y)
  totale <- sum(peso[utili])
  if (!any(utili) || totale <= 0) {
    return(vuoto)
  }
  c(
    longitudine = origine_x + sum(peso[utili] * centro_x[utili]) / totale,
    latitudine = origine_y + sum(peso[utili] * centro_y[utili]) / totale,
    area = totale
  )
}

#' Baricentro di ogni comune servito
#'
#' È il punto in cui la mappa principale mette la bolla di un comune. Di
#' norma è il baricentro dell'area del comune. Se quel punto cade fuori dal
#' confine, come può succedere a un comune a mezzaluna o fatto di più parti,
#' vale il baricentro della parte più grande; se anche quello è fuori, il
#' centro abitato della tabella dei comuni. Il punto sta sempre dentro il
#' confine del suo comune.
#'
#' @return Tibble con `comune`, `latitudine` e `longitudine`, una riga per
#'   comune, nell'ordine della tabella dei comuni.
#' @noRd
baricentri_comuni <- function() {
  if (is.null(memoria_geografia$baricentri)) {
    comuni <- comuni_cantieri()
    confini <- poligoni_comuni()
    confini <- split(confini, confini$comune)
    punti <- lapply(seq_len(nrow(comuni)), function(i) {
      centro_abitato <- c(
        longitudine = comuni$longitudine[i],
        latitudine = comuni$latitudine[i]
      )
      confine <- confini[[comuni$comune[i]]]
      if (is.null(confine)) {
        return(centro_abitato)
      }
      candidati <- list(baricentro_poligoni(confine))
      parti <- split(confine, confine$poligono)
      if (length(parti) > 1) {
        misure <- lapply(parti, baricentro_poligoni)
        aree <- vapply(misure, function(m) m[["area"]], numeric(1))
        candidati <- c(candidati, misure[which.max(aree)])
      }
      for (punto in candidati) {
        if (
          !anyNA(punto) &&
            dentro_poligoni(
              punto[["longitudine"]],
              punto[["latitudine"]],
              confine
            )
        ) {
          return(punto[c("longitudine", "latitudine")])
        }
      }
      centro_abitato
    })
    memoria_geografia$baricentri <- dplyr::tibble(
      comune = comuni$comune,
      latitudine = vapply(punti, function(p) p[["latitudine"]], numeric(1)),
      longitudine = vapply(punti, function(p) p[["longitudine"]], numeric(1))
    )
  }
  memoria_geografia$baricentri
}

#' Punti dentro un comune e loro distanza dal confine
#'
#' Scorre i lati del confine una volta sola, per tutti i punti insieme: un
#' punto è dentro se una semiretta che parte da lui attraversa un numero
#' dispari di lati, come in `dentro_poligoni()`. La distanza è quella dal
#' lato più vicino, in metri, in una proiezione locale: dentro un comune la
#' lunghezza di un grado di longitudine cambia di pochissimo.
#'
#' @param lng,lat Coordinate dei punti.
#' @param lati Confine di un comune, un elemento di `lati_comuni()`.
#' @param distanza `TRUE` per calcolare anche la distanza dal confine.
#' @return Lista con `dentro`, un valore logico per punto, e `distanza`, in
#'   metri, oppure `NULL` senza `distanza`.
#' @noRd
misura_comune <- function(lng, lat, lati, distanza = FALSE) {
  x <- lati$x
  y <- lati$y
  # Il vertice che precede il primo di un anello è l'ultimo dello stesso anello.
  precedente <- seq_along(x) - 1L
  primi <- !duplicated(lati$anello)
  precedente[primi] <- which(!duplicated(lati$anello, fromLast = TRUE))
  dentro <- logical(length(lng))
  if (distanza) {
    metri_x <- 111320 * cos(mean(c(lati$y_min, lati$y_max)) * pi / 180)
    metri_y <- 110574
    px <- lng * metri_x
    py <- lat * metri_y
    quadrato <- rep(Inf, length(lng))
  }
  for (j in seq_along(x)) {
    xa <- x[precedente[j]]
    ya <- y[precedente[j]]
    attraversa <- ((y[j] > lat) != (ya > lat)) &
      (lng < (xa - x[j]) * (lat - y[j]) / (ya - y[j]) + x[j])
    dentro <- xor(dentro, attraversa %in% TRUE)
    if (distanza) {
      ax <- xa * metri_x
      ay <- ya * metri_y
      dx <- x[j] * metri_x - ax
      dy <- y[j] * metri_y - ay
      lunghezza <- dx * dx + dy * dy
      t <- if (lunghezza > 0) {
        pmin(pmax(((px - ax) * dx + (py - ay) * dy) / lunghezza, 0), 1)
      } else {
        0
      }
      quadrato <- pmin(quadrato, (px - ax - t * dx)^2 + (py - ay - t * dy)^2)
    }
  }
  list(dentro = dentro, distanza = if (distanza) sqrt(quadrato))
}

#' Comune servito che contiene ogni punto
#'
#' Serve alla preparazione dei dati, quando il comune di lettura non è nella
#' tabella dei comuni, e al dataset di esempio. L'app non lo usa: il comune di
#' una lettura viene dal file. I punti ripetuti si trattano una volta sola.
#'
#' @param lat,lng Coordinate dei punti.
#' @return Il nome del comune, nella grafia della tabella, oppure `NA` per un
#'   punto fuori dai comuni serviti o senza coordinate.
#' @noRd
comune_del_punto <- function(lat, lng) {
  esito <- rep(NA_character_, length(lat))
  validi <- which(!is.na(lat) & !is.na(lng))
  if (length(validi) == 0) {
    return(esito)
  }
  chiave <- paste(lat[validi], lng[validi])
  distinti <- !duplicated(chiave)
  plat <- lat[validi][distinti]
  plng <- lng[validi][distinti]
  trovato <- rep(NA_character_, length(plat))
  nomi <- comuni_cantieri()$comune
  lati <- lati_comuni()
  for (r in seq_along(lati)) {
    confine <- lati[[r]]
    if (is.null(confine)) {
      next
    }
    candidati <- which(
      is.na(trovato) &
        plng >= confine$x_min & plng <= confine$x_max &
        plat >= confine$y_min & plat <= confine$y_max
    )
    if (length(candidati) == 0) {
      next
    }
    dentro <- misura_comune(plng[candidati], plat[candidati], confine)$dentro
    trovato[candidati[dentro]] <- nomi[r]
  }
  esito[validi] <- trovato[match(chiave, chiave[distinti])]
  esito
}

#' Distanza di ogni punto da un comune servito, in metri
#'
#' Zero per un punto dentro il comune, altrimenti la distanza dal suo confine.
#' Serve alla preparazione dei dati: dice se una lettura fatta in un altro
#' comune è appena oltre il confine del comune dichiarato, vedi
#' `distanza_comune_dichiarato()`.
#'
#' @param lat,lng Coordinate dei punti.
#' @param comune Comune di ogni punto, in qualsiasi grafia.
#' @return Un numero per punto; `NA` se il comune non è nella tabella, non ha
#'   un confine, o il punto non ha coordinate.
#' @noRd
distanza_dal_comune <- function(lat, lng, comune) {
  esito <- rep(NA_real_, length(lat))
  riga <- indice_comune(comune)
  lati <- lati_comuni()
  for (r in sort(unique(stats::na.omit(riga)))) {
    confine <- lati[[r]]
    quali <- which(riga == r & !is.na(lat) & !is.na(lng))
    if (is.null(confine) || length(quali) == 0) {
      next
    }
    misura <- misura_comune(lng[quali], lat[quali], confine, distanza = TRUE)
    esito[quali] <- ifelse(misura$dentro, 0, misura$distanza)
  }
  esito
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
#' Il comune di una lettura è quello in cui è stata fatta, come lo dà il
#' sistema di localizzazione dei mezzi nella colonna `comune_lettura`: il
#' calendario dei mezzi non conta, perché un mezzo può sostituirne un altro e
#' fare un giro che non era il suo. La posizione non serve: il comune è già
#' quello esatto del punto.
#'
#' - `comune_lettura`: il comune del file, nella grafia della tabella dei
#'   comuni.
#' - `comune_geografico`: lo stesso comune, mancante se non è tra i comuni
#'   serviti.
#' - `comune_assegnato`: il comune geografico; fuori dai comuni serviti, o
#'   senza comune di lettura, quello del database, per i censiti. Un non
#'   censito letto fuori dai comuni serviti non ha un comune assegnato.
#' - `cantiere`: quello del comune assegnato, secondo la tabella dei comuni.
#'   Per un comune che non è in tabella resta il cantiere indicato nel file.
#' - `coerenza_comune`: il comune di lettura a confronto con quello
#'   dichiarato, vedi `coerenza_comune()`.
#'
#' @param letture Letture con `comune_lettura`, `comune_fornitura`,
#'   `comune_giro`, `cantiere` e `presente_a_database`. Le colonne dei comuni
#'   e del cantiere, se assenti, sono trattate come vuote.
#' @return Le letture con `comune_lettura`, `comune_geografico`,
#'   `comune_assegnato`, `cantiere` e `coerenza_comune` compilati.
#' @noRd
aggiungi_geografia <- function(letture) {
  for (campo in colonne_geografiche()) {
    if (!campo %in% names(letture)) letture[[campo]] <- NA_character_
  }
  letto <- normalizza_comune(letture$comune_lettura)
  letto[!is.na(letto) & !nzchar(letto)] <- NA_character_
  tabella <- comuni_cantieri()$comune
  letture$comune_lettura <- letto
  letture$comune_geografico <- tabella[match(letto, tabella)]
  letture$comune_assegnato <- dplyr::coalesce(
    letture$comune_geografico,
    as.character(letture$comune_fornitura)
  )
  letture$cantiere <- dplyr::coalesce(
    cantiere_di_comune(letture$comune_assegnato),
    as.character(letture$cantiere)
  )
  letture$coerenza_comune <- coerenza_comune(letture)
  letture
}

#' Comune e cantiere di ogni RFID
#'
#' Un RFID può avere letture in più comuni. Il suo comune è l'ultimo indicato
#' dal database. Il database non conosce i non censiti: per loro è il comune
#' di lettura del maggior numero delle letture. Un non censito letto solo
#' fuori dai comuni serviti non ha un comune.
#'
#' Il cantiere è quello del comune. Per un comune che non è nella tabella dei
#' comuni vale il cantiere scritto nel file sulle letture di quel comune; per
#' un RFID senza comune, quello di una sua lettura qualsiasi.
#'
#' @param letture Letture validate, in ordine di tempo dentro ogni RFID. Se
#'   manca `comune_geografico`, viene ricavato da `comune_lettura`.
#' @param gruppo Gruppo di ogni lettura, vedi `indice_gruppi()`.
#' @param n_gruppi Numero dei gruppi.
#' @return Tibble con `comune` e `cantiere`, una riga per gruppo.
#' @noRd
geografia_per_gruppo <- function(letture, gruppo, n_gruppi = max(gruppo)) {
  for (campo in colonne_geografiche()) {
    if (!campo %in% names(letture)) letture[[campo]] <- NA_character_
  }
  comune <- ultimo_valido_per_gruppo(
    letture$comune_fornitura,
    gruppo,
    n_gruppi
  )
  # Il comune di lettura serve solo agli RFID che il database non conosce.
  letto_a <- if (!is.null(letture[["comune_geografico"]])) {
    as.character(letture$comune_geografico)
  } else {
    tabella <- comuni_cantieri()$comune
    tabella[match(normalizza_comune(letture$comune_lettura), tabella)]
  }
  letto_a[!is.na(comune)[gruppo]] <- NA_character_
  comune <- dplyr::coalesce(
    comune,
    piu_frequente_per_gruppo(letto_a, gruppo, n_gruppi)
  )
  # Il cantiere del file vale per le letture che dichiarano il comune dell'RFID.
  dichiarato <- as.character(letture$comune_fornitura)
  del_file <- as.character(letture$cantiere)
  altro_comune <- !is.na(comune)[gruppo] &
    (is.na(dichiarato) | dichiarato != comune[gruppo])
  del_file[altro_comune] <- NA_character_
  dplyr::tibble(
    comune = comune,
    cantiere = dplyr::coalesce(
      cantiere_di_comune(comune),
      ultimo_valido_per_gruppo(del_file, gruppo, n_gruppi)
    )
  )
}
