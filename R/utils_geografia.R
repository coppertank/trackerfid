# Territorio servito: comuni e cantieri (`inst/extdata/comuni_cantieri.csv`,
# unico elenco) e confini (`inst/extdata/comuni_confini.geojson`, scritto da
# `prepara_confini_comuni()`).

# Tabelle lette una sola volta per sessione di R.
memoria_geografia <- new.env(parent = emptyenv())

#' Comuni serviti: `comune`, `cantiere`, `latitudine`, `longitudine` del centro abitato
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

#' Cantieri, in ordine alfabetico
#' @noRd
cantieri <- function() sort(unique(comuni_cantieri()$cantiere))

#' Vertici di ogni comune, per contorno e inquadratura dei giri
#'
#' @return Lista nell'ordine della tabella: `x`, `y`, `anello` e il riquadro
#'   `x_min`, `x_max`, `y_min`, `y_max`; `NULL` senza confine.
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

#' Riga della tabella dei comuni di ogni nome, in qualsiasi grafia; `NA` fuori tabella
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

#' Vertici di una geometria GeoJSON `Polygon` o `MultiPolygon`
#'
#' Il primo anello di un poligono è il contorno, gli altri sono fori.
#' @return Tibble: `poligono`, `anello`, `punto`, `longitudine`, `latitudine`.
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
      # Coppie longitudine, latitudine; a volte segue la quota.
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

#' Vertici dei confini di una `FeatureCollection` di comuni, con `comune` e `cantiere`
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

#' Vertici dei confini dei comuni serviti (Regione del Veneto, IODL 2.0)
#'
#' Per `ggplot2::geom_polygon()`: `group = poligono`, `subgroup = anello`.
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

#' Il punto è dentro i poligoni? Semiretta e numero dispari di lati attraversati
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

#' Baricentro dell'area e superficie, in gradi, di uno o più poligoni con fori
#' @noRd
baricentro_poligoni <- function(poligoni) {
  vuoto <- c(longitudine = NA_real_, latitudine = NA_real_, area = 0)
  if (nrow(poligoni) == 0) {
    return(vuoto)
  }
  # Coordinate riferite al punto medio, per non perdere cifre.
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

#' Baricentro di ogni comune, sempre dentro il suo confine: punto della bolla del comune
#'
#' Se il baricentro dell'area è fuori, quello della parte più grande; poi il
#' centro abitato della tabella.
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

#' Punti dentro un comune e, se chiesta, loro distanza dal confine in metri
#'
#' Scorre i lati una volta per tutti i punti; proiezione locale.
#' @param lati Un elemento di `lati_comuni()`.
#' @return Lista con `dentro` e `distanza` (o `NULL`).
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

#' Comune servito che contiene ogni punto, o `NA`; per pipeline e dataset di esempio
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
        plng >= confine$x_min &
        plng <= confine$x_max &
        plat >= confine$y_min &
        plat <= confine$y_max
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

#' Metri di ogni punto da un comune servito, zero dentro; `NA` se il comune non ha confine
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

#' Riquadro della zona servita: `lat_min`, `lat_max`, `lng_min`, `lng_max`
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

#' Applica `f` ai soli valori distinti di `x`
#' @noRd
per_valori_distinti <- function(x, f) {
  distinti <- unique(x)
  f(distinti)[match(x, distinti)]
}

#' Chiave di confronto dei comuni: maiuscolo, senza accenti né punteggiatura
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

#' Nomi dei comuni nella grafia della tabella; fuori tabella in maiuscolo
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

#' Cantiere di ogni comune, o `NA` fuori tabella
#' @noRd
cantiere_di_comune <- function(comune) {
  per_valori_distinti(as.character(comune), function(nomi) {
    tabella <- comuni_cantieri()
    tabella$cantiere[match(chiave_comune(nomi), chiave_comune(tabella$comune))]
  })
}

#' Completa la geografia delle letture dal comune di lettura del file
#'
#' `comune_lettura` nella grafia della tabella; `comune_geografico`, lo
#' stesso ma vuoto fuori tabella; `comune_assegnato`, il geografico o, per
#' un censito fuori tabella, quello del database; `cantiere` del comune
#' assegnato (fuori tabella resta quello del file); `coerenza_comune`. Il
#' calendario non assegna comuni. Stessa regola in SQL in `valida_archivio()`.
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

#' Comune e cantiere di ogni RFID, per la tabella dei cluster
#'
#' Censito: l'ultimo comune del database. Non censito: il comune geografico
#' più frequente. Fuori tabella vale il cantiere del file.
#' @param gruppo Gruppo di ogni lettura, da `indice_gruppi()`.
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
