# Funzioni di utilità per la costruzione delle mappe Leaflet.
#
# Tutte le funzioni accettano sia una mappa creata con `leaflet()` sia un
# `leafletProxy()`: i moduli disegnano la mappa di base una volta sola e poi
# aggiornano i marker tramite proxy.

#' Confini dei comuni serviti, pronti per Leaflet
#'
#' Ogni comune porta il colore del suo cantiere, vedi `colori_cantieri()`. Le
#' coordinate hanno cinque decimali, circa un metro: il testo pesa poco più
#' di 100 kB e viene preparato una volta per sessione di R.
#'
#' @return Testo GeoJSON: una `FeatureCollection` con un comune per elemento
#'   e, tra le proprietà, `comune`, `cantiere` e lo stile del confine.
#' @noRd
geojson_confini <- function() {
  if (is.null(memoria_geografia$geojson_mappa)) {
    confini <- jsonlite::fromJSON(
      percorso_confini_comuni(),
      simplifyVector = FALSE
    )
    elementi <- lapply(confini$features, function(elemento) {
      colore <- colore_cantiere(elemento$properties$cantiere)
      list(
        type = "Feature",
        properties = list(
          comune = elemento$properties$comune,
          cantiere = elemento$properties$cantiere,
          style = list(color = colore, fillColor = colore)
        ),
        geometry = elemento$geometry[c("type", "coordinates")]
      )
    })
    memoria_geografia$geojson_mappa <- as.character(jsonlite::toJSON(
      list(type = "FeatureCollection", features = elementi),
      auto_unbox = TRUE,
      digits = 5
    ))
  }
  memoria_geografia$geojson_mappa
}

#' Disegna sulla mappa i confini dei comuni, colorati per cantiere
#'
#' I confini stanno in un livello loro, sotto marker, cluster e riquadri, e
#' non rispondono al mouse: click e passaggi arrivano alla mappa e ai marker.
#' `pulisci_mappa()` non li tocca.
#'
#' @param map Mappa Leaflet.
#' @noRd
aggiungi_confini <- function(map) {
  stile <- stile_confini()
  map |>
    leaflet::addMapPane("confini", zIndex = 350) |>
    leaflet::addGeoJSON(
      geojson_confini(),
      group = "confini",
      weight = stile$weight,
      opacity = stile$opacity,
      fillOpacity = stile$fillOpacity,
      dashArray = stile$dashArray,
      options = leaflet::pathOptions(pane = "confini", interactive = FALSE)
    )
}

#' Mappa di base: sfondo, confini dei comuni, scala e legenda
#'
#' @param modalita `"cluster"`, `"rfid"` o `"utenza"`.
#' @return Mappa Leaflet che inquadra tutta la zona servita.
#' @noRd
mappa_base <- function(modalita = "cluster") {
  zona <- riquadro_zona()
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    aggiungi_confini() |>
    leaflet::fitBounds(
      lng1 = zona$lng_min,
      lat1 = zona$lat_min,
      lng2 = zona$lng_max,
      lat2 = zona$lat_max
    ) |>
    leaflet::addScaleBar(
      position = "bottomleft",
      options = leaflet::scaleBarOptions(imperial = FALSE)
    ) |>
    leaflet::addControl(
      html = html_legenda(modalita),
      position = "bottomright",
      className = "info legend legenda-rfid"
    )
}

#' Opzioni del clustering geografico (plugin Leaflet.markercluster)
#' @noRd
opzioni_cluster <- function() {
  leaflet::markerClusterOptions(
    iconCreateFunction = leaflet::JS(js_icona_cluster()),
    showCoverageOnHover = FALSE,
    spiderfyOnMaxZoom = TRUE,
    maxClusterRadius = 60
  )
}

#' Rimuove marker, cluster e riquadri dalla mappa
#'
#' I confini dei comuni restano: Leaflet li tiene tra i livelli GeoJSON, non
#' tra le forme.
#' @noRd
pulisci_mappa <- function(map) {
  map |>
    leaflet::clearMarkers() |>
    leaflet::clearMarkerClusters() |>
    leaflet::clearShapes()
}

#' Aggiunge i marker delle letture alla mappa
#'
#' L'identificativo di ogni marker è il numero di riga di `df`, così il click
#' può essere ricondotto alla lettura anche quando lo stesso RFID compare più
#' volte.
#'
#' @param map Mappa o proxy Leaflet.
#' @param df Letture da disegnare (una riga per marker).
#' @param cluster Se `TRUE` raggruppa i marker vicini.
#' @param stile Stile del bordo dei marker, vedi `svg_marker()`.
#' @param opacita Opacità dei marker.
#' @param rialzo Priorità di sovrapposizione (zIndexOffset).
#' @noRd
aggiungi_marker <- function(
  map,
  df,
  cluster = FALSE,
  stile = "normale",
  opacita = 1,
  rialzo = 0
) {
  # Per i non censiti l'icona segue il servizio atteso prevalente dell'RFID.
  servizio <- if ("servizio_icona" %in% names(df)) {
    df$servizio_icona
  } else {
    df$servizio_transponder
  }
  leaflet::addMarkers(
    map,
    lng = df$longitudine,
    lat = df$latitudine,
    layerId = as.character(seq_len(nrow(df))),
    icon = get_leaflet_icon(
      servizio,
      df$presente_a_database,
      stile,
      fuori_comune = df[["fuori_comune"]] %||% FALSE,
      fuori_giro = df[["fuori_giro"]] %||% FALSE
    ),
    popup = create_popup_html(
      rfid = df$RFID,
      servizio_atteso = df$servizio_atteso,
      targa = df$targa_veicolo,
      matricola = df$matricola_veicolo,
      giorno_lettura = df$giorno_lettura,
      id_utenza = df$id_utenza,
      presente = df$presente_a_database,
      comune = df[["comune_da_database"]],
      comune_lettura = df[["comune_lettura"]],
      comune_giro = df[["comune_giro"]],
      cantiere = df[["cantiere"]],
      comune_geografico = df[["comune_geografico"]],
      fuori_comune = df[["fuori_comune"]],
      fuori_giro = df[["fuori_giro"]],
      servizio_giro = df[["servizio_giro"]],
      coerenza = df[["coerenza"]]
    ),
    label = df$RFID,
    options = leaflet::markerOptions(
      censito = df$presente_a_database == "Presente",
      opacity = opacita,
      zIndexOffset = rialzo,
      riseOnHover = TRUE
    ),
    clusterOptions = if (cluster) opzioni_cluster()
  )
}

#' Aggiunge alla mappa le bolle della vista aggregata
#'
#' L'identificativo di una bolla è `bolla-` seguito dal numero di riga: il
#' click la distingue così da un marker. Il nome di un cantiere resta
#' sempre visibile sotto la bolla, gli altri compaiono al passaggio.
#'
#' @param map Mappa o proxy Leaflet.
#' @param bolle Risultato di `aggrega_marker()`.
#' @noRd
aggiungi_bolle <- function(map, bolle) {
  if (nrow(bolle) == 0) {
    return(map)
  }
  per_cantiere <- identical(bolle$livello[1], "cantiere")
  nome <- dplyr::coalesce(
    bolle$nome,
    if (identical(bolle$livello[1], "griglia")) "Zona" else senza_cantiere()
  )
  descrizione <- sprintf(
    "%s: %s, %s censiti",
    nome,
    purrr::map_chr(bolle$n, conta, "bidone", "bidoni"),
    formatta_numero(bolle$censiti)
  )
  leaflet::addMarkers(
    map,
    lng = bolle$longitudine,
    lat = bolle$latitudine,
    layerId = paste0("bolla-", seq_len(nrow(bolle))),
    icon = icone_bolle(bolle$n, bolle$censiti),
    label = if (per_cantiere) nome else descrizione,
    labelOptions = leaflet::labelOptions(
      permanent = per_cantiere,
      direction = "bottom",
      # L'etichetta sta sotto la bolla, senza coprirne la percentuale.
      offset = c(0, max(lato_bolla(bolle$n)) / 2 - 6),
      className = "etichetta-bolla"
    ),
    options = leaflet::markerOptions(riseOnHover = TRUE)
  )
}

#' Disegna un riquadro attorno a tutte le letture di ogni RFID
#'
#' Gli RFID letti sempre nello stesso punto non hanno riquadro.
#'
#' @param map Mappa o proxy Leaflet.
#' @param df Letture degli RFID cercati.
#' @noRd
add_rfid_bounds <- function(map, df) {
  riquadri <- split(df, df$RFID) |>
    purrr::map(calcola_bbox) |>
    purrr::keep(function(b) b$lat_min < b$lat_max || b$lng_min < b$lng_max)
  if (length(riquadri) == 0) {
    return(map)
  }
  leaflet::addRectangles(
    map,
    lng1 = purrr::map_dbl(riquadri, "lng_min"),
    lat1 = purrr::map_dbl(riquadri, "lat_min"),
    lng2 = purrr::map_dbl(riquadri, "lng_max"),
    lat2 = purrr::map_dbl(riquadri, "lat_max"),
    color = "gray",
    weight = 2,
    fillOpacity = 0,
    dashArray = NULL,
    label = names(riquadri),
    group = paste0("bounds_", names(riquadri))
  )
}

#' Mette in evidenza il confine dei comuni dei giri
#'
#' Un contorno scuro e continuo attorno ai comuni in cui i giri dovevano
#' lavorare: le letture che stanno fuori si vedono a colpo d'occhio. Con molti
#' comuni, come in una ricerca su tutti i mezzi, il contorno non aiuta e non
#' viene disegnato.
#'
#' @param map Mappa o proxy Leaflet.
#' @param comuni Comuni dei giri, anche ripetuti.
#' @param massimo Numero massimo di comuni messi in evidenza.
#' @noRd
aggiungi_comuni_giro <- function(map, comuni, massimo = 6) {
  indici <- unique(stats::na.omit(indice_comune(unique(comuni))))
  if (length(indici) == 0 || length(indici) > massimo) {
    return(map)
  }
  lati <- lati_comuni()[indici]
  nomi <- comuni_cantieri()$comune[indici]
  for (i in seq_along(lati)) {
    confine <- lati[[i]]
    if (is.null(confine)) {
      next
    }
    # Tra un anello e il successivo va un valore mancante: così Leaflet li
    # disegna separati.
    n <- length(confine$x)
    chiude <- c(diff(confine$anello) != 0, TRUE)
    posizione <- seq_len(n) + cumsum(c(0L, chiude[-n]))
    lng <- rep(NA_real_, n + sum(chiude) - 1L)
    lat <- lng
    lng[posizione] <- confine$x
    lat[posizione] <- confine$y
    map <- leaflet::addPolygons(
      map,
      lng = lng,
      lat = lat,
      color = colori_stato()$bordo,
      weight = 3,
      opacity = 0.9,
      fill = FALSE,
      label = paste("Comune del giro:", nomi[i]),
      group = "comuni_giro"
    )
  }
  map
}

#' Ridisegna i contenuti della mappa secondo la modalità
#'
#' - `"cluster"`: una riga per RFID, marker vicini raggruppati in cluster
#'   colorati. Da lontano la mappa principale disegna al loro posto una bolla
#'   per cantiere o per comune, vedi `scegli_vista()`.
#' - `"rfid"`: tutte le letture degli RFID cercati, senza clustering, con
#'   l'ultima lettura in evidenza e il riquadro degli spostamenti.
#' - `"utenza"`: ultima lettura di ogni RFID, senza clustering.
#' - `"giro"`: tutte le letture di uno o più giri, senza clustering, con il
#'   confine dei comuni dei giri in evidenza.
#'
#' @param map Mappa o proxy Leaflet.
#' @param df Letture da disegnare, oppure `NULL`.
#' @param modalita Modalità di visualizzazione.
#' @param riquadri Letture su cui calcolare i riquadri della ricerca RFID:
#'   tutte quelle trovate, anche quando `df` ne contiene solo una parte.
#' @noRd
disegna_marker <- function(
  map,
  df,
  modalita = c("cluster", "rfid", "utenza", "giro"),
  riquadri = df
) {
  modalita <- match.arg(modalita)
  map <- pulisci_mappa(map)
  if (is.null(df) || nrow(df) == 0) {
    return(map)
  }
  switch(
    modalita,
    cluster = aggiungi_marker(map, df, cluster = TRUE),
    utenza = aggiungi_marker(map, df),
    giro = map |>
      aggiungi_comuni_giro(riquadri[["comune_giro"]]) |>
      aggiungi_marker(df),
    rfid = map |>
      add_rfid_bounds(riquadri) |>
      aggiungi_marker(
        df,
        stile = ifelse(df$is_ultimo, "ultimo", "precedente"),
        opacita = ifelse(df$is_ultimo, 1, 0.6),
        rialzo = ifelse(df$is_ultimo, 1000, 0)
      )
  )
}

#' Inquadra la mappa sulle letture indicate
#' @noRd
adatta_vista <- function(map, df, margine = 0.0015) {
  if (is.null(df) || nrow(df) == 0) {
    return(map)
  }
  b <- calcola_bbox(df)
  leaflet::fitBounds(
    map,
    lng1 = b$lng_min - margine,
    lat1 = b$lat_min - margine,
    lng2 = b$lng_max + margine,
    lat2 = b$lat_max + margine,
    options = list(maxZoom = 17)
  )
}
