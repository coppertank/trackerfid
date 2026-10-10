# Funzioni di utilità per la costruzione delle mappe Leaflet.
#
# Tutte le funzioni accettano sia una mappa creata con `leaflet()` sia un
# `leafletProxy()`: i moduli disegnano la mappa di base una volta sola e poi
# aggiornano i marker tramite proxy.

#' Mappa di base: sfondo, scala e legenda
#'
#' @param modalita `"cluster"`, `"rfid"` o `"utenza"`.
#' @return Mappa Leaflet che inquadra tutta la zona servita.
#' @noRd
mappa_base <- function(modalita = "cluster") {
  zona <- riquadro_zona()
  leaflet::leaflet() |>
    leaflet::addTiles() |>
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
    icon = get_leaflet_icon(servizio, df$presente_a_database, stile),
    popup = create_popup_html(
      rfid = df$RFID,
      servizio = df$servizio_transponder,
      servizio_atteso = df$servizio_atteso,
      targa = df$targa_veicolo,
      matricola = df$matricola_veicolo,
      giorno_lettura = df$giorno_lettura,
      id_utenza = df$id_utenza,
      presente = df$presente_a_database,
      comune = df[["comune_da_database"]],
      comune_lettura = df[["comune_lettura"]],
      cantiere = df[["cantiere"]]
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

#' Ridisegna i contenuti della mappa secondo la modalità
#'
#' - `"cluster"`: una riga per RFID, marker raggruppati in cluster colorati.
#' - `"rfid"`: tutte le letture degli RFID cercati, senza clustering, con
#'   l'ultima lettura in evidenza e il riquadro degli spostamenti.
#' - `"utenza"`: ultima lettura di ogni RFID, senza clustering.
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
  modalita = c("cluster", "rfid", "utenza"),
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
