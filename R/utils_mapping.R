# Funzioni di utilità per la costruzione delle mappe Leaflet.
#
# Tutte le funzioni accettano sia una mappa creata con `leaflet()` sia un
# `leafletProxy()`: i moduli disegnano la mappa di base una volta sola e poi
# aggiornano i marker tramite proxy.

#' Mappa di base: sfondo, scala e legenda
#'
#' @param modalita `"cluster"`, `"rfid"` o `"utenza"`.
#' @return Mappa Leaflet centrata su Roma.
#' @noRd
mappa_base <- function(modalita = "cluster") {
  leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::setView(lng = 12.5, lat = 41.9, zoom = 11) |>
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
    iconCreateFunction = htmlwidgets::JS(js_icona_cluster()),
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
aggiungi_marker <- function(map, df, cluster = FALSE, stile = "normale", opacita = 1, rialzo = 0) {
  leaflet::addMarkers(
    map,
    lng = df$longitudine,
    lat = df$latitudine,
    layerId = as.character(seq_len(nrow(df))),
    icon = get_leaflet_icon(df$servizio_transponder, df$presente_a_database, stile),
    popup = create_popup_html(
      rfid = df$RFID,
      servizio = df$servizio_transponder,
      servizio_atteso = df$servizio_atteso,
      targa = df$targa_veicolo,
      matricola = df$matricola_veicolo,
      giorno_lettura = df$giorno_lettura,
      id_utenza = df$id_utenza,
      presente = df$presente_a_database
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
    color = "gray", weight = 2, fillOpacity = 0, dashArray = NULL,
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
#' @noRd
disegna_marker <- function(map, df, modalita = c("cluster", "rfid", "utenza")) {
  modalita <- match.arg(modalita)
  map <- pulisci_mappa(map)
  if (is.null(df) || nrow(df) == 0) {
    return(map)
  }
  switch(modalita,
    cluster = aggiungi_marker(map, df, cluster = TRUE),
    utenza = aggiungi_marker(map, df),
    rfid = map |>
      add_rfid_bounds(df) |>
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
    lng1 = b$lng_min - margine, lat1 = b$lat_min - margine,
    lng2 = b$lng_max + margine, lat2 = b$lat_max + margine,
    options = list(maxZoom = 17)
  )
}
