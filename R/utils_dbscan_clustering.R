# Clustering spaziale DBSCAN delle letture di un contenitore.
#
# Le coordinate vengono proiettate in metri su un piano locale centrato sulle
# letture dell'RFID. Su estensioni di pochi chilometri lo scarto rispetto alla
# distanza di Haversine è inferiore all'1%, molto meno dell'errore del GPS, e
# DBSCAN può usare un indice spaziale invece della matrice delle distanze:
# il costo resta contenuto anche per tag con migliaia di letture.

#' Raggio medio della Terra in metri, usato in tutte le distanze
#' @noRd
raggio_terra_m <- function() 6371000

#' Proietta coordinate geografiche in metri su un piano locale
#'
#' Proiezione equirettangolare centrata sul punto medio.
#'
#' @param lat,lon Vettori di latitudine e longitudine in gradi decimali.
#' @return Matrice a due colonne (`x`, `y`) in metri.
#' @noRd
proietta_metri <- function(lat, lon) {
  lat0 <- mean(lat)
  lon0 <- mean(lon)
  in_radianti <- pi / 180
  cbind(
    x = raggio_terra_m() * (lon - lon0) * in_radianti * cos(lat0 * in_radianti),
    y = raggio_terra_m() * (lat - lat0) * in_radianti
  )
}

#' Distanza di Haversine in metri tra punti e un centro
#'
#' @param lat,lon Coordinate dei punti.
#' @param lat_centro,lon_centro Coordinate del centro (un valore o uno per punto).
#' @noRd
distanza_haversine_m <- function(lat, lon, lat_centro, lon_centro) {
  if (length(lat) == 0) {
    return(numeric(0))
  }
  geosphere::distHaversine(
    cbind(lon, lat),
    cbind(rep_len(lon_centro, length(lon)), rep_len(lat_centro, length(lat))),
    r = raggio_terra_m()
  )
}

#' Dispersione di un cluster: 90° percentile della distanza dal baricentro
#'
#' @param lat_vec,lon_vec Coordinate dei punti del cluster.
#' @param lat_centro,lon_centro Coordinate del baricentro.
#' @return Distanza in metri.
#' @noRd
calcola_dispersione_90th <- function(lat_vec, lon_vec, lat_centro, lon_centro) {
  distanze <- distanza_haversine_m(lat_vec, lon_vec, lat_centro, lon_centro)
  as.numeric(stats::quantile(distanze, 0.90, names = FALSE))
}

#' Cluster spaziali DBSCAN di un insieme di letture
#'
#' Con `min_pts = 1` ogni lettura appartiene a un cluster: due letture stanno
#' nello stesso cluster se sono collegate da una catena di letture distanti
#' tra loro al massimo `eps_m`. Con `min_pts` maggiore di 1 le letture isolate
#' sono rumore e ricevono il cluster 0.
#'
#' @param lat,lon Coordinate delle letture di un solo RFID.
#' @param eps_m Raggio di ricerca in metri.
#' @param min_pts Numero minimo di letture per formare un cluster.
#' @return Vettore intero con il cluster di ogni lettura.
#' @noRd
cluster_dbscan <- function(lat, lon, eps_m = 100, min_pts = 1) {
  n <- length(lat)
  if (n == 0) {
    return(integer(0))
  }
  if (n == 1) {
    return(if (min_pts <= 1) 1L else 0L)
  }
  as.integer(
    dbscan::dbscan(
      proietta_metri(lat, lon),
      eps = eps_m,
      minPts = min_pts
    )$cluster
  )
}

#' Rinumera i cluster in ordine di prima lettura
#'
#' Il cluster 1 è il luogo in cui il contenitore è stato visto per primo: per
#' un bidone spostato la numerazione segue quindi l'ordine degli spostamenti.
#' Il cluster 0 (rumore) resta invariato.
#'
#' @param cluster Vettore dei cluster assegnati da DBSCAN.
#' @param quando Data e ora di ogni lettura.
#' @noRd
ordina_cluster <- function(cluster, quando) {
  validi <- cluster > 0
  if (!any(validi)) {
    return(cluster)
  }
  prima <- tapply(as.numeric(quando[validi]), cluster[validi], min)
  nuovo <- rank(prima, ties.method = "first")
  cluster[validi] <- as.integer(nuovo[as.character(cluster[validi])])
  cluster
}

#' Assegna a ogni lettura il cluster spaziale del proprio RFID
#'
#' Il clustering è indipendente per ogni RFID: lo stesso `cluster_id` su due
#' RFID diversi non indica lo stesso luogo.
#'
#' @param letture Letture con `RFID`, `giorno_lettura`, `latitudine`, `longitudine`.
#' @param eps_m,min_pts Parametri di DBSCAN, vedi `cluster_dbscan()`.
#' @return Le letture con la colonna `cluster_id`.
#' @noRd
assegna_cluster <- function(letture, eps_m = 100, min_pts = 1) {
  dplyr::mutate(
    letture,
    cluster_id = ordina_cluster(
      cluster_dbscan(.data$latitudine, .data$longitudine, eps_m, min_pts),
      .data$giorno_lettura
    ),
    .by = "RFID"
  )
}
