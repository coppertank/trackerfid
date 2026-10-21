# DBSCAN delle letture di ogni contenitore, su un piano locale in metri: su
# pochi chilometri lo scarto dalla distanza di Haversine è sotto l'1%, e
# DBSCAN usa un indice spaziale invece della matrice delle distanze.

#' Raggio medio della Terra in metri, usato in tutte le distanze
#' @noRd
raggio_terra_m <- function() 6371000

#' Proiezione equirettangolare in metri, centrata sul punto medio: matrice `x`, `y`
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
#' @noRd
distanza_haversine_m <- function(lat, lon, lat_centro, lon_centro) {
  if (length(lat) == 0) {
    return(numeric(0))
  }
  in_radianti <- pi / 180
  mezzo_lat <- (lat_centro - lat) * in_radianti / 2
  mezzo_lon <- (lon_centro - lon) * in_radianti / 2
  a <- sin(mezzo_lat)^2 +
    cos(lat * in_radianti) * cos(lat_centro * in_radianti) * sin(mezzo_lon)^2
  # `pmin` ripara dagli arrotondamenti tra punti agli antipodi.
  2 * raggio_terra_m() * asin(pmin(1, sqrt(a)))
}

#' Letture oltre cui le coordinate di un RFID si arrotondano a 2 metri prima di DBSCAN
#' @noRd
soglia_arrotondamento_cluster <- function() 2000

#' Cluster DBSCAN delle letture di un RFID
#'
#' Con `min_pts = 1` ogni lettura sta in un cluster; con di più le isolate
#' sono rumore, cluster 0. Coordinate uguali entrano una volta, con un peso.
#' @param eps_m Raggio di ricerca in metri.
#' @return Il cluster di ogni lettura.
#' @noRd
cluster_dbscan <- function(lat, lon, eps_m = 100, min_pts = 1) {
  n <- length(lat)
  if (n == 0) {
    return(integer(0))
  }
  if (n == 1) {
    return(if (min_pts <= 1) 1L else 0L)
  }
  punti <- proietta_metri(lat, lon)
  if (n > soglia_arrotondamento_cluster()) {
    punti <- round(punti / 2) * 2
  }
  if (n >= 200) {
    posizione <- complex(real = punti[, 1], imaginary = punti[, 2])
    distinte <- unique(posizione)
    if (length(distinte) < n) {
      indice <- match(posizione, distinte)
      cluster <- dbscan::dbscan(
        cbind(Re(distinte), Im(distinte)),
        eps = eps_m,
        minPts = min_pts,
        weights = tabulate(indice, length(distinte))
      )$cluster
      return(as.integer(cluster[indice]))
    }
  }
  as.integer(dbscan::dbscan(punti, eps = eps_m, minPts = min_pts)$cluster)
}

#' Rinumera i cluster in ordine di prima lettura; lo 0 (rumore) resta
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

#' Aggiunge `cluster_id`, il cluster spaziale di ogni lettura dentro il suo RFID
#'
#' Un RFID il cui riquadro ha la diagonale più corta di `eps_m` ha un solo
#' cluster, senza DBSCAN: stesso risultato, molto più veloce.
#' @noRd
assegna_cluster <- function(letture, eps_m = 100, min_pts = 1) {
  if (nrow(letture) == 0) {
    letture$cluster_id <- integer(0)
    return(letture)
  }
  gruppo <- indice_gruppi(letture$RFID)
  n_gruppi <- max(gruppo)
  n_letture <- tabulate(gruppo, n_gruppi)

  # Diagonale del riquadro di ogni RFID, sul piano di `cluster_dbscan()`.
  in_radianti <- pi / 180
  lat <- estremi_per_gruppo(letture$latitudine, gruppo, n_gruppi)
  lon <- estremi_per_gruppo(letture$longitudine, gruppo, n_gruppi)
  lat_media <- media_per_gruppo(letture$latitudine, gruppo, n_gruppi)
  larghezza <- raggio_terra_m() *
    (lon$massimo - lon$minimo) *
    in_radianti *
    cos(lat_media * in_radianti)
  altezza <- raggio_terra_m() * (lat$massimo - lat$minimo) * in_radianti
  compatto <- sqrt(larghezza^2 + altezza^2) < eps_m

  # Un RFID compatto ha un cluster, o solo rumore se ha meno di `min_pts` letture.
  cluster <- as.integer(n_letture >= min_pts)[gruppo]
  altri <- which(!compatto[gruppo])
  for (righe in split(altri, gruppo[altri])) {
    cluster[righe] <- ordina_cluster(
      cluster_dbscan(
        letture$latitudine[righe],
        letture$longitudine[righe],
        eps_m,
        min_pts
      ),
      letture$giorno_lettura[righe]
    )
  }
  letture$cluster_id <- cluster
  letture
}
