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
  in_radianti <- pi / 180
  mezzo_lat <- (lat_centro - lat) * in_radianti / 2
  mezzo_lon <- (lon_centro - lon) * in_radianti / 2
  a <- sin(mezzo_lat)^2 +
    cos(lat * in_radianti) * cos(lat_centro * in_radianti) * sin(mezzo_lon)^2
  # `pmin` ripara dagli arrotondamenti tra punti agli antipodi.
  2 * raggio_terra_m() * asin(pmin(1, sqrt(a)))
}

#' Numero di letture oltre il quale le coordinate vengono arrotondate
#'
#' Il costo di DBSCAN cresce con il quadrato delle letture che cadono nello
#' stesso luogo. Per un RFID con più letture di questa soglia le coordinate
#' sono arrotondate a 2 metri prima del clustering: uno scarto molto inferiore
#' all'errore del GPS, che riduce i punti da confrontare.
#' @noRd
soglia_arrotondamento_cluster <- function() 2000

#' Cluster spaziali DBSCAN di un insieme di letture
#'
#' Con `min_pts = 1` ogni lettura appartiene a un cluster: due letture stanno
#' nello stesso cluster se sono collegate da una catena di letture distanti
#' tra loro al massimo `eps_m`. Con `min_pts` maggiore di 1 le letture isolate
#' sono rumore e ricevono il cluster 0.
#'
#' Le letture con le stesse coordinate entrano in DBSCAN una volta sola, con
#' il loro numero come peso: il risultato è lo stesso e il calcolo è più
#' rapido per i tag letti centinaia di volte nello stesso punto.
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
#' Quasi tutti i contenitori sono letti sempre nello stesso punto. Se il
#' riquadro che contiene le letture di un RFID ha la diagonale più corta del
#' raggio di ricerca, tutte le letture distano tra loro meno del raggio e
#' formano per forza un solo cluster: DBSCAN viene eseguito solo sugli altri
#' RFID. Il risultato è identico, il tempo molto minore.
#'
#' @param letture Letture con `RFID`, `giorno_lettura`, `latitudine`, `longitudine`.
#' @param eps_m,min_pts Parametri di DBSCAN, vedi `cluster_dbscan()`.
#' @return Le letture con la colonna `cluster_id`.
#' @noRd
assegna_cluster <- function(letture, eps_m = 100, min_pts = 1) {
  if (nrow(letture) == 0) {
    letture$cluster_id <- integer(0)
    return(letture)
  }
  gruppo <- indice_gruppi(letture$RFID)
  n_gruppi <- max(gruppo)
  n_letture <- tabulate(gruppo, n_gruppi)

  # Diagonale del riquadro di ogni RFID, in metri, sullo stesso piano locale
  # usato da `cluster_dbscan()`.
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

  # Un RFID compatto ha un solo cluster, oppure solo rumore se le letture sono
  # meno del minimo richiesto.
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
