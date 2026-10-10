# Comune in cui cade una lettura, e incongruenze con i comuni dichiarati.
#
# Ogni lettura ha una posizione GPS e fino a due comuni dichiarati: quello del
# database, per i censiti, e quello del giro del mezzo. Il comune geografico è
# quello il cui confine contiene il punto. Una lettura è "fuori comune" se sta
# oltre la tolleranza dal comune del database, "fuori giro" se sta oltre la
# tolleranza dal comune del giro.
#
# Con milioni di letture un controllo punto per punto contro ogni confine
# costerebbe decine di secondi. Le funzioni di questo file usano una griglia
# dei comuni, preparata una volta per sessione di R: la gran parte dei punti
# si risolve con una ricerca nella griglia, e il controllo esatto resta ai
# soli punti delle celle attraversate da un confine.

#' Distanza oltre la quale una lettura è fuori dal comune dichiarato
#'
#' Scelta dal proprietario del progetto: un chilometro. Assorbe l'errore del
#' GPS, i bidoni sulle strade di confine e i giri che sconfinano di poco.
#'
#' @return Metri.
#' @noRd
tolleranza_confine_m <- function() 1000

#' Metri per grado nella zona servita
#'
#' La zona è alta meno di un grado: un solo fattore per la longitudine,
#' calcolato alla latitudine media, sbaglia meno dell'1%.
#'
#' @return Vettore con `lng` e `lat`.
#' @noRd
metri_per_grado <- function() {
  zona <- riquadro_zona()
  per_grado <- raggio_terra_m() * pi / 180
  latitudine_media <- (zona$lat_min + zona$lat_max) / 2
  c(lng = per_grado * cos(latitudine_media * pi / 180), lat = per_grado)
}

#' Confini dei comuni, pronti per i controlli sui punti
#'
#' @return Lista con un elemento per comune, nell'ordine della tabella dei
#'   comuni. Ogni elemento ha i vertici `x` e `y`, quelli che li precedono
#'   nello stesso anello, `xp` e `yp`, il numero dell'anello di ogni vertice,
#'   `anello`, e il riquadro `x_min`, `x_max`, `y_min`, `y_max`. Un comune
#'   senza confine ha `NULL`.
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
        anello <- paste(confine$poligono, confine$anello)
        # Il vertice che precede il primo di un anello è l'ultimo dello stesso.
        precedente <- seq_along(x) - 1L
        precedente[!duplicated(anello)] <- which(
          !duplicated(anello, fromLast = TRUE)
        )
        list(
          x = x,
          y = y,
          xp = x[precedente],
          yp = y[precedente],
          anello = indice_gruppi(anello),
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

#' I punti sono dentro il confine di un comune?
#'
#' Stessa regola di `dentro_poligoni()`, ma per molti punti insieme: un ciclo
#' sui lati del confine, con i punti trattati come vettori.
#'
#' @param lng,lat Coordinate dei punti.
#' @param lati Un elemento di `lati_comuni()`.
#' @return Vettore logico, un valore per punto.
#' @noRd
punti_nel_comune <- function(lng, lat, lati) {
  dentro <- logical(length(lng))
  if (is.null(lati) || length(lng) == 0) {
    return(dentro)
  }
  for (i in seq_along(lati$x)) {
    # Solo i punti alla quota del lato possono attraversarlo.
    alla_quota <- which((lati$y[i] > lat) != (lati$yp[i] > lat))
    if (length(alla_quota) == 0) {
      next
    }
    incrocio <- (lati$xp[i] - lati$x[i]) *
      (lat[alla_quota] - lati$y[i]) /
      (lati$yp[i] - lati$y[i]) +
      lati$x[i]
    attraversa <- alla_quota[lng[alla_quota] < incrocio]
    dentro[attraversa] <- !dentro[attraversa]
  }
  dentro
}

#' Comune in cui cade ogni punto, con il controllo esatto sui confini
#'
#' @param lng,lat Coordinate dei punti.
#' @return Per ogni punto la riga della tabella dei comuni, 0 se il punto è
#'   fuori da tutti i comuni serviti.
#' @noRd
indice_comune_esatto <- function(lng, lat) {
  esito <- integer(length(lng))
  lati <- lati_comuni()
  for (k in seq_along(lati)) {
    confine <- lati[[k]]
    if (is.null(confine)) {
      next
    }
    candidati <- which(
      esito == 0L &
        lng >= confine$x_min &
        lng <= confine$x_max &
        lat >= confine$y_min &
        lat <= confine$y_max
    )
    if (length(candidati) == 0) {
      next
    }
    dentro <- punti_nel_comune(lng[candidati], lat[candidati], confine)
    esito[candidati[dentro]] <- k
  }
  esito
}

#' Griglia dei comuni serviti
#'
#' Celle di circa 55 metri di lato sulla zona servita. Una cella che nessun
#' confine attraversa sta tutta in un comune, o tutta fuori: vale il comune
#' del suo centro. Le celle attraversate da un confine, o vicine a uno, sono
#' marcate: per i loro punti serve il controllo esatto.
#'
#' @return Lista con l'origine `lng0`, `lat0`, i passi `passo_lng`,
#'   `passo_lat`, le dimensioni `n_righe`, `n_colonne` e `celle`: per ogni
#'   cella la riga della tabella dei comuni, 0 se è fuori dai comuni serviti,
#'   `NA` se va controllata. Le celle sono in ordine di colonna.
#' @noRd
griglia_comuni <- function() {
  if (!is.null(memoria_geografia$griglia)) {
    return(memoria_geografia$griglia)
  }
  zona <- riquadro_zona()
  passo_lng <- 0.0007
  passo_lat <- 0.0005
  lng0 <- zona$lng_min - passo_lng
  lat0 <- zona$lat_min - passo_lat
  n_colonne <- as.integer(ceiling((zona$lng_max - lng0) / passo_lng)) + 1L
  n_righe <- as.integer(ceiling((zona$lat_max - lat0) / passo_lat)) + 1L
  celle <- integer(n_righe * n_colonne)
  da_controllare <- logical(n_righe * n_colonne)
  lati <- lati_comuni()

  for (k in seq_along(lati)) {
    confine <- lati[[k]]
    if (is.null(confine)) {
      next
    }
    # Riempimento: per ogni riga della griglia, le celle il cui centro sta tra
    # due attraversamenti del confine sono dentro il comune.
    righe <- seq(
      max(1L, floor((confine$y_min - lat0) / passo_lat)),
      min(n_righe, ceiling((confine$y_max - lat0) / passo_lat) + 1L)
    )
    quota <- lat0 + (righe - 0.5) * passo_lat
    sopra <- outer(quota, confine$y, "<")
    sopra_p <- outer(quota, confine$yp, "<")
    incroci <- which(sopra != sopra_p, arr.ind = TRUE)
    if (nrow(incroci) > 0) {
      i <- incroci[, 2]
      x_incrocio <- (confine$xp[i] - confine$x[i]) *
        (quota[incroci[, 1]] - confine$y[i]) /
        (confine$yp[i] - confine$y[i]) +
        confine$x[i]
      ordine <- order(incroci[, 1], x_incrocio)
      riga <- righe[incroci[ordine, 1]]
      x_incrocio <- x_incrocio[ordine]
      # Gli attraversamenti di una riga vanno a coppie: entrata e uscita.
      entrata <- seq(1L, length(riga) - 1L, by = 2L)
      da <- floor((x_incrocio[entrata] - lng0) / passo_lng + 0.5) + 1
      a <- ceiling((x_incrocio[entrata + 1L] - lng0) / passo_lng + 0.5) - 1
      da <- pmax(da, 1)
      a <- pmin(a, n_colonne)
      piene <- which(a >= da)
      quante <- as.integer(a[piene] - da[piene] + 1)
      colonna <- sequence(quante, from = as.integer(da[piene]))
      celle[(colonna - 1L) * n_righe + rep(riga[entrata][piene], quante)] <- k
    }

    # Celle toccate dal confine: campioni lungo ogni lato, a meno di mezza
    # cella l'uno dall'altro, e le otto celle attorno a ogni campione.
    passi <- as.integer(ceiling(
      2 *
        pmax(
          abs(confine$x - confine$xp) / passo_lng,
          abs(confine$y - confine$yp) / passo_lat
        )
    )) +
      1L
    lato <- rep(seq_along(passi), passi + 1L)
    quota_parte <- (sequence(passi + 1L) - 1L) / passi[lato]
    x <- confine$xp[lato] + quota_parte * (confine$x[lato] - confine$xp[lato])
    y <- confine$yp[lato] + quota_parte * (confine$y[lato] - confine$yp[lato])
    colonna <- floor((x - lng0) / passo_lng) + 1
    riga <- floor((y - lat0) / passo_lat) + 1
    for (spostamento_colonna in -1:1) {
      for (spostamento_riga in -1:1) {
        c2 <- colonna + spostamento_colonna
        r2 <- riga + spostamento_riga
        dentro <- c2 >= 1 & c2 <= n_colonne & r2 >= 1 & r2 <= n_righe
        da_controllare[(c2[dentro] - 1) * n_righe + r2[dentro]] <- TRUE
      }
    }
  }
  celle[da_controllare] <- NA_integer_

  memoria_geografia$griglia <- list(
    lng0 = lng0,
    lat0 = lat0,
    passo_lng = passo_lng,
    passo_lat = passo_lat,
    n_righe = n_righe,
    n_colonne = n_colonne,
    celle = celle
  )
  memoria_geografia$griglia
}

#' Riga della tabella dei comuni del comune in cui cade ogni punto
#'
#' @param lat,lng Coordinate dei punti.
#' @return Vettore di interi: 0 per un punto fuori dai comuni serviti o senza
#'   coordinate.
#' @noRd
indice_comune_geografico <- function(lat, lng) {
  esito <- integer(length(lat))
  if (length(lat) == 0) {
    return(esito)
  }
  griglia <- griglia_comuni()
  colonna <- floor((lng - griglia$lng0) / griglia$passo_lng) + 1
  riga <- floor((lat - griglia$lat0) / griglia$passo_lat) + 1
  nella_griglia <- which(
    !is.na(colonna) &
      !is.na(riga) &
      colonna >= 1 &
      colonna <= griglia$n_colonne &
      riga >= 1 &
      riga <= griglia$n_righe
  )
  cella <- griglia$celle[
    (colonna[nella_griglia] - 1) * griglia$n_righe + riga[nella_griglia]
  ]
  esito[nella_griglia] <- cella
  # Le celle attraversate da un confine chiedono il controllo esatto.
  incerti <- nella_griglia[is.na(cella)]
  if (length(incerti) > 0) {
    esito[incerti] <- indice_comune_esatto(lng[incerti], lat[incerti])
  }
  esito
}

#' Comune in cui cade ogni punto
#'
#' @param lat,lng Coordinate dei punti.
#' @return Il nome del comune servito il cui confine contiene il punto, `NA`
#'   per un punto fuori da tutti i comuni serviti.
#' @noRd
comune_geografico <- function(lat, lng) {
  indice <- indice_comune_geografico(lat, lng)
  indice[indice == 0L] <- NA_integer_
  comuni_cantieri()$comune[indice]
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

#' Distanza di ogni punto dal confine di un comune
#'
#' Zero per un punto dentro il comune. La distanza è misurata su un piano, in
#' metri: nella zona servita sbaglia meno dell'1%.
#'
#' @param lat,lng Coordinate dei punti.
#' @param comune Comune da cui misurare la distanza, uno per punto.
#' @param limite Distanza oltre la quale basta sapere che il punto è lontano:
#'   per i punti più lontani di così dal riquadro del comune la funzione
#'   restituisce la distanza dal riquadro, che è minore di quella vera ma
#'   resta oltre il limite. Con `Inf` la distanza è sempre quella vera.
#' @param geografico Riga della tabella dei comuni del comune in cui cade ogni
#'   punto, se è già stata calcolata.
#' @return Metri, `NA` se il comune manca o non è nella tabella dei comuni, o
#'   se il punto non ha coordinate.
#' @noRd
distanza_dal_comune_m <- function(
  lat,
  lng,
  comune,
  limite = Inf,
  geografico = indice_comune_geografico(lat, lng)
) {
  n <- length(lat)
  esito <- rep(NA_real_, n)
  dichiarato <- indice_comune(rep_len(comune, n))
  noti <- !is.na(dichiarato) & !is.na(lat) & !is.na(lng)
  esito[noti & geografico == dichiarato] <- 0
  fuori <- which(noti & geografico != dichiarato)
  if (length(fuori) == 0) {
    return(esito)
  }
  scala <- metri_per_grado()
  lati <- lati_comuni()
  for (k in unique(dichiarato[fuori])) {
    confine <- lati[[k]]
    if (is.null(confine)) {
      next
    }
    punti <- fuori[dichiarato[fuori] == k]
    px <- lng[punti] * scala[["lng"]]
    py <- lat[punti] * scala[["lat"]]
    # Distanza dal riquadro del comune: non supera mai quella dal confine.
    dal_riquadro <- sqrt(
      pmax(
        confine$x_min * scala[["lng"]] - px,
        px - confine$x_max * scala[["lng"]],
        0
      )^2 +
        pmax(
          confine$y_min * scala[["lat"]] - py,
          py - confine$y_max * scala[["lat"]],
          0
        )^2
    )
    esito[punti] <- dal_riquadro
    vicini <- which(dal_riquadro <= limite)
    if (length(vicini) == 0) {
      next
    }
    # Distanza dal lato più vicino del confine.
    ax <- confine$xp * scala[["lng"]]
    ay <- confine$yp * scala[["lat"]]
    bx <- confine$x * scala[["lng"]]
    by <- confine$y * scala[["lat"]]
    qx <- px[vicini]
    qy <- py[vicini]
    minima <- rep(Inf, length(vicini))
    for (i in seq_along(ax)) {
      dx <- bx[i] - ax[i]
      dy <- by[i] - ay[i]
      lunghezza <- dx^2 + dy^2
      lungo_il_lato <- if (lunghezza > 0) {
        pmin(pmax(((qx - ax[i]) * dx + (qy - ay[i]) * dy) / lunghezza, 0), 1)
      } else {
        0
      }
      minima <- pmin(
        minima,
        (qx - ax[i] - lungo_il_lato * dx)^2 +
          (qy - ay[i] - lungo_il_lato * dy)^2
      )
    }
    esito[punti[vicini]] <- sqrt(minima)
  }
  esito
}

#' I punti sono oltre la tolleranza dal comune dichiarato?
#'
#' @param lat,lng Coordinate dei punti.
#' @param comune Comune dichiarato, uno per punto.
#' @param tolleranza Distanza in metri, vedi `tolleranza_confine_m()`.
#' @param geografico Riga della tabella dei comuni del comune in cui cade ogni
#'   punto, se è già stata calcolata.
#' @return Vettore logico. `FALSE` quando il confronto non si può fare: comune
#'   mancante o assente dalla tabella dei comuni.
#' @noRd
oltre_il_comune <- function(
  lat,
  lng,
  comune,
  tolleranza = tolleranza_confine_m(),
  geografico = indice_comune_geografico(lat, lng)
) {
  distanza <- distanza_dal_comune_m(
    lat,
    lng,
    comune,
    limite = tolleranza,
    geografico = geografico
  )
  !is.na(distanza) & distanza > tolleranza
}
