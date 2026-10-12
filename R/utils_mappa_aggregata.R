# Vista della mappa principale secondo lo zoom.
#
# Da lontano la mappa mostra una bolla per cantiere, avvicinandosi una bolla
# per comune, da vicino i singoli bidoni: i gruppi che si vedono sono quelli
# del territorio, non raggruppamenti casuali di marker vicini. I conti li fa
# il server, che conosce zoom e area inquadrata.
#
# Da vicino i bidoni arrivano tutti al browser finché stanno sotto il limite
# di `limiti_mappa()`. Oltre, il browser riceverebbe decine di megabyte a
# ogni cambio di filtro: la mappa disegna allora i soli bidoni dell'area
# inquadrata oppure, se sono ancora troppi, una bolla per riquadro.

#' Limiti del disegno delle mappe
#'
#' Si possono cambiare con le opzioni `trackerfid.max_marker` e
#' `trackerfid.max_letture_ricerca`.
#'
#' @return Lista con `marker`, il numero massimo di bidoni disegnati uno per
#'   uno sulla mappa principale, e `letture_ricerca`, il numero massimo di
#'   letture disegnate dalla ricerca RFID.
#' @noRd
limiti_mappa <- function() {
  list(
    marker = getOption("trackerfid.max_marker", 10000),
    letture_ricerca = getOption("trackerfid.max_letture_ricerca", 3000)
  )
}

#' Zoom fino a cui la mappa mostra le bolle di cantieri e comuni
#'
#' @return Lista con `cantiere` e `comune`: l'ultimo zoom a cui si vede una
#'   bolla per cantiere e l'ultimo a cui se ne vede una per comune.
#' @noRd
soglie_zoom <- function() {
  list(cantiere = 11, comune = 14)
}

#' Livello di aggregazione adatto a uno zoom
#'
#' Con la zona servita inquadrata per intero lo zoom è 10 o meno: si vedono
#' i cantieri, e restano anche a zoom 11, quando un cantiere occupa buona
#' parte della mappa. Da zoom 12 a zoom 14 si vedono i comuni: a 12 un comune
#' occupa un paio di centinaia di pixel, a 14 più della mappa. Da 15 in su si
#' vedono i singoli bidoni. Le soglie stanno in `soglie_zoom()`.
#'
#' @param zoom Zoom della mappa.
#' @return `"cantiere"` fino a zoom 11, `"comune"` da zoom 12 a zoom 14, poi
#'   `"griglia"`, cioè nessun raggruppamento per territorio.
#' @noRd
livello_aggregazione <- function(zoom) {
  soglie <- soglie_zoom()
  if (is.null(zoom) || zoom <= soglie$cantiere) {
    "cantiere"
  } else if (zoom <= soglie$comune) {
    "comune"
  } else {
    "griglia"
  }
}

#' Zoom minimo a cui porta il click su una bolla
#'
#' Il click su una bolla deve aprirla: quella di un cantiere mostra i suoi
#' comuni, quella di un comune i suoi bidoni. Lo zoom a cui i bidoni della
#' bolla entrano nella mappa può non bastare: un cantiere entra per intero a
#' uno zoom a cui si vedono ancora i cantieri.
#'
#' @param livello `"cantiere"`, `"comune"` oppure `"griglia"`.
#' @return Il primo zoom del livello successivo. `NA` per una bolla per
#'   riquadro, che non ha un livello successivo.
#' @noRd
zoom_minimo_bolla <- function(livello) {
  soglie <- soglie_zoom()
  switch(
    as.character(livello),
    cantiere = soglie$cantiere + 1,
    comune = soglie$comune + 1,
    NA_real_
  )
}

#' Zoom a cui un riquadro entra per intero nella mappa
#'
#' Lo ricava dall'area inquadrata e dallo zoom attuali, come fa Leaflet: a
#' ogni livello di zoom l'area inquadrata dimezza di lato.
#'
#' @param riquadro Riquadro da inquadrare: lista con `north`, `south`, `east`,
#'   `west`.
#' @param vista Area inquadrata ora, nella stessa forma.
#' @param zoom Zoom attuale.
#' @return Lo zoom, intero. `Inf` per un riquadro ridotto a un punto.
#' @noRd
zoom_del_riquadro <- function(riquadro, vista, zoom) {
  # In latitudine le distanze sullo schermo seguono la proiezione di Mercatore.
  mercatore <- function(lat) log(tan(pi / 4 + lat * pi / 360))
  rapporto <- min(
    (vista$east - vista$west) / (riquadro$east - riquadro$west),
    (mercatore(vista$north) - mercatore(vista$south)) /
      (mercatore(riquadro$north) - mercatore(riquadro$south))
  )
  floor(zoom + log2(rapporto))
}

#' Allarga un riquadro di una quota della sua ampiezza
#'
#' Caricare anche i bidoni appena fuori dall'area inquadrata evita di
#' ridisegnare la mappa a ogni piccolo spostamento.
#'
#' @param riquadro Lista con `north`, `south`, `east`, `west`, come quella
#'   che Leaflet comunica a Shiny.
#' @param margine Quota di ampiezza aggiunta su ogni lato.
#' @noRd
allarga_riquadro <- function(riquadro, margine = 0.3) {
  altezza <- riquadro$north - riquadro$south
  larghezza <- riquadro$east - riquadro$west
  list(
    north = riquadro$north + margine * altezza,
    south = riquadro$south - margine * altezza,
    east = riquadro$east + margine * larghezza,
    west = riquadro$west - margine * larghezza
  )
}

#' Il primo riquadro contiene il secondo?
#' @noRd
riquadro_contiene <- function(esterno, interno) {
  !is.null(esterno) &&
    !is.null(interno) &&
    esterno$north >= interno$north &&
    esterno$south <= interno$south &&
    esterno$east >= interno$east &&
    esterno$west <= interno$west
}

#' Bidoni che cadono in un riquadro
#'
#' @param df Letture con `latitudine` e `longitudine`.
#' @param riquadro Lista con `north`, `south`, `east`, `west`.
#' @return Vettore logico, un valore per riga. Tutti veri senza riquadro.
#' @noRd
nel_riquadro <- function(df, riquadro) {
  if (is.null(riquadro)) {
    return(rep(TRUE, nrow(df)))
  }
  df$latitudine >= riquadro$south &
    df$latitudine <= riquadro$north &
    df$longitudine >= riquadro$west &
    df$longitudine <= riquadro$east
}

#' Lato delle celle della griglia a uno zoom, in gradi di longitudine
#'
#' Una cella occupa circa 90 pixel sullo schermo.
#' @noRd
passo_griglia <- function(zoom) {
  360 / 2^zoom * 90 / 256
}

#' Raggruppa i bidoni in bolle
#'
#' Ogni bolla riporta il numero dei suoi bidoni, i censiti e il riquadro che
#' li contiene. La bolla di un comune sta nel baricentro del comune, vedi
#' `baricentri_comuni()`: resta ferma qualunque siano i bidoni filtrati, in
#' mezzo al confine disegnato sulla mappa. Le altre bolle, e quelle di un
#' comune che non è nella tabella dei comuni, stanno nel punto medio dei loro
#' bidoni.
#'
#' @param df Una riga per bidone, con stato a database, coordinate, `cantiere`
#'   e `comune_assegnato`.
#' @param livello `"cantiere"`, `"comune"` oppure `"griglia"`.
#' @param zoom Zoom della mappa: decide il lato delle celle della griglia.
#' @return Tibble con `livello`, `nome`, `latitudine`, `longitudine`, `n`,
#'   `censiti`, `lat_min`, `lat_max`, `lng_min`, `lng_max`. I bidoni senza
#'   cantiere o senza comune formano una bolla con nome mancante.
#' @noRd
aggrega_marker <- function(
  df,
  livello = c("cantiere", "comune", "griglia"),
  zoom = 13
) {
  livello <- match.arg(livello)
  if (nrow(df) == 0) {
    return(dplyr::tibble(
      livello = character(0),
      nome = character(0),
      latitudine = numeric(0),
      longitudine = numeric(0),
      n = integer(0),
      censiti = integer(0),
      lat_min = numeric(0),
      lat_max = numeric(0),
      lng_min = numeric(0),
      lng_max = numeric(0)
    ))
  }
  chiave <- switch(
    livello,
    cantiere = df[["cantiere"]] %||% rep(NA_character_, nrow(df)),
    comune = df[["comune_assegnato"]] %||% rep(NA_character_, nrow(df)),
    griglia = {
      passo <- passo_griglia(zoom)
      # Celle quadrate sullo schermo: in latitudine il passo si accorcia.
      passo_lat <- passo * cos(mean(df$latitudine) * pi / 180)
      paste(
        floor(df$latitudine / passo_lat),
        floor(df$longitudine / passo)
      )
    }
  )
  gruppo <- indice_gruppi(chiave)
  n_gruppi <- max(gruppo)
  lat <- estremi_per_gruppo(df$latitudine, gruppo, n_gruppi)
  lng <- estremi_per_gruppo(df$longitudine, gruppo, n_gruppi)
  nome <- if (livello == "griglia") {
    rep(NA_character_, n_gruppi)
  } else {
    primo_per_gruppo(chiave, gruppo, n_gruppi)
  }
  latitudine <- media_per_gruppo(df$latitudine, gruppo, n_gruppi)
  longitudine <- media_per_gruppo(df$longitudine, gruppo, n_gruppi)
  if (livello == "comune") {
    baricentri <- baricentri_comuni()
    riga <- match(chiave_comune(nome), chiave_comune(baricentri$comune))
    latitudine <- dplyr::coalesce(baricentri$latitudine[riga], latitudine)
    longitudine <- dplyr::coalesce(baricentri$longitudine[riga], longitudine)
  }
  dplyr::tibble(
    livello = livello,
    nome = nome,
    latitudine = latitudine,
    longitudine = longitudine,
    n = tabulate(gruppo, n_gruppi),
    censiti = as.integer(somma_per_gruppo(
      df$presente_a_database == "Presente",
      gruppo,
      n_gruppi
    )),
    lat_min = lat$minimo,
    lat_max = lat$massimo,
    lng_min = lng$minimo,
    lng_max = lng$massimo
  )
}

#' Sceglie come disegnare la mappa principale
#'
#' Da lontano una bolla per cantiere, poi una per comune, secondo
#' `livello_aggregazione()`: vale per qualsiasi numero di bidoni. Da vicino,
#' o se il file non ha cantieri né comuni, i singoli bidoni: tutti, se
#' stanno sotto il limite, altrimenti quelli dell'area inquadrata o, se sono
#' ancora troppi, una bolla per riquadro.
#'
#' @param df Una riga per bidone, oppure `NULL`.
#' @param riquadro Area inquadrata, oppure `NULL` se non è ancora nota.
#' @param zoom Zoom della mappa.
#' @param limite Numero massimo di bidoni disegnati uno per uno.
#' @return Lista con `tipo`: `"cantiere"` o `"comune"` per le bolle del
#'   territorio, `"tutti"` per tutti i bidoni, `"singoli"` per quelli
#'   dell'area inquadrata, `"griglia"` per le bolle per riquadro. Con
#'   `"singoli"` e `"griglia"` la lista riporta anche `riquadro`, l'area
#'   caricata, e `righe`, i bidoni che contiene.
#' @noRd
scegli_vista <- function(df, riquadro, zoom, limite = limiti_mappa()$marker) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(tipo = "tutti"))
  }
  livello <- livello_aggregazione(zoom)
  colonna <- c(cantiere = "cantiere", comune = "comune_assegnato")[livello]
  # Senza cantieri o comuni nel dataset non ci sono bolle del territorio.
  if (livello != "griglia" && !all(is.na(df[[colonna]]))) {
    return(list(tipo = livello))
  }
  if (nrow(df) <= limite) {
    return(list(tipo = "tutti"))
  }
  caricato <- if (!is.null(riquadro)) allarga_riquadro(riquadro)
  righe <- nel_riquadro(df, caricato)
  if (!is.null(riquadro) && sum(righe) <= limite) {
    return(list(tipo = "singoli", riquadro = caricato, righe = righe))
  }
  list(tipo = "griglia", riquadro = caricato, righe = righe)
}

#' La vista disegnata va rifatta dopo uno spostamento o uno zoom?
#'
#' Le bolle di cantieri e comuni coprono tutto il territorio e restano valide
#' finché lo zoom non cambia fascia, come tutti i bidoni disegnati insieme.
#' I bidoni dell'area inquadrata e la griglia valgono per l'area caricata; la
#' griglia cambia anche a ogni zoom.
#'
#' @param disegnata Vista disegnata: lista con `tipo`, `riquadro` e `zoom`.
#' @param nuova Vista scelta da `scegli_vista()` per la posizione attuale.
#' @param riquadro,zoom Area inquadrata e zoom attuali.
#' @noRd
vista_da_rifare <- function(disegnata, nuova, riquadro, zoom) {
  if (is.null(disegnata) || !identical(disegnata$tipo, nuova$tipo)) {
    return(TRUE)
  }
  switch(
    nuova$tipo,
    singoli = !riquadro_contiene(disegnata$riquadro, riquadro),
    griglia = !identical(disegnata$zoom, zoom) ||
      !riquadro_contiene(disegnata$riquadro, riquadro),
    FALSE
  )
}

#' La vista va disegnata subito, senza attendere la fine degli spostamenti?
#'
#' Le bolle di cantieri e comuni sono poche e non dipendono dall'area
#' inquadrata: quando lo zoom entra nella loro fascia si disegnano subito,
#' altrimenti le bolle del livello precedente resterebbero sulla mappa per un
#' terzo di secondo. Le altre viste aspettano: i singoli bidoni possono essere
#' migliaia, e sopra il limite la vista cambia a ogni spostamento.
#'
#' @param tipo Tipo della vista scelta da `scegli_vista()`.
#' @noRd
vista_immediata <- function(tipo) {
  tipo %in% c("cantiere", "comune")
}

#' Frase che descrive la vista della mappa, per la sua intestazione
#' @noRd
descrizione_vista <- function(tipo) {
  switch(
    tipo,
    cantiere = "una bolla per cantiere: ingrandisci per vedere i comuni",
    comune = "una bolla per comune: ingrandisci per vedere i singoli bidoni",
    griglia = "bolle per zona: ingrandisci per vedere i singoli bidoni",
    singoli = "solo i bidoni dell'area inquadrata",
    NULL
  )
}

#' Tiene le letture da disegnare nella ricerca RFID quando sono troppe
#'
#' Restano l'ultima lettura di ogni RFID e, fino al limite, le letture più
#' recenti.
#'
#' @param df Letture trovate, con la colonna `is_ultimo`.
#' @param massimo Numero massimo di letture da disegnare.
#' @return Le letture da disegnare. L'attributo `totale` riporta quante erano.
#' @noRd
limita_letture_ricerca <- function(
  df,
  massimo = limiti_mappa()$letture_ricerca
) {
  if (is.null(df)) {
    return(NULL)
  }
  totale <- nrow(df)
  if (totale > massimo) {
    ordine <- order(
      !df$is_ultimo,
      -as.numeric(df$giorno_lettura),
      method = "radix"
    )
    df <- df[sort(utils::head(ordine, massimo)), , drop = FALSE]
  }
  structure(df, totale = totale)
}
