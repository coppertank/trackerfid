# Vista aggregata della mappa principale.
#
# Finché i bidoni da disegnare sono pochi la mappa li riceve tutti e li
# raggruppa nel browser. Oltre il limite di `limiti_mappa()` il browser
# riceverebbe decine di megabyte a ogni cambio di filtro: la mappa mostra
# allora una bolla per cantiere, per comune o per riquadro, secondo lo zoom, e
# i singoli bidoni solo quando quelli nell'area inquadrata tornano sotto il
# limite. I conti restano sul server, che conosce l'area inquadrata.

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

#' Livello di aggregazione adatto a uno zoom
#'
#' @param zoom Zoom della mappa.
#' @return `"cantiere"` fino a zoom 10, `"comune"` a 11 e 12, poi `"griglia"`.
#' @noRd
livello_aggregazione <- function(zoom) {
  if (is.null(zoom) || zoom <= 10) {
    "cantiere"
  } else if (zoom <= 12) {
    "comune"
  } else {
    "griglia"
  }
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
#' Ogni bolla sta nel punto medio dei suoi bidoni e ne riporta il numero, i
#' censiti e il riquadro che li contiene.
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
  dplyr::tibble(
    livello = livello,
    nome = if (livello == "griglia") {
      rep(NA_character_, n_gruppi)
    } else {
      primo_per_gruppo(chiave, gruppo, n_gruppi)
    },
    latitudine = media_per_gruppo(df$latitudine, gruppo, n_gruppi),
    longitudine = media_per_gruppo(df$longitudine, gruppo, n_gruppi),
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
#' @param df Una riga per bidone, oppure `NULL`.
#' @param riquadro Area inquadrata, oppure `NULL` se non è ancora nota.
#' @param zoom Zoom della mappa.
#' @param limite Numero massimo di bidoni disegnati uno per uno.
#' @return Lista con `tipo`: `"tutti"` se i bidoni stanno tutti sotto il
#'   limite, `"singoli"` se ci stanno quelli dell'area inquadrata, altrimenti
#'   il livello di aggregazione. Con `"singoli"` e `"griglia"` la lista
#'   riporta anche `riquadro`, l'area caricata, e `righe`, i bidoni che
#'   contiene.
#' @noRd
scegli_vista <- function(df, riquadro, zoom, limite = limiti_mappa()$marker) {
  if (is.null(df) || nrow(df) <= limite) {
    return(list(tipo = "tutti"))
  }
  caricato <- if (!is.null(riquadro)) allarga_riquadro(riquadro)
  righe <- nel_riquadro(df, caricato)
  if (!is.null(riquadro) && sum(righe) <= limite) {
    return(list(tipo = "singoli", riquadro = caricato, righe = righe))
  }
  livello <- livello_aggregazione(zoom)
  # Senza cantieri o comuni nel dataset resta solo la griglia.
  colonna <- c(cantiere = "cantiere", comune = "comune_assegnato")[livello]
  if (livello != "griglia" && all(is.na(df[[colonna]]))) {
    livello <- "griglia"
  }
  if (livello == "griglia") {
    return(list(tipo = "griglia", riquadro = caricato, righe = righe))
  }
  list(tipo = livello)
}

#' La vista disegnata va rifatta dopo uno spostamento o uno zoom?
#'
#' Le bolle di cantieri e comuni coprono tutto il territorio e restano valide
#' finché lo zoom non cambia fascia. I singoli bidoni e la griglia valgono
#' per l'area caricata; la griglia cambia anche a ogni zoom.
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

#' Frase che descrive la vista aggregata, per l'intestazione della mappa
#' @noRd
descrizione_vista <- function(tipo) {
  switch(
    tipo,
    cantiere = "una bolla per cantiere: ingrandisci per il dettaglio",
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
