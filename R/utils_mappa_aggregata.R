# Bolle per cantiere e per comune, calcolate dal server a ogni filtro; il
# browser mostra ogni livello nella sua fascia di zoom.

#' Limiti del disegno delle ricerche: letture in tutto e per RFID
#'
#' Opzioni `trackerfid.max_letture_ricerca` e `trackerfid.letture_per_rfid`.
#' @noRd
limiti_mappa <- function() {
  list(
    letture_ricerca = getOption("trackerfid.max_letture_ricerca", 50000),
    letture_per_rfid = getOption("trackerfid.letture_per_rfid", 10)
  )
}

#' Ultimo zoom di MapLibre con le bolle dei cantieri (10) e dei comuni (13)
#'
#' Lo zoom `z` di MapLibre ha la scala dello zoom `z + 1` di Leaflet.
#' @noRd
soglie_zoom <- function() {
  list(cantiere = 10, comune = 13)
}

#' Livello di uno zoom: `"cantiere"`, `"comune"` o `"bidoni"`, da `soglie_zoom()`
#' @noRd
livello_aggregazione <- function(zoom) {
  soglie <- soglie_zoom()
  if (is.null(zoom) || zoom < soglie$cantiere + 1) {
    "cantiere"
  } else if (zoom < soglie$comune + 1) {
    "comune"
  } else {
    "bidoni"
  }
}

#' Zoom a cui porta il click su una bolla: il primo del livello successivo
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

#' Bolle dei bidoni per cantiere o per comune
#'
#' La bolla di un comune sta nel suo baricentro (`baricentri_comuni()`);
#' quelle dei cantieri, e dei comuni fuori tabella, nel punto medio dei
#' bidoni. I bidoni senza cantiere o comune fanno una bolla senza nome.
#' @return Tibble con `livello`, `nome`, coordinate, `n`, `censiti` e
#'   riquadro.
#' @noRd
aggrega_marker <- function(df, livello = c("cantiere", "comune")) {
  livello <- match.arg(livello)
  if (is.null(df) || nrow(df) == 0) {
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
    comune = df[["comune_assegnato"]] %||% rep(NA_character_, nrow(df))
  )
  gruppo <- indice_gruppi(chiave)
  n_gruppi <- max(gruppo)
  lat <- estremi_per_gruppo(df$latitudine, gruppo, n_gruppi)
  lng <- estremi_per_gruppo(df$longitudine, gruppo, n_gruppi)
  nome <- primo_per_gruppo(chiave, gruppo, n_gruppi)
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
      df$presente_a_database == "PIA",
      gruppo,
      n_gruppi
    )),
    lat_min = lat$minimo,
    lat_max = lat$massimo,
    lng_min = lng$minimo,
    lng_max = lng$massimo
  )
}

#' Frase dell'intestazione sulla fascia di zoom
#' @noRd
descrizione_vista <- function(livello) {
  switch(
    livello,
    cantiere = "una bolla per cantiere: ingrandisci per vedere i comuni",
    comune = "una bolla per comune: ingrandisci per vedere i singoli bidoni",
    NULL
  )
}

#' Letture da disegnare nella ricerca RFID: le ultime `per_rfid` di ogni RFID
#'
#' Se sono ancora più di `massimo`, l'ultima di ogni RFID e le più recenti.
#' @return Nell'ordine di partenza, con l'attributo `totale`.
#' @noRd
limita_letture_ricerca <- function(
  df,
  per_rfid = limiti_mappa()$letture_per_rfid,
  massimo = limiti_mappa()$letture_ricerca
) {
  if (is.null(df)) {
    return(NULL)
  }
  totale <- nrow(df)
  if (totale > 0) {
    # Posizione di ogni lettura tra quelle del suo RFID, dalla più recente.
    ordine <- order(
      df$RFID,
      !df$is_ultimo,
      -as.numeric(df$data_ora_lettura),
      method = "radix"
    )
    posizione <- integer(totale)
    posizione[ordine] <- sequence(rle(df$RFID[ordine])$lengths)
    df <- df[posizione <= per_rfid, , drop = FALSE]
  }
  if (nrow(df) > massimo) {
    ordine <- order(
      !df$is_ultimo,
      -as.numeric(df$data_ora_lettura),
      method = "radix"
    )
    df <- df[sort(utils::head(ordine, massimo)), , drop = FALSE]
  }
  structure(df, totale = totale)
}
