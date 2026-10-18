# Bolle del territorio e fasce di zoom della mappa principale.
#
# Da lontano la mappa mostra una bolla per cantiere, avvicinandosi una bolla
# per comune, da vicino i singoli bidoni: i gruppi che si vedono sono quelli
# del territorio, non raggruppamenti casuali di marker vicini. Le bolle le
# calcola il server, una volta per ogni cambio di filtro; il passaggio da una
# fascia di zoom all'altra lo fa il browser, che mostra ogni livello solo
# nella sua fascia, senza chiedere niente al server. Da vicino i bidoni
# vicini si raggruppano nei cluster calcolati dal browser.

#' Limiti del disegno delle mappe
#'
#' Si possono cambiare con le opzioni `trackerfid.max_letture_ricerca` e
#' `trackerfid.letture_per_rfid`.
#'
#' @return Lista con `letture_ricerca`, il numero massimo di letture
#'   disegnate dalla ricerca RFID e dalla ricerca dei giri, e
#'   `letture_per_rfid`, le letture più recenti di ogni RFID che la ricerca
#'   RFID disegna: le altre stanno solo nella tabella delle letture.
#' @noRd
limiti_mappa <- function() {
  list(
    letture_ricerca = getOption("trackerfid.max_letture_ricerca", 50000),
    letture_per_rfid = getOption("trackerfid.letture_per_rfid", 10)
  )
}

#' Zoom fino a cui la mappa mostra le bolle di cantieri e comuni
#'
#' Sono zoom di MapLibre, che disegna tessere da 512 pixel: lo zoom `z` di
#' MapLibre mostra la scala dello zoom `z + 1` di Leaflet. Le fasce sono
#' quelle scelte dal proprietario del progetto con Leaflet, cantieri fino a
#' zoom 11 e comuni fino a zoom 14, alla stessa scala sullo schermo.
#'
#' @return Lista con `cantiere` e `comune`: l'ultimo zoom intero a cui si
#'   vede una bolla per cantiere e l'ultimo a cui se ne vede una per comune.
#' @noRd
soglie_zoom <- function() {
  list(cantiere = 10, comune = 13)
}

#' Livello di aggregazione adatto a uno zoom
#'
#' Con la zona servita inquadrata per intero lo zoom è 9 o meno: si vedono
#' i cantieri, e restano anche a zoom 10, quando un cantiere occupa buona
#' parte della mappa. Da zoom 11 a zoom 13 si vedono i comuni: a 11 un comune
#' occupa un paio di centinaia di pixel, a 13 più della mappa. Da 14 in su si
#' vedono i singoli bidoni. Lo zoom di MapLibre ha anche i decimali: la fascia
#' dei cantieri arriva fin sotto lo zoom 11. Le soglie stanno in
#' `soglie_zoom()`; il browser le applica ai livelli della mappa, vedi
#' `livelli_mappa()`.
#'
#' @param zoom Zoom della mappa.
#' @return `"cantiere"` sotto lo zoom 11, `"comune"` sotto lo zoom 14, poi
#'   `"bidoni"`.
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

#' Zoom minimo a cui porta il click su una bolla
#'
#' Il click su una bolla deve aprirla: quella di un cantiere mostra i suoi
#' comuni, quella di un comune i suoi bidoni. Lo zoom a cui i bidoni della
#' bolla entrano nella mappa può non bastare: un cantiere entra per intero a
#' uno zoom a cui si vedono ancora i cantieri.
#'
#' @param livello `"cantiere"` oppure `"comune"`.
#' @return Il primo zoom del livello successivo.
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

#' Raggruppa i bidoni in bolle
#'
#' Ogni bolla riporta il numero dei suoi bidoni, i censiti e il riquadro che
#' li contiene. La bolla di un comune sta nel baricentro del comune, vedi
#' `baricentri_comuni()`: resta ferma qualunque siano i bidoni filtrati, in
#' mezzo al confine disegnato sulla mappa. Le bolle dei cantieri, e quelle di
#' un comune che non è nella tabella dei comuni, stanno nel punto medio dei
#' loro bidoni.
#'
#' @param df Una riga per bidone, con stato a database, coordinate, `cantiere`
#'   e `comune_assegnato`.
#' @param livello `"cantiere"` oppure `"comune"`.
#' @return Tibble con `livello`, `nome`, `latitudine`, `longitudine`, `n`,
#'   `censiti`, `lat_min`, `lat_max`, `lng_min`, `lng_max`. I bidoni senza
#'   cantiere o senza comune formano una bolla con nome mancante.
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

#' Frase che descrive la vista della mappa, per la sua intestazione
#'
#' @param livello Livello di `livello_aggregazione()`.
#' @noRd
descrizione_vista <- function(livello) {
  switch(
    livello,
    cantiere = "una bolla per cantiere: ingrandisci per vedere i comuni",
    comune = "una bolla per comune: ingrandisci per vedere i singoli bidoni",
    NULL
  )
}

#' Tiene le letture da disegnare nella ricerca RFID
#'
#' Per ogni RFID restano le sue letture più recenti, al massimo `per_rfid`:
#' con centinaia di letture nello stesso punto i marker si coprono, e la
#' storia completa sta nella tabella delle letture. Se sono ancora troppe,
#' restano l'ultima lettura di ogni RFID e, fino a `massimo`, le più recenti.
#'
#' @param df Letture trovate, con la colonna `is_ultimo`.
#' @param per_rfid Numero massimo di letture disegnate per ogni RFID.
#' @param massimo Numero massimo di letture da disegnare.
#' @return Le letture da disegnare, nell'ordine di partenza. L'attributo
#'   `totale` riporta quante erano.
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
      -as.numeric(df$giorno_lettura),
      method = "radix"
    )
    posizione <- integer(totale)
    posizione[ordine] <- sequence(rle(df$RFID[ordine])$lengths)
    df <- df[posizione <= per_rfid, , drop = FALSE]
  }
  if (nrow(df) > massimo) {
    ordine <- order(
      !df$is_ultimo,
      -as.numeric(df$giorno_lettura),
      method = "radix"
    )
    df <- df[sort(utils::head(ordine, massimo)), , drop = FALSE]
  }
  structure(df, totale = totale)
}
