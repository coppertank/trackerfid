# Stile di marker, cluster e legenda.
#
# I marker sono icone SVG generate al volo: il colore dello sfondo indica lo
# stato a database (verde = Presente, rosso = Non Presente), il glifo Font
# Awesome al centro indica il servizio. Per chi non distingue rosso e verde lo
# stato è ripetuto dalla forma (goccia = censito, quadrato = non censito).

#' Colori usati per lo stato a database
#' @noRd
colori_stato <- function() {
  list(
    presente = "#2ecc71",
    non_presente = "#e74c3c",
    misto = "#f39c12",
    bordo = "#1f2d3a"
  )
}

#' Colori dei cantieri sulla mappa
#'
#' Una tinta per cantiere, usata dai confini dei comuni e dalla legenda. Le
#' quattro tinte si distinguono a coppie anche per chi non distingue rosso e
#' verde, e stanno lontane dal verde e dal rosso dello stato a database e dai
#' colori dei cluster. Il colore segue il nome del cantiere, non la sua
#' posizione nell'elenco.
#'
#' @return Vettore di colori con i nomi dei cantieri.
#' @noRd
colori_cantieri <- function() {
  c(
    ASIAGO = "#5b3a9e",
    BASSANO = "#8f5a0a",
    CAMPOSAMPIERO = "#2a78d6",
    RUBANO = "#c2548f"
  )
}

#' Colore di un cantiere
#'
#' @param cantiere Nomi di cantiere.
#' @return Il colore di `colori_cantieri()`, grigio per un cantiere che non ne
#'   ha uno.
#' @noRd
colore_cantiere <- function(cantiere) {
  dplyr::coalesce(
    unname(colori_cantieri()[as.character(cantiere)]),
    "#6b7280"
  )
}

#' Stile dei confini dei comuni sulle mappe
#'
#' Bordo sottile e riempimento appena accennato, nel colore del cantiere: i
#' confini devono leggersi senza coprire lo sfondo né i marker. Il bordo è
#' tratteggiato, come i confini amministrativi sulle carte: così non si
#' confonde con le strade dello sfondo.
#'
#' @return Lista con `weight`, `opacity`, `fillOpacity` e `dashArray`.
#' @noRd
stile_confini <- function() {
  list(weight = 1.5, opacity = 0.75, fillOpacity = 0.1, dashArray = "5 4")
}

#' Tipologie di servizio: icona Font Awesome, colore del glifo, emoji
#'
#' PUNTO UNICO DA MODIFICARE per aggiungere servizi o cambiare icone.
#' Ogni tipologia raggruppa i nomi che condividono la stessa icona: i nomi
#' del database, usati in `servizio_transponder`, e i nomi dei giri del
#' calendario, usati in `servizio_giro` (PAP = porta a porta). Il nome della
#' tipologia, in maiuscolo, è il nome uniforme a cui la tabella di conversione
#' riporta tutti gli altri, vedi `conversione_servizi()`: è quello di
#' `servizio_atteso`, ed è sempre un nome riconosciuto. I sacchetti hanno una
#' tipologia loro: non hanno un servizio a database, e l'app li riconosce dal
#' codice RFID, vedi `rfid_sacchetto()`. Per un nuovo nome basta aggiungerlo
#' al vettore `servizi` della tipologia giusta; per una nuova tipologia si
#' aggiunge una voce alla lista. I nomi vanno scritti in maiuscolo, come dopo
#' il caricamento del CSV. Le icone sono nomi Font Awesome
#' (https://fontawesome.com/icons). L'ordine qui è anche l'ordine di filtri,
#' statistiche e legenda.
#' @noRd
tipologie_servizio <- function() {
  list(
    list(
      tipologia = "Secco",
      icona = "trash-can",
      colore = "#7f8c8d",
      emoji = "\U0001F5D1\ufe0f",
      servizi = c("SECCO", "SECCO PAP", "SCARICO SECCO", "SECCO CONT.STRADALI")
    ),
    list(
      tipologia = "Carta",
      icona = "file-lines",
      colore = "#8e5b2e",
      emoji = "\U0001F4C4",
      servizi = c(
        "CARTA",
        "CARTA CONT.STRADALI",
        "CARTA/CARTONE PAP",
        "SCARICO CARTA-CART."
      )
    ),
    list(
      tipologia = "Vetro",
      icona = "wine-bottle",
      colore = "#2471a3",
      emoji = "\U0001F37E",
      servizi = c("VETRO", "VETRO PAP", "VETRO CONT.STRADALI")
    ),
    list(
      tipologia = "Umido",
      icona = "leaf",
      colore = "#1e8449",
      emoji = "\U0001F342",
      servizi = c("UMIDO", "UMIDO CONT.STRADALI", "UMIDO PAP")
    ),
    list(
      tipologia = "Plastica e metalli",
      icona = "recycle",
      colore = "#0e9aa7",
      emoji = "\u267b\ufe0f",
      servizi = c(
        "PLASTICA E METALLI",
        "PLAST.CONT.STRADALI",
        "PLASTICA PAP",
        "SCARICO PLASTICA"
      )
    ),
    list(
      tipologia = "Verde e ramaglie",
      icona = "tree",
      colore = "#6b8e23",
      emoji = "\U0001F333",
      servizi = c("VERDE E RAMAGLIE", "VERDE PAP", "SCARICO VERDE")
    ),
    list(
      tipologia = "Sacchetti",
      icona = "sack-xmark",
      colore = "#ad1457",
      emoji = "\U0001F6CD\ufe0f",
      servizi = servizio_sacchetti()
    ),
    list(
      tipologia = "Pulizia territorio",
      icona = "broom",
      colore = "#b9770e",
      emoji = "\U0001F9F9",
      servizi = "PULIZIA TERRIT."
    ),
    list(
      tipologia = "Servizi",
      icona = "store",
      colore = "#884ea0",
      emoji = "\U0001F3EA",
      servizi = c("SERVIZI MERCATI", "SERV.MANIFESTAZIONE")
    )
  )
}

#' Configurazione dei servizi: una riga per nome di servizio
#'
#' Tabella derivata da `tipologie_servizio()`. Il nome uniforme di una
#' tipologia, cioè il suo nome in maiuscolo, è sempre tra i nomi: è quello
#' del servizio atteso, vedi `conversione_servizi()`.
#' @noRd
servizi_config <- function() {
  righe <- purrr::map(tipologie_servizio(), function(tp) {
    data.frame(
      servizio = union(tp$servizi, stringr::str_to_upper(tp$tipologia)),
      tipologia = tp$tipologia,
      icona = tp$icona,
      colore = tp$colore,
      emoji = tp$emoji,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, righe)
}

#' Icona, colore ed emoji di uno o più servizi
#'
#' I servizi mancanti (non censiti) o non previsti usano il punto di domanda.
#'
#' @param servizio Vettore di servizi.
#' @return Data frame con una riga per elemento di `servizio`.
#' @noRd
info_servizio <- function(servizio) {
  config <- servizi_config()
  indice <- match(servizio, config$servizio)
  noto <- !is.na(indice)
  data.frame(
    servizio = servizio,
    icona = ifelse(noto, config$icona[indice], "question"),
    colore = ifelse(noto, config$colore[indice], "#566573"),
    emoji = ifelse(noto, config$emoji[indice], "\u2753"),
    stringsAsFactors = FALSE
  )
}

#' Tipologia di uno o più servizi
#'
#' @param servizio Vettore di servizi.
#' @return La tipologia di ciascun servizio; un nome non previsto fa tipologia a sé.
#' @noRd
tipologia_servizio <- function(servizio) {
  config <- servizi_config()
  indice <- match(servizio, config$servizio)
  ifelse(is.na(indice), servizio, config$tipologia[indice])
}

#' Tabella di conversione dei nomi dei servizi
#'
#' Il calendario dei mezzi chiama i giri con nomi suoi, ad esempio
#' `CARTA/CARTONE PAP` e `CARTA CONT.STRADALI`. Il database chiama il rifiuto
#' in un modo solo: `CARTA`. La tabella riporta ogni nome a quello della sua
#' tipologia, così il servizio di un giro e quello di un contenitore si
#' confrontano senza passaggi.
#'
#' Viene da `tipologie_servizio()`: il nome uniforme è il nome della
#' tipologia, in maiuscolo. Per le sei tipologie di rifiuto è anche il nome
#' del database; i giri che non raccolgono un rifiuto diventano ad esempio
#' `PULIZIA TERRITORIO` e `SERVIZI`. Per aggiungere un nome del calendario
#' basta aggiungerlo ai `servizi` della sua tipologia.
#'
#' @return Tibble con `servizio_calendario`, il nome da convertire,
#'   `servizio`, il nome uniforme, e `tipologia`.
#' @noRd
conversione_servizi <- function() {
  righe <- purrr::map(tipologie_servizio(), function(tp) {
    dplyr::tibble(
      servizio_calendario = tp$servizi,
      servizio = stringr::str_to_upper(tp$tipologia),
      tipologia = tp$tipologia
    )
  })
  dplyr::bind_rows(righe)
}

#' Riporta i nomi dei servizi a quelli del database
#'
#' Maiuscole e spazi ai margini non contano. Un nome che la tabella di
#' `conversione_servizi()` non contiene resta com'è.
#'
#' @param servizio Nomi di servizi o di giri.
#' @return I nomi uniformi, uno per elemento.
#' @noRd
uniforma_servizio <- function(servizio) {
  per_valori_distinti(as.character(servizio), function(nomi) {
    tabella <- conversione_servizi()
    riga <- match(
      stringr::str_to_upper(stringr::str_trim(nomi)),
      tabella$servizio_calendario
    )
    dplyr::coalesce(tabella$servizio[riga], nomi)
  })
}

#' Colore del marker in base allo stato a database
#'
#' @param presente Vettore con i valori di `presente_a_database`.
#' @return Colori HEX: verde per "Presente", rosso negli altri casi.
#' @noRd
get_color <- function(presente) {
  colori <- colori_stato()
  ifelse(
    !is.na(presente) & presente == "Presente",
    colori$presente,
    colori$non_presente
  )
}

#' Genera colore cluster basato su percentuale Presente
#'
#' Interpolazione lineare rosso (0%) - giallo (50%) - verde (100%).
#'
#' @param pct_presente Numeric tra 0 e 1
#' @return Colore HEX string
#' @noRd
genera_colore_cluster <- function(pct_presente) {
  colori <- colori_stato()
  rosso <- grDevices::col2rgb(colori$non_presente)[, 1]
  giallo <- grDevices::col2rgb(colori$misto)[, 1]
  verde <- grDevices::col2rgb(colori$presente)[, 1]

  pct_presente <- pmin(pmax(pct_presente, 0), 1)
  purrr::map_chr(pct_presente, function(pct) {
    if (pct < 0.5) {
      ratio <- pct * 2
      canali <- rosso + (giallo - rosso) * ratio
    } else {
      ratio <- (pct - 0.5) * 2
      canali <- giallo + (verde - giallo) * ratio
    }
    tolower(grDevices::rgb(
      round(canali[1]),
      round(canali[2]),
      round(canali[3]),
      maxColorValue = 255
    ))
  })
}

# Cache dei glifi Font Awesome già estratti.
.cache_glifi <- new.env(parent = emptyenv())

#' Estrae viewBox e tracciato di un'icona Font Awesome
#' @noRd
glifo_fa <- function(nome) {
  if (!is.null(.cache_glifi[[nome]])) {
    return(.cache_glifi[[nome]])
  }
  svg <- as.character(fontawesome::fa(nome))
  glifo <- list(
    viewBox = stringr::str_match(svg, "viewBox=\"([^\"]+)\"")[, 2],
    path = stringr::str_match(svg, "<path d=\"([^\"]+)\"")[, 2]
  )
  assign(nome, glifo, envir = .cache_glifi)
  glifo
}

#' Frammento SVG con un glifo Font Awesome colorato
#' @noRd
svg_glifo <- function(nome, colore, x = 0, y = 0, lato = 16) {
  glifo <- glifo_fa(nome)
  paste0(
    "<svg x=\"",
    x,
    "\" y=\"",
    y,
    "\" width=\"",
    lato,
    "\" height=\"",
    lato,
    "\" viewBox=\"",
    glifo$viewBox,
    "\" preserveAspectRatio=\"xMidYMid meet\">",
    "<path d=\"",
    glifo$path,
    "\" fill=\"",
    colore,
    "\"/></svg>"
  )
}

#' Icona SVG autonoma con il solo glifo di un servizio (per la legenda)
#' @noRd
svg_icona_servizio <- function(servizio, lato = 16) {
  info <- info_servizio(servizio)
  paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"",
    lato,
    "\" height=\"",
    lato,
    "\" viewBox=\"0 0 ",
    lato,
    " ",
    lato,
    "\">",
    svg_glifo(info$icona, info$colore, 0, 0, lato),
    "</svg>"
  )
}

#' Disegna il marker SVG di un contenitore
#'
#' @param servizio Servizio del contenitore (un solo valore, anche `NA`).
#' @param presente Valore di `presente_a_database`.
#' @param stile `"normale"`, `"ultimo"` (bordo spesso) o `"precedente"`
#'   (bordo sottile tratteggiato): gli ultimi due servono nella ricerca RFID.
#' @param glifo Se `FALSE` il marker non riporta l'icona del servizio.
#' @return Stringa SVG.
#' @noRd
svg_marker <- function(
  servizio,
  presente,
  stile = "normale",
  glifo = TRUE,
  fuori_comune = FALSE,
  fuori_giro = FALSE
) {
  colori <- colori_stato()
  censito <- !is.na(presente) && presente == "Presente"
  sagoma <- if (censito) {
    "M18 45.5C16.5 39 4 28.5 4 17a14 14 0 1 1 28 0c0 11.5-12.5 22-14 28.5z"
  } else {
    "M8.5 3h19a4.5 4.5 0 0 1 4.5 4.5v19a4.5 4.5 0 0 1-4.5 4.5H24l-6 14.5-6-14.5H8.5A4.5 4.5 0 0 1 4 26.5v-19A4.5 4.5 0 0 1 8.5 3z"
  }
  tratto <- switch(
    stile,
    ultimo = "stroke-width=\"3\"",
    precedente = "stroke-width=\"1\" stroke-dasharray=\"5,5\"",
    "stroke-width=\"1.5\""
  )
  info <- info_servizio(servizio)
  paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"36\" height=\"48\" viewBox=\"0 0 36 48\">",
    "<path d=\"",
    sagoma,
    "\" fill=\"",
    get_color(presente),
    "\" stroke=\"",
    colori$bordo,
    "\" stroke-linejoin=\"round\" ",
    tratto,
    "/>",
    "<circle cx=\"18\" cy=\"17\" r=\"10.5\" fill=\"#ffffff\"/>",
    if (glifo) svg_glifo(info$icona, info$colore, 11, 10, 14),
    if (isTRUE(fuori_comune)) svg_segnale("C", 7.5),
    if (isTRUE(fuori_giro)) svg_segnale("G", 28.5),
    "</svg>"
  )
}

#' Segnale di incongruenza sul marker
#'
#' Un bollino scuro con una lettera, in un angolo del marker: `C` per una
#' lettura fuori comune, `G` per una fuori giro. Non cambia colore, forma né
#' icona del marker, che continuano a dire stato e servizio.
#'
#' @param lettera Lettera del segnale.
#' @param x Ascissa del centro del bollino, nel riquadro del marker.
#' @noRd
svg_segnale <- function(lettera, x) {
  sprintf(
    paste0(
      "<circle cx=\"%s\" cy=\"7.5\" r=\"7\" fill=\"%s\" stroke=\"#ffffff\" stroke-width=\"1.5\"/>",
      "<text x=\"%s\" y=\"10.9\" text-anchor=\"middle\" font-family=\"Arial, Helvetica, sans-serif\" ",
      "font-size=\"9.5\" font-weight=\"bold\" fill=\"#ffffff\">%s</text>"
    ),
    x,
    colori_stato()$bordo,
    x,
    lettera
  )
}

#' Converte un SVG in data URI utilizzabile come immagine
#' @noRd
uri_svg <- function(svg) {
  paste0(
    "data:image/svg+xml;charset=utf-8,",
    utils::URLencode(svg, reserved = TRUE)
  )
}

#' Lato in pixel della bolla di un gruppo di bidoni
#'
#' Cresce con il numero dei bidoni, come i cluster disegnati dal browser.
#' @noRd
lato_bolla <- function(n) {
  dplyr::case_when(
    n < 10 ~ 40,
    n < 100 ~ 46,
    n < 1000 ~ 54,
    n < 10000 ~ 62,
    .default = 72
  )
}

#' Legenda della mappa
#'
#' @param modalita `"cluster"`, `"rfid"` o `"utenza"`.
#' @return Stringa HTML.
#' @noRd
html_legenda <- function(modalita = "cluster") {
  tags <- htmltools::tags
  immagine <- function(svg, larghezza, altezza) {
    tags$img(src = uri_svg(svg), width = larghezza, height = altezza, alt = "")
  }
  voce <- function(icona, testo) {
    tags$div(
      class = "legenda-voce",
      tags$span(class = "legenda-icona", icona),
      tags$span(testo)
    )
  }

  # Una voce per tipologia: i servizi con la stessa icona sono raggruppati.
  voci_servizio <- purrr::map(tipologie_servizio(), function(tp) {
    voce(immagine(svg_icona_servizio(tp$servizi[1]), 14, 14), tp$tipologia)
  })
  voci_servizio <- c(
    voci_servizio,
    list(
      voce(immagine(svg_icona_servizio(NA), 14, 14), "Non determinato"),
      tags$div(
        class = "legenda-nota",
        "Non censiti: servizio atteso prevalente (almeno 80%)"
      )
    )
  )

  extra <- switch(
    modalita,
    cluster = list(
      tags$div(class = "legenda-titolo", "Cluster: % censiti"),
      tags$div(
        class = "legenda-gradiente",
        style = sprintf(
          "background: linear-gradient(to right, %s);",
          paste(genera_colore_cluster(c(0, 0.5, 1)), collapse = ", ")
        )
      ),
      tags$div(
        class = "legenda-scala",
        tags$span("0%"),
        tags$span("50%"),
        tags$span("100%")
      )
    ),
    rfid = list(
      tags$div(class = "legenda-titolo", "Letture"),
      voce(
        immagine(svg_marker(NA, "Presente", "ultimo", glifo = FALSE), 15, 20),
        "Ultima lettura"
      ),
      voce(
        immagine(
          svg_marker(NA, "Presente", "precedente", glifo = FALSE),
          15,
          20
        ),
        "Letture precedenti"
      ),
      voce(tags$span(class = "legenda-riquadro"), "Area di spostamento"),
      tags$div(
        class = "legenda-nota",
        sprintf(
          "Sulla mappa le ultime %d letture di ogni RFID; l'area le comprende tutte",
          limiti_mappa()$letture_per_rfid
        )
      )
    ),
    giro = list(
      tags$div(class = "legenda-titolo", "Giro"),
      voce(tags$span(class = "legenda-comune-giro"), "Comune del giro")
    ),
    NULL
  )
  # Segnali sui marker: comune di lettura diverso da quello dichiarato.
  segnale <- function(...) {
    immagine(svg_marker(NA, "Presente", glifo = FALSE, ...), 15, 20)
  }
  voci_coerenza_comune <- list(
    tags$div(class = "legenda-titolo", "Coerenza comune"),
    voce(
      segnale(fuori_comune = TRUE),
      "C: censito fuori dal comune del database"
    ),
    voce(
      segnale(fuori_giro = TRUE),
      "G: non censito fuori dal comune del giro"
    )
  )

  # Una voce per cantiere, con il colore dei confini dei suoi comuni.
  stile <- stile_confini()
  voci_cantiere <- purrr::imap(colori_cantieri(), function(colore, cantiere) {
    rgb <- grDevices::col2rgb(colore)[, 1]
    voce(
      tags$span(
        class = "legenda-cantiere",
        style = sprintf(
          "border-color: %s; background: rgba(%d, %d, %d, %s);",
          colore,
          rgb[["red"]],
          rgb[["green"]],
          rgb[["blue"]],
          format(2 * stile$fillOpacity)
        )
      ),
      cantiere
    )
  })

  # La legenda nasce chiusa, a ogni avvio dell'app, e si apre con un click
  # sull'intestazione: `script.js` applica la scelta a tutte le mappe.
  as.character(tags$details(
    class = "legenda-dettagli",
    tags$summary(
      class = "legenda-intestazione",
      title = "Mostra o nascondi la legenda",
      "Legenda"
    ),
    tags$div(class = "legenda-titolo", "Stato a database"),
    voce(
      immagine(svg_marker(NA, "Presente", glifo = FALSE), 15, 20),
      "Presente (censito)"
    ),
    voce(
      immagine(svg_marker(NA, "Non Presente", glifo = FALSE), 15, 20),
      "Non Presente"
    ),
    tags$div(class = "legenda-titolo", "Servizio"),
    voci_servizio,
    voci_coerenza_comune,
    extra,
    tags$div(class = "legenda-titolo", "Confini dei comuni: cantiere"),
    unname(voci_cantiere)
  ))
}
