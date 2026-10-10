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

#' Tipologie di servizio: icona Font Awesome, colore del glifo, emoji
#'
#' PUNTO UNICO DA MODIFICARE per aggiungere servizi o cambiare icone.
#' Ogni tipologia raggruppa i nomi che condividono la stessa icona: i valori
#' di `servizio_transponder` e i nomi dei giri usati in `servizio_atteso`
#' (PAP = porta a porta). I sacchetti hanno una tipologia loro: non hanno un
#' servizio a database, e l'app glielo assegna riconoscendoli dal codice
#' RFID, vedi `rfid_sacchetto()`. Per un nuovo nome basta aggiungerlo al vettore
#' `servizi` della tipologia giusta; per una nuova tipologia si aggiunge una
#' voce alla lista. I nomi vanno scritti in maiuscolo, come dopo il caricamento
#' del CSV. Le icone sono nomi Font Awesome (https://fontawesome.com/icons).
#' L'ordine qui è anche l'ordine di filtri, statistiche e legenda.
#' @noRd
tipologie_servizio <- function() {
  list(
    list(
      tipologia = "Secco",
      icona = "trash-can",
      colore = "#7f8c8d",
      emoji = "\U0001F5D1\ufe0f",
      servizi = c("SECCO", "SECCO PAP")
    ),
    list(
      tipologia = "Carta",
      icona = "file-lines",
      colore = "#8e5b2e",
      emoji = "\U0001F4C4",
      servizi = c("CARTA", "CARTA CONT.STRADALI", "CARTA/CARTONE PAP")
    ),
    list(
      tipologia = "Vetro",
      icona = "wine-bottle",
      colore = "#2471a3",
      emoji = "\U0001F37E",
      servizi = c("VETRO", "VETRO PAP")
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
      servizi = c("PLASTICA E METALLI", "PLAST.CONT.STRADALI", "PLASTICA PAP")
    ),
    list(
      tipologia = "Verde e ramaglie",
      icona = "tree",
      colore = "#6b8e23",
      emoji = "\U0001F333",
      servizi = c("VERDE E RAMAGLIE", "VERDE PAP")
    ),
    list(
      tipologia = "Sacchetti",
      icona = "sack-xmark",
      colore = "#ad1457",
      emoji = "\U0001F6CD\ufe0f",
      servizi = servizio_sacchetti()
    ),
    list(
      tipologia = "Assistente servizi",
      icona = "user-gear",
      colore = "#5d6d7e",
      emoji = "\U0001F6E0\ufe0f",
      servizi = "ASSISTENTE SERVIZI"
    ),
    list(
      tipologia = "Pulizia territorio",
      icona = "broom",
      colore = "#b9770e",
      emoji = "\U0001F9F9",
      servizi = "PULIZIA TERRIT."
    ),
    list(
      tipologia = "Servizi mercati",
      icona = "store",
      colore = "#884ea0",
      emoji = "\U0001F3EA",
      servizi = "SERVIZI MERCATI"
    )
  )
}

#' Configurazione dei servizi: una riga per nome di servizio
#'
#' Tabella derivata da `tipologie_servizio()`.
#' @noRd
servizi_config <- function() {
  righe <- purrr::map(tipologie_servizio(), function(tp) {
    data.frame(
      servizio = tp$servizi,
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
#' @param servizio Servizio transponder (un solo valore, anche `NA`).
#' @param presente Valore di `presente_a_database`.
#' @param stile `"normale"`, `"ultimo"` (bordo spesso) o `"precedente"`
#'   (bordo sottile tratteggiato): gli ultimi due servono nella ricerca RFID.
#' @param glifo Se `FALSE` il marker non riporta l'icona del servizio.
#' @return Stringa SVG.
#' @noRd
svg_marker <- function(servizio, presente, stile = "normale", glifo = TRUE) {
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
    "</svg>"
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

#' Genera icone Leaflet per servizio e stato a database
#'
#' @param servizio Vettore di `servizio_transponder`.
#' @param presente Vettore di `presente_a_database`.
#' @param stile Stile del bordo, vedi `svg_marker()`.
#' @return Oggetto icone di Leaflet, un'icona per elemento.
#' @noRd
get_leaflet_icon <- function(servizio, presente, stile = "normale") {
  n <- length(servizio)
  presente <- rep_len(presente, n)
  stile <- rep_len(stile, n)

  # Ogni combinazione viene disegnata una sola volta.
  chiave <- paste(servizio, presente, stile, sep = "|")
  uniche <- !duplicated(chiave)
  uri <- purrr::pmap_chr(
    list(servizio[uniche], presente[uniche], stile[uniche]),
    function(s, p, st) uri_svg(svg_marker(s, p, st))
  )

  leaflet::icons(
    iconUrl = uri[match(chiave, chiave[uniche])],
    iconWidth = 36,
    iconHeight = 48,
    iconAnchorX = 18,
    iconAnchorY = 46,
    popupAnchorX = 1,
    popupAnchorY = -40
  )
}

#' Funzione JavaScript che disegna l'icona di un cluster
#'
#' Il colore segue la percentuale di contenitori censiti nel cluster con la
#' stessa interpolazione di `genera_colore_cluster()`. Ogni marker porta
#' l'opzione `censito`, impostata in `aggiungi_marker()`.
#' @noRd
js_icona_cluster <- function() {
  colori <- colori_stato()
  canali <- function(hex) {
    paste0("[", paste(grDevices::col2rgb(hex)[, 1], collapse = ","), "]")
  }
  paste0(
    "function(cluster) {
      var figli = cluster.getAllChildMarkers();
      var n = figli.length, censiti = 0;
      for (var i = 0; i < n; i++) { if (figli[i].options.censito) censiti++; }
      var pct = n > 0 ? censiti / n : 0;
      var rosso = ",
    canali(colori$non_presente),
    ", giallo = ",
    canali(colori$misto),
    ", verde = ",
    canali(colori$presente),
    ";
      var da = pct < 0.5 ? rosso : giallo, a = pct < 0.5 ? giallo : verde;
      var ratio = pct < 0.5 ? pct * 2 : (pct - 0.5) * 2;
      var c = [0, 1, 2].map(function(k) { return Math.round(da[k] + (a[k] - da[k]) * ratio); });
      var lato = n < 10 ? 40 : (n < 100 ? 46 : 54);
      var pctTesto = Math.round(pct * 100) + '%';
      var stile = 'width:' + lato + 'px;height:' + lato + 'px;border-radius:50%;' +
        'background-color:rgb(' + c.join(',') + ');border:3px solid rgba(255,255,255,0.9);' +
        'box-shadow:0 1px 5px rgba(0,0,0,0.45);box-sizing:border-box;display:flex;' +
        'flex-direction:column;align-items:center;justify-content:center;' +
        'color:",
    colori$bordo,
    ";font-family:Arial,sans-serif;line-height:1.05;' +
        'text-shadow:0 0 3px rgba(255,255,255,0.8);';
      return L.divIcon({
        html: '<div class=\"cluster-rfid-interno\" style=\"' + stile + '\" title=\"' + censiti +
          ' censiti su ' + n + ' (' + pctTesto + ')\">' +
          '<span style=\"font-size:13px;font-weight:700;\">' + n + '</span>' +
          '<span style=\"font-size:9px;font-weight:600;\">' + pctTesto + '</span></div>',
        className: 'cluster-rfid',
        iconSize: L.point(lato, lato)
      });
    }"
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

#' Bolla SVG di un gruppo di bidoni
#'
#' Ha lo stesso aspetto dei cluster di marker disegnati dal browser: colore
#' secondo la quota di censiti, numero dei bidoni e percentuale. Serve alla
#' vista aggregata, dove i gruppi sono calcolati dal server.
#'
#' @param n Numero di bidoni del gruppo.
#' @param censiti Numero di bidoni censiti.
#' @return Testo SVG, un elemento per gruppo.
#' @noRd
svg_bolla <- function(n, censiti) {
  quota <- ifelse(n > 0, censiti / n, 0)
  lato <- lato_bolla(n)
  sprintf(
    paste0(
      "<svg xmlns='http://www.w3.org/2000/svg' width='%1$d' height='%1$d' viewBox='0 0 %1$d %1$d'>",
      "<circle cx='%2$s' cy='%2$s' r='%3$s' fill='%4$s' stroke='#ffffff' stroke-opacity='0.92' stroke-width='3'/>",
      "<circle cx='%2$s' cy='%2$s' r='%5$s' fill='none' stroke='#000000' stroke-opacity='0.22' stroke-width='1'/>",
      "<text x='%2$s' y='%6$s' text-anchor='middle' font-family='Arial,sans-serif' font-size='13' font-weight='700' fill='%7$s'>%8$s</text>",
      "<text x='%2$s' y='%9$s' text-anchor='middle' font-family='Arial,sans-serif' font-size='9' font-weight='600' fill='%7$s'>%10$d%%</text>",
      "</svg>"
    ),
    as.integer(lato),
    lato / 2,
    lato / 2 - 2.5,
    genera_colore_cluster(quota),
    lato / 2 - 0.5,
    lato / 2 + 2,
    colori_stato()$bordo,
    formatta_numero(n),
    lato / 2 + 13,
    as.integer(round(100 * quota))
  )
}

#' Icone Leaflet delle bolle della vista aggregata
#'
#' @param n,censiti Bidoni e bidoni censiti di ogni gruppo.
#' @noRd
icone_bolle <- function(n, censiti) {
  lato <- lato_bolla(n)
  leaflet::icons(
    iconUrl = uri_svg(svg_bolla(n, censiti)),
    iconWidth = lato,
    iconHeight = lato,
    iconAnchorX = lato / 2,
    iconAnchorY = lato / 2
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
      voce(tags$span(class = "legenda-riquadro"), "Area di spostamento")
    ),
    NULL
  )

  as.character(tags$details(
    open = NA,
    tags$summary("Legenda"),
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
    extra
  ))
}
