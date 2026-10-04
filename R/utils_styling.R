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

#' Configurazione dei servizi: icona Font Awesome, colore del glifo, emoji
#' @noRd
servizi_config <- function() {
  data.frame(
    servizio = c("SECCO", "CARTA", "VETRO", "UMIDO", "PLASTICA"),
    icona = c("trash-can", "file-lines", "wine-bottle", "leaf", "recycle"),
    colore = c("#7f8c8d", "#8e5b2e", "#2471a3", "#1e8449", "#0e9aa7"),
    emoji = c("\U0001F5D1\ufe0f", "\U0001F4C4", "\U0001F37E", "\U0001F342", "\u267b\ufe0f"),
    stringsAsFactors = FALSE
  )
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

#' Colore del marker in base allo stato a database
#'
#' @param presente Vettore con i valori di `presente_a_database`.
#' @return Colori HEX: verde per "Presente", rosso negli altri casi.
#' @noRd
get_color <- function(presente) {
  colori <- colori_stato()
  ifelse(!is.na(presente) & presente == "Presente", colori$presente, colori$non_presente)
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
    tolower(grDevices::rgb(round(canali[1]), round(canali[2]), round(canali[3]), maxColorValue = 255))
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
    "<svg x=\"", x, "\" y=\"", y, "\" width=\"", lato, "\" height=\"", lato,
    "\" viewBox=\"", glifo$viewBox, "\" preserveAspectRatio=\"xMidYMid meet\">",
    "<path d=\"", glifo$path, "\" fill=\"", colore, "\"/></svg>"
  )
}

#' Icona SVG autonoma con il solo glifo di un servizio (per la legenda)
#' @noRd
svg_icona_servizio <- function(servizio, lato = 16) {
  info <- info_servizio(servizio)
  paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"", lato, "\" height=\"", lato,
    "\" viewBox=\"0 0 ", lato, " ", lato, "\">",
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
  tratto <- switch(stile,
    ultimo = "stroke-width=\"3\"",
    precedente = "stroke-width=\"1\" stroke-dasharray=\"5,5\"",
    "stroke-width=\"1.5\""
  )
  info <- info_servizio(servizio)
  paste0(
    "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"36\" height=\"48\" viewBox=\"0 0 36 48\">",
    "<path d=\"", sagoma, "\" fill=\"", get_color(presente), "\" stroke=\"", colori$bordo,
    "\" stroke-linejoin=\"round\" ", tratto, "/>",
    "<circle cx=\"18\" cy=\"17\" r=\"10.5\" fill=\"#ffffff\"/>",
    if (glifo) svg_glifo(info$icona, info$colore, 11, 10, 14),
    "</svg>"
  )
}

#' Converte un SVG in data URI utilizzabile come immagine
#' @noRd
uri_svg <- function(svg) {
  paste0("data:image/svg+xml;charset=utf-8,", utils::URLencode(svg, reserved = TRUE))
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
    iconWidth = 36, iconHeight = 48,
    iconAnchorX = 18, iconAnchorY = 46,
    popupAnchorX = 1, popupAnchorY = -40
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
  canali <- function(hex) paste0("[", paste(grDevices::col2rgb(hex)[, 1], collapse = ","), "]")
  paste0(
    "function(cluster) {
      var figli = cluster.getAllChildMarkers();
      var n = figli.length, censiti = 0;
      for (var i = 0; i < n; i++) { if (figli[i].options.censito) censiti++; }
      var pct = n > 0 ? censiti / n : 0;
      var rosso = ", canali(colori$non_presente), ", giallo = ", canali(colori$misto),
    ", verde = ", canali(colori$presente), ";
      var da = pct < 0.5 ? rosso : giallo, a = pct < 0.5 ? giallo : verde;
      var ratio = pct < 0.5 ? pct * 2 : (pct - 0.5) * 2;
      var c = [0, 1, 2].map(function(k) { return Math.round(da[k] + (a[k] - da[k]) * ratio); });
      var lato = n < 10 ? 40 : (n < 100 ? 46 : 54);
      var pctTesto = Math.round(pct * 100) + '%';
      var stile = 'width:' + lato + 'px;height:' + lato + 'px;border-radius:50%;' +
        'background-color:rgb(' + c.join(',') + ');border:3px solid rgba(255,255,255,0.9);' +
        'box-shadow:0 1px 5px rgba(0,0,0,0.45);box-sizing:border-box;display:flex;' +
        'flex-direction:column;align-items:center;justify-content:center;' +
        'color:", colori$bordo, ";font-family:Arial,sans-serif;line-height:1.05;' +
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
    tags$div(class = "legenda-voce", tags$span(class = "legenda-icona", icona), tags$span(testo))
  }

  config <- servizi_config()
  voci_servizio <- purrr::map(
    c(config$servizio, NA),
    function(s) {
      voce(
        immagine(svg_icona_servizio(s), 14, 14),
        if (is.na(s)) "Non censito" else s
      )
    }
  )

  extra <- switch(modalita,
    cluster = list(
      tags$div(class = "legenda-titolo", "Cluster: % censiti"),
      tags$div(
        class = "legenda-gradiente",
        style = sprintf(
          "background: linear-gradient(to right, %s);",
          paste(genera_colore_cluster(c(0, 0.5, 1)), collapse = ", ")
        )
      ),
      tags$div(class = "legenda-scala", tags$span("0%"), tags$span("50%"), tags$span("100%"))
    ),
    rfid = list(
      tags$div(class = "legenda-titolo", "Letture"),
      voce(immagine(svg_marker(NA, "Presente", "ultimo", glifo = FALSE), 15, 20), "Ultima lettura"),
      voce(immagine(svg_marker(NA, "Presente", "precedente", glifo = FALSE), 15, 20), "Letture precedenti"),
      voce(tags$span(class = "legenda-riquadro"), "Area di spostamento")
    ),
    NULL
  )

  as.character(tags$details(
    open = NA,
    tags$summary("Legenda"),
    tags$div(class = "legenda-titolo", "Stato a database"),
    voce(immagine(svg_marker(NA, "Presente", glifo = FALSE), 15, 20), "Presente (censito)"),
    voce(immagine(svg_marker(NA, "Non Presente", glifo = FALSE), 15, 20), "Non Presente"),
    tags$div(class = "legenda-titolo", "Servizio"),
    voci_servizio,
    extra
  ))
}
