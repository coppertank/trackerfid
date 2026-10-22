# Mappe WebGL (mapgl, MapLibre GL JS): base disegnata una volta, dati con i
# messaggi a `inst/app/www/mappa.js`. Sorgenti, livelli, icone e messaggi si
# costruiscono qui, senza reattività.

# Dati delle mappe preparati una volta per sessione di R.
memoria_mappa <- new.env(parent = emptyenv())

#' Stile dello sfondo: `bright` di OpenFreeMap, o l'opzione `trackerfid.stile_mappa`
#'
#' Deve avere il carattere di `carattere_mappa()`.
#' @noRd
stile_mappa <- function() {
  getOption(
    "trackerfid.stile_mappa",
    "https://tiles.openfreemap.org/styles/bright"
  )
}

#' Sfondo chiaro senza tessere, per quando lo stile non si scarica
#'
#' I numeri di cluster e bolle, senza la rete, mancano.
#' @noRd
stile_ripiego <- function() {
  list(
    version = 8,
    glyphs = "https://tiles.openfreemap.org/fonts/{fontstack}/{range}.pbf",
    sources = oggetto_vuoto(),
    layers = list(list(
      id = "sfondo",
      type = "background",
      paint = list("background-color" = "#f2f2ef")
    ))
  )
}

#' Carattere dei testi sulla mappa: deve essere nello stile dello sfondo
#' @noRd
carattere_mappa <- function() list("Noto Sans Bold")

#' Indirizzo da cui MapLibre scarica i confini dei comuni
#'
#' Risorsa statica, non nella configurazione: 1,25 MB per cinque mappe. `v`
#' cambia con il file.
#' @noRd
indirizzo_confini <- function() {
  file <- percorso_confini_comuni()
  sprintf(
    "%s/%s?v=%s",
    risorsa_confini(),
    basename(file),
    format(file.info(file)$mtime, "%Y%m%d%H%M%S")
  )
}

#' Nome della risorsa statica con il file dei confini
#' @noRd
risorsa_confini <- function() "trackerfid-confini"

#' Espressione MapLibre del colore del cantiere di ogni comune; grigio se manca
#' @noRd
espressione_colore_cantiere <- function() {
  colori <- colori_cantieri()
  c(
    list("match", list("get", "cantiere")),
    as.list(as.vector(rbind(names(colori), unname(colori)))),
    list(colore_cantiere(NA_character_))
  )
}

# --- Icone ----------------------------------------------------------------------

#' Icone dei marker: ogni combinazione di stato, glifo, stile e segnali
#'
#' Il browser le registra all'apertura; i punti portano solo il numero.
#' @noRd
icone_mappa <- function() {
  if (is.null(memoria_mappa$icone)) {
    config <- servizi_config()
    glifi <- unique(rbind(
      config[, c("icona", "colore")],
      data.frame(icona = "question", colore = info_servizio(NA)$colore)
    ))
    # Un servizio di ogni glifo, per disegnarlo con `svg_marker()`.
    glifi$servizio <- config$servizio[match(
      paste(glifi$icona, glifi$colore),
      paste(config$icona, config$colore)
    )]
    combinazioni <- expand.grid(
      glifo = seq_len(nrow(glifi)),
      presente = stati_database(),
      stile = c("normale", "ultimo", "precedente"),
      segnali = c("00", "10", "01"),
      stringsAsFactors = FALSE
    )
    combinazioni$fuori_comune <- substr(combinazioni$segnali, 1, 1) == "1"
    combinazioni$fuori_giro <- substr(combinazioni$segnali, 2, 2) == "1"
    memoria_mappa$icone <- dplyr::tibble(
      id = sprintf(
        "m-%s-%d-%s-%s",
        ifelse(combinazioni$presente == "Presente", "p", "n"),
        combinazioni$glifo,
        substr(combinazioni$stile, 1, 1),
        combinazioni$segnali
      ),
      svg = purrr::pmap_chr(
        list(
          glifi$servizio[combinazioni$glifo],
          combinazioni$presente,
          combinazioni$stile,
          combinazioni$fuori_comune,
          combinazioni$fuori_giro
        ),
        function(servizio, presente, stile, fc, fg) {
          svg_marker(
            servizio,
            presente,
            stile,
            fuori_comune = fc,
            fuori_giro = fg
          )
        }
      ),
      presente = combinazioni$presente,
      icona = glifi$icona[combinazioni$glifo],
      colore = glifi$colore[combinazioni$glifo],
      stile = combinazioni$stile,
      fuori_comune = combinazioni$fuori_comune,
      fuori_giro = combinazioni$fuori_giro
    )
  }
  memoria_mappa$icone
}

#' Numero dell'icona di ogni marker in `icone_mappa()`, da 0
#' @param servizio Da `servizio_mostrato()`; mancante: punto di domanda.
#' @noRd
indice_icona <- function(
  servizio,
  presente,
  stile = "normale",
  fuori_comune = FALSE,
  fuori_giro = FALSE
) {
  n <- length(servizio)
  icone <- icone_mappa()
  info <- per_valori_distinti(as.character(servizio), function(valori) {
    dati <- info_servizio(valori)
    paste(dati$icona, dati$colore)
  })
  chiave <- paste(
    info,
    rep_len(presente, n) %in% "Presente",
    rep_len(stile, n),
    rep_len(fuori_comune %in% TRUE, n),
    rep_len(fuori_giro %in% TRUE, n)
  )
  catalogo <- paste(
    paste(icone$icona, icone$colore),
    icone$presente == "Presente",
    icone$stile,
    icone$fuori_comune,
    icone$fuori_giro
  )
  match(chiave, catalogo) - 1L
}

# --- Sorgenti e livelli -------------------------------------------------------------

#' Oggetto JSON vuoto, `{}`
#' @noRd
oggetto_vuoto <- function() stats::setNames(list(), character(0))

#' Espressioni MapLibre di colore, raggio e testo di un cluster o di una bolla
#'
#' Colore dalla quota di censiti, lato da `lato_bolla()`.
#' @noRd
espressioni_gruppo <- function(n, censiti) {
  colori <- colori_stato()
  quota <- list("/", censiti, list("max", n, 1))
  soglie <- c(10, 100, 1000, 10000)
  raggi <- lato_bolla(c(1, soglie)) / 2 - 2.5
  list(
    colore = list(
      "interpolate",
      list("linear"),
      quota,
      0,
      colori$non_presente,
      0.5,
      colori$misto,
      1,
      colori$presente
    ),
    raggio = c(
      list("step", n, raggi[1]),
      as.list(as.vector(rbind(soglie, raggi[-1])))
    ),
    testo = list(
      "format",
      list("number-format", n, list(locale = "it-IT")),
      oggetto_vuoto(),
      "\n",
      oggetto_vuoto(),
      list(
        "concat",
        list("to-string", list("round", list("*", 100, quota))),
        "%"
      ),
      list("font-scale" = 0.7)
    )
  )
}

#' Livelli di un gruppo di bidoni: cerchio e numeri, in una fascia di zoom
#' @noRd
livelli_gruppo <- function(
  id,
  sorgente,
  n,
  censiti,
  filtro = NULL,
  minzoom = 0,
  maxzoom = 24
) {
  espressioni <- espressioni_gruppo(n, censiti)
  comuni <- list(source = sorgente, minzoom = minzoom, maxzoom = maxzoom)
  if (!is.null(filtro)) {
    comuni$filter <- filtro
  }
  list(
    c(
      list(
        id = id,
        type = "circle",
        paint = list(
          "circle-color" = espressioni$colore,
          "circle-radius" = espressioni$raggio,
          "circle-stroke-color" = "#ffffff",
          "circle-stroke-width" = 3,
          "circle-stroke-opacity" = 0.92
        )
      ),
      comuni
    ),
    c(
      list(
        id = paste0(id, "-testo"),
        type = "symbol",
        layout = list(
          "text-field" = espressioni$testo,
          "text-font" = carattere_mappa(),
          "text-size" = 13,
          "text-line-height" = 1.05,
          "text-allow-overlap" = TRUE,
          "text-ignore-placement" = TRUE
        ),
        paint = list(
          "text-color" = colori_stato()$bordo,
          "text-halo-color" = "rgba(255, 255, 255, 0.8)",
          "text-halo-width" = 1
        )
      ),
      comuni
    )
  )
}

#' Livello dei marker; la punta cade sul punto, nessuno nasconde gli altri
#' @param ordinati `TRUE` se la proprietà `u` mette sopra l'ultima lettura.
#' @noRd
livello_marker <- function(
  id,
  sorgente,
  filtro = NULL,
  ordinati = FALSE,
  minzoom = 0
) {
  layout <- list(
    "icon-image" = list("get", "k"),
    "icon-anchor" = "bottom",
    "icon-offset" = c(0, 2.5),
    "icon-allow-overlap" = TRUE,
    "icon-ignore-placement" = TRUE
  )
  paint <- list()
  if (ordinati) {
    layout[["symbol-sort-key"]] <- list("get", "u")
    paint[["icon-opacity"]] <- list(
      "case",
      list("==", list("get", "u"), 1),
      1,
      0.6
    )
  }
  livello <- list(
    id = id,
    type = "symbol",
    source = sorgente,
    minzoom = minzoom,
    layout = layout
  )
  if (length(paint) > 0) {
    livello$paint <- paint
  }
  if (!is.null(filtro)) {
    livello$filter <- filtro
  }
  livello
}

#' Sorgenti e livelli di una mappa
#'
#' Sempre i confini dei comuni, sotto le etichette dello sfondo. Poi, per
#' modalità: `"cluster"` bolle per cantiere e per comune e bidoni, nelle
#' fasce di `soglie_zoom()`; `"rfid"` le letture cercate, l'ultima in
#' evidenza, con il riquadro di ogni RFID; `"utenza"` l'ultima lettura;
#' `"giro"` le letture con il contorno dei comuni dei giri; `"analisi"`
#' baricentri e cerchi di dispersione.
#' @return Lista con `sorgenti`, `livelli` e `interattivi` (click e testo al
#'   passaggio).
#' @noRd
livelli_mappa <- function(
  modalita = c("cluster", "rfid", "utenza", "giro", "analisi")
) {
  modalita <- match.arg(modalita)
  stile <- stile_confini()
  vuota <- list(type = "FeatureCollection", features = list())
  geojson <- function(...) c(list(type = "geojson", data = vuota), list(...))
  trattino <- as.numeric(strsplit(stile$dashArray, " ")[[1]]) / stile$weight
  confini <- list(
    list(
      id = "confini-riempimento",
      type = "fill",
      source = "confini",
      sotto_etichette = TRUE,
      paint = list(
        "fill-color" = espressione_colore_cantiere(),
        "fill-opacity" = stile$fillOpacity
      )
    ),
    list(
      id = "confini-bordo",
      type = "line",
      source = "confini",
      sotto_etichette = TRUE,
      paint = list(
        "line-color" = espressione_colore_cantiere(),
        "line-width" = stile$weight,
        "line-opacity" = stile$opacity,
        "line-dasharray" = as.list(trattino)
      )
    )
  )
  soglie <- soglie_zoom()
  # Attribuzione chiesta dalla licenza dei confini, IODL 2.0.
  sorgenti <- list(
    confini = list(
      type = "geojson",
      data = "confini",
      attribution = "Confini dei comuni: Regione del Veneto, IODL 2.0"
    )
  )
  interattivi <- list()
  livelli <- switch(
    modalita,
    cluster = {
      sorgenti$bidoni <- geojson(
        cluster = TRUE,
        clusterRadius = 60,
        clusterMaxZoom = 18,
        clusterProperties = list(censiti = list("+", list("get", "c")))
      )
      sorgenti$bolle_cantiere <- geojson()
      sorgenti$bolle_comune <- geojson()
      interattivi <- list(
        "bolle-cantiere" = list(click = "bolla", testo = "d"),
        "bolle-comune" = list(click = "bolla", testo = "d"),
        "cluster" = list(click = "cluster", testo = "d"),
        "bidoni" = list(click = "punto", testo = "r")
      )
      c(
        livelli_gruppo(
          "bolle-cantiere",
          "bolle_cantiere",
          list("get", "n"),
          list("get", "censiti"),
          maxzoom = soglie$cantiere + 1
        ),
        list(list(
          id = "bolle-cantiere-nome",
          type = "symbol",
          source = "bolle_cantiere",
          maxzoom = soglie$cantiere + 1,
          layout = list(
            "text-field" = list("get", "nome"),
            "text-font" = carattere_mappa(),
            "text-size" = 11,
            "text-anchor" = "top",
            "text-offset" = c(0, 1.9),
            "text-allow-overlap" = TRUE
          ),
          paint = list(
            "text-color" = colori_stato()$bordo,
            "text-halo-color" = "#ffffff",
            "text-halo-width" = 2
          )
        )),
        livelli_gruppo(
          "bolle-comune",
          "bolle_comune",
          list("get", "n"),
          list("get", "censiti"),
          minzoom = soglie$cantiere + 1,
          maxzoom = soglie$comune + 1
        ),
        livelli_gruppo(
          "cluster",
          "bidoni",
          list("get", "point_count"),
          list("get", "censiti"),
          filtro = list("has", "point_count"),
          minzoom = soglie$comune + 1
        ),
        list(livello_marker(
          "bidoni",
          "bidoni",
          filtro = list("!", list("has", "point_count")),
          minzoom = soglie$comune + 1
        ))
      )
    },
    rfid = {
      sorgenti$riquadri <- geojson()
      sorgenti$letture <- geojson()
      interattivi <- list(letture = list(click = "punto", testo = "r"))
      list(
        list(
          id = "riquadri",
          type = "line",
          source = "riquadri",
          paint = list("line-color" = "#808080", "line-width" = 2)
        ),
        livello_marker("letture", "letture", ordinati = TRUE)
      )
    },
    utenza = {
      sorgenti$letture <- geojson()
      interattivi <- list(letture = list(click = "punto", testo = "r"))
      list(livello_marker("letture", "letture"))
    },
    giro = {
      sorgenti$comuni_giro <- geojson()
      sorgenti$letture <- geojson()
      interattivi <- list(letture = list(click = "punto", testo = "r"))
      list(
        list(
          id = "comuni-giro",
          type = "line",
          source = "comuni_giro",
          paint = list(
            "line-color" = colori_stato()$bordo,
            "line-width" = 3,
            "line-opacity" = 0.9
          )
        ),
        livello_marker("letture", "letture")
      )
    },
    analisi = {
      sorgenti$cluster <- geojson()
      interattivi <- list(baricentri = list(click = "punto", testo = "r"))
      # Raggio in metri: `r22` è il raggio in pixel a zoom 22.
      list(
        list(
          id = "dispersione",
          type = "circle",
          source = "cluster",
          paint = list(
            "circle-color" = list("get", "colore"),
            "circle-opacity" = 0.12,
            "circle-stroke-color" = list("get", "colore"),
            "circle-stroke-width" = 1,
            "circle-stroke-opacity" = 0.9,
            "circle-radius" = list(
              "interpolate",
              list("exponential", 2),
              list("zoom"),
              0,
              list("/", list("get", "r22"), 2^22),
              22,
              list("get", "r22")
            )
          )
        ),
        list(
          id = "baricentri",
          type = "circle",
          source = "cluster",
          paint = list(
            "circle-color" = list("get", "colore"),
            "circle-radius" = 6,
            "circle-stroke-color" = "#ffffff",
            "circle-stroke-width" = 1.5
          )
        )
      )
    }
  )
  list(
    sorgenti = sorgenti,
    livelli = c(confini, livelli),
    interattivi = interattivi
  )
}

#' Configurazione che `mappa.js` riceve all'apertura di una mappa
#' @noRd
configurazione_mappa <- function(modalita) {
  livelli <- livelli_mappa(modalita)
  icone <- icone_mappa()
  stile <- stile_mappa()
  list(
    modalita = modalita,
    # Se lo stile a questo indirizzo non si scarica, la mappa passa allo
    # sfondo di ripiego.
    stile_url = if (is.character(stile)) stile,
    stile_ripiego = stile_ripiego(),
    icone = list(id = icone$id, svg = icone$svg),
    sorgenti = livelli$sorgenti,
    livelli = livelli$livelli,
    interattivi = livelli$interattivi,
    confini = indirizzo_confini(),
    zoom_bolle = list(
      cantiere = zoom_minimo_bolla("cantiere"),
      comune = zoom_minimo_bolla("comune")
    ),
    # Lo zoom 17 di Leaflet: un inquadramento non va oltre la scala di una via.
    zoom_massimo = 16
  )
}

#' Mappa di base (widget mapgl): sfondo, confini, livelli vuoti, vista sulla zona servita
#' @noRd
mappa_base <- function(modalita = "cluster") {
  zona <- riquadro_zona()
  mapgl::maplibre(
    style = stile_mappa(),
    bounds = c(zona$lng_min, zona$lat_min, zona$lng_max, zona$lat_max),
    projection = "mercator",
    maxZoom = 19
  ) |>
    htmlwidgets::onRender(
      "function(el, x, config) { TrackerfidMappa.inizia(this, el, config); }",
      data = configurazione_mappa(modalita)
    )
}

# --- Dati inviati alla mappa -----------------------------------------------------------

#' Coordinate intere in centomillesimi di grado: messaggi più leggeri
#' @noRd
coordinate_intere <- function(x) as.integer(round(x * 1e5))

#' Punti da disegnare, in colonne
#'
#' `x`, `y`, icona `k`, censito `c`, RFID `r`, ultima lettura `u` (ricerca
#' RFID). La posizione nell'elenco, da 0, torna al server con il click.
#' @noRd
punti_mappa <- function(df, modalita) {
  incongruenze <- incongruenze_comune(df)
  stile <- if (modalita == "rfid") {
    ifelse(df$is_ultimo, "ultimo", "precedente")
  } else {
    "normale"
  }
  punti <- list(
    x = coordinate_intere(df$longitudine),
    y = coordinate_intere(df$latitudine),
    k = indice_icona(
      servizio_mostrato(df),
      df$presente_a_database,
      stile,
      incongruenze$fuori_comune,
      incongruenze$fuori_giro
    ),
    c = as.integer(df$presente_a_database %in% "Presente"),
    r = as.character(df$RFID)
  )
  if (modalita == "rfid") {
    punti$u <- as.integer(df$is_ultimo)
  }
  punti
}

#' Bolle di `aggrega_marker()`, in colonne per il browser
#' @noRd
bolle_mappa <- function(bolle) {
  nome <- dplyr::coalesce(bolle$nome, senza_cantiere())
  list(
    x = coordinate_intere(bolle$longitudine),
    y = coordinate_intere(bolle$latitudine),
    n = as.integer(bolle$n),
    censiti = as.integer(bolle$censiti),
    nome = nome,
    d = sprintf(
      "%s: %s, %s censiti",
      nome,
      purrr::map_chr(bolle$n, conta, "bidone", "bidoni"),
      formatta_numero(bolle$censiti)
    ),
    ovest = bolle$lng_min,
    sud = bolle$lat_min,
    est = bolle$lng_max,
    nord = bolle$lat_max,
    livello = bolle$livello
  )
}

#' Riquadri degli spostamenti degli RFID cercati; nessuno per un RFID fermo
#' @noRd
riquadri_rfid <- function(df) {
  if (nrow(df) == 0) {
    return(list(
      r = character(0),
      ovest = numeric(0),
      sud = numeric(0),
      est = numeric(0),
      nord = numeric(0)
    ))
  }
  gruppo <- indice_gruppi(df$RFID)
  n <- max(gruppo)
  lat <- estremi_per_gruppo(df$latitudine, gruppo, n)
  lng <- estremi_per_gruppo(df$longitudine, gruppo, n)
  tieni <- lat$minimo < lat$massimo | lng$minimo < lng$massimo
  list(
    r = primo_per_gruppo(as.character(df$RFID), gruppo, n)[tieni],
    ovest = lng$minimo[tieni],
    sud = lat$minimo[tieni],
    est = lng$massimo[tieni],
    nord = lat$massimo[tieni]
  )
}

#' Contorni dei comuni dei giri, se non sono più di `massimo`
#' @return Lista di anelli con `x` e `y`, uno per parte di comune.
#' @noRd
contorni_comuni_giro <- function(comuni, massimo = 6) {
  indici <- unique(stats::na.omit(indice_comune(unique(comuni))))
  if (length(indici) == 0 || length(indici) > massimo) {
    return(list())
  }
  anelli <- lapply(lati_comuni()[indici], function(confine) {
    if (is.null(confine)) {
      return(NULL)
    }
    lapply(split(seq_along(confine$x), confine$anello), function(i) {
      list(x = round(confine$x[i], 5), y = round(confine$y[i], 5))
    })
  })
  unname(do.call(c, Filter(Negate(is.null), anelli)))
}

#' Dati di una mappa, pronti per il browser
#'
#' @param riquadri Letture su cui calcolare riquadri RFID e comuni dei
#'   giri: tutte quelle trovate, anche se `df` ne ha solo una parte.
#' @return Lista con `punti` e i dati della modalità: bolle e `territorio`,
#'   riquadri, contorni.
#' @noRd
dati_mappa <- function(
  df,
  modalita = c("cluster", "rfid", "utenza", "giro"),
  riquadri = df
) {
  modalita <- match.arg(modalita)
  if (is.null(df) || nrow(df) == 0) {
    df <- NULL
  }
  dati <- list(
    punti = if (is.null(df)) {
      list(
        x = integer(0),
        y = integer(0),
        k = integer(0),
        c = integer(0),
        r = character(0)
      )
    } else {
      punti_mappa(df, modalita)
    }
  )
  switch(
    modalita,
    cluster = {
      dati$bolle_cantiere <- bolle_mappa(aggrega_marker(df, "cantiere"))
      dati$bolle_comune <- bolle_mappa(aggrega_marker(df, "comune"))
      dati$territorio <- !is.null(df) &&
        (any(!is.na(df[["cantiere"]])) || any(!is.na(df[["comune_assegnato"]])))
    },
    rfid = {
      dati$riquadri <- riquadri_rfid(
        if (is.null(riquadri)) dplyr::tibble(RFID = character(0)) else riquadri
      )
    },
    giro = {
      dati$comuni_giro <- if (is.null(riquadri)) {
        list()
      } else {
        contorni_comuni_giro(riquadri[["comune_giro"]])
      }
    }
  )
  dati
}

#' Campi che partono nel messaggio `"bolle"`, prima dei bidoni
#' @noRd
campi_bolle <- function() c("bolle_cantiere", "bolle_comune", "territorio")

#' Riquadro da inquadrare: ovest, sud, est, nord; `NULL` senza punti
#' @noRd
riquadro_vista <- function(df, margine = 0.0015) {
  if (is.null(df) || nrow(df) == 0) {
    return(NULL)
  }
  b <- calcola_bbox(df)
  c(
    b$lng_min - margine,
    b$lat_min - margine,
    b$lng_max + margine,
    b$lat_max + margine
  )
}

#' Messaggio a una mappa nel browser; prima che sia pronta vale l'ultimo di ogni tipo
#' @param tipo `"bolle"`, `"dati"`, `"inquadra"` o `"popup"`.
#' @noRd
invia_mappa <- function(session, id, tipo, ...) {
  session$sendCustomMessage(
    "trackerfid-mappa",
    list(id = id, tipo = tipo, ...)
  )
}

#' Popup di una lettura disegnata, vedi `create_popup_html()`
#' @noRd
popup_lettura <- function(df, riga) {
  r <- df[riga, , drop = FALSE]
  create_popup_html(
    rfid = r$RFID,
    servizio_atteso = r$servizio_atteso,
    targa = r$targa_veicolo,
    matricola = r$matricola_veicolo,
    data_ora_lettura = r$data_ora_lettura,
    id_utenza = r$id_utenza,
    presente = r$presente_a_database,
    comune = r[["comune_fornitura"]],
    comune_lettura = r[["comune_lettura"]],
    comune_giro = r[["comune_giro"]],
    cantiere = r[["cantiere"]],
    coerenza_comune = r[["coerenza_comune"]],
    servizio_giro = r[["servizio_giro"]],
    coerenza_servizio = r[["coerenza_servizio"]],
    servizio_transponder = r[["servizio_transponder"]],
    distanza_comune_dichiarato = r[["distanza_comune_dichiarato"]]
  )
}
