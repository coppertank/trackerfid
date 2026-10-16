# Mappe WebGL con mapgl (MapLibre GL JS).
#
# La mappa di base si disegna una volta: sfondo vettoriale, confini dei
# comuni e i livelli vuoti della modalità. I dati arrivano dopo, con un
# messaggio al browser, e il codice di `inst/app/www/mappa.js` li mette nelle
# sorgenti della mappa. Così filtri e ricerche non ricaricano lo sfondo, e
# zoom e spostamenti non chiedono niente al server.
#
# Sorgenti, livelli, icone e messaggi si costruiscono qui, in R, con funzioni
# senza reattività; `mappa.js` li applica così come arrivano.

# Dati delle mappe preparati una volta per sessione di R.
memoria_mappa <- new.env(parent = emptyenv())

#' Stile dello sfondo delle mappe
#'
#' Lo stile «bright» di OpenFreeMap, vettoriale e senza chiave di accesso,
#' con i colori della mappa di OpenStreetMap: l'ha scelto il proprietario del
#' progetto. Disegnato in WebGL, è nitido a ogni zoom. Ha anche i caratteri
#' usati dai numeri di cluster e bolle, vedi `carattere_mappa()`. Si può cambiare con
#' l'opzione `trackerfid.stile_mappa`, ad esempio per uno sfondo servito
#' dalla rete aziendale.
#'
#' @return Indirizzo dello stile, oppure lo stile come lista.
#' @noRd
stile_mappa <- function() {
  getOption(
    "trackerfid.stile_mappa",
    "https://tiles.openfreemap.org/styles/bright"
  )
}

#' Sfondo di ripiego, quando lo stile dello sfondo non si scarica
#'
#' Senza la rete, o con il servizio dello sfondo fuori uso, lo stile di
#' `stile_mappa()` non arriva e MapLibre non disegnerebbe niente: `mappa.js`
#' passa allora a questo sfondo, un fondo chiaro senza tessere, e confini,
#' bolle, cluster e marker si disegnano lo stesso. I numeri di cluster e
#' bolle usano i caratteri del servizio dello sfondo: senza la rete mancano.
#'
#' @return Stile MapLibre, come lista.
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

#' Carattere dei testi disegnati sulla mappa
#'
#' Deve essere tra quelli dello stile dello sfondo, vedi `stile_mappa()`.
#' @noRd
carattere_mappa <- function() list("Noto Sans Bold")

#' Confini dei comuni serviti, pronti per la mappa
#'
#' Ogni comune porta il colore del suo cantiere, vedi `colori_cantieri()`. Le
#' coordinate hanno cinque decimali, circa un metro: il testo pesa poco più
#' di 100 kB e viene preparato una volta per sessione di R.
#'
#' @return Testo GeoJSON: una `FeatureCollection` con un comune per elemento
#'   e, tra le proprietà, `comune`, `cantiere` e `colore`.
#' @noRd
geojson_confini <- function() {
  if (is.null(memoria_mappa$confini)) {
    confini <- jsonlite::fromJSON(
      percorso_confini_comuni(),
      simplifyVector = FALSE
    )
    elementi <- lapply(confini$features, function(elemento) {
      list(
        type = "Feature",
        properties = list(
          comune = elemento$properties$comune,
          cantiere = elemento$properties$cantiere,
          colore = colore_cantiere(elemento$properties$cantiere)
        ),
        geometry = elemento$geometry[c("type", "coordinates")]
      )
    })
    memoria_mappa$confini <- as.character(jsonlite::toJSON(
      list(type = "FeatureCollection", features = elementi),
      auto_unbox = TRUE,
      digits = 5
    ))
  }
  memoria_mappa$confini
}

# --- Icone ----------------------------------------------------------------------

#' Icone dei marker: tutte le combinazioni disegnabili
#'
#' Una per ogni combinazione di stato a database, glifo del servizio (icona e
#' colore, vedi `info_servizio()`), stile del bordo e segnali di coerenza del
#' comune. Il browser le disegna una volta, all'apertura della mappa: i dati
#' portano solo il numero dell'icona.
#'
#' @return Tibble con `id`, `svg`, `presente`, `icona`, `colore`, `stile`,
#'   `fuori_comune` e `fuori_giro`.
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
          svg_marker(servizio, presente, stile, fuori_comune = fc, fuori_giro = fg)
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

#' Numero dell'icona di ogni marker, in `icone_mappa()`
#'
#' @param servizio Servizio con cui il marker compare, vedi
#'   `servizio_mostrato()`. Un servizio mancante o sconosciuto ha il punto di
#'   domanda.
#' @param presente Stato a database.
#' @param stile Stile del bordo, vedi `svg_marker()`.
#' @param fuori_comune,fuori_giro Segnali di coerenza del comune.
#' @return Vettore intero, da 0: è la posizione dell'icona nell'elenco che
#'   riceve il browser.
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

#' Espressioni MapLibre di colore, raggio e testo di un gruppo di bidoni
#'
#' Cluster e bolle hanno l'aspetto dei cluster di marker di prima: colore
#' secondo la quota di censiti, da rosso a verde passando per l'arancio, vedi
#' `genera_colore_cluster()`; lato che cresce con i bidoni, vedi
#' `lato_bolla()`; il numero e, sotto, la percentuale di censiti.
#'
#' @param n Espressione con il numero dei bidoni del gruppo.
#' @param censiti Espressione con il numero dei censiti.
#' @return Lista con `colore`, `raggio` e `testo`.
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
      list("concat", list("to-string", list("round", list("*", 100, quota))), "%"),
      list("font-scale" = 0.7)
    )
  )
}

#' Livelli di un gruppo di bidoni: cerchio e numeri
#'
#' @param id Prefisso degli identificativi dei livelli.
#' @param sorgente Sorgente dei dati.
#' @param n,censiti Espressioni dei conteggi, vedi `espressioni_gruppo()`.
#' @param filtro Filtro MapLibre, oppure `NULL`.
#' @param minzoom,maxzoom Fascia di zoom in cui il livello si vede.
#' @noRd
livelli_gruppo <- function(id, sorgente, n, censiti, filtro = NULL, minzoom = 0, maxzoom = 24) {
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

#' Livello di marker con le icone di `icone_mappa()`
#'
#' La punta del marker cade sul punto della lettura. I marker non si
#' nascondono a vicenda: due bidoni vicini restano visibili tutti e due.
#'
#' @param id Identificativo del livello.
#' @param sorgente Sorgente dei dati.
#' @param filtro Filtro MapLibre, oppure `NULL`.
#' @param ordinati `TRUE` se la proprietà `u` decide quali marker stanno
#'   sopra e quali sono attenuati, come nella ricerca RFID.
#' @param minzoom Zoom da cui il livello si vede.
#' @noRd
livello_marker <- function(id, sorgente, filtro = NULL, ordinati = FALSE, minzoom = 0) {
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
    paint[["icon-opacity"]] <- list("case", list("==", list("get", "u"), 1), 1, 0.6)
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
#' Tutte le mappe hanno i confini dei comuni, sotto le etichette dello
#' sfondo, colorati per cantiere: riempimento appena accennato e bordo
#' tratteggiato, vedi `stile_confini()`. I confini non rispondono al mouse.
#' Sopra stanno i livelli della modalità:
#'
#' - `"cluster"`: una bolla per cantiere sotto lo zoom 11, una per comune
#'   sotto lo zoom 14, i singoli bidoni da zoom 14, con quelli vicini
#'   raggruppati in cluster colorati secondo la quota di censiti. Le fasce
#'   vengono da `soglie_zoom()`; senza cantieri né comuni i bidoni si vedono
#'   a ogni zoom, e lo decide il browser all'arrivo dei dati.
#' - `"rfid"`: tutte le letture degli RFID cercati, l'ultima in evidenza e
#'   sopra le altre, con il riquadro degli spostamenti di ogni RFID.
#' - `"utenza"`: l'ultima lettura di ogni bidone.
#' - `"giro"`: tutte le letture dei giri, con il contorno dei comuni dei giri.
#' - `"analisi"`: il baricentro di ogni cluster spaziale e il cerchio della
#'   sua dispersione, in metri, nel colore dell'indicatore.
#'
#' @param modalita Modalità della mappa.
#' @return Lista con `sorgenti`, `livelli` e `interattivi`: per ogni livello
#'   che risponde al mouse, che cosa fa il click (`punto`, `cluster`,
#'   `bolla`) e quale proprietà dà il testo al passaggio.
#' @noRd
livelli_mappa <- function(modalita = c("cluster", "rfid", "utenza", "giro", "analisi")) {
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
        "fill-color" = list("get", "colore"),
        "fill-opacity" = stile$fillOpacity
      )
    ),
    list(
      id = "confini-bordo",
      type = "line",
      source = "confini",
      sotto_etichette = TRUE,
      paint = list(
        "line-color" = list("get", "colore"),
        "line-width" = stile$weight,
        "line-opacity" = stile$opacity,
        "line-dasharray" = as.list(trattino)
      )
    )
  )
  soglie <- soglie_zoom()
  sorgenti <- list(confini = list(type = "geojson", data = "confini"))
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
      # Il raggio della dispersione è in metri: a ogni livello di zoom un
      # metro occupa il doppio dei pixel. `r22` è il raggio in pixel a zoom 22.
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
#'
#' @param modalita Modalità della mappa, vedi `livelli_mappa()`.
#' @return Lista con `modalita`, l'indirizzo dello stile dello sfondo e lo
#'   sfondo di ripiego, `icone` (identificativi e SVG), sorgenti, livelli e
#'   livelli interattivi, i confini dei comuni, lo zoom a cui portano i click
#'   sulle bolle e lo zoom massimo di un inquadramento.
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
    confini = structure(geojson_confini(), class = "json"),
    zoom_bolle = list(
      cantiere = zoom_minimo_bolla("cantiere"),
      comune = zoom_minimo_bolla("comune")
    ),
    # Lo zoom 17 di Leaflet: un inquadramento non va oltre la scala di una via.
    zoom_massimo = 16
  )
}

#' Mappa di base: sfondo, confini dei comuni, livelli vuoti della modalità
#'
#' I dati arrivano dopo, con `invia_mappa()`. La legenda sta nell'interfaccia
#' del modulo, sopra la mappa, vedi `mod_mappa_ui()`.
#'
#' @param modalita Modalità della mappa, vedi `livelli_mappa()`.
#' @return Widget mapgl che inquadra tutta la zona servita.
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

#' Coordinate come interi, in centomillesimi di grado
#'
#' Cinque decimali sono circa un metro. Gli interi pesano meno dei decimali
#' nel messaggio al browser, che li divide per centomila.
#' @noRd
coordinate_intere <- function(x) as.integer(round(x * 1e5))

#' Punti da disegnare, in colonne
#'
#' Ogni punto ha le coordinate (`x`, `y`), il numero dell'icona (`k`, vedi
#' `indice_icona()`), lo stato a database (`c`, 1 per un censito), l'RFID
#' (`r`, per il testo al passaggio del mouse) e, nella ricerca RFID,
#' l'ultima lettura (`u`, 1 per l'ultima). La posizione di un punto
#' nell'elenco è il numero di riga delle letture disegnate, da 0: il click
#' torna al server con quella.
#'
#' @param df Letture da disegnare.
#' @param modalita Modalità della mappa.
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

#' Bolle di un livello del territorio, in colonne
#'
#' @param bolle Risultato di `aggrega_marker()`.
#' @return Lista con le coordinate (`x`, `y`, vedi `coordinate_intere()`),
#'   `n`, `censiti`, il nome (`nome`), il testo al passaggio del mouse (`d`),
#'   il riquadro dei bidoni (`ovest`, `sud`, `est`, `nord`) e il `livello`,
#'   una voce per bolla.
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

#' Riquadri degli spostamenti degli RFID cercati
#'
#' Gli RFID letti sempre nello stesso punto non hanno riquadro.
#'
#' @param df Letture degli RFID cercati.
#' @return Lista con `r` e gli estremi `ovest`, `sud`, `est`, `nord`, una
#'   voce per riquadro.
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

#' Contorni dei comuni dei giri
#'
#' Un contorno scuro e continuo attorno ai comuni in cui i giri dovevano
#' lavorare: le letture che stanno fuori si vedono a colpo d'occhio. Con molti
#' comuni, come in una ricerca su tutti i mezzi, il contorno non aiuta e non
#' viene disegnato.
#'
#' @param comuni Comuni dei giri, anche ripetuti.
#' @param massimo Numero massimo di comuni messi in evidenza.
#' @return Lista di anelli, ognuno con `x` e `y` in gradi; vuota oltre il
#'   massimo. Gli anelli di un comune con più parti, come `VALBRENTA`, sono
#'   separati.
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
#' @param df Letture da disegnare, oppure `NULL`.
#' @param modalita Modalità della mappa, vedi `livelli_mappa()`.
#' @param riquadri Letture su cui calcolare i riquadri della ricerca RFID e i
#'   comuni dei giri: tutte quelle trovate, anche quando `df` ne contiene solo
#'   una parte.
#' @return Lista con `punti`, vedi `punti_mappa()`, e i dati propri della
#'   modalità: per la mappa principale le bolle di cantieri e comuni e
#'   `territorio` (`TRUE` se i bidoni hanno cantiere o comune); per la
#'   ricerca RFID i riquadri; per i giri i contorni dei comuni.
#' @noRd
dati_mappa <- function(df, modalita = c("cluster", "rfid", "utenza", "giro"), riquadri = df) {
  modalita <- match.arg(modalita)
  if (is.null(df) || nrow(df) == 0) {
    df <- NULL
  }
  dati <- list(
    punti = if (is.null(df)) {
      list(x = integer(0), y = integer(0), k = integer(0), c = integer(0), r = character(0))
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

#' Campi dei dati della mappa principale che partono con le bolle
#'
#' Il modulo della mappa li manda in un messaggio a sé, `"bolle"`, prima di
#' quello con i bidoni: vedi `mod_mappa_server()`.
#' @noRd
campi_bolle <- function() c("bolle_cantiere", "bolle_comune", "territorio")

#' Riquadro su cui inquadrare la mappa
#'
#' @param df Punti da inquadrare, con `latitudine` e `longitudine`.
#' @param margine Margine attorno ai punti, in gradi.
#' @return Vettore con ovest, sud, est e nord; `NULL` senza punti.
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

#' Invia un messaggio a una mappa nel browser
#'
#' `mappa.js` lo applica quando la mappa è pronta: un messaggio che arriva
#' prima resta in attesa, e di ogni tipo vale l'ultimo.
#'
#' @param session Sessione Shiny.
#' @param id Identificativo della mappa nella pagina, con lo spazio dei nomi.
#' @param tipo `"dati"`, `"inquadra"` o `"popup"`.
#' @param ... Contenuto del messaggio.
#' @noRd
invia_mappa <- function(session, id, tipo, ...) {
  session$sendCustomMessage(
    "trackerfid-mappa",
    list(id = id, tipo = tipo, ...)
  )
}

#' Testo del popup di una lettura
#'
#' @param df Letture disegnate.
#' @param riga Numero di riga della lettura.
#' @return HTML del popup, vedi `create_popup_html()`.
#' @noRd
popup_lettura <- function(df, riga) {
  r <- df[riga, , drop = FALSE]
  create_popup_html(
    rfid = r$RFID,
    servizio_atteso = r$servizio_atteso,
    targa = r$targa_veicolo,
    matricola = r$matricola_veicolo,
    giorno_lettura = r$giorno_lettura,
    id_utenza = r$id_utenza,
    presente = r$presente_a_database,
    comune = r[["comune_fornitura"]],
    comune_lettura = r[["comune_lettura"]],
    comune_giro = r[["comune_giro"]],
    cantiere = r[["cantiere"]],
    coerenza_comune = r[["coerenza_comune"]],
    servizio_giro = r[["servizio_giro"]],
    coerenza_servizio = r[["coerenza_servizio"]],
    servizio_transponder = r[["servizio_transponder"]]
  )
}
