# Confini dei comuni serviti dallo shapefile della Regione del Veneto
# (`data-raw/rvccomuni/`, non semplificati) a
# `inst/extdata/comuni_confini.geojson`. Fuori dall'app; serve `sf`.
# Con confini nuovi: sostituire la cartella e `Rscript data-raw/prepara_comuni.R`.

#' Fonte dei confini dei comuni
#' Contesto: i confini sono della Regione del Veneto (IODL 2.0): la fonte va
#' citata e annotata nel GeoJSON.
#' Risultato: `fonte`, `licenza`, `indirizzo` della richiesta WFS e `cartella`
#' con lo shapefile scaricato.
#' @noRd
origine_confini <- function() {
  list(
    fonte = "Regione del Veneto, Infrastruttura dati territoriali: strato c0104011_comuni",
    licenza = "IODL 2.0. Fonte dei dati: Regione del Veneto",
    indirizzo = "http://idt2-geoserver.regione.veneto.it/geoserver/wfs?service=WFS&request=GetFeature&version=1.0.0&typeNames=rv%3Ac0104011_comuni&outputFormat=SHAPE-ZIP",
    cartella = file.path("data-raw", "rvccomuni")
  )
}

#' Legge lo shapefile della Regione e lo restituisce in WGS84
#'
#' Contesto: la Regione pubblica i confini in Monte Mario; le mappe usano WGS84.
#' Funzionamento: legge l'unico shapefile di una cartella (codifica dal file `.cpg`
#' o `.cst`), lo trasforma (dal `.prj`) e passa da un GeoJSON temporaneo letto con
#' `jsonlite`: serve `sf` (facoltativo, solo qui).
#' Risultato: una `FeatureCollection` con tutti i comuni del Veneto.
#' @param cartella Cartella, o percorso del `.shp`.
#' @noRd
leggi_confini_regione <- function(cartella = origine_confini()$cartella) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Per leggere gli shapefile serve il pacchetto sf: install.packages(\"sf\")."
    )
  }
  # 1. Lo shapefile della cartella.
  shp <- if (grepl("\\.shp$", cartella, ignore.case = TRUE)) {
    cartella
  } else {
    list.files(
      cartella,
      pattern = "\\.shp$",
      full.names = TRUE,
      ignore.case = TRUE
    )
  }
  if (length(shp) != 1 || !file.exists(shp)) {
    stop(
      "Serve uno e un solo shapefile in ",
      cartella,
      ": trovati ",
      length(shp),
      "."
    )
  }

  # 2. Lettura, con la codifica dichiarata accanto al file.
  codifica <- NULL
  for (estensione in c("cpg", "cst")) {
    dichiarazione <- sub(
      "\\.shp$",
      paste0(".", estensione),
      shp,
      ignore.case = TRUE
    )
    if (file.exists(dichiarazione)) {
      codifica <- trimws(readLines(dichiarazione, n = 1, warn = FALSE))
      break
    }
  }
  comuni <- sf::st_read(
    shp,
    quiet = TRUE,
    options = if (!is.null(codifica)) {
      paste0("ENCODING=", codifica)
    } else {
      character(0)
    }
  )

  # 3. Da Monte Mario, o da qualunque sistema dichiarato nel .prj, a WGS84.
  if (is.na(sf::st_crs(comuni))) {
    stop(
      "Lo shapefile non dichiara il suo sistema di coordinate: manca il file .prj."
    )
  }
  comuni <- sf::st_transform(comuni, 4326)

  # 4. In GeoJSON, letto come lista.
  temporaneo <- tempfile(fileext = ".geojson")
  on.exit(unlink(temporaneo), add = TRUE)
  sf::st_write(comuni, temporaneo, driver = "GeoJSON", quiet = TRUE)
  jsonlite::fromJSON(temporaneo, simplifyVector = FALSE)
}

#' Associa ogni comune della tabella al suo confine
#' Contesto: la tabella dei comuni serviti (`comuni_cantieri.csv`) è l'unico
#' elenco; i confini devono corrispondere uno a uno.
#' Funzionamento: nomi confrontati con `chiave_comune()`; tra omonimi vale quello
#' che contiene il centro abitato. Si ferma elencando i comuni senza confine e
#' quelli il cui confine non contiene il centro.
#' Risultato: una `FeatureCollection` nell'ordine della tabella, con `comune`,
#' `cantiere`, `nome_istat`, `codice_istat` (sei cifre), `provincia`.
#' @noRd
seleziona_confini_comuni <- function(geojson, comuni = comuni_cantieri()) {
  elementi <- geojson$features
  nomi <- vapply(
    elementi,
    function(elemento) as.character(elemento$properties$nomcom %||% NA),
    character(1)
  )
  chiavi <- chiave_comune(nomi)
  cercate <- chiave_comune(comuni$comune)
  if (anyDuplicated(cercate) > 0) {
    stop(
      "La tabella dei comuni contiene pi\u00f9 volte: ",
      paste(unique(comuni$comune[duplicated(cercate)]), collapse = ", "),
      "."
    )
  }

  scelti <- rep(NA_integer_, nrow(comuni))
  assenti <- character(0)
  fuori <- character(0)
  omonimi <- character(0)
  for (i in seq_len(nrow(comuni))) {
    candidati <- which(chiavi == cercate[i])
    if (length(candidati) == 0) {
      assenti <- c(assenti, comuni$comune[i])
      next
    }
    con_centro <- !is.na(comuni$latitudine[i]) && !is.na(comuni$longitudine[i])
    if (!con_centro) {
      if (length(candidati) > 1) {
        omonimi <- c(omonimi, comuni$comune[i])
      } else {
        scelti[i] <- candidati
      }
      next
    }
    contiene <- vapply(
      candidati,
      function(k) {
        dentro_poligoni(
          comuni$longitudine[i],
          comuni$latitudine[i],
          anelli_geometria(elementi[[k]]$geometry)
        )
      },
      logical(1)
    )
    if (sum(contiene) == 1) {
      scelti[i] <- candidati[contiene]
    } else {
      fuori <- c(fuori, comuni$comune[i])
    }
  }

  problemi <- c(
    if (length(assenti) > 0) {
      paste0("non trovati: ", paste(assenti, collapse = ", "))
    },
    if (length(fuori) > 0) {
      paste0(
        "il centro abitato cade fuori dal confine con lo stesso nome: ",
        paste(fuori, collapse = ", ")
      )
    },
    if (length(omonimi) > 0) {
      paste0(
        "pi\u00f9 comuni con lo stesso nome, servono le coordinate del centro: ",
        paste(omonimi, collapse = ", ")
      )
    }
  )
  if (length(problemi) > 0) {
    stop(
      "Confini dei comuni non associati. ",
      paste(problemi, collapse = "; "),
      ".",
      call. = FALSE
    )
  }
  stopifnot(!anyNA(scelti), anyDuplicated(scelti) == 0)

  origine <- origine_confini()
  list(
    type = "FeatureCollection",
    origine = origine$fonte,
    licenza = origine$licenza,
    features = lapply(seq_along(scelti), function(i) {
      elemento <- elementi[[scelti[i]]]
      # Il codice ISTAT della Regione perde lo zero iniziale: 24009 per 024009.
      codice <- suppressWarnings(as.integer(elemento$properties$codistat))
      list(
        type = "Feature",
        properties = list(
          comune = comuni$comune[i],
          cantiere = comuni$cantiere[i],
          nome_istat = elemento$properties$nomcom,
          codice_istat = if (is.na(codice)) {
            NA_character_
          } else {
            sprintf("%06d", codice)
          },
          provincia = elemento$properties$provincia
        ),
        geometry = elemento$geometry[c("type", "coordinates")]
      )
    })
  )
}

#' Scrive il GeoJSON, un comune per riga, sei decimali
#' Contesto: un comune per riga rende leggibili le differenze in git; sei
#' decimali sono centimetri, abbastanza per le tolleranze.
#' @noRd
scrivi_confini_comuni <- function(confini, file) {
  in_json <- function(x) {
    as.character(jsonlite::toJSON(
      x,
      auto_unbox = TRUE,
      digits = 6,
      null = "null"
    ))
  }
  testata <- in_json(confini[setdiff(names(confini), "features")])
  elementi <- vapply(confini$features, in_json, character(1))
  n <- length(elementi)
  righe <- c(
    paste0(sub("\\}$", "", testata), ",\"features\":["),
    paste0(elementi, c(rep(",", max(n - 1, 0)), "")[seq_len(n)]),
    "]}"
  )
  connessione <- file(file, open = "wb")
  on.exit(close(connessione))
  writeLines(enc2utf8(righe), connessione, useBytes = TRUE)
  invisible(file)
}

#' Prepara `inst/extdata/comuni_confini.geojson`; chiamata da `data-raw/prepara_comuni.R`
#' Contesto: va rieseguita se cambiano i comuni serviti o lo shapefile; dopo va
#' rigenerato il dataset di esempio (cambiano le coerenze).
#' Funzionamento: legge lo shapefile, seleziona i comuni della tabella, annota
#' fonte, licenza e data e scrive il file.
#' Risultato: il percorso, invisibile.
#' @noRd
prepara_confini_comuni <- function(
  cartella = origine_confini()$cartella,
  file = file.path("inst", "extdata", "comuni_confini.geojson"),
  comuni = comuni_cantieri()
) {
  regione <- leggi_confini_regione(cartella)
  confini <- seleziona_confini_comuni(regione, comuni)
  shp <- list.files(
    cartella,
    pattern = "\\.shp$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  versione <- if (length(shp) == 1) {
    format(file.info(shp)$mtime, "%Y-%m-%d")
  } else {
    format(Sys.Date())
  }
  confini <- c(
    confini[c("type", "origine", "licenza")],
    list(versione = versione),
    confini["features"]
  )
  scrivi_confini_comuni(confini, file)
  message(sprintf(
    "Scritti i confini di %d comuni in %s, dallo shapefile del %s.",
    length(confini$features),
    file,
    versione
  ))
  invisible(file)
}
