# Confini dei comuni serviti, dai file della Regione del Veneto.
#
# Queste funzioni preparano un file del pacchetto,
# `inst/extdata/comuni_confini.geojson`, e non fanno parte dell'app: l'app
# legge solo il file già scritto. I confini vengono dallo strato dei comuni
# del geoportale della Regione del Veneto, scaricato come shapefile in
# `data-raw/rvccomuni/`. Non sono semplificati: sono quelli ufficiali della
# Regione, vertice per vertice.
#
# Quando la Regione pubblica confini nuovi basta sostituire la cartella degli
# shapefile e rieseguire, dalla radice del progetto:
#   Rscript data-raw/prepara_comuni.R
#
# Servono il pacchetto `sf`, che arriva comunque con `mapgl`, e nient'altro:
# niente connessione a internet.

#' Fonte dei confini dei comuni
#'
#' @return Lista con `fonte`, `licenza`, `indirizzo` (la richiesta WFS da cui
#'   viene lo shapefile) e `cartella`, la cartella in cui lo shapefile sta nel
#'   progetto.
#' @noRd
origine_confini <- function() {
  list(
    fonte = "Regione del Veneto, Infrastruttura dati territoriali: strato c0104011_comuni",
    licenza = "IODL 2.0. Fonte dei dati: Regione del Veneto",
    indirizzo = "http://idt2-geoserver.regione.veneto.it/geoserver/wfs?service=WFS&request=GetFeature&version=1.0.0&typeNames=rv%3Ac0104011_comuni&outputFormat=SHAPE-ZIP",
    cartella = file.path("data-raw", "rvccomuni")
  )
}

#' Legge i confini dei comuni di una cartella di shapefile
#'
#' Passaggi:
#' 1. trova lo shapefile della cartella: deve essercene uno solo, così una
#'    cartella nuova della Regione si usa senza cambiare nomi di file;
#' 2. lo legge con `sf`, con la codifica dei caratteri scritta nel file
#'    `.cst` o `.cpg` accanto allo shapefile, se c'è: la Regione usa
#'    ISO-8859-1;
#' 3. riporta le coordinate dal sistema della Regione, Monte Mario fuso
#'    ovest, a longitudine e latitudine in WGS84, quelle della mappa;
#' 4. passa per un GeoJSON temporaneo e lo rilegge con `jsonlite`, nella
#'    stessa forma di lista che usano le altre funzioni sui confini.
#'
#' @param cartella Cartella con lo shapefile, oppure il percorso del file
#'   `.shp`.
#' @return `FeatureCollection` con un comune per elemento e le proprietà
#'   dello shapefile: tra queste `nomcom`, `codistat` e `provincia`.
#' @noRd
leggi_confini_regione <- function(cartella = origine_confini()$cartella) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Per leggere gli shapefile serve il pacchetto sf: install.packages(\"sf\").")
  }
  # 1. Lo shapefile della cartella.
  shp <- if (grepl("\\.shp$", cartella, ignore.case = TRUE)) {
    cartella
  } else {
    list.files(cartella, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
  }
  if (length(shp) != 1 || !file.exists(shp)) {
    stop(
      "Serve uno e un solo shapefile in ", cartella, ": trovati ",
      length(shp), "."
    )
  }

  # 2. Lettura, con la codifica dichiarata accanto al file.
  codifica <- NULL
  for (estensione in c("cpg", "cst")) {
    dichiarazione <- sub("\\.shp$", paste0(".", estensione), shp, ignore.case = TRUE)
    if (file.exists(dichiarazione)) {
      codifica <- trimws(readLines(dichiarazione, n = 1, warn = FALSE))
      break
    }
  }
  comuni <- sf::st_read(
    shp,
    quiet = TRUE,
    options = if (!is.null(codifica)) paste0("ENCODING=", codifica) else character(0)
  )

  # 3. Da Monte Mario, o da qualunque sistema dichiarato nel .prj, a WGS84.
  if (is.na(sf::st_crs(comuni))) {
    stop("Lo shapefile non dichiara il suo sistema di coordinate: manca il file .prj.")
  }
  comuni <- sf::st_transform(comuni, 4326)

  # 4. In GeoJSON, letto come lista.
  temporaneo <- tempfile(fileext = ".geojson")
  on.exit(unlink(temporaneo), add = TRUE)
  sf::st_write(comuni, temporaneo, driver = "GeoJSON", quiet = TRUE)
  jsonlite::fromJSON(temporaneo, simplifyVector = FALSE)
}

#' Sceglie i confini dei comuni serviti
#'
#' I nomi si confrontano con `chiave_comune()`, quindi senza badare a
#' maiuscole, accenti e apostrofi: `ROSÀ` della tabella trova `Rosa'` della
#' Regione, `SAN PIETRO IN GU` trova `San Pietro in Gu'`. Tra due comuni con
#' lo stesso nome vale quello che contiene il centro abitato indicato nella
#' tabella dei comuni. Per lo stesso motivo un confine che non contiene il
#' centro del suo comune è un errore: vuol dire che il nome ha trovato un
#' altro comune, oppure che le coordinate sono sbagliate.
#'
#' Il risultato ha un confine per ogni riga della tabella dei comuni, nello
#' stesso ordine. Se anche un solo comune non si trova la funzione si ferma e
#' li elenca: un comune che manca non deve sparire in silenzio dalla mappa.
#'
#' @param geojson Confini della Regione, vedi `leggi_confini_regione()`: una
#'   `FeatureCollection` in cui ogni comune ha le proprietà `nomcom`,
#'   `codistat` e `provincia`.
#' @param comuni Tabella dei comuni serviti: `comune`, `cantiere`,
#'   `latitudine`, `longitudine`.
#' @return `FeatureCollection` con i soli comuni serviti. Ogni comune ha le
#'   proprietà `comune` e `cantiere`, come nella tabella, e `nome_istat`,
#'   `codice_istat`, di sei cifre, e `provincia`, come nella fonte.
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
      # La Regione scrive il codice ISTAT come numero, senza lo zero iniziale
      # della provincia: 24009 per 024009.
      codice <- suppressWarnings(as.integer(elemento$properties$codistat))
      list(
        type = "Feature",
        properties = list(
          comune = comuni$comune[i],
          cantiere = comuni$cantiere[i],
          nome_istat = elemento$properties$nomcom,
          codice_istat = if (is.na(codice)) NA_character_ else sprintf("%06d", codice),
          provincia = elemento$properties$provincia
        ),
        geometry = elemento$geometry[c("type", "coordinates")]
      )
    })
  )
}

#' Scrive i confini dei comuni in un file GeoJSON
#'
#' Un comune per riga, così una modifica al confine di un comune cambia una
#' riga sola del file. Le coordinate hanno sei decimali, circa dieci
#' centimetri: i vertici sono tutti quelli della fonte, senza
#' semplificazioni.
#'
#' @param confini Risultato di `seleziona_confini_comuni()`.
#' @param file Percorso del file da scrivere.
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

#' Prepara il file dei confini dei comuni serviti
#'
#' È il punto di ingresso, richiamato da `data-raw/prepara_comuni.R`:
#' 1. legge i confini della Regione, vedi `leggi_confini_regione()`;
#' 2. tiene i soli comuni della tabella dei comuni, con il loro cantiere,
#'    vedi `seleziona_confini_comuni()`: si ferma, senza scrivere niente, se
#'    anche un solo comune non si trova;
#' 3. annota nel file la fonte, la licenza e la data dello shapefile, cioè
#'    del giorno in cui è stato scaricato;
#' 4. scrive il file, un comune per riga, vedi `scrivi_confini_comuni()`.
#'
#' Dopo aver cambiato i confini va rigenerato il dataset di esempio, che
#' mette i contenitori dentro il confine del loro comune.
#'
#' @param cartella Cartella dello shapefile della Regione.
#' @param file File da scrivere. Il valore predefinito è quello del pacchetto,
#'   a partire dalla radice del progetto.
#' @param comuni Tabella dei comuni serviti.
#' @return Il percorso del file scritto, in modo invisibile.
#' @noRd
prepara_confini_comuni <- function(
  cartella = origine_confini()$cartella,
  file = file.path("inst", "extdata", "comuni_confini.geojson"),
  comuni = comuni_cantieri()
) {
  # 1. Confini della Regione.
  regione <- leggi_confini_regione(cartella)
  # 2. Solo i comuni serviti, nell'ordine della tabella.
  confini <- seleziona_confini_comuni(regione, comuni)
  # 3. Fonte, licenza e data dello shapefile.
  shp <- list.files(cartella, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
  versione <- if (length(shp) == 1) format(file.info(shp)$mtime, "%Y-%m-%d") else format(Sys.Date())
  confini <- c(
    confini[c("type", "origine", "licenza")],
    list(versione = versione),
    confini["features"]
  )
  # 4. Il file, un comune per riga.
  scrivi_confini_comuni(confini, file)
  message(sprintf(
    "Scritti i confini di %d comuni in %s, dallo shapefile del %s.",
    length(confini$features),
    file,
    versione
  ))
  invisible(file)
}
