# Confini dei comuni serviti, presi dalla raccolta geojson-italy.
#
# Queste funzioni preparano un file del pacchetto e non fanno parte dell'app:
# servono quando cambia l'elenco dei comuni serviti o quando la raccolta
# pubblica confini più recenti. Richiedono la connessione a internet.
#
# Dalla radice del progetto:
#   devtools::load_all()
#   scarica_confini_comuni()

#' Indirizzi della raccolta dei confini amministrativi italiani
#'
#' La raccolta `guglielmo/geojson-italy` ripubblica i confini dell'ISTAT, con
#' licenza CC-BY 4.0. Ogni data in cui i comuni italiani sono cambiati ha un
#' suo rilascio, e il file `temporal/INDEX.csv` dice quale rilascio vale per
#' ogni data: il più aggiornato è quello in vigore oggi.
#'
#' @return Lista con `raccolta`, `licenza`, `indice` e `comuni`. L'ultimo è
#'   l'indirizzo del file nazionale dei comuni, con `%s` al posto del rilascio.
#' @noRd
origine_confini <- function() {
  list(
    raccolta = "https://github.com/guglielmo/geojson-italy",
    licenza = "CC-BY 4.0. Fonte dei dati: ISTAT",
    indice = "https://raw.githubusercontent.com/guglielmo/geojson-italy/main/temporal/INDEX.csv",
    comuni = "https://github.com/guglielmo/geojson-italy/releases/download/%s/limits_IT_municipalities.geojson.gz"
  )
}

#' Rilascio dei confini in vigore in una data
#'
#' @param indice Tabella di `temporal/INDEX.csv`: una riga per rilascio, con
#'   `valid_from`, `release_tag` e `municipalities`.
#' @param giorno Data di riferimento.
#' @return La riga dell'ultimo rilascio entrato in vigore entro quella data.
#' @noRd
rilascio_confini <- function(indice, giorno = Sys.Date()) {
  dal <- as.Date(indice$valid_from)
  in_vigore <- which(!is.na(dal) & dal <= as.Date(giorno))
  if (length(in_vigore) == 0) {
    stop("Nessun rilascio dei confini in vigore il ", format(giorno), ".")
  }
  indice[in_vigore[which.max(dal[in_vigore])], , drop = FALSE]
}

#' Sceglie da un file nazionale i confini dei comuni serviti
#'
#' I nomi si confrontano con `chiave_comune()`, quindi senza badare a
#' maiuscole, accenti e apostrofi: `ROSA'` trova `Rosà`. Tra due comuni
#' italiani con lo stesso nome vale quello che contiene il centro abitato
#' indicato nella tabella dei comuni. Per lo stesso motivo un confine che non
#' contiene il centro del suo comune è un errore: vuol dire che il nome ha
#' trovato un altro comune, oppure che le coordinate sono sbagliate.
#'
#' Il risultato ha un confine per ogni riga della tabella dei comuni, nello
#' stesso ordine. Se anche un solo comune non si trova la funzione si ferma e
#' li elenca.
#'
#' @param geojson File nazionale dei comuni, letto con
#'   `jsonlite::fromJSON(simplifyVector = FALSE)`: una `FeatureCollection` in
#'   cui ogni comune ha la proprietà `name`.
#' @param comuni Tabella dei comuni serviti: `comune`, `cantiere`,
#'   `latitudine`, `longitudine`.
#' @return `FeatureCollection` con i soli comuni serviti. Ogni comune ha le
#'   proprietà `comune` e `cantiere`, come nella tabella, e `nome_istat`,
#'   `codice_istat`, `codice_catastale` e `provincia`, come nella fonte.
#' @noRd
seleziona_confini_comuni <- function(geojson, comuni = comuni_cantieri()) {
  elementi <- geojson$features
  nomi <- vapply(
    elementi,
    function(elemento) as.character(elemento$properties$name %||% NA),
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
    origine = origine$raccolta,
    licenza = origine$licenza,
    features = lapply(seq_along(scelti), function(i) {
      elemento <- elementi[[scelti[i]]]
      list(
        type = "Feature",
        properties = list(
          comune = comuni$comune[i],
          cantiere = comuni$cantiere[i],
          nome_istat = elemento$properties$name,
          codice_istat = elemento$properties$com_istat_code,
          codice_catastale = elemento$properties$com_catasto_code,
          provincia = elemento$properties$prov_acr
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
#' centimetri.
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

#' Scarica i confini più aggiornati dei comuni serviti
#'
#' Legge dall'indice della raccolta qual è il rilascio in vigore, scarica il
#' file nazionale dei comuni di quel rilascio, ne tiene i soli comuni della
#' tabella dei comuni e li scrive nel file dei confini del pacchetto.
#'
#' @param file File da scrivere. Il valore predefinito è quello del pacchetto,
#'   a partire dalla radice del progetto.
#' @param comuni Tabella dei comuni serviti.
#' @param giorno Data per cui si vogliono i confini. Oggi, se manca.
#' @param sorgente File nazionale già scaricato, anche compresso con gzip. Se
#'   manca viene scaricato.
#' @param versione Rilascio da cui viene `sorgente`, da annotare nel file.
#' @return Il percorso del file scritto, in modo invisibile.
#' @noRd
scarica_confini_comuni <- function(
  file = file.path("inst", "extdata", "comuni_confini.geojson"),
  comuni = comuni_cantieri(),
  giorno = Sys.Date(),
  sorgente = NULL,
  versione = NULL
) {
  attesi <- NA_integer_
  if (is.null(sorgente)) {
    origine <- origine_confini()
    opzioni <- options(timeout = max(600, getOption("timeout")))
    on.exit(options(opzioni), add = TRUE)
    rilascio <- rilascio_confini(
      utils::read.csv(origine$indice, stringsAsFactors = FALSE),
      giorno
    )
    versione <- rilascio$release_tag
    attesi <- rilascio$municipalities
    sorgente <- tempfile(fileext = ".geojson.gz")
    on.exit(unlink(sorgente), add = TRUE)
    utils::download.file(
      sprintf(origine$comuni, versione),
      sorgente,
      mode = "wb",
      quiet = TRUE
    )
  }

  da_leggere <- sorgente
  if (tolower(tools::file_ext(sorgente)) == "gz") {
    da_leggere <- decomprimi_gz(sorgente)
    on.exit(unlink(da_leggere), add = TRUE)
  }
  nazionale <- jsonlite::fromJSON(da_leggere, simplifyVector = FALSE)
  # Un file scaricato a metà non ha tutti i comuni dichiarati dall'indice.
  if (!is.na(attesi) && length(nazionale$features) != attesi) {
    stop(sprintf(
      "Il file scaricato contiene %d comuni, l'indice ne dichiara %d.",
      length(nazionale$features),
      attesi
    ))
  }

  confini <- seleziona_confini_comuni(nazionale, comuni)
  confini <- c(
    confini[c("type", "origine", "licenza")],
    list(versione = versione),
    confini["features"]
  )
  scrivi_confini_comuni(confini, file)
  message(sprintf(
    "Scritti i confini di %d comuni in %s%s.",
    length(confini$features),
    file,
    if (is.null(versione)) "" else paste0(", dal rilascio ", versione)
  ))
  invisible(file)
}
