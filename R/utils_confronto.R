# Confronto tra letture storiche (saponette) e letture con le antenne, per il
# report `docs/confronto_antenne.qmd`. L'app non usa queste funzioni.

#' Letture da un data frame o da un CSV della pipeline, letto tutto come testo
#' @noRd
leggi_letture_confronto <- function(letture) {
  if (is.character(letture)) leggi_tabella_origine(letture) else letture
}

#' Valori di un filtro: vettore di testi senza vuoti, o `NULL` se il filtro non c'è
#' @noRd
valori_filtro <- function(x) {
  x <- trimws(as.character(unlist(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) NULL else x
}

#' Colonne che un filtro richiede, con un errore se mancano
#' @noRd
richiedi_colonne <- function(letture, colonne, filtro) {
  mancanti <- setdiff(colonne, names(letture))
  if (length(mancanti) > 0) {
    stop(
      "Il filtro `", filtro, "` richiede le colonne: ",
      paste(colonne, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

#' Letture filtrate per servizio, categoria e sottocategoria
#'
#' Vale per le letture con le antenne e per le storiche. Un filtro vuoto
#' (`NULL`, `NA` o `""`) non filtra; più valori in un filtro valgono "uno
#' qualsiasi di".
#' @param letture Data frame con le colonne dei filtri usati.
#' @param servizio Si tengono le letture con `servizio_atteso` oppure
#'   `servizio_transponder` uguale a uno dei valori (maiuscole ininfluenti);
#'   le storiche hanno solo il secondo.
#' @param categorie_escluse Si tolgono le letture di queste `categoria`; i non
#'   censiti, senza categoria, restano.
#' @param sottocategoria Si tengono solo le letture con questa
#'   `sottocategoria`; i non censiti, senza sottocategoria, escono.
#' @noRd
filtra_letture <- function(
  letture,
  servizio = NULL,
  categorie_escluse = NULL,
  sottocategoria = NULL
) {
  # Nomi diversi dalle colonne, che dentro `filter()` hanno la precedenza.
  voci_servizio <- valori_filtro(servizio)
  voci_categorie <- valori_filtro(categorie_escluse)
  voci_sottocategoria <- valori_filtro(sottocategoria)

  if (!is.null(voci_servizio)) {
    voci_servizio <- toupper(voci_servizio)
    colonne <- intersect(
      c("servizio_atteso", "servizio_transponder"),
      names(letture)
    )
    if (length(colonne) == 0) {
      richiedi_colonne(
        letture,
        c("servizio_atteso", "servizio_transponder"),
        "servizio"
      )
    }
    letture <- dplyr::filter(
      letture,
      dplyr::if_any(
        dplyr::all_of(colonne),
        \(x) toupper(x) %in% voci_servizio
      )
    )
  }
  if (!is.null(voci_categorie)) {
    richiedi_colonne(letture, "categoria", "categorie_escluse")
    letture <- dplyr::filter(
      letture,
      !as.character(.data$categoria) %in% voci_categorie
    )
  }
  if (!is.null(voci_sottocategoria)) {
    richiedi_colonne(letture, "sottocategoria", "sottocategoria")
    letture <- dplyr::filter(
      letture,
      as.character(.data$sottocategoria) %in% voci_sottocategoria
    )
  }
  letture
}

#' Letture storiche dei giri in cui le antenne hanno letto almeno un RFID
#'
#' 1. tiene le letture storiche degli RFID letti dalle antenne;
#' 2. ne prende le coppie distinte di `data_lettura` e `id_trasmissione` (una
#'    trasmissione in un giorno è un giro);
#' 3. tiene tutte le letture storiche di quei giri.
#' @param letture_storiche Data frame con `RFID`, `data_ora_lettura` e
#'   `id_trasmissione`.
#' @param letture_antenne Data frame con `RFID`: le letture già filtrate.
#' @return Le letture storiche di quei giri, con `data_lettura` (data) al
#'   posto di quella del file.
#' @noRd
storiche_di_confronto <- function(letture_storiche, letture_antenne) {
  storiche <- letture_storiche |>
    dplyr::mutate(data_lettura = lubridate::as_date(.data$data_ora_lettura))
  giri <- storiche |>
    dplyr::filter(.data$RFID %in% letture_antenne$RFID) |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione)
  dplyr::semi_join(storiche, giri, by = c("data_lettura", "id_trasmissione"))
}

#' RFID letti ogni giorno, con l'origine della lettura: unione di due serie
#'
#' Un RFID al più una volta al giorno per ognuno dei due metodi.
#' @param storiche Letture storiche già scelte, con `data_lettura` e `RFID`.
#' @param antenne Letture con le antenne già scelte, con `data_ora_lettura` e
#'   `RFID`.
#' @return Come `confronta_letture()`.
#' @noRd
unisci_origini <- function(storiche, antenne) {
  da_saponette <- storiche |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(saponette = TRUE)
  da_antenne <- antenne |>
    dplyr::distinct(
      data_lettura = lubridate::as_date(.data$data_ora_lettura),
      RFID = .data$RFID
    ) |>
    dplyr::mutate(antenne = TRUE)

  dplyr::full_join(
    da_saponette,
    da_antenne,
    by = c("data_lettura", "RFID")
  ) |>
    dplyr::mutate(
      origine = dplyr::case_when(
        .data$saponette %in% TRUE & .data$antenne %in% TRUE ~ "Entrambe",
        .data$saponette %in% TRUE ~ "Saponette",
        .default = "Antenne"
      )
    ) |>
    dplyr::select("data_lettura", "RFID", "origine") |>
    dplyr::arrange(.data$data_lettura, .data$RFID)
}

#' RFID letti ogni giorno da saponette, antenne o entrambe
#'
#' 1. filtra le letture con le antenne (`filtra_letture()`);
#' 2. tiene le letture storiche dei giri in cui le saponette hanno letto un
#'    RFID di quelle letture (`storiche_di_confronto()`);
#' 3. unisce letture storiche e letture con le antenne, un RFID al più una
#'    volta al giorno.
#' @param letture_storiche,letture_app Data frame (anche da `targets::tar_load()`)
#'   o percorsi dei CSV della pipeline, con `RFID` e
#'   `data_ora_lettura`; le storiche anche con `id_trasmissione`.
#' @inheritParams filtra_letture
#' @return Tibble con `data_lettura` (data), `RFID` e `origine`:
#'   `"Saponette"`, `"Antenne"` o `"Entrambe"`, per giorno e RFID.
#' @noRd
confronta_letture <- function(
  letture_storiche = "data-raw/output/letture_storiche.csv",
  letture_app = "data-raw/output/letture_app.csv",
  servizio = NULL,
  categorie_escluse = NULL,
  sottocategoria = NULL
) {
  antenne <- leggi_letture_confronto(letture_app) |>
    filtra_letture(servizio, categorie_escluse, sottocategoria)
  storiche <- leggi_letture_confronto(letture_storiche) |>
    storiche_di_confronto(antenne)

  unisci_origini(storiche, antenne)
}
