# Letture storiche, cioè quelle del sistema precedente alle antenne: lettura
# del file e conteggi per anno, per l'istogramma del dettaglio di un bidone.
# Le letture storiche hanno solo RFID e istante.

# ---- Lettura e validazione ---------------------------------------------------

#' Colonne del CSV delle letture storiche
#' @noRd
colonne_storico <- function() c("RFID", "giorno_lettura")

#' Percorso delle letture storiche di esempio incluse nel pacchetto
#' @noRd
percorso_storico_esempio <- function() {
  app_sys("extdata", "sample_letture_storiche.csv")
}

#' Valida le letture storiche e converte la data
#'
#' Servono le colonne `RFID` e `giorno_lettura`: maiuscole e ordine non
#' contano, le altre colonne sono ignorate. Una data non interpretabile ferma
#' il caricamento. Le righe incomplete e quelle ripetute vengono scartate e
#' segnalate tra gli avvisi.
#'
#' @param data Data frame letto da `read_csv_auto()`.
#' @return Lista con `dati` (tibble con `RFID` e `giorno_lettura`) e `avvisi`.
#' @noRd
valida_storico <- function(data) {
  attese <- colonne_storico()
  posizione <- match(tolower(attese), tolower(stringr::str_trim(names(data))))
  mancanti <- attese[is.na(posizione)]
  if (length(mancanti) > 0) {
    errore_validazione(sprintf(
      "Colonne mancanti nel file delle letture storiche: %s. Il CSV deve contenere le colonne RFID e giorno_lettura.",
      paste(mancanti, collapse = ", ")
    ))
  }
  data <- dplyr::as_tibble(data[, posizione, drop = FALSE])
  names(data) <- attese
  if (nrow(data) == 0) {
    errore_validazione(
      "Il file delle letture storiche non contiene righe di dati."
    )
  }

  data$RFID <- as.character(pulisci_testo(data$RFID))
  data$giorno_lettura <- pulisci_testo(data$giorno_lettura)
  giorno <- converti_data_ora(data$giorno_lettura)
  non_validi <- which(!is.na(data$giorno_lettura) & is.na(giorno))
  if (length(non_validi) > 0) {
    errore_validazione(sprintf(
      "giorno_lettura: %d valori non riconosciuti come data e ora (formato atteso YYYY-MM-DD HH:MM:SS). Righe del file: %s.",
      length(non_validi),
      elenco_righe(non_validi)
    ))
  }
  data$giorno_lettura <- giorno

  avvisi <- character(0)
  completi <- tidyr::drop_na(data)
  if (nrow(completi) == 0) {
    errore_validazione(
      "Nessuna riga utilizzabile: in tutte manca l'RFID o la data."
    )
  }
  if (nrow(completi) < nrow(data)) {
    avvisi <- c(
      avvisi,
      sprintf(
        "%d righe scartate perch\u00e9 prive di RFID o di data.",
        nrow(data) - nrow(completi)
      )
    )
  }
  unici <- dplyr::distinct(completi)
  if (nrow(unici) < nrow(completi)) {
    avvisi <- c(
      avvisi,
      sprintf(
        "%d letture ripetute, con lo stesso RFID e lo stesso istante, contate una volta sola.",
        nrow(completi) - nrow(unici)
      )
    )
  }

  list(dati = unici, avvisi = avvisi)
}

#' Legge e valida un file di letture storiche
#'
#' @param path Percorso del file: CSV, CSV compresso con gzip o Parquet.
#' @param nome Nome originale del file, vedi `leggi_tabella()`.
#' @return Lista con `dati` e `avvisi`, vedi `valida_storico()`.
#' @noRd
carica_storico <- function(path, nome = basename(path)) {
  valida_storico(leggi_tabella(path, nome))
}

# ---- Letture per anno --------------------------------------------------------

#' Mette in un'unica tabella le letture dei due sistemi
#'
#' @param letture Letture con le antenne, validate.
#' @param storico Letture storiche validate, oppure `NULL`.
#' @return Tibble con `RFID`, `giorno_lettura` e `fonte`: `"storico"` o
#'   `"antenne"`.
#' @noRd
unisci_letture <- function(letture, storico = NULL) {
  antenne <- dplyr::tibble(
    RFID = letture$RFID,
    giorno_lettura = letture$giorno_lettura,
    fonte = "antenne"
  )
  if (is.null(storico) || nrow(storico) == 0) {
    return(antenne)
  }
  dplyr::bind_rows(
    dplyr::tibble(
      RFID = storico$RFID,
      giorno_lettura = storico$giorno_lettura,
      fonte = "storico"
    ),
    antenne
  )
}

#' Conta le letture per RFID, anno e sistema
#'
#' @param unite Risultato di `unisci_letture()`.
#' @return Tibble con `RFID`, `anno`, `fonte`, `letture`. Mancano le
#'   combinazioni senza letture.
#' @noRd
conta_letture_annuali <- function(unite) {
  unite |>
    dplyr::mutate(anno = as.integer(lubridate::year(.data$giorno_lettura))) |>
    dplyr::count(.data$RFID, .data$anno, .data$fonte, name = "letture")
}

#' Letture di ogni RFID in ogni anno, zeri compresi
#'
#' Per ogni RFID gli anni vanno da quello della sua prima lettura all'ultimo
#' anno dei dati. Gli anni precedenti mancano: il contenitore poteva non
#' essere ancora in servizio.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @param rfid RFID da tenere. Tutti, se manca.
#' @return Tibble con `RFID`, `anno`, `storico`, `antenne`, `letture` (la
#'   somma) e `primo_anno`.
#' @noRd
griglia_annuale <- function(unite, rfid = NULL) {
  if (!is.null(rfid)) {
    unite <- unite[unite$RFID %in% rfid, , drop = FALSE]
  }
  if (nrow(unite) == 0) {
    return(dplyr::tibble(
      RFID = character(0),
      anno = integer(0),
      storico = integer(0),
      antenne = integer(0),
      letture = integer(0),
      primo_anno = integer(0)
    ))
  }
  ultimo_anno <- as.integer(lubridate::year(max(unite$giorno_lettura)))
  conteggi <- conta_letture_annuali(unite) |>
    dplyr::mutate(
      fonte = factor(.data$fonte, levels = c("storico", "antenne"))
    ) |>
    tidyr::pivot_wider(
      names_from = "fonte",
      values_from = "letture",
      names_expand = TRUE,
      values_fill = 0L
    )
  # I conteggi sono in ordine di RFID e di anno: la prima riga di ogni RFID è
  # il suo primo anno.
  primi <- conteggi[!duplicated(conteggi$RFID), c("RFID", "anno")]
  n_anni <- ultimo_anno - primi$anno + 1L
  primo_anno <- rep(primi$anno, n_anni)

  dplyr::tibble(
    RFID = rep(primi$RFID, n_anni),
    primo_anno = primo_anno,
    anno = primo_anno + sequence(n_anni) - 1L
  ) |>
    dplyr::left_join(conteggi, by = c("RFID", "anno")) |>
    dplyr::mutate(
      storico = dplyr::coalesce(.data$storico, 0L),
      antenne = dplyr::coalesce(.data$antenne, 0L),
      letture = .data$storico + .data$antenne
    ) |>
    dplyr::select(dplyr::all_of(c(
      "RFID",
      "anno",
      "storico",
      "antenne",
      "letture",
      "primo_anno"
    )))
}

#' Letture per anno di un solo RFID
#'
#' Serve all'istogramma del dettaglio del bidone. Gli anni arrivano fino
#' all'ultimo anno delle letture con le antenne, anche se l'RFID non è più
#' stato letto.
#'
#' @param rfid Codice RFID.
#' @param letture Letture con le antenne, di tutti gli RFID.
#' @param storico Letture storiche, oppure `NULL`.
#' @return Tibble con `anno`, `storico`, `antenne`.
#' @noRd
andamento_rfid <- function(rfid, letture, storico = NULL) {
  vuoto <- dplyr::tibble(
    anno = integer(0),
    storico = integer(0),
    antenne = integer(0)
  )
  if (is.null(rfid) || is.null(letture) || nrow(letture) == 0) {
    return(vuoto)
  }
  proprie <- letture[letture$RFID == rfid, , drop = FALSE]
  if (!is.null(storico)) {
    storico <- storico[storico$RFID == rfid, , drop = FALSE]
  }
  griglia <- griglia_annuale(unisci_letture(proprie, storico))
  if (nrow(griglia) == 0) {
    return(vuoto)
  }
  ultimo_anno <- as.integer(lubridate::year(max(letture$giorno_lettura)))
  dplyr::tibble(
    anno = seq(min(griglia$anno), max(ultimo_anno, griglia$anno))
  ) |>
    dplyr::left_join(griglia[, c("anno", "storico", "antenne")], by = "anno") |>
    dplyr::mutate(
      storico = dplyr::coalesce(.data$storico, 0L),
      antenne = dplyr::coalesce(.data$antenne, 0L)
    )
}
