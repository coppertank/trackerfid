#' Analisi dei cluster spaziali da un file di letture RFID
#'
#' Valida il file, raggruppa con DBSCAN le letture di ogni RFID e
#' restituisce una riga per cluster: tempi, letture, baricentro,
#' dispersione, servizio, raccolte annue, indice di fiducia e indicatore. I
#' sacchetti (codici `00BD...` di 24 caratteri) restano fuori. Da riga di
#' comando: `system.file("scripts", "generate_cluster_analysis.R", package =
#' "trackerfid")`.
#'
#' @param csv_path File di letture, in un formato accettato dall'app.
#' @param data_inizio,data_fine Periodo di analisi, inclusi. Senza tutti e
#'   due: l'anno più recente del dataset; con uno solo, completato con
#'   l'anno dell'altro.
#' @param eps_m Raggio di DBSCAN in metri.
#' @param min_pts Letture minime di un cluster; oltre 1 le isolate vanno nel
#'   cluster 0.
#' @return Tibble di 29 colonne, una riga per cluster.
#'
#' @examples
#' csv <- system.file("extdata", "sample_rfid_dataset.csv", package = "trackerfid")
#' risultato <- genera_analisi_cluster(
#'   csv,
#'   data_inizio = as.Date("2025-01-01"),
#'   data_fine = as.Date("2025-12-31")
#' )
#' table(risultato$indicatore_cluster)
#'
#' @seealso [scrivi_analisi_cluster()] per salvare il risultato in CSV.
#' @export
genera_analisi_cluster <- function(
  csv_path,
  data_inizio = NULL,
  data_fine = NULL,
  eps_m = 100,
  min_pts = 1
) {
  esito <- carica_dataset(csv_path)
  for (avviso in esito$avvisi) {
    message("Avviso: ", avviso)
  }
  letture <- esito$dati

  if (is.null(data_inizio) && is.null(data_fine)) {
    periodo <- periodo_anno(max(lubridate::year(letture$giorno_lettura)))
    data_inizio <- periodo[1]
    data_fine <- periodo[2]
  } else if (is.null(data_inizio)) {
    data_inizio <- periodo_anno(lubridate::year(lubridate::as_date(data_fine)))[
      1
    ]
  } else if (is.null(data_fine)) {
    data_fine <- periodo_anno(lubridate::year(lubridate::as_date(data_inizio)))[
      2
    ]
  }

  calcola_analisi_cluster(
    letture,
    data_inizio,
    data_fine,
    eps_m = eps_m,
    min_pts = min_pts
  )
}

#' Salva l'analisi dei cluster in `cluster_analysis_[dal]_[al].csv`
#'
#' @param risultato Tabella di [genera_analisi_cluster()].
#' @param cartella Cartella di destinazione, creata se manca.
#' @return Il percorso del file, invisibile.
#'
#' @examples
#' csv <- system.file("extdata", "sample_rfid_dataset.csv", package = "trackerfid")
#' risultato <- genera_analisi_cluster(csv)
#' percorso <- scrivi_analisi_cluster(risultato, tempdir())
#' basename(percorso)
#'
#' @export
scrivi_analisi_cluster <- function(risultato, cartella = ".") {
  if (nrow(risultato) == 0) {
    rlang::abort(
      "L'analisi non contiene righe: nessuna lettura nel periodo indicato."
    )
  }
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  file <- file.path(
    cartella,
    nome_file_analisi_cluster(risultato$analisi_dal[1], risultato$analisi_al[1])
  )
  esporta_analisi_cluster(risultato, file)
}
