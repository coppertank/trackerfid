#' Analisi dei cluster spaziali da un file CSV di letture RFID
#'
#' Legge e valida un file di letture, raggruppa le letture di ogni RFID in
#' cluster spaziali con DBSCAN e restituisce una riga per ogni cluster, con
#' tempi, numero di letture, baricentro, dispersione, servizio, raccolte
#' annue, indice di fiducia e indicatore.
#'
#' L'analisi riguarda i contenitori. I sacchetti, riconosciuti dal codice RFID
#' di 24 caratteri che inizia per `00BD`, sono monouso e restano fuori: il
#' risultato è quello di un file che non li contiene.
#'
#' La funzione non richiede l'app Shiny. Lo script
#' `system.file("scripts", "generate_cluster_analysis.R", package = "trackerfid")`
#' la rende disponibile anche da riga di comando.
#'
#' @param csv_path Percorso del file CSV, nello stesso formato accettato dall'app.
#' @param data_inizio,data_fine Estremi del periodo di analisi, inclusi. Se
#'   mancano entrambi si analizza l'anno solare più recente del dataset; se ne
#'   manca uno, si completa con l'inizio o la fine dell'anno dell'altro.
#' @param eps_m Raggio di ricerca di DBSCAN in metri: due letture entro questa
#'   distanza appartengono allo stesso cluster.
#' @param min_pts Numero minimo di letture per formare un cluster. Con 1 ogni
#'   lettura appartiene a un cluster; con valori maggiori le letture isolate
#'   finiscono nel cluster 0.
#'
#' @return Un tibble con 27 colonne, una riga per cluster di ogni RFID:
#'   identificazione (`RFID`, `cluster_id`, `globale_conteggio_cluster`),
#'   tempi globali e del cluster, `presente_a_database`, `cantiere` e
#'   `comune` dell'RFID, servizio transponder
#'   (attuale e cronologia dei cambi), servizio atteso (prevalente e
#'   composizione), volumi, periodo di analisi, raccolte annue previste e
#'   presunte, numero di letture, baricentro, `cluster_dispersione_90th_m`,
#'   `cluster_indice_fiducia` e `indicatore_cluster`.
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

#' Salva l'analisi dei cluster in un file CSV
#'
#' Il nome del file segue lo schema `cluster_analysis_[dal]_[al].csv`, con le
#' date del periodo di analisi.
#'
#' @param risultato Tabella restituita da [genera_analisi_cluster()].
#' @param cartella Cartella di destinazione, creata se non esiste.
#'
#' @return Il percorso del file scritto, in modo invisibile.
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
