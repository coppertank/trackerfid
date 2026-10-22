# Confronto tra letture storiche (saponette) e letture con le antenne, per il
# report sul beneficio delle antenne, che scrive il proprietario. L'app non
# usa queste funzioni.

#' Letture da un data frame o da un CSV della pipeline, letto tutto come testo
#' @noRd
leggi_letture_confronto <- function(letture) {
  if (is.character(letture)) leggi_tabella_origine(letture) else letture
}

#' RFID letti ogni giorno da saponette, antenne o entrambe
#'
#' 1. tiene le letture storiche degli RFID letti anche dalle antenne;
#' 2. ne prende le coppie distinte di giorno e `id_trasmissione` (una
#'    trasmissione in un giorno è un giro);
#' 3. tiene tutte le letture storiche di quei giri;
#' 4. unisce letture storiche e letture con le antenne, un RFID al più una
#'    volta al giorno.
#' @param letture_storiche,letture_app Data frame (anche da `targets::tar_load()`)
#'   o percorsi dei CSV della pipeline, con `RFID` e
#'   `data_ora_lettura`; le storiche anche con `id_trasmissione`.
#' @return Tibble con `data_lettura` (data), `RFID` e `origine`:
#'   `"Saponette"`, `"Antenne"` o `"Entrambe"`, per giorno e RFID.
#' @noRd
confronta_letture <- function(
  letture_storiche = "data-raw/output/letture_storiche.csv",
  letture_app = "data-raw/output/letture_app.csv"
) {
  storiche <- leggi_letture_confronto(letture_storiche) |>
    dplyr::mutate(data_lettura = lubridate::as_date(.data$data_ora_lettura))
  antenne <- leggi_letture_confronto(letture_app)

  # I giri, trasmissione e giorno, in cui le saponette hanno letto almeno un
  # RFID visto anche dalle antenne.
  giri <- storiche |>
    dplyr::filter(.data$RFID %in% antenne$RFID) |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione)

  # Un RFID al più una volta al giorno, per ognuno dei due metodi.
  da_saponette <- storiche |>
    dplyr::semi_join(giri, by = c("data_lettura", "id_trasmissione")) |>
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
