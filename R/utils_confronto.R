# Confronto tra letture delle saponette e letture con le antenne, per il
# report `docs/confronto_antenne.qmd`. L'app non usa queste funzioni.

#' Letture da un data frame o da un CSV della pipeline, letto tutto come testo
#'
#' Contesto: le funzioni di confronto accettano sia i dataset già in memoria
#' (per esempio da `targets::tar_load()`) sia i CSV di `data-raw/output/`.
#' Risultato: un data frame; un CSV si legge con `leggi_tabella_origine()`, quindi
#' tutte le colonne sono testo (anche le date).
#' @noRd
leggi_letture_confronto <- function(letture) {
  if (is.character(letture)) leggi_tabella_origine(letture) else letture
}

#' Valori di un filtro: vettore di testi senza vuoti, o `NULL` se il filtro non c'è
#'
#' Contesto: i parametri dei report (`servizio`, `categorie_escluse`,
#' `sottocategoria`) possono essere vuoti, un valore o un elenco.
#' Risultato: i valori puliti da spazi, `NA` e testi vuoti; `NULL` se non ne
#' resta nessuno (il filtro non filtra).
#' @noRd
valori_filtro <- function(x) {
  x <- trimws(as.character(unlist(x)))
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0) NULL else x
}

#' Colonne che un filtro o un'analisi richiede, con un errore se mancano
#'
#' Contesto: i report girano su file reali che possono avere meno colonne di
#' quelle attese; un errore chiaro evita risultati vuoti inspiegabili.
#' Risultato: niente se le colonne ci sono; altrimenti ferma con un messaggio che
#' nomina il filtro e le colonne.
#' @param letture Data frame da controllare.
#' @param colonne Colonne richieste.
#' @param filtro Nome da mostrare nel messaggio.
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
#' Contesto: i report di confronto si limitano a una parte dei bidoni (per
#' esempio il secco, escluse alcune categorie, solo le domestiche). Il filtro
#' vale allo stesso modo per le letture delle antenne e per quelle delle
#' saponette.
#'
#' Funzionamento: un filtro vuoto (`NULL`, `NA` o `""`) non filtra; più valori in
#' un filtro valgono «uno qualsiasi di»; i tre filtri si applicano insieme.
#' `servizio` tiene le letture con `servizio_atteso` **oppure**
#' `servizio_transponder` uguale (maiuscole ininfluenti; le saponette hanno solo il
#' secondo). `categorie_escluse` toglie quelle categorie e lascia i non censiti,
#' che non ne hanno una. `sottocategoria` tiene solo quella indicata, quindi i
#' non censiti escono.
#'
#' Risultato: le stesse letture, meno quelle escluse.
#' @param letture Data frame con le colonne dei filtri usati.
#' @param servizio Servizio o servizi da tenere.
#' @param categorie_escluse Categorie da togliere.
#' @param sottocategoria Sottocategoria da tenere.
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

#' Letture delle saponette dei giri in cui le antenne hanno letto almeno un RFID
#'
#' Contesto: le saponette non hanno il mezzo né il giro del calendario, ma
#' hanno `matricola_saponetta`: i codici di una trasmissione in un giorno sono letti
#' nello stesso giro. Per confrontare i due sistemi sugli stessi giri si
#' parte dagli RFID delle antenne e si cerca dove le saponette li hanno letti.
#' Il confronto che ne esce non è del tutto equo (un giro comprende anche RFID
#' che le antenne non hanno letto): il report `docs/confronto_antenne.qmd` lo
#' spiega e lo corregge.
#'
#' Funzionamento:
#' 1. tiene le letture delle saponette degli RFID letti dalle antenne;
#' 2. ne prende le coppie distinte di `data_lettura` e `matricola_saponetta`;
#' 3. tiene tutte le letture delle saponette di quelle coppie.
#' Gli RFID si abbinano senza guardare la data; per abbinare giorno per giorno
#' c'è `trasmissioni_dei_giri()`.
#'
#' Risultato: le letture delle saponette di quei giri, con `data_lettura`
#' (data) al posto di quella del file.
#' @param letture_saponette Data frame con `RFID`, `data_ora_lettura` e
#'   `matricola_saponetta`.
#' @param letture_antenne Data frame con `RFID`: le letture già filtrate.
#' @return Le letture delle saponette di quei giri.
#' @noRd
saponette_di_confronto <- function(letture_saponette, letture_antenne) {
  saponette <- letture_saponette |>
    dplyr::mutate(data_lettura = lubridate::as_date(.data$data_ora_lettura))
  giri <- saponette |>
    dplyr::filter(.data$RFID %in% letture_antenne$RFID) |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta)
  dplyr::semi_join(saponette, giri, by = c("data_lettura", "matricola_saponetta"))
}

#' RFID letti ogni giorno, con l'origine della lettura: unione di due serie
#'
#' Contesto: per confrontare i due sistemi si conta ogni RFID una volta al giorno
#' per metodo, e si dice chi lo ha letto.
#' Funzionamento: toglie i doppioni dei due lati (un RFID e giorno una sola
#' volta) e li unisce: `Entrambe` se lo hanno letto lo stesso giorno sia
#' le saponette sia le antenne, altrimenti `Saponette` o `Antenne`.
#' Risultato: `data_lettura`, `RFID` e `origine`, in ordine di giorno e RFID.
#' @param saponette Letture delle saponette già scelte, con `data_lettura` e `RFID`.
#' @param antenne Letture con le antenne già scelte, con `data_ora_lettura` e `RFID`.
#' @return Come `confronta_letture()`.
#' @noRd
unisci_origini <- function(saponette, antenne) {
  da_saponette <- saponette |>
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
#' Contesto: è la funzione di partenza del report `docs/confronto_antenne.qmd`:
#' dice, giorno per giorno, quali RFID hanno visto le saponette (il metodo
#' manuale) e quali le antenne (il metodo nuovo).
#'
#' Funzionamento:
#' 1. filtra le letture con le antenne (`filtra_letture()`);
#' 2. tiene le letture delle saponette dei giri in cui le saponette hanno letto un
#'    RFID di quelle letture (`saponette_di_confronto()`);
#' 3. le unisce, un RFID al più una volta al giorno (`unisci_origini()`).
#'
#' Risultato: `data_lettura`, `RFID` e `origine` (`Saponette`, `Antenne` o
#' `Entrambe`), per giorno e RFID.
#' @param letture_saponette,letture_antenne Data frame (anche da `targets::tar_load()`)
#'   o percorsi dei CSV della pipeline, con `RFID` e `data_ora_lettura`; le
#'   saponette anche con `matricola_saponetta`.
#' @inheritParams filtra_letture
#' @return Tibble con `data_lettura` (data), `RFID` e `origine`.
#' @noRd
confronta_letture <- function(
  letture_saponette = "data-raw/output/letture_saponette.csv",
  letture_antenne = "data-raw/output/letture_antenne.csv",
  servizio = NULL,
  categorie_escluse = NULL,
  sottocategoria = NULL
) {
  antenne <- leggi_letture_confronto(letture_antenne) |>
    filtra_letture(servizio, categorie_escluse, sottocategoria)
  saponette <- leggi_letture_confronto(letture_saponette) |>
    saponette_di_confronto(antenne)

  unisci_origini(saponette, antenne)
}
