# Controllo dei giri con antenne e saponette insieme, per il report
# `docs/controllo_giri_saponette.qmd`: giri pianificati di un giorno, con mezzi
# già attrezzati, in cui gli operatori dovevano usare anche le saponette.
# L'app non usa queste funzioni.

#' Matricola senza spazi, zeri iniziali e maiuscole, per confrontare "0101" con "101"
#' @noRd
chiave_matricola <- function(x) {
  toupper(sub("^0+", "", trimws(as.character(x))))
}

#' Nome del giro senza spazi ai bordi e in maiuscolo, per confrontare "SECCO PaP" con "SECCO PAP"
#' @noRd
chiave_servizio_giro <- function(x) {
  toupper(trimws(as.character(x)))
}

#' Servizio atteso (tipologia) di un servizio del giro, dalla tabella di conversione
#'
#' `"SECCO PaP"` dà `"SECCO"`. Un nome che la tabella non riconosce dà un
#' errore: va aggiunto a `tipologie_servizio()` o indicato nel file dei giri.
#' @param servizio_giro Nomi del calendario.
#' @return I servizi attesi, in maiuscolo.
#' @noRd
servizio_atteso_del_giro <- function(servizio_giro) {
  noti <- unique(conversione_servizi()$servizio)
  atteso <- stringr::str_to_upper(uniforma_servizio(servizio_giro))
  ignoti <- unique(servizio_giro[!atteso %in% noti])
  if (length(ignoti) > 0) {
    stop(
      "Servizio del giro non riconosciuto dalla tabella di conversione: ",
      paste0("\u00ab", ignoti, "\u00bb", collapse = ", "),
      ". Aggiungilo a `tipologie_servizio()` oppure indica il servizio atteso ",
      "nella colonna `servizio_atteso` del file dei giri.",
      call. = FALSE
    )
  }
  atteso
}

#' I giri da controllare, letti da un CSV: una riga per giorno, mezzo e servizio del giro
#'
#' Colonne `data_giro` (`AAAA-MM-GG` o `GG/MM/AAAA`), `matricola_veicolo` e
#' `servizio_giro`; facoltativa `servizio_atteso`, il servizio (tipologia)
#' delle letture di un mezzo senza giro. Senza, si ricava dal nome del giro
#' con la tabella di conversione (`SECCO PaP` dà `SECCO`). Separatore virgola
#' o punto e virgola, nomi delle colonne senza badare alle maiuscole, righe
#' vuote ignorate. Più giorni, mezzi e servizi nello stesso file valgono tutti.
#' @param percorso Percorso del CSV.
#' @return `data_giro` (data), `matricola_veicolo`, `servizio_giro` e
#'   `servizio_atteso` (testi).
#' @noRd
giri_di_controllo <- function(percorso) {
  colonne <- c("data_giro", "matricola_veicolo", "servizio_giro")
  if (!file.exists(percorso)) {
    stop(
      "File dei giri non trovato: ", percorso, ". Serve un CSV con le colonne ",
      paste(colonne, collapse = ", "), ".",
      call. = FALSE
    )
  }
  giri <- leggi_tabella_origine(percorso)
  names(giri) <- tolower(trimws(names(giri)))
  richiedi_colonne(giri, colonne, "giri_controllo")
  giri$servizio_atteso <- giri[["servizio_atteso"]] %||% NA_character_
  giri <- giri[c(colonne, "servizio_atteso")] |>
    dplyr::mutate(riga = dplyr::row_number() + 1L) |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(colonne), \(x) !is.na(x)))

  incompleti <- giri$riga[!stats::complete.cases(giri[colonne])]
  if (length(incompleti) > 0) {
    stop(
      "Righe incomplete nel file dei giri (riga del file): ",
      paste(incompleti, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  giri <- dplyr::mutate(
    giri,
    data_giro = lubridate::as_date(
      lubridate::parse_date_time(.data$data_giro, c("Ymd", "dmY"), quiet = TRUE)
    )
  )
  if (anyNA(giri$data_giro)) {
    stop(
      "Date dei giri non valide (usare AAAA-MM-GG o GG/MM/AAAA), righe: ",
      paste(giri$riga[is.na(giri$data_giro)], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  giri$servizio_atteso <- stringr::str_to_upper(giri$servizio_atteso)
  # La conversione solo dove il file non indica il servizio atteso.
  da_ricavare <- is.na(giri$servizio_atteso)
  if (any(da_ricavare)) {
    giri$servizio_atteso[da_ricavare] <- servizio_atteso_del_giro(
      giri$servizio_giro[da_ricavare]
    )
  }
  giri |>
    dplyr::select(dplyr::all_of(c(colonne, "servizio_atteso"))) |>
    dplyr::distinct()
}

#' Tutte le letture con le antenne dei mezzi della lista, con il criterio che le sceglie
#'
#' Per ogni mezzo e giorno della lista una lettura entra nel confronto se:
#' 1. il suo `servizio_giro` è quello della lista (`"giro del calendario"`); oppure
#' 2. il suo `servizio_giro` è vuoto (il mezzo non ha un giro nel calendario) e
#'    il suo `servizio_atteso` è il servizio atteso della lista
#'    (`"servizio dedotto dai censiti"`).
#' Il nome del giro non si sovrascrive mai: il servizio atteso di un mezzo
#' senza giro viene dai bidoni censiti che ha letto quel giorno.
#' @param antenne Letture con le antenne, con `data_ora_lettura`,
#'   `matricola_veicolo`, `servizio_giro` e `servizio_atteso`.
#' @param giri Risultato di `giri_di_controllo()`.
#' @return Tutte le letture dei mezzi della lista nei giorni della lista, con
#'   `data_lettura`, `chiave_mezzo`, `matricola_lista` (come nella lista) e
#'   `criterio` (mancante se la lettura non entra nel confronto).
#' @noRd
letture_dei_mezzi_della_lista <- function(antenne, giri) {
  richiedi_colonne(
    antenne,
    c("matricola_veicolo", "servizio_giro", "servizio_atteso"),
    "giri_controllo"
  )
  elenco <- giri |>
    dplyr::transmute(
      data_lettura = .data$data_giro,
      matricola_lista = .data$matricola_veicolo,
      chiave_mezzo = chiave_matricola(.data$matricola_veicolo),
      chiave_giro = chiave_servizio_giro(.data$servizio_giro),
      atteso_lista = chiave_servizio_giro(.data$servizio_atteso)
    ) |>
    dplyr::distinct()
  criteri <- c("giro del calendario", "servizio dedotto dai censiti")
  antenne |>
    con_data_lettura() |>
    dplyr::mutate(
      lettura = dplyr::row_number(),
      chiave_mezzo = chiave_matricola(.data$matricola_veicolo),
      giro_lettura = chiave_servizio_giro(.data$servizio_giro),
      atteso_lettura = chiave_servizio_giro(.data$servizio_atteso)
    ) |>
    dplyr::inner_join(
      elenco,
      by = c("data_lettura", "chiave_mezzo"),
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      criterio = dplyr::case_when(
        (.data$giro_lettura == .data$chiave_giro) %in% TRUE ~ criteri[1],
        is.na(.data$giro_lettura) &
          (.data$atteso_lettura == .data$atteso_lista) %in% TRUE ~ criteri[2],
        .default = NA_character_
      )
    ) |>
    # Una lettura con due righe della lista: vale il criterio migliore.
    dplyr::arrange(.data$lettura, is.na(.data$criterio), .data$criterio) |>
    dplyr::distinct(.data$lettura, .keep_all = TRUE) |>
    dplyr::mutate(
      criterio = factor(.data$criterio, criteri)
    ) |>
    dplyr::select(
      -"lettura", -"giro_lettura", -"atteso_lettura", -"chiave_giro", -"atteso_lista"
    )
}

#' Letture con le antenne dei giri della lista
#'
#' Le letture dei mezzi della lista che entrano nel confronto, con il criterio
#' che le ha scelte (`letture_dei_mezzi_della_lista()`).
#' @inheritParams letture_dei_mezzi_della_lista
#' @return Le letture, con `data_lettura`, `chiave_mezzo` e `criterio`.
#' @noRd
antenne_dei_giri <- function(antenne, giri) {
  antenne |>
    letture_dei_mezzi_della_lista(giri) |>
    dplyr::filter(!is.na(.data$criterio)) |>
    dplyr::select(-"matricola_lista")
}

#' Tutte le letture dei mezzi della lista nei giorni della lista, per servizio del giro
#'
#' Serve a capire perché un mezzo risulta senza letture: le letture ci sono
#' ma con un altro servizio del giro, o senza giro (`servizio_giro` vuoto,
#' quando la pipeline non lo ha associato al calendario).
#' @inheritParams letture_dei_mezzi_della_lista
#' @return `data_lettura`, `matricola_veicolo` (come nella lista),
#'   `servizio_giro` (`"(senza giro)"` se vuoto), `servizio_atteso`
#'   (`"(vuoto)"` se mancante), `criterio` (mancante se la lettura non entra) e
#'   `letture`.
#' @noRd
servizi_dei_mezzi_della_lista <- function(antenne, giri) {
  antenne |>
    letture_dei_mezzi_della_lista(giri) |>
    dplyr::mutate(
      servizio_giro = dplyr::coalesce(.data$servizio_giro, "(senza giro)"),
      servizio_atteso = dplyr::coalesce(.data$servizio_atteso, "(vuoto)")
    ) |>
    dplyr::count(
      .data$data_lettura,
      matricola_veicolo = .data$matricola_lista,
      .data$servizio_giro,
      .data$servizio_atteso,
      .data$criterio,
      name = "letture"
    ) |>
    dplyr::arrange(
      .data$data_lettura,
      .data$matricola_veicolo,
      dplyr::desc(.data$letture)
    )
}

#' Letture storiche di tutte le trasmissioni che hanno letto un RFID dei giri
#'
#' Una trasmissione in un giorno è un giro delle saponette: si prendono le
#' coppie di giorno e `id_trasmissione` in cui compare, lo stesso giorno,
#' almeno un RFID letto dalle antenne nei giri della lista, e tutte le
#' letture storiche di quelle coppie.
#' @param storiche Letture storiche, con `data_ora_lettura`,
#'   `id_trasmissione` e `RFID`.
#' @param antenne_giri Risultato di `antenne_dei_giri()`.
#' @return Le letture storiche, con `data_lettura`.
#' @noRd
trasmissioni_dei_giri <- function(storiche, antenne_giri) {
  storiche <- con_data_lettura(storiche)
  giri <- storiche |>
    dplyr::inner_join(
      dplyr::distinct(antenne_giri, .data$data_lettura, .data$RFID),
      by = c("data_lettura", "RFID")
    ) |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione)
  dplyr::semi_join(storiche, giri, by = c("data_lettura", "id_trasmissione"))
}

#' A quale mezzo della lista appartiene ogni trasmissione delle saponette
#'
#' Le letture storiche non hanno il mezzo: una trasmissione è abbinata al
#' mezzo con cui ha più RFID in comune lo stesso giorno (a parità, il primo
#' in ordine di matricola).
#' @param storiche_giri Risultato di `trasmissioni_dei_giri()`.
#' @param antenne_giri Risultato di `antenne_dei_giri()`.
#' @return `data_lettura`, `id_trasmissione`, `chiave_mezzo`, `condivisi`
#'   (RFID in comune con quel mezzo) e `mezzi_in_comune` (mezzi con almeno un
#'   RFID in comune).
#' @noRd
abbina_trasmissioni <- function(storiche_giri, antenne_giri) {
  storiche_giri |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione, .data$RFID) |>
    dplyr::inner_join(
      dplyr::distinct(
        antenne_giri,
        .data$data_lettura,
        .data$chiave_mezzo,
        .data$RFID
      ),
      by = c("data_lettura", "RFID"),
      relationship = "many-to-many"
    ) |>
    dplyr::count(
      .data$data_lettura,
      .data$id_trasmissione,
      .data$chiave_mezzo,
      name = "condivisi"
    ) |>
    dplyr::mutate(
      mezzi_in_comune = dplyr::n(),
      .by = c("data_lettura", "id_trasmissione")
    ) |>
    dplyr::arrange(dplyr::desc(.data$condivisi), .data$chiave_mezzo) |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione, .keep_all = TRUE) |>
    dplyr::arrange(.data$data_lettura, .data$id_trasmissione)
}

#' RFID del giorno letti da entrambi i sistemi, solo dalle antenne, solo dalle saponette
#' @return `data_lettura`, `RFID` e `classe` (fattore).
#' @noRd
classi_rfid <- function(antenne_giri, storiche_giri) {
  da_antenne <- antenne_giri |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(antenne = TRUE)
  da_saponette <- storiche_giri |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(saponette = TRUE)
  dplyr::full_join(da_antenne, da_saponette, by = c("data_lettura", "RFID")) |>
    dplyr::mutate(
      classe = dplyr::case_when(
        .data$antenne %in% TRUE & .data$saponette %in% TRUE ~ "Entrambi",
        .data$antenne %in% TRUE ~ "Solo antenne",
        .default = "Solo saponette"
      ),
      classe = factor(
        .data$classe,
        c("Solo saponette", "Entrambi", "Solo antenne")
      )
    ) |>
    dplyr::select("data_lettura", "RFID", "classe") |>
    dplyr::arrange(.data$data_lettura, .data$RFID)
}

#' Per ogni mezzo della lista: RFID letti dalle antenne, dalle saponette e da entrambi
#'
#' Le saponette di un mezzo sono quelle delle trasmissioni abbinate a lui.
#' Un mezzo senza letture delle antenne o senza trasmissioni abbinate resta
#' nell'elenco, con zero.
#' @param giri Risultato di `giri_di_controllo()`.
#' @param abbinamenti Risultato di `abbina_trasmissioni()`.
#' @return `data_lettura`, `matricola_veicolo`, `trasmissioni`,
#'   `chip_antenne`, `chip_saponette`, `chip_entrambi`, `solo_antenne`,
#'   `solo_saponette`.
#' @noRd
confronto_per_mezzo <- function(giri, antenne_giri, storiche_giri, abbinamenti) {
  mezzi <- giri |>
    dplyr::distinct(data_lettura = .data$data_giro, .data$matricola_veicolo) |>
    dplyr::mutate(chiave_mezzo = chiave_matricola(.data$matricola_veicolo)) |>
    dplyr::distinct(.data$data_lettura, .data$chiave_mezzo, .keep_all = TRUE)
  chip_antenne <- antenne_giri |>
    dplyr::distinct(.data$data_lettura, .data$chiave_mezzo, .data$RFID)
  chip_saponette <- storiche_giri |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione, .data$RFID) |>
    dplyr::inner_join(
      abbinamenti[c("data_lettura", "id_trasmissione", "chiave_mezzo")],
      by = c("data_lettura", "id_trasmissione")
    ) |>
    dplyr::distinct(.data$data_lettura, .data$chiave_mezzo, .data$RFID)
  entrambi <- dplyr::inner_join(
    chip_antenne,
    chip_saponette,
    by = c("data_lettura", "chiave_mezzo", "RFID")
  )
  per_mezzo <- c("data_lettura", "chiave_mezzo")

  mezzi |>
    dplyr::left_join(
      dplyr::count(abbinamenti, dplyr::pick(dplyr::all_of(per_mezzo)), name = "trasmissioni"),
      by = per_mezzo
    ) |>
    dplyr::left_join(
      dplyr::count(chip_antenne, dplyr::pick(dplyr::all_of(per_mezzo)), name = "chip_antenne"),
      by = per_mezzo
    ) |>
    dplyr::left_join(
      dplyr::count(chip_saponette, dplyr::pick(dplyr::all_of(per_mezzo)), name = "chip_saponette"),
      by = per_mezzo
    ) |>
    dplyr::left_join(
      dplyr::count(entrambi, dplyr::pick(dplyr::all_of(per_mezzo)), name = "chip_entrambi"),
      by = per_mezzo
    ) |>
    dplyr::mutate(
      dplyr::across(
        c("trasmissioni", "chip_antenne", "chip_saponette", "chip_entrambi"),
        \(x) dplyr::coalesce(x, 0L)
      ),
      solo_antenne = .data$chip_antenne - .data$chip_entrambi,
      solo_saponette = .data$chip_saponette - .data$chip_entrambi
    ) |>
    dplyr::select(-"chiave_mezzo") |>
    dplyr::arrange(.data$data_lettura, .data$matricola_veicolo)
}

#' Dove sono gli RFID letti solo dalle saponette
#'
#' Se un'antenna li ha letti lo stesso giorno fuori dai giri della lista, la
#' differenza non è una lettura mancata dell'antenna: l'RFID è passato in un
#' altro giro. Per gli RFID letti solo dalle antenne la domanda non ha senso:
#' una trasmissione che li avesse letti sarebbe già tra quelle scelte.
#' @param classi Risultato di `classi_rfid()`.
#' @param antenne_tutte Letture con le antenne di tutti i mezzi, senza filtri.
#' @return `motivo` e `n`.
#' @noRd
solo_saponette_altrove <- function(classi, antenne_tutte) {
  antenne_del_giorno <- antenne_tutte |>
    con_data_lettura() |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(antenna_del_giorno = TRUE)
  classi |>
    dplyr::filter(.data$classe == "Solo saponette") |>
    dplyr::left_join(antenne_del_giorno, by = c("data_lettura", "RFID")) |>
    dplyr::mutate(
      motivo = dplyr::if_else(
        .data$antenna_del_giorno %in% TRUE,
        "Letto da un'antenna lo stesso giorno, fuori dai giri della lista",
        "Non letto da nessuna antenna lo stesso giorno"
      )
    ) |>
    dplyr::count(.data$motivo)
}

#' Servizio a database degli RFID, per classe di lettura
#' @return `classe`, `servizio` (`"(non censito)"` se manca) e `n`.
#' @noRd
servizi_per_classe <- function(classi, antenne_giri, storiche_giri) {
  anagrafica <- dplyr::bind_rows(
    dplyr::select(antenne_giri, "data_lettura", "RFID", "servizio_transponder"),
    dplyr::select(storiche_giri, "data_lettura", "RFID", "servizio_transponder")
  ) |>
    dplyr::arrange(is.na(.data$servizio_transponder)) |>
    dplyr::distinct(.data$data_lettura, .data$RFID, .keep_all = TRUE)
  classi |>
    dplyr::left_join(anagrafica, by = c("data_lettura", "RFID")) |>
    dplyr::mutate(
      servizio = dplyr::coalesce(.data$servizio_transponder, "(non censito)")
    ) |>
    dplyr::count(.data$classe, .data$servizio, .drop = FALSE) |>
    dplyr::filter(.data$n > 0)
}

#' Trasmissioni per numero di RFID in comune con il mezzo abbinato
#'
#' Una trasmissione abbinata per un solo RFID è la meno sicura: la tabella
#' dice quante ce ne sono.
#' @return `fascia` (fattore) e `trasmissioni`.
#' @noRd
fasce_di_abbinamento <- function(abbinamenti) {
  fasce <- c("1 RFID", "2-4 RFID", "5 RFID o pi\u00f9")
  abbinamenti |>
    dplyr::mutate(
      fascia = dplyr::case_when(
        .data$condivisi == 1 ~ fasce[1],
        .data$condivisi <= 4 ~ fasce[2],
        .default = fasce[3]
      ),
      fascia = factor(.data$fascia, fasce)
    ) |>
    dplyr::count(.data$fascia, name = "trasmissioni", .drop = FALSE)
}
