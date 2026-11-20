# Controllo dei giri con antenne e saponette insieme, per il report
# `docs/controllo_giri_saponette.qmd`: giri pianificati di un giorno, con mezzi
# già attrezzati, in cui gli operatori dovevano usare anche le saponette.
# L'app non usa queste funzioni.

#' Nome del giro senza spazi ai bordi e in maiuscolo, per confrontare "SECCO PaP" con "SECCO PAP"
#'
#' Contesto: la lista dei giri da controllare è scritta a mano e può avere
#' maiuscole diverse dal calendario. Risultato: la chiave di confronto.
#' @noRd
chiave_servizio_giro <- function(x) {
  toupper(trimws(as.character(x)))
}

#' Servizio atteso (tipologia) di un servizio del giro, dalla tabella di conversione
#'
#' Contesto: per un mezzo senza giro nel calendario le letture hanno il servizio
#' atteso dedotto dai bidoni (es. `SECCO`), non il nome del giro (`SECCO PAP`);
#' per riconoscerle il report deve sapere quale tipologia corrisponde al giro
#' della lista.
#' Funzionamento: converte il nome con la tabella `conversione_servizi()`:
#' `SECCO PaP` dà `SECCO`, `CARTA CONT.STRADALI` dà `CARTA`.
#' Risultato: i servizi in maiuscolo. Un nome che la tabella non riconosce
#' ferma con un messaggio: va aggiunto a `tipologie_servizio()` oppure indicato
#' nella colonna `servizio_atteso` del file dei giri.
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
#' Contesto: il report `docs/controllo_giri_saponette.qmd` confronta i due
#' sistemi su giri in cui si sa che il mezzo ha lavorato e gli operatori
#' dovevano usare anche le saponette. La lista dei giri sta in un file
#' (`data-raw/origine/giri_controllo.csv`), così per un nuovo confronto basta
#' cambiare il file.
#'
#' Funzionamento: legge le colonne `data_giro` (`AAAA-MM-GG` o `GG/MM/AAAA`),
#' `matricola_veicolo`, `servizio_giro` e, facoltativa, `servizio_atteso`
#' (il servizio delle letture di un mezzo senza giro; senza, si ricava dal nome
#' del giro con `servizio_atteso_del_giro()`). Separatore virgola o punto e
#' virgola, nomi delle colonne senza badare alle maiuscole, righe vuote ignorate;
#' righe a metà e date illeggibili fermano con il numero di riga. Più giorni,
#' mezzi e servizi nello stesso file valgono tutti, ognuno con il servizio della
#' sua riga.
#'
#' Risultato: `data_giro` (data), `matricola_veicolo`, `servizio_giro` e
#' `servizio_atteso` (testi), senza righe ripetute. Con `facoltative`, anche
#' quelle colonne, come testo (vuote se il file non le ha): il report degli
#' indicatori ne usa tre, `matricola_saponetta`, `ora_inizio` e `ora_fine`.
#' @param percorso Percorso del CSV.
#' @param facoltative Altre colonne da leggere, se ci sono.
#' @return `data_giro` (data), `matricola_veicolo`, `servizio_giro` e
#'   `servizio_atteso` (testi), più le colonne facoltative.
#' @noRd
giri_di_controllo <- function(percorso, facoltative = character(0)) {
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
  for (colonna in setdiff(facoltative, names(giri))) {
    giri[[colonna]] <- NA_character_
  }
  giri <- giri[c(colonne, "servizio_atteso", facoltative)] |>
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
    dplyr::select(dplyr::all_of(c(colonne, "servizio_atteso", facoltative))) |>
    dplyr::distinct()
}

#' Tutte le letture con le antenne dei mezzi della lista, con il criterio che le sceglie
#'
#' Contesto: un mezzo senza un giro nel calendario ha le letture con
#' `servizio_giro` vuoto, ma la lista dice che il giro c'era. Il report tiene
#' quindi due criteri e li mostra separati.
#'
#' Funzionamento: per ogni mezzo e giorno della lista (mezzo senza zeri iniziali)
#' una lettura entra nel confronto se:
#' 1. il suo `servizio_giro` è quello della lista («giro del calendario»); oppure
#' 2. il suo `servizio_giro` è vuoto e il suo `servizio_atteso` è il servizio
#'    atteso della lista («servizio dedotto dai censiti»).
#' Il nome del giro non si sovrascrive mai: il servizio atteso di un mezzo senza
#' giro viene dai bidoni censiti del tratto che ha letto. Se una lettura
#' soddisfa due righe della lista vale il criterio del giro del calendario.
#'
#' Risultato: tutte le letture dei mezzi della lista nei giorni della lista, anche
#' quelle che non entrano (`criterio` mancante), con `riga_lista`, la riga della
#' lista che le ha scelte.
#' @param antenne Letture con le antenne, con `data_ora_lettura`,
#'   `matricola_veicolo`, `servizio_giro` e `servizio_atteso`.
#' @param giri Risultato di `giri_di_controllo()`.
#' @return Letture con `data_lettura`, `chiave_mezzo`, `matricola_lista` (come
#'   nella lista), `riga_lista` e `criterio`.
#' @noRd
letture_dei_mezzi_della_lista <- function(antenne, giri) {
  richiedi_colonne(
    antenne,
    c("matricola_veicolo", "servizio_giro", "servizio_atteso"),
    "giri_controllo"
  )
  elenco <- giri |>
    dplyr::transmute(
      riga_lista = dplyr::row_number(),
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
#' Contesto: la prima tappa del report sui giri di controllo: quali letture
#' delle antenne appartengono ai giri che si vogliono confrontare.
#' Risultato: le letture dei mezzi della lista che entrano nel confronto, con il
#' criterio che le ha scelte (`letture_dei_mezzi_della_lista()`).
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
#' Contesto: serve a capire perché un mezzo risulta senza letture nel confronto:
#' le letture ci sono ma con un altro servizio del giro, o senza giro
#' (`servizio_giro` vuoto, quando la pipeline non lo ha associato al calendario).
#' Risultato: per mezzo, giorno, servizio del giro, servizio atteso e criterio,
#' quante letture, con `(senza giro)` e `(vuoto)` dove mancano i valori.
#' @inheritParams letture_dei_mezzi_della_lista
#' @return `data_lettura`, `matricola_veicolo` (come nella lista), `servizio_giro`,
#'   `servizio_atteso`, `criterio` e `letture`.
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

#' Letture delle saponette di tutte le trasmissioni che hanno letto un RFID dei giri
#'
#' Contesto: le saponette non hanno il mezzo, ma hanno `matricola_saponetta`: una
#' trasmissione in un giorno è un giro delle saponette. Per sapere che cosa hanno
#' letto le saponette nei giri della lista si parte dagli RFID letti dalle antenne.
#' Funzionamento: prende le coppie giorno e `matricola_saponetta` in cui compare,
#' lo **stesso giorno**, almeno uno degli RFID delle antenne dei giri, e tutte le
#' letture delle saponette di quelle coppie (anche di RFID che le antenne non
#' hanno letto).
#' Risultato: le letture delle saponette, con `data_lettura`.
#' @param saponette Letture delle saponette, con `data_ora_lettura`,
#'   `matricola_saponetta` e `RFID`.
#' @param antenne_giri Risultato di `antenne_dei_giri()`.
#' @return Le letture delle saponette, con `data_lettura`.
#' @noRd
trasmissioni_dei_giri <- function(saponette, antenne_giri) {
  saponette <- con_data_lettura(saponette)
  giri <- saponette |>
    dplyr::inner_join(
      dplyr::distinct(antenne_giri, .data$data_lettura, .data$RFID),
      by = c("data_lettura", "RFID")
    ) |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta)
  dplyr::semi_join(saponette, giri, by = c("data_lettura", "matricola_saponetta"))
}

#' A quale mezzo della lista appartiene ogni trasmissione delle saponette
#'
#' Contesto: le letture delle saponette non hanno il mezzo, ma il confronto è
#' per mezzo. Si deduce il mezzo dagli RFID che lui e la trasmissione hanno
#' letto lo stesso giorno.
#' Funzionamento: per ogni trasmissione conta gli RFID in comune con ogni mezzo
#' della lista e sceglie il mezzo con più RFID in comune (a parità, il primo in
#' ordine di matricola). Una trasmissione con un solo RFID in comune è la meno
#' sicura.
#' Risultato: `data_lettura`, `matricola_saponetta`, `chiave_mezzo`, `condivisi`
#' (RFID in comune con quel mezzo) e `mezzi_in_comune` (mezzi con almeno un RFID
#' in comune).
#' @param saponette_giri Risultato di `trasmissioni_dei_giri()`.
#' @param antenne_giri Risultato di `antenne_dei_giri()`.
#' @noRd
abbina_trasmissioni <- function(saponette_giri, antenne_giri) {
  saponette_giri |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .data$RFID) |>
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
      .data$matricola_saponetta,
      .data$chiave_mezzo,
      name = "condivisi"
    ) |>
    dplyr::mutate(
      mezzi_in_comune = dplyr::n(),
      .by = c("data_lettura", "matricola_saponetta")
    ) |>
    dplyr::arrange(dplyr::desc(.data$condivisi), .data$chiave_mezzo) |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .keep_all = TRUE) |>
    dplyr::arrange(.data$data_lettura, .data$matricola_saponetta)
}

#' RFID del giorno letti da entrambi i sistemi, solo dalle antenne, solo dalle saponette
#'
#' Contesto: il risultato principale del report sui giri di controllo: se le
#' saponette fossero complete, ogni RFID sarebbe letto da entrambi.
#' Risultato: per ogni giorno e RFID, la `classe` (fattore: `Solo saponette`,
#' `Entrambi`, `Solo antenne`).
#' @return `data_lettura`, `RFID` e `classe`.
#' @noRd
classi_rfid <- function(antenne_giri, saponette_giri) {
  da_antenne <- antenne_giri |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(antenne = TRUE)
  da_saponette <- saponette_giri |>
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
#' Contesto: il dettaglio per mezzo del report sui giri di controllo. Le
#' saponette di un mezzo sono quelle delle trasmissioni abbinate a lui
#' (`abbina_trasmissioni()`).
#' Risultato: una riga per mezzo e giorno con le trasmissioni abbinate e gli RFID
#' delle due serie. Un mezzo senza letture delle antenne o senza trasmissioni
#' abbinate resta nell'elenco, con zero.
#' @param giri Risultato di `giri_di_controllo()`.
#' @param antenne_giri Risultato di `antenne_dei_giri()`.
#' @param saponette_giri Risultato di `trasmissioni_dei_giri()`.
#' @param abbinamenti Risultato di `abbina_trasmissioni()`.
#' @return `data_lettura`, `matricola_veicolo`, `trasmissioni`, `chip_antenne`,
#'   `chip_saponette`, `chip_entrambi`, `solo_antenne`, `solo_saponette`.
#' @noRd
confronto_per_mezzo <- function(giri, antenne_giri, saponette_giri, abbinamenti) {
  mezzi <- giri |>
    dplyr::distinct(data_lettura = .data$data_giro, .data$matricola_veicolo) |>
    dplyr::mutate(chiave_mezzo = chiave_matricola(.data$matricola_veicolo)) |>
    dplyr::distinct(.data$data_lettura, .data$chiave_mezzo, .keep_all = TRUE)
  chip_antenne <- antenne_giri |>
    dplyr::distinct(.data$data_lettura, .data$chiave_mezzo, .data$RFID)
  chip_saponette <- saponette_giri |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .data$RFID) |>
    dplyr::inner_join(
      abbinamenti[c("data_lettura", "matricola_saponetta", "chiave_mezzo")],
      by = c("data_lettura", "matricola_saponetta")
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
#' Contesto: un RFID letto solo dalle saponette può essere una lettura mancata
#' dall'antenna oppure un bidone passato in un altro giro. Se un'antenna fuori
#' lista lo ha letto lo stesso giorno, è il secondo caso.
#' Per gli RFID letti solo dalle antenne la domanda non ha senso: una
#' trasmissione che li avesse letti sarebbe già tra quelle scelte.
#' Risultato: quanti sono letti da un'antenna fuori dai giri della lista e
#' quanti da nessuna antenna lo stesso giorno.
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
#'
#' Contesto: un giro di un servizio dovrebbe leggere i contenitori di quel
#' servizio; la tabella dice quanto le tre classi (solo saponette, entrambi,
#' solo antenne) lo rispettano.
#' Risultato: per classe e servizio, quanti RFID (`(non censito)` se manca il
#' servizio).
#' @return `classe`, `servizio` e `n`.
#' @noRd
servizi_per_classe <- function(classi, antenne_giri, saponette_giri) {
  anagrafica <- dplyr::bind_rows(
    dplyr::select(antenne_giri, "data_lettura", "RFID", "servizio_transponder"),
    dplyr::select(saponette_giri, "data_lettura", "RFID", "servizio_transponder")
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
#' Contesto: una trasmissione abbinata per un solo RFID è la meno sicura.
#' Risultato: quante trasmissioni hanno 1, da 2 a 4, o 5 e più RFID in comune.
#' @param abbinamenti Risultato di `abbina_trasmissioni()`.
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

# ---- Tipo di chip dei bidoni letti dalle saponette e non dalle antenne ----------
# Le antenne leggono solo i chip UHF; le saponette leggono sia i chip LF sia gli
# UHF (richiesta del proprietario, 21/11/2026). Un bidone letto dalla saponetta e
# non dall'antenna è giustificato se il suo chip è solo LF; se ha una parte UHF
# l'antenna avrebbe dovuto leggerlo.

#' Categorie di chip per la lettura delle antenne
#'
#' Contesto: i nove tipi di supporto (`tipi_supporto_chip()`) si riducono a quello
#' che interessa al confronto: l'antenna poteva leggere il chip?
#' Risultato: i nomi delle quattro categorie, nell'ordine del report: `Chip solo LF`
#' (le antenne non lo leggono), `Chip con UHF` (le antenne lo leggono), `Possibile UHF`
#' (non è nei tag ma è stato attivato dal 2016, vedi `associa_tipo_supporto_chip()`),
#' `Tipo non determinabile` (`SCONOSCIUTO` o senza tipo).
#' @noRd
categorie_chip <- function() {
  c("Chip solo LF", "Chip con UHF", "Possibile UHF", "Tipo non determinabile")
}

#' Categoria di chip di una descrizione del tipo di supporto
#'
#' Contesto: `categorie_chip()`.
#' Funzionamento: la descrizione `POSSIBILE UHF` è una categoria a sé; per gli
#' altri tipi vale la colonna `ha_uhf` di `tipi_supporto_chip()` (`TRUE` con UHF,
#' `FALSE` solo LF). `SCONOSCIUTO`, una descrizione mancante o non riconosciuta
#' sono «non determinabili».
#' Risultato: un fattore con i livelli di `categorie_chip()`.
#' @param descrizione `descrizione_tipo_supporto_chip`.
#' @noRd
categoria_chip <- function(descrizione) {
  tipi <- tipi_supporto_chip()
  ha_uhf <- tipi$ha_uhf[match(descrizione, tipi$descrizione_tipo_supporto_chip)]
  categoria <- dplyr::case_when(
    descrizione %in% "POSSIBILE UHF" ~ "Possibile UHF",
    ha_uhf %in% TRUE ~ "Chip con UHF",
    ha_uhf %in% FALSE ~ "Chip solo LF",
    .default = "Tipo non determinabile"
  )
  factor(categoria, categorie_chip())
}

#' Esiti possibili per un bidone letto dalla saponetta
#'
#' Contesto: i grafici del report dividono i bidoni letti dalle saponette tra quelli
#' che le antenne hanno letto e quelli che non hanno letto, questi ultimi secondo la
#' categoria del chip.
#' Risultato: i cinque esiti, nell'ordine dei grafici.
#' @noRd
esiti_antenne <- function() {
  c(
    "Letto dalle antenne",
    "Non letto: chip solo LF",
    "Non letto: chip con UHF",
    "Non letto: possibile UHF",
    "Non letto: tipo non determinabile"
  )
}

#' Esito per le antenne di un bidone letto dalle saponette
#'
#' Contesto: `esiti_antenne()`. Funzionamento: `Entrambi` è «letto dalle antenne»;
#' `Solo saponette` è «non letto», con la categoria del chip; `Solo antenne` non ha
#' un esito (non è un bidone letto dalle saponette).
#' Risultato: un fattore con i livelli di `esiti_antenne()`.
#' @param classe Classe di `classi_rfid()`.
#' @param categoria Categoria di `categoria_chip()`.
#' @noRd
esito_antenne <- function(classe, categoria) {
  esiti <- esiti_antenne()
  classe <- as.character(classe)
  categoria <- as.character(categoria)
  esito <- dplyr::case_when(
    classe == "Entrambi" ~ esiti[1],
    classe == "Solo saponette" & categoria == "Chip solo LF" ~ esiti[2],
    classe == "Solo saponette" & categoria == "Chip con UHF" ~ esiti[3],
    classe == "Solo saponette" & categoria == "Possibile UHF" ~ esiti[4],
    classe == "Solo saponette" ~ esiti[5],
    .default = NA_character_
  )
  factor(esito, esiti)
}

#' Tipo di chip di ogni RFID letto da uno dei due sistemi
#'
#' Contesto: il tipo di supporto del chip è nei file delle letture, antenne e
#' saponette, come `descrizione_tipo_supporto_chip` (la pipeline lo associa dopo i
#' tentativi di associazione). Per ogni RFID e giorno serve un solo valore.
#' Funzionamento: unisce le due serie e tiene, per giorno e RFID, la prima
#' descrizione non vuota. Una serie senza la colonna conta come vuota.
#' Risultato: `data_lettura`, `RFID` e `descrizione_tipo_supporto_chip`.
#' @param antenne_giri Risultato di `antenne_dei_giri()`.
#' @param saponette_giri Risultato di `trasmissioni_dei_giri()`.
#' @noRd
chip_degli_rfid <- function(antenne_giri, saponette_giri) {
  della_serie <- function(x) {
    dplyr::tibble(
      data_lettura = x$data_lettura,
      RFID = x$RFID,
      descrizione_tipo_supporto_chip = as.character(
        x[["descrizione_tipo_supporto_chip"]] %||% rep(NA_character_, nrow(x))
      )
    )
  }
  dplyr::bind_rows(della_serie(saponette_giri), della_serie(antenne_giri)) |>
    dplyr::arrange(is.na(.data$descrizione_tipo_supporto_chip)) |>
    dplyr::distinct(.data$data_lettura, .data$RFID, .keep_all = TRUE)
}

#' Le classi di lettura (entrambi, solo saponette, solo antenne) con il tipo di chip
#'
#' Contesto: il punto di partenza dell'analisi del tipo di chip: a ogni RFID del
#' confronto si aggiungono il tipo di chip, la categoria per le antenne e, per i
#' bidoni letti dalle saponette, l'esito.
#' Risultato: `data_lettura`, `RFID`, `classe`, `descrizione_tipo_supporto_chip`,
#' `categoria` (`categoria_chip()`) ed `esito` (`esito_antenne()`, vuoto per i
#' bidoni letti solo dalle antenne).
#' @param classi Risultato di `classi_rfid()`.
#' @inheritParams chip_degli_rfid
#' @noRd
classi_con_chip <- function(classi, antenne_giri, saponette_giri) {
  classi |>
    dplyr::left_join(
      chip_degli_rfid(antenne_giri, saponette_giri),
      by = c("data_lettura", "RFID")
    ) |>
    dplyr::mutate(
      categoria = categoria_chip(.data$descrizione_tipo_supporto_chip),
      esito = esito_antenne(.data$classe, .data$categoria)
    )
}

#' Bidoni letti dalle saponette e quota letta dalle antenne, per tipo di chip
#'
#' Contesto: la domanda del report: i bidoni che le antenne non hanno letto hanno
#' un chip che le antenne potevano leggere? Per ogni tipo (o categoria) di chip si
#' vede quanti bidoni hanno letto le saponette e quanti di questi le antenne.
#' Funzionamento: «bidoni letti dalle saponette» sono quelli `Entrambi` più i
#' `Solo saponette`; la quota delle antenne è `Entrambi` diviso quel totale (vuota
#' senza bidoni letti dalle saponette). Un tipo mancante diventa `(nessun tipo)`.
#' Risultato: una riga per categoria (e per descrizione con `per = "descrizione"`)
#' con `saponette`, `entrambi`, `solo_saponette`, `solo_antenne` e `quota_antenne`.
#' Le righe seguono l'ordine di `categorie_chip()` e dei tipi.
#' @param classi_chip Risultato di `classi_con_chip()`.
#' @param per `"descrizione"` (predefinito) o `"categoria"`.
#' @noRd
riepilogo_per_chip <- function(classi_chip, per = c("descrizione", "categoria")) {
  per <- match.arg(per)
  ordine <- c(
    tipi_supporto_chip()$descrizione_tipo_supporto_chip,
    "POSSIBILE UHF",
    "(nessun tipo)"
  )
  chiavi <- if (per == "descrizione") c("categoria", "descrizione") else "categoria"
  classi_chip |>
    dplyr::mutate(
      descrizione = dplyr::coalesce(.data$descrizione_tipo_supporto_chip, "(nessun tipo)")
    ) |>
    dplyr::summarise(
      saponette = sum(.data$classe %in% c("Entrambi", "Solo saponette")),
      entrambi = sum(.data$classe %in% "Entrambi"),
      solo_saponette = sum(.data$classe %in% "Solo saponette"),
      solo_antenne = sum(.data$classe %in% "Solo antenne"),
      .by = dplyr::all_of(chiavi)
    ) |>
    dplyr::mutate(
      quota_antenne = rapporto(.data$entrambi, .data$saponette),
      posizione = if (per == "descrizione") match(.data$descrizione, ordine) else 0L
    ) |>
    dplyr::arrange(.data$categoria, .data$posizione) |>
    dplyr::select(-"posizione")
}

#' I numeri che dicono se le antenne sono giustificate
#'
#' Contesto: sintesi del report sul tipo di chip. Se i bidoni che le antenne non
#' hanno letto sono quasi tutti solo LF, le antenne sono giustificate; se molti
#' hanno un chip con UHF, c'è un problema.
#' Funzionamento: parte dai bidoni letti dalle saponette. Calcola: quanti sono
#' (`saponette`), quanti le antenne hanno letto (`antenne`) e non letto
#' (`non_letti`), e questi ultimi per categoria di chip (`non_letti_solo_lf`,
#' `non_letti_con_uhf`, `non_letti_possibile_uhf`, `non_letti_non_determinabile`).
#' Tre coperture: `copertura` (letti / tutti), `copertura_senza_lf` (letti / tutti
#' meno i non letti solo LF, cioè sui bidoni che l'antenna poteva leggere o di cui
#' non si sa), `copertura_con_uhf` (sui soli bidoni con chip UHF noto) e
#' `copertura_con_uhf_possibile` (aggiunge i possibili UHF). Infine
#' `antenne_su_chip_solo_lf`: gli RFID che le antenne hanno letto pur avendo un chip
#' solo LF (non dovrebbero esserci: sarebbe un dato da controllare).
#' Risultato: un tibble con una riga.
#' @inheritParams riepilogo_per_chip
#' @noRd
sintesi_chip <- function(classi_chip) {
  esiti <- esiti_antenne()
  dalle_saponette <- classi_chip[classi_chip$classe != "Solo antenne", ]
  conta <- function(esito, categoria = NULL) {
    sum(
      dalle_saponette$esito %in% esito &
        (is.null(categoria) | dalle_saponette$categoria %in% categoria)
    )
  }
  letti <- conta(esiti[1])
  letti_con_uhf <- conta(esiti[1], "Chip con UHF")
  letti_possibile <- conta(esiti[1], "Possibile UHF")
  non_letti_solo_lf <- conta(esiti[2])
  non_letti_con_uhf <- conta(esiti[3])
  non_letti_possibile <- conta(esiti[4])
  non_letti_non_determinabile <- conta(esiti[5])
  saponette <- nrow(dalle_saponette)
  dplyr::tibble(
    saponette = saponette,
    antenne = letti,
    non_letti = saponette - letti,
    non_letti_solo_lf = non_letti_solo_lf,
    non_letti_con_uhf = non_letti_con_uhf,
    non_letti_possibile_uhf = non_letti_possibile,
    non_letti_non_determinabile = non_letti_non_determinabile,
    copertura = rapporto(letti, saponette),
    copertura_senza_lf = rapporto(letti, saponette - non_letti_solo_lf),
    copertura_con_uhf = rapporto(letti_con_uhf, letti_con_uhf + non_letti_con_uhf),
    copertura_con_uhf_possibile = rapporto(
      letti_con_uhf + letti_possibile,
      letti_con_uhf + letti_possibile + non_letti_con_uhf + non_letti_possibile
    ),
    antenne_su_chip_solo_lf = sum(
      classi_chip$classe %in% c("Entrambi", "Solo antenne") &
        classi_chip$categoria %in% "Chip solo LF"
    )
  )
}

#' I bidoni letti solo dalle saponette, per categoria di chip e dove li hanno letti le antenne
#'
#' Contesto: un bidone con chip UHF letto dalla saponetta e non dalle antenne del
#' giro può essere passato in un altro giro: se un'antenna fuori lista lo ha letto
#' lo stesso giorno, non è una lettura mancata (`solo_saponette_altrove()`).
#' Risultato: per ogni categoria di chip e per `letto_altrove` (vero se un'antenna
#' fuori dai giri della lista lo ha letto lo stesso giorno) quanti bidoni, anche
#' zero.
#' @inheritParams riepilogo_per_chip
#' @param antenne_tutte Letture con le antenne di tutti i mezzi, senza filtri.
#' @return `categoria`, `letto_altrove` e `n`.
#' @noRd
solo_saponette_per_categoria <- function(classi_chip, antenne_tutte) {
  antenne_del_giorno <- antenne_tutte |>
    con_data_lettura() |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(antenna_del_giorno = TRUE)
  conteggi <- classi_chip |>
    dplyr::filter(.data$classe == "Solo saponette") |>
    dplyr::left_join(antenne_del_giorno, by = c("data_lettura", "RFID")) |>
    dplyr::mutate(letto_altrove = .data$antenna_del_giorno %in% TRUE) |>
    dplyr::count(.data$categoria, .data$letto_altrove)
  expand.grid(
    categoria = factor(categorie_chip(), categorie_chip()),
    letto_altrove = c(FALSE, TRUE)
  ) |>
    dplyr::as_tibble() |>
    dplyr::left_join(conteggi, by = c("categoria", "letto_altrove")) |>
    dplyr::mutate(n = dplyr::coalesce(.data$n, 0L)) |>
    dplyr::arrange(.data$categoria, .data$letto_altrove)
}

#' Esito dei bidoni letti dalle saponette, mezzo per mezzo
#'
#' Contesto: dice quali mezzi hanno molti bidoni con chip UHF non letti dalle antenne
#' (un problema dell'antenna di quel mezzo) e quali solo bidoni con chip LF (atteso).
#' Funzionamento: come in `confronto_per_mezzo()`, le saponette di un mezzo sono
#' quelle delle trasmissioni abbinate a lui (`abbina_trasmissioni()`).
#' Risultato: per giorno, mezzo ed esito (`esiti_antenne()`, tutti anche con zero)
#' quanti bidoni: `data_lettura`, `matricola_veicolo` (come nella lista), `esito`, `n`.
#' Un mezzo senza trasmissioni abbinate non compare.
#' @param giri Risultato di `giri_di_controllo()`.
#' @param saponette_giri Risultato di `trasmissioni_dei_giri()`.
#' @param abbinamenti Risultato di `abbina_trasmissioni()`.
#' @inheritParams riepilogo_per_chip
#' @noRd
persi_per_mezzo <- function(giri, saponette_giri, abbinamenti, classi_chip) {
  mezzi <- giri |>
    dplyr::distinct(data_lettura = .data$data_giro, .data$matricola_veicolo) |>
    dplyr::mutate(chiave_mezzo = chiave_matricola(.data$matricola_veicolo)) |>
    dplyr::distinct(.data$data_lettura, .data$chiave_mezzo, .keep_all = TRUE)
  saponette_giri |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .data$RFID) |>
    dplyr::inner_join(
      abbinamenti[c("data_lettura", "matricola_saponetta", "chiave_mezzo")],
      by = c("data_lettura", "matricola_saponetta")
    ) |>
    dplyr::distinct(.data$data_lettura, .data$chiave_mezzo, .data$RFID) |>
    dplyr::inner_join(
      classi_chip[c("data_lettura", "RFID", "esito")],
      by = c("data_lettura", "RFID")
    ) |>
    dplyr::count(.data$data_lettura, .data$chiave_mezzo, .data$esito, .drop = FALSE) |>
    dplyr::inner_join(mezzi, by = c("data_lettura", "chiave_mezzo")) |>
    dplyr::select("data_lettura", "matricola_veicolo", "esito", "n") |>
    dplyr::arrange(.data$data_lettura, .data$matricola_veicolo, .data$esito)
}

#' Elenco dei bidoni con un chip che le antenne potevano leggere e non hanno letto
#'
#' Contesto: sono i casi da controllare uno per uno: un chip UHF (o possibile UHF)
#' letto dalla saponetta e non dalle antenne del giro. Se un'antenna fuori lista lo
#' ha letto lo stesso giorno, è passato in un altro giro.
#' Risultato: per ogni bidone `data_lettura`, `RFID`, `matricola_veicolo` (il mezzo
#' abbinato alla sua trasmissione), `tipo_chip` (la descrizione, o `(nessun tipo)`),
#' `categoria`, `servizio` (a database, `(non censito)` se manca) e `letto_altrove`.
#' In ordine: prima i chip con UHF, poi i possibili UHF; prima quelli non letti da
#' nessuna antenna.
#' @param antenne_tutte Letture con le antenne di tutti i mezzi, senza filtri.
#' @inheritParams persi_per_mezzo
#' @noRd
bidoni_uhf_non_letti <- function(classi_chip, saponette_giri, abbinamenti, giri, antenne_tutte) {
  antenne_del_giorno <- antenne_tutte |>
    con_data_lettura() |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(antenna_del_giorno = TRUE)
  mezzi <- giri |>
    dplyr::distinct(.data$matricola_veicolo) |>
    dplyr::mutate(chiave_mezzo = chiave_matricola(.data$matricola_veicolo)) |>
    dplyr::distinct(.data$chiave_mezzo, .keep_all = TRUE)
  della_trasmissione <- saponette_giri |>
    dplyr::distinct(
      .data$data_lettura,
      .data$matricola_saponetta,
      .data$RFID,
      servizio = .data$servizio_transponder
    ) |>
    dplyr::inner_join(
      abbinamenti[c("data_lettura", "matricola_saponetta", "chiave_mezzo")],
      by = c("data_lettura", "matricola_saponetta")
    ) |>
    dplyr::distinct(.data$data_lettura, .data$RFID, .keep_all = TRUE)
  classi_chip |>
    dplyr::filter(
      .data$classe == "Solo saponette",
      .data$categoria %in% c("Chip con UHF", "Possibile UHF")
    ) |>
    dplyr::left_join(della_trasmissione, by = c("data_lettura", "RFID")) |>
    dplyr::left_join(mezzi, by = "chiave_mezzo") |>
    dplyr::left_join(antenne_del_giorno, by = c("data_lettura", "RFID")) |>
    dplyr::transmute(
      .data$data_lettura,
      .data$RFID,
      .data$matricola_veicolo,
      tipo_chip = dplyr::coalesce(.data$descrizione_tipo_supporto_chip, "(nessun tipo)"),
      .data$categoria,
      servizio = dplyr::coalesce(.data$servizio, "(non censito)"),
      letto_altrove = .data$antenna_del_giorno %in% TRUE
    ) |>
    dplyr::arrange(
      .data$categoria,
      .data$letto_altrove,
      .data$data_lettura,
      .data$matricola_veicolo,
      .data$RFID
    )
}
