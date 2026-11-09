# Indicatori delle letture di antenne e saponette, per il report
# `docs/indicatori_letture.qmd`: quante letture ha ogni sistema, quante scarta,
# quante sono utili (hanno una fornitura) e quanto i due sistemi si sovrappongono.
# L'app non usa queste funzioni.
#
# Una «lettura» qui è la coppia RFID e giorno di un sistema: lo stesso codice letto
# da un'antenna e da una saponetta nello stesso giorno è una lettura sola (non
# si svuota due volte un bidone). L'incontro si fa sui codici già normalizzati e
# riparati dalla pulizia della pipeline. Le letture «compatibili» sono quelle
# che la pulizia tiene, anche se il database non le conosce (`PIA`, `PIT` o
# `NP`): sono potenzialmente bidoni dell'azienda. Quelle «associate» sono le
# compatibili con una fornitura (`PIA`); «non associate» le altre. Sono
# «scartate» tutte le righe che la pulizia toglie: codici nulli, codici lunghi che
# non sono bidoni, doppioni dello stesso giorno, righe senza RFID o con l'istante
# illeggibile. I primi due hanno una chiave (RFID e giorno) e si incontrano tra i
# due sistemi come le letture tenute; gli altri sono righe in più di un solo
# sistema e si contano per riga (`righe_extra()`).
# (Il 29/10/2026 le compatibili erano le sole `PIA`, e i doppioni non contavano
# tra le scartate; il proprietario ha precisato la regola il 30/10 e il 4/11.)

#' Motivi di scarto di una lettura con la sua chiave (RFID e giorno)
#'
#' Contesto: il registro della pipeline (`righe_scartate()`) ha righe di due tipi.
#' Un codice nullo o non compatibile è una lettura scartata con la sua chiave, e lo
#' stesso codice letto dai due sistemi nello stesso giorno è una lettura sola.
#' Risultato: i motivi con chiave; gli altri sono `motivi_senza_chiave()`.
#' @noRd
motivi_di_scarto <- function() {
  c("codice_non_compatibile", "codice_nullo")
}

#' Motivi di scarto di una riga che non è una lettura distinta
#'
#' Contesto: un doppione è una riga in più dello stesso bidone nello stesso giorno
#' (la lettura è già tenuta); una riga senza RFID o con l'istante illeggibile non
#' ha una chiave. Sono scartate, ma si contano per riga, per sistema.
#' Risultato: i motivi.
#' @noRd
motivi_senza_chiave <- function() {
  c("doppione", "senza_rfid", "istante_illeggibile")
}

#' Righe scartate senza chiave di ogni sistema
#'
#' Contesto: vedi `motivi_senza_chiave()`. Si sommano alle letture e alle scartate
#' del loro sistema (non si incontrano con l'altro sistema).
#' Risultato: un vettore con nome, `saponette` e `antenne`.
#' @param scartate_antenne,scartate_saponette Registro delle scartate.
#' @noRd
righe_extra <- function(scartate_antenne, scartate_saponette) {
  conta <- function(registro) sum(registro$motivo %in% motivi_senza_chiave())
  c(
    saponette = conta(scartate_saponette),
    antenne = conta(scartate_antenne)
  )
}

#' Colonne che il report usa dei file delle letture
#' @noRd
colonne_letture_indicatori <- function() {
  c(
    "data_ora_lettura",
    "data_lettura",
    "RFID",
    "presente_a_database",
    "id_fornitura",
    "id_utenza",
    "volume",
    "servizio_transponder",
    "servizio_atteso",
    "servizio_giro",
    "matricola_veicolo",
    "matricola_saponetta"
  )
}

#' Letture (o registro delle scartate) in un data frame con i tipi giusti
#'
#' Contesto: i CSV della pipeline si leggono come testo; le funzioni degli
#' indicatori lavorano su istanti, giorni e litri. Un percorso vuoto o un file che
#' non esiste vale «questo sistema non c'è» (per esempio gli anni delle sole
#' saponette): il risultato è una tabella senza righe.
#'
#' Funzionamento: legge il CSV (`leggi_tabella_origine()`), aggiunge come vuote
#' le colonne che mancano e converte `data_ora_lettura` (UTC, senza fuso),
#' `data_lettura` (se manca la ricava dall'istante) e `volume`.
#'
#' Risultato: un tibble con le colonne di `colonne_letture_indicatori()` più, per
#' il registro, `motivo`.
#' @param percorso Percorso del CSV, o un data frame già letto.
#' @noRd
leggi_letture_indicatori <- function(percorso = NULL) {
  grezze <- if (is.data.frame(percorso)) {
    percorso
  } else if (is.null(percorso) || !nzchar(percorso) || !file.exists(percorso)) {
    NULL
  } else {
    leggi_tabella_origine(percorso, na = c("", "NA"))
  }
  colonne <- c(colonne_letture_indicatori(), "motivo")
  if (is.null(grezze)) {
    grezze <- stats::setNames(
      replicate(length(colonne), character(0), simplify = FALSE),
      colonne
    )
  }
  grezze <- dplyr::as_tibble(grezze)
  grezze[setdiff(colonne, names(grezze))] <- NA_character_
  grezze <- dplyr::select(grezze, dplyr::all_of(colonne), dplyr::everything())
  istante <- istante_utc(grezze$data_ora_lettura)
  giorno <- suppressWarnings(as.Date(as.character(grezze$data_lettura)))
  grezze |>
    dplyr::mutate(
      data_ora_lettura = istante,
      data_lettura = dplyr::coalesce(giorno, as.Date(istante)),
      volume = suppressWarnings(as.numeric(.data$volume))
    )
}

#' Istanti da testi `AAAA-MM-GG hh:mm:ss`, con o senza ora, o da istanti già pronti
#' @noRd
istante_utc <- function(x) {
  if (inherits(x, "POSIXct")) {
    return(x)
  }
  lubridate::parse_date_time(
    as.character(x),
    orders = c("Ymd HMS", "Ymd HM", "Ymd"),
    tz = "UTC",
    quiet = TRUE
  )
}

#' Tiene le righe con `data_lettura` nel periodo, estremi inclusi
#'
#' Contesto: il periodo si sceglie a piacere; vuoto vuol dire «senza limite».
#' Risultato: le righe del periodo.
#' @param x Letture o registro, con `data_lettura`.
#' @param dal,al Primo e ultimo giorno, o vuoti.
#' @noRd
nel_periodo <- function(x, dal = NULL, al = NULL) {
  dal <- valori_filtro(dal)
  al <- valori_filtro(al)
  if (!is.null(dal)) {
    x <- dplyr::filter(x, .data$data_lettura >= as.Date(dal))
  }
  if (!is.null(al)) {
    x <- dplyr::filter(x, .data$data_lettura <= as.Date(al))
  }
  x
}

# --- La lista dei giri e la finestra oraria ---------------------------------------

#' I giri su cui fare l'analisi, letti da un CSV
#'
#' Contesto: non tutti i mezzi hanno l'antenna, quindi si analizzano giri di cui si sa
#' che li ha fatti un certo mezzo con una certa saponetta. Il file è lo stesso
#' dei giri di controllo (`giri_di_controllo()`) con tre colonne facoltative in più.
#'
#' Funzionamento: legge `data_giro`, `matricola_veicolo` (la matricola del mezzo
#' con l'antenna), `servizio_giro`, `servizio_atteso` e `matricola_saponetta`, la
#' saponetta che era nel mezzo (vuota: nessuna saponetta in quel giro). Le colonne
#' `ora_inizio` e `ora_fine` (`HH:MM` o `HH:MM:SS`), se ci sono, fissano la finestra oraria
#' del giro. Ogni riga del file è un giro; più giri, mezzi e giorni valgono tutti.
#'
#' Risultato: un tibble con una riga per giro, in più `riga_giro`.
#' @param percorso Percorso del CSV.
#' @noRd
giri_di_analisi <- function(percorso) {
  giri_di_controllo(
    percorso,
    facoltative = c("matricola_saponetta", "ora_inizio", "ora_fine")
  )
}

#' Finestra oraria di ogni giro: orari indicati, letture delle antenne o giornata
#'
#' Contesto: antenne e saponette vanno confrontate nello stesso intervallo di
#' tempo, altrimenti una saponetta usata nel pomeriggio su un altro servizio
#' sembrerebbe un giro in più.
#'
#' Funzionamento: per ogni giro, in quest'ordine:
#' 1. se il file indica `ora_inizio` e `ora_fine`, vale quello intervallo;
#' 2. altrimenti dalla prima all'ultima lettura delle antenne di quel giro, allargata
#'    di `margine_minuti` per parte;
#' 3. se non ci sono neppure letture delle antenne, la giornata intera.
#'
#' Risultato: una riga per giro con `riga_giro`, `data_giro`, `matricola_veicolo`,
#' `servizio_giro`, `matricola_saponetta`, `da`, `a` (istanti) e `finestra`, il
#' modo in cui si è ricavata.
#' @param giri Risultato di `giri_di_analisi()`.
#' @param antenne_giri Letture delle antenne dei giri, con `riga_giro` e
#'   `data_ora_lettura`.
#' @param margine_minuti Minuti da aggiungere ai due estremi delle letture.
#' @noRd
finestre_dei_giri <- function(giri, antenne_giri, margine_minuti = 30) {
  orari <- function(data, ora) {
    lubridate::parse_date_time(
      paste(data, ora),
      c("Ymd HMS", "Ymd HM"),
      tz = "UTC",
      quiet = TRUE
    )
  }
  dalle_antenne <- if (nrow(antenne_giri) == 0) {
    dplyr::tibble(
      riga_giro = integer(0),
      prima = as.POSIXct(character(0), tz = "UTC"),
      ultima = as.POSIXct(character(0), tz = "UTC")
    )
  } else {
    dplyr::summarise(
      antenne_giri,
      prima = min(.data$data_ora_lettura),
      ultima = max(.data$data_ora_lettura),
      .by = "riga_giro"
    )
  }
  giri |>
    dplyr::mutate(riga_giro = dplyr::row_number()) |>
    dplyr::left_join(dalle_antenne, by = "riga_giro") |>
    dplyr::mutate(
      inizio_indicato = orari(.data$data_giro, .data$ora_inizio),
      fine_indicata = orari(.data$data_giro, .data$ora_fine),
      indicata = !is.na(.data$inizio_indicato) & !is.na(.data$fine_indicata),
      con_antenne = !is.na(.data$prima),
      finestra = dplyr::case_when(
        .data$indicata ~ "orari indicati",
        .data$con_antenne ~ "letture delle antenne",
        .default = "giornata intera"
      ),
      da = dplyr::case_when(
        .data$indicata ~ .data$inizio_indicato,
        .data$con_antenne ~ .data$prima - margine_minuti * 60,
        .default = orari(.data$data_giro, "00:00:00")
      ),
      a = dplyr::case_when(
        .data$indicata ~ .data$fine_indicata,
        .data$con_antenne ~ .data$ultima + margine_minuti * 60,
        .default = orari(.data$data_giro, "23:59:59")
      )
    ) |>
    dplyr::select(
      "riga_giro",
      "data_giro",
      "matricola_veicolo",
      "servizio_giro",
      "matricola_saponetta",
      "da",
      "a",
      "finestra"
    )
}

#' Unisce le righe a quelle dei giri e tiene quelle dentro la finestra
#'
#' Contesto: serve a `seleziona_per_giri()`. Una riga di letture entra in un giro se
#' è dello stesso giorno e della stessa matricola e il suo istante cade nella
#' finestra del giro.
#' Risultato: le righe dentro una finestra, con `riga_giro`; una riga che cade
#' in due finestre compare due volte.
#' @param x Letture o registro con `data_lettura`, `data_ora_lettura` e la
#'   colonna della matricola.
#' @param finestre Risultato di `finestre_dei_giri()`.
#' @param colonna Colonna della matricola di `x`: `matricola_veicolo` o
#'   `matricola_saponetta`.
#' @noRd
righe_nelle_finestre <- function(x, finestre, colonna) {
  chiave <- if (colonna == "matricola_veicolo") chiave_matricola else chiave_servizio_giro
  finestre <- finestre |>
    dplyr::transmute(
      .data$riga_giro,
      data_lettura = .data$data_giro,
      chiave_matricola = chiave(.data[[colonna]]),
      .data$da,
      .data$a
    ) |>
    dplyr::filter(!is.na(.data$chiave_matricola))
  x |>
    dplyr::mutate(chiave_matricola = chiave(.data[[colonna]])) |>
    dplyr::inner_join(
      finestre,
      by = c("data_lettura", "chiave_matricola"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter((.data$data_ora_lettura >= .data$da & .data$data_ora_lettura <= .data$a) %in% TRUE) |>
    dplyr::select(-"chiave_matricola", -"da", -"a")
}

#' Le letture dei giri indicati: antenne e saponette nella stessa finestra oraria
#'
#' Contesto: il filtro automatico dell'analisi sui giri. Per ogni giro (giorno,
#' servizio, mezzo con antenna, saponetta che era nel mezzo) tiene le letture dei
#' due sistemi che stanno insieme.
#'
#' Funzionamento:
#' 1. antenne: le letture del mezzo in quel giorno con il servizio del giro, con gli
#'    stessi due criteri del controllo dei giri (`letture_dei_mezzi_della_lista()`:
#'    il giro del calendario oppure il servizio dedotto dai censiti);
#' 2. la finestra oraria del giro (`finestre_dei_giri()`);
#' 3. saponette: le letture di quella `matricola_saponetta`, in quel giorno, dentro
#'    la finestra;
#' 4. le letture scartate dei due sistemi (registro della pipeline) dello stesso
#'    mezzo o saponetta, dentro la finestra: il servizio non c'è, conta l'orario.
#' Una lettura può stare in più giri della lista (due giri dello stesso mezzo nello
#' stesso giorno): ogni riga porta `riga_giro`, e i conteggi non la contano due volte.
#'
#' Risultato: una lista con `antenne`, `saponette`, `scartate_antenne`,
#' `scartate_saponette` (ognuna con `riga_giro`) e `finestre`.
#' @param letture Lista con `antenne`, `saponette`, `scartate_antenne`,
#'   `scartate_saponette`, come da `leggi_letture_indicatori()`.
#' @param giri Risultato di `giri_di_analisi()`.
#' @param margine_minuti Minuti aggiunti alla finestra ricavata dalle antenne.
#' @noRd
seleziona_per_giri <- function(letture, giri, margine_minuti = 30) {
  antenne_giri <- letture$antenne |>
    letture_dei_mezzi_della_lista(giri) |>
    dplyr::filter(!is.na(.data$criterio)) |>
    dplyr::select(
      -dplyr::any_of(c("matricola_lista", "chiave_mezzo"))
    ) |>
    dplyr::rename(riga_giro = "riga_lista")
  finestre <- finestre_dei_giri(giri, antenne_giri, margine_minuti)
  # Un numero per ogni riga del registro: una riga che cade in due finestre non si
  # conta due volte nel totale.
  letture$scartate_antenne <- dplyr::mutate(letture$scartate_antenne, riga_registro = dplyr::row_number())
  letture$scartate_saponette <- dplyr::mutate(letture$scartate_saponette, riga_registro = dplyr::row_number())
  list(
    antenne = antenne_giri,
    saponette = righe_nelle_finestre(letture$saponette, finestre, "matricola_saponetta"),
    scartate_antenne = righe_nelle_finestre(
      letture$scartate_antenne,
      finestre,
      "matricola_veicolo"
    ),
    scartate_saponette = righe_nelle_finestre(
      letture$scartate_saponette,
      finestre,
      "matricola_saponetta"
    ),
    finestre = finestre
  )
}

# --- Le unità di lettura e gli indicatori ------------------------------------------

#' Stati di una lettura, dal migliore al peggiore
#' @noRd
stati_lettura <- function() c("PIA", "PIT", "NP", "Scartata")

#' Sistemi di una lettura: solo saponette, solo antenne o entrambe
#' @noRd
sistemi_lettura <- function() c("Solo saponette", "Solo antenne", "Entrambe")

#' Letture di un sistema: una per RFID e giorno, con il loro stato
#'
#' Contesto: tappa di `unita_di_lettura()`.
#' Funzionamento: unisce le righe dei file finali (stato `PIA`, `PIT` o `NP`) e
#' quelle del registro con un motivo di scarto (`motivi_di_scarto()`, stato
#' `Scartata`); ne tiene una per RFID e giorno, la migliore. Le righe senza RFID o
#' senza giorno non hanno una chiave e non contano.
#' Risultato: `RFID`, `data_lettura`, `stato` (un fattore) e i dati della
#' fornitura per le letture `PIA`.
#' @noRd
letture_di_un_sistema <- function(finali, scartate) {
  tenute <- finali |>
    dplyr::transmute(
      .data$RFID,
      .data$data_lettura,
      # I valori di prima del 28/10/2026 valgono come `PIA` e `NP`.
      stato = dplyr::case_when(
        .data$presente_a_database %in% "Presente" ~ "PIA",
        .data$presente_a_database %in% "Non Presente" ~ "NP",
        .default = as.character(.data$presente_a_database)
      ),
      .data$id_fornitura,
      .data$volume,
      .data$servizio_transponder
    )
  perse <- scartate |>
    dplyr::filter(.data$motivo %in% motivi_di_scarto()) |>
    dplyr::transmute(.data$RFID, .data$data_lettura, stato = "Scartata")
  dplyr::bind_rows(tenute, perse) |>
    dplyr::filter(!is.na(.data$RFID), !is.na(.data$data_lettura)) |>
    dplyr::mutate(stato = factor(.data$stato, stati_lettura())) |>
    dplyr::arrange(.data$stato) |>
    dplyr::distinct(.data$RFID, .data$data_lettura, .keep_all = TRUE)
}

#' Le letture dei due sistemi, ognuna una volta: solo saponette, solo antenne o entrambe
#'
#' Contesto: è la base di tutti gli indicatori. Un bidone letto dalla saponetta e
#' dall'antenna nello stesso giorno è stato svuotato una volta, quindi vale una
#' lettura sola, che si chiama «Entrambe».
#'
#' Funzionamento: unisce le letture dei due sistemi per RFID e giorno. Lo stato
#' è il migliore dei due (`stati_lettura()`: `PIA`, `PIT`, `NP`, `Scartata`), e i
#' dati della fornitura vengono dal sistema che li ha.
#'
#' Risultato: una riga per lettura con `RFID`, `data_lettura`, `sistema`,
#' `stato`, `id_fornitura`, `volume` e `servizio_transponder`. Il filtro sul secco
#' si fa prima, sulle letture di ogni sistema (`letture_associate_del_secco()`), solo per le sezioni sul secco del report.
#' @param antenne,saponette Letture dei file finali dei due sistemi.
#' @param scartate_antenne,scartate_saponette Registro delle scartate.
#' @noRd
unita_di_lettura <- function(antenne, saponette, scartate_antenne, scartate_saponette) {
  a <- letture_di_un_sistema(antenne, scartate_antenne)
  s <- letture_di_un_sistema(saponette, scartate_saponette)
  dplyr::full_join(
    s,
    a,
    by = c("RFID", "data_lettura"),
    suffix = c("_s", "_a")
  ) |>
    dplyr::mutate(
      sistema = factor(
        dplyr::case_when(
          !is.na(.data$stato_s) & !is.na(.data$stato_a) ~ "Entrambe",
          !is.na(.data$stato_s) ~ "Solo saponette",
          .default = "Solo antenne"
        ),
        sistemi_lettura()
      ),
      stato = factor(
        stati_lettura()[pmin(
          as.integer(.data$stato_s),
          as.integer(.data$stato_a),
          na.rm = TRUE
        )],
        stati_lettura()
      ),
      id_fornitura = dplyr::coalesce(.data$id_fornitura_a, .data$id_fornitura_s),
      volume = dplyr::coalesce(.data$volume_a, .data$volume_s),
      servizio_transponder = dplyr::coalesce(
        .data$servizio_transponder_a,
        .data$servizio_transponder_s
      )
    ) |>
    dplyr::select(
      "RFID",
      "data_lettura",
      "sistema",
      "stato",
      "id_fornitura",
      "volume",
      "servizio_transponder"
    )
}

#' Rapporto che dà mancante se il denominatore è zero
#' @noRd
rapporto <- function(numeratore, denominatore) {
  numeratore / ifelse(denominatore > 0, denominatore, NA_real_)
}

#' Indicatori sulle letture dei due sistemi
#'
#' Contesto: gli indicatori base e i KPI del documento del proprietario. Le
#' letture sono divise in tre insiemi disgiunti: solo saponette, solo antenne e in
#' comune. **Compatibili** sono quelle che la pulizia tiene (`PIA`, `PIT`, `NP`:
#' potenziali bidoni dell'azienda), **associate** le compatibili con una fornitura
#' (`PIA`), **non associate** le altre compatibili; **scartate** quelle che la
#' pulizia toglie.
#'
#' Funzionamento: conta le letture di `unita_di_lettura()` per sistema e stato.
#' Le righe scartate senza chiave (`righe_extra()`: doppioni, righe senza RFID o
#' senza istante) si sommano alle letture e alle scartate del loro sistema.
#' - `letture_*`, `compatibili_*`, `associate_*`, `non_associate_*` e `scartate_*`,
#'   ognuna per solo saponette, solo antenne, in comune e in totale;
#' - indice di scarto (quanto filtra via un sistema): `(scartate solo del
#'   sistema + scartate in comune) / letture totali`, per le saponette, le antenne e
#'   in totale;
#' - indice di associazione (quanta parte delle compatibili è associata e vista
#'   da un sistema): `(associate solo del sistema + associate in comune) /
#'   compatibili totali`; `associazione_in_comune` è la parte vista da tutti e due;
#' - indice di efficacia (quanta parte delle compatibili vede un sistema):
#'   `(compatibili solo del sistema + in comune) / compatibili totali`;
#'   `efficacia_in_comune` è la quota vista da tutti e due.
#' Un indice con il denominatore a zero è `NA`.
#'
#' Risultato: un tibble di una riga.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param extra Risultato di `righe_extra()`; senza, nessuna.
#' @noRd
indicatori_letture <- function(unita, extra = c(saponette = 0, antenne = 0)) {
  conta <- function(sistema, stati = NULL) {
    sum(
      unita$sistema == sistema &
        (is.null(stati) | unita$stato %in% stati)
    )
  }
  sistemi <- sistemi_lettura()
  compatibili <- vapply(sistemi, conta, numeric(1), stati = c("PIA", "PIT", "NP"))
  letture <- vapply(sistemi, conta, numeric(1)) +
    c(extra[["saponette"]], extra[["antenne"]], 0)
  associate <- vapply(sistemi, conta, numeric(1), stati = "PIA")
  scartate <- letture - compatibili
  totali <- sum(letture)
  compatibili_totali <- sum(compatibili)
  dplyr::tibble(
    letture_solo_saponette = letture[[1]],
    letture_solo_antenne = letture[[2]],
    letture_in_comune = letture[[3]],
    letture_totali = totali,
    compatibili_solo_saponette = compatibili[[1]],
    compatibili_solo_antenne = compatibili[[2]],
    compatibili_in_comune = compatibili[[3]],
    compatibili_totali = compatibili_totali,
    associate_solo_saponette = associate[[1]],
    associate_solo_antenne = associate[[2]],
    associate_in_comune = associate[[3]],
    associate_totali = sum(associate),
    non_associate_solo_saponette = compatibili[[1]] - associate[[1]],
    non_associate_solo_antenne = compatibili[[2]] - associate[[2]],
    non_associate_in_comune = compatibili[[3]] - associate[[3]],
    non_associate_totali = compatibili_totali - sum(associate),
    scartate_solo_saponette = scartate[[1]],
    scartate_solo_antenne = scartate[[2]],
    scartate_in_comune = scartate[[3]],
    scartate_totali = sum(scartate),
    indice_scarto_saponette = rapporto(scartate[[1]] + scartate[[3]], totali),
    indice_scarto_antenne = rapporto(scartate[[2]] + scartate[[3]], totali),
    indice_scarto_totale = rapporto(sum(scartate), totali),
    associazione_in_comune = rapporto(associate[[3]], compatibili_totali),
    associazione_saponette = rapporto(associate[[1]] + associate[[3]], compatibili_totali),
    associazione_antenne = rapporto(associate[[2]] + associate[[3]], compatibili_totali),
    efficacia_in_comune = rapporto(compatibili[[3]], compatibili_totali),
    efficacia_saponette = rapporto(compatibili[[1]] + compatibili[[3]], compatibili_totali),
    efficacia_antenne = rapporto(compatibili[[2]] + compatibili[[3]], compatibili_totali)
  )
}

#' Come si dividono le letture di ogni sistema tra `PIA`, `PIT`, `NP` e scartate
#'
#' Contesto: dice da che cosa dipende lo scarto di un sistema. `PIA` è il bidone
#' che si può fatturare; `PIT` è un contenitore dell'azienda senza fornitura; `NP` è
#' un codice valido che il database non conosce; `Scartata` è un codice
#' scartato dalla pulizia (codice nullo o lungo e non di un bidone, doppione,
#' riga senza RFID o senza istante).
#' Funzionamento: ogni sistema comprende le letture sue e quelle in comune; le
#' righe scartate senza chiave di `righe_extra()` sono `Scartata` del loro sistema.
#' Risultato: per sistema e stato, il numero di letture e la quota sul sistema.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param extra Risultato di `righe_extra()`; senza, nessuna.
#' @noRd
composizione_letture <- function(unita, extra = c(saponette = 0, antenne = 0)) {
  sistemi <- list(
    Saponette = c("Solo saponette", "Entrambe"),
    Antenne = c("Solo antenne", "Entrambe")
  )
  purrr::imap(sistemi, function(insieme, nome) {
    unita |>
      dplyr::filter(.data$sistema %in% insieme) |>
      dplyr::count(.data$stato, name = "letture", .drop = FALSE) |>
      dplyr::mutate(
        letture = .data$letture +
          ifelse(.data$stato == "Scartata", extra[[tolower(nome)]], 0),
        sistema = nome,
        quota = rapporto(.data$letture, sum(.data$letture))
      )
  }) |>
    dplyr::bind_rows() |>
    dplyr::select("sistema", "stato", "letture", "quota")
}

#' Le letture scartate di ogni sistema, per motivo
#'
#' Contesto: dice da che cosa dipende lo scarto di un sistema. Ogni motivo è un
#' passo della pulizia dei codici (`motivi_di_scarto()` e `motivi_senza_chiave()`).
#' Funzionamento: per i codici nulli e non compatibili conta le letture (RFID e
#' giorno), come gli indicatori; per i doppioni e le righe senza RFID o senza
#' istante conta le righe.
#' Risultato: per sistema e motivo, il numero di letture o di righe.
#' @param scartate_antenne,scartate_saponette Registro delle scartate.
#' @noRd
scartate_per_motivo <- function(scartate_antenne, scartate_saponette) {
  conta <- function(registro, sistema) {
    con_chiave <- registro |>
      dplyr::filter(.data$motivo %in% motivi_di_scarto()) |>
      dplyr::distinct(.data$RFID, .data$data_lettura, .data$motivo) |>
      dplyr::count(.data$motivo, name = "letture")
    senza_chiave <- registro |>
      dplyr::filter(.data$motivo %in% motivi_senza_chiave()) |>
      dplyr::count(.data$motivo, name = "letture")
    dplyr::bind_rows(con_chiave, senza_chiave) |>
      dplyr::mutate(sistema = sistema)
  }
  dplyr::bind_rows(
    conta(scartate_saponette, "Saponette"),
    conta(scartate_antenne, "Antenne")
  ) |>
    dplyr::select("sistema", "motivo", "letture")
}

#' Indicatori di ogni giro e del totale
#'
#' Contesto: la tabella per giro più il totale del report. Le righe in più finestre
#' non si contano due volte nel totale.
#' Funzionamento: per ogni `riga_giro` della selezione (`seleziona_per_giri()`)
#' calcola `indicatori_letture()`; il totale usa tutte le letture dei giri,
#' ognuna una volta.
#' Risultato: un tibble con una riga per giro (dati del giro, finestra e
#' indicatori) e l'ultima riga `Totale`.
#' @param selezione Risultato di `seleziona_per_giri()`.
#' @noRd
indicatori_per_giro <- function(selezione) {
  per_giro <- function(riga) {
    scartate_antenne <- dplyr::filter(selezione$scartate_antenne, .data$riga_giro == riga)
    scartate_saponette <- dplyr::filter(selezione$scartate_saponette, .data$riga_giro == riga)
    unita_di_lettura(
      dplyr::filter(selezione$antenne, .data$riga_giro == riga),
      dplyr::filter(selezione$saponette, .data$riga_giro == riga),
      scartate_antenne,
      scartate_saponette
    ) |>
      indicatori_letture(righe_extra(scartate_antenne, scartate_saponette))
  }
  # Nel totale una riga del registro che sta in più finestre conta una volta.
  una_per_riga <- function(registro) {
    dplyr::distinct(registro, .data$riga_registro, .keep_all = TRUE)
  }
  finestre <- selezione$finestre
  giri <- finestre |>
    dplyr::mutate(giro = as.character(.data$riga_giro)) |>
    dplyr::bind_cols(dplyr::bind_rows(lapply(finestre$riga_giro, per_giro)))
  totale <- unita_di_lettura(
    selezione$antenne,
    selezione$saponette,
    selezione$scartate_antenne,
    selezione$scartate_saponette
  ) |>
    indicatori_letture(righe_extra(
      una_per_riga(selezione$scartate_antenne),
      una_per_riga(selezione$scartate_saponette)
    )) |>
    dplyr::mutate(giro = "Totale")
  dplyr::bind_rows(giri, totale)
}
