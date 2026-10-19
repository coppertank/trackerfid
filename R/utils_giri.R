# Giri dei mezzi: che cosa ha letto un mezzo in un giorno, e dove.
#
# Un giro è il lavoro di un mezzo in un giorno, per un servizio e in un
# comune: lo dice il calendario dei mezzi, e nel file sta su ogni lettura come
# `servizio_giro` e `comune_giro`. Le funzioni di questo file servono
# alla scheda "Ricerca Giro": riassumono giro per giro le letture trovate da
# `cerca_giri()`, con le letture fuori giro e quelle fuori comune, vedi
# `coerenza_comune()`. Le ricerche nell'archivio stanno in
# `R/utils_selezione.R`.

#' Servizio del giro di ogni lettura, secondo il calendario
#'
#' È `servizio_giro`. Il servizio atteso di una lettura può essere
#' diverso: per un non censito può venire dai censiti letti dallo stesso
#' mezzo. Senza quella colonna vale `servizio_atteso`.
#' @noRd
servizio_del_giro <- function(letture) {
  as.character(
    letture[["servizio_giro"]] %||%
      letture[["servizio_atteso"]] %||%
      rep(NA_character_, nrow(letture))
  )
}

#' Colonne del riepilogo dei giri
#' @noRd
colonne_riepilogo_giri <- function() {
  c(
    "giorno",
    "matricola_veicolo",
    "targa_veicolo",
    "servizio_giro",
    "comune_giro",
    "eseguito",
    "prima_lettura",
    "ultima_lettura",
    "letture",
    "bidoni",
    "fuori_giro",
    "fuori_comune",
    "letto_in"
  )
}

#' Riepilogo dei giri: una riga per giorno, mezzo, servizio e comune del giro
#'
#' Di ogni giro dice che cosa il mezzo doveva fare, cioè servizio atteso e
#' comune del calendario, se il giro è stato confermato, e che cosa ha fatto:
#' quante letture, di quanti bidoni, quante fuori giro e quante fuori comune,
#' vedi `coerenza_comune()`, e in quali comuni sono state fatte le letture.
#' La conferma, `eseguito`, è `NO` se una lettura del giro la ha a `NO`,
#' altrimenti `SI` se una la ha a `SI`, altrimenti manca: vedi
#' `colonna_conferma()`.
#'
#' @param letture Letture trovate da `cerca_giri()`.
#' @return Tibble con le colonne di `colonne_riepilogo_giri()`, in ordine di
#'   giorno, mezzo e prima lettura. `letto_in` riporta il comune di lettura
#'   della gran parte delle letture e quanti altri comuni compaiono.
#' @noRd
riepilogo_giri <- function(letture) {
  if (is.null(letture) || nrow(letture) == 0) {
    return(dplyr::tibble(
      giorno = as.Date(character(0)),
      matricola_veicolo = character(0),
      targa_veicolo = character(0),
      servizio_giro = character(0),
      comune_giro = character(0),
      eseguito = character(0),
      prima_lettura = as.POSIXct(character(0), tz = "UTC"),
      ultima_lettura = as.POSIXct(character(0), tz = "UTC"),
      letture = integer(0),
      bidoni = integer(0),
      fuori_giro = integer(0),
      fuori_comune = integer(0),
      letto_in = character(0)
    ))
  }
  letture <- letture[order(letture$giorno_lettura, method = "radix"), ]
  giorno <- letture[["giorno"]] %||% lubridate::as_date(letture$giorno_lettura)
  comune_giro <- as.character(
    letture[["comune_giro"]] %||% rep(NA_character_, nrow(letture))
  )
  servizio_giro <- servizio_del_giro(letture)
  gruppo <- indice_gruppi(paste(
    as.integer(giorno),
    letture$matricola_veicolo,
    servizio_giro,
    comune_giro,
    sep = "\r"
  ))
  n <- max(gruppo)
  incongruenze <- incongruenze_comune(letture)
  letto <- as.character(
    letture[["comune_lettura"]] %||% rep(NA_character_, nrow(letture))
  )
  comune_prevalente <- piu_frequente_per_gruppo(letto, gruppo, n)
  altri_comuni <- n_distinti_per_gruppo(letto, gruppo, n) - 1L
  letto_in <- dplyr::case_when(
    is.na(comune_prevalente) ~ "comune sconosciuto",
    altri_comuni == 1 ~ paste(comune_prevalente, "e un altro comune"),
    altri_comuni > 1 ~ paste(comune_prevalente, "e altri", altri_comuni, "comuni"),
    .default = comune_prevalente
  )
  bidoni <- n_distinti_per_gruppo(letture$RFID, gruppo, n)
  conferma <- toupper(as.character(
    letture[[colonna_conferma()]] %||% rep(NA_character_, nrow(letture))
  ))
  eseguito <- dplyr::case_when(
    tabulate(gruppo[conferma %in% "NO"], n) > 0 ~ "NO",
    tabulate(gruppo[conferma %in% "SI"], n) > 0 ~ "SI",
    .default = NA_character_
  )
  riepilogo <- dplyr::tibble(
    giorno = primo_per_gruppo(giorno, gruppo, n),
    matricola_veicolo = primo_per_gruppo(letture$matricola_veicolo, gruppo, n),
    targa_veicolo = primo_per_gruppo(letture$targa_veicolo, gruppo, n),
    servizio_giro = primo_per_gruppo(servizio_giro, gruppo, n),
    comune_giro = primo_per_gruppo(comune_giro, gruppo, n),
    eseguito = eseguito,
    prima_lettura = primo_per_gruppo(letture$giorno_lettura, gruppo, n),
    ultima_lettura = ultimo_per_gruppo(letture$giorno_lettura, gruppo, n),
    # Calcolato qui sopra: dentro `tibble()` la colonna `letture` nasconderebbe
    # le letture.
    bidoni = bidoni,
    letture = tabulate(gruppo, n),
    fuori_giro = as.integer(
      somma_per_gruppo(incongruenze$fuori_giro, gruppo, n)
    ),
    fuori_comune = as.integer(
      somma_per_gruppo(incongruenze$fuori_comune, gruppo, n)
    ),
    letto_in = letto_in
  )
  riepilogo <- riepilogo[, colonne_riepilogo_giri()]
  riepilogo[
    order(
      riepilogo$giorno,
      riepilogo$matricola_veicolo,
      riepilogo$prima_lettura,
      method = "radix"
    ),
  ]
}

#' Letture di un giro del riepilogo
#'
#' @param letture Letture trovate da `cerca_giri()`.
#' @param giro Una riga di `riepilogo_giri()`.
#' @return Le letture di quel giorno, mezzo, servizio e comune del giro.
#' @noRd
letture_del_giro <- function(letture, giro) {
  uguale <- function(valori, valore) {
    if (is.na(valore)) is.na(valori) else valori %in% valore
  }
  comune_giro <- as.character(
    letture[["comune_giro"]] %||% rep(NA_character_, nrow(letture))
  )
  letture[
    letture$giorno == giro$giorno &
      letture$matricola_veicolo == giro$matricola_veicolo &
      uguale(servizio_del_giro(letture), giro$servizio_giro) &
      uguale(comune_giro, giro$comune_giro),
    ,
    drop = FALSE
  ]
}

#' Area da inquadrare nella ricerca dei giri
#'
#' Le letture trovate e, se i giri lavorano in pochi comuni, anche quei
#' comuni: così si vedono insieme le letture e il confine del comune in cui
#' il mezzo doveva lavorare, anche quando sono lontani.
#'
#' @param letture Letture trovate.
#' @param massimo Numero massimo di comuni dei giri: oltre, valgono le sole
#'   letture, come per `aggiungi_comuni_giro()`.
#' @return Punti da inquadrare, con `latitudine` e `longitudine`; `NULL`
#'   senza letture.
#' @noRd
area_giri <- function(letture, massimo = 6) {
  if (is.null(letture) || nrow(letture) == 0) {
    return(NULL)
  }
  punti <- letture[, c("latitudine", "longitudine")]
  indici <- unique(stats::na.omit(indice_comune(unique(letture[["comune_giro"]]))))
  if (length(indici) == 0 || length(indici) > massimo) {
    return(punti)
  }
  riquadri <- Filter(Negate(is.null), lati_comuni()[indici])
  dplyr::bind_rows(
    punti,
    dplyr::tibble(
      latitudine = unlist(lapply(riquadri, function(r) c(r$y_min, r$y_max))),
      longitudine = unlist(lapply(riquadri, function(r) c(r$x_min, r$x_max)))
    )
  )
}

#' Tiene le letture da disegnare nella ricerca dei giri quando sono troppe
#'
#' Restano per prime le letture fuori giro o fuori comune, poi le più recenti.
#'
#' @param df Letture trovate.
#' @param massimo Numero massimo di letture da disegnare.
#' @return Le letture da disegnare, in ordine di tempo. L'attributo `totale`
#'   riporta quante erano.
#' @noRd
limita_letture_giro <- function(df, massimo = limiti_mappa()$letture_ricerca) {
  if (is.null(df)) {
    return(NULL)
  }
  totale <- nrow(df)
  if (totale > massimo) {
    incongruenze <- incongruenze_comune(df)
    ordine <- order(
      !(incongruenze$fuori_giro | incongruenze$fuori_comune),
      -as.numeric(df$giorno_lettura),
      method = "radix"
    )
    df <- df[sort(utils::head(ordine, massimo)), , drop = FALSE]
  }
  structure(df, totale = totale)
}

#' Frase che riassume le letture trovate nella ricerca dei giri
#'
#' @param letture Letture trovate.
#' @return Testo per l'intestazione della mappa, vuoto senza letture.
#' @noRd
descrizione_giri <- function(letture) {
  if (is.null(letture) || nrow(letture) == 0) {
    return("")
  }
  incongruenze <- incongruenze_comune(letture)
  paste0(
    conta(nrow(letture), "lettura", "letture"),
    " di ",
    conta(dplyr::n_distinct(letture$RFID), "bidone", "bidoni"),
    " \u00b7 ",
    formatta_numero(sum(incongruenze$fuori_giro)),
    " fuori giro \u00b7 ",
    formatta_numero(sum(incongruenze$fuori_comune)),
    " fuori comune"
  )
}

#' Tabella interattiva dei giri
#'
#' Una riga per giro, in ordine di letture fuori giro: i giri da guardare
#' stanno in cima. Un click su una riga la sceglie, e la mappa mostra le
#' letture di quel giro; un secondo click toglie la scelta. Ogni colonna ha un
#' filtro: i testi cercano una parte del valore, senza badare alle maiuscole;
#' la data si cerca come si vede, `24/09`, o come `2025-09`; i numeri
#' accettano un valore o un confronto, `>0`, `<5`, `>=2`, vedi
#' `filtro_numerico_js()`. Le righe restano
#' nell'ordine di `riepilogo`: il numero della riga scelta, che reactable
#' comunica al server, è la riga del riepilogo anche dopo un ordinamento.
#'
#' @param riepilogo Risultato di `riepilogo_giri()`.
#' @return Widget reactable.
#' @noRd
tabella_giri <- function(riepilogo) {
  ora <- function(x) format(x, "%H:%M", tz = "UTC")
  tabella <- data.frame(
    Data = riepilogo$giorno,
    Mezzo = paste(riepilogo$matricola_veicolo, riepilogo$targa_veicolo),
    `Giro previsto` = riepilogo$servizio_giro,
    `Comune del giro` = riepilogo$comune_giro,
    # Un giro senza l'informazione ha un trattino: si filtra come gli altri.
    Confermato = dplyr::coalesce(riepilogo$eseguito, "\u2013"),
    Orario = paste(ora(riepilogo$prima_lettura), ora(riepilogo$ultima_lettura), sep = "\u2013"),
    Letture = riepilogo$letture,
    Bidoni = riepilogo$bidoni,
    `Fuori giro` = riepilogo$fuori_giro,
    `Fuori comune` = riepilogo$fuori_comune,
    `Letto a` = riepilogo$letto_in,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  # Le incongruenze in rosso. La data arriva al browser come AAAA-MM-GG, che
  # si ordina come data, e si mostra come GG/MM/AAAA.
  incongruenze <- function(valore) {
    if (isTRUE(valore > 0)) list(color = "#b03a2e", fontWeight = 700)
  }
  # Un giro non ancora confermato in arancio: non e' un errore, ma va
  # guardato se resta cosi' a lungo.
  non_confermato <- function(valore) {
    if (identical(valore, "NO")) list(color = "#b35c00", fontWeight = 700)
  }
  data_italiana <- reactable::JS(
    "function(cell) { var p = String(cell.value).split('-'); return p.length === 3 ? p[2] + '/' + p[1] + '/' + p[0] : cell.value; }"
  )
  # La data si filtra come si vede, GG/MM/AAAA, o come arriva, AAAA-MM-GG.
  filtro_data <- reactable::JS(paste(
    "function(rows, columnId, filterValue) {",
    "  var cerca = String(filterValue).trim();",
    "  return rows.filter(function(row) {",
    "    var v = String(row.values[columnId]);",
    "    var p = v.split('-');",
    "    var it = p.length === 3 ? p[2] + '/' + p[1] + '/' + p[0] : v;",
    "    return it.indexOf(cerca) !== -1 || v.indexOf(cerca) !== -1;",
    "  });",
    "}",
    sep = "\n"
  ))
  numero <- function(...) {
    reactable::colDef(..., filterMethod = filtro_numerico_js())
  }
  reactable::reactable(
    tabella,
    columns = list(
      .selection = reactable::colDef(show = FALSE),
      Data = reactable::colDef(cell = data_italiana, filterMethod = filtro_data, minWidth = 85),
      Mezzo = reactable::colDef(minWidth = 115),
      `Giro previsto` = reactable::colDef(minWidth = 120),
      `Comune del giro` = reactable::colDef(minWidth = 110),
      Confermato = reactable::colDef(style = non_confermato, align = "center", minWidth = 85),
      Orario = reactable::colDef(minWidth = 85),
      Letture = numero(minWidth = 60),
      Bidoni = numero(minWidth = 60),
      `Fuori giro` = numero(style = incongruenze, minWidth = 70),
      `Fuori comune` = numero(style = incongruenze, minWidth = 80),
      `Letto a` = reactable::colDef(minWidth = 150)
    ),
    defaultSorted = list(`Fuori giro` = "desc", Data = "desc"),
    selection = "single",
    onClick = "select",
    filterable = TRUE,
    defaultPageSize = 5,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(5, 10, 25, 50),
    wrap = FALSE,
    compact = TRUE,
    highlight = TRUE,
    rowStyle = list(cursor = "pointer"),
    theme = tema_tabelle_mappa(),
    language = lingua_reactable("Nessun giro nei giorni scelti")
  )
}

#' Filtro di reactable per le colonne di numeri
#'
#' Accetta un valore, `3`, oppure un confronto: `>0`, `<5`, `>=2`, `<=10`,
#' `=4`. Un testo che non è nessuno dei due non filtra niente.
#' @noRd
filtro_numerico_js <- function() {
  reactable::JS(paste(
    "function(rows, columnId, filterValue) {",
    "  var m = String(filterValue).replace(/\\s/g, '').match(/^(>=|<=|>|<|=)?(\\d+(?:[.,]\\d+)?)$/);",
    "  if (!m) return rows;",
    "  var op = m[1] || '=';",
    "  var n = parseFloat(m[2].replace(',', '.'));",
    "  return rows.filter(function(row) {",
    "    var v = row.values[columnId];",
    "    if (op === '>') return v > n;",
    "    if (op === '<') return v < n;",
    "    if (op === '>=') return v >= n;",
    "    if (op === '<=') return v <= n;",
    "    return v === n;",
    "  });",
    "}",
    sep = "\n"
  ))
}
