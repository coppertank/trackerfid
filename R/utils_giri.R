# Scheda «Ricerca Giro»: riepilogo giro per giro delle letture di
# `cerca_giri()`, tabella e mappa. Un giro è il lavoro di un mezzo in un
# giorno per un servizio e un comune (`servizio_giro`, `comune_giro`).

#' Giro del calendario di ogni lettura: `servizio_giro` o, se manca, `servizio_atteso`
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
    "data_giro",
    "matricola_veicolo",
    "targa_veicolo",
    "servizio_giro",
    "comune_giro",
    "eseguito",
    "data_ora_prima_lettura",
    "data_ora_ultima_lettura",
    "letture",
    "bidoni",
    "fuori_giro",
    "fuori_comune",
    "letto_in"
  )
}

#' Riepilogo dei giri: una riga per giorno, mezzo, servizio e comune del giro
#'
#' Letture, bidoni, fuori giro, fuori comune, comuni di lettura e conferma:
#' `NO` se una lettura ha `NO`, poi `SI`, altrimenti mancante.
#' @return Colonne di `colonne_riepilogo_giri()`, per giorno, mezzo e prima
#'   lettura.
#' @noRd
riepilogo_giri <- function(letture) {
  if (is.null(letture) || nrow(letture) == 0) {
    return(dplyr::tibble(
      data_giro = as.Date(character(0)),
      matricola_veicolo = character(0),
      targa_veicolo = character(0),
      servizio_giro = character(0),
      comune_giro = character(0),
      eseguito = character(0),
      data_ora_prima_lettura = as.POSIXct(character(0), tz = "UTC"),
      data_ora_ultima_lettura = as.POSIXct(character(0), tz = "UTC"),
      letture = integer(0),
      bidoni = integer(0),
      fuori_giro = integer(0),
      fuori_comune = integer(0),
      letto_in = character(0)
    ))
  }
  letture <- letture[order(letture$data_ora_lettura, method = "radix"), ]
  data_giro <- letture[["data_lettura"]] %||%
    lubridate::as_date(letture$data_ora_lettura)
  comune_giro <- as.character(
    letture[["comune_giro"]] %||% rep(NA_character_, nrow(letture))
  )
  servizio_giro <- servizio_del_giro(letture)
  gruppo <- indice_gruppi(paste(
    as.integer(data_giro),
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
    altri_comuni > 1 ~ paste(
      comune_prevalente,
      "e altri",
      altri_comuni,
      "comuni"
    ),
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
    data_giro = primo_per_gruppo(data_giro, gruppo, n),
    matricola_veicolo = primo_per_gruppo(letture$matricola_veicolo, gruppo, n),
    targa_veicolo = primo_per_gruppo(letture$targa_veicolo, gruppo, n),
    servizio_giro = primo_per_gruppo(servizio_giro, gruppo, n),
    comune_giro = primo_per_gruppo(comune_giro, gruppo, n),
    eseguito = eseguito,
    data_ora_prima_lettura = primo_per_gruppo(letture$data_ora_lettura, gruppo, n),
    data_ora_ultima_lettura = ultimo_per_gruppo(letture$data_ora_lettura, gruppo, n),
    # Calcolato prima: dentro `tibble()` la colonna `letture` nasconderebbe il
    # data frame.
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
      riepilogo$data_giro,
      riepilogo$matricola_veicolo,
      riepilogo$data_ora_prima_lettura,
      method = "radix"
    ),
  ]
}

#' Letture di una riga di `riepilogo_giri()`
#' @noRd
letture_del_giro <- function(letture, giro) {
  uguale <- function(valori, valore) {
    if (is.na(valore)) is.na(valori) else valori %in% valore
  }
  comune_giro <- as.character(
    letture[["comune_giro"]] %||% rep(NA_character_, nrow(letture))
  )
  letture[
    letture$data_lettura == giro$data_giro &
      letture$matricola_veicolo == giro$matricola_veicolo &
      uguale(servizio_del_giro(letture), giro$servizio_giro) &
      uguale(comune_giro, giro$comune_giro),
    ,
    drop = FALSE
  ]
}

#' Punti da inquadrare: le letture e, se sono al più `massimo`, i comuni dei giri
#' @noRd
area_giri <- function(letture, massimo = 6) {
  if (is.null(letture) || nrow(letture) == 0) {
    return(NULL)
  }
  punti <- letture[, c("latitudine", "longitudine")]
  indici <- unique(stats::na.omit(indice_comune(unique(letture[[
    "comune_giro"
  ]]))))
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

#' Letture da disegnare nella ricerca dei giri: prima le incongruenze, poi le più recenti
#' @return In ordine di tempo, con l'attributo `totale`.
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
      -as.numeric(df$data_ora_lettura),
      method = "radix"
    )
    df <- df[sort(utils::head(ordine, massimo)), , drop = FALSE]
  }
  structure(df, totale = totale)
}

#' Frase dell'intestazione della mappa dei giri
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

#' Tabella dei giri (reactable), un filtro per colonna
#'
#' In cima i giri con più letture fuori giro. Un click sceglie un giro, un
#' secondo lo toglie. Le righe restano nell'ordine di `riepilogo`: reactable
#' comunica il numero di riga dei dati, non della vista.
#' @noRd
tabella_giri <- function(riepilogo) {
  ora <- function(x) format(x, "%H:%M", tz = "UTC")
  tabella <- data.frame(
    Data = riepilogo$data_giro,
    Mezzo = paste(riepilogo$matricola_veicolo, riepilogo$targa_veicolo),
    `Giro previsto` = riepilogo$servizio_giro,
    `Comune del giro` = riepilogo$comune_giro,
    # Un giro senza l'informazione ha un trattino: si filtra come gli altri.
    Confermato = dplyr::coalesce(riepilogo$eseguito, "\u2013"),
    Orario = paste(
      ora(riepilogo$data_ora_prima_lettura),
      ora(riepilogo$data_ora_ultima_lettura),
      sep = "\u2013"
    ),
    Letture = riepilogo$letture,
    Bidoni = riepilogo$bidoni,
    `Fuori giro` = riepilogo$fuori_giro,
    `Fuori comune` = riepilogo$fuori_comune,
    `Letto a` = riepilogo$letto_in,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  # Incongruenze in rosso. La data arriva come AAAA-MM-GG e si mostra GG/MM/AAAA.
  incongruenze <- function(valore) {
    if (isTRUE(valore > 0)) list(color = "#b03a2e", fontWeight = 700)
  }
  # Non ancora confermato: arancio.
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
      Data = reactable::colDef(
        cell = data_italiana,
        filterMethod = filtro_data,
        minWidth = 85
      ),
      Mezzo = reactable::colDef(minWidth = 115),
      `Giro previsto` = reactable::colDef(minWidth = 120),
      `Comune del giro` = reactable::colDef(minWidth = 110),
      Confermato = reactable::colDef(
        style = non_confermato,
        align = "center",
        minWidth = 85
      ),
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

#' Filtro reactable per i numeri: un valore o un confronto (`>0`, `<5`, `>=2`)
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
