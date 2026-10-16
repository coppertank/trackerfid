# Giri dei mezzi: che cosa ha letto un mezzo in un giorno, e dove.
#
# Un giro è il lavoro di un mezzo in un giorno, per un servizio e in un
# comune: lo dice il calendario dei mezzi, e nel file sta su ogni lettura come
# `servizio_giro` e `comune_giro`. Le funzioni di questo file servono
# alla scheda "Ricerca Giro": trovano le letture di un mezzo in un intervallo
# di giorni e le riassumono giro per giro, con le letture fuori giro e quelle
# fuori comune, vedi `aggiungi_geografia()`.

#' Mezzi presenti nelle letture
#'
#' @param dati Letture validate.
#' @return Vettore delle matricole, in ordine: i nomi sono le etichette per
#'   l'elenco, con matricola e targa.
#' @noRd
mezzi_presenti <- function(dati) {
  if (is.null(dati) || nrow(dati) == 0) {
    return(character(0))
  }
  mezzi <- dati[
    !duplicated(dati$matricola_veicolo),
    c("matricola_veicolo", "targa_veicolo")
  ]
  mezzi <- mezzi[order(mezzi$matricola_veicolo), ]
  stats::setNames(
    as.character(mezzi$matricola_veicolo),
    paste(mezzi$matricola_veicolo, "\u00b7", mezzi$targa_veicolo)
  )
}

#' Giorni coperti dalle letture
#'
#' @param dati Letture validate.
#' @return Vettore con il primo e l'ultimo giorno, `NULL` senza letture.
#' @noRd
giorni_letture <- function(dati) {
  if (is.null(dati) || nrow(dati) == 0) {
    return(NULL)
  }
  lubridate::as_date(range(dati$giorno_lettura))
}

#' Letture dei mezzi in un intervallo di giorni
#'
#' @param dati Letture validate.
#' @param mezzo Matricola del mezzo. Tutti i mezzi, se manca o è vuota.
#' @param dal,al Primo e ultimo giorno, inclusi.
#' @param solo Voci tra `"fuori_giro"` e `"fuori_comune"`: restano le sole
#'   letture con almeno una delle incongruenze indicate. Tutte, se è vuoto.
#' @return Le letture trovate, in ordine di tempo, con la colonna `giorno`.
#' @noRd
cerca_giri <- function(dati, mezzo = NULL, dal, al, solo = character(0)) {
  dal <- lubridate::as_date(dal)
  al <- lubridate::as_date(al)
  # Il confronto è sugli istanti: convertire in giorni milioni di letture a
  # ogni ricerca costerebbe di più.
  inizio <- as.POSIXct(paste(format(dal), "00:00:00"), tz = "UTC")
  fine <- as.POSIXct(paste(format(al + 1), "00:00:00"), tz = "UTC")
  tieni <- dati$giorno_lettura >= inizio & dati$giorno_lettura < fine
  if (!is.null(mezzo) && length(mezzo) == 1 && !is.na(mezzo) && nzchar(mezzo)) {
    tieni <- tieni & dati$matricola_veicolo == mezzo
  }
  solo <- intersect(solo, c("fuori_giro", "fuori_comune"))
  if (length(solo) > 0) {
    incongruenze <- incongruenze_comune(dati)
    tieni <- tieni & Reduce(`|`, incongruenze[solo])
  }
  trovate <- dati[which(tieni), , drop = FALSE]
  trovate <- trovate[
    order(trovate$giorno_lettura, trovate$RFID, method = "radix"),
    ,
    drop = FALSE
  ]
  trovate$giorno <- lubridate::as_date(trovate$giorno_lettura)
  trovate
}

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
#' comune del calendario, e che cosa ha fatto: quante letture, di quanti
#' bidoni, quante fuori giro e quante fuori comune, vedi `coerenza_comune()`,
#' e in quali comuni sono state fatte le letture.
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
  riepilogo <- dplyr::tibble(
    giorno = primo_per_gruppo(giorno, gruppo, n),
    matricola_veicolo = primo_per_gruppo(letture$matricola_veicolo, gruppo, n),
    targa_veicolo = primo_per_gruppo(letture$targa_veicolo, gruppo, n),
    servizio_giro = primo_per_gruppo(servizio_giro, gruppo, n),
    comune_giro = primo_per_gruppo(comune_giro, gruppo, n),
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
#' stanno in cima. Un click su una riga mostra sulla mappa le sole letture di
#' quel giro.
#'
#' @param riepilogo Risultato di `riepilogo_giri()`.
#' @noRd
tabella_giri_dt <- function(riepilogo) {
  ora <- function(x) format(x, "%H:%M", tz = "UTC")
  tabella <- data.frame(
    Data = format(riepilogo$giorno, "%d/%m/%Y"),
    Mezzo = paste(riepilogo$matricola_veicolo, riepilogo$targa_veicolo),
    `Giro previsto` = riepilogo$servizio_giro,
    `Comune del giro` = riepilogo$comune_giro,
    Orario = paste(ora(riepilogo$prima_lettura), ora(riepilogo$ultima_lettura), sep = "\u2013"),
    Letture = riepilogo$letture,
    Bidoni = riepilogo$bidoni,
    `Fuori giro` = riepilogo$fuori_giro,
    `Fuori comune` = riepilogo$fuori_comune,
    `Letto a` = riepilogo$letto_in,
    # Colonna nascosta: la data come numero, per ordinare.
    ordine_giorno = as.integer(riepilogo$giorno),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  lingua <- lingua_dt()
  lingua$zeroRecords <- "Nessun giro trovato"
  lingua$emptyTable <- "Nessun giro nei giorni scelti"
  nascosta <- ncol(tabella) - 1L
  DT::datatable(
    tabella,
    rownames = FALSE,
    selection = "single",
    class = "compact stripe hover nowrap",
    options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 25, 50),
      scrollX = TRUE,
      language = lingua,
      order = list(list(7, "desc"), list(nascosta, "desc")),
      columnDefs = list(
        list(targets = nascosta, visible = FALSE, searchable = FALSE),
        # La data si ordina con la colonna nascosta.
        list(targets = 0, orderData = nascosta)
      )
    )
  ) |>
    DT::formatStyle(
      c("Fuori giro", "Fuori comune"),
      fontWeight = DT::styleInterval(0, c("normal", "bold")),
      color = DT::styleInterval(0, c("inherit", "#b03a2e"))
    )
}
