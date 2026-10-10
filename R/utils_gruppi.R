# Calcoli per gruppo su vettori interi, senza un ciclo sui gruppi.
#
# Con centinaia di migliaia di RFID un riepilogo valutato un gruppo alla volta
# costa decine di secondi: queste funzioni fanno lo stesso lavoro con poche
# operazioni vettoriali. Il gruppo è sempre un intero da 1 al numero dei
# gruppi, come quello restituito da `indice_gruppi()`.

#' Numera i gruppi nell'ordine in cui compaiono
#'
#' @param chiave Vettore con il valore che identifica il gruppo di ogni riga.
#' @return Vettore intero: 1 per il primo valore incontrato, 2 per il secondo.
#' @noRd
indice_gruppi <- function(chiave) {
  match(chiave, unique(chiave))
}

#' Primo e ultimo elemento di ogni gruppo, nell'ordine delle righe
#'
#' @param x Vettore dei valori.
#' @param gruppo Gruppo di ogni riga.
#' @param n_gruppi Numero dei gruppi.
#' @return Un valore per gruppo.
#' @noRd
primo_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  esito <- x[rep(NA_integer_, n_gruppi)]
  primi <- which(!duplicated(gruppo))
  esito[gruppo[primi]] <- x[primi]
  esito
}

#' @rdname primo_per_gruppo
#' @noRd
ultimo_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  esito <- x[rep(NA_integer_, n_gruppi)]
  ultimi <- which(!duplicated(gruppo, fromLast = TRUE))
  esito[gruppo[ultimi]] <- x[ultimi]
  esito
}

#' Ultimo valore non mancante di ogni gruppo, nell'ordine delle righe
#'
#' Un gruppo senza valori ha un mancante dello stesso tipo.
#' @noRd
ultimo_valido_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  esito <- x[rep(NA_integer_, n_gruppi)]
  validi <- which(!is.na(x))
  ultimi <- validi[!duplicated(gruppo[validi], fromLast = TRUE)]
  esito[gruppo[ultimi]] <- x[ultimi]
  esito
}

#' Somma dei valori di ogni gruppo
#'
#' Un gruppo senza righe ha somma zero.
#' @noRd
somma_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  esito <- numeric(n_gruppi)
  somme <- rowsum(as.numeric(x), gruppo, reorder = TRUE)
  esito[as.integer(rownames(somme))] <- somme[, 1]
  esito
}

#' Media dei valori di ogni gruppo
#'
#' Come `mean()`, corregge la prima stima con la media degli scarti: senza
#' questo passaggio l'ultima cifra può differire da quella di `mean()`.
#' @noRd
media_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  n <- tabulate(gruppo, n_gruppi)
  media <- somma_per_gruppo(x, gruppo, n_gruppi) / n
  media + somma_per_gruppo(x - media[gruppo], gruppo, n_gruppi) / n
}

#' Minimo e massimo di ogni gruppo
#'
#' @return Lista con `minimo` e `massimo`, un valore per gruppo. Ogni gruppo
#'   deve avere almeno una riga.
#' @noRd
estremi_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  n <- tabulate(gruppo, n_gruppi)
  ordinati <- x[order(gruppo, x, method = "radix")]
  fine <- cumsum(n)
  list(minimo = ordinati[fine - n + 1], massimo = ordinati[fine])
}

#' Quantile di ogni gruppo
#'
#' Stessa regola di `stats::quantile()` con il tipo predefinito: interpolazione
#' lineare tra i due valori più vicini. Ogni gruppo deve avere almeno una riga.
#'
#' @param prob Probabilità del quantile, tra 0 e 1.
#' @noRd
quantile_per_gruppo <- function(x, gruppo, prob, n_gruppi = max(gruppo)) {
  n <- tabulate(gruppo, n_gruppi)
  ordinati <- x[order(gruppo, x, method = "radix")]
  inizio <- cumsum(n) - n
  indice <- 1 + (n - 1) * prob
  basso <- floor(indice)
  quantile <- ordinati[inizio + basso]
  superiore <- ordinati[inizio + ceiling(indice)]
  peso <- indice - basso
  da_interpolare <- indice > basso & superiore != quantile
  quantile[da_interpolare] <- ((1 - peso) * quantile + peso * superiore)[
    da_interpolare
  ]
  quantile
}

#' Numero di valori distinti e non mancanti di ogni gruppo
#' @noRd
n_distinti_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  validi <- !is.na(x)
  coppie <- dplyr::distinct(dplyr::tibble(g = gruppo[validi], x = x[validi]))
  tabulate(coppie$g, n_gruppi)
}

#' Valore più frequente di ogni gruppo
#'
#' I mancanti non contano. A parità di frequenza vince il valore comparso per
#' ultimo nell'ordine delle righe.
#' @noRd
piu_frequente_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  esito <- x[rep(NA_integer_, n_gruppi)]
  validi <- which(!is.na(x))
  if (length(validi) == 0) {
    return(esito)
  }
  # Una coppia per ogni valore distinto di ogni gruppo.
  coppia <- indice_gruppi(paste(gruppo[validi], x[validi], sep = "\r"))
  n_coppie <- max(coppia)
  frequenza <- tabulate(coppia, n_coppie)
  ultima <- validi[!duplicated(coppia, fromLast = TRUE)]
  ultima_per_coppia <- integer(n_coppie)
  ultima_per_coppia[coppia[match(ultima, validi)]] <- ultima
  gruppo_coppia <- gruppo[ultima_per_coppia]
  migliore <- order(gruppo_coppia, -frequenza, -ultima_per_coppia)
  migliore <- migliore[!duplicated(gruppo_coppia[migliore])]
  esito[gruppo_coppia[migliore]] <- x[ultima_per_coppia[migliore]]
  esito
}
