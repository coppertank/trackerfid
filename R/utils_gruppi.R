# Calcoli per gruppo su vettori interi, senza cicli sui gruppi: con centinaia
# di migliaia di RFID un riepilogo gruppo per gruppo costa decine di
# secondi. Il gruppo è un intero da 1 al numero dei gruppi (`indice_gruppi()`).

#' Numera i gruppi nell'ordine in cui compaiono
#' @noRd
indice_gruppi <- function(chiave) {
  match(chiave, unique(chiave))
}

#' Primo e ultimo elemento di ogni gruppo, nell'ordine delle righe
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

#' Ultimo valore non mancante di ogni gruppo
#' @noRd
ultimo_valido_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  esito <- x[rep(NA_integer_, n_gruppi)]
  validi <- which(!is.na(x))
  ultimi <- validi[!duplicated(gruppo[validi], fromLast = TRUE)]
  esito[gruppo[ultimi]] <- x[ultimi]
  esito
}

#' Somma di ogni gruppo; zero senza righe
#' @noRd
somma_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  esito <- numeric(n_gruppi)
  somme <- rowsum(as.numeric(x), gruppo, reorder = TRUE)
  esito[as.integer(rownames(somme))] <- somme[, 1]
  esito
}

#' Media di ogni gruppo, con la stessa correzione di `mean()`
#' @noRd
media_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  n <- tabulate(gruppo, n_gruppi)
  media <- somma_per_gruppo(x, gruppo, n_gruppi) / n
  media + somma_per_gruppo(x - media[gruppo], gruppo, n_gruppi) / n
}

#' Minimo e massimo di ogni gruppo, che deve avere almeno una riga
#' @noRd
estremi_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  n <- tabulate(gruppo, n_gruppi)
  ordinati <- x[order(gruppo, x, method = "radix")]
  fine <- cumsum(n)
  list(minimo = ordinati[fine - n + 1], massimo = ordinati[fine])
}

#' Quantile di ogni gruppo, come `stats::quantile()` con il tipo predefinito
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

#' Valori distinti non mancanti di ogni gruppo
#' @noRd
n_distinti_per_gruppo <- function(x, gruppo, n_gruppi = max(gruppo)) {
  validi <- !is.na(x)
  coppie <- dplyr::distinct(dplyr::tibble(g = gruppo[validi], x = x[validi]))
  tabulate(coppie$g, n_gruppi)
}

#' Valore più frequente di ogni gruppo; a parità vince l'ultimo comparso
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
