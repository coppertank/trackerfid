# Compila un report Quarto di docs/ e mette l'HTML in data-raw/output/.
# I report non fanno parte della pipeline: si compilano a mano, e l'HTML
# resta in data-raw/output/ finché non si rifà.
#
# Il nome dell'HTML porta i parametri indicati, così compilando lo stesso
# report con parametri diversi si conservano tutte le versioni, da confrontare:
#   indicatori_letture_dal-2025-01-01_al-2025-12-31.html
#   indicatori_letture_dal-2026-01-01_al-2026-12-31.html
# Senza parametri il nome è quello del report (indicatori_letture.html).
#
# Dalla radice del progetto:
#   source("data-raw/compila_report.R")
#   compila_report("docs/confronto_antenne.qmd", list(servizio = "SECCO"))
# Serve il programma quarto (quello incluso in Positron va bene).

#' Compila un report Quarto e ne restituisce l'HTML in `cartella`
#'
#' L'HTML si chiama come il report, seguito dai parametri indicati: con
#' parametri diversi i file sono diversi e non si sovrascrivono; con gli stessi
#' parametri la versione nuova sostituisce la vecchia.
#' @param report Percorso del `.qmd`, dalla radice del progetto.
#' @param parametri Lista dei parametri del report; quelli omessi valgono
#'   quanto nell'intestazione del `.qmd`.
#' @param cartella Dove mettere l'HTML.
#' @param etichetta Parte del nome che segue quello del report. `NULL`
#'   (predefinito): ricavata dai parametri indicati (`etichetta_parametri()`).
#'   Un testo: usato al loro posto, per esempio `"anno_2025"`. `""`: nessun
#'   suffisso, il nome è quello del report e il file precedente è sostituito.
#'   Un'etichetta che farebbe un nome troppo lungo è accorciata, con un'impronta
#'   dell'etichetta intera in fondo.
compila_report <- function(
  report,
  parametri = list(),
  cartella = "data-raw/output",
  etichetta = NULL
) {
  if (!file.exists("DESCRIPTION") || !file.exists(report)) {
    stop("Esegui dalla radice del progetto trackerfid, con il percorso di un report.")
  }
  if (is.null(etichetta)) {
    etichetta <- etichetta_parametri(parametri)
  } else {
    etichetta <- pulisci_per_nome_file(etichetta)
  }
  base <- tools::file_path_sans_ext(basename(report))
  # Troppi parametri danno un nome troppo lungo: lo si accorcia e si aggiunge
  # un'impronta dell'etichetta intera, così parametri diversi restano distinti.
  if (nchar(base) + nchar(etichetta) > 150) {
    etichetta <- paste0(substr(etichetta, 1, 80), "_", impronta_testo(etichetta))
    message("Etichetta troppo lunga per un nome di file: accorciata in «", etichetta, "».")
  }
  nome <- paste0(base, if (nzchar(etichetta)) paste0("_", etichetta), ".html")
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  # Non in silenzio: con `quiet = TRUE` un errore del report si vedrebbe come
  # un generico "Error running quarto CLI".
  quarto::quarto_render(report, execute_params = parametri, quiet = FALSE)
  # Quarto scrive l'HTML accanto al sorgente, con il nome del report: lo si
  # sposta, con il nome che porta i parametri.
  compilato <- file.path(dirname(report), paste0(base, ".html"))
  finale <- file.path(normalizePath(cartella), nome)
  sostituito <- file.exists(finale)
  file.copy(compilato, finale, overwrite = TRUE)
  file.remove(compilato)
  message(
    "Report scritto in ", finale,
    if (sostituito) " (sostituisce la versione precedente con gli stessi parametri)"
  )
  invisible(finale)
}

#' Rende un testo adatto al nome di un file
#'
#' Tiene lettere e cifre non accentate; ogni altro gruppo di caratteri, per
#' esempio `/` di una data o di un percorso, diventa un trattino.
#' @param x Testo.
pulisci_per_nome_file <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  gsub("^-+|-+$", "", x)
}

#' Ricava dai parametri indicati la parte del nome dell'HTML che li descrive
#'
#' Una voce per parametro, `nome-valore`, nell'ordine indicato e separate da
#' `_`: `list(dal = "2025/01/01", al = "2025/12/31")` dà
#' `"dal-2025-01-01_al-2025-12-31"`. Un vettore diventa i suoi valori uniti da
#' un trattino; un percorso, il solo nome del file senza estensione; i parametri
#' vuoti non compaiono.
#' @param parametri Lista con nome, come per `compila_report()`.
etichetta_parametri <- function(parametri) {
  if (length(parametri) == 0) {
    return("")
  }
  nomi <- names(parametri)
  if (is.null(nomi) || any(is.na(nomi) | !nzchar(nomi))) {
    stop("I parametri devono avere tutti un nome.", call. = FALSE)
  }
  voci <- vapply(names(parametri), function(nome) {
    valori <- as.character(unlist(parametri[[nome]]))
    valori <- trimws(valori[!is.na(valori)])
    valori <- valori[nzchar(valori)]
    if (length(valori) == 0) {
      return("")
    }
    # Una data come 2025/01/01 ha la barra ma non è un percorso.
    e_data <- grepl("^[0-9]{4}[-/.][0-9]{1,2}[-/.][0-9]{1,2}$", valori)
    e_percorso <- !e_data & grepl("[/\\\\]", valori)
    valori[e_percorso] <- tools::file_path_sans_ext(basename(gsub("\\\\", "/", valori[e_percorso])))
    paste0(nome, "-", pulisci_per_nome_file(paste(valori, collapse = "-")))
  }, character(1))
  paste(voci[nzchar(voci)], collapse = "_")
}

#' Impronta di 8 caratteri di un testo (per distinguere nomi accorciati)
#' @param x Testo.
impronta_testo <- function(x) {
  file <- tempfile()
  on.exit(unlink(file))
  writeLines(x, file)
  substr(unname(tools::md5sum(file)), 1, 8)
}
