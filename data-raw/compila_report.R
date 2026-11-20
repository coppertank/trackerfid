# Compila un report Quarto di docs/ e mette il risultato in data-raw/output/.
# I report non fanno parte della pipeline: si compilano a mano, e il file
# resta in data-raw/output/ finché non si rifà.
#
# Il nome del file porta i parametri indicati, così compilando lo stesso
# report con parametri diversi si conservano tutte le versioni, da confrontare:
#   indicatori_letture_dal-2025-01-01_al-2025-12-31.html
#   indicatori_letture_dal-2026-01-01_al-2026-12-31.html
# Senza parametri il nome è quello del report (indicatori_letture.html).
#
# Il formato è HTML (predefinito) o PDF; con tutti e due, un file per formato
# con lo stesso nome. Il PDF è pensato per leggere su carta, o in un file fermo,
# le tabelle con molti campi (pagina A4 orizzontale, colonne dimensionate sul
# contenuto): lo produce Typst, che è incluso in Quarto e non richiede LaTeX.
#
# Dalla radice del progetto:
#   source("data-raw/compila_report.R")
#   compila_report("docs/confronto_antenne.qmd", list(servizio = "SECCO"))
#   compila_report("docs/indicatori_letture.qmd", formato = "pdf")
#   compila_report("docs/indicatori_letture.qmd", formato = c("html", "pdf"))
# Serve il programma quarto (quello incluso in Positron va bene).

#' Compila un report Quarto e ne restituisce i file in `cartella`
#'
#' Il file si chiama come il report, seguito dai parametri indicati: con
#' parametri diversi i file sono diversi e non si sovrascrivono; con gli stessi
#' parametri la versione nuova sostituisce la vecchia.
#' @param report Percorso del `.qmd`, dalla radice del progetto.
#' @param parametri Lista dei parametri del report; quelli omessi valgono
#'   quanto nell'intestazione del `.qmd`.
#' @param cartella Dove mettere i file.
#' @param etichetta Parte del nome che segue quello del report. `NULL`
#'   (predefinito): ricavata dai parametri indicati (`etichetta_parametri()`).
#'   Un testo: usato al loro posto, per esempio `"anno_2025"`. `""`: nessun
#'   suffisso, il nome è quello del report e il file precedente è sostituito.
#'   Un'etichetta che farebbe un nome troppo lungo è accorciata, con un'impronta
#'   dell'etichetta intera in fondo.
#' @param formato `"html"` (predefinito), `"pdf"` o tutti e due, `c("html", "pdf")`.
#'   Con più formati il report si esegue una volta per formato. Il PDF usa il
#'   formato `typst` del `.qmd`, se c'è, altrimenti quello predefinito di Quarto.
#' @return I percorsi dei file scritti, uno per formato, in modo invisibile.
compila_report <- function(
  report,
  parametri = list(),
  cartella = "data-raw/output",
  etichetta = NULL,
  formato = "html"
) {
  if (!file.exists("DESCRIPTION") || !file.exists(report)) {
    stop("Esegui dalla radice del progetto trackerfid, con il percorso di un report.")
  }
  formato <- match.arg(formato, c("html", "pdf"), several.ok = TRUE)
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
  nome <- paste0(base, if (nzchar(etichetta)) paste0("_", etichetta))
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  # Il formato di Quarto e l'estensione del file che produce.
  formati <- c(html = "html", pdf = "typst")
  estensioni <- c(html = "html", pdf = "pdf")
  # Le figure e il sorgente Typst che Quarto lascia accanto al report, se non
  # c'erano già, si tolgono a fine lavoro.
  cartella_figure <- file.path(dirname(report), paste0(base, "_files"))
  c_era_la_cartella <- dir.exists(cartella_figure)
  scritti <- character(0)
  for (f in unique(formato)) {
    # Non in silenzio: con `quiet = TRUE` un errore del report si vedrebbe come
    # un generico "Error running quarto CLI".
    quarto::quarto_render(
      report,
      output_format = formati[[f]],
      execute_params = parametri,
      quiet = FALSE
    )
    # Quarto scrive il file accanto al sorgente, con il nome del report: lo si
    # sposta, con il nome che porta i parametri.
    compilato <- file.path(dirname(report), paste0(base, ".", estensioni[[f]]))
    finale <- file.path(normalizePath(cartella), paste0(nome, ".", estensioni[[f]]))
    sostituito <- file.exists(finale)
    file.copy(compilato, finale, overwrite = TRUE)
    file.remove(compilato)
    message(
      "Report scritto in ", finale,
      if (sostituito) " (sostituisce la versione precedente con gli stessi parametri)"
    )
    scritti <- c(scritti, stats::setNames(finale, f))
  }
  sorgente_typst <- file.path(dirname(report), paste0(base, ".typ"))
  if (file.exists(sorgente_typst)) {
    file.remove(sorgente_typst)
  }
  if (!c_era_la_cartella && dir.exists(cartella_figure)) {
    unlink(cartella_figure, recursive = TRUE)
  }
  invisible(scritti)
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
