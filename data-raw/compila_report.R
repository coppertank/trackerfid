# Compila un report Quarto di docs/ e mette l'HTML in data-raw/output/.
# I report non fanno parte della pipeline: si compilano a mano, e l'HTML
# resta in data-raw/output/ finché non si rifà.
#
# Dalla radice del progetto:
#   source("data-raw/compila_report.R")
#   compila_report("docs/confronto_antenne.qmd", list(servizio = "SECCO"))
# Serve il programma quarto (quello incluso in Positron va bene).

#' Compila un report Quarto e ne restituisce l'HTML in `cartella`
#' @param report Percorso del `.qmd`, dalla radice del progetto.
#' @param parametri Lista dei parametri del report; quelli omessi valgono
#'   quanto nell'intestazione del `.qmd`.
#' @param cartella Dove mettere l'HTML.
compila_report <- function(
  report,
  parametri = list(),
  cartella = "data-raw/output"
) {
  if (!file.exists("DESCRIPTION") || !file.exists(report)) {
    stop("Esegui dalla radice del progetto trackerfid, con il percorso di un report.")
  }
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  nome <- paste0(tools::file_path_sans_ext(basename(report)), ".html")
  quarto::quarto_render(report, execute_params = parametri, quiet = TRUE)
  # Quarto scrive l'HTML accanto al sorgente: lo si sposta.
  compilato <- file.path(dirname(report), nome)
  finale <- file.path(normalizePath(cartella), nome)
  file.copy(compilato, finale, overwrite = TRUE)
  file.remove(compilato)
  message("Report scritto in ", finale)
  invisible(finale)
}
