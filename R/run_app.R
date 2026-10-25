#' Avvia l'app
#'
#' @param ... Opzioni di golem: `demo = TRUE` apre i dati di esempio;
#'   `letture = "percorso"` legge un file dal disco all'avvio (CSV, Parquet o
#'   `.duckdb` della pipeline), condiviso da tutte le sessioni del processo.
#' @inheritParams shiny::shinyApp
#'
#' @examples
#' if (interactive()) {
#'   run_app(demo = TRUE)
#'   run_app(letture = "data-raw/output/letture_antenne.duckdb")
#' }
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
