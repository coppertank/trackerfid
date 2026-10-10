#' Run the Shiny Application
#'
#' Avvia la dashboard RFID dei bidoni per rifiuti.
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' Con `demo = TRUE` l'app si apre con i dati di esempio già caricati.
#' Con `letture = "percorso"` e, se serve, `storico = "percorso"` l'app legge
#' i file dal disco all'avvio, senza passare dal caricamento del browser: è
#' il modo più rapido per i file con milioni di letture. I file restano in
#' memoria per tutte le sessioni dello stesso processo.
#' @inheritParams shiny::shinyApp
#'
#' @examples
#' if (interactive()) {
#'   run_app()
#'   run_app(demo = TRUE)
#'   run_app(letture = "data-raw/output/letture_app.csv")
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
