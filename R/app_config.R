#' Percorso di un file dell'app nel pacchetto (golem)
#' @noRd
app_sys <- function(
	...
) {
	system.file(..., package = "trackerfid")
}
