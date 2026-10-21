#' Testi dell'interfaccia di ricerca
#' @noRd
testi_ricerca <- function(tipo) {
  switch(
    tipo,
    rfid = list(
      titolo = "Ricerca per RFID",
      icona = "barcode",
      etichetta = "Ricerca RFID",
      segnaposto = "Inserisci uno o pi\u00f9 RFID separati da virgola o newline",
      pulsante = "Cerca RFID"
    ),
    utenza = list(
      titolo = "Ricerca per Utenza",
      icona = "user",
      etichetta = "Ricerca ID Utenza",
      segnaposto = "Inserisci uno o pi\u00f9 ID utenza separati da virgola o newline",
      pulsante = "Cerca Utenza"
    )
  )
}

#' Ricerca per RFID o per utenza: campo di testo, Cerca, Pulisci
#' @param tipo `"rfid"` o `"utenza"`.
#' @noRd
mod_ricerca_ui <- function(id, tipo = c("rfid", "utenza")) {
  tipo <- match.arg(tipo)
  ns <- NS(id)
  testi <- testi_ricerca(tipo)
  sezione_sidebar(
    testi$titolo,
    testi$icona,
    textAreaInput(
      ns("testo"),
      label = testi$etichetta,
      placeholder = testi$segnaposto,
      rows = 4,
      width = "100%",
      resize = "vertical"
    ),
    div(
      class = "pulsanti-ricerca",
      actionButton(
        ns("cerca"),
        testi$pulsante,
        icon = icon("magnifying-glass"),
        class = "btn-primary"
      ),
      actionButton(ns("pulisci"), "\u274c Pulisci Ricerca")
    ),
    uiOutput(ns("esito"))
  )
}

#' Tabella delle letture degli RFID cercati, da passare come `sovrapposto` a `mod_mappa_ui()`
#' @noRd
mod_ricerca_tabella_ui <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    "output.con_letture",
    ns = ns,
    riquadro_tabella_mappa(
      "Letture degli RFID cercati",
      reactable::reactableOutput(ns("letture")),
      nota = textOutput(ns("nota_letture"), inline = TRUE),
      classe = "tabella-letture"
    )
  )
}
