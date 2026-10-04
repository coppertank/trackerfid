#' Modulo ricerca avanzata (RFID o utenza): logica
#'
#' - `"rfid"`: restituisce tutte le letture degli RFID cercati.
#' - `"utenza"`: restituisce l'ultima lettura degli RFID la cui utenza
#'   *attuale* è tra quelle cercate. Un bidone passato a un'altra utenza non
#'   compare cercando il vecchio proprietario.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con il dataset validato (o `NULL`).
#' @param tipo `"rfid"` oppure `"utenza"`.
#' @return Lista di reactive: `risultati` (letture trovate o `NULL`) e
#'   `codici` (codici cercati, `NULL` se non c'è una ricerca attiva).
#' @noRd
mod_ricerca_server <- function(id, dati, tipo = c("rfid", "utenza")) {
  tipo <- match.arg(tipo)
  campo <- if (tipo == "rfid") "RFID" else "id_utenza"

  moduleServer(id, function(input, output, session) {
    codici <- reactiveVal(NULL)

    observeEvent(input$cerca, {
      codici(analizza_input_ricerca(input$testo))
    })

    observeEvent(input$pulisci, {
      updateTextAreaInput(session, "testo", value = "")
      codici(NULL)
    })

    risultati <- reactive({
      d <- dati()
      cercati <- codici()
      if (is.null(d) || length(cercati) == 0) {
        return(NULL)
      }
      if (tipo == "rfid") {
        cerca_per_rfid(d, cercati)
      } else {
        filtra_ultimo_per_utenza(d, cercati)
      }
    })

    output$esito <- renderUI({
      cercati <- codici()
      if (is.null(cercati)) {
        return(NULL)
      }
      if (length(cercati) == 0) {
        return(div(class = "esito-ricerca esito-avviso", "Inserisci almeno un codice da cercare."))
      }
      if (is.null(dati())) {
        return(div(class = "esito-ricerca esito-avviso", "Carica prima un file CSV."))
      }
      trovati <- risultati()
      non_trovati <- cercati[!codice_in(cercati, trovati[[campo]])]
      n_trovati <- length(cercati) - length(non_trovati)

      riepilogo <- if (tipo == "rfid") {
        sprintf(
          "%s su %d trovati \u00b7 %s",
          conta(n_trovati, "RFID", "RFID"), length(cercati),
          conta(nrow(trovati), "lettura", "letture")
        )
      } else {
        sprintf(
          "%s su %d con bidoni \u00b7 %s",
          conta(n_trovati, "utenza", "utenze"), length(cercati),
          conta(nrow(trovati), "bidone", "bidoni")
        )
      }
      div(
        class = paste("esito-ricerca", if (n_trovati > 0) "esito-ok" else "esito-avviso"),
        tags$div(riepilogo),
        if (length(non_trovati) > 0) {
          tags$div(
            class = "esito-non-trovati",
            if (tipo == "rfid") "Non trovati: " else "Nessun bidone attualmente associato a: ",
            paste(non_trovati, collapse = ", ")
          )
        }
      )
    })

    list(risultati = risultati, codici = reactive(codici()))
  })
}
