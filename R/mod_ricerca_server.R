#' Ricerca per RFID o per utenza
#'
#' `"rfid"`: tutte le letture del periodo degli RFID cercati, anche nella
#' tabella. `"utenza"`: l'ultima lettura dei bidoni la cui utenza *attuale* è
#' tra quelle cercate.
#' @param dati Reactive con la selezione del periodo, o `NULL`.
#' @return Lista di reactive: `risultati` (o `NULL`) e `codici` cercati.
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

    # Tabella delle letture, sopra la mappa: solo per la ricerca RFID.
    if (tipo == "rfid") {
      output$con_letture <- reactive({
        trovati <- risultati()
        !is.null(trovati) && nrow(trovati) > 0
      })
      outputOptions(output, "con_letture", suspendWhenHidden = FALSE)
      output$letture <- reactable::renderReactable({
        trovati <- risultati()
        req(trovati, nrow(trovati) > 0)
        tabella_letture_rfid(trovati)
      })
      output$nota_letture <- renderText({
        trovati <- risultati()
        req(trovati)
        sprintf(
          "%s nel periodo. Sulla mappa le ultime %d di ogni RFID.",
          conta(nrow(trovati), "lettura", "letture"),
          limiti_mappa()$letture_per_rfid
        )
      })
    }

    output$esito <- renderUI({
      cercati <- codici()
      if (is.null(cercati)) {
        return(NULL)
      }
      if (length(cercati) == 0) {
        return(div(
          class = "esito-ricerca esito-avviso",
          "Inserisci almeno un codice da cercare."
        ))
      }
      if (is.null(dati())) {
        return(div(
          class = "esito-ricerca esito-avviso",
          "Carica prima un file CSV."
        ))
      }
      trovati <- risultati()
      non_trovati <- cercati[!codice_in(cercati, trovati[[campo]])]
      n_trovati <- length(cercati) - length(non_trovati)

      riepilogo <- if (tipo == "rfid") {
        sprintf(
          "%s su %d trovati \u00b7 %s",
          conta(n_trovati, "RFID", "RFID"),
          length(cercati),
          conta(nrow(trovati), "lettura", "letture")
        )
      } else {
        sprintf(
          "%s su %d con bidoni \u00b7 %s",
          conta(n_trovati, "utenza", "utenze"),
          length(cercati),
          conta(nrow(trovati), "bidone", "bidoni")
        )
      }
      div(
        class = paste(
          "esito-ricerca",
          if (n_trovati > 0) "esito-ok" else "esito-avviso"
        ),
        tags$div(riepilogo),
        if (length(non_trovati) > 0) {
          tags$div(
            class = "esito-non-trovati",
            if (tipo == "rfid") {
              "Non trovati: "
            } else {
              "Nessun bidone attualmente associato a: "
            },
            paste(non_trovati, collapse = ", ")
          )
        }
      )
    })

    list(risultati = risultati, codici = reactive(codici()))
  })
}
