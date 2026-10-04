#' The application server-side
#'
#' Collega i moduli: il dataset caricato alimenta filtri e ricerche, che a loro
#' volta alimentano le tre mappe. Il click su un marker apre il dettaglio del
#' bidone nella barra laterale.
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data %||%
#' @noRd
app_server <- function(input, output, session) {
  # I file di letture superano facilmente il limite predefinito di 5 MB.
  options(shiny.maxRequestSize = 100 * 1024^2)

  dati <- mod_caricamento_server("caricamento")
  filtri <- mod_filtri_server("filtri", dati = dati)
  ricerca_rfid <- mod_ricerca_server("ricerca_rfid", dati = dati, tipo = "rfid")
  ricerca_utenza <- mod_ricerca_server("ricerca_utenza", dati = dati, tipo = "utenza")

  # --- Avvisi mostrati sopra le mappe quando non c'è nulla da disegnare -------
  senza_dati <- "Carica un file CSV dal pannello laterale per visualizzare i bidoni sulla mappa."
  vuoto <- function(df) is.null(df) || nrow(df) == 0

  messaggio_principale <- reactive({
    if (is.null(dati())) {
      senza_dati
    } else if (vuoto(filtri$dati_filtrati())) {
      "Nessun bidone corrisponde ai filtri selezionati."
    }
  })
  messaggio_ricerca <- function(ricerca, invito, nessun_risultato) {
    reactive({
      if (is.null(dati())) {
        senza_dati
      } else if (length(ricerca$codici()) == 0) {
        invito
      } else if (vuoto(ricerca$risultati())) {
        nessun_risultato
      }
    })
  }

  # --- Mappe -------------------------------------------------------------------
  scheda_attiva <- function(nome) reactive(identical(input$scheda_attiva, nome))

  clic_principale <- mod_mappa_server(
    "mappa_principale",
    dati = filtri$dati_filtrati,
    modalita = "cluster",
    # La vista si adatta al dataset, non a ogni modifica dei filtri.
    dati_vista = dati,
    attiva = scheda_attiva("mappa"),
    messaggio = messaggio_principale
  )
  clic_rfid <- mod_mappa_server(
    "mappa_rfid",
    dati = ricerca_rfid$risultati,
    modalita = "rfid",
    attiva = scheda_attiva("rfid"),
    messaggio = messaggio_ricerca(
      ricerca_rfid,
      "Inserisci uno o pi\u00f9 RFID nel pannello laterale e premi \u00abCerca RFID\u00bb.",
      "Nessuna lettura trovata per gli RFID indicati."
    )
  )
  clic_utenza <- mod_mappa_server(
    "mappa_utenza",
    dati = ricerca_utenza$risultati,
    modalita = "utenza",
    attiva = scheda_attiva("utenza"),
    messaggio = messaggio_ricerca(
      ricerca_utenza,
      "Inserisci uno o pi\u00f9 ID utenza nel pannello laterale e premi \u00abCerca Utenza\u00bb.",
      "Nessun bidone attualmente associato alle utenze indicate."
    )
  )

  # --- Dettaglio del bidone selezionato ------------------------------------------
  rfid_selezionato <- reactiveVal(NULL)

  purrr::walk(list(clic_principale, clic_rfid, clic_utenza), function(clic) {
    observeEvent(clic(), {
      rfid_selezionato(clic()$rfid)
      # Porta il pannello in vista dopo che è stato aggiornato.
      session$onFlushed(function() {
        session$sendCustomMessage("trackerfid-mostra", "pannello_info")
      })
    })
  })
  observeEvent(dati(), rfid_selezionato(NULL), ignoreNULL = FALSE)
  observeEvent(input$chiudi_info, rfid_selezionato(NULL))

  output$info_panel <- renderUI({
    d <- dati()
    if (is.null(d)) {
      return(NULL)
    }
    rfid <- rfid_selezionato()
    letture <- if (!is.null(rfid)) d[d$RFID == rfid, , drop = FALSE]
    if (vuoto(letture)) {
      return(sezione_sidebar(
        "Dettaglio Bidone", "circle-info",
        p(class = "suggerimento", "Clicca su un marker della mappa per vedere il dettaglio del bidone.")
      ))
    }
    sezione_sidebar(
      "Dettaglio Bidone", "circle-info",
      azione = actionLink("chiudi_info", "Chiudi", icon = icon("xmark"), class = "chiudi-info"),
      crea_info_panel(analizza_rfid(letture))
    )
  })
}
