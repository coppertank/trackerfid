#' The application server-side
#'
#' Collega i moduli: il dataset caricato viene ristretto al periodo di analisi,
#' che alimenta filtri, ricerche e mappe. L'analisi dei cluster lavora sullo
#' stesso periodo. Il click su un marker o su un cluster apre il dettaglio del
#' bidone nella barra laterale, con le letture per anno: lì entrano anche le
#' letture storiche, se sono state caricate.
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom rlang .data %||%
#' @noRd
app_server <- function(input, output, session) {
  # Un anno di letture supera di molto il limite predefinito di 5 MB: il
  # limite del caricamento dal browser è di 2 GB, modificabile con l'opzione
  # `trackerfid.max_upload_mb`.
  options(
    shiny.maxRequestSize = getOption("trackerfid.max_upload_mb", 2048) * 1024^2
  )

  caricamento <- mod_caricamento_server("caricamento")
  dati_caricati <- caricamento$letture
  periodo <- mod_periodo_server("periodo", dati = dati_caricati)
  # Letture del periodo di analisi: da qui in poi tutta l'app lavora su queste.
  dati <- periodo$dati
  filtri <- mod_filtri_server("filtri", dati = dati, azzera = dati_caricati)
  ricerca_rfid <- mod_ricerca_server("ricerca_rfid", dati = dati, tipo = "rfid")
  ricerca_utenza <- mod_ricerca_server(
    "ricerca_utenza",
    dati = dati,
    tipo = "utenza"
  )

  output$titolo <- renderText({
    intervallo <- periodo$periodo()
    if (is.null(intervallo)) {
      return("DASHBOARD RFID BIDONI RIFIUTI")
    }
    etichetta <- periodo$etichetta()
    # Un periodo personalizzato va scritto in breve per restare nel titolo.
    if (!startsWith(etichetta, "Anno")) {
      etichetta <- paste(format(intervallo, "%d/%m/%y"), collapse = "-")
    }
    toupper(paste("Dashboard RFID -", etichetta))
  })

  # --- Avvisi mostrati sopra le mappe quando non c'è nulla da disegnare -------
  vuoto <- function(df) is.null(df) || nrow(df) == 0
  # Avviso comune a tutte le mappe quando mancano dataset o letture del periodo.
  senza_dati <- reactive({
    if (is.null(dati_caricati())) {
      "Carica un file CSV dal pannello laterale per visualizzare i bidoni sulla mappa."
    } else if (is.null(dati())) {
      "Scegli un periodo di analisi valido nel pannello laterale."
    } else if (nrow(dati()) == 0) {
      "Nessuna lettura nel periodo di analisi selezionato."
    }
  })

  messaggio_principale <- reactive({
    if (!is.null(senza_dati())) {
      senza_dati()
    } else if (vuoto(filtri$dati_filtrati())) {
      "Nessun bidone corrisponde ai filtri selezionati."
    }
  })
  messaggio_ricerca <- function(ricerca, invito, nessun_risultato) {
    reactive({
      if (!is.null(senza_dati())) {
        senza_dati()
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
    # La vista si adatta al periodo e ai filtri di cantiere e comune, non
    # agli altri filtri.
    dati_vista = filtri$area,
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

  # --- Analisi dei cluster spaziali ---------------------------------------------
  clic_cluster <- mod_cluster_analysis_server(
    "cluster",
    dati = dati_caricati,
    periodo = periodo$periodo,
    etichetta = periodo$etichetta
  )

  # --- Dettaglio del bidone selezionato ------------------------------------------
  rfid_selezionato <- reactiveVal(NULL)

  purrr::walk(
    list(clic_principale, clic_rfid, clic_utenza, clic_cluster),
    function(clic) {
      observeEvent(clic(), {
        rfid_selezionato(clic()$rfid)
        # Porta il pannello in vista dopo che è stato aggiornato.
        session$onFlushed(function() {
          session$sendCustomMessage("trackerfid-mostra", "pannello_info")
        })
      })
    }
  )
  observeEvent(dati_caricati(), rfid_selezionato(NULL), ignoreNULL = FALSE)
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
        "Dettaglio Bidone",
        "circle-info",
        p(
          class = "suggerimento",
          "Clicca su un marker della mappa per vedere il dettaglio del bidone nel periodo."
        )
      ))
    }
    sezione_sidebar(
      "Dettaglio Bidone",
      "circle-info",
      azione = actionLink(
        "chiudi_info",
        "Chiudi",
        icon = icon("xmark"),
        class = "chiudi-info"
      ),
      crea_info_panel(
        analizza_rfid(letture),
        # Le letture per anno non dipendono dal periodo di analisi.
        andamento_rfid(rfid, dati_caricati(), caricamento$storico())
      )
    )
  })
}
