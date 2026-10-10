#' Modulo mappa Leaflet: logica
#'
#' La mappa di base viene disegnata una sola volta; marker e inquadratura si
#' aggiornano tramite `leafletProxy()`, così filtri e ricerche non ricaricano
#' lo sfondo. Gli aggiornamenti restano in attesa finché la scheda della mappa
#' non è visibile: Leaflet non gestisce bene i disegni su mappe nascoste.
#'
#' Sulla mappa principale il modulo segue spostamenti e zoom e disegna la
#' vista scelta da `scegli_vista()`: da lontano una bolla per cantiere, poi
#' una per comune, da vicino i singoli bidoni. Se i bidoni sono troppi per il
#' browser, oltre il limite di `limiti_mappa()`, da vicino arrivano i soli
#' bidoni dell'area inquadrata, oppure una bolla per riquadro.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con le letture da disegnare (o `NULL`).
#' @param modalita `"cluster"`, `"rfid"`, `"utenza"` o `"giro"`, vedi
#'   `disegna_marker()`.
#' @param dati_vista Reactive con le letture su cui inquadrare la mappa: la
#'   vista si adatta solo quando questo valore cambia.
#' @param attiva Reactive: `TRUE` quando la scheda della mappa è visibile.
#' @param messaggio Reactive con un eventuale avviso da mostrare sulla mappa.
#' @return Reactive con l'ultimo marker cliccato: lista con `rfid` e `quando`.
#' @noRd
mod_mappa_server <- function(
  id,
  dati,
  modalita = c("cluster", "rfid", "utenza", "giro"),
  dati_vista = dati,
  attiva = function() TRUE,
  messaggio = function() NULL
) {
  modalita <- match.arg(modalita)
  moduleServer(id, function(input, output, session) {
    output$mappa <- leaflet::renderLeaflet(mappa_base(modalita))

    # Leaflet comunica lo zoom solo dopo aver disegnato la mappa nel browser.
    pronta <- reactiveVal(FALSE)
    observeEvent(input$mappa_zoom, pronta(TRUE), once = TRUE)

    in_attesa <- reactiveValues(marker = FALSE, vista = FALSE)
    observeEvent(dati(), in_attesa$marker <- TRUE, ignoreNULL = FALSE)
    observeEvent(dati_vista(), in_attesa$vista <- TRUE, ignoreNULL = FALSE)

    # Letture disegnate una per una, bolle disegnate e vista che le descrive:
    # lista con `tipo`, `riquadro` (l'area caricata) e `zoom`.
    disegnati <- reactiveVal(NULL)
    bolle <- reactiveVal(NULL)
    vista <- reactiveVal(NULL)

    disegna <- function(df) {
      proxy <- leaflet::leafletProxy("mappa", session = session)
      zoom <- isolate(input$mappa_zoom)
      scelta <- if (modalita == "cluster") {
        scegli_vista(df, isolate(input$mappa_bounds), zoom)
      } else {
        list(tipo = "tutti")
      }
      if (scelta$tipo %in% c("tutti", "singoli")) {
        righe <- if (scelta$tipo == "singoli") {
          df[scelta$righe, , drop = FALSE]
        } else if (modalita == "rfid") {
          limita_letture_ricerca(df)
        } else if (modalita == "giro") {
          limita_letture_giro(df)
        } else {
          df
        }
        disegna_marker(proxy, righe, modalita, riquadri = df)
        disegnati(righe)
        bolle(NULL)
      } else {
        da_raggruppare <- if (scelta$tipo == "griglia") {
          df[scelta$righe, , drop = FALSE]
        } else {
          df
        }
        gruppi <- aggrega_marker(da_raggruppare, scelta$tipo, zoom)
        aggiungi_bolle(pulisci_mappa(proxy), gruppi)
        disegnati(NULL)
        bolle(gruppi)
      }
      vista(list(tipo = scelta$tipo, riquadro = scelta$riquadro, zoom = zoom))
    }

    observe({
      req(pronta(), isTRUE(attiva()))
      if (in_attesa$marker) {
        disegna(isolate(dati()))
        in_attesa$marker <- FALSE
      }
      if (in_attesa$vista) {
        adatta_vista(
          leaflet::leafletProxy("mappa", session = session),
          isolate(dati_vista())
        )
        in_attesa$vista <- FALSE
      }
    })

    # Dopo uno spostamento o uno zoom la mappa si ridisegna solo se la vista
    # disegnata non va più bene per lo zoom e per l'area inquadrata.
    if (modalita == "cluster") {
      spostamento <- debounce(
        reactive(list(input$mappa_bounds, input$mappa_zoom)),
        250
      )
      observeEvent(spostamento(), {
        df <- dati()
        req(pronta(), isTRUE(attiva()), !in_attesa$marker, !is.null(df))
        nuova <- scegli_vista(df, input$mappa_bounds, input$mappa_zoom)
        if (
          vista_da_rifare(vista(), nuova, input$mappa_bounds, input$mappa_zoom)
        ) {
          disegna(df)
        }
      })
    }

    # L'identificativo di un marker è il numero di riga delle letture
    # disegnate; quello di una bolla inizia con "bolla-".
    selezione <- reactiveVal(NULL)
    observeEvent(input$mappa_marker_click, {
      id <- as.character(input$mappa_marker_click$id %||% "")
      if (startsWith(id, "bolla-")) {
        gruppi <- bolle()
        riga <- suppressWarnings(as.integer(sub("^bolla-", "", id)))
        req(gruppi, !is.na(riga), riga >= 1, riga <= nrow(gruppi))
        # Un click su una bolla ingrandisce sui suoi bidoni.
        leaflet::fitBounds(
          leaflet::leafletProxy("mappa", session = session),
          lng1 = gruppi$lng_min[riga] - 0.001,
          lat1 = gruppi$lat_min[riga] - 0.001,
          lng2 = gruppi$lng_max[riga] + 0.001,
          lat2 = gruppi$lat_max[riga] + 0.001,
          options = list(maxZoom = 17)
        )
        return()
      }
      df <- disegnati()
      riga <- suppressWarnings(as.integer(id))
      req(df, length(riga) == 1, !is.na(riga), riga >= 1, riga <= nrow(df))
      selezione(list(rfid = df$RFID[riga], quando = Sys.time()))
    })

    output$riepilogo <- renderText({
      df <- dati()
      if (is.null(df) || nrow(df) == 0) {
        return("")
      }
      n_rfid <- dplyr::n_distinct(df$RFID)
      # Con le bolle l'intestazione dice che cosa si sta guardando.
      tipo <- vista()$tipo
      descrizione <- if (!is.null(tipo)) descrizione_vista(tipo)
      switch(
        modalita,
        cluster = paste0(
          conta(n_rfid, "bidone", "bidoni"),
          " sulla mappa (ultima lettura)",
          if (!is.null(descrizione)) paste0(" \u00b7 ", descrizione)
        ),
        rfid = paste0(
          conta(nrow(df), "lettura", "letture"),
          " di ",
          conta(n_rfid, "RFID", "RFID"),
          if (nrow(df) > limiti_mappa()$letture_ricerca) {
            sprintf(
              " \u00b7 sulla mappa le %s pi\u00f9 recenti",
              formatta_numero(limiti_mappa()$letture_ricerca)
            )
          }
        ),
        utenza = paste(
          conta(n_rfid, "bidone", "bidoni"),
          "di",
          conta(dplyr::n_distinct(df$id_utenza), "utenza", "utenze")
        ),
        giro = paste0(
          descrizione_giri(df),
          if (nrow(df) > limiti_mappa()$letture_ricerca) {
            sprintf(
              " \u00b7 sulla mappa %s letture: prima quelle fuori giro o fuori comune",
              formatta_numero(limiti_mappa()$letture_ricerca)
            )
          }
        )
      )
    })

    output$messaggio <- renderUI({
      testo <- messaggio()
      if (is.null(testo)) {
        return(NULL)
      }
      div(class = "mappa-messaggio", testo)
    })

    selezione
  })
}
