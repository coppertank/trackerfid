#' Modulo mappa Leaflet: logica
#'
#' La mappa di base viene disegnata una sola volta; marker e inquadratura si
#' aggiornano tramite `leafletProxy()`, così filtri e ricerche non ricaricano
#' lo sfondo. Gli aggiornamenti restano in attesa finché la scheda della mappa
#' non è visibile: Leaflet non gestisce bene i disegni su mappe nascoste.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con le letture da disegnare (o `NULL`).
#' @param modalita `"cluster"`, `"rfid"` o `"utenza"`, vedi `disegna_marker()`.
#' @param dati_vista Reactive con le letture su cui inquadrare la mappa: la
#'   vista si adatta solo quando questo valore cambia.
#' @param attiva Reactive: `TRUE` quando la scheda della mappa è visibile.
#' @param messaggio Reactive con un eventuale avviso da mostrare sulla mappa.
#' @return Reactive con l'ultimo marker cliccato: lista con `rfid` e `quando`.
#' @noRd
mod_mappa_server <- function(id,
                             dati,
                             modalita = c("cluster", "rfid", "utenza"),
                             dati_vista = dati,
                             attiva = function() TRUE,
                             messaggio = function() NULL) {
  modalita <- match.arg(modalita)
  moduleServer(id, function(input, output, session) {
    output$mappa <- leaflet::renderLeaflet(mappa_base(modalita))

    # Leaflet comunica lo zoom solo dopo aver disegnato la mappa nel browser.
    pronta <- reactiveVal(FALSE)
    observeEvent(input$mappa_zoom, pronta(TRUE), once = TRUE)

    in_attesa <- reactiveValues(marker = FALSE, vista = FALSE)
    observeEvent(dati(), in_attesa$marker <- TRUE, ignoreNULL = FALSE)
    observeEvent(dati_vista(), in_attesa$vista <- TRUE, ignoreNULL = FALSE)

    disegnati <- reactiveVal(NULL)

    observe({
      req(pronta(), isTRUE(attiva()))
      proxy <- leaflet::leafletProxy("mappa", session = session)
      if (in_attesa$marker) {
        df <- isolate(dati())
        disegna_marker(proxy, df, modalita)
        disegnati(df)
        in_attesa$marker <- FALSE
      }
      if (in_attesa$vista) {
        adatta_vista(proxy, isolate(dati_vista()))
        in_attesa$vista <- FALSE
      }
    })

    # L'identificativo del marker è il numero di riga dei dati disegnati.
    selezione <- reactiveVal(NULL)
    observeEvent(input$mappa_marker_click, {
      df <- disegnati()
      riga <- suppressWarnings(as.integer(input$mappa_marker_click$id))
      req(df, length(riga) == 1, !is.na(riga), riga >= 1, riga <= nrow(df))
      selezione(list(rfid = df$RFID[riga], quando = Sys.time()))
    })

    output$riepilogo <- renderText({
      df <- dati()
      if (is.null(df) || nrow(df) == 0) {
        return("")
      }
      n_rfid <- dplyr::n_distinct(df$RFID)
      switch(modalita,
        cluster = paste(conta(n_rfid, "bidone", "bidoni"), "sulla mappa (ultima lettura)"),
        rfid = paste(conta(nrow(df), "lettura", "letture"), "di", conta(n_rfid, "RFID", "RFID")),
        utenza = paste(
          conta(n_rfid, "bidone", "bidoni"), "di",
          conta(dplyr::n_distinct(df$id_utenza), "utenza", "utenze")
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

#' "1 bidone", "12 bidoni": conteggio con singolare e plurale
#' @noRd
conta <- function(n, singolare, plurale) {
  paste(formatta_numero(n), if (n == 1) singolare else plurale)
}
