#' Mappa WebGL: base disegnata una volta, dati con i messaggi di `invia_mappa()`
#'
#' I messaggi partono solo con la scheda visibile. Sulla mappa principale le
#' bolle partono prima dei bidoni; zoom e spostamenti restano nel browser.
#' @param dati Reactive con le letture da disegnare, o `NULL`.
#' @param dati_vista Reactive con i punti da inquadrare: la vista cambia solo
#'   quando cambia questo.
#' @param attiva Reactive: `TRUE` con la scheda visibile.
#' @param messaggio Reactive con un avviso da mostrare sulla mappa.
#' @return Reactive con l'ultimo marker cliccato: `rfid` e `quando`.
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
    output$mappa <- mapgl::renderMaplibre(mappa_base(modalita))
    id_mappa <- session$ns("mappa")

    in_attesa <- reactiveValues(dati = FALSE, vista = FALSE)
    observeEvent(dati(), in_attesa$dati <- TRUE, ignoreNULL = FALSE)
    observeEvent(dati_vista(), in_attesa$vista <- TRUE, ignoreNULL = FALSE)

    # Letture disegnate: un marker porta il suo numero di riga, da 0.
    disegnati <- reactiveVal(NULL)

    observe({
      req(isTRUE(attiva()))
      if (in_attesa$dati) {
        df <- isolate(dati())
        righe <- switch(
          modalita,
          rfid = limita_letture_ricerca(df),
          giro = limita_letture_giro(df),
          df
        )
        disegnati(righe)
        dati <- dati_mappa(righe, modalita, riquadri = df)
        if (modalita == "cluster") {
          # Prima le bolle, le sole visibili da lontano, poi i bidoni.
          invia_mappa(session, id_mappa, "bolle", dati = dati[campi_bolle()])
          dati <- dati["punti"]
        }
        invia_mappa(session, id_mappa, "dati", dati = dati)
        in_attesa$dati <- FALSE
      }
      if (in_attesa$vista) {
        riquadro <- riquadro_vista(isolate(dati_vista()))
        if (!is.null(riquadro)) {
          invia_mappa(session, id_mappa, "inquadra", riquadro = riquadro)
        }
        in_attesa$vista <- FALSE
      }
    })

    selezione <- reactiveVal(NULL)
    observeEvent(input$mappa_clic, {
      df <- disegnati()
      riga <- suppressWarnings(as.integer(input$mappa_clic$i)) + 1L
      req(df, length(riga) == 1, !is.na(riga), riga >= 1, riga <= nrow(df))
      selezione(list(rfid = df$RFID[riga], quando = Sys.time()))
      invia_mappa(
        session,
        id_mappa,
        "popup",
        lng = df$longitudine[riga],
        lat = df$latitudine[riga],
        html = popup_lettura(df, riga)
      )
    })

    output$riepilogo <- renderText({
      df <- dati()
      if (is.null(df) || nrow(df) == 0) {
        return("")
      }
      n_rfid <- dplyr::n_distinct(df$RFID)
      switch(
        modalita,
        cluster = {
          # Con le bolle del territorio l'intestazione dice che cosa si vede.
          territorio <- any(!is.na(df[["cantiere"]]))
          descrizione <- if (territorio) {
            descrizione_vista(livello_aggregazione(input$mappa_zoom))
          }
          letture <- attr(df, "letture")
          paste0(
            if (!is.null(letture)) {
              paste(conta(letture, "lettura", "letture"), "di ")
            },
            conta(n_rfid, "RFID", "RFID"),
            " (ultima lettura registrata)",
            if (!is.null(descrizione)) paste0(" \u00b7 ", descrizione)
          )
        },
        rfid = {
          limiti <- limiti_mappa()
          sulla_mappa <- nrow(limita_letture_ricerca(df))
          paste0(
            conta(nrow(df), "lettura", "letture"),
            " di ",
            conta(n_rfid, "RFID", "RFID"),
            if (sulla_mappa < nrow(df)) {
              sprintf(
                " \u00b7 sulla mappa %s: %s",
                formatta_numero(sulla_mappa),
                if (sulla_mappa >= limiti$letture_ricerca) {
                  "le pi\u00f9 recenti"
                } else {
                  sprintf("le ultime %d di ogni RFID", limiti$letture_per_rfid)
                }
              )
            }
          )
        },
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
