#' Modulo mappa WebGL: logica
#'
#' La mappa di base viene disegnata una sola volta, vedi `mappa_base()`; i
#' dati e l'inquadratura arrivano dopo, con i messaggi di `invia_mappa()`,
#' così filtri e ricerche non ricaricano lo sfondo. I messaggi partono solo
#' quando la scheda della mappa è visibile: una mappa nascosta non riceve
#' dati che nessuno guarda. Nel browser `mappa.js` tiene in attesa quelli che
#' arrivano prima che la mappa sia pronta.
#'
#' Sulla mappa principale zoom e spostamenti restano nel browser: le bolle di
#' cantieri e comuni arrivano con i dati, e ogni livello si vede solo nella
#' sua fascia di zoom, vedi `livelli_mappa()`. Le bolle partono in un
#' messaggio a sé, prima dei bidoni: con centinaia di migliaia di bidoni il
#' browser impiega qualche decimo di secondo a raggrupparli, e le bolle, che
#' da lontano sono le sole visibili, non devono aspettarli.
#'
#' Il click su un bidone apre il dettaglio e il popup: il server lo costruisce
#' con `popup_lettura()`, dalla lettura disegnata.
#'
#' @param id Identificativo del modulo.
#' @param dati Reactive con le letture da disegnare (o `NULL`).
#' @param modalita `"cluster"`, `"rfid"`, `"utenza"` o `"giro"`, vedi
#'   `livelli_mappa()`.
#' @param dati_vista Reactive con i punti su cui inquadrare la mappa: la vista
#'   si adatta solo quando questo valore cambia.
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
    output$mappa <- mapgl::renderMaplibre(mappa_base(modalita))
    id_mappa <- session$ns("mappa")

    in_attesa <- reactiveValues(dati = FALSE, vista = FALSE)
    observeEvent(dati(), in_attesa$dati <- TRUE, ignoreNULL = FALSE)
    observeEvent(dati_vista(), in_attesa$vista <- TRUE, ignoreNULL = FALSE)

    # Letture disegnate: la posizione di un marker nel messaggio è il suo
    # numero di riga qui, da 0.
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
          # Prima le bolle, poche e le sole visibili da lontano; poi i
          # bidoni, che il browser raggruppa in qualche decimo di secondo:
          # così le bolle non aspettano i bidoni.
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
          # Le letture che superano i filtri, se il modulo dei filtri le ha
          # contate, e gli RFID sulla mappa, ognuno nella sua ultima lettura.
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
          # Sulla mappa le ultime letture di ogni RFID; tutte nella tabella.
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
