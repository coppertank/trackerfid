# Popup dei marker, dettaglio del bidone e riquadro delle statistiche.

#' Formatta data e ora nel formato italiano
#' @noRd
formatta_data_ora <- function(x, formato = "%d/%m/%Y %H:%M:%S") {
  format(x, formato, tz = "UTC")
}

#' Conteggio con il separatore delle migliaia, mai in notazione scientifica
#' @noRd
formatta_numero <- function(x) {
  format(
    x,
    big.mark = ".",
    decimal.mark = ",",
    trim = TRUE,
    scientific = FALSE
  )
}

#' "1 bidone", "12 bidoni": conteggio con singolare e plurale
#' @noRd
conta <- function(n, singolare, plurale) {
  paste(formatta_numero(n), if (n == 1) singolare else plurale)
}

#' Popup dei marker, uno per lettura, con i valori del file in escape HTML
#'
#' I campi facoltativi compaiono solo se ci sono. Righe in più per una
#' lettura fuori comune o fuori giro, «Al confine» per una lettura in un
#' altro comune ma entro la tolleranza, e una per i servizi non coerenti.
#' @param servizio_atteso Servizio atteso dal giro.
#' @param comune Comune della fornitura.
#' @param comune_giro Mostrato solo se diverso dal comune di lettura.
#' @param coerenza_comune,coerenza_servizio Valori delle due coerenze.
#' @param distanza_comune_dichiarato Metri dal comune dichiarato.
#' @noRd
create_popup_html <- function(
  rfid,
  servizio_atteso,
  targa,
  matricola,
  data_ora_lettura,
  id_utenza,
  presente,
  comune = NULL,
  comune_lettura = NULL,
  comune_giro = NULL,
  cantiere = NULL,
  coerenza_comune = NULL,
  servizio_giro = NULL,
  coerenza_servizio = NULL,
  servizio_transponder = NULL,
  distanza_comune_dichiarato = NULL
) {
  testo <- function(x, se_mancante = "N/A") {
    htmltools::htmlEscape(tidyr::replace_na(as.character(x), se_mancante))
  }
  data_formattata <- formatta_data_ora(data_ora_lettura)
  riga_facoltativa <- function(etichetta, valori, mostra = TRUE) {
    if (is.null(valori)) {
      return("")
    }
    ifelse(
      !is.na(valori) & mostra,
      paste0("<b>", etichetta, ":</b> ", testo(valori), "<br>"),
      ""
    )
  }
  riga_comune <- riga_facoltativa("Comune a database", comune)
  riga_lettura <- paste0(
    riga_facoltativa("Comune di lettura", comune_lettura),
    riga_facoltativa(
      "Comune del giro",
      comune_giro,
      if (is.null(comune_lettura)) {
        TRUE
      } else {
        is.na(comune_lettura) | comune_lettura != comune_giro
      }
    )
  )
  riga_cantiere <- riga_facoltativa("Cantiere", cantiere)
  # In un altro comune, ma coerente perché vicina al confine.
  riga_confine <- if (
    is.null(distanza_comune_dichiarato) || is.null(coerenza_comune)
  ) {
    ""
  } else {
    distanza <- suppressWarnings(as.numeric(distanza_comune_dichiarato))
    di <- ifelse(
      presente %in% "Presente",
      "del comune a database",
      "del comune del giro"
    )
    ifelse(
      coerenza_comune %in%
        stati_coerenza_comune()[["coerente"]] &
        !is.na(distanza),
      paste0(
        "<b>Al confine:</b> letto ",
        ifelse(
          distanza == 0,
          paste("dentro il confine", di),
          paste0(formatta_numero(distanza), " m oltre il confine ", di)
        ),
        "<br>"
      ),
      ""
    )
  }
  # Fuori comune o fuori giro: dove era dichiarata e dove è stata fatta.
  riga_incongruenza <- function(etichetta, dichiarato) {
    n <- length(rfid)
    if (is.null(coerenza_comune)) {
      return(rep("", n))
    }
    dove <- if (is.null(comune_lettura)) {
      rep(NA_character_, n)
    } else {
      as.character(comune_lettura)
    }
    ifelse(
      coerenza_comune %in% etichetta,
      paste0(
        "<b class='popup-incongruenza'>\u26a0 ",
        etichetta,
        ":</b> ",
        dichiarato,
        " ",
        testo(rep_len(dichiarato_da(etichetta), n)),
        ", letto ",
        ifelse(
          is.na(dove),
          "in un comune sconosciuto",
          paste("a", testo(dove))
        ),
        "<br>"
      ),
      ""
    )
  }
  dichiarato_da <- function(etichetta) {
    valori <- if (etichetta == stati_coerenza_comune()[["fuori_comune"]]) {
      comune
    } else {
      comune_giro
    }
    if (is.null(valori)) NA_character_ else valori
  }
  stati_comune <- stati_coerenza_comune()
  riga_fuori_comune <- riga_incongruenza(
    stati_comune[["fuori_comune"]],
    "a database"
  )
  riga_fuori_giro <- riga_incongruenza(stati_comune[["fuori_giro"]], "giro a")
  riga_database <- riga_facoltativa("Servizio a Database", servizio_transponder)
  riga_giro <- riga_facoltativa("Servizio Giro", servizio_giro)
  # Servizio a database e servizio atteso che non concordano.
  riga_coerenza <- if (is.null(coerenza_servizio)) {
    ""
  } else {
    ifelse(
      coerenza_servizio %in% stati_coerenza()[["non_coerente"]],
      paste0(
        "<b class='popup-incongruenza'>\u26a0 Servizio non coerente:</b> ",
        htmltools::htmlEscape(descrivi_servizi(
          database = servizio_transponder,
          atteso = servizio_atteso
        )),
        "<br>"
      ),
      ""
    )
  }

  paste0(
    "<div style='font-family: Arial; font-size: 12px;'>",
    "<b>RFID:</b> ",
    testo(rfid),
    "<br>",
    riga_database,
    "<b>Servizio Atteso:</b> ",
    testo(servizio_atteso),
    "<br>",
    riga_giro,
    "<b>Targa:</b> ",
    testo(targa),
    "<br>",
    "<b>Matricola:</b> ",
    testo(matricola),
    "<br>",
    "<b>Lettura:</b> ",
    data_formattata,
    "<br>",
    "<b>Utenza:</b> ",
    # Un censito senza utenza, come un sacchetto, non è un non censito.
    ifelse(
      is.na(id_utenza) & !(presente %in% "Presente"),
      "Non Censito",
      testo(id_utenza)
    ),
    "<br>",
    riga_comune,
    riga_lettura,
    riga_confine,
    riga_cantiere,
    "<b>Censito:</b> ",
    testo(presente),
    "<br>",
    riga_fuori_comune,
    riga_fuori_giro,
    riga_coerenza,
    "</div>"
  )
}

#' Servizi di una lettura in un testo: "a database CARTA, atteso SECCO, giro SECCO PAP"
#' @noRd
descrivi_servizi <- function(database = NULL, atteso = NULL, giro = NULL) {
  parti <- list(
    "a database" = database,
    "atteso" = atteso,
    "giro" = giro
  )
  n <- max(lengths(parti))
  testi <- purrr::imap(parti, function(valori, etichetta) {
    valori <- rep_len(as.character(valori %||% NA_character_), n)
    ifelse(is.na(valori), NA_character_, paste(etichetta, valori))
  })
  apply(
    do.call(cbind, testi),
    1,
    function(riga) paste(riga[!is.na(riga)], collapse = ", ")
  )
}

#' Blocco del pannello di dettaglio
#' @noRd
blocco_info <- function(tipo, titolo, ...) {
  htmltools::tags$div(
    class = paste("info-blocco", paste0("info-", tipo)),
    htmltools::tags$h5(class = "info-titolo", titolo),
    ...
  )
}

#' Elenco cronologico dei cambi di servizio o di utenza
#' @noRd
elenco_cronologia <- function(cronologia, etichetta_attuale) {
  tags <- htmltools::tags
  voci <- purrr::pmap(
    list(
      formatta_data_ora(cronologia$data_ora_lettura, "%d/%m/%Y %H:%M"),
      cronologia$valore,
      cronologia$attuale
    ),
    function(quando, valore, attuale) {
      tags$li(
        class = if (attuale) "attuale",
        tags$span(class = "cronologia-data", quando),
        " \u2192 ",
        tags$b(valore),
        if (attuale) paste0(" ", etichetta_attuale)
      )
    }
  )
  tags$ul(class = "info-cronologia", voci)
}

#' Barre della stima del servizio di un non censito
#' @noRd
barre_stima <- function(stima) {
  tags <- htmltools::tags
  righe <- purrr::pmap(
    list(stima$servizio, stima$pct),
    function(servizio, pct) {
      tags$div(
        class = "stima-riga",
        tags$span(class = "stima-servizio", paste0(servizio, ":")),
        tags$span(class = "stima-pct", sprintf("%.0f%%", pct)),
        tags$span(
          class = "stima-barra",
          tags$span(
            class = "stima-riempimento",
            style = sprintf("width: %.0f%%;", pct)
          )
        )
      )
    }
  )
  tags$div(class = "info-stima", righe)
}

#' Dettaglio di un bidone secondo il caso di `analizza_rfid()`
#' @noRd
blocco_bidone <- function(analisi) {
  tags <- htmltools::tags
  switch(
    analisi$caso,
    censito_stabile = blocco_info(
      "ok",
      "\u2713 TRANSPONDER CENSITO",
      tags$p(
        tags$b("Tipo di Rifiuto: "),
        tidyr::replace_na(analisi$servizio_attuale, "N/D")
      ),
      tags$p(
        if (is.na(analisi$servizio_attuale)) {
          "Questo bidone \u00e8 censito nel database aziendale, ma senza una tipologia di rifiuto associata."
        } else {
          sprintf(
            "Questo bidone \u00e8 regolarmente censito nel database aziendale come contenitore di %s.",
            analisi$servizio_attuale
          )
        }
      )
    ),
    cambio_servizio = blocco_info(
      "attenzione",
      "\u26a0\ufe0f CAMBIO DI SERVIZIO RILEVATO",
      tags$p("Questo bidone ha cambiato tipologia nel tempo:"),
      elenco_cronologia(analisi$cronologia_servizio, "(ATTUALE)"),
      tags$p(tags$b("Ultimo Servizio: "), analisi$servizio_attuale)
    ),
    non_censito_con_stima = blocco_info(
      "errore",
      "\u274c NON CENSITO NEL DATABASE",
      tags$p(tags$b("Stima Predittiva del servizio:")),
      barre_stima(analisi$stima),
      tags$p(
        class = "info-avvertenza",
        "Nota: la stima viene dal servizio atteso dei giri che lo hanno letto. Potrebbe non corrispondere alla realt\u00e0."
      )
    ),
    non_censito_senza_stima = blocco_info(
      "errore",
      "\u274c NON CENSITO NEL DATABASE",
      tags$p(
        "Non sono disponibili informazioni per stimare la tipologia di rifiuto di questo bidone."
      )
    )
  )
}

#' Dettaglio di un sacchetto: censito, o i giri che lo hanno letto
#' @noRd
blocco_sacchetto <- function(analisi) {
  tags <- htmltools::tags
  riconoscimento <- tags$p(
    class = "info-avvertenza",
    "Riconosciuto dal codice RFID. I sacchetti non hanno una tipologia di rifiuto."
  )
  if (analisi$censito) {
    return(blocco_info(
      "ok",
      "\u2713 SACCHETTO CENSITO",
      tags$p("Questo sacchetto \u00e8 censito nel database aziendale."),
      riconoscimento
    ))
  }
  blocco_info(
    "errore",
    "\u274c SACCHETTO NON CENSITO NEL DATABASE",
    if (nrow(analisi$stima) > 0) {
      htmltools::tagList(
        tags$p(tags$b("Servizio dei giri che lo hanno letto:")),
        barre_stima(analisi$stima)
      )
    },
    riconoscimento
  )
}

#' Pannello di dettaglio: blocco del bidone o del sacchetto, incongruenze, cambi di utenza
#' @param analisi Risultato di `analizza_rfid()`.
#' @noRd
crea_info_panel <- function(analisi) {
  tags <- htmltools::tags

  # Un sacchetto non ha una tipologia di rifiuto: ha un blocco suo.
  di_sacchetto <- isTRUE(analisi$sacchetto) && analisi$caso != "cambio_servizio"
  corpo <- if (di_sacchetto) {
    blocco_sacchetto(analisi)
  } else {
    blocco_bidone(analisi)
  }

  utenza <- if (analisi$cambio_utenza) {
    blocco_info(
      "informazione",
      "\u2139\ufe0f CAMBIO DI PROPRIETARIO",
      tags$p("L'utenza associata \u00e8 cambiata:"),
      elenco_cronologia(analisi$cronologia_utenza, "[ATTUALE]")
    )
  }

  tags$div(
    class = "info-rfid",
    tags$div(class = "info-codice", analisi$rfid),
    tags$div(
      class = "info-letture",
      if (analisi$n_letture == 1) {
        sprintf(
          "1 lettura \u00b7 %s",
          formatta_data_ora(analisi$ultima_lettura, "%d/%m/%Y %H:%M")
        )
      } else {
        sprintf(
          "%s letture \u00b7 dal %s al %s",
          formatta_numero(analisi$n_letture),
          formatta_data_ora(analisi$prima_lettura, "%d/%m/%Y"),
          formatta_data_ora(analisi$ultima_lettura, "%d/%m/%Y")
        )
      }
    ),
    corpo,
    blocco_coerenza_comune(analisi$coerenza_comune, analisi$n_letture),
    blocco_coerenza(analisi$coerenza, analisi$n_letture),
    utenza
  )
}

#' Blocchi del dettaglio sulle letture fuori comune e fuori giro
#' @param coerenza Risultato di `coerenza_comune_rfid()`.
#' @noRd
blocco_coerenza_comune <- function(coerenza, n_letture) {
  tags <- htmltools::tags
  if (is.null(coerenza)) {
    return(NULL)
  }
  quante <- function(n) {
    sprintf(
      "%s su %s",
      conta(n, "lettura", "letture"),
      formatta_numero(n_letture)
    )
  }
  elenco <- function(comuni) {
    paste(utils::head(comuni, 3), collapse = ", ")
  }
  blocchi <- list()
  fuori_comune <- coerenza$fuori_comune
  if (isTRUE(fuori_comune$n > 0)) {
    blocchi$comune <- blocco_info(
      "attenzione",
      "\u26a0\ufe0f LETTO FUORI DAL SUO COMUNE",
      tags$p(
        "A database il contenitore \u00e8 a ",
        tags$b(elenco(fuori_comune$dichiarati)),
        ". ",
        quante(fuori_comune$n),
        " in un altro comune: ",
        tags$b(elenco(fuori_comune$letti)),
        "."
      ),
      tags$p(class = "info-nota", "Probabile contenitore spostato.")
    )
  }
  fuori_giro <- coerenza$fuori_giro
  if (isTRUE(fuori_giro$n > 0)) {
    righe <- fuori_giro$letture
    blocchi$giro <- blocco_info(
      "attenzione",
      "\u26a0\ufe0f LETTO FUORI DAL GIRO DEL MEZZO",
      tags$p(
        quante(fuori_giro$n),
        " in un comune diverso da quello del giro. Le pi\u00f9 recenti:"
      ),
      tags$ul(
        class = "info-cronologia",
        lapply(seq_len(nrow(righe)), function(i) {
          letto <- righe$comune_lettura[i]
          tags$li(sprintf(
            "%s \u00b7 %s \u00b7 giro a %s, letto %s",
            formatta_data_ora(righe$data_ora_lettura[i], "%d/%m/%Y"),
            righe$matricola_veicolo[i],
            righe$comune_giro[i],
            if (is.na(letto)) "in un comune sconosciuto" else paste("a", letto)
          ))
        })
      ),
      tags$p(
        class = "info-nota",
        "Il giro di un mezzo in un giorno si vede nella scheda Ricerca Giro."
      )
    )
  }
  unname(blocchi)
}

#' Blocco del dettaglio sulle letture non coerenti, o `NULL`
#' @param coerenza Risultato di `coerenza_rfid()`.
#' @noRd
blocco_coerenza <- function(coerenza, n_letture) {
  tags <- htmltools::tags
  if (is.null(coerenza) || !isTRUE(coerenza$n > 0)) {
    return(NULL)
  }
  righe <- coerenza$letture
  blocco_info(
    "attenzione",
    "\u26a0\ufe0f SERVIZI NON COERENTI",
    tags$p(
      sprintf(
        "%s su %s",
        conta(coerenza$n, "lettura", "letture"),
        formatta_numero(n_letture)
      ),
      " con il servizio a database diverso da quello atteso dal giro.",
      " Le pi\u00f9 recenti:"
    ),
    tags$ul(
      class = "info-cronologia",
      lapply(seq_len(nrow(righe)), function(i) {
        tags$li(sprintf(
          "%s \u00b7 %s \u00b7 %s",
          formatta_data_ora(righe$data_ora_lettura[i], "%d/%m/%Y"),
          righe$matricola_veicolo[i],
          descrivi_servizi(
            database = righe[["servizio_transponder"]][i],
            atteso = righe[["servizio_atteso"]][i],
            giro = righe[["servizio_giro"]][i]
          )
        ))
      })
    ),
    tags$p(
      class = "info-nota",
      "Il contenitore \u00e8 stato letto in un giro di un altro rifiuto,",
      "oppure il mezzo non stava facendo il giro del calendario."
    )
  )
}

#' Righe "servizio: conteggio" del riquadro statistiche
#' @noRd
righe_servizi <- function(conteggi) {
  tags <- htmltools::tags
  if (length(conteggi) == 0) {
    return(tags$div(class = "stat-riga stat-vuoto", "nessuno"))
  }
  purrr::pmap(
    list(names(conteggi), conteggi, info_servizio(names(conteggi))$emoji),
    function(servizio, n, emoji) {
      tags$div(
        class = "stat-riga",
        tags$span(paste0(emoji, " ", servizio, ":")),
        tags$b(formatta_numero(n))
      )
    }
  )
}

#' Righe "cantiere: RFID e letture" del riquadro statistiche, o `NULL`
#' @noRd
righe_cantieri <- function(rfid, letture = NULL) {
  if (length(rfid) == 0) {
    return(NULL)
  }
  tags <- htmltools::tags
  con_letture <- length(letture) > 0
  righe <- purrr::map(names(rfid), function(cantiere) {
    tags$div(
      class = "stat-riga",
      tags$span(paste0(cantiere, ":")),
      tags$b(formatta_numero(rfid[[cantiere]])),
      if (con_letture) {
        tags$span(
          class = "stat-letture",
          paste0(
            "\u00b7 ",
            formatta_numero(dplyr::coalesce(letture[cantiere], 0L)),
            " letture"
          )
        )
      }
    )
  })
  htmltools::tagList(
    tags$div(
      class = "stat-gruppo",
      if (con_letture) "RFID e letture per Cantiere:" else "RFID per Cantiere:"
    ),
    righe
  )
}

#' Riquadro delle statistiche, da `calcola_statistiche()`
#'
#' I non censiti stanno sotto il servizio stimato o, in fondo, la stima
#' incerta.
#' @noRd
crea_box_statistiche <- function(stat) {
  tags <- htmltools::tags
  tags$div(
    class = "stat-box",
    tags$div(class = "stat-titolo", "\U0001F4CA STATISTICHE FILTRATE"),
    tags$div(
      class = "stat-totale",
      tags$span("TOTALE RFID (Ultimo):"),
      tags$b(formatta_numero(stat$totale))
    ),
    righe_cantieri(stat$rfid_cantieri, stat$letture_cantieri),
    tags$div(class = "stat-gruppo", "Presente a Database:"),
    tags$div(
      class = "stat-riga",
      tags$span("\u2713 Presente:"),
      tags$b(formatta_numero(stat$presente))
    ),
    tags$div(
      class = "stat-riga",
      tags$span("\u2717 Non Presente:"),
      tags$b(formatta_numero(stat$non_presente))
    ),
    tags$div(class = "stat-gruppo", "Servizio a database, censiti:"),
    righe_servizi(stat$servizio_censiti),
    tags$div(class = "stat-gruppo", "Servizio stimato, non censiti:"),
    # Senza servizi stimati resta "nessuno" solo se mancano anche gli incerti.
    if (length(stat$servizio_stimato) > 0 || stat$stima_incerta == 0) {
      righe_servizi(stat$servizio_stimato)
    },
    if (stat$stima_incerta > 0) {
      tags$div(
        class = "stat-riga",
        tags$span("\u2753 Stima incerta:"),
        tags$b(formatta_numero(stat$stima_incerta))
      )
    },
    tags$div(class = "stat-gruppo", "Giro del calendario:"),
    righe_servizi(stat$giri),
    # Servizio a database e servizio atteso dal giro dell'ultima lettura.
    tags$div(class = "stat-gruppo", "Coerenza dei servizi:"),
    tags$div(
      class = "stat-riga",
      tags$span("\u26a0 Non coerenti:"),
      tags$b(formatta_numero(stat$non_coerenti %||% 0))
    ),
    # Comune dell'ultima lettura a confronto con quello dichiarato.
    tags$div(class = "stat-gruppo", "Coerenza del comune:"),
    tags$div(
      class = "stat-riga",
      tags$span("\u26a0 Fuori comune:"),
      tags$b(formatta_numero(stat$fuori_comune %||% 0))
    ),
    tags$div(
      class = "stat-riga",
      tags$span("\u26a0 Fuori giro:"),
      tags$b(formatta_numero(stat$fuori_giro %||% 0))
    ),
    tags$div(
      class = "stat-riga",
      tags$span("\u2753 Coerenza ignota:"),
      tags$b(formatta_numero(stat$coerenza_comune_ignota %||% 0))
    )
  )
}
