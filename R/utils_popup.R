# Creazione di popup, pannello di dettaglio e riquadro delle statistiche.

#' Formatta data e ora nel formato italiano
#' @noRd
formatta_data_ora <- function(x, formato = "%d/%m/%Y %H:%M:%S") {
  format(x, formato, tz = "UTC")
}

#' Formatta un conteggio con il separatore delle migliaia italiano
#'
#' Mai in notazione scientifica: centomila resta "100.000", non "1e+05".
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

#' Crea HTML popup per marker
#'
#' La funzione è vettoriale: restituisce un popup per ogni lettura. I valori
#' provenienti dal file vengono sempre sottoposti a escape HTML.
#'
#' @param rfid Character
#' @param servizio Character
#' @param servizio_atteso Character
#' @param targa Character
#' @param matricola Character
#' @param giorno_lettura POSIXct
#' @param id_utenza Character
#' @param presente Character
#' @param comune Character, facoltativo: comune del database.
#' @param comune_lettura Character, facoltativo: comune in cui cade il punto
#'   della lettura, oppure `fuori_servizio()`.
#' @param comune_giro Character, facoltativo: comune in cui il calendario
#'   mandava il mezzo. Compare solo se è diverso dal comune di lettura.
#' @param cantiere Character, facoltativo.
#' @return HTML string
#' @noRd
create_popup_html <- function(
  rfid,
  servizio,
  servizio_atteso,
  targa,
  matricola,
  giorno_lettura,
  id_utenza,
  presente,
  comune = NULL,
  comune_lettura = NULL,
  comune_giro = NULL,
  cantiere = NULL,
  comune_geografico = NULL,
  fuori_comune = NULL,
  fuori_giro = NULL,
  servizio_giro = NULL,
  coerenza = NULL
) {
  testo <- function(x, se_mancante = "N/A") {
    htmltools::htmlEscape(tidyr::replace_na(as.character(x), se_mancante))
  }
  data_formattata <- formatta_data_ora(giorno_lettura)
  # Le righe geografiche compaiono solo nei popup delle letture che hanno il dato.
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
  # Le incongruenze tra la posizione e i comuni dichiarati hanno una riga loro,
  # che dice dove sta il punto.
  riga_incongruenza <- function(etichetta, attiva, dichiarato) {
    n <- length(rfid)
    if (is.null(attiva)) {
      return(rep("", n))
    }
    dove <- if (is.null(comune_geografico)) {
      rep(NA_character_, n)
    } else {
      as.character(comune_geografico)
    }
    ifelse(
      attiva %in% TRUE,
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
          "fuori dai comuni serviti",
          paste("a", testo(dove))
        ),
        "<br>"
      ),
      ""
    )
  }
  dichiarato_da <- function(etichetta) {
    valori <- if (etichetta == "Fuori comune") comune else comune_giro
    if (is.null(valori)) NA_character_ else valori
  }
  riga_fuori_comune <- riga_incongruenza("Fuori comune", fuori_comune, "a database")
  riga_fuori_giro <- riga_incongruenza("Fuori giro", fuori_giro, "giro a")
  riga_giro <- riga_facoltativa("Servizio Giro", servizio_giro)
  # I servizi che non concordano hanno una riga loro, che li riporta tutti.
  riga_coerenza <- if (is.null(coerenza)) {
    ""
  } else {
    ifelse(
      coerenza %in% stati_coerenza_servizio()[["non_coerente"]],
      paste0(
        "<b class='popup-incongruenza'>\u26a0 Servizi non coerenti:</b> ",
        htmltools::htmlEscape(
          descrivi_servizi(servizio, servizio_atteso, servizio_giro)
        ),
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
    "<b>Servizio:</b> ",
    testo(servizio),
    "<br>",
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
    testo(id_utenza, "Non Censito"),
    "<br>",
    riga_comune,
    riga_lettura,
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

#' Elenco dei servizi di una lettura, per popup e dettaglio
#'
#' @param contenitore,atteso,giro Servizio del contenitore a database,
#'   servizio atteso e giro del calendario. Uno che manca non viene scritto.
#' @return Un testo per lettura, ad esempio
#'   `"contenitore CARTA, atteso SECCO, giro SECCO PAP"`.
#' @noRd
descrivi_servizi <- function(contenitore, atteso, giro) {
  n <- max(length(contenitore), length(atteso), length(giro))
  parte <- function(etichetta, valori) {
    valori <- rep_len(as.character(valori %||% NA_character_), n)
    ifelse(is.na(valori), NA_character_, paste(etichetta, valori))
  }
  parti <- list(
    parte("contenitore", contenitore),
    parte("atteso", atteso),
    parte("giro", giro)
  )
  testo <- rep("", n)
  for (aggiunta in parti) {
    testo <- ifelse(
      is.na(aggiunta),
      testo,
      ifelse(nzchar(testo), paste0(testo, ", ", aggiunta), aggiunta)
    )
  }
  testo
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

#' Elenco cronologico delle transizioni (servizio o utenza)
#' @noRd
elenco_cronologia <- function(cronologia, etichetta_attuale) {
  tags <- htmltools::tags
  voci <- purrr::pmap(
    list(
      formatta_data_ora(cronologia$giorno_lettura, "%d/%m/%Y %H:%M"),
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

#' Barre con la stima percentuale del servizio di un bidone non censito
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

#' Istogramma delle letture per anno di un bidone
#'
#' Una colonna per anno, alta in proporzione alle letture. Le letture del
#' sistema precedente e quelle delle antenne hanno colori diversi. Un anno
#' senza letture resta vuoto, con lo zero in evidenza.
#'
#' @param andamento Risultato di `andamento_rfid()`.
#' @return Tag HTML, oppure `NULL` se non ci sono anni da mostrare.
#' @noRd
istogramma_annuale <- function(andamento) {
  if (is.null(andamento) || nrow(andamento) == 0) {
    return(NULL)
  }
  tags <- htmltools::tags
  massimo <- max(andamento$storico + andamento$antenne, 1)
  segmento <- function(fonte, letture) {
    if (letture > 0) {
      tags$span(
        class = paste0("anni-segmento anni-", fonte),
        style = sprintf("height: %.1f%%;", 100 * letture / massimo)
      )
    }
  }
  colonne <- purrr::pmap(
    list(andamento$anno, andamento$storico, andamento$antenne),
    function(anno, storico, antenne) {
      totale <- storico + antenne
      tags$div(
        class = "anni-colonna",
        tags$span(
          class = if (totale == 0) "anni-valore anni-zero" else "anni-valore",
          formatta_numero(totale)
        ),
        tags$span(
          class = "anni-barra",
          segmento("antenne", antenne),
          segmento("storico", storico)
        ),
        tags$span(class = "anni-etichetta", anno)
      )
    }
  )
  legenda <- if (any(andamento$storico > 0)) {
    tags$div(
      class = "anni-legenda",
      tags$span(tags$i(class = "anni-storico"), "Sistema precedente"),
      tags$span(tags$i(class = "anni-antenne"), "Antenne")
    )
  }
  blocco_info(
    "neutro",
    "\U0001F4CA LETTURE PER ANNO",
    tags$div(class = "anni-grafico", colonne),
    legenda
  )
}

#' Blocco del pannello di dettaglio per un bidone
#'
#' Sceglie il messaggio in base alla storia dell'RFID: censito con servizio
#' coerente, censito con cambio di servizio, non censito con o senza stima.
#'
#' @param analisi Risultato di `analizza_rfid()`.
#' @noRd
blocco_bidone <- function(analisi) {
  tags <- htmltools::tags
  switch(
    analisi$caso,
    censito_coerente = blocco_info(
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
      tags$p(tags$b("Stima Predittiva (da Calendario Mezzi):")),
      barre_stima(analisi$stima),
      tags$p(
        class = "info-avvertenza",
        "Nota: questa \u00e8 una stima basata sul calendario dei mezzi e potrebbe non corrispondere alla realt\u00e0."
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

#' Blocco del pannello di dettaglio per un sacchetto
#'
#' Un sacchetto si riconosce dal codice RFID e non ha una tipologia di
#' rifiuto. Se è censito il blocco lo conferma. Se non lo è riporta i giri
#' che lo hanno letto, senza presentarli come stima di un servizio.
#'
#' @param analisi Risultato di `analizza_rfid()`.
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
        tags$p(tags$b("Giri che lo hanno letto (da Calendario Mezzi):")),
        barre_stima(analisi$stima)
      )
    },
    riconoscimento
  )
}

#' Pannello di dettaglio di un bidone
#'
#' Sceglie il messaggio da mostrare in base alla storia dell'RFID, vedi
#' `blocco_bidone()`; per un sacchetto, vedi `blocco_sacchetto()`. In più
#' segnala l'eventuale cambio di utenza e, se viene passato l'andamento,
#' mostra l'istogramma delle letture per anno.
#'
#' @param analisi Risultato di `analizza_rfid()`.
#' @param andamento Risultato di `andamento_rfid()`, facoltativo.
#' @return Tag HTML.
#' @noRd
crea_info_panel <- function(analisi, andamento = NULL) {
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
    blocco_posizione(analisi$posizione, analisi$n_letture),
    blocco_coerenza(analisi$coerenza, analisi$n_letture, analisi$censito),
    utenza,
    istogramma_annuale(andamento)
  )
}

#' Blocchi del dettaglio sulle letture fuori dai comuni dichiarati
#'
#' @param posizione Risultato di `posizione_rfid()`.
#' @param n_letture Letture dell'RFID nel periodo.
#' @return Elenco di blocchi, vuoto se tutte le letture sono coerenti.
#' @noRd
blocco_posizione <- function(posizione, n_letture) {
  tags <- htmltools::tags
  if (is.null(posizione)) {
    return(NULL)
  }
  quante <- function(n) {
    sprintf(
      "%s su %s",
      conta(n, "lettura", "letture"),
      formatta_numero(n_letture)
    )
  }
  distanza <- formatta_numero(tolleranza_confine_m() / 1000)
  elenco <- function(comuni) {
    paste(utils::head(comuni, 3), collapse = ", ")
  }
  blocchi <- list()
  fuori_comune <- posizione$fuori_comune
  if (isTRUE(fuori_comune$n > 0)) {
    blocchi$comune <- blocco_info(
      "attenzione",
      "\u26a0\ufe0f LETTO FUORI DAL SUO COMUNE",
      tags$p(
        "A database il contenitore \u00e8 a ",
        tags$b(elenco(fuori_comune$dichiarati)),
        ". ",
        quante(fuori_comune$n),
        sprintf(" a pi\u00f9 di %s km dal confine: ", distanza),
        tags$b(elenco(fuori_comune$letti)),
        "."
      ),
      tags$p(class = "info-nota", "Probabile contenitore spostato.")
    )
  }
  fuori_giro <- posizione$fuori_giro
  if (isTRUE(fuori_giro$n > 0)) {
    righe <- fuori_giro$letture
    blocchi$giro <- blocco_info(
      "attenzione",
      "\u26a0\ufe0f LETTO FUORI DAL GIRO DEL MEZZO",
      tags$p(
        quante(fuori_giro$n),
        sprintf(
          " a pi\u00f9 di %s km dal comune del giro. Le pi\u00f9 recenti:",
          distanza
        )
      ),
      tags$ul(
        class = "info-cronologia",
        lapply(seq_len(nrow(righe)), function(i) {
          letto <- righe$comune_geografico[i]
          tags$li(sprintf(
            "%s \u00b7 %s \u00b7 giro a %s, letto %s",
            formatta_data_ora(righe$giorno_lettura[i], "%d/%m/%Y"),
            righe$matricola_veicolo[i],
            righe$comune_giro[i],
            if (is.na(letto)) "fuori dai comuni serviti" else paste("a", letto)
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

#' Blocco del dettaglio sulle letture con i servizi che non concordano
#'
#' @param coerenza Risultato di `coerenza_rfid()`.
#' @param n_letture Numero delle letture dell'RFID.
#' @param censito Vero se l'RFID è a database: cambia la spiegazione.
#' @return Un blocco, oppure `NULL` se tutte le letture sono coerenti.
#' @noRd
blocco_coerenza <- function(coerenza, n_letture, censito = TRUE) {
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
      " con servizi di rifiuti diversi. Le pi\u00f9 recenti:"
    ),
    tags$ul(
      class = "info-cronologia",
      lapply(seq_len(nrow(righe)), function(i) {
        tags$li(sprintf(
          "%s \u00b7 %s \u00b7 %s",
          formatta_data_ora(righe$giorno_lettura[i], "%d/%m/%Y"),
          righe$matricola_veicolo[i],
          descrivi_servizi(
            righe[["servizio_transponder"]][i],
            righe[["servizio_atteso"]][i],
            righe[["servizio_giro"]][i]
          )
        ))
      })
    ),
    tags$p(
      class = "info-nota",
      if (isTRUE(censito)) {
        "Il contenitore \u00e8 stato letto in un giro di un altro rifiuto."
      } else {
        "Il mezzo raccoglieva un rifiuto diverso da quello del calendario."
      }
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

#' Righe "cantiere: RFID e letture" del riquadro statistiche
#'
#' @param rfid Numero di RFID per cantiere, vedi `conta_cantieri()`.
#' @param letture Numero di letture per cantiere, facoltativo.
#' @return Lista di tag, oppure `NULL` se il dataset non ha cantieri.
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

#' Riquadro con le statistiche del dataset filtrato
#'
#' Il servizio transponder conta i censiti. Il servizio atteso conta i non
#' censiti: una riga per servizio stimato e, in fondo, quelli con la stima
#' incerta, che sulla mappa hanno il punto di domanda.
#'
#' @param stat Risultato di `calcola_statistiche()`.
#' @return Tag HTML.
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
    tags$div(class = "stat-gruppo", "Servizio Atteso, censiti:"),
    righe_servizi(stat$atteso_censiti),
    tags$div(class = "stat-gruppo", "Servizio Atteso, non censiti:"),
    # Senza servizi stimati resta "nessuno" solo se mancano anche gli incerti.
    if (length(stat$atteso) > 0 || stat$stima_incerta == 0) {
      righe_servizi(stat$atteso)
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
    # Servizi dell'ultima lettura: contenitore, servizio atteso e giro.
    tags$div(class = "stat-gruppo", "Coerenza dei servizi:"),
    tags$div(
      class = "stat-riga",
      tags$span("\u26a0 Non coerenti:"),
      tags$b(formatta_numero(stat$non_coerenti %||% 0))
    ),
    # Posizione dell'ultima lettura a confronto con i comuni dichiarati.
    tags$div(class = "stat-gruppo", "Posizione e comuni dichiarati:"),
    tags$div(
      class = "stat-riga",
      tags$span("\u26a0 Fuori comune:"),
      tags$b(formatta_numero(stat$fuori_comune %||% 0))
    ),
    tags$div(
      class = "stat-riga",
      tags$span("\u26a0 Fuori giro:"),
      tags$b(formatta_numero(stat$fuori_giro %||% 0))
    )
  )
}
