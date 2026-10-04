# Creazione di popup, pannello di dettaglio e riquadro delle statistiche.

#' Formatta data e ora nel formato italiano
#' @noRd
formatta_data_ora <- function(x, formato = "%d/%m/%Y %H:%M:%S") {
  format(x, formato, tz = "UTC")
}

#' Formatta un conteggio con il separatore delle migliaia italiano
#' @noRd
formatta_numero <- function(x) {
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE)
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
#' @return HTML string
#' @noRd
create_popup_html <- function(rfid, servizio, servizio_atteso,
                              targa, matricola, giorno_lettura,
                              id_utenza, presente) {
  testo <- function(x, se_mancante = "N/A") {
    htmltools::htmlEscape(tidyr::replace_na(as.character(x), se_mancante))
  }
  data_formattata <- formatta_data_ora(giorno_lettura)

  paste0(
    "<div style='font-family: Arial; font-size: 12px;'>",
    "<b>RFID:</b> ", testo(rfid), "<br>",
    "<b>Servizio:</b> ", testo(servizio), "<br>",
    "<b>Servizio Atteso:</b> ", testo(servizio_atteso), "<br>",
    "<b>Targa:</b> ", testo(targa), "<br>",
    "<b>Matricola:</b> ", testo(matricola), "<br>",
    "<b>Lettura:</b> ", data_formattata, "<br>",
    "<b>Utenza:</b> ", testo(id_utenza, "Non Censito"), "<br>",
    "<b>Censito:</b> ", testo(presente), "<br>",
    "</div>"
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
          tags$span(class = "stima-riempimento", style = sprintf("width: %.0f%%;", pct))
        )
      )
    }
  )
  tags$div(class = "info-stima", righe)
}

#' Pannello di dettaglio di un bidone
#'
#' Sceglie il messaggio da mostrare in base alla storia dell'RFID: censito con
#' servizio coerente, censito con cambio di servizio, non censito con o senza
#' stima. In più segnala l'eventuale cambio di utenza.
#'
#' @param analisi Risultato di `analizza_rfid()`.
#' @return Tag HTML.
#' @noRd
crea_info_panel <- function(analisi) {
  tags <- htmltools::tags

  corpo <- switch(analisi$caso,
    censito_coerente = blocco_info(
      "ok", "\u2713 TRANSPONDER CENSITO",
      tags$p(tags$b("Tipo di Rifiuto: "), tidyr::replace_na(analisi$servizio_attuale, "N/D")),
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
      "attenzione", "\u26a0\ufe0f CAMBIO DI SERVIZIO RILEVATO",
      tags$p("Questo bidone ha cambiato tipologia nel tempo:"),
      elenco_cronologia(analisi$cronologia_servizio, "(ATTUALE)"),
      tags$p(tags$b("Ultimo Servizio: "), analisi$servizio_attuale)
    ),
    non_censito_con_stima = blocco_info(
      "errore", "\u274c NON CENSITO NEL DATABASE",
      tags$p(tags$b("Stima Predittiva (da Calendario Mezzi):")),
      barre_stima(analisi$stima),
      tags$p(
        class = "info-avvertenza",
        "Nota: questa \u00e8 una stima basata sul calendario dei mezzi e potrebbe non corrispondere alla realt\u00e0."
      )
    ),
    non_censito_senza_stima = blocco_info(
      "errore", "\u274c NON CENSITO NEL DATABASE",
      tags$p("Non sono disponibili informazioni per stimare la tipologia di rifiuto di questo bidone.")
    )
  )

  utenza <- if (analisi$cambio_utenza) {
    blocco_info(
      "informazione", "\u2139\ufe0f CAMBIO DI PROPRIETARIO",
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
        sprintf("1 lettura \u00b7 %s", formatta_data_ora(analisi$ultima_lettura, "%d/%m/%Y %H:%M"))
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
    utenza
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

#' Riquadro con le statistiche del dataset filtrato
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
    tags$div(class = "stat-gruppo", "Presente a Database:"),
    tags$div(
      class = "stat-riga",
      tags$span("\u2713 Presente:"), tags$b(formatta_numero(stat$presente))
    ),
    tags$div(
      class = "stat-riga",
      tags$span("\u2717 Non Presente:"), tags$b(formatta_numero(stat$non_presente))
    ),
    tags$div(class = "stat-gruppo", "Servizio Transponder:"),
    righe_servizi(stat$transponder),
    tags$div(class = "stat-gruppo", "Servizio Atteso:"),
    righe_servizi(stat$atteso),
    if (stat$senza_stima > 0) {
      tags$div(
        class = "stat-riga",
        tags$span("\u2753 Senza stima:"), tags$b(formatta_numero(stat$senza_stima))
      )
    }
  )
}
