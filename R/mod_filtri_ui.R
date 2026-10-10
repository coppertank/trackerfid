#' Modulo filtri laterali: interfaccia
#'
#' Filtri per cantiere, comune, stato a database e servizio, pulsante di reset
#' e riquadro con le statistiche del dataset filtrato. Il periodo si sceglie
#' nel modulo dedicato.
#'
#' L'elenco dei comuni permette più scelte. Il foglio di stile gli dà lo
#' stesso aspetto dell'elenco dell'anno di analisi: vedi `elenco-comuni`.
#'
#' I servizi stanno in due elenchi, uno per stato a database: il servizio
#' transponder per i bidoni "Presente", il servizio stimato dai giri per i
#' bidoni "Non Presente". Ogni elenco ha le scelte rapide "Tutti" e "Nessuno".
#'
#' @param id Identificativo del modulo.
#' @noRd
mod_filtri_ui <- function(id) {
  ns <- NS(id)
  tagList(
    sezione_sidebar(
      "Filtri",
      "filter",
      conditionalPanel(
        "!output.caricato",
        ns = ns,
        p(class = "suggerimento", "Carica un file CSV per attivare i filtri.")
      ),
      conditionalPanel(
        "output.caricato",
        ns = ns,
        # I filtri geografici compaiono solo se il dataset ha cantieri.
        conditionalPanel(
          "output.con_geografia",
          ns = ns,
          checkboxGroupInput(
            ns("cantiere_check"),
            "Filtro Cantiere",
            choices = character(0),
            inline = FALSE
          ),
          # Più comuni, anche di cantieri diversi. Nessuna scelta: tutti.
          div(
            class = "elenco-comuni",
            selectizeInput(
              ns("comuni"),
              "Filtro Comune",
              choices = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                placeholder = "Tutti i comuni",
                plugins = list("remove_button")
              )
            )
          )
        ),
        checkboxGroupInput(
          ns("presente_filter"),
          "Filtro Stato Database",
          choices = stati_database(),
          selected = stati_database(),
          inline = FALSE
        ),
        div(
          class = "gruppo-servizi",
          intestazione_filtro(
            ns,
            "transponder",
            "Filtro Servizio Transponder",
            "Bidoni \u00abPresente\u00bb a database"
          ),
          checkboxGroupInput(
            ns("servizio_transponder_check"),
            NULL,
            choices = character(0),
            inline = FALSE
          )
        ),
        div(
          class = "gruppo-servizi",
          intestazione_filtro(
            ns,
            "atteso",
            "Filtro Servizio Atteso",
            "Bidoni \u00abNon Presente\u00bb: servizio stimato dai giri"
          ),
          checkboxGroupInput(
            ns("servizio_atteso_check"),
            NULL,
            choices = character(0),
            inline = FALSE
          ),
          # I non censiti che sulla mappa hanno il punto di domanda.
          checkboxInput(
            ns("includi_stima_incerta"),
            "Includi Non Censiti con stima incerta (?)",
            value = TRUE
          )
        ),
        actionButton(
          ns("reset"),
          "\U0001F504 Reset Filtri",
          class = "btn-block"
        )
      )
    ),
    conditionalPanel(
      "output.caricato",
      ns = ns,
      sezione_sidebar("Statistiche", "chart-simple", uiOutput(ns("stat_box")))
    )
  )
}

#' Titolo di un elenco di servizi, con le scelte rapide
#'
#' A destra del titolo stanno "Tutti" e "Nessuno", per spuntare o togliere in
#' un colpo tutte le voci dell'elenco. Sotto, una riga dice a quali bidoni si
#' applica l'elenco.
#'
#' @param ns Funzione dello spazio dei nomi del modulo.
#' @param gruppo Prefisso degli identificativi delle due scelte rapide:
#'   `<gruppo>_tutti` e `<gruppo>_nessuno`.
#' @param titolo Titolo dell'elenco.
#' @param nota Bidoni a cui l'elenco si applica.
#' @noRd
intestazione_filtro <- function(ns, gruppo, titolo, nota) {
  div(
    class = "intestazione-filtro",
    div(
      class = "titolo-filtro",
      span(class = "control-label", titolo),
      span(
        class = "azioni-filtro",
        actionLink(
          ns(paste0(gruppo, "_tutti")),
          "Tutti",
          title = "Seleziona tutte le voci"
        ),
        actionLink(
          ns(paste0(gruppo, "_nessuno")),
          "Nessuno",
          title = "Deseleziona tutte le voci"
        )
      )
    ),
    p(class = "nota-filtro", nota)
  )
}
