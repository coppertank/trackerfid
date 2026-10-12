#' Modulo filtri laterali: interfaccia
#'
#' Filtri per cantiere, comune, stato a database e servizio, pulsante di reset
#' e riquadro con le statistiche del dataset filtrato. Il periodo si sceglie
#' nel modulo dedicato.
#'
#' L'elenco dei comuni permette più scelte. Il foglio di stile gli dà lo
#' stesso aspetto dell'elenco dell'anno di analisi: vedi `elenco-comuni`.
#'
#' I servizi stanno in due elenchi, che valgono per i bidoni di tutti e due
#' gli stati a database: il servizio atteso, cioè quello che il mezzo
#' raccoglieva, e il giro del calendario, con il suo nome. Ogni elenco ha le
#' scelte rapide "Tutti" e "Nessuno". Un terzo filtro riguarda la coerenza
#' tra i servizi di una lettura. Lo stato a database si sceglie a parte.
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
          ),
          # Posizione delle letture a confronto con i comuni dichiarati.
          checkboxGroupInput(
            ns("coerenza_check"),
            "Filtro Posizione",
            choiceNames = unname(stati_coerenza()),
            choiceValues = names(stati_coerenza()),
            selected = names(stati_coerenza()),
            inline = FALSE
          ),
          p(
            class = "nota-filtro",
            "Fuori comune: censito letto lontano dal comune del database.",
            "Fuori giro: lettura lontana dal comune del giro del mezzo."
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
            "atteso",
            "Filtro Servizio Atteso",
            "Servizio che il mezzo raccoglieva. Non censiti: servizio stimato"
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
        div(
          class = "gruppo-servizi",
          intestazione_filtro(
            ns,
            "giro",
            "Filtro Servizio Giro",
            "Giro del calendario dei mezzi, con il suo nome"
          ),
          checkboxGroupInput(
            ns("servizio_giro_check"),
            NULL,
            choices = character(0),
            inline = FALSE
          )
        ),
        # Servizio del contenitore, servizio atteso e giro a confronto.
        checkboxGroupInput(
          ns("servizio_coerenza_check"),
          "Filtro Coerenza Servizio",
          choiceNames = unname(stati_coerenza_servizio()),
          choiceValues = names(stati_coerenza_servizio()),
          selected = names(stati_coerenza_servizio()),
          inline = FALSE
        ),
        p(
          class = "nota-filtro",
          "Non coerente: contenitore, servizio atteso e giro non sono dello",
          "stesso rifiuto."
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
#' un colpo tutte le voci dell'elenco. Sotto, una riga dice che cosa riporta
#' l'elenco.
#'
#' @param ns Funzione dello spazio dei nomi del modulo.
#' @param gruppo Prefisso degli identificativi delle due scelte rapide:
#'   `<gruppo>_tutti` e `<gruppo>_nessuno`.
#' @param titolo Titolo dell'elenco.
#' @param nota Che cosa riporta l'elenco.
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
