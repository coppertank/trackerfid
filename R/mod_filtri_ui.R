#' Filtri della mappa principale: una riga di menu a discesa sopra la mappa
#'
#' Cantiere, comune, coerenza del comune, stato a database, servizio, giro,
#' coerenza del servizio, reset. Le statistiche sono in
#' `mod_filtri_sidebar_ui()`.
#' @noRd
mod_filtri_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "barra-filtri",
    conditionalPanel(
      "!output.caricato",
      ns = ns,
      p(class = "suggerimento", "Carica un file CSV per attivare i filtri.")
    ),
    conditionalPanel(
      "output.caricato",
      ns = ns,
      class = "gruppo-filtri",
      # I filtri geografici compaiono solo se il dataset ha cantieri.
      conditionalPanel(
        "output.con_geografia",
        ns = ns,
        class = "gruppo-filtri",
        menu_filtro(
          "Cantiere",
          "city",
          checkboxGroupInput(
            ns("cantiere_check"),
            NULL,
            choices = character(0)
          )
        ),
        # Più comuni, anche di cantieri diversi. Nessuna scelta: tutti.
        div(
          class = "elenco-comuni filtro-comuni",
          selectizeInput(
            ns("comuni"),
            NULL,
            choices = NULL,
            multiple = TRUE,
            width = "100%",
            options = list(
              placeholder = "Tutti i comuni",
              plugins = list("remove_button")
            )
          )
        ),
        # Comune di lettura a confronto con il comune dichiarato.
        menu_filtro(
          "Coerenza comune",
          "location-dot",
          checkboxGroupInput(
            ns("coerenza_comune_check"),
            NULL,
            choiceNames = unname(stati_coerenza_comune()),
            choiceValues = names(stati_coerenza_comune()),
            selected = names(stati_coerenza_comune())
          ),
          p(
            class = "nota-filtro",
            "Fuori comune: censito letto in un comune diverso da quello del",
            "database. Fuori giro: non censito letto in un comune diverso da",
            "quello del giro."
          )
        )
      ),
      menu_filtro(
        "Stato database",
        "database",
        checkboxGroupInput(
          ns("presente_filter"),
          NULL,
          choices = stati_database(),
          selected = stati_database()
        )
      ),
      menu_filtro(
        "Servizio",
        "trash-can",
        intestazione_filtro(
          ns,
          "servizio",
          "Servizio del contenitore",
          "Censiti: servizio a database. Non censiti: servizio stimato dai giri"
        ),
        checkboxGroupInput(
          ns("servizio_check"),
          NULL,
          choices = character(0)
        ),
        # I non censiti che sulla mappa hanno il punto di domanda.
        checkboxInput(
          ns("includi_stima_incerta"),
          "Non censiti con stima incerta (?)",
          value = TRUE
        )
      ),
      menu_filtro(
        "Giro",
        "truck",
        intestazione_filtro(
          ns,
          "giro",
          "Giro del calendario",
          "Con il nome del calendario dei mezzi"
        ),
        checkboxGroupInput(
          ns("servizio_giro_check"),
          NULL,
          choices = character(0)
        )
      ),
      # Servizio a database e servizio atteso dal giro a confronto.
      menu_filtro(
        "Coerenza servizio",
        "triangle-exclamation",
        checkboxGroupInput(
          ns("coerenza_check"),
          NULL,
          choiceNames = unname(etichette_coerenza()),
          choiceValues = names(stati_coerenza()),
          selected = names(stati_coerenza())
        ),
        p(
          class = "nota-filtro",
          "Non coerente: il servizio a database e quello atteso dal giro non",
          "sono dello stesso rifiuto."
        )
      ),
      actionButton(
        ns("reset"),
        "Reset",
        icon = icon("rotate-left"),
        class = "filtro-reset",
        title = "Riporta tutti i filtri ai valori iniziali"
      )
    )
  )
}

#' Statistiche della mappa principale, nella barra laterale (stesso id dei filtri)
#' @noRd
mod_filtri_sidebar_ui <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    "output.caricato",
    ns = ns,
    sezione_sidebar("Statistiche", "chart-simple", uiOutput(ns("stat_box")))
  )
}

#' Menu a discesa di un filtro: resta aperto mentre si spuntano le voci
#'
#' Il numero delle voci rimaste sul pulsante lo scrive `script.js`.
#' @noRd
menu_filtro <- function(etichetta, icona, ...) {
  div(
    class = "dropdown filtro-mappa",
    tags$button(
      type = "button",
      class = "btn btn-default dropdown-toggle",
      `data-toggle` = "dropdown",
      `aria-haspopup` = "true",
      `aria-expanded` = "false",
      icon(icona),
      span(class = "filtro-etichetta", etichetta),
      span(class = "filtro-conteggio"),
      span(class = "caret")
    ),
    div(class = "dropdown-menu filtro-menu", ...)
  )
}

#' Titolo di un elenco di servizi, con "Tutti", "Nessuno" e una nota
#' @param gruppo Prefisso di `<gruppo>_tutti` e `<gruppo>_nessuno`.
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
