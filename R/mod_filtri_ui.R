#' Modulo filtri della mappa principale: barra sopra la mappa
#'
#' Una riga di menu a discesa sopra la mappa principale: cantiere, comune,
#' coerenza del comune, stato a database, servizio, giro e coerenza dei
#' servizi, più il pulsante di reset. Ogni menu contiene le caselle del suo filtro; il comune
#' si sceglie da un elenco a scelta multipla, che è già un menu a discesa. Le
#' statistiche stanno nella barra laterale, vedi `mod_filtri_sidebar_ui()`.
#'
#' I servizi stanno in due menu, che valgono per i bidoni di tutti e due gli
#' stati a database: il servizio del contenitore, cioè quello a database per
#' un censito e quello stimato per un non censito, e il giro del calendario,
#' con il suo nome. Ogni menu ha le scelte rapide "Tutti" e "Nessuno". Un
#' terzo menu riguarda la coerenza tra servizio a database e servizio atteso
#' dal giro. Lo stato a database si sceglie a parte.
#'
#' Il filtro sulla coerenza del comune, con le voci di
#' `stati_coerenza_comune()`, è un'altra cosa: riguarda il comune di lettura a
#' confronto con quello dichiarato.
#'
#' @param id Identificativo del modulo.
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

#' Modulo filtri della mappa principale: statistiche nella barra laterale
#'
#' @param id Identificativo del modulo: lo stesso di `mod_filtri_ui()`.
#' @noRd
mod_filtri_sidebar_ui <- function(id) {
  ns <- NS(id)
  conditionalPanel(
    "output.caricato",
    ns = ns,
    sezione_sidebar("Statistiche", "chart-simple", uiOutput(ns("stat_box")))
  )
}

#' Menu a discesa di un filtro della barra sopra la mappa
#'
#' Un pulsante con il nome del filtro apre un riquadro con le sue caselle. Il
#' riquadro resta aperto mentre si spuntano le voci, e si chiude con un click
#' fuori. Quando qualche voce è tolta il pulsante lo segnala, con il numero
#' delle voci rimaste: lo scrive `script.js`.
#'
#' @param etichetta Nome del filtro, sul pulsante.
#' @param icona Nome dell'icona Font Awesome.
#' @param ... Contenuto del riquadro.
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
