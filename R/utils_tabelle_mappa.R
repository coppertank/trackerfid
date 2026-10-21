# Tabelle delle ricerche (reactable) in un riquadro sopra la mappa, in basso
# a sinistra: si chiude con un click sull'intestazione e nasce aperta.

#' Riquadro `details` di una tabella sopra la mappa, con classi diverse dalla legenda
#' @noRd
riquadro_tabella_mappa <- function(titolo, ..., nota = NULL, classe = NULL) {
  tags <- htmltools::tags
  tags$div(
    class = paste(c("tabella-mappa", classe), collapse = " "),
    tags$details(
      open = NA,
      class = "tabella-dettagli",
      tags$summary(
        class = "tabella-intestazione",
        title = "Mostra o nascondi la tabella",
        titolo
      ),
      if (!is.null(nota)) tags$div(class = "tabella-nota", nota),
      ...
    )
  )
}

#' Testi di reactable in italiano
#' @noRd
lingua_reactable <- function(vuota = "Nessuna riga") {
  reactable::reactableLang(
    sortLabel = "Ordina per {name}",
    searchPlaceholder = "Cerca",
    filterPlaceholder = "Filtra",
    filterLabel = "Filtra {name}",
    searchLabel = "Cerca nella tabella",
    noData = vuota,
    pageNext = "Successiva",
    pagePrevious = "Precedente",
    pageInfo = "{rowStart}\u2013{rowEnd} di {rows} righe",
    pageSizeOptions = "Mostra {rows}",
    pageNextLabel = "Pagina successiva",
    pagePreviousLabel = "Pagina precedente",
    pageNumberLabel = "Pagina {page}",
    pageJumpLabel = "Vai alla pagina",
    pageSizeOptionsLabel = "Righe per pagina",
    groupExpandLabel = "Mostra o nascondi le letture",
    selectRowLabel = "Scegli la riga"
  )
}

#' Aspetto compatto delle tabelle sopra le mappe
#' @noRd
tema_tabelle_mappa <- function() {
  reactable::reactableTheme(
    color = "#2c3e50",
    backgroundColor = "transparent",
    borderColor = "#dde3e8",
    stripedColor = "rgba(0, 0, 0, 0.03)",
    highlightColor = "rgba(36, 113, 163, 0.08)",
    cellPadding = "3px 6px",
    style = list(fontSize = "11px"),
    headerStyle = list(color = "#5f6b76", fontWeight = 700),
    rowGroupStyle = list(fontWeight = 600),
    rowSelectedStyle = list(
      backgroundColor = "#eaf2f8",
      boxShadow = "inset 3px 0 0 0 #2471a3"
    ),
    inputStyle = list(fontSize = "11px", padding = "2px 6px"),
    filterInputStyle = list(
      fontSize = "11px",
      padding = "1px 4px",
      height = "22px"
    ),
    selectStyle = list(fontSize = "11px", padding = "0 4px"),
    paginationStyle = list(fontSize = "11px"),
    pageButtonStyle = list(padding = "1px 6px")
  )
}

#' Tabella delle letture degli RFID cercati, raggruppate per RFID
#'
#' La riga di un RFID riassume letture, giorni, mezzi e giri; con un solo
#' RFID il gruppo è aperto.
#' @noRd
tabella_letture_rfid <- function(letture) {
  ordine <- order(
    letture$RFID,
    -as.numeric(letture$giorno_lettura),
    method = "radix"
  )
  letture <- letture[ordine, , drop = FALSE]
  dati <- data.frame(
    RFID = letture$RFID,
    Lettura = formatta_data_ora(letture$giorno_lettura),
    Mezzo = paste(letture$matricola_veicolo, letture$targa_veicolo),
    Giro = servizio_del_giro(letture),
    stringsAsFactors = FALSE
  )
  # Righe dalla più recente: primo e ultimo giorno sono l'ultima e la prima.
  giorni <- reactable::JS(
    paste(
      "function(values) {",
      "  if (values.length === 0) return '';",
      "  var dal = values[values.length - 1].slice(0, 10);",
      "  var al = values[0].slice(0, 10);",
      "  return dal === al ? al : dal + ' ' + String.fromCharCode(8211) + ' ' + al;",
      "}",
      sep = "\n"
    )
  )
  reactable::reactable(
    dati,
    groupBy = "RFID",
    columns = list(
      RFID = reactable::colDef(name = "RFID", minWidth = 170),
      Lettura = reactable::colDef(
        name = "Data e ora",
        aggregate = giorni,
        minWidth = 150
      ),
      Mezzo = reactable::colDef(
        name = "Mezzo",
        aggregate = "frequency",
        minWidth = 130
      ),
      Giro = reactable::colDef(
        name = "Servizio del giro",
        aggregate = "frequency",
        minWidth = 150
      )
    ),
    defaultExpanded = dplyr::n_distinct(dati$RFID) == 1,
    sortable = FALSE,
    paginateSubRows = TRUE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    wrap = FALSE,
    compact = TRUE,
    highlight = TRUE,
    theme = tema_tabelle_mappa(),
    language = lingua_reactable("Nessuna lettura")
  )
}
