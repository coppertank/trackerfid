# Viste dell'analisi dei cluster: tabella interattiva, grafici e mappa.
#
# I colori degli indicatori sono quelli di `indicatori_config()`, sempre nello
# stesso ordine: ogni indicatore mantiene il suo colore in tutte le viste.

# Colori neutri di testo, assi e griglia dei grafici.
inchiostro_grafici <- function() {
  list(
    primario = "#0b0b0b",
    secondario = "#52514e",
    griglia = "#e1e0d9",
    asse = "#c3c2b7",
    contesto = "#d9d8d2"
  )
}

#' Testi italiani della tabella interattiva
#' @noRd
lingua_dt <- function() {
  list(
    search = "Cerca:",
    lengthMenu = "Mostra _MENU_ righe",
    info = "Righe da _START_ a _END_ di _TOTAL_",
    infoEmpty = "Nessuna riga",
    infoFiltered = "(filtrate da _MAX_ totali)",
    zeroRecords = "Nessun cluster corrisponde ai filtri",
    emptyTable = "Nessun dato",
    paginate = list(previous = "Precedente", `next` = "Successiva"),
    thousands = ".",
    decimal = ","
  )
}

#' Tabella interattiva dell'analisi dei cluster
#'
#' Ogni colonna ha il proprio filtro; le colonne categoriche si filtrano da
#' un elenco. L'indicatore è evidenziato dal bordo colorato della cella.
#'
#' @param risultato Tabella prodotta da `calcola_analisi_cluster()`.
#' @noRd
tabella_cluster_dt <- function(risultato) {
  config <- indicatori_config()
  categoriche <- c(
    "presente_a_database",
    "servizio_transponder",
    "servizio_atteso"
  )
  tabella <- as.data.frame(risultato) |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(categoriche), as.factor),
      # Come testo: il formato data del browser sposterebbe il giorno con il fuso orario.
      dplyr::across(
        dplyr::all_of(c("analisi_dal", "analisi_al")),
        ~ format(.x, "%d/%m/%Y")
      ),
      indicatore_cluster = factor(
        .data$indicatore_cluster,
        levels = intersect(config$indicatore, .data$indicatore_cluster)
      )
    )
  formato_ora <- list(
    timeZone = "UTC",
    year = "numeric",
    month = "2-digit",
    day = "2-digit",
    hour = "2-digit",
    minute = "2-digit"
  )

  DT::datatable(
    tabella,
    rownames = FALSE,
    filter = "top",
    selection = "single",
    class = "compact stripe hover nowrap",
    options = list(
      pageLength = 25,
      lengthMenu = c(10, 25, 50, 100),
      scrollX = TRUE,
      scrollY = "calc(100vh - 400px)",
      language = lingua_dt()
    ),
    callback = DT::JS(
      "$(table.table().container()).find('input[placeholder=\"All\"]').attr('placeholder', 'Tutti');"
    )
  ) |>
    DT::formatDate(
      c(
        "globale_prima_lettura",
        "globale_ultima_lettura",
        "cluster_prima_lettura",
        "cluster_ultima_lettura"
      ),
      method = "toLocaleString",
      params = list("it-IT", formato_ora)
    ) |>
    DT::formatRound(
      c("lat_baricentro_cluster", "lon_baricentro_cluster"),
      5,
      mark = ""
    ) |>
    DT::formatRound(
      "cluster_dispersione_90th_m",
      1,
      mark = ".",
      dec.mark = ","
    ) |>
    DT::formatRound("cluster_indice_fiducia", 2, mark = ".", dec.mark = ",") |>
    DT::formatStyle(
      "indicatore_cluster",
      borderLeft = DT::styleEqual(
        config$indicatore,
        paste("5px solid", config$colore)
      ),
      fontWeight = "600"
    )
}

#' Grafico a barre: numero di cluster per indicatore
#'
#' Le barre seguono l'ordine fisso degli indicatori e riportano il conteggio;
#' gli indicatori senza cluster restano visibili con valore zero.
#'
#' @param risultato Tabella dell'analisi, anche filtrata.
#' @noRd
grafico_indicatori <- function(risultato) {
  config <- indicatori_config()
  inchiostro <- inchiostro_grafici()
  conteggi <- dplyr::summarise(
    risultato,
    cluster = dplyr::n(),
    rfid = dplyr::n_distinct(.data$RFID),
    .by = "indicatore_cluster"
  )
  indice <- match(config$indicatore, conteggi$indicatore_cluster)
  config$cluster <- dplyr::coalesce(conteggi$cluster[indice], 0L)
  config$rfid <- dplyr::coalesce(conteggi$rfid[indice], 0L)
  config$descrizione <- sprintf(
    "<b>%s</b><br>%s<br>%s di %s",
    config$indicatore,
    config$significato,
    purrr::map_chr(config$cluster, conta, "cluster", "cluster"),
    purrr::map_chr(config$rfid, conta, "RFID", "RFID")
  )

  plotly::plot_ly(
    config,
    x = ~cluster,
    y = ~ factor(indicatore, levels = rev(indicatore)),
    type = "bar",
    orientation = "h",
    marker = list(color = ~colore),
    text = ~ formatta_numero(cluster),
    textposition = "outside",
    cliponaxis = FALSE,
    textfont = list(color = inchiostro$primario, size = 12),
    hovertext = ~descrizione,
    hoverinfo = "text"
  ) |>
    plotly::layout(
      title = list(
        text = "<b>Cluster per indicatore</b>",
        x = 0,
        xref = "paper",
        xanchor = "left",
        font = list(size = 14, color = inchiostro$primario)
      ),
      font = list(size = 12, color = inchiostro$secondario),
      xaxis = list(
        title = "Numero di cluster",
        rangemode = "tozero",
        fixedrange = TRUE,
        gridcolor = inchiostro$griglia,
        zerolinecolor = inchiostro$asse
      ),
      yaxis = list(title = "", fixedrange = TRUE, ticksuffix = "  "),
      bargap = 0.45,
      showlegend = FALSE,
      margin = list(l = 10, r = 50, t = 40, b = 40),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

#' Grafici a dispersione: letture dell'RFID e dispersione dei cluster
#'
#' Un riquadro per indicatore, tutti con gli stessi assi logaritmici: in colore
#' i cluster dell'indicatore, in grigio gli altri come contesto. Le linee della
#' griglia coincidono con le soglie della classificazione.
#'
#' @param risultato Tabella dell'analisi, anche filtrata.
#' @param soglie Soglie della classificazione.
#' @param giorni_osservati Giorni del periodo coperti dal dataset.
#' @noRd
grafico_dispersione <- function(
  risultato,
  soglie = soglie_indicatore(),
  giorni_osservati = 365
) {
  config <- indicatori_config()
  inchiostro <- inchiostro_grafici()
  presenti <- config[
    config$indicatore %in% risultato$indicatore_cluster,
    ,
    drop = FALSE
  ]

  dati <- dplyr::mutate(
    as.data.frame(risultato),
    x = .data$globale_numero_letture,
    # Asse logaritmico: le dispersioni sotto il metro sono riportate a 1 m.
    y = pmax(.data$cluster_dispersione_90th_m, 1),
    testo = sprintf(
      "<b>%s</b> \u00b7 cluster %d di %d<br>Letture: %s nel cluster, %s in tutto<br>Dispersione: %s m<br>Indice di fiducia: %s",
      .data$RFID,
      .data$cluster_id,
      .data$globale_conteggio_cluster,
      formatta_numero(.data$cluster_numero_letture),
      formatta_numero(.data$globale_numero_letture),
      format(
        .data$cluster_dispersione_90th_m,
        big.mark = ".",
        decimal.mark = ",",
        nsmall = 1,
        trim = TRUE
      ),
      format(.data$cluster_indice_fiducia, decimal.mark = ",", nsmall = 2)
    )
  )

  # Tacche degli assi sulle soglie: letture per anno riportate al periodo osservato.
  soglie_letture <- round(
    c(soglie$letture_min, soglie$letture_max) * giorni_osservati / 365
  )
  tacche_x <- sort(unique(c(1, soglie_letture, 10 * soglie_letture[2])))
  tacche_y <- c(
    1,
    10,
    soglie$compatto_m,
    soglie$disperso_m,
    soglie$rumore_m,
    10000
  )
  asse <- function(titolo, tacche, etichette, limiti) {
    list(
      title = list(text = titolo, font = list(size = 11)),
      type = "log",
      range = log10(limiti),
      tickvals = tacche,
      ticktext = etichette,
      tickfont = list(size = 9),
      gridcolor = inchiostro$griglia,
      linecolor = inchiostro$asse,
      zeroline = FALSE,
      fixedrange = TRUE
    )
  }
  asse_x <- asse(
    "Letture dell'RFID nel periodo",
    tacche_x,
    formatta_numero(tacche_x),
    c(0.7, max(dati$x, 10 * soglie_letture[2]) * 1.5)
  )
  asse_y <- asse(
    "Dispersione del cluster (m)",
    tacche_y,
    c("\u2264 1", formatta_numero(tacche_y[-1])),
    c(0.7, max(dati$y, 10000) * 1.5)
  )
  tipo <- if (nrow(dati) > 5000) "scattergl" else "scatter"

  riquadro <- function(k) {
    scelto <- dati$indicatore_cluster == presenti$indicatore[k]
    grafico <- plotly::plot_ly()
    if (any(!scelto)) {
      grafico <- plotly::add_trace(
        grafico,
        data = dati[!scelto, , drop = FALSE],
        x = ~x,
        y = ~y,
        type = tipo,
        mode = "markers",
        marker = list(color = inchiostro$contesto, size = 5),
        hoverinfo = "skip",
        showlegend = FALSE
      )
    }
    grafico |>
      plotly::add_trace(
        data = dati[scelto, , drop = FALSE],
        x = ~x,
        y = ~y,
        text = ~testo,
        type = tipo,
        mode = "markers",
        marker = list(
          color = presenti$colore[k],
          size = 9,
          line = list(color = "#ffffff", width = 1)
        ),
        hoverinfo = "text",
        showlegend = FALSE
      ) |>
      plotly::layout(
        xaxis = asse_x,
        yaxis = asse_y,
        annotations = list(list(
          text = sprintf(
            "<b>%s</b> \u00b7 %s",
            presenti$indicatore[k],
            formatta_numero(sum(scelto))
          ),
          x = 0,
          y = 1,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "bottom",
          showarrow = FALSE,
          font = list(size = 11, color = inchiostro$primario)
        ))
      )
  }

  colonne <- min(4, nrow(presenti))
  plotly::subplot(
    purrr::map(seq_len(nrow(presenti)), riquadro),
    nrows = ceiling(nrow(presenti) / colonne),
    shareX = TRUE,
    shareY = TRUE,
    titleX = TRUE,
    titleY = TRUE,
    margin = c(0.015, 0.015, 0.09, 0.07)
  ) |>
    plotly::layout(
      title = list(
        text = "<b>Letture e dispersione dei cluster</b>",
        x = 0,
        xref = "paper",
        xanchor = "left",
        font = list(size = 14, color = inchiostro$primario)
      ),
      font = list(size = 11, color = inchiostro$secondario),
      showlegend = FALSE,
      margin = list(l = 10, r = 10, t = 60, b = 40),
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    ) |>
    plotly::config(displayModeBar = FALSE)
}

#' Popup di un cluster sulla mappa
#' @noRd
popup_cluster <- function(righe) {
  testo <- function(x, se_mancante = "N/D") {
    htmltools::htmlEscape(tidyr::replace_na(as.character(x), se_mancante))
  }
  servizio <- dplyr::coalesce(
    righe$servizio_transponder,
    righe$servizio_atteso,
    righe$servizio_atteso_dettaglio
  )
  paste0(
    "<div style='font-family: Arial; font-size: 12px;'>",
    "<b>RFID:</b> ",
    testo(righe$RFID),
    "<br>",
    "<b>Cluster:</b> ",
    righe$cluster_id,
    " di ",
    righe$globale_conteggio_cluster,
    "<br>",
    "<b>Indicatore:</b> ",
    testo(righe$indicatore_cluster),
    "<br>",
    "<b>Indice di fiducia:</b> ",
    format(righe$cluster_indice_fiducia, decimal.mark = ",", nsmall = 2),
    "<br>",
    "<b>Letture:</b> ",
    formatta_numero(righe$cluster_numero_letture),
    " nel cluster, ",
    formatta_numero(righe$globale_numero_letture),
    " in tutto<br>",
    "<b>Dispersione:</b> ",
    format(
      righe$cluster_dispersione_90th_m,
      big.mark = ".",
      decimal.mark = ",",
      nsmall = 1,
      trim = TRUE
    ),
    " m<br>",
    "<b>Dal:</b> ",
    formatta_data_ora(righe$cluster_prima_lettura, "%d/%m/%Y"),
    " <b>al:</b> ",
    formatta_data_ora(righe$cluster_ultima_lettura, "%d/%m/%Y"),
    "<br>",
    "<b>Censito:</b> ",
    testo(righe$presente_a_database),
    "<br>",
    "<b>Servizio:</b> ",
    testo(servizio),
    "<br>",
    "</div>"
  )
}

#' Mappa dei cluster: baricentri e cerchi di dispersione
#'
#' Ogni cluster è un punto sul baricentro, circondato da un cerchio il cui
#' raggio è la dispersione al 90° percentile. Gli indicatori sono livelli
#' attivabili dal controllo in alto a destra, che fa anche da legenda.
#'
#' @param righe Righe dell'analisi da mostrare.
#' @noRd
mappa_cluster <- function(righe) {
  mappa <- leaflet::leaflet(
    options = leaflet::leafletOptions(preferCanvas = TRUE)
  ) |>
    leaflet::addTiles() |>
    leaflet::addScaleBar(
      position = "bottomleft",
      options = leaflet::scaleBarOptions(imperial = FALSE)
    )
  if (is.null(righe) || nrow(righe) == 0) {
    return(leaflet::setView(mappa, lng = 12.5, lat = 41.9, zoom = 11))
  }

  config <- indicatori_config()
  config <- config[
    config$indicatore %in% righe$indicatore_cluster,
    ,
    drop = FALSE
  ]
  config$gruppo <- sprintf(
    "<span style='color:%s;'>&#9679;</span> %s",
    config$colore,
    config$indicatore
  )
  righe$riga <- seq_len(nrow(righe))

  for (k in seq_len(nrow(config))) {
    parte <- righe[
      righe$indicatore_cluster == config$indicatore[k],
      ,
      drop = FALSE
    ]
    mappa <- mappa |>
      leaflet::addCircles(
        lng = parte$lon_baricentro_cluster,
        lat = parte$lat_baricentro_cluster,
        radius = pmax(parte$cluster_dispersione_90th_m, 1),
        color = config$colore[k],
        weight = 1,
        opacity = 0.9,
        fillOpacity = 0.12,
        group = config$gruppo[k],
        options = leaflet::pathOptions(interactive = FALSE)
      ) |>
      leaflet::addCircleMarkers(
        lng = parte$lon_baricentro_cluster,
        lat = parte$lat_baricentro_cluster,
        radius = 6,
        stroke = TRUE,
        color = "#ffffff",
        weight = 1.5,
        opacity = 1,
        fillColor = config$colore[k],
        fillOpacity = 0.95,
        layerId = as.character(parte$riga),
        label = parte$RFID,
        popup = popup_cluster(parte),
        group = config$gruppo[k]
      )
  }

  mappa |>
    leaflet::addLayersControl(
      overlayGroups = config$gruppo,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::fitBounds(
      lng1 = min(righe$lon_baricentro_cluster) - 0.002,
      lat1 = min(righe$lat_baricentro_cluster) - 0.002,
      lng2 = max(righe$lon_baricentro_cluster) + 0.002,
      lat2 = max(righe$lat_baricentro_cluster) + 0.002
    )
}
