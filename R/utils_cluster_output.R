# Tabella di output dell'analisi dei cluster spaziali: una riga per ogni
# cluster di ogni RFID nel periodo di analisi.

#' Colonne della tabella di analisi, nell'ordine di esportazione
#' @noRd
colonne_analisi_cluster <- function() {
  c(
    "RFID",
    "cluster_id",
    "globale_conteggio_cluster",
    "globale_prima_lettura",
    "globale_ultima_lettura",
    "cluster_prima_lettura",
    "cluster_ultima_lettura",
    "presente_a_database",
    "cantiere",
    "comune",
    "servizio_transponder",
    "servizio_transponder_cronologia",
    "servizio_atteso",
    "servizio_atteso_dettaglio",
    "volume_previsto",
    "volume_atteso",
    "analisi_dal",
    "analisi_al",
    "numero_raccolte_annue_previste",
    "numero_raccolte_annue_presunte",
    "globale_numero_letture",
    "cluster_numero_letture",
    "lat_baricentro_cluster",
    "lon_baricentro_cluster",
    "cluster_dispersione_90th_m",
    "cluster_indice_fiducia",
    "indicatore_cluster"
  )
}

#' Soglie della classificazione dei cluster
#'
#' Le soglie sulle letture si intendono per anno: vengono confrontate con la
#' frequenza annua dell'RFID, cioè le letture rapportate ai giorni osservati.
#'
#' @return Lista con: `letture_min` e `letture_max` (frequenza annua minima e
#'   massima di un contenitore reale), `compatto_m` (dispersione massima di un
#'   cluster compatto), `disperso_m` e `rumore_m` (dispersione oltre la quale
#'   il tag viaggia o le letture sono sparse), `cluster_max` (numero massimo
#'   di cluster di un contenitore spostato), `coerenza_atteso` (quota minima
#'   del servizio atteso prevalente).
#' @noRd
soglie_indicatore <- function() {
  list(
    letture_min = 5,
    letture_max = 300,
    compatto_m = 50,
    disperso_m = 500,
    rumore_m = 1000,
    cluster_max = 3,
    coerenza_atteso = 0.8
  )
}

#' Pesi delle componenti dell'indice di fiducia (somma 1)
#' @noRd
pesi_fiducia <- function() {
  c(
    censimento = 0.15,
    letture = 0.25,
    compattezza = 0.25,
    coerenza = 0.15,
    raccolte = 0.10,
    unicita = 0.10
  )
}

#' Indicatori di cluster: ordine, colore e significato
#'
#' L'ordine è fisso e determina i colori in tabella, grafici e mappa.
#' @noRd
indicatori_config <- function() {
  data.frame(
    indicatore = c(
      "VALID_TARGET",
      "RELOCATED_BIN",
      "GHOST_TAG",
      "DEPOT_STUCK",
      "TRUCK_STOWAWAY",
      "SCATTERED_READS / GPS_NOISE",
      "UNCATEGORIZED"
    ),
    colore = c(
      "#2a78d6",
      "#eb6834",
      "#1baf7a",
      "#eda100",
      "#e87ba4",
      "#008300",
      "#898781"
    ),
    significato = c(
      "Contenitore stabile nella sua posizione",
      "Contenitore spostato da un punto a un altro",
      "Poche letture: tag smarrito o letto per caso",
      "Tag fermo vicino a un lettore, ad esempio in deposito",
      "Tag rimasto sul mezzo, letto lungo tutto il giro",
      "Letture sparse o GPS impreciso",
      "Nessuna delle categorie precedenti"
    ),
    stringsAsFactors = FALSE
  )
}

#' Sequenza dei valori distinti consecutivi, ignorando i mancanti
#' @noRd
sequenza_valori <- function(x) {
  x <- x[!is.na(x)]
  x[c(TRUE, x[-1] != x[-length(x)])[seq_along(x)]]
}

#' Limita un valore all'intervallo 0-1
#' @noRd
tra_zero_e_uno <- function(x) pmin(pmax(x, 0), 1)

#' Classifica i cluster in categorie
#'
#' Le regole sono valutate in ordine, la prima che vale determina l'indicatore.
#' La frequenza è quella annua dell'RFID; la dispersione è quella del cluster.
#'
#' @param frequenza_annua Letture annue dell'RFID.
#' @param n_cluster Numero di cluster dell'RFID.
#' @param dispersione_m Dispersione del cluster in metri.
#' @param tutti_compatti `TRUE` se tutti i cluster dell'RFID sono compatti.
#' @param soglie Soglie, vedi `soglie_indicatore()`.
#' @return Vettore di indicatori.
#' @noRd
classifica_cluster <- function(
  frequenza_annua,
  n_cluster,
  dispersione_m,
  tutti_compatti,
  soglie = soglie_indicatore()
) {
  regolare <- frequenza_annua >= soglie$letture_min &
    frequenza_annua <= soglie$letture_max
  eccessiva <- frequenza_annua > soglie$letture_max
  compatto <- dispersione_m < soglie$compatto_m

  dplyr::case_when(
    frequenza_annua < soglie$letture_min ~ "GHOST_TAG",
    regolare &
      n_cluster >= 2 &
      n_cluster <= soglie$cluster_max &
      tutti_compatti ~ "RELOCATED_BIN",
    eccessiva & n_cluster == 1 & compatto ~ "DEPOT_STUCK",
    eccessiva & dispersione_m > soglie$disperso_m ~ "TRUCK_STOWAWAY",
    regolare &
      (n_cluster > soglie$cluster_max | dispersione_m > soglie$rumore_m) ~
      "SCATTERED_READS / GPS_NOISE",
    regolare & n_cluster == 1 & compatto ~ "VALID_TARGET",
    .default = "UNCATEGORIZED"
  )
}

#' Indice di fiducia di un cluster (0-1)
#'
#' Somma pesata di sei componenti, ognuna tra 0 e 1:
#' - censimento: 1 se il contenitore è a database;
#' - letture: cresce con le letture del cluster, piena da 10 in su;
#' - compattezza: 1 fino a 50 m di dispersione, 0 da 500 m, scala logaritmica;
#' - coerenza: per i censiti 1 se il servizio non è cambiato (0,5 altrimenti),
#'   per i non censiti cresce con la quota del servizio atteso prevalente
#'   (0 fino al 50%, 1 dall'80%);
#' - raccolte: accordo tra raccolte presunte e previste (0,5 se le previste
#'   non sono note);
#' - unicità: quota delle letture dell'RFID che cadono nel cluster.
#'
#' @param censito `TRUE` se il contenitore è presente a database.
#' @param cluster_letture,globale_letture Letture del cluster e dell'RFID.
#' @param dispersione_m Dispersione del cluster in metri.
#' @param servizio_stabile `TRUE` se il servizio transponder non è cambiato.
#' @param quota_atteso Quota del servizio atteso prevalente nel cluster.
#' @param previste,presunte Raccolte annue previste e presunte.
#' @param soglie,pesi Vedi `soglie_indicatore()` e `pesi_fiducia()`.
#' @return Indice arrotondato a due decimali.
#' @noRd
indice_fiducia <- function(
  censito,
  cluster_letture,
  globale_letture,
  dispersione_m,
  servizio_stabile,
  quota_atteso,
  previste,
  presunte,
  soglie = soglie_indicatore(),
  pesi = pesi_fiducia()
) {
  p_censimento <- as.numeric(censito)
  p_letture <- tra_zero_e_uno(cluster_letture / 10)
  p_compattezza <- 1 -
    tra_zero_e_uno(
      (log(pmax(dispersione_m, 1)) - log(soglie$compatto_m)) /
        (log(soglie$disperso_m) - log(soglie$compatto_m))
    )
  p_coerenza <- ifelse(
    censito,
    ifelse(servizio_stabile, 1, 0.5),
    tra_zero_e_uno(
      (dplyr::coalesce(quota_atteso, 0) - 0.5) / (soglie$coerenza_atteso - 0.5)
    )
  )
  rapporto <- presunte / previste
  p_raccolte <- ifelse(
    is.na(previste) | previste <= 0,
    0.5,
    ifelse(presunte <= 0, 0, pmin(rapporto, 1 / rapporto))
  )
  p_unicita <- cluster_letture / globale_letture

  round(
    pesi[["censimento"]] *
      p_censimento +
      pesi[["letture"]] * p_letture +
      pesi[["compattezza"]] * p_compattezza +
      pesi[["coerenza"]] * p_coerenza +
      pesi[["raccolte"]] * p_raccolte +
      pesi[["unicita"]] * p_unicita,
    2
  )
}

#' Tabella di analisi senza righe, con le colonne e i tipi corretti
#' @noRd
analisi_cluster_vuota <- function() {
  istante <- as.POSIXct(character(0), tz = "UTC")
  giorno <- as.Date(character(0))
  dplyr::tibble(
    RFID = character(0),
    cluster_id = integer(0),
    globale_conteggio_cluster = integer(0),
    globale_prima_lettura = istante,
    globale_ultima_lettura = istante,
    cluster_prima_lettura = istante,
    cluster_ultima_lettura = istante,
    presente_a_database = character(0),
    cantiere = character(0),
    comune = character(0),
    servizio_transponder = character(0),
    servizio_transponder_cronologia = character(0),
    servizio_atteso = character(0),
    servizio_atteso_dettaglio = character(0),
    volume_previsto = numeric(0),
    volume_atteso = numeric(0),
    analisi_dal = giorno,
    analisi_al = giorno,
    numero_raccolte_annue_previste = numeric(0),
    numero_raccolte_annue_presunte = numeric(0),
    globale_numero_letture = integer(0),
    cluster_numero_letture = integer(0),
    lat_baricentro_cluster = numeric(0),
    lon_baricentro_cluster = numeric(0),
    cluster_dispersione_90th_m = numeric(0),
    cluster_indice_fiducia = numeric(0),
    indicatore_cluster = character(0)
  )
}

#' Calcola la tabella di analisi dei cluster spaziali
#'
#' Per ogni RFID raggruppa le letture del periodo in cluster spaziali e
#' descrive ogni cluster: tempi, letture, baricentro, dispersione, servizio,
#' raccolte annue, indice di fiducia e indicatore.
#'
#' I conteggi annui usano i giorni osservati, cioè la parte del periodo di
#' analisi effettivamente coperta dal dataset: così un dataset che copre solo
#' alcuni mesi dell'anno non porta a sottostimare frequenze e raccolte.
#'
#' I sacchetti restano fuori dall'analisi, vedi `senza_sacchetti()`: sono
#' monouso, con una lettura sola, e l'analisi riguarda i contenitori.
#'
#' I riepiloghi per RFID e per cluster sono calcolati con le funzioni di
#' `R/utils_gruppi.R`, su tutte le letture insieme: con milioni di letture un
#' calcolo ripetuto RFID per RFID richiederebbe minuti.
#'
#' @param letture Dataset validato (tutte le letture, non solo quelle del periodo).
#' @param analisi_dal,analisi_al Estremi del periodo di analisi, inclusi.
#' @param eps_m,min_pts Parametri di DBSCAN, vedi `cluster_dbscan()`.
#' @param soglie,pesi Vedi `soglie_indicatore()` e `pesi_fiducia()`.
#' @param durata_minima_giorni Durata minima di un cluster perché le sue
#'   raccolte annue vengano stimate anche dalla sua durata effettiva.
#' @return Tibble con le colonne di `colonne_analisi_cluster()`, una riga per
#'   cluster. L'attributo `parametri` riporta i parametri usati.
#' @noRd
calcola_analisi_cluster <- function(
  letture,
  analisi_dal,
  analisi_al,
  eps_m = 100,
  min_pts = 1,
  soglie = soglie_indicatore(),
  pesi = pesi_fiducia(),
  durata_minima_giorni = 30
) {
  analisi_dal <- lubridate::as_date(analisi_dal)
  analisi_al <- lubridate::as_date(analisi_al)
  if (
    length(analisi_dal) != 1 ||
      length(analisi_al) != 1 ||
      is.na(analisi_dal) ||
      is.na(analisi_al) ||
      analisi_dal > analisi_al
  ) {
    rlang::abort(
      "Periodo di analisi non valido: servono due date, con l'inizio non successivo alla fine."
    )
  }
  for (campo in colonne_quantita()) {
    if (!campo %in% names(letture)) letture[[campo]] <- NA_real_
  }
  # L'analisi riguarda i contenitori: i sacchetti, monouso, restano fuori. Il
  # risultato è quello di un file che non li contiene.
  letture <- senza_sacchetti(letture)

  nel_periodo <- filtra_periodo(letture, c(analisi_dal, analisi_al))
  parametri <- list(
    eps_m = eps_m,
    min_pts = min_pts,
    soglie = soglie,
    giorni_osservati = 0
  )
  if (nrow(nel_periodo) == 0) {
    return(structure(analisi_cluster_vuota(), parametri = parametri))
  }

  # Parte del periodo effettivamente coperta dal dataset.
  copertura <- range(lubridate::as_date(letture$giorno_lettura))
  giorni_osservati <- as.numeric(
    min(analisi_al, copertura[2]) - max(analisi_dal, copertura[1])
  ) +
    1
  parametri$giorni_osservati <- giorni_osservati

  # Letture in ordine di RFID e di tempo: i riepiloghi contano su quest'ordine.
  ordine <- order(
    nel_periodo$RFID,
    nel_periodo$giorno_lettura,
    method = "radix"
  )
  dati <- assegna_cluster(nel_periodo[ordine, , drop = FALSE], eps_m, min_pts)
  rfid <- indice_gruppi(dati$RFID)
  n_rfid <- max(rfid)

  # Un gruppo per ogni cluster di ogni RFID, in ordine di RFID e di cluster.
  chiave_cluster <- rfid * (max(dati$cluster_id) + 1) + dati$cluster_id
  cluster <- match(chiave_cluster, sort(unique(chiave_cluster)))
  n_cluster <- max(cluster)
  rfid_del_cluster <- primo_per_gruppo(rfid, cluster, n_cluster)

  lat_baricentro <- media_per_gruppo(dati$latitudine, cluster, n_cluster)
  lon_baricentro <- media_per_gruppo(dati$longitudine, cluster, n_cluster)
  distanza_m <- distanza_haversine_m(
    dati$latitudine,
    dati$longitudine,
    lat_baricentro[cluster],
    lon_baricentro[cluster]
  )

  # --- Valori globali dell'RFID nel periodo -----------------------------------
  n_servizi <- n_distinti_per_gruppo(dati$servizio_transponder, rfid, n_rfid)
  # La cronologia dei servizi serve solo agli RFID che ne hanno più di uno.
  sequenza_servizi <- rep(NA_character_, n_rfid)
  con_cambi <- which(n_servizi[rfid] > 1)
  if (length(con_cambi) > 0) {
    sequenze <- tapply(
      dati$servizio_transponder[con_cambi],
      rfid[con_cambi],
      function(servizi) paste(sequenza_valori(servizi), collapse = " > ")
    )
    sequenza_servizi[as.integer(names(sequenze))] <- sequenze
  }
  geografia <- geografia_per_gruppo(dati, rfid, n_rfid)

  per_rfid <- dplyr::tibble(
    RFID = primo_per_gruppo(dati$RFID, rfid, n_rfid),
    globale_conteggio_cluster = tabulate(rfid_del_cluster, n_rfid),
    globale_prima_lettura = primo_per_gruppo(dati$giorno_lettura, rfid, n_rfid),
    globale_ultima_lettura = ultimo_per_gruppo(
      dati$giorno_lettura,
      rfid,
      n_rfid
    ),
    globale_numero_letture = tabulate(rfid, n_rfid),
    presente_a_database = ultimo_per_gruppo(
      dati$presente_a_database,
      rfid,
      n_rfid
    ),
    cantiere = geografia$cantiere,
    comune = geografia$comune,
    n_servizi = n_servizi,
    sequenza_servizi = sequenza_servizi,
    servizio_transponder = ultimo_valido_per_gruppo(
      dati$servizio_transponder,
      rfid,
      n_rfid
    ),
    volume_previsto = ultimo_valido_per_gruppo(
      dati$volume_previsto,
      rfid,
      n_rfid
    ),
    numero_raccolte_annue_previste = ultimo_valido_per_gruppo(
      dati$numero_raccolte_annue_previste,
      rfid,
      n_rfid
    )
  ) |>
    dplyr::mutate(
      censito = .data$presente_a_database == "Presente",
      # Servizio, volume e raccolte previste valgono solo per i censiti.
      servizio_transponder = dplyr::if_else(
        .data$censito,
        .data$servizio_transponder,
        NA_character_
      ),
      servizio_transponder_cronologia = dplyr::if_else(
        .data$censito & .data$n_servizi > 1,
        .data$sequenza_servizi,
        NA_character_
      ),
      volume_previsto = dplyr::if_else(
        .data$censito,
        .data$volume_previsto,
        NA_real_
      ),
      numero_raccolte_annue_previste = dplyr::if_else(
        .data$censito,
        .data$numero_raccolte_annue_previste,
        NA_real_
      ),
      frequenza_annua = .data$globale_numero_letture * 365 / giorni_osservati
    )

  # --- Valori del singolo cluster ---------------------------------------------
  per_cluster <- dplyr::tibble(
    RFID = per_rfid$RFID[rfid_del_cluster],
    cluster_id = primo_per_gruppo(dati$cluster_id, cluster, n_cluster),
    cluster_prima_lettura = primo_per_gruppo(
      dati$giorno_lettura,
      cluster,
      n_cluster
    ),
    cluster_ultima_lettura = ultimo_per_gruppo(
      dati$giorno_lettura,
      cluster,
      n_cluster
    ),
    cluster_numero_letture = tabulate(cluster, n_cluster),
    lat_baricentro_cluster = lat_baricentro,
    lon_baricentro_cluster = lon_baricentro,
    cluster_dispersione_90th_m = quantile_per_gruppo(
      distanza_m,
      cluster,
      0.90,
      n_cluster
    ),
    atteso_prevalente = NA_character_,
    quota_atteso = NA_real_,
    composizione_atteso = NA_character_
  )
  # Un RFID ha tutti i cluster compatti se nessuno supera la soglia.
  non_compatti <- tabulate(
    rfid_del_cluster[
      !(per_cluster$cluster_dispersione_90th_m < soglie$compatto_m)
    ],
    n_rfid
  )
  per_cluster$tutti_compatti <- (non_compatti == 0)[rfid_del_cluster]

  # Composizione del servizio atteso nel cluster, dal più al meno frequente.
  # Contano le sole letture non censite, come per l'icona dei marker: nei
  # dati reali il giro c'è su ogni lettura, e quello dei censiti non serve.
  con_atteso <- which(
    !is.na(dati$servizio_atteso) & dati$presente_a_database == "Non Presente"
  )
  if (length(con_atteso) > 0) {
    atteso <- dplyr::count(
      dplyr::tibble(
        cluster = cluster[con_atteso],
        servizio_atteso = dati$servizio_atteso[con_atteso]
      ),
      .data$cluster,
      .data$servizio_atteso,
      name = "n"
    )
    atteso$quota <- atteso$n /
      somma_per_gruppo(atteso$n, atteso$cluster, n_cluster)[atteso$cluster]
    atteso <- atteso[
      order(
        atteso$cluster,
        -atteso$n,
        atteso$servizio_atteso,
        method = "radix"
      ),
    ]
    prevalente <- !duplicated(atteso$cluster)
    per_cluster$atteso_prevalente[
      atteso$cluster[prevalente]
    ] <- atteso$servizio_atteso[prevalente]
    per_cluster$quota_atteso[atteso$cluster[prevalente]] <- atteso$quota[
      prevalente
    ]
    composizione <- tapply(
      paste0(atteso$servizio_atteso, " (", round(100 * atteso$quota), "%)"),
      atteso$cluster,
      paste,
      collapse = "; "
    )
    per_cluster$composizione_atteso[
      as.integer(names(composizione))
    ] <- composizione
  }

  risultato <- per_cluster |>
    dplyr::left_join(per_rfid, by = "RFID") |>
    dplyr::mutate(
      servizio_atteso = dplyr::if_else(
        !.data$censito &
          dplyr::coalesce(.data$quota_atteso, 0) >= soglie$coerenza_atteso,
        .data$atteso_prevalente,
        NA_character_
      ),
      servizio_atteso_dettaglio = dplyr::if_else(
        .data$censito,
        NA_character_,
        .data$composizione_atteso
      ),
      volume_atteso = .data$volume_previsto,
      analisi_dal = analisi_dal,
      analisi_al = analisi_al,
      durata_cluster_giorni = as.numeric(
        difftime(
          .data$cluster_ultima_lettura,
          .data$cluster_prima_lettura,
          units = "days"
        )
      ),
      # Massimo tra la stima sul periodo osservato e quella sulla durata del
      # cluster; la seconda solo se il cluster dura abbastanza da non gonfiarla.
      numero_raccolte_annue_presunte = pmax(
        floor(.data$cluster_numero_letture * 365 / giorni_osservati),
        dplyr::if_else(
          .data$durata_cluster_giorni >= durata_minima_giorni,
          floor(
            .data$cluster_numero_letture * 365 / .data$durata_cluster_giorni
          ),
          NA_real_
        ),
        na.rm = TRUE
      ),
      indicatore_cluster = classifica_cluster(
        .data$frequenza_annua,
        .data$globale_conteggio_cluster,
        .data$cluster_dispersione_90th_m,
        .data$tutti_compatti,
        soglie
      ),
      cluster_indice_fiducia = indice_fiducia(
        censito = .data$censito,
        cluster_letture = .data$cluster_numero_letture,
        globale_letture = .data$globale_numero_letture,
        dispersione_m = .data$cluster_dispersione_90th_m,
        servizio_stabile = .data$n_servizi <= 1,
        quota_atteso = .data$quota_atteso,
        previste = .data$numero_raccolte_annue_previste,
        presunte = .data$numero_raccolte_annue_presunte,
        soglie = soglie,
        pesi = pesi
      ),
      lat_baricentro_cluster = round(.data$lat_baricentro_cluster, 6),
      lon_baricentro_cluster = round(.data$lon_baricentro_cluster, 6),
      cluster_dispersione_90th_m = round(.data$cluster_dispersione_90th_m, 1)
    ) |>
    dplyr::arrange(.data$RFID, .data$cluster_id) |>
    dplyr::select(dplyr::all_of(colonne_analisi_cluster()))

  structure(risultato, parametri = parametri)
}

#' Nome del file di esportazione dell'analisi
#' @noRd
nome_file_analisi_cluster <- function(analisi_dal, analisi_al) {
  sprintf(
    "cluster_analysis_%s_%s.csv",
    format(lubridate::as_date(analisi_dal)),
    format(lubridate::as_date(analisi_al))
  )
}

#' Scrive la tabella di analisi in un file CSV
#'
#' Data e ora sono scritte come `YYYY-MM-DD HH:MM:SS`, i valori mancanti come
#' celle vuote.
#'
#' @param risultato Tabella prodotta da `calcola_analisi_cluster()`.
#' @param file Percorso del file da scrivere.
#' @noRd
esporta_analisi_cluster <- function(risultato, file) {
  tabella <- dplyr::mutate(
    as.data.frame(risultato),
    dplyr::across(
      dplyr::where(lubridate::is.POSIXct),
      ~ format(.x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
    )
  )
  utils::write.csv(
    tabella,
    file,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  invisible(file)
}
