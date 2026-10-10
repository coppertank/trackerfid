# Letture storiche, cioè quelle del sistema precedente alle antenne, e
# confronto tra i due sistemi. Le letture storiche hanno solo RFID e istante.

# ---- Lettura e validazione ---------------------------------------------------

#' Colonne del CSV delle letture storiche
#' @noRd
colonne_storico <- function() c("RFID", "giorno_lettura")

#' Percorso delle letture storiche di esempio incluse nel pacchetto
#' @noRd
percorso_storico_esempio <- function() {
  app_sys("extdata", "sample_letture_storiche.csv")
}

#' Valida le letture storiche e converte la data
#'
#' Servono le colonne `RFID` e `giorno_lettura`: maiuscole e ordine non
#' contano, le altre colonne sono ignorate. Una data non interpretabile ferma
#' il caricamento. Le righe incomplete e quelle ripetute vengono scartate e
#' segnalate tra gli avvisi.
#'
#' @param data Data frame letto da `read_csv_auto()`.
#' @return Lista con `dati` (tibble con `RFID` e `giorno_lettura`) e `avvisi`.
#' @noRd
valida_storico <- function(data) {
  attese <- colonne_storico()
  posizione <- match(tolower(attese), tolower(stringr::str_trim(names(data))))
  mancanti <- attese[is.na(posizione)]
  if (length(mancanti) > 0) {
    errore_validazione(sprintf(
      "Colonne mancanti nel file delle letture storiche: %s. Il CSV deve contenere le colonne RFID e giorno_lettura.",
      paste(mancanti, collapse = ", ")
    ))
  }
  data <- dplyr::as_tibble(data[, posizione, drop = FALSE])
  names(data) <- attese
  if (nrow(data) == 0) {
    errore_validazione(
      "Il file delle letture storiche non contiene righe di dati."
    )
  }

  data$RFID <- as.character(pulisci_testo(data$RFID))
  data$giorno_lettura <- pulisci_testo(data$giorno_lettura)
  giorno <- converti_data_ora(data$giorno_lettura)
  non_validi <- which(!is.na(data$giorno_lettura) & is.na(giorno))
  if (length(non_validi) > 0) {
    errore_validazione(sprintf(
      "giorno_lettura: %d valori non riconosciuti come data e ora (formato atteso YYYY-MM-DD HH:MM:SS). Righe del file: %s.",
      length(non_validi),
      elenco_righe(non_validi)
    ))
  }
  data$giorno_lettura <- giorno

  avvisi <- character(0)
  completi <- tidyr::drop_na(data)
  if (nrow(completi) == 0) {
    errore_validazione(
      "Nessuna riga utilizzabile: in tutte manca l'RFID o la data."
    )
  }
  if (nrow(completi) < nrow(data)) {
    avvisi <- c(
      avvisi,
      sprintf(
        "%d righe scartate perch\u00e9 prive di RFID o di data.",
        nrow(data) - nrow(completi)
      )
    )
  }
  unici <- dplyr::distinct(completi)
  if (nrow(unici) < nrow(completi)) {
    avvisi <- c(
      avvisi,
      sprintf(
        "%d letture ripetute, con lo stesso RFID e lo stesso istante, contate una volta sola.",
        nrow(completi) - nrow(unici)
      )
    )
  }

  list(dati = unici, avvisi = avvisi)
}

#' Legge e valida un file di letture storiche
#'
#' @param path Percorso del file: CSV, CSV compresso con gzip o Parquet.
#' @param nome Nome originale del file, vedi `leggi_tabella()`.
#' @return Lista con `dati` e `avvisi`, vedi `valida_storico()`.
#' @noRd
carica_storico <- function(path, nome = basename(path)) {
  valida_storico(leggi_tabella(path, nome))
}

# ---- Letture per anno --------------------------------------------------------

#' Mette in un'unica tabella le letture dei due sistemi
#'
#' @param letture Letture con le antenne, validate.
#' @param storico Letture storiche validate, oppure `NULL`.
#' @return Tibble con `RFID`, `giorno_lettura` e `fonte`: `"storico"` o
#'   `"antenne"`.
#' @noRd
unisci_letture <- function(letture, storico = NULL) {
  antenne <- dplyr::tibble(
    RFID = letture$RFID,
    giorno_lettura = letture$giorno_lettura,
    fonte = "antenne"
  )
  if (is.null(storico) || nrow(storico) == 0) {
    return(antenne)
  }
  dplyr::bind_rows(
    dplyr::tibble(
      RFID = storico$RFID,
      giorno_lettura = storico$giorno_lettura,
      fonte = "storico"
    ),
    antenne
  )
}

#' Conta le letture per RFID, anno e sistema
#'
#' @param unite Risultato di `unisci_letture()`.
#' @return Tibble con `RFID`, `anno`, `fonte`, `letture`. Mancano le
#'   combinazioni senza letture.
#' @noRd
conta_letture_annuali <- function(unite) {
  unite |>
    dplyr::mutate(anno = as.integer(lubridate::year(.data$giorno_lettura))) |>
    dplyr::count(.data$RFID, .data$anno, .data$fonte, name = "letture")
}

#' Letture di ogni RFID in ogni anno, zeri compresi
#'
#' Per ogni RFID gli anni vanno da quello della sua prima lettura all'ultimo
#' anno dei dati. Gli anni precedenti mancano: il contenitore poteva non
#' essere ancora in servizio.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @param rfid RFID da tenere. Tutti, se manca.
#' @return Tibble con `RFID`, `anno`, `storico`, `antenne`, `letture` (la
#'   somma) e `primo_anno`.
#' @noRd
griglia_annuale <- function(unite, rfid = NULL) {
  if (!is.null(rfid)) {
    unite <- unite[unite$RFID %in% rfid, , drop = FALSE]
  }
  if (nrow(unite) == 0) {
    return(dplyr::tibble(
      RFID = character(0),
      anno = integer(0),
      storico = integer(0),
      antenne = integer(0),
      letture = integer(0),
      primo_anno = integer(0)
    ))
  }
  ultimo_anno <- as.integer(lubridate::year(max(unite$giorno_lettura)))
  conteggi <- conta_letture_annuali(unite) |>
    dplyr::mutate(
      fonte = factor(.data$fonte, levels = c("storico", "antenne"))
    ) |>
    tidyr::pivot_wider(
      names_from = "fonte",
      values_from = "letture",
      names_expand = TRUE,
      values_fill = 0L
    )
  # I conteggi sono in ordine di RFID e di anno: la prima riga di ogni RFID è
  # il suo primo anno.
  primi <- conteggi[!duplicated(conteggi$RFID), c("RFID", "anno")]
  n_anni <- ultimo_anno - primi$anno + 1L
  primo_anno <- rep(primi$anno, n_anni)

  dplyr::tibble(
    RFID = rep(primi$RFID, n_anni),
    primo_anno = primo_anno,
    anno = primo_anno + sequence(n_anni) - 1L
  ) |>
    dplyr::left_join(conteggi, by = c("RFID", "anno")) |>
    dplyr::mutate(
      storico = dplyr::coalesce(.data$storico, 0L),
      antenne = dplyr::coalesce(.data$antenne, 0L),
      letture = .data$storico + .data$antenne
    ) |>
    dplyr::select(dplyr::all_of(c(
      "RFID",
      "anno",
      "storico",
      "antenne",
      "letture",
      "primo_anno"
    )))
}

#' Letture per anno di un solo RFID
#'
#' Serve all'istogramma del dettaglio del bidone. Gli anni arrivano fino
#' all'ultimo anno delle letture con le antenne, anche se l'RFID non è più
#' stato letto.
#'
#' @param rfid Codice RFID.
#' @param letture Letture con le antenne, di tutti gli RFID.
#' @param storico Letture storiche, oppure `NULL`.
#' @return Tibble con `anno`, `storico`, `antenne`.
#' @noRd
andamento_rfid <- function(rfid, letture, storico = NULL) {
  vuoto <- dplyr::tibble(
    anno = integer(0),
    storico = integer(0),
    antenne = integer(0)
  )
  if (is.null(rfid) || is.null(letture) || nrow(letture) == 0) {
    return(vuoto)
  }
  proprie <- letture[letture$RFID == rfid, , drop = FALSE]
  if (!is.null(storico)) {
    storico <- storico[storico$RFID == rfid, , drop = FALSE]
  }
  griglia <- griglia_annuale(unisci_letture(proprie, storico))
  if (nrow(griglia) == 0) {
    return(vuoto)
  }
  ultimo_anno <- as.integer(lubridate::year(max(letture$giorno_lettura)))
  dplyr::tibble(
    anno = seq(min(griglia$anno), max(ultimo_anno, griglia$anno))
  ) |>
    dplyr::left_join(griglia[, c("anno", "storico", "antenne")], by = "anno") |>
    dplyr::mutate(
      storico = dplyr::coalesce(.data$storico, 0L),
      antenne = dplyr::coalesce(.data$antenne, 0L)
    )
}

#' Giorni di ogni anno coperti dai due sistemi
#'
#' Il periodo di un sistema va dalla sua prima alla sua ultima lettura. Un
#' anno è completo se i due periodi, insieme, ne coprono almeno la quota
#' `quota_completo`. Per un anno incompleto `fattore` riporta i conteggi a
#' dodici mesi.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @param quota_completo Quota minima di giorni coperti per un anno completo.
#' @return Tibble con `anno`, `giorni_anno`, `giorni_storico`,
#'   `giorni_antenne`, `giorni_coperti`, `sistema` (`"storico"`, `"misto"` o
#'   `"antenne"`), `completo` e `fattore`.
#' @noRd
copertura_annuale <- function(unite, quota_completo = 0.95) {
  giorni <- lubridate::as_date(unite$giorno_lettura)
  periodi <- purrr::map(split(giorni, unite$fonte), range)
  anni <- seq(lubridate::year(min(giorni)), lubridate::year(max(giorni)))

  righe <- purrr::map(anni, function(anno) {
    calendario <- seq(
      lubridate::make_date(anno, 1, 1),
      lubridate::make_date(anno, 12, 31),
      by = "day"
    )
    coperto <- function(fonte) {
      periodo <- periodi[[fonte]]
      if (is.null(periodo)) {
        return(rep(FALSE, length(calendario)))
      }
      calendario >= periodo[1] & calendario <= periodo[2]
    }
    dplyr::tibble(
      anno = as.integer(anno),
      giorni_anno = length(calendario),
      giorni_storico = sum(coperto("storico")),
      giorni_antenne = sum(coperto("antenne")),
      giorni_coperti = sum(coperto("storico") | coperto("antenne"))
    )
  })

  dplyr::bind_rows(righe) |>
    dplyr::mutate(
      sistema = dplyr::case_when(
        .data$giorni_antenne == 0 ~ "storico",
        .data$giorni_storico == 0 ~ "antenne",
        TRUE ~ "misto"
      ),
      completo = .data$giorni_coperti >= quota_completo * .data$giorni_anno,
      fattore = dplyr::if_else(
        .data$completo | .data$giorni_coperti == 0,
        1,
        .data$giorni_anno / .data$giorni_coperti
      )
    )
}

# ---- Anagrafica e tabella per anno -------------------------------------------

#' Anagrafica degli RFID letti dalle antenne
#'
#' Una riga per RFID, con lo stato dell'ultima lettura e l'ultimo valore noto
#' degli altri campi. Servizio, raccolte e volume valgono solo per i censiti.
#' Comune e cantiere ci sono anche per i non censiti, quando il giro che li
#' ha letti ha un comune: vedi `geografia_per_gruppo()`.
#'
#' @param letture Letture con le antenne, validate.
#' @return Tibble con `RFID`, `presente_a_database`, `servizio_transponder`,
#'   `cantiere`, `comune`, `numero_raccolte_annue_previste`, `volume_previsto`
#'   e `letture`, il numero di letture con le antenne. Gli RFID sono
#'   nell'ordine della loro prima lettura.
#' @noRd
anagrafica_rfid <- function(letture) {
  for (campo in colonne_quantita()) {
    if (!campo %in% names(letture)) letture[[campo]] <- NA_real_
  }
  letture <- letture[order(letture$giorno_lettura), , drop = FALSE]
  rfid <- indice_gruppi(letture$RFID)
  n_rfid <- if (length(rfid) > 0) max(rfid) else 0L
  geografia <- geografia_per_gruppo(letture, rfid, n_rfid)
  stato <- ultimo_per_gruppo(letture$presente_a_database, rfid, n_rfid)
  censito <- stato == "Presente"
  solo_censiti <- function(x) {
    valori <- ultimo_valido_per_gruppo(x, rfid, n_rfid)
    valori[!censito] <- NA
    valori
  }

  dplyr::tibble(
    RFID = primo_per_gruppo(letture$RFID, rfid, n_rfid),
    presente_a_database = stato,
    servizio_transponder = solo_censiti(letture$servizio_transponder),
    cantiere = geografia$cantiere,
    comune = geografia$comune,
    numero_raccolte_annue_previste = solo_censiti(
      letture$numero_raccolte_annue_previste
    ),
    volume_previsto = solo_censiti(letture$volume_previsto),
    letture = tabulate(rfid, n_rfid)
  )
}

#' Tabella delle letture per anno, una riga per RFID
#'
#' Una colonna `letture_<anno>` per ogni anno dei dati, con la somma delle
#' letture dei due sistemi. Il valore manca negli anni che precedono la prima
#' lettura dell'RFID ed è zero negli anni successivi senza letture.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @param anagrafica Risultato di `anagrafica_rfid()`: decide gli RFID.
#' @return Tibble con `RFID`, le colonne degli anni, `cantiere`, `comune` e
#'   `numero_raccolte_annue_previste`.
#' @noRd
tabella_letture_annuali <- function(unite, anagrafica) {
  anni <- seq(
    lubridate::year(min(unite$giorno_lettura)),
    lubridate::year(max(unite$giorno_lettura))
  )
  per_anno <- griglia_annuale(unite, anagrafica$RFID) |>
    dplyr::mutate(anno = factor(.data$anno, levels = anni)) |>
    tidyr::pivot_wider(
      id_cols = "RFID",
      names_from = "anno",
      values_from = "letture",
      names_prefix = "letture_",
      names_expand = TRUE
    )
  anagrafica |>
    dplyr::select(dplyr::all_of("RFID")) |>
    dplyr::left_join(per_anno, by = "RFID") |>
    dplyr::left_join(
      anagrafica[, c(
        "RFID",
        "cantiere",
        "comune",
        "numero_raccolte_annue_previste"
      )],
      by = "RFID"
    )
}

# ---- Confronto tra i due sistemi ---------------------------------------------

#' Letture medie per contenitore, anno per anno
#'
#' Considera i contenitori con le raccolte annue previste. Un contenitore
#' entra nel conteggio dall'anno successivo a quello della sua prima lettura:
#' da lì in poi era di sicuro in servizio per tutto l'anno. Gli anni incompleti
#' sono riportati a dodici mesi. Le letture utili di un contenitore non
#' superano mai le raccolte previste.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @param anagrafica Risultato di `anagrafica_rfid()`.
#' @param per Colonne dell'anagrafica per cui raggruppare: `"cantiere"`,
#'   `"comune"` o entrambe. Gli RFID senza quel dato restano fuori.
#' @return Tibble con le colonne di `per`, `anno`, `sistema`, `completo`,
#'   `n_rfid`, `letture_medie`, `letture_utili_medie`, `raccolte_medie`,
#'   `tasso` (letture utili su raccolte previste).
#' @noRd
media_annuale <- function(unite, anagrafica, per = NULL) {
  anagrafica <- anagrafica[
    !is.na(anagrafica$numero_raccolte_annue_previste),
    ,
    drop = FALSE
  ]
  if (!is.null(per)) {
    anagrafica <- tidyr::drop_na(anagrafica, dplyr::all_of(per))
  }
  copertura <- copertura_annuale(unite)

  griglia_annuale(unite, anagrafica$RFID) |>
    dplyr::filter(.data$anno > .data$primo_anno) |>
    dplyr::left_join(
      anagrafica[, c("RFID", per, "numero_raccolte_annue_previste")],
      by = "RFID"
    ) |>
    dplyr::left_join(
      copertura[, c("anno", "sistema", "completo", "fattore")],
      by = "anno"
    ) |>
    dplyr::mutate(
      letture = .data$letture * .data$fattore,
      utili = pmin(.data$letture, .data$numero_raccolte_annue_previste)
    ) |>
    dplyr::summarise(
      n_rfid = dplyr::n(),
      letture_medie = mean(.data$letture),
      letture_utili_medie = mean(.data$utili),
      raccolte_medie = mean(.data$numero_raccolte_annue_previste),
      .by = dplyr::all_of(c(per, "anno", "sistema", "completo"))
    ) |>
    dplyr::mutate(tasso = .data$letture_utili_medie / .data$raccolte_medie) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(per, "anno"))))
}

#' Confronto tra i due sistemi sugli stessi RFID
#'
#' Tiene gli RFID letti da entrambi i sistemi. Per il sistema precedente
#' contano solo gli anni interi, coperti soltanto dalle letture storiche e
#' successivi all'anno della prima lettura dell'RFID: in quegli anni il
#' contenitore era di sicuro in servizio. Un anno vuoto è uno di questi anni
#' senza nemmeno una lettura. Per le antenne le letture sono riportate a un
#' anno in base ai giorni coperti dalle antenne.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @param anagrafica Risultato di `anagrafica_rfid()`.
#' @return Una riga per RFID, con `cantiere`, `comune`,
#'   `numero_raccolte_annue_previste`, `volume_previsto`, `anni_servizio`,
#'   `anni_vuoti`, `letture_annue_storico`, `letture_annue_antenne` e, dove le
#'   raccolte previste sono note, le letture utili (mai più delle raccolte
#'   previste) e i tassi dei due sistemi. L'attributo `giorni_antenne`
#'   riporta i giorni coperti dalle antenne, `anni_storico` gli anni interi
#'   del sistema precedente.
#' @noRd
confronto_sistemi <- function(unite, anagrafica) {
  copertura <- copertura_annuale(unite)
  anni_storico <- copertura$anno[
    copertura$sistema == "storico" & copertura$completo
  ]
  giorni_antenne <- sum(copertura$giorni_antenne)
  griglia <- griglia_annuale(unite, anagrafica$RFID)

  # Sistema precedente: anni interi in cui il contenitore era in servizio.
  servizio <- griglia[
    griglia$anno %in% anni_storico & griglia$anno > griglia$primo_anno,
    ,
    drop = FALSE
  ]
  rfid <- indice_gruppi(servizio$RFID)
  n_rfid <- if (length(rfid) > 0) max(rfid) else 0L
  prima <- dplyr::tibble(
    RFID = primo_per_gruppo(servizio$RFID, rfid, n_rfid),
    anni_servizio = tabulate(rfid, n_rfid),
    anni_vuoti = as.integer(somma_per_gruppo(
      servizio$storico == 0,
      rfid,
      n_rfid
    )),
    letture_annue_storico = media_per_gruppo(servizio$storico, rfid, n_rfid)
  )

  # Antenne: letture di tutto il periodo, riportate a un anno.
  con_antenne <- griglia[griglia$antenne > 0, , drop = FALSE]
  rfid <- indice_gruppi(con_antenne$RFID)
  n_rfid <- if (length(rfid) > 0) max(rfid) else 0L
  dopo <- dplyr::tibble(
    RFID = primo_per_gruppo(con_antenne$RFID, rfid, n_rfid),
    letture_annue_antenne = somma_per_gruppo(
      con_antenne$antenne,
      rfid,
      n_rfid
    ) *
      365 /
      giorni_antenne
  )

  confronto <- prima |>
    dplyr::inner_join(dopo, by = "RFID") |>
    dplyr::left_join(
      anagrafica[, c(
        "RFID",
        "cantiere",
        "comune",
        "numero_raccolte_annue_previste",
        "volume_previsto"
      )],
      by = "RFID"
    ) |>
    dplyr::mutate(
      letture_utili_storico = pmin(
        .data$letture_annue_storico,
        .data$numero_raccolte_annue_previste
      ),
      letture_utili_antenne = pmin(
        .data$letture_annue_antenne,
        .data$numero_raccolte_annue_previste
      ),
      tasso_storico = .data$letture_utili_storico /
        .data$numero_raccolte_annue_previste,
      tasso_antenne = .data$letture_utili_antenne /
        .data$numero_raccolte_annue_previste
    ) |>
    dplyr::arrange(.data$RFID)
  structure(
    confronto,
    giorni_antenne = giorni_antenne,
    anni_storico = anni_storico
  )
}

#' Riepilogo del confronto tra i due sistemi
#'
#' Le prime colonne riguardano tutti gli RFID del confronto, quelle da
#' `n_con_raccolte` in poi solo gli RFID con le raccolte annue previste. Il
#' tasso è la quota delle raccolte previste che risulta letta. Le letture
#' recuperate sono quelle utili che in un anno le antenne registrano in più,
#' i litri recuperati le pesano con il volume del contenitore.
#'
#' @param confronto Risultato di `confronto_sistemi()`.
#' @param per Colonne per cui raggruppare: `"cantiere"`, `"comune"` o
#'   entrambe. Senza, il riepilogo riguarda tutta la zona servita.
#' @return Una riga in tutto, oppure una per gruppo.
#' @noRd
riepilogo_confronto <- function(confronto, per = NULL) {
  confronto |>
    dplyr::mutate(
      con_raccolte = !is.na(.data$numero_raccolte_annue_previste),
      recuperate = .data$letture_utili_antenne - .data$letture_utili_storico
    ) |>
    dplyr::summarise(
      n_rfid = dplyr::n(),
      rfid_con_anni_vuoti = sum(.data$anni_vuoti > 0),
      totale_anni_servizio = sum(.data$anni_servizio),
      totale_anni_vuoti = sum(.data$anni_vuoti),
      media_storico = mean(.data$letture_annue_storico),
      media_antenne = mean(.data$letture_annue_antenne),
      n_con_raccolte = sum(.data$con_raccolte),
      raccolte_previste = sum(
        .data$numero_raccolte_annue_previste,
        na.rm = TRUE
      ),
      utili_storico = sum(.data$letture_utili_storico, na.rm = TRUE),
      utili_antenne = sum(.data$letture_utili_antenne, na.rm = TRUE),
      litri_recuperati = sum(
        .data$recuperate * .data$volume_previsto,
        na.rm = TRUE
      ),
      .by = dplyr::all_of(per)
    ) |>
    dplyr::mutate(
      quota_anni_vuoti = .data$totale_anni_vuoti / .data$totale_anni_servizio,
      rapporto = .data$media_antenne / .data$media_storico,
      tasso_storico = .data$utili_storico / .data$raccolte_previste,
      tasso_antenne = .data$utili_antenne / .data$raccolte_previste,
      letture_recuperate = .data$utili_antenne - .data$utili_storico
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(per)))
}

#' Media con intervallo di confidenza
#'
#' Intervallo basato sulla distribuzione t di Student, adatto a una media su
#' molti contenitori.
#'
#' @param x Valori numerici. I mancanti sono ignorati.
#' @param livello Livello di confidenza.
#' @return Vettore con `media`, `inferiore`, `superiore`.
#' @noRd
intervallo_media <- function(x, livello = 0.95) {
  x <- x[!is.na(x)]
  n <- length(x)
  media <- if (n > 0) mean(x) else NA_real_
  margine <- if (n > 1) {
    stats::qt(1 - (1 - livello) / 2, n - 1) * stats::sd(x) / sqrt(n)
  } else {
    NA_real_
  }
  c(media = media, inferiore = media - margine, superiore = media + margine)
}

#' RFID letti solo dal sistema precedente
#'
#' Sono contenitori ritirati, oppure contenitori che le antenne non hanno
#' ancora letto. L'anno dell'ultima lettura aiuta a distinguerli.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @return Una riga per RFID, con `letture`, `prima_lettura`, `ultima_lettura`
#'   e `ultimo_anno`.
#' @noRd
rfid_solo_storico <- function(unite) {
  con_antenne <- unique(unite$RFID[unite$fonte == "antenne"])
  soli <- unite[
    unite$fonte == "storico" & !unite$RFID %in% con_antenne,
    ,
    drop = FALSE
  ]
  if (nrow(soli) == 0) {
    return(dplyr::tibble(
      RFID = character(0),
      letture = integer(0),
      prima_lettura = soli$giorno_lettura,
      ultima_lettura = soli$giorno_lettura,
      ultimo_anno = integer(0)
    ))
  }
  soli <- soli[order(soli$RFID, soli$giorno_lettura, method = "radix"), ]
  rfid <- indice_gruppi(soli$RFID)
  n_rfid <- max(rfid)
  ultima <- ultimo_per_gruppo(soli$giorno_lettura, rfid, n_rfid)
  dplyr::tibble(
    RFID = primo_per_gruppo(soli$RFID, rfid, n_rfid),
    letture = tabulate(rfid, n_rfid),
    prima_lettura = primo_per_gruppo(soli$giorno_lettura, rfid, n_rfid),
    ultima_lettura = ultima,
    ultimo_anno = as.integer(lubridate::year(ultima))
  )
}
