# Confronto tra il sistema precedente e le antenne.
#
# Serve al report `docs/beneficio_antenne.Rmd`: l'app non richiama queste
# funzioni. Il confronto riguarda la coorte, cioè i contenitori letti da tutti
# e due i sistemi, e conta una lettura al giorno per contenitore. I contenitori
# letti solo dalle antenne restano fuori dal confronto: vedi
# `coorte_dei_due_sistemi()`.
#
# Il sistema precedente si misura nel periodo di riferimento: i dodici mesi
# che precedono la prima lettura con le antenne, o la parte di quei mesi
# coperta dalle letture storiche. Le antenne si misurano dal giorno dopo la
# prima lettura di ogni contenitore: quella lettura fa entrare il contenitore
# nella coorte e non si conta. Contarla gonfierebbe il ritmo delle antenne,
# perché la coorte è scelta proprio tra i contenitori che hanno almeno una
# lettura.

# ---- Letture e periodi -------------------------------------------------------

#' Tiene una lettura al giorno per contenitore e per sistema
#'
#' Un contenitore viene svuotato al più una volta al giorno: due letture
#' nello stesso giorno sono lo stesso svuotamento, letto due volte.
#'
#' @param unite Risultato di `unisci_letture()`.
#' @return Le stesse colonne, in ordine di tempo, con la prima lettura di ogni
#'   RFID in ogni giorno e per ogni sistema.
#' @noRd
una_lettura_al_giorno <- function(unite) {
  if (nrow(unite) == 0) {
    return(unite)
  }
  unite <- unite[order(unite$giorno_lettura, method = "radix"), , drop = FALSE]
  giorno <- as.integer(lubridate::as_date(unite$giorno_lettura))
  chiave <- (indice_gruppi(unite$RFID) * 1e5 + (giorno - min(giorno))) * 2 +
    (unite$fonte == "antenne")
  unite[!duplicated(chiave), , drop = FALSE]
}

#' Periodi su cui si confrontano i due sistemi
#'
#' Il periodo di riferimento del sistema precedente finisce il giorno prima
#' della prima lettura con le antenne e dura al più `giorni` giorni: se le
#' letture storiche iniziano dopo, parte dalla prima.
#'
#' @param unite Risultato di `unisci_letture()`, con le letture di tutti gli
#'   RFID: i periodi descrivono i due sistemi, non un gruppo di contenitori.
#' @param giorni Durata massima del periodo di riferimento.
#' @return Lista con `storico`, `antenne` e `riferimento`, ciascuno con il
#'   primo e l'ultimo giorno; `giorni_riferimento` e `giorni_antenne`;
#'   `giorni_prima`, i giorni di letture storiche che precedono il periodo di
#'   riferimento; `sovrapposti`, vero se il sistema precedente ha letture
#'   anche dopo l'arrivo delle antenne.
#' @noRd
periodi_confronto <- function(unite, giorni = 365) {
  giorno <- lubridate::as_date(unite$giorno_lettura)
  di_antenne <- unite$fonte == "antenne"
  if (!any(di_antenne)) {
    stop("Servono letture con le antenne.", call. = FALSE)
  }
  antenne <- range(giorno[di_antenne])
  senza_storico <- all(di_antenne)
  storico <- if (senza_storico) {
    as.Date(c(NA, NA))
  } else {
    range(giorno[!di_antenne])
  }
  fine <- antenne[1] - 1
  inizio <- if (senza_storico) fine + 1 else max(storico[1], antenne[1] - giorni)
  list(
    storico = storico,
    antenne = antenne,
    riferimento = c(inizio, fine),
    giorni_riferimento = max(0L, as.integer(fine - inizio) + 1L),
    giorni_antenne = as.integer(antenne[2] - antenne[1]) + 1L,
    giorni_prima = if (senza_storico) {
      0L
    } else {
      max(0L, as.integer(min(inizio, fine + 1) - storico[1]))
    },
    sovrapposti = !senza_storico && storico[2] >= antenne[1]
  )
}

# ---- Confronto sulla coorte --------------------------------------------------

#' Raccolte previste in un anno per ogni contenitore dell'anagrafica
#'
#' Per un censito sono quelle del database. Un non censito non le ha: il
#' numero di passaggi dipende però dal giro, non dal contenitore, quindi
#' valgono le raccolte più frequenti tra i censiti dello stesso servizio nello
#' stesso comune o, se lì non ce ne sono, tra i censiti dello stesso servizio.
#' Restano senza raccolte i non censiti con la stima del servizio incerta.
#'
#' @param anagrafica Risultato di `anagrafica_rfid()`.
#' @return Tibble con `raccolte_previste` e `raccolte_stimate`, vero dove il
#'   valore non viene dal database. Una riga per riga dell'anagrafica.
#' @noRd
raccolte_di_riferimento <- function(anagrafica) {
  del_database <- anagrafica$numero_raccolte_annue_previste
  note <- which(!is.na(del_database) & !is.na(anagrafica$tipologia))
  # Raccolte più frequenti per ogni valore della chiave, tra i censiti.
  piu_frequenti <- function(chiave) {
    if (length(note) == 0) {
      return(rep(NA_real_, nrow(anagrafica)))
    }
    gruppo <- indice_gruppi(chiave[note])
    valori <- piu_frequente_per_gruppo(del_database[note], gruppo)
    valori[match(chiave, chiave[note][!duplicated(gruppo)])]
  }
  nel_comune <- piu_frequenti(paste(
    anagrafica$tipologia,
    anagrafica$comune,
    sep = "\r"
  ))
  nel_comune[is.na(anagrafica$comune)] <- NA
  del_servizio <- piu_frequenti(anagrafica$tipologia)
  raccolte <- dplyr::coalesce(del_database, nel_comune, del_servizio)
  raccolte[is.na(anagrafica$tipologia) & is.na(del_database)] <- NA
  dplyr::tibble(
    raccolte_previste = raccolte,
    raccolte_stimate = is.na(del_database) & !is.na(raccolte)
  )
}

#' Confronto tra i due sistemi sui contenitori letti dalle antenne
#'
#' Una riga per ogni RFID dell'anagrafica, anche se il sistema precedente non
#' lo ha mai letto: quei contenitori non entrano nella coorte, ma il report li
#' conta a parte. Vedi `coorte_dei_due_sistemi()`.
#'
#' Per il sistema precedente contano le letture del periodo di riferimento,
#' riportate a un anno. Per le antenne contano le letture successive al
#' giorno della prima, e i giorni passati da allora alla fine dei dati: il
#' loro rapporto è il ritmo di lettura. Le letture utili di un contenitore non
#' superano le raccolte previste nei giorni osservati.
#'
#' @param unite Risultato di `unisci_letture()`, di norma passato da
#'   `una_lettura_al_giorno()`.
#' @param anagrafica Risultato di `anagrafica_rfid()`: decide la coorte.
#' @param periodi Risultato di `periodi_confronto()`, calcolato sulle letture
#'   di tutti gli RFID.
#' @return Tibble con `RFID`, `presente_a_database`, `tipologia`, `cantiere`,
#'   `comune`, `raccolte_previste` e `raccolte_stimate`, vedi
#'   `raccolte_di_riferimento()`, `volume_previsto`; per il
#'   sistema precedente `letture_riferimento`, `letture_annue_storico`,
#'   `letture_prima` (prima del periodo di riferimento),
#'   `letture_sovrapposte` (dopo l'arrivo delle antenne), `letture_storico`
#'   (tutte, cioè la somma delle tre) e `prima_lettura_storico`; per le
#'   antenne `letture_antenne`,
#'   `prima_lettura_antenne`, `giorni_dopo`, `letture_dopo` e
#'   `letture_annue_antenne`; per i contenitori con le raccolte previste
#'   `previste_storico`, `utili_storico`, `previste_antenne`,
#'   `utili_antenne`. L'attributo `periodi` riporta i periodi usati.
#' @noRd
confronto_sistemi <- function(
  unite,
  anagrafica,
  periodi = periodi_confronto(unite)
) {
  n <- nrow(anagrafica)
  riga <- match(unite$RFID, anagrafica$RFID)
  tenute <- which(!is.na(riga))
  riga <- riga[tenute]
  giorno <- as.integer(lubridate::as_date(unite$giorno_lettura[tenute]))
  storico <- unite$fonte[tenute] == "storico"
  conta <- function(quali) tabulate(riga[quali], n)
  # Primo giorno di lettura di ogni contenitore, tra le letture indicate.
  primo_giorno <- function(quali) {
    scelte <- which(quali)
    scelte <- scelte[order(riga[scelte], giorno[scelte], method = "radix")]
    scelte <- scelte[!duplicated(riga[scelte])]
    esito <- rep(NA_integer_, n)
    esito[riga[scelte]] <- giorno[scelte]
    esito
  }
  riferimento <- as.integer(periodi$riferimento)
  inizio_antenne <- as.integer(periodi$antenne[1])
  fine_antenne <- as.integer(periodi$antenne[2])
  giorni_riferimento <- periodi$giorni_riferimento

  prima_antenne <- primo_giorno(!storico)
  giorni_dopo <- dplyr::coalesce(fine_antenne - prima_antenne, 0L)
  letture_riferimento <- conta(
    storico & giorno >= riferimento[1] & giorno <= riferimento[2]
  )
  letture_dopo <- conta(!storico & giorno > prima_antenne[riga])
  riferimento_raccolte <- raccolte_di_riferimento(anagrafica)
  raccolte <- riferimento_raccolte$raccolte_previste
  previste_storico <- raccolte * giorni_riferimento / 365
  previste_antenne <- raccolte * giorni_dopo / 365

  confronto <- dplyr::tibble(
    RFID = anagrafica$RFID,
    presente_a_database = anagrafica$presente_a_database,
    tipologia = anagrafica$tipologia,
    cantiere = anagrafica$cantiere,
    comune = anagrafica$comune,
    raccolte_previste = raccolte,
    raccolte_stimate = riferimento_raccolte$raccolte_stimate,
    volume_previsto = anagrafica$volume_previsto,
    letture_riferimento = letture_riferimento,
    letture_annue_storico = letture_riferimento * 365 / giorni_riferimento,
    letture_prima = conta(storico & giorno < riferimento[1]),
    letture_sovrapposte = conta(storico & giorno >= inizio_antenne),
    letture_storico = conta(storico),
    prima_lettura_storico = lubridate::as_date(primo_giorno(storico)),
    letture_antenne = conta(!storico),
    prima_lettura_antenne = lubridate::as_date(prima_antenne),
    giorni_dopo = giorni_dopo,
    letture_dopo = letture_dopo,
    letture_annue_antenne = dplyr::if_else(
      giorni_dopo > 0,
      365 * letture_dopo / giorni_dopo,
      NA_real_
    ),
    previste_storico = previste_storico,
    utili_storico = pmin(letture_riferimento, previste_storico),
    previste_antenne = previste_antenne,
    utili_antenne = pmin(letture_dopo, previste_antenne)
  )
  structure(confronto, periodi = periodi)
}

#' Contenitori letti da tutti e due i sistemi
#'
#' La coorte del confronto, per scelta del proprietario del progetto: le
#' antenne coprono poche settimane e una parte dei mezzi, quindi si confrontano
#' i soli contenitori che compaiono in tutti e due i file. Un contenitore letto
#' dalle antenne e mai dal sistema precedente può essere nuovo, oppure avere le
#' letture precedenti sotto un altro codice: non si può dire, e resta fuori.
#'
#' Entra anche il contenitore che il sistema precedente ha letto solo dopo
#' l'arrivo delle antenne: nel periodo di riferimento risulta senza letture.
#' `riepilogo_confronto()` li conta in `solo_dopo`.
#'
#' @param confronto Risultato di `confronto_sistemi()`.
#' @return Le righe con almeno una lettura del sistema precedente, con le
#'   stesse colonne.
#' @noRd
coorte_dei_due_sistemi <- function(confronto) {
  confronto[confronto$letture_storico > 0, , drop = FALSE]
}

#' Riepilogo del confronto tra i due sistemi
#'
#' Le letture in un anno del sistema precedente sono la media sui contenitori.
#' Il ritmo delle antenne è il totale delle letture successive alla prima
#' diviso per il totale dei giorni osservati, riportato a un anno: così un
#' contenitore entrato nella coorte da pochi giorni pesa poco. Il tasso di
#' lettura, calcolato sui contenitori con le raccolte previste, è la quota
#' delle raccolte previste nei giorni osservati che risulta letta.
#'
#' @param confronto Risultato di `confronto_sistemi()`.
#' @param per Colonne per cui raggruppare, ad esempio `"cantiere"`, `"comune"`
#'   o `"tipologia"`. Senza, il riepilogo riguarda tutti i contenitori.
#' @return Una riga in tutto, oppure una per gruppo, con: `n_rfid`, `censiti`;
#'   `mai_letti` e `una_o_meno`, i contenitori con zero e con al più una
#'   lettura nel periodo di riferimento, e le loro quote; `gia_in_servizio`, i
#'   contenitori che il sistema precedente aveva letto prima del periodo di
#'   riferimento, con `mai_letti_in_servizio` e `una_o_meno_in_servizio`;
#'   `solo_dopo`, i contenitori che il sistema precedente ha letto solo dopo
#'   l'arrivo delle antenne;
#'   `media_storico`, `mediana_storico`, `media_antenne`, `rapporto`;
#'   `n_con_raccolte`, `raccolte_previste`, `tasso_storico`, `tasso_antenne`,
#'   `letture_recuperate` in un anno.
#' @noRd
riepilogo_confronto <- function(confronto, per = NULL) {
  confronto |>
    dplyr::mutate(
      con_raccolte = !is.na(.data$raccolte_previste),
      in_servizio = .data$letture_prima > 0,
      dopo = .data$letture_sovrapposte > 0 &
        .data$letture_prima == 0 &
        .data$letture_riferimento == 0
    ) |>
    dplyr::summarise(
      n_rfid = dplyr::n(),
      censiti = sum(.data$presente_a_database == "Presente"),
      mai_letti = sum(.data$letture_riferimento == 0),
      una_o_meno = sum(.data$letture_riferimento <= 1),
      gia_in_servizio = sum(.data$in_servizio),
      mai_letti_in_servizio = sum(
        .data$in_servizio & .data$letture_riferimento == 0
      ),
      una_o_meno_in_servizio = sum(
        .data$in_servizio & .data$letture_riferimento <= 1
      ),
      solo_dopo = sum(.data$dopo),
      media_storico = mean(.data$letture_annue_storico),
      mediana_storico = stats::median(.data$letture_annue_storico),
      letture_dopo = sum(.data$letture_dopo),
      giorni_dopo = sum(.data$giorni_dopo),
      n_con_raccolte = sum(.data$con_raccolte),
      raccolte_previste = sum(.data$raccolte_previste, na.rm = TRUE),
      previste_storico = sum(.data$previste_storico, na.rm = TRUE),
      utili_storico = sum(.data$utili_storico, na.rm = TRUE),
      previste_antenne = sum(.data$previste_antenne, na.rm = TRUE),
      utili_antenne = sum(.data$utili_antenne, na.rm = TRUE),
      .by = dplyr::all_of(per)
    ) |>
    dplyr::mutate(
      quota_mai_letti = .data$mai_letti / .data$n_rfid,
      quota_una_o_meno = .data$una_o_meno / .data$n_rfid,
      media_antenne = 365 * .data$letture_dopo / .data$giorni_dopo,
      rapporto = .data$media_antenne / .data$media_storico,
      tasso_storico = .data$utili_storico / .data$previste_storico,
      tasso_antenne = .data$utili_antenne / .data$previste_antenne,
      letture_recuperate = (.data$tasso_antenne - .data$tasso_storico) *
        .data$raccolte_previste
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(per)))
}

#' Intervallo di confidenza di una statistica fatta di somme
#'
#' Metodo jackknife: la statistica viene ricalcolata togliendo un contenitore
#' alla volta, e la variabilità di quei valori dà l'errore della stima. Non
#' usa numeri casuali. Per una media dà lo stesso intervallo della
#' distribuzione t di Student.
#'
#' @param colonne Data frame con una riga per contenitore e una colonna per
#'   ogni somma che entra nella statistica. Non deve avere mancanti.
#' @param statistica Funzione che riceve la lista delle somme, con i nomi
#'   delle colonne, e restituisce la statistica. Deve lavorare su vettori:
#'   viene chiamata una volta con i totali e una con i totali senza ciascun
#'   contenitore.
#' @param livello Livello di confidenza.
#' @return Vettore con `stima`, `inferiore`, `superiore`.
#' @noRd
intervallo_jackknife <- function(colonne, statistica, livello = 0.95) {
  colonne <- as.list(colonne)
  n <- length(colonne[[1]])
  totali <- lapply(colonne, sum)
  stima <- if (n > 0) statistica(totali) else NA_real_
  margine <- NA_real_
  if (n > 1) {
    senza <- statistica(Map(function(totale, x) totale - x, totali, colonne))
    errore <- sqrt((n - 1) / n * sum((senza - mean(senza))^2))
    margine <- stats::qt(1 - (1 - livello) / 2, n - 1) * errore
  }
  c(stima = stima, inferiore = stima - margine, superiore = stima + margine)
}

# ---- Andamento nel tempo -----------------------------------------------------

#' Letture e contenitori letti, mese per mese e per sistema
#'
#' @param unite Risultato di `unisci_letture()`.
#' @param rfid RFID da tenere. Tutti, se manca.
#' @return Tibble con `mese` (il suo primo giorno), `fonte`, `letture` e
#'   `contenitori`, il numero di RFID diversi letti nel mese. Per ogni sistema
#'   ci sono tutti i mesi dalla sua prima alla sua ultima lettura: quelli
#'   senza letture hanno zero.
#' @noRd
letture_mensili <- function(unite, rfid = NULL) {
  if (!is.null(rfid)) {
    unite <- unite[unite$RFID %in% rfid, , drop = FALSE]
  }
  vuoto <- dplyr::tibble(
    mese = as.Date(character(0)),
    fonte = character(0),
    letture = integer(0),
    contenitori = integer(0)
  )
  if (nrow(unite) == 0) {
    return(vuoto)
  }
  conteggi <- dplyr::tibble(
    mese = lubridate::floor_date(
      lubridate::as_date(unite$giorno_lettura),
      "month"
    ),
    fonte = unite$fonte,
    RFID = unite$RFID
  ) |>
    dplyr::summarise(
      letture = dplyr::n(),
      contenitori = dplyr::n_distinct(.data$RFID),
      .by = c("mese", "fonte")
    )
  mesi <- purrr::map(split(conteggi$mese, conteggi$fonte), function(x) {
    seq(min(x), max(x), by = "month")
  })
  dplyr::tibble(
    mese = do.call(c, unname(mesi)),
    fonte = rep(names(mesi), lengths(mesi))
  ) |>
    dplyr::left_join(conteggi, by = c("mese", "fonte")) |>
    dplyr::mutate(
      letture = dplyr::coalesce(.data$letture, 0L),
      contenitori = dplyr::coalesce(.data$contenitori, 0L)
    ) |>
    dplyr::arrange(.data$mese, .data$fonte)
}

#' Mezzi attivi e contenitori letti dalle antenne, settimana per settimana
#'
#' @param letture Letture con le antenne.
#' @return Tibble con `settimana` (il suo lunedì), `mezzi`, `contenitori`,
#'   `nuovi` (i contenitori letti per la prima volta), `letture` e `cumulati`,
#'   i contenitori diversi letti fino a quella settimana. Le settimane senza
#'   letture mancano.
#' @noRd
copertura_settimanale <- function(letture) {
  letture <- letture[
    order(letture$giorno_lettura, method = "radix"),
    ,
    drop = FALSE
  ]
  dplyr::tibble(
    settimana = lubridate::floor_date(
      lubridate::as_date(letture$giorno_lettura),
      "week",
      week_start = 1
    ),
    mezzo = letture$matricola_veicolo,
    RFID = letture$RFID,
    nuovo = !duplicated(letture$RFID)
  ) |>
    dplyr::summarise(
      mezzi = dplyr::n_distinct(.data$mezzo),
      contenitori = dplyr::n_distinct(.data$RFID),
      nuovi = sum(.data$nuovo),
      letture = dplyr::n(),
      .by = "settimana"
    ) |>
    dplyr::arrange(.data$settimana) |>
    dplyr::mutate(cumulati = cumsum(.data$nuovi))
}

# ---- Codici letti male -------------------------------------------------------

#' Coppie di codici RFID che differiscono per un solo carattere
#'
#' Un codice letto o scritto male per un bit differisce da quello giusto in
#' un carattere solo. Anche due contenitori diversi possono avere codici così
#' vicini, ad esempio due numeri di serie consecutivi: la coppia è un indizio,
#' non una prova. Vedi `gemelli_sospetti()`.
#'
#' Per ogni posizione i codici si raggruppano dopo aver tolto il carattere di
#' quella posizione: due codici nello stesso gruppo differiscono solo lì.
#'
#' @param codici Codici RFID. I doppioni e i mancanti sono ignorati.
#' @param tra Codici di cui interessano i gemelli: restano le sole coppie con
#'   almeno un codice tra questi. Senza, restano tutte. Con centinaia di
#'   migliaia di codici vicini tra loro le coppie sono milioni: indicare i soli
#'   codici letti dalle antenne le riduce a quelle che servono.
#' @return Tibble con `codice_a` e `codice_b`, in ordine alfabetico,
#'   `posizione` del carattere diverso, `carattere_a`, `carattere_b` e
#'   `un_bit`: vero se i due caratteri, letti come cifre esadecimali,
#'   differiscono per un bit solo; mancante se non sono cifre esadecimali.
#' @noRd
codici_gemelli <- function(codici, tra = NULL) {
  codici <- unique(as.character(codici[!is.na(codici)]))
  scelto <- if (is.null(tra)) rep(TRUE, length(codici)) else codici %in% tra
  vuoto <- dplyr::tibble(
    codice_a = character(0),
    codice_b = character(0),
    posizione = integer(0),
    carattere_a = character(0),
    carattere_b = character(0),
    un_bit = logical(0)
  )
  if (length(codici) < 2) {
    return(vuoto)
  }
  lunghezza <- nchar(codici)
  coppie <- purrr::map(seq_len(max(lunghezza)), function(posizione) {
    scelti <- which(lunghezza >= posizione)
    testo <- codici[scelti]
    chiave <- paste0(
      substr(testo, 1, posizione - 1),
      "\r",
      substr(testo, posizione + 1, lunghezza[scelti])
    )
    ripetute <- chiave %in% chiave[duplicated(chiave)] &
      chiave %in% chiave[scelto[scelti]]
    if (!any(ripetute)) {
      return(NULL)
    }
    gruppo <- dplyr::tibble(
      chiave = chiave[ripetute],
      codice = testo[ripetute],
      scelto = scelto[scelti][ripetute]
    )
    # A sinistra i soli codici scelti: una coppia di due codici scelti esce due
    # volte, e si tiene quella in ordine alfabetico.
    unite <- dplyr::inner_join(
      gruppo[gruppo$scelto, , drop = FALSE],
      gruppo,
      by = "chiave",
      suffix = c("_a", "_b"),
      relationship = "many-to-many"
    )
    unite <- unite[
      unite$codice_a != unite$codice_b &
        (unite$codice_a < unite$codice_b | !unite$scelto_b),
      ,
      drop = FALSE
    ]
    dplyr::tibble(
      codice_a = pmin(unite$codice_a, unite$codice_b),
      codice_b = pmax(unite$codice_a, unite$codice_b),
      posizione = posizione
    )
  })
  coppie <- dplyr::bind_rows(coppie)
  if (nrow(coppie) == 0) {
    return(vuoto)
  }
  coppie$carattere_a <- substr(
    coppie$codice_a,
    coppie$posizione,
    coppie$posizione
  )
  coppie$carattere_b <- substr(
    coppie$codice_b,
    coppie$posizione,
    coppie$posizione
  )
  valore_a <- strtoi(coppie$carattere_a, 16L)
  valore_b <- strtoi(coppie$carattere_b, 16L)
  esadecimali <- !is.na(valore_a) & !is.na(valore_b)
  coppie$un_bit <- NA
  coppie$un_bit[esadecimali] <- bitwXor(
    valore_a[esadecimali],
    valore_b[esadecimali]
  ) %in%
    c(1L, 2L, 4L, 8L)
  coppie[order(coppie$codice_a, coppie$codice_b, method = "radix"), ]
}

#' Coppie di codici che potrebbero essere lo stesso contenitore
#'
#' Tiene le coppie di `codici_gemelli()` in cui un codice solo è censito:
#' quello è il codice giusto, l'altro il gemello, che il database non
#' conosce. Per ogni coppia raccoglie gli indizi che i dati permettono.
#'
#' - Uno svuotamento dà una lettura sola: se i due codici sono lo stesso
#'   contenitore non compaiono mai nello stesso giorno. Due contenitori vicini,
#'   serviti dallo stesso giro, compaiono invece negli stessi giorni.
#' - Lo stesso contenitore è servito negli stessi giorni della settimana.
#' - Con le antenne ogni lettura ha una posizione: se anche il gemello è
#'   stato letto dalle antenne, la distanza tra i due dice se è lo stesso
#'   punto.
#'
#' Una coppia è sospetta se i due codici non compaiono mai nello stesso
#' giorno, se almeno `quota_giorni` delle letture del gemello cadono nei
#' giorni della settimana del codice giusto, e se la differenza tra i codici,
#' quando si può dire, è di un bit solo. Se ci sono le posizioni di entrambi,
#' devono distare meno di `distanza_massima_m`.
#'
#' @param coppie Risultato di `codici_gemelli()`.
#' @param unite Risultato di `unisci_letture()`, con una lettura al giorno.
#' @param letture Letture con le antenne: danno lo stato a database e la
#'   posizione.
#' @param quota_giorni Quota minima delle letture del gemello nei giorni
#'   della settimana del codice giusto.
#' @param distanza_massima_m Distanza massima tra le posizioni dei due codici.
#' @return Tibble con `codice`, `gemello`, `posizione`, `un_bit`,
#'   `gemello_letto_da` (`"antenne"`, `"sistema precedente"` o `"entrambi"`),
#'   `letture_codice`, `letture_gemello`, `quota_gemello` (la quota delle
#'   letture del contenitore finite sotto il codice sbagliato);
#'   `letture_codice_storico`, `letture_gemello_storico` e
#'   `quota_gemello_storico`, gli stessi conti sulle sole letture del sistema
#'   precedente; `giorni_in_comune`, `quota_stessi_giorni`, `distanza_m` e
#'   `sospetto`.
#'   Prima le coppie sospette, poi per letture del gemello.
#' @noRd
gemelli_sospetti <- function(
  coppie,
  unite,
  letture,
  quota_giorni = 0.8,
  distanza_massima_m = 200
) {
  ultime <- letture[
    order(letture$giorno_lettura, method = "radix"),
    c("RFID", "presente_a_database", "latitudine", "longitudine")
  ]
  ultime <- ultime[!duplicated(ultime$RFID, fromLast = TRUE), , drop = FALSE]
  censito_a <- ultime$presente_a_database[match(coppie$codice_a, ultime$RFID)] %in%
    "Presente"
  censito_b <- ultime$presente_a_database[match(coppie$codice_b, ultime$RFID)] %in%
    "Presente"
  tenute <- which(censito_a != censito_b)
  esito <- dplyr::tibble(
    codice = ifelse(censito_a, coppie$codice_a, coppie$codice_b)[tenute],
    gemello = ifelse(censito_a, coppie$codice_b, coppie$codice_a)[tenute],
    posizione = coppie$posizione[tenute],
    un_bit = coppie$un_bit[tenute]
  )

  # Giorni di lettura di ogni codice, senza distinguere il sistema.
  indizi <- indizi_giorni(esito$codice, esito$gemello, unite)
  letture_codice <- indizi$letture_codice
  letture_gemello <- indizi$letture_gemello

  # Letture del sistema precedente, per codice.
  coinvolti <- unique(c(esito$codice, esito$gemello))
  proprie <- unite[unite$RFID %in% coinvolti, , drop = FALSE]
  storiche <- tabulate(
    match(proprie$RFID[proprie$fonte == "storico"], coinvolti),
    length(coinvolti)
  )
  codice_storico <- storiche[match(esito$codice, coinvolti)]
  gemello_storico <- storiche[match(esito$gemello, coinvolti)]
  da_antenne <- esito$gemello %in% proprie$RFID[proprie$fonte == "antenne"]
  da_storico <- gemello_storico > 0
  posizione_codice <- match(esito$codice, ultime$RFID)
  posizione_gemello <- match(esito$gemello, ultime$RFID)

  esito$gemello_letto_da <- dplyr::case_when(
    da_antenne & da_storico ~ "entrambi",
    da_antenne ~ "antenne",
    .default = "sistema precedente"
  )
  esito$letture_codice <- letture_codice
  esito$letture_gemello <- letture_gemello
  esito$quota_gemello <- letture_gemello / (letture_codice + letture_gemello)
  esito$letture_codice_storico <- codice_storico
  esito$letture_gemello_storico <- gemello_storico
  esito$quota_gemello_storico <- gemello_storico /
    (codice_storico + gemello_storico)
  esito$giorni_in_comune <- indizi$giorni_in_comune
  esito$quota_stessi_giorni <- indizi$quota_stessi_giorni
  esito$distanza_m <- distanza_haversine_m(
    ultime$latitudine[posizione_codice],
    ultime$longitudine[posizione_codice],
    ultime$latitudine[posizione_gemello],
    ultime$longitudine[posizione_gemello]
  )
  esito$sospetto <- esito$letture_gemello > 0 &
    esito$giorni_in_comune == 0 &
    esito$quota_stessi_giorni >= quota_giorni &
    !esito$un_bit %in% FALSE &
    (is.na(esito$distanza_m) | esito$distanza_m <= distanza_massima_m)
  esito[
    order(
      !esito$sospetto,
      -esito$letture_gemello,
      esito$codice,
      method = "radix"
    ),
  ]
}

#' Giorni di lettura di due codici a confronto
#'
#' Per ogni coppia conta i giorni in cui compaiono tutti e due i codici e la
#' quota delle letture del gemello che cade nei giorni della settimana del
#' codice. Il codice e il giorno stanno in un numero solo: così i giorni in
#' comune si trovano con un confronto tra vettori, senza unire tabelle. Con
#' centinaia di migliaia di coppie un'unione riga per riga non starebbe in
#' memoria.
#'
#' @param codice,gemello I due codici di ogni coppia.
#' @param del_codice Letture da cui prendere i giorni del codice: servono
#'   `RFID` e `giorno_lettura`.
#' @param del_gemello Letture da cui prendere i giorni del gemello. Le stesse,
#'   se manca.
#' @return Lista con `letture_codice` e `letture_gemello`, i giorni diversi di
#'   lettura, `giorni_in_comune` e `quota_stessi_giorni`: un valore per coppia.
#' @noRd
indizi_giorni <- function(codice, gemello, del_codice, del_gemello = del_codice) {
  n <- length(codice)
  coinvolti <- unique(c(codice, gemello))
  giorni_di <- function(letture) {
    di_chi <- match(letture$RFID, coinvolti)
    tenute <- which(!is.na(di_chi))
    list(
      di_chi = di_chi[tenute],
      giorno = as.integer(lubridate::as_date(letture$giorno_lettura[tenute]))
    )
  }
  lato_codice <- giorni_di(del_codice)
  lato_gemello <- giorni_di(del_gemello)
  tutti <- c(lato_codice$giorno, lato_gemello$giorno)
  origine <- if (length(tutti) > 0) min(tutti) else 0L
  chiavi <- function(lato) {
    sort(unique(lato$di_chi * 1e5 + (lato$giorno - origine)))
  }
  chiave_codice <- chiavi(lato_codice)
  chiave_gemello <- chiavi(lato_gemello)
  di_chi <- function(chiave) as.integer(chiave %/% 1e5)
  giorno_di <- function(chiave) as.integer(chiave %% 1e5) + origine
  giorni_codice <- tabulate(di_chi(chiave_codice), length(coinvolti))
  giorni_gemello <- tabulate(di_chi(chiave_gemello), length(coinvolti))
  inizio <- cumsum(giorni_gemello) - giorni_gemello + 1L
  # Giorni della settimana in cui ogni codice viene letto, un bit per giorno.
  # Il primo gennaio 1970 era un giovedì.
  bit_del_giorno <- function(g) bitwShiftL(1L, (g + 3L) %% 7L)
  settimana <- unique(
    di_chi(chiave_codice) * 128L + bit_del_giorno(giorno_di(chiave_codice))
  )
  giorni_settimana <- as.integer(somma_per_gruppo(
    settimana %% 128L,
    settimana %/% 128L,
    length(coinvolti)
  ))

  riga_codice <- match(codice, coinvolti)
  riga_gemello <- match(gemello, coinvolti)
  letture_gemello <- giorni_gemello[riga_gemello]
  # Una riga per ogni giorno di lettura del gemello di ogni coppia.
  coppia <- rep.int(seq_len(n), letture_gemello)
  giorno_gemello <- giorno_di(chiave_gemello)[
    sequence(letture_gemello, from = inizio[riga_gemello])
  ]
  in_comune <- (riga_codice[coppia] * 1e5 + (giorno_gemello - origine)) %in%
    chiave_codice
  stesso_giorno <- bitwAnd(
    giorni_settimana[riga_codice[coppia]],
    bit_del_giorno(giorno_gemello)
  ) >
    0L
  list(
    letture_codice = giorni_codice[riga_codice],
    letture_gemello = letture_gemello,
    giorni_in_comune = tabulate(coppia[in_comune], n),
    quota_stessi_giorni = tabulate(coppia[stesso_giorno], n) / letture_gemello
  )
}

#' Codici letti dalle antenne con un gemello noto solo al sistema precedente
#'
#' Un contenitore letto dalle antenne e mai dal sistema precedente può avere
#' le letture precedenti sotto un altro codice, diverso per un carattere: uno
#' dei due sistemi lo registra male. Qui si tengono le coppie di
#' `codici_gemelli()` con un codice letto dalle antenne e l'altro letto solo
#' dal sistema precedente.
#'
#' Gli indizi si calcolano tra i giorni delle antenne del codice e i giorni
#' del sistema precedente del gemello, anche quando il codice ha letture
#' precedenti sue. Così la regola è la stessa per i codici esclusi dalla
#' coorte e per quelli della coorte, che fanno da termine di paragone: vedi
#' `riepilogo_gemelli()`.
#'
#' @param coppie Risultato di `codici_gemelli()`.
#' @param unite Risultato di `unisci_letture()`.
#' @param quota_giorni Quota minima delle letture del gemello nei giorni
#'   della settimana in cui le antenne leggono il codice.
#' @return Tibble con `codice`, letto dalle antenne, e `gemello`, letto solo
#'   dal sistema precedente; `posizione`, `un_bit`; `codice_in_storico`, vero
#'   se anche il codice ha letture del sistema precedente; `letture_codice`, i
#'   giorni di lettura con le antenne, e `letture_gemello`, quelli del sistema
#'   precedente; `giorni_in_comune`, `quota_stessi_giorni`; `compatibile`,
#'   vero se i due codici non compaiono mai nello stesso giorno e il gemello
#'   è letto negli stessi giorni della settimana; `sospetto`, vero se in più
#'   la differenza, quando si può dire, è di un bit solo. Prima le coppie
#'   sospette, poi per letture del gemello.
#' @noRd
gemelli_tra_sistemi <- function(coppie, unite, quota_giorni = 0.8) {
  di_antenne <- unite$fonte == "antenne"
  con_antenne <- unique(unite$RFID[di_antenne])
  letto_a <- coppie$codice_a %in% con_antenne
  letto_b <- coppie$codice_b %in% con_antenne
  tenute <- which(letto_a != letto_b)
  esito <- dplyr::tibble(
    codice = ifelse(letto_a, coppie$codice_a, coppie$codice_b)[tenute],
    gemello = ifelse(letto_a, coppie$codice_b, coppie$codice_a)[tenute],
    posizione = coppie$posizione[tenute],
    un_bit = coppie$un_bit[tenute]
  )
  indizi <- indizi_giorni(
    esito$codice,
    esito$gemello,
    unite[di_antenne, , drop = FALSE],
    unite[!di_antenne, , drop = FALSE]
  )
  esito$codice_in_storico <- esito$codice %in% unite$RFID[!di_antenne]
  esito$letture_codice <- indizi$letture_codice
  esito$letture_gemello <- indizi$letture_gemello
  esito$giorni_in_comune <- indizi$giorni_in_comune
  esito$quota_stessi_giorni <- indizi$quota_stessi_giorni
  esito$compatibile <- esito$letture_gemello > 0 &
    esito$giorni_in_comune == 0 &
    esito$quota_stessi_giorni >= quota_giorni
  esito$sospetto <- esito$compatibile & !esito$un_bit %in% FALSE
  esito[
    order(
      !esito$sospetto,
      !esito$compatibile,
      -esito$letture_gemello,
      esito$codice,
      method = "radix"
    ),
  ]
}

#' Quanti contenitori esclusi hanno un gemello, a confronto con la coorte
#'
#' Con molti codici vicini tra loro quasi ogni codice ha un gemello per caso:
#' il numero degli esclusi con un gemello, da solo, non dice niente. Il
#' termine di paragone sono i contenitori della coorte, che le loro letture
#' precedenti le hanno sotto il proprio codice: tra loro un gemello è quasi
#' sempre un altro contenitore. Solo la parte degli esclusi che supera la
#' quota della coorte si può attribuire a codici registrati male.
#'
#' Se una quota `f` degli esclusi ha le letture precedenti sotto un gemello, e
#' gli altri hanno un gemello per caso quanto la coorte, con quota `c`, la
#' quota osservata tra gli esclusi è `e = f + (1 - f) * c`: da qui
#' `f = (e - c) / (1 - c)`. La stima è prudente: non tutti i codici registrati
#' male superano i controlli, e anche tra i contenitori della coorte qualche
#' gemello è un codice letto male. Vale se i codici degli esclusi sono fitti
#' quanto quelli della coorte.
#'
#' @param gemelli Risultato di `gemelli_tra_sistemi()`.
#' @param esclusi Codici letti dalle antenne e mai dal sistema precedente.
#' @param coorte Codici letti da tutti e due i sistemi.
#' @return Tibble con una riga per indizio, dal più debole al più forte:
#'   `"gemello"`, `"compatibile"`, `"sospetto"`. Per ognuno `esclusi` e
#'   `coorte`, i codici con almeno una coppia di quel tipo, le quote
#'   `quota_esclusi` e `quota_coorte`, `per_caso`, gli esclusi attesi con la
#'   quota della coorte, e `in_piu`, la stima degli esclusi con le letture
#'   precedenti sotto un gemello: mai meno di zero, mancante se tutti i
#'   contenitori della coorte hanno un gemello.
#' @noRd
riepilogo_gemelli <- function(gemelli, esclusi, coorte) {
  esclusi <- unique(esclusi)
  coorte <- unique(coorte)
  indizi <- list(
    gemello = rep(TRUE, nrow(gemelli)),
    compatibile = gemelli$compatibile,
    sospetto = gemelli$sospetto
  )
  conta <- function(codici) {
    vapply(
      indizi,
      function(quali) sum(codici %in% gemelli$codice[quali]),
      numeric(1)
    )
  }
  n_esclusi <- unname(conta(esclusi))
  n_coorte <- unname(conta(coorte))
  # Le quote si calcolano prima: dentro `tibble()` le colonne `esclusi` e
  # `coorte` nasconderebbero i due vettori di codici.
  quota_esclusi <- n_esclusi / length(esclusi)
  quota_coorte <- n_coorte / length(coorte)
  per_caso <- length(esclusi) * quota_coorte
  in_piu <- length(esclusi) *
    pmax(0, (quota_esclusi - quota_coorte) / (1 - quota_coorte))
  in_piu[!is.finite(in_piu) | quota_coorte >= 1] <- NA_real_
  dplyr::tibble(
    indizio = names(indizi),
    esclusi = n_esclusi,
    quota_esclusi = quota_esclusi,
    coorte = n_coorte,
    quota_coorte = quota_coorte,
    per_caso = per_caso,
    in_piu = in_piu
  )
}

# ---- Contenitori lontani dal comune del database -----------------------------

#' Contenitori censiti letti fuori dal comune del database
#'
#' Una riga per ogni RFID censito con almeno una lettura fuori comune, vedi
#' `aggiungi_geografia()`. Si riconoscono solo con le antenne: le letture del
#' sistema precedente non hanno una posizione.
#'
#' @param letture Letture con le antenne, validate.
#' @return Tibble con `RFID`, `cantiere` e `comune_da_database`, quelli del
#'   database; `letto_a`, il comune in cui cade il maggior numero delle
#'   letture fuori comune, mancante se cadono fuori dai comuni serviti;
#'   `letture`, `letture_fuori`, `quota_fuori`; `prima_fuori` e
#'   `ultima_fuori`, i giorni della prima e dell'ultima lettura fuori comune;
#'   `stabile`, vero se anche l'ultima lettura è fuori comune. Prima i
#'   contenitori con più letture fuori comune.
#' @noRd
rfid_fuori_comune <- function(letture) {
  vuoto <- dplyr::tibble(
    RFID = character(0),
    cantiere = character(0),
    comune_da_database = character(0),
    letto_a = character(0),
    letture = integer(0),
    letture_fuori = integer(0),
    quota_fuori = numeric(0),
    prima_fuori = as.Date(character(0)),
    ultima_fuori = as.Date(character(0)),
    stabile = logical(0)
  )
  fuori <- coerenza_letture(letture)$fuori_comune &
    letture$presente_a_database == "Presente"
  if (!any(fuori)) {
    return(vuoto)
  }
  proprie <- letture[
    letture$RFID %in% letture$RFID[fuori] &
      letture$presente_a_database == "Presente",
    ,
    drop = FALSE
  ]
  proprie <- proprie[order(proprie$giorno_lettura, method = "radix"), ]
  fuori <- coerenza_letture(proprie)$fuori_comune
  gruppo <- indice_gruppi(proprie$RFID)
  n <- max(gruppo)
  geografico <- as.character(
    proprie[["comune_geografico"]] %||% rep(NA_character_, nrow(proprie))
  )
  geografico[!fuori] <- NA_character_
  giorno <- lubridate::as_date(proprie$giorno_lettura)
  giorno_fuori <- giorno
  giorno_fuori[!fuori] <- NA
  # Il comune del database al momento dell'ultima lettura fuori comune.
  dichiarato <- as.character(proprie$comune_da_database)
  dichiarato[!fuori] <- NA_character_
  comune <- ultimo_valido_per_gruppo(dichiarato, gruppo, n)
  letture_fuori <- as.integer(somma_per_gruppo(fuori, gruppo, n))
  esito <- dplyr::tibble(
    RFID = primo_per_gruppo(proprie$RFID, gruppo, n),
    cantiere = cantiere_di_comune(comune),
    comune_da_database = comune,
    letto_a = piu_frequente_per_gruppo(geografico, gruppo, n),
    letture = tabulate(gruppo, n),
    letture_fuori = letture_fuori,
    quota_fuori = letture_fuori / tabulate(gruppo, n),
    prima_fuori = lubridate::as_date(primo_per_gruppo(
      as.integer(giorno_fuori)[fuori],
      gruppo[fuori],
      n
    )),
    ultima_fuori = lubridate::as_date(ultimo_valido_per_gruppo(
      as.integer(giorno_fuori),
      gruppo,
      n
    )),
    stabile = ultimo_per_gruppo(fuori, gruppo, n)
  )
  esito[order(-esito$letture_fuori, esito$RFID, method = "radix"), ]
}
