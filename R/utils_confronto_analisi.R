# Analisi del report `docs/confronto_antenne.qmd`, sul confronto tra letture
# storiche (saponette) e letture con le antenne. L'app non usa queste funzioni.
#
# Dove non diversamente detto, `confronto` è la tabella di `confronta_letture()`
# o di `unisci_origini()`: `data_lettura`, `RFID` e `origine`.

#' Letture con `data_lettura` (data) ricavata da `data_ora_lettura`
#' @noRd
con_data_lettura <- function(letture) {
  dplyr::mutate(
    letture,
    data_lettura = lubridate::as_date(.data$data_ora_lettura)
  )
}

#' Solo le letture dei contenitori censiti, se la colonna c'è
#' @noRd
solo_censiti <- function(letture) {
  if (!"presente_a_database" %in% names(letture)) {
    return(letture)
  }
  dplyr::filter(letture, .data$presente_a_database %in% "Presente")
}

#' Letture storiche degli RFID letti dalle antenne, in qualunque giro
#'
#' A differenza di `storiche_di_confronto()`, che tiene tutte le letture dei
#' giri in cui compare un RFID delle antenne, qui restano i soli RFID delle
#' antenne: i due lati confrontano gli stessi contenitori.
#' @noRd
storiche_stessi_chip <- function(letture_storiche, letture_antenne) {
  letture_storiche |>
    con_data_lettura() |>
    dplyr::filter(.data$RFID %in% letture_antenne$RFID)
}

#' Giorni di ogni mese compresi tra `dal` e `al`
#' @param mesi Primo giorno di ogni mese.
#' @noRd
giorni_del_mese <- function(mesi, dal, al) {
  inizio <- pmax(mesi, dal)
  fine <- pmin(lubridate::ceiling_date(mesi, "month") - 1, al)
  pmax(as.integer(fine - inizio) + 1L, 0L)
}

#' RFID letti in un giorno per mese e origine, con la media per giorno
#'
#' I mesi parziali (il primo e l'ultimo del periodo osservato) contano solo i
#' giorni osservati, così le barre si confrontano.
#' @return `mese`, `origine`, `n`, `giorni` e `per_giorno`.
#' @noRd
riepilogo_mensile <- function(
  confronto,
  dal = min(confronto$data_lettura),
  al = max(confronto$data_lettura)
) {
  confronto |>
    dplyr::mutate(mese = lubridate::floor_date(.data$data_lettura, "month")) |>
    dplyr::count(.data$mese, .data$origine, name = "n") |>
    dplyr::mutate(
      giorni = giorni_del_mese(.data$mese, dal, al),
      per_giorno = dplyr::if_else(
        .data$giorni > 0,
        .data$n / .data$giorni,
        NA_real_
      )
    )
}

#' RFID delle storiche dei giri di confronto, per rapporto con le antenne
#'
#' Dice perché le letture storiche dei giri sono più di quelle delle antenne:
#' un giro comprende RFID che le antenne non hanno letto (mezzo non attrezzato,
#' bidone non raccolto da un mezzo attrezzato, bidone non letto dall'antenna).
#' @param storiche_confronto Risultato di `storiche_di_confronto()`.
#' @param antenne Letture con le antenne dopo i filtri.
#' @param antenne_tutte Letture con le antenne, senza filtri.
#' @return `data_lettura`, `RFID` e `gruppo` (fattore).
#' @noRd
composizione_storiche <- function(storiche_confronto, antenne, antenne_tutte) {
  gruppi <- c(
    "RFID letti dalle antenne",
    "RFID letti dalle antenne, ma fuori dai filtri",
    "RFID mai letti dalle antenne"
  )
  storiche_confronto |>
    dplyr::distinct(.data$data_lettura, .data$RFID) |>
    dplyr::mutate(
      gruppo = dplyr::case_when(
        .data$RFID %in% antenne$RFID ~ gruppi[1],
        .data$RFID %in% antenne_tutte$RFID ~ gruppi[2],
        .default = gruppi[3]
      ),
      gruppo = factor(.data$gruppo, gruppi)
    )
}

#' Per ogni giro delle storiche, quanti RFID ha e quanti ne hanno letti le antenne
#' @inheritParams composizione_storiche
#' @return `data_lettura`, `id_trasmissione`, `rfid`, `rfid_antenne`, `quota`.
#' @noRd
composizione_giri <- function(storiche_confronto, antenne) {
  storiche_confronto |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione, .data$RFID) |>
    dplyr::summarise(
      rfid = dplyr::n(),
      rfid_antenne = sum(.data$RFID %in% antenne$RFID),
      .by = c("data_lettura", "id_trasmissione")
    ) |>
    dplyr::mutate(quota = .data$rfid_antenne / .data$rfid)
}

#' Letture al giorno in un elenco di giorni, 0 dove non ce ne sono
#' @noRd
n_per_giorno <- function(confronto, giorni) {
  conteggi <- dplyr::count(confronto, .data$data_lettura, name = "n")
  dplyr::coalesce(conteggi$n[match(giorni, conteggi$data_lettura)], 0L)
}

#' Intervallo al 95% del rapporto tra due somme, ricampionando i giorni
#' @param numeratore,denominatore Una cifra per giorno, allineate.
#' @return Limite basso e alto, o `NA` senza denominatore.
#' @noRd
intervallo_rapporto <- function(
  numeratore,
  denominatore,
  ripetizioni = 2000L,
  seme = 1L
) {
  if (sum(denominatore) == 0) {
    return(c(NA_real_, NA_real_))
  }
  giorni <- length(numeratore)
  rapporti <- withr::with_seed(
    seme,
    replicate(ripetizioni, {
      scelti <- sample.int(giorni, giorni, replace = TRUE)
      sum(numeratore[scelti]) / sum(denominatore[scelti])
    })
  )
  unname(stats::quantile(rapporti, c(0.025, 0.975), na.rm = TRUE))
}

#' Il periodo delle antenne contro lo stesso periodo di un anno prima
#'
#' Il periodo di confronto è spostato di `giorni_indietro` giorni (364: 52
#' settimane, stessi giorni della settimana), così stagionalità e giorni di
#' calendario coincidono. Si confrontano gli RFID letti in un giorno, da
#' qualunque sistema, nei due periodi.
#' @param confronto Tabella con le letture di entrambi gli anni.
#' @param dal,al Periodo delle antenne.
#' @return Lista: `riepilogo` (una riga) e `giorni` (una riga per giorno).
#' @noRd
anno_su_anno <- function(
  confronto,
  dal,
  al,
  giorni_indietro = 364L,
  ripetizioni = 2000L,
  seme = 1L
) {
  serie <- dplyr::tibble(data_lettura = seq(dal, al, by = "day")) |>
    dplyr::mutate(
      corrente = n_per_giorno(confronto, .data$data_lettura),
      base = n_per_giorno(confronto, .data$data_lettura - giorni_indietro)
    )
  limiti <- intervallo_rapporto(
    serie$corrente,
    serie$base,
    ripetizioni,
    seme
  )
  riepilogo <- dplyr::tibble(
    dal = dal,
    al = al,
    dal_base = dal - giorni_indietro,
    al_base = al - giorni_indietro,
    giorni = nrow(serie),
    rfid_giorno_base = sum(serie$base),
    rfid_giorno_corrente = sum(serie$corrente),
    per_giorno_base = mean(serie$base),
    per_giorno_corrente = mean(serie$corrente),
    rapporto = sum(serie$corrente) / sum(serie$base),
    rapporto_basso = limiti[1],
    rapporto_alto = limiti[2]
  )
  list(riepilogo = riepilogo, giorni = serie)
}

#' Quota dei contenitori attivi letti almeno una volta in ogni finestra mobile
#'
#' Il denominatore sono i contenitori censiti attivi nella finestra, ricavati
#' dalle letture stesse (date di attivazione e cessazione del contenitore): un
#' contenitore mai letto da nessuno dei due sistemi non c'è, e la quota è per
#' difetto per entrambi.
#' @param storiche,antenne Letture già filtrate, con `RFID`,
#'   `data_ora_lettura`, `presente_a_database` e le date di attivazione e
#'   cessazione del contenitore.
#' @param finestra Giorni di ogni finestra; `passo` giorni tra una e l'altra.
#' @return `data_fine`, `attivi`, `letti_saponette`, `letti_antenne`,
#'   `letti_unione` e `antenne_complete` (la finestra è tutta nel periodo
#'   delle antenne).
#' @noRd
copertura_contenitori <- function(
  storiche,
  antenne,
  finestra = 28L,
  passo = 7L
) {
  colonne <- c("data_attivazione_contenitore", "data_cessazione_contenitore")
  richiedi_colonne(storiche, colonne, "copertura")
  richiedi_colonne(antenne, colonne, "copertura")
  storiche <- storiche |> con_data_lettura() |> solo_censiti()
  antenne <- antenne |> con_data_lettura() |> solo_censiti()

  # Quando ogni contenitore è attivo; una data di attivazione mancante vale da sempre.
  contenitori <- dplyr::bind_rows(
    dplyr::select(storiche, "RFID", dplyr::all_of(colonne)),
    dplyr::select(antenne, "RFID", dplyr::all_of(colonne))
  ) |>
    dplyr::summarise(
      inizio = lubridate::as_date(
        suppressWarnings(min(
          lubridate::as_date(.data$data_attivazione_contenitore),
          na.rm = TRUE
        ))
      ),
      fine = max(data_o_aperta(.data$data_cessazione_contenitore)),
      .by = "RFID"
    ) |>
    dplyr::mutate(
      inizio = dplyr::if_else(
        is.finite(.data$inizio),
        .data$inizio,
        as.Date("1900-01-01")
      )
    )

  giorni_letti <- list(
    saponette = dplyr::distinct(storiche, .data$data_lettura, .data$RFID),
    antenne = dplyr::distinct(antenne, .data$data_lettura, .data$RFID)
  )
  tutti <- c(storiche$data_lettura, antenne$data_lettura)
  fine_finestre <- seq(min(tutti) + finestra - 1, max(tutti), by = passo)
  prima_antenne <- min(antenne$data_lettura)

  letti <- function(letture, inizio, fine, attivi) {
    nel_periodo <- letture$data_lettura >= inizio & letture$data_lettura <= fine
    unique(letture$RFID[nel_periodo & letture$RFID %in% attivi])
  }
  purrr::map(fine_finestre, function(fine) {
    inizio <- fine - finestra + 1
    attivi <- contenitori$RFID[
      contenitori$inizio <= fine & contenitori$fine >= inizio
    ]
    da_saponette <- letti(giorni_letti$saponette, inizio, fine, attivi)
    da_antenne <- letti(giorni_letti$antenne, inizio, fine, attivi)
    dplyr::tibble(
      data_fine = fine,
      attivi = length(attivi),
      letti_saponette = length(da_saponette),
      letti_antenne = length(da_antenne),
      letti_unione = length(union(da_saponette, da_antenne)),
      antenne_complete = inizio >= prima_antenne
    )
  }) |>
    dplyr::bind_rows()
}

#' Chip letti da entrambi i sistemi sullo stesso mezzo nello stesso giorno
#'
#' Una trasmissione delle saponette in un giorno è abbinata al mezzo con
#' antenna che ha letto con lei almeno `chip_minimi` RFID lo stesso giorno (il
#' mezzo con più RFID in comune). Per ogni mezzo e giorno abbinati si contano
#' gli RFID letti dalle saponette, dalle antenne e da entrambe. L'abbinamento
#' richiede almeno un'attività dei due sistemi: dove le saponette sono state
#' usate solo a metà giro, gli RFID letti solo dalle antenne sono per difetto.
#' @param storiche Letture storiche già filtrate, con `data_lettura`,
#'   `id_trasmissione` e `RFID`; solo censiti.
#' @param antenne Letture con le antenne già filtrate, con `data_ora_lettura`,
#'   `matricola_veicolo` e `RFID`; solo censiti.
#' @return `matricola_veicolo`, `data_lettura`, `giri`, `chip_saponette`,
#'   `chip_antenne` e `chip_entrambi`.
#' @noRd
giri_in_comune <- function(storiche, antenne, chip_minimi = 3L) {
  solo_saponette <- storiche |>
    solo_censiti() |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione, .data$RFID)
  solo_antenne <- antenne |>
    solo_censiti() |>
    con_data_lettura() |>
    dplyr::distinct(.data$data_lettura, .data$matricola_veicolo, .data$RFID)

  abbinati <- solo_saponette |>
    dplyr::inner_join(
      solo_antenne,
      by = c("data_lettura", "RFID"),
      relationship = "many-to-many"
    ) |>
    dplyr::count(
      .data$data_lettura,
      .data$id_trasmissione,
      .data$matricola_veicolo,
      name = "condivisi"
    ) |>
    dplyr::filter(.data$condivisi >= chip_minimi) |>
    dplyr::arrange(dplyr::desc(.data$condivisi)) |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione, .keep_all = TRUE)

  chip_saponette <- solo_saponette |>
    dplyr::inner_join(
      abbinati[c("data_lettura", "id_trasmissione", "matricola_veicolo")],
      by = c("data_lettura", "id_trasmissione")
    ) |>
    dplyr::distinct(.data$data_lettura, .data$matricola_veicolo, .data$RFID) |>
    dplyr::mutate(saponette = TRUE)
  chip_antenne <- solo_antenne |>
    dplyr::semi_join(abbinati, by = c("data_lettura", "matricola_veicolo")) |>
    dplyr::mutate(antenne = TRUE)
  giri <- dplyr::count(
    abbinati,
    .data$data_lettura,
    .data$matricola_veicolo,
    name = "giri"
  )

  dplyr::full_join(
    chip_saponette,
    chip_antenne,
    by = c("data_lettura", "matricola_veicolo", "RFID")
  ) |>
    dplyr::summarise(
      chip_saponette = sum(.data$saponette %in% TRUE),
      chip_antenne = sum(.data$antenne %in% TRUE),
      chip_entrambi = sum(.data$saponette %in% TRUE & .data$antenne %in% TRUE),
      .by = c("matricola_veicolo", "data_lettura")
    ) |>
    dplyr::left_join(giri, by = c("data_lettura", "matricola_veicolo")) |>
    dplyr::select(
      "matricola_veicolo",
      "data_lettura",
      "giri",
      "chip_saponette",
      "chip_antenne",
      "chip_entrambi"
    ) |>
    dplyr::arrange(.data$data_lettura, .data$matricola_veicolo)
}

#' Giri o mezzi al giorno e RFID per giro, per mese e sistema
#'
#' Scompone le letture in quanti giri sono stati registrati e quanti RFID ha
#' letto ogni giro: per le saponette un giro è una trasmissione in un giorno,
#' per le antenne un mezzo in un giorno. Solo censiti.
#' @param storiche,antenne Letture già filtrate; le storiche con
#'   `id_trasmissione`, le antenne con `matricola_veicolo`.
#' @return `mese`, `sistema`, `giri`, `giorni`, `giri_per_giorno`,
#'   `rfid_per_giro`.
#' @noRd
produttivita_giri <- function(storiche, antenne, dal, al) {
  giri_saponette <- storiche |>
    solo_censiti() |>
    dplyr::distinct(.data$data_lettura, .data$id_trasmissione, .data$RFID) |>
    dplyr::summarise(
      rfid = dplyr::n(),
      .by = c("data_lettura", "id_trasmissione")
    ) |>
    dplyr::mutate(sistema = "Saponette")
  giri_antenne <- antenne |>
    solo_censiti() |>
    con_data_lettura() |>
    dplyr::distinct(.data$data_lettura, .data$matricola_veicolo, .data$RFID) |>
    dplyr::summarise(
      rfid = dplyr::n(),
      .by = c("data_lettura", "matricola_veicolo")
    ) |>
    dplyr::mutate(sistema = "Antenne")

  dplyr::bind_rows(
    dplyr::select(giri_saponette, "data_lettura", "rfid", "sistema"),
    dplyr::select(giri_antenne, "data_lettura", "rfid", "sistema")
  ) |>
    dplyr::mutate(mese = lubridate::floor_date(.data$data_lettura, "month")) |>
    dplyr::summarise(
      giri = dplyr::n(),
      rfid_per_giro = mean(.data$rfid),
      .by = c("mese", "sistema")
    ) |>
    dplyr::mutate(
      giorni = giorni_del_mese(.data$mese, dal, al),
      giri_per_giorno = dplyr::if_else(
        .data$giorni > 0,
        .data$giri / .data$giorni,
        NA_real_
      )
    ) |>
    dplyr::arrange(.data$sistema, .data$mese)
}

#' Primo giorno di lettura di ogni mezzo con le antenne
#'
#' Un mezzo conta come attrezzato dal giorno della sua prima lettura con le
#' antenne, di qualunque servizio.
#' @noRd
attivazioni_mezzi <- function(antenne_tutte) {
  antenne_tutte |>
    con_data_lettura() |>
    dplyr::summarise(
      prima_lettura = min(.data$data_lettura),
      .by = "matricola_veicolo"
    )
}

#' Mezzi attrezzati ogni giorno di un elenco
#' @noRd
mezzi_attivi_per_giorno <- function(attivazioni, giorni) {
  vapply(
    giorni,
    function(giorno) sum(attivazioni$prima_lettura <= giorno),
    integer(1)
  )
}

#' Una riga per settimana con letture per giorno e mezzi attrezzati
#'
#' La settimana comincia il lunedì e conta i soli giorni tra `dal` e `al`.
#' `base` sono le letture delle stesse settimane di `giorni_indietro` giorni
#' prima, `rapporto` il rapporto tra le letture della settimana e la base.
#' @param confronto Tabella con le letture di entrambi gli anni.
#' @param antenne_tutte Letture con le antenne, senza filtri.
#' @return `settimana`, `giorni`, `mezzi_attrezzati`, `per_giorno`,
#'   `per_giorno_saponette`, `per_giorno_antenne`, `per_giorno_base`,
#'   `rapporto`.
#' @noRd
serie_settimanale <- function(
  confronto,
  antenne_tutte,
  dal,
  al,
  giorni_indietro = 364L
) {
  attivazioni <- attivazioni_mezzi(antenne_tutte)
  da_saponette <- dplyr::filter(
    confronto,
    .data$origine %in% c("Saponette", "Entrambe")
  )
  da_antenne <- dplyr::filter(
    confronto,
    .data$origine %in% c("Antenne", "Entrambe")
  )
  dplyr::tibble(data_lettura = seq(dal, al, by = "day")) |>
    dplyr::mutate(
      settimana = lubridate::floor_date(
        .data$data_lettura,
        "week",
        week_start = 1
      ),
      totale = n_per_giorno(confronto, .data$data_lettura),
      saponette = n_per_giorno(da_saponette, .data$data_lettura),
      antenne = n_per_giorno(da_antenne, .data$data_lettura),
      base = n_per_giorno(confronto, .data$data_lettura - giorni_indietro),
      mezzi = mezzi_attivi_per_giorno(attivazioni, .data$data_lettura)
    ) |>
    dplyr::summarise(
      giorni = dplyr::n(),
      mezzi_attrezzati = max(.data$mezzi),
      per_giorno = sum(.data$totale) / dplyr::n(),
      per_giorno_saponette = sum(.data$saponette) / dplyr::n(),
      per_giorno_antenne = sum(.data$antenne) / dplyr::n(),
      per_giorno_base = sum(.data$base) / dplyr::n(),
      .by = "settimana"
    ) |>
    dplyr::mutate(
      rapporto = dplyr::if_else(
        .data$per_giorno_base > 0,
        .data$per_giorno / .data$per_giorno_base,
        NA_real_
      )
    )
}

#' Chip delle antenne che nessuna saponetta ha mai letto
#'
#' @param antenne Letture con le antenne dopo i filtri, con `RFID`,
#'   `presente_a_database` e `data_attivazione_contenitore`.
#' @param storiche_tutte Tutte le letture storiche, senza filtri: un chip
#'   conta come letto dalle saponette in qualunque giro.
#' @return Una riga per gruppo: `gruppo`, `chip`, `mai_letti`, `quota`.
#' @noRd
chip_senza_saponette <- function(antenne, storiche_tutte) {
  inizio_storiche <- min(lubridate::as_date(storiche_tutte$data_ora_lettura))
  gruppi <- c(
    "Censiti attivi dall'inizio delle saponette",
    "Censiti attivati dopo l'inizio delle saponette",
    "Non censiti"
  )
  antenne |>
    dplyr::summarise(
      censito = any(.data$presente_a_database %in% "Presente"),
      attivazione = suppressWarnings(min(
        lubridate::as_date(.data$data_attivazione_contenitore),
        na.rm = TRUE
      )),
      .by = "RFID"
    ) |>
    dplyr::mutate(
      gruppo = dplyr::case_when(
        !.data$censito ~ gruppi[3],
        !is.finite(.data$attivazione) |
          .data$attivazione <= inizio_storiche ~ gruppi[1],
        .default = gruppi[2]
      ),
      gruppo = factor(.data$gruppo, gruppi),
      mai_letto = !.data$RFID %in% storiche_tutte$RFID
    ) |>
    dplyr::summarise(
      chip = dplyr::n(),
      mai_letti = sum(.data$mai_letto),
      .by = "gruppo"
    ) |>
    dplyr::mutate(quota = .data$mai_letti / .data$chip) |>
    dplyr::arrange(.data$gruppo)
}

#' Coerenza del servizio e del comune delle letture con le antenne
#'
#' Una lettura con un RFID fuori posto (un tag sul mezzo, un bidone di un altro
#' comune, un servizio diverso dal giro) fa salire il numero di letture senza
#' fare un conferimento in più: la quota di letture coerenti dice quanto le
#' letture in più sono credibili.
#' @param antenne Letture con le antenne, con `coerenza_servizio` e
#'   `coerenza_comune`.
#' @return `indicatore`, `valore`, `n` e `quota` (sul totale dell'indicatore).
#' @noRd
qualita_letture_antenne <- function(antenne) {
  tabella <- function(colonna, indicatore) {
    if (!colonna %in% names(antenne)) {
      return(NULL)
    }
    antenne |>
      dplyr::count(valore = dplyr::coalesce(.data[[colonna]], "(vuoto)")) |>
      dplyr::mutate(
        indicatore = indicatore,
        quota = .data$n / sum(.data$n)
      ) |>
      dplyr::select("indicatore", "valore", "n", "quota")
  }
  dplyr::bind_rows(
    tabella("coerenza_servizio", "Coerenza del servizio"),
    tabella("coerenza_comune", "Coerenza del comune")
  )
}

#' Letture del periodo delle antenne proiettate a tutta la flotta
#'
#' Per ogni giorno, le letture con le antenne sono divise per i mezzi
#' attrezzati quel giorno e moltiplicate per i mezzi della flotta: la
#' proiezione assume che un mezzo non ancora attrezzato legga come uno
#' attrezzato. È una stima, non una misura.
#' @inheritParams anno_su_anno
#' @inheritParams serie_settimanale
#' @param mezzi_totali Mezzi della flotta.
#' @return Una riga: `per_giorno_antenne`, `per_giorno_proiettato`,
#'   `per_giorno_base`, `rapporto`, `rapporto_basso`, `rapporto_alto`,
#'   `quota_mezzi` (media dei mezzi attrezzati sulla flotta).
#' @noRd
proiezione_flotta <- function(
  confronto,
  antenne_tutte,
  mezzi_totali,
  dal,
  al,
  giorni_indietro = 364L,
  ripetizioni = 2000L,
  seme = 1L
) {
  attivazioni <- attivazioni_mezzi(antenne_tutte)
  da_antenne <- dplyr::filter(
    confronto,
    .data$origine %in% c("Antenne", "Entrambe")
  )
  giorni <- dplyr::tibble(data_lettura = seq(dal, al, by = "day")) |>
    dplyr::mutate(
      antenne = n_per_giorno(da_antenne, .data$data_lettura),
      mezzi = pmax(mezzi_attivi_per_giorno(attivazioni, .data$data_lettura), 1L),
      proiettato = .data$antenne * mezzi_totali / .data$mezzi,
      base = n_per_giorno(confronto, .data$data_lettura - giorni_indietro)
    )
  limiti <- intervallo_rapporto(
    giorni$proiettato,
    giorni$base,
    ripetizioni,
    seme
  )
  dplyr::tibble(
    per_giorno_antenne = mean(giorni$antenne),
    per_giorno_proiettato = mean(giorni$proiettato),
    per_giorno_base = mean(giorni$base),
    rapporto = sum(giorni$proiettato) / sum(giorni$base),
    rapporto_basso = limiti[1],
    rapporto_alto = limiti[2],
    quota_mezzi = mean(giorni$mezzi) / mezzi_totali
  )
}
