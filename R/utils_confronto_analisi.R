# Analisi del report `docs/confronto_antenne.qmd`, sul confronto tra letture
# letture delle saponette e letture con le antenne. L'app non usa queste funzioni.
#
# Dove non diversamente detto, `confronto` è la tabella di `confronta_letture()`
# o di `unisci_origini()`: `data_lettura`, `RFID` e `origine`.

#' Letture con `data_lettura` (data) ricavata da `data_ora_lettura`
#'
#' Contesto: i CSV letti come testo non hanno una vera data; i confronti
#' lavorano per giorno. Risultato: le stesse letture con `data_lettura` come data.
#' @noRd
con_data_lettura <- function(letture) {
  dplyr::mutate(
    letture,
    data_lettura = lubridate::as_date(.data$data_ora_lettura)
  )
}

#' Solo le letture dei contenitori censiti, se la colonna c'è
#'
#' Contesto: copertura, giri in comune e produttività si calcolano sui censiti,
#' perché solo per loro si conosce il servizio e la data di attivazione.
#' Risultato: le letture con `presente_a_database` uguale a `PIA`.
#' @noRd
solo_censiti <- function(letture) {
  if (!"presente_a_database" %in% names(letture)) {
    return(letture)
  }
  dplyr::filter(letture, .data$presente_a_database %in% "PIA")
}

#' Letture delle saponette degli RFID letti dalle antenne, in qualunque giro
#'
#' Contesto: è la correzione 2 del report di confronto. A differenza di
#' `saponette_di_confronto()`, che tiene tutte le letture dei giri in cui
#' compare un RFID delle antenne, qui restano i soli RFID delle antenne: i due lati
#' confrontano gli stessi contenitori.
#' Risultato: le letture delle saponette con `data_lettura`, solo di quegli RFID.
#' @noRd
saponette_degli_stessi_rfid <- function(letture_saponette, letture_antenne) {
  letture_saponette |>
    con_data_lettura() |>
    dplyr::filter(.data$RFID %in% letture_antenne$RFID)
}

#' Giorni di ogni mese compresi tra `dal` e `al`
#'
#' Contesto: il primo e l'ultimo mese del periodo osservato sono parziali; per
#' confrontare i mesi si divide per i giorni osservati.
#' @param mesi Primo giorno di ogni mese.
#' @param dal,al Primo e ultimo giorno osservati.
#' @return I giorni del mese dentro il periodo (zero se il mese è fuori).
#' @noRd
giorni_del_mese <- function(mesi, dal, al) {
  inizio <- pmax(mesi, dal)
  fine <- pmin(lubridate::ceiling_date(mesi, "month") - 1, al)
  pmax(as.integer(fine - inizio) + 1L, 0L)
}

#' RFID letti in un giorno per mese e origine, con la media per giorno
#'
#' Contesto: correzione 1 del report di confronto. Un mese parziale (agosto
#' 2026 comincia il 5, ottobre finisce il 21) non è confrontabile con uno intero:
#' si divide per i giorni osservati.
#' Funzionamento: conta le righe di `confronto` per mese e origine e le divide
#' per `giorni_del_mese()`.
#' Risultato: `mese`, `origine`, `n`, `giorni` e `per_giorno`.
#' @param confronto Tabella di `confronta_letture()` o `unisci_origini()`.
#' @param dal,al Periodo osservato (di norma il primo e l'ultimo giorno).
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

#' Classifica gli RFID dei giri delle saponette secondo il loro rapporto con le antenne
#'
#' Contesto: spiega perché il confronto iniziale non è equo. Le letture delle
#' saponette dei giri di confronto sono più di quelle delle antenne perché un
#' giro comprende RFID che le antenne non hanno letto: un mezzo senza antenna,
#' un bidone non raccolto da un mezzo attrezzato, un bidone non letto
#' dall'antenna.
#' Funzionamento: ogni RFID-giorno delle saponette è «letto dalle antenne»
#' (dopo i filtri), «letto dalle antenne ma fuori dai filtri» o «mai letto dalle
#' antenne».
#' Risultato: `data_lettura`, `RFID` e `gruppo` (fattore a tre livelli).
#' @param saponette_confronto Risultato di `saponette_di_confronto()`.
#' @param antenne Letture con le antenne dopo i filtri.
#' @param antenne_tutte Letture con le antenne, senza filtri.
#' @return `data_lettura`, `RFID` e `gruppo` (fattore).
#' @noRd
classifica_rfid_dei_giri_saponette <- function(saponette_confronto, antenne, antenne_tutte) {
  gruppi <- c(
    "RFID letti dalle antenne",
    "RFID letti dalle antenne, ma fuori dai filtri",
    "RFID mai letti dalle antenne"
  )
  saponette_confronto |>
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

#' Per ogni giro delle saponette, quanti RFID ha e quanti ne hanno letti le antenne
#'
#' Contesto: mostra quanto ogni giro (trasmissione) del confronto iniziale è
#' «coperto» dagli RFID delle antenne; i giri con poca copertura gonfiano il lato
#' delle saponette.
#' Risultato: `data_lettura`, `matricola_saponetta`, `rfid`, `rfid_antenne` e
#' `quota` (la quota letta anche dalle antenne).
#' @inheritParams classifica_rfid_dei_giri_saponette
#' @return `data_lettura`, `matricola_saponetta`, `rfid`, `rfid_antenne`, `quota`.
#' @noRd
copertura_antenne_per_giro <- function(saponette_confronto, antenne) {
  saponette_confronto |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .data$RFID) |>
    # Una volta sola: dentro un raggruppamento sarebbe una ricerca per giro.
    dplyr::mutate(delle_antenne = .data$RFID %in% antenne$RFID) |>
    dplyr::summarise(
      rfid = dplyr::n(),
      rfid_antenne = sum(.data$delle_antenne),
      .by = c("data_lettura", "matricola_saponetta")
    ) |>
    dplyr::mutate(quota = .data$rfid_antenne / .data$rfid)
}

#' Letture al giorno in un elenco di giorni, 0 dove non ce ne sono
#' Risultato: un intero per giorno, nell'ordine dato; serve ai confronti per
#' giorno (`anno_su_anno()`, `serie_settimanale()`).
#' @noRd
n_per_giorno <- function(confronto, giorni) {
  conteggi <- dplyr::count(confronto, .data$data_lettura, name = "n")
  dplyr::coalesce(conteggi$n[match(giorni, conteggi$data_lettura)], 0L)
}

#' Intervallo al 95% del rapporto tra due somme, ricampionando i giorni
#'
#' Contesto: dice quanto è incerto un rapporto calcolato su pochi giorni
#' (per esempio gli RFID letti con le antenne contro un anno prima).
#' Funzionamento: ricampiona con reinserimento le coppie di giorni (così i giorni
#' della settimana restano allineati), calcola ogni volta il rapporto tra le
#' somme e prende i percentili 2,5 e 97,5. Il seme è fisso: il risultato è
#' ripetibile.
#' Risultato: i due limiti, o `NA` se il denominatore è zero.
#' @param numeratore,denominatore Una cifra per giorno, allineate.
#' @param ripetizioni Numero di ricampionamenti.
#' @param seme Seme del generatore casuale.
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
#' Contesto: analisi A del report di confronto. Un mese si confronta con lo
#' stesso mese dell'anno prima per togliere la stagionalità.
#' Funzionamento: il periodo di confronto è spostato di `giorni_indietro` giorni
#' (364: 52 settimane, stessi giorni della settimana). Si contano gli RFID letti
#' in un giorno, da qualunque sistema, nei due periodi, e si calcola il
#' rapporto con il suo intervallo (`intervallo_rapporto()`).
#' Risultato: una lista con `riepilogo` (una riga: giorni, totali e medie dei
#' due periodi, rapporto e intervallo) e `giorni` (una riga per giorno).
#' @param confronto Tabella con le letture di entrambi gli anni.
#' @param dal,al Periodo delle antenne.
#' @param giorni_indietro Giorni di scarto del periodo di confronto.
#' @param ripetizioni,seme Vedi `intervallo_rapporto()`.
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
#' Contesto: analisi B. Misura quanto del parco contenitori ciascun sistema
#' riesce a vedere, con un denominatore che non dipende da quanti mezzi hanno le
#' antenne.
#' Funzionamento: ricava dalle letture stesse quando ogni contenitore censito è
#' attivo (attivazione e cessazione); per ogni finestra di `finestra` giorni,
#' spostata di `passo` giorni, conta i contenitori attivi e quelli letti dalle
#' saponette, dalle antenne e da almeno uno dei due. Un contenitore mai letto da
#' nessuno dei due sistemi non c'è: la quota è per difetto per entrambi.
#' Risultato: una riga per finestra con `data_fine`, `attivi`, `letti_saponette`,
#' `letti_antenne`, `letti_unione` e `antenne_complete`.
#' @param saponette,antenne Letture già filtrate, con `RFID`, `data_ora_lettura`,
#'   `presente_a_database` e le date di attivazione e cessazione del contenitore.
#' @param finestra Giorni di ogni finestra; `passo` giorni tra una e l'altra.
#' @return `data_fine`, `attivi`, `letti_saponette`, `letti_antenne`,
#'   `letti_unione` e `antenne_complete` (la finestra è tutta nel periodo delle
#'   antenne).
#' @noRd
copertura_contenitori <- function(
  saponette,
  antenne,
  finestra = 28L,
  passo = 7L
) {
  colonne <- c("data_attivazione_contenitore", "data_cessazione_contenitore")
  richiedi_colonne(saponette, colonne, "copertura")
  richiedi_colonne(antenne, colonne, "copertura")
  saponette <- saponette |> con_data_lettura() |> solo_censiti()
  antenne <- antenne |> con_data_lettura() |> solo_censiti()

  # Quando ogni contenitore è attivo; una data di attivazione mancante vale da sempre.
  contenitori <- dplyr::bind_rows(
    dplyr::select(saponette, "RFID", dplyr::all_of(colonne)),
    dplyr::select(antenne, "RFID", dplyr::all_of(colonne))
  ) |>
    # Pochi periodi distinti per contenitore: si raggruppa dopo averli ridotti.
    dplyr::distinct() |>
    dplyr::mutate(
      inizio = lubridate::as_date(.data$data_attivazione_contenitore),
      fine = data_o_aperta(.data$data_cessazione_contenitore)
    ) |>
    dplyr::summarise(
      inizio = suppressWarnings(min(.data$inizio, na.rm = TRUE)),
      fine = max(.data$fine),
      .by = "RFID"
    ) |>
    dplyr::mutate(
      inizio = dplyr::if_else(
        is.finite(.data$inizio),
        .data$inizio,
        as.Date("1900-01-01")
      )
    )

  # Per ogni sistema, i giorni di lettura con il numero del contenitore.
  giorni_letti <- function(letture) {
    letture |>
      dplyr::distinct(.data$data_lettura, .data$RFID) |>
      dplyr::mutate(numero = match(.data$RFID, contenitori$RFID)) |>
      dplyr::filter(!is.na(.data$numero))
  }
  da_saponette <- giorni_letti(saponette)
  da_antenne <- giorni_letti(antenne)
  tutti <- c(saponette$data_lettura, antenne$data_lettura)
  fine_finestre <- seq(min(tutti) + finestra - 1, max(tutti), by = passo)
  prima_antenne <- min(antenne$data_lettura)

  letti <- function(letture, inizio, fine) {
    nel_periodo <- letture$data_lettura >= inizio & letture$data_lettura <= fine
    unique(letture$numero[nel_periodo])
  }
  purrr::map(fine_finestre, function(fine) {
    inizio <- fine - finestra + 1
    attivo <- contenitori$inizio <= fine & contenitori$fine >= inizio
    numeri_saponette <- letti(da_saponette, inizio, fine)
    numeri_antenne <- letti(da_antenne, inizio, fine)
    dplyr::tibble(
      data_fine = fine,
      attivi = sum(attivo),
      letti_saponette = sum(attivo[numeri_saponette]),
      letti_antenne = sum(attivo[numeri_antenne]),
      letti_unione = sum(attivo[union(numeri_saponette, numeri_antenne)]),
      antenne_complete = inizio >= prima_antenne
    )
  }) |>
    dplyr::bind_rows()
}

#' RFID letti da entrambi i sistemi sullo stesso mezzo nello stesso giorno
#'
#' Contesto: analisi C. Per sapere quanto un sistema vede rispetto all'altro
#' bisogna guardare le giornate in cui hanno lavorato insieme. Le saponette non
#' hanno il mezzo: si abbina ogni trasmissione al mezzo con antenna con cui
#' condivide gli RFID.
#' Funzionamento: una trasmissione in un giorno è abbinata al mezzo con antenna
#' che ha letto lo stesso giorno almeno `chip_minimi` degli stessi RFID (il mezzo
#' con più RFID in comune). Per ogni mezzo e giorno abbinati conta gli RFID
#' letti dalle saponette, dalle antenne e da entrambe. Dove le saponette sono
#' state usate solo a metà giro, gli RFID letti solo dalle antenne sono per difetto.
#' Risultato: `matricola_veicolo`, `data_lettura`, `giri`, `chip_saponette`,
#' `chip_antenne` e `chip_entrambi`.
#' @param saponette Letture delle saponette già filtrate, con `data_lettura`,
#'   `matricola_saponetta` e `RFID`; solo censiti.
#' @param antenne Letture con le antenne già filtrate, con `data_ora_lettura`,
#'   `matricola_veicolo` e `RFID`; solo censiti.
#' @param chip_minimi RFID in comune richiesti per abbinare una trasmissione.
#' @return `matricola_veicolo`, `data_lettura`, `giri`, `chip_saponette`,
#'   `chip_antenne` e `chip_entrambi`.
#' @noRd
giri_con_entrambi_i_sistemi <- function(saponette, antenne, chip_minimi = 3L) {
  solo_saponette <- saponette |>
    solo_censiti() |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .data$RFID)
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
      .data$matricola_saponetta,
      .data$matricola_veicolo,
      name = "condivisi"
    ) |>
    dplyr::filter(.data$condivisi >= chip_minimi) |>
    dplyr::arrange(dplyr::desc(.data$condivisi)) |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .keep_all = TRUE)

  chip_saponette <- solo_saponette |>
    dplyr::inner_join(
      abbinati[c("data_lettura", "matricola_saponetta", "matricola_veicolo")],
      by = c("data_lettura", "matricola_saponetta")
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
#' Contesto: analisi D. Le letture sono il prodotto di quanti giri si registrano
#' e di quanti RFID legge ogni giro: se calano i giri e non gli RFID per giro, il
#' calo è mancato utilizzo del lettore e non una lettura incompleta.
#' Funzionamento: per le saponette un giro è una trasmissione in un giorno, per
#' le antenne un mezzo in un giorno; conta i giri per mese e li divide per i
#' giorni osservati; calcola gli RFID medi per giro. Solo censiti.
#' Risultato: `mese`, `sistema`, `giri`, `giorni`, `giri_per_giorno`, `rfid_per_giro`.
#' @param saponette,antenne Letture già filtrate; le saponette con
#'   `matricola_saponetta`, le antenne con `matricola_veicolo`.
#' @param dal,al Periodo osservato.
#' @noRd
produttivita_giri <- function(saponette, antenne, dal, al) {
  giri_saponette <- saponette |>
    solo_censiti() |>
    dplyr::distinct(.data$data_lettura, .data$matricola_saponetta, .data$RFID) |>
    dplyr::summarise(
      rfid = dplyr::n(),
      .by = c("data_lettura", "matricola_saponetta")
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
#' Contesto: non si conosce la data di installazione dell'antenna di ogni
#' mezzo dai file di lettura; si usa la prima lettura, di qualunque servizio.
#' Risultato: `matricola_veicolo` e `prima_lettura`.
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
#'
#' Risultato: per ogni giorno, quanti mezzi hanno la prima lettura entro quel
#' giorno (`attivazioni_mezzi()`).
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
#' Contesto: analisi E. I mezzi sono attrezzati un po' alla volta: se le letture
#' crescono con i mezzi attrezzati, l'effetto è attribuibile alle antenne e non a
#' un andamento generale.
#' Funzionamento: la settimana comincia il lunedì e conta i soli giorni tra `dal`
#' e `al`; calcola le letture per giorno (totali, delle saponette, delle antenne)
#' e quelle delle stesse settimane di `giorni_indietro` giorni prima (`base`), e il
#' rapporto.
#' Risultato: `settimana`, `giorni`, `mezzi_attrezzati`, `per_giorno`,
#' `per_giorno_saponette`, `per_giorno_antenne`, `per_giorno_base`, `rapporto`.
#' @param confronto Tabella con le letture di entrambi gli anni.
#' @param antenne_tutte Letture con le antenne, senza filtri.
#' @param dal,al Periodo delle antenne.
#' @param giorni_indietro Giorni di scarto della base.
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

#' RFID delle antenne che nessuna saponetta ha mai letto
#'
#' Contesto: analisi F. Sono contenitori che i lettori manuali non hanno mai
#' visto: la prova più semplice che le antenne arrivano dove le saponette non
#' arrivano. Un contenitore attivato dopo l'inizio delle saponette può non essere
#' mai stato letto semplicemente perché non c'era, e i non censiti non sono in
#' anagrafica.
#' Funzionamento: per ogni RFID delle antenne dice se è censito e da quando è
#' attivo; lo classifica in tre gruppi (censiti attivi dall'inizio delle saponette,
#' censiti attivati dopo, non censiti) e conta quanti non compaiono mai nelle
#' letture delle saponette.
#' Risultato: una riga per gruppo con `gruppo`, `chip`, `mai_letti`, `quota`.
#' @param antenne Letture con le antenne dopo i filtri, con `RFID`,
#'   `presente_a_database` e `data_attivazione_contenitore`.
#' @param saponette_tutte Tutte le letture delle saponette, senza filtri: un RFID
#'   conta come letto dalle saponette in qualunque giro.
#' @return Una riga per gruppo: `gruppo`, `chip`, `mai_letti`, `quota`.
#' @noRd
chip_senza_saponette <- function(antenne, saponette_tutte) {
  inizio_saponette <- min(lubridate::as_date(saponette_tutte$data_ora_lettura))
  gruppi <- c(
    "Censiti attivi dall'inizio delle saponette",
    "Censiti attivati dopo l'inizio delle saponette",
    "Non censiti"
  )
  censiti <- unique(antenne$RFID[antenne$presente_a_database %in% "PIA"])
  antenne |>
    dplyr::distinct(.data$RFID, .data$data_attivazione_contenitore) |>
    dplyr::mutate(
      attivazione = lubridate::as_date(.data$data_attivazione_contenitore)
    ) |>
    # Un contenitore, la sua prima attivazione (le date mancanti in coda).
    dplyr::arrange(is.na(.data$attivazione), .data$attivazione) |>
    dplyr::distinct(.data$RFID, .keep_all = TRUE) |>
    dplyr::mutate(
      gruppo = dplyr::case_when(
        !.data$RFID %in% censiti ~ gruppi[3],
        is.na(.data$attivazione) |
          .data$attivazione <= inizio_saponette ~ gruppi[1],
        .default = gruppi[2]
      ),
      gruppo = factor(.data$gruppo, gruppi),
      mai_letto = !.data$RFID %in% saponette_tutte$RFID
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
#' Contesto: analisi G. Una lettura con un RFID fuori posto (un tag sul mezzo,
#' un bidone di un altro comune, un servizio diverso dal giro) fa salire il
#' numero di letture senza un conferimento in più: la quota di letture coerenti
#' dice quanto le letture in più sono credibili.
#' Risultato: per `coerenza_servizio` e `coerenza_comune`, quante letture e
#' quale quota hanno ogni valore (i vuoti compaiono come `(vuoto)`).
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
#' Contesto: analisi H. Le antenne sono su una parte dei mezzi; la proiezione
#' stima quante letture ci sarebbero con tutti i mezzi attrezzati. È una stima,
#' non una misura.
#' Funzionamento: per ogni giorno divide le letture con le antenne per i mezzi
#' attrezzati quel giorno e le moltiplica per i mezzi della flotta; la proiezione
#' assume che un mezzo non ancora attrezzato legga come uno attrezzato. Il
#' rapporto con la base di un anno prima ha il suo intervallo.
#' Risultato: una riga con `per_giorno_antenne`, `per_giorno_proiettato`,
#' `per_giorno_base`, `rapporto`, `rapporto_basso`, `rapporto_alto` e
#' `quota_mezzi` (media dei mezzi attrezzati sulla flotta).
#' @inheritParams anno_su_anno
#' @inheritParams serie_settimanale
#' @param mezzi_totali Mezzi della flotta.
#' @return Una riga: `per_giorno_antenne`, `per_giorno_proiettato`,
#'   `per_giorno_base`, `rapporto`, `rapporto_basso`, `rapporto_alto`,
#'   `quota_mezzi`.
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
