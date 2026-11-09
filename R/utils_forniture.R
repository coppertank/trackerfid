# Forniture e contenitori di un periodo, per il report
# `docs/indicatori_letture.qmd`. Vengono dalle tabelle estratte, non dalle
# letture: una fornitura senza letture esiste comunque, ed è proprio quella da
# segnalare. L'app non usa queste funzioni.

#' Giorni dell'anno con cui si riportano le soglie al periodo
#'
#' Contesto: tariffe e conferimenti attesi sono annui; per un periodo più corto
#' si moltiplicano per giorni / giorni dell'anno (un semestre vale circa la metà).
#' Funzionamento: 366 se il periodo sta tutto in un anno bisestile, altrimenti
#' 365.
#' Risultato: un numero.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @noRd
giorni_dell_anno <- function(dal, al) {
  dal <- as.Date(dal)
  al <- as.Date(al)
  stesso_anno <- lubridate::year(dal) == lubridate::year(al)
  if (stesso_anno && lubridate::leap_year(dal)) 366 else 365
}

#' Tipo di una fornitura dalla categoria: domestica, condominio o non domestica
#'
#' Contesto: le tre famiglie hanno regole di fatturazione diverse. La
#' precedenza è delle domestiche: categoria 31 o 32. Poi i condomini, categoria 34,
#' poi le non domestiche, `sottocategoria` `ND`. Il resto è «altra».
#' Risultato: un vettore di testi.
#' @param categoria,sottocategoria Vettori della stessa lunghezza.
#' @noRd
tipo_fornitura <- function(categoria, sottocategoria) {
  categoria <- trimws(as.character(categoria))
  dplyr::case_when(
    categoria %in% c("31", "32") ~ "domestica",
    categoria %in% "34" ~ "condominio",
    toupper(trimws(as.character(sottocategoria))) %in% "ND" ~ "non domestica",
    .default = "altra"
  )
}

#' Gruppo di una fornitura per i conteggi: casa vuota, potenziamento, 1 ... 6 o più
#'
#' Contesto: i conteggi di forniture e contenitori dividono le domestiche in
#' casa vuota, con potenziamento del secco e per fascia di componenti.
#' Funzionamento: con il potenziamento «Potenziamento»; altrimenti la fascia dei
#' componenti medi (`fascia_componenti()`); senza componenti noti «Componenti non
#' noti». Le altre famiglie hanno il loro nome.
#' Risultato: un fattore con i gruppi in ordine.
#' @param tipo Risultato di `tipo_fornitura()`.
#' @param componenti_medi,potenziamento Vettori della stessa lunghezza.
#' @noRd
gruppo_fornitura <- function(tipo, componenti_medi, potenziamento) {
  gruppi <- gruppi_forniture()
  fascia <- fascia_componenti(componenti_medi)
  gruppo <- dplyr::case_when(
    tipo == "condominio" ~ "Condomini",
    tipo == "non domestica" ~ "Non domestiche",
    tipo == "altra" ~ "Altre",
    potenziamento %in% TRUE ~ "Potenziamento",
    is.na(fascia) ~ "Componenti non noti",
    fascia == 0 ~ "Casa vuota",
    .default = gruppi[2L + fascia]
  )
  factor(gruppo, gruppi)
}

#' Gruppi di forniture in ordine
#' @noRd
gruppi_forniture <- function() {
  c(
    "Casa vuota",
    "Potenziamento",
    "1 componente",
    paste(2:5, "componenti"),
    "6 o pi\u00f9",
    "Componenti non noti",
    "Condomini",
    "Non domestiche",
    "Altre"
  )
}

#' Gruppo di forniture scelto con il parametro `componenti` del report
#'
#' Contesto: ogni report può guardare una fascia di componenti (da 1 a 6 o più),
#' la casa vuota o il potenziamento del secco; vuoto vuol dire tutte.
#' Funzionamento: accetta `0` o `casa vuota`, da `1` a `5`, `6`, `6+` o `6 o piu`,
#' `potenziamento` (maiuscole ininfluenti). Altri valori fermano con un messaggio.
#' Risultato: il nome del gruppo di `gruppi_forniture()`, o `NULL` se il filtro è vuoto.
#' @param componenti Valore del parametro.
#' @noRd
gruppo_da_filtro <- function(componenti) {
  voce <- valori_filtro(componenti)
  if (is.null(voce)) {
    return(NULL)
  }
  voce <- tolower(voce[[1]])
  gruppi <- gruppi_forniture()
  gruppo <- dplyr::case_when(
    voce %in% c("0", "casa vuota") ~ gruppi[1],
    voce == "potenziamento" ~ gruppi[2],
    voce %in% as.character(1:5) ~ gruppi[2L + suppressWarnings(as.integer(voce))],
    voce %in% c("6", "6+", "6 o piu", "6 o pi\u00f9") ~ gruppi[8],
    .default = NA_character_
  )
  if (is.na(gruppo)) {
    stop(
      "Valore di `componenti` non riconosciuto: ",
      voce,
      ". Usa un numero da 0 a 6 (6 vale 6 o pi\u00f9), `6+` o `potenziamento`.",
      call. = FALSE
    )
  }
  gruppo
}

#' Giorni coperti da un insieme di intervalli, senza contare due volte i sovrapposti
#'
#' Contesto: le dichiarazioni del potenziamento di una fornitura possono
#' sovrapporsi; i giorni si contano una volta.
#' Funzionamento: ordina gli intervalli di ogni fornitura per inizio e toglie la
#' parte già coperta dai precedenti.
#' Risultato: `id_fornitura` e `giorni`.
#' @param intervalli Data frame con `id_fornitura`, `da` e `a` (date, estremi
#'   inclusi, con `da` non dopo `a`).
#' @noRd
giorni_coperti <- function(intervalli) {
  ordinati <- dplyr::arrange(intervalli, .data$id_fornitura, .data$da, .data$a)
  gruppo <- indice_gruppi(ordinati$id_fornitura)
  n_gruppi <- max(c(gruppo, 0L))
  # Un salto per gruppo, più grande di ogni data: il massimo cumulato non passa
  # da un gruppo al successivo.
  salto <- gruppo * 1e6
  fine <- as.numeric(ordinati$a) + salto
  fine_precedente <- c(-Inf, cummax(fine)[-length(fine)])
  inizio_gruppo <- !duplicated(gruppo)
  fine_precedente[inizio_gruppo] <- -Inf
  inizio_nuovo <- pmax(as.numeric(ordinati$da) + salto, fine_precedente + 1)
  giorni <- pmax(fine - inizio_nuovo + 1, 0)
  dplyr::tibble(
    id_fornitura = ordinati$id_fornitura[inizio_gruppo],
    giorni = somma_per_gruppo(giorni, gruppo, n_gruppi)
  )
}

#' Componenti medi di ogni fornitura nel suo periodo di attività, pesati sul tempo
#'
#' Contesto: i componenti di una fornitura cambiano; per scegliere la fascia
#' tariffaria nel periodo si usa la media pesata sul tempo (scelta del
#' proprietario).
#' Funzionamento: ogni riga della composizione vale dal suo inizio fino al giorno
#' prima della riga seguente (o alla propria fine, se prima), tagliata sul periodo
#' di attività della fornitura. Si pesano i `nucleo_residenti` con i giorni. Una
#' riga senza inizio non vale (come nella pipeline).
#' Risultato: `id_fornitura`, `componenti_medi`, `giorni_con_componenti` e la
#' `sottocategoria` e la `categoria_nuclei` dell'ultima riga del periodo.
#' @param attivita Data frame con `id_fornitura`, `inizio` e `fine`.
#' @param componenti Tabella di `importa_componenti()`.
#' @noRd
componenti_medi_pesati <- function(attivita, componenti) {
  componenti$categoria <- componenti[["categoria"]] %||% NA_character_
  righe <- componenti |>
    dplyr::transmute(
      id_fornitura = as.character(.data$id_fornitura),
      categoria = as.character(.data$categoria),
      # Istanti nel fuso della sessione, come li legge `importa_componenti()`.
      da_riga = as.numeric(as.Date(.data$data_inizio_decorrenza, tz = "")),
      a_riga = as.numeric(as.Date(.data$data_fine_decorrenza, tz = "")),
      nucleo_residenti = .data$nucleo_residenti,
      sottocategoria = .data$sottocategoria
    ) |>
    dplyr::filter(!is.na(.data$da_riga)) |>
    dplyr::arrange(.data$id_fornitura, .data$da_riga)
  n <- nrow(righe)
  aperta <- as.numeric(as.Date("2099-12-31"))
  # Ogni riga vale fino al giorno prima della riga seguente della stessa fornitura.
  stessa_fornitura <- c(righe$id_fornitura[-1] == righe$id_fornitura[-n], FALSE)
  prossimo_inizio <- c(righe$da_riga[-1], NA)
  righe$a_riga <- pmin(
    dplyr::coalesce(righe$a_riga, aperta),
    ifelse(stessa_fornitura, prossimo_inizio - 1, aperta)
  )
  pesate <- righe |>
    dplyr::inner_join(
      dplyr::mutate(
        attivita,
        inizio = as.numeric(.data$inizio),
        fine = as.numeric(.data$fine)
      ),
      by = "id_fornitura",
      relationship = "many-to-many"
    ) |>
    dplyr::mutate(
      giorni = pmax(pmin(.data$a_riga, .data$fine) - pmax(.data$da_riga, .data$inizio) + 1, 0)
    ) |>
    dplyr::filter(.data$giorni > 0, !is.na(.data$nucleo_residenti))
  gruppo <- indice_gruppi(pesate$id_fornitura)
  n_gruppi <- max(c(gruppo, 0L))
  giorni <- somma_per_gruppo(pesate$giorni, gruppo, n_gruppi)
  dplyr::tibble(
    id_fornitura = pesate$id_fornitura[!duplicated(gruppo)],
    componenti_medi = somma_per_gruppo(pesate$nucleo_residenti * pesate$giorni, gruppo, n_gruppi) / giorni,
    giorni_con_componenti = giorni,
    sottocategoria = ultimo_per_gruppo(pesate$sottocategoria, gruppo, n_gruppi),
    categoria_nuclei = ultimo_per_gruppo(pesate$categoria, gruppo, n_gruppi)
  )
}

#' Forniture attive in un periodo, con categoria, componenti, potenziamento e contenitori
#'
#' Contesto: i denominatori degli indicatori (forniture, contenitori, propensione,
#' analisi economica) e la popolazione di cui cercare le forniture senza
#' conferimenti vengono dall'anagrafica, non dalle letture. Categoria dal file dei
#' contenitori (`codice_categoria`; se il file non l'ha vale quella della
#' composizione dei nuclei), componenti dalla composizione dei nuclei,
#' potenziamento dalle manovre sociali del secco (scelte del proprietario).
#'
#' Funzionamento: tiene i contenitori con un tag attivi almeno un giorno tra `dal` e
#' `al`. L'attività di una fornitura va dalla prima attivazione all'ultima
#' cessazione dei suoi contenitori, tagliata sul periodo (`giorni_attivi`).
#' **`fattore` non dipende dall'attività**: è la parte di anno che il periodo
#' occupa, giorni da `dal` a `al` / giorni dell'anno (`giorni_dell_anno()`), uguale
#' per tutte le forniture (scelta del proprietario, 10/11/2026: una fornitura
#' attivata a fine anno non deve gonfiare l'annualizzazione). I componenti sono la
#' media pesata sul tempo (`componenti_medi_pesati()`); il potenziamento vale se una
#' manovra (non annullata) è in vigore per almeno metà dei giorni di attività. La
#' statistica parte dai **contenitori**: un contenitore senza tag o con un codice
#' nullo (soli zeri) non compare, e una fornitura senza nessun contenitore
#' (per esempio chi conferisce nelle isole ecologiche) non entra nell'anagrafica.
#' Per ogni fornitura si contano i contenitori gratuiti (`contenitore_gratuito`
#' `S`: conferimenti illimitati, non fatturati) e quelli non gratuiti.
#'
#' Risultato: una riga per fornitura con `id_fornitura`, `servizio`, `id_utenza`,
#' `categoria`, `sottocategoria`, `tipo`, `contenitori`, `volume_contenitore` (il
#' più frequente), `volume_totale`, `conferimenti_massimi_annui` (somma dei massimi annui dei
#' contenitori), `litri_massimi_annui`, `inizio`, `fine`,
#' `contenitori_gratuiti`, `contenitori_non_gratuiti`, `volume_contenitore_non_gratuito`
#' (il volume più frequente tra i contenitori non gratuiti, vuoto se sono tutti
#' gratuiti), `giorni_attivi`, `fattore`, `componenti_medi`, `giorni_con_componenti`,
#' `potenziamento` e `gruppo`.
#' @param contenitori Tabella di `importa_contenitori(categoria = TRUE)`.
#' @param componenti Tabella di `importa_componenti()`.
#' @param manovre Tabella di `importa_manovre()`.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @param servizio Servizio dei contenitori da considerare (per esempio `SECCO`),
#'   o `NULL` per tutti. Si filtra sui singoli contenitori, prima di contare: una
#'   fornitura che avesse contenitori di servizi diversi conterebbe solo quelli del
#'   servizio scelto.
#' @noRd
anagrafica_forniture <- function(contenitori, componenti, manovre, dal, al, servizio = NULL) {
  dal <- as.Date(dal)
  al <- as.Date(al)
  attivi <- contenitori |>
    dplyr::mutate(id_fornitura = as.character(.data$id_fornitura)) |>
    dplyr::filter(
      !is.na(.data$id_fornitura),
      (.data$data_attivazione_contenitore <= al &
        .data$data_cessazione_contenitore >= dal) %in% TRUE
    )
  # Un codice mancante o nullo (soli zeri) non è un bidone.
  attivi <- dplyr::filter(
    attivi,
    !is.na(.data$codice_transponder),
    !grepl("^0*$", .data$codice_transponder)
  )
  if (!is.null(servizio)) {
    attivi <- dplyr::filter(
      attivi,
      toupper(trimws(.data$servizio_transponder)) %in% toupper(trimws(servizio))
    )
  }

  # Calcoli per gruppo su vettori (`R/utils_gruppi.R`): con centinaia di
  # migliaia di forniture un riepilogo gruppo per gruppo costa minuti.
  attivi <- dplyr::arrange(
    attivi,
    .data$id_fornitura,
    .data$data_attivazione_contenitore
  )
  gruppo <- indice_gruppi(attivi$id_fornitura)
  n_gruppi <- max(c(gruppo, 0L))
  attivazione <- estremi_per_gruppo(as.numeric(attivi$data_attivazione_contenitore), gruppo, n_gruppi)
  cessazione <- estremi_per_gruppo(as.numeric(attivi$data_cessazione_contenitore), gruppo, n_gruppi)
  # Il volume totale conta ogni contenitore una volta.
  primo_del_contenitore <- !duplicated(paste(gruppo, attivi$codice_transponder))
  # A parità di frequenza vince il volume più grande: si ordinano per volume.
  per_volume <- order(gruppo, attivi$volume)
  # Contenitori gratuiti: `S` nel campo `contenitore_gratuito` (assente = `N`).
  gratuito <- (attivi[["contenitore_gratuito"]] %||% rep("N", nrow(attivi))) %in% "S"
  gratuiti <- somma_per_gruppo(as.numeric(primo_del_contenitore & gratuito), gruppo, n_gruppi)
  # `numero_raccolte_annue` è il massimo di conferimenti che un bidone può fare in
  # un anno (26 per una raccolta ogni due settimane, 52 per una a settimana), non
  # quanti ne farà: per la fornitura si sommano i massimi dei suoi contenitori,
  # ognuno una volta, e i litri massimi (volume × massimo di ogni contenitore).
  raccolte_bidone <- ifelse(primo_del_contenitore, as.numeric(attivi$numero_raccolte_annue), NA_real_)
  con_massimo <- somma_per_gruppo(as.numeric(!is.na(raccolte_bidone)), gruppo, n_gruppi) > 0
  massimo_annuo <- somma_per_gruppo(dplyr::coalesce(raccolte_bidone, 0), gruppo, n_gruppi)
  litri_massimi <- somma_per_gruppo(
    dplyr::coalesce(raccolte_bidone * attivi$volume, 0),
    gruppo,
    n_gruppi
  )
  per_fornitura <- dplyr::tibble(
    id_fornitura = attivi$id_fornitura[!duplicated(gruppo)],
    servizio = toupper(primo_per_gruppo(attivi$servizio_transponder, gruppo, n_gruppi)),
    id_utenza = primo_per_gruppo(attivi$id_utenza, gruppo, n_gruppi),
    categoria = ultimo_per_gruppo(attivi$categoria_contenitore, gruppo, n_gruppi),
    contenitori = n_distinti_per_gruppo(attivi$codice_transponder, gruppo, n_gruppi),
    volume_contenitore = piu_frequente_per_gruppo(
      attivi$volume[per_volume],
      gruppo[per_volume],
      n_gruppi
    ),
    volume_totale = somma_per_gruppo(
      ifelse(primo_del_contenitore, dplyr::coalesce(attivi$volume, 0), 0),
      gruppo,
      n_gruppi
    ),
    contenitori_gratuiti = as.integer(gratuiti),
    volume_contenitore_non_gratuito = piu_frequente_per_gruppo(
      ifelse(gratuito, NA_real_, as.numeric(attivi$volume))[per_volume],
      gruppo[per_volume],
      n_gruppi
    ),
    conferimenti_massimi_annui = ifelse(con_massimo, massimo_annuo, NA_real_),
    litri_massimi_annui = ifelse(con_massimo, litri_massimi, NA_real_),
    inizio = pmax(as.Date(attivazione$minimo, origin = "1970-01-01"), dal),
    fine = pmin(as.Date(cessazione$massimo, origin = "1970-01-01"), al)
  ) |>
    dplyr::mutate(
      contenitori_non_gratuiti = .data$contenitori - .data$contenitori_gratuiti,
      giorni_attivi = as.numeric(.data$fine - .data$inizio) + 1,
      # Il periodo, non l'attività della fornitura: vedi la documentazione.
      fattore = (as.numeric(al - dal) + 1) / giorni_dell_anno(dal, al)
    )

  attivita <- dplyr::select(per_fornitura, "id_fornitura", "inizio", "fine")
  medie <- componenti_medi_pesati(attivita, componenti)

  # Il potenziamento: manovre non annullate, giorni in vigore nell'attività.
  vigore <- manovre |>
    dplyr::transmute(
      id_fornitura = as.character(.data$id_fornitura),
      da_manovra = as.Date(.data$data_inizio_dichiarazione),
      a_manovra = dplyr::coalesce(
        as.Date(.data$data_fine_dichiarazione),
        as.Date("2099-12-31")
      )
    ) |>
    dplyr::filter(!is.na(.data$da_manovra), .data$a_manovra >= .data$da_manovra) |>
    dplyr::inner_join(attivita, by = "id_fornitura") |>
    dplyr::transmute(
      .data$id_fornitura,
      da = pmax(.data$da_manovra, .data$inizio),
      a = pmin(.data$a_manovra, .data$fine)
    ) |>
    dplyr::filter(.data$da <= .data$a) |>
    giorni_coperti() |>
    dplyr::rename(giorni_potenziamento = "giorni")

  per_fornitura |>
    dplyr::left_join(medie, by = "id_fornitura") |>
    dplyr::left_join(vigore, by = "id_fornitura") |>
    dplyr::mutate(
      giorni_potenziamento = dplyr::coalesce(.data$giorni_potenziamento, 0),
      potenziamento = .data$giorni_potenziamento >= .data$giorni_attivi / 2,
      # Senza la categoria del contenitore vale quella della composizione dei nuclei.
      categoria = dplyr::coalesce(.data$categoria, .data$categoria_nuclei),
      tipo = tipo_fornitura(.data$categoria, .data$sottocategoria),
      gruppo = gruppo_fornitura(
        .data$tipo,
        .data$componenti_medi,
        .data$potenziamento
      )
    ) |>
    dplyr::select(-"giorni_potenziamento", -"categoria_nuclei")
}

#' Bidoni di alcune forniture, per servizio
#'
#' Contesto: l'anagrafica delle forniture non conserva i codici dei contenitori; per
#' mostrare di quali bidoni parla una riga di una tabella (per esempio la fornitura
#' con più eccedenza) si cercano nell'associazione dei contenitori.
#' Funzionamento: tiene i contenitori delle forniture indicate, attivi almeno un
#' giorno nel periodo e del servizio scelto (maiuscole e spazi ininfluenti); per
#' ogni fornitura elenca i codici distinti con il volume tra parentesi e, per un
#' contenitore gratuito, la parola «gratuito».
#' Risultato: `id_fornitura` e `bidoni`, per esempio `0000012345 (120 L)` o
#' `0000012345 (120 L, gratuito)`; i contenitori di una stessa fornitura sono
#' separati da una virgola.
#' @param contenitori Tabella di `importa_contenitori()`.
#' @param id_forniture Forniture da cercare.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @param servizio Servizio dei contenitori, o `NULL` per tutti.
#' @noRd
bidoni_delle_forniture <- function(contenitori, id_forniture, dal, al, servizio = "SECCO") {
  dal <- as.Date(dal)
  al <- as.Date(al)
  attivi <- contenitori |>
    dplyr::mutate(id_fornitura = as.character(.data$id_fornitura)) |>
    dplyr::filter(
      .data$id_fornitura %in% as.character(id_forniture),
      (.data$data_attivazione_contenitore <= al &
        .data$data_cessazione_contenitore >= dal) %in% TRUE
    )
  if (!is.null(servizio)) {
    attivi <- dplyr::filter(
      attivi,
      toupper(trimws(.data$servizio_transponder)) %in% toupper(trimws(servizio))
    )
  }
  attivi$contenitore_gratuito <- attivi[["contenitore_gratuito"]] %||% rep("N", nrow(attivi))
  attivi |>
    dplyr::distinct(.data$id_fornitura, .data$codice_transponder, .keep_all = TRUE) |>
    dplyr::mutate(
      volume_testo = ifelse(is.na(.data$volume), "", paste0(formatta_numero(.data$volume), " L")),
      gratuito = .data$contenitore_gratuito %in% "S",
      dettaglio = paste0(
        .data$volume_testo,
        ifelse(.data$gratuito, paste0(ifelse(nzchar(.data$volume_testo), ", ", ""), "gratuito"), "")
      ),
      bidone = paste0(
        .data$codice_transponder,
        ifelse(nzchar(.data$dettaglio), paste0(" (", .data$dettaglio, ")"), "")
      )
    ) |>
    dplyr::summarise(bidoni = paste(.data$bidone, collapse = ", "), .by = "id_fornitura")
}

#' Anagrafica delle forniture di tutti i servizi: una riga per fornitura e servizio
#'
#' Contesto: la tabella di forniture e contenitori del report mostra tutti i
#' servizi dell'associazione dei contenitori (secco, carta, plastica...), non solo
#' il secco.
#' Funzionamento: individua i servizi dei contenitori attivi nel periodo e per
#' ognuno chiama `anagrafica_forniture(servizio =)`, che conta solo i contenitori di
#' quel servizio. Una fornitura con contenitori di servizi diversi compare quindi
#' una volta per servizio, ciascuna con i suoi contenitori e volumi; gruppo,
#' componenti e potenziamento sono quelli della fornitura. I contenitori senza
#' servizio indicato non compaiono.
#' Risultato: la tabella di `anagrafica_forniture()` con in più le righe di tutti
#' i servizi (colonna `servizio` in maiuscolo).
#' @inheritParams anagrafica_forniture
#' @noRd
anagrafica_forniture_per_servizio <- function(contenitori, componenti, manovre, dal, al) {
  dal_data <- as.Date(dal)
  al_data <- as.Date(al)
  servizi <- contenitori |>
    dplyr::filter(
      (.data$data_attivazione_contenitore <= al_data &
        .data$data_cessazione_contenitore >= dal_data) %in% TRUE
    ) |>
    dplyr::pull("servizio_transponder") |>
    trimws() |>
    toupper() |>
    unique()
  servizi <- sort(servizi[!is.na(servizi) & nzchar(servizi)])
  dplyr::bind_rows(lapply(servizi, function(servizio) {
    anagrafica_forniture(contenitori, componenti, manovre, dal, al, servizio)
  }))
}

#' Anagrafica per servizio, dalle tabelle estratte
#'
#' Contesto: legge i tre file dell'anagrafica una volta sola e li passa ad
#' `anagrafica_forniture_per_servizio()`.
#' Risultato: la tabella di `anagrafica_forniture_per_servizio()`.
#' @param file Percorsi di `file_origine()`.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @noRd
leggi_anagrafica_forniture_per_servizio <- function(file = file_origine(), dal, al) {
  anagrafica_forniture_per_servizio(
    importa_contenitori(file$contenitori, categoria = TRUE),
    importa_componenti(file$componenti),
    importa_manovre(file$manovre),
    dal,
    al
  )
}

#' Forniture, contenitori e volumi per gruppo e servizio
#'
#' Contesto: la prima tabella del report. Per ogni gruppo di forniture (casa vuota,
#' potenziamento, da 1 a 6 o più componenti, condomini, non domestiche...) dice
#' quante forniture ci sono e quanti contenitori hanno, divisi per servizio.
#'
#' Funzionamento: per ogni gruppo una riga con `servizio` «Tutti i servizi» (le
#' forniture distinte del gruppo; i contenitori e i volumi sommati su tutti i
#' servizi) e una riga per ogni servizio presente (le forniture del gruppo con
#' contenitori di quel servizio; i loro contenitori e volumi). In coda le righe
#' «Domestiche (totale)» e «Tutte le forniture». `volume_totale` è la somma dei
#' volumi dei contenitori, `volume_medio` il volume totale diviso i contenitori.
#' `contenitori_gratuiti` sono quelli con `contenitore_gratuito` `S` (conferimenti
#' illimitati, non fatturati), `contenitori_non_gratuiti` gli altri.
#' I gruppi senza forniture non compaiono.
#'
#' Risultato: `gruppo`, `servizio`, `forniture`, `contenitori`,
#' `contenitori_gratuiti`, `contenitori_non_gratuiti`, `volume_totale` e
#' `volume_medio`.
#' @param anagrafica Tabella di `anagrafica_forniture_per_servizio()`.
#' @noRd
conta_forniture_per_servizio <- function(anagrafica) {
  gruppi <- gruppi_forniture()
  riga <- function(x, gruppo, servizio) {
    dplyr::tibble(
      gruppo = gruppo,
      servizio = servizio,
      forniture = dplyr::n_distinct(x$id_fornitura),
      contenitori = sum(x$contenitori),
      contenitori_gratuiti = sum(x$contenitori_gratuiti),
      contenitori_non_gratuiti = sum(x$contenitori_non_gratuiti),
      volume_totale = sum(x$volume_totale),
      volume_medio = ifelse(sum(x$contenitori) > 0, sum(x$volume_totale) / sum(x$contenitori), NA_real_)
    )
  }
  servizi <- sort(unique(anagrafica$servizio))
  # Il secco per primo: è il servizio fatturato.
  servizi <- c(intersect("SECCO", servizi), setdiff(servizi, "SECCO"))
  blocco <- function(x, nome) {
    if (nrow(x) == 0) {
      return(NULL)
    }
    dplyr::bind_rows(
      riga(x, nome, "Tutti i servizi"),
      lapply(servizi, function(servizio) {
        del_servizio <- x[x$servizio == servizio, ]
        if (nrow(del_servizio) > 0) riga(del_servizio, nome, servizio)
      })
    )
  }
  dplyr::bind_rows(
    lapply(gruppi, function(g) blocco(anagrafica[anagrafica$gruppo %in% g, ], g)),
    blocco(anagrafica[anagrafica$gruppo %in% gruppi[1:9], ], "Domestiche (totale)"),
    blocco(anagrafica, "Tutte le forniture")
  )
}

#' Anagrafica delle forniture di un periodo, dalle tabelle estratte
#'
#' Contesto: legge i tre file dell'anagrafica nella cartella dei dati estratti
#' (`file_origine()`) e li passa ad `anagrafica_forniture()`.
#' Risultato: la tabella di `anagrafica_forniture()`.
#' @param file Percorsi di `file_origine()`.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @param servizio Servizio dei contenitori da considerare, o `NULL` per tutti.
#' @noRd
leggi_anagrafica_forniture <- function(file = file_origine(), dal, al, servizio = NULL) {
  anagrafica_forniture(
    importa_contenitori(file$contenitori, categoria = TRUE),
    importa_componenti(file$componenti),
    importa_manovre(file$manovre),
    dal,
    al,
    servizio
  )
}

#' Conteggi di forniture e contenitori per gruppo
#'
#' Contesto: gli indicatori base `F` (forniture) e `C` (contenitori) del report:
#' casa vuota, potenziamento, da 1 a 6 o più componenti, condomini, non domestiche.
#' Risultato: per gruppo, `forniture` e `contenitori`, con il totale delle
#' domestiche (`F_D`) e di tutte le forniture in coda.
#' @param anagrafica Tabella di `anagrafica_forniture()`.
#' @noRd
conta_forniture <- function(anagrafica) {
  per_gruppo <- anagrafica |>
    dplyr::summarise(
      forniture = dplyr::n(),
      contenitori = sum(.data$contenitori),
      .by = "gruppo"
    ) |>
    dplyr::right_join(
      dplyr::tibble(gruppo = factor(gruppi_forniture(), gruppi_forniture())),
      by = "gruppo"
    ) |>
    dplyr::mutate(
      forniture = dplyr::coalesce(.data$forniture, 0L),
      contenitori = dplyr::coalesce(.data$contenitori, 0L)
    ) |>
    dplyr::arrange(.data$gruppo)
  domestiche <- per_gruppo$gruppo %in% gruppi_forniture()[1:9]
  dplyr::bind_rows(
    dplyr::mutate(per_gruppo, gruppo = as.character(.data$gruppo)),
    dplyr::tibble(
      gruppo = c("Domestiche (totale)", "Tutte le forniture"),
      forniture = c(sum(per_gruppo$forniture[domestiche]), sum(per_gruppo$forniture)),
      contenitori = c(sum(per_gruppo$contenitori[domestiche]), sum(per_gruppo$contenitori))
    )
  )
}

#' Forniture non domestiche per categoria
#'
#' Contesto: gli indicatori `F_ND[c]`: quante non domestiche ci sono per categoria.
#' Risultato: per categoria, il numero di forniture non domestiche.
#' @param anagrafica Tabella di `anagrafica_forniture()`.
#' @noRd
conta_non_domestiche <- function(anagrafica) {
  anagrafica |>
    dplyr::filter(.data$tipo == "non domestica") |>
    dplyr::count(categoria = dplyr::coalesce(.data$categoria, "(senza)"), name = "forniture") |>
    dplyr::arrange(dplyr::desc(.data$forniture))
}
