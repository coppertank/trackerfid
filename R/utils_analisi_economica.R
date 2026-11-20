# Propensione al conferimento e analisi economica del secco per il report
# `docs/indicatori_letture.qmd`. Parte dalle letture (`unita_di_lettura()`) e dalle
# forniture del periodo (`anagrafica_forniture()`). L'app non usa queste funzioni.
#
# Un conferimento è una lettura associata (`PIA`): il bidone è stato svuotato quel
# giorno, e vale il volume del suo contenitore. Ha senso confrontare i tre scenari
# di rilevazione: le letture di entrambi i sistemi insieme, solo delle saponette e
# solo delle antenne.

#' Gli scenari di rilevazione: quali letture contano
#'
#' Contesto: l'analisi economica fotografa la situazione reale, con le letture
#' che ci sono nel periodo. Le antenne sono in funzione solo da agosto 2026 e su una
#' parte dei mezzi, le saponette dal 2025: un confronto a sistema singolo ha senso
#' soltanto se entrambi i sistemi hanno letture. Con un solo sistema simulare
#' l'altro darebbe zero litri a tutte le forniture, quindi il premio massimo a
#' tutte: un'informazione senza senso. Ogni scenario guarda le sole forniture che
#' le sue letture hanno visto (`analisi_economica()`).
#'
#' Funzionamento:
#' - con tutti e due i sistemi: «Saponette e antenne» (il riferimento, sempre il
#'   primo: unisce le letture dei due sistemi, ognuna una volta), poi «Solo
#'   saponette» e «Solo antenne» per il confronto;
#' - con le sole saponette: un solo scenario, «Rilevazione reale (Saponette)»;
#' - con le sole antenne: un solo scenario, «Rilevazione reale (Antenne)»;
#' - con nessuno dei due: nessuno scenario.
#' Con un sistema solo lo scenario prende tutte le letture disponibili, perché
#' l'altro sistema non c'è.
#'
#' Risultato: una lista con nome di insiemi di `sistemi_lettura()`.
#' @param con_antenne,con_saponette Se il sistema ha letture nel periodo.
#' @noRd
scenari_rilevazione <- function(con_antenne = TRUE, con_saponette = TRUE) {
  if (con_antenne && con_saponette) {
    list(
      "Saponette e antenne" = c("Solo saponette", "Solo antenne", "Entrambe"),
      "Solo saponette" = c("Solo saponette", "Entrambe"),
      "Solo antenne" = c("Solo antenne", "Entrambe")
    )
  } else if (con_saponette) {
    list("Rilevazione reale (Saponette)" = sistemi_lettura())
  } else if (con_antenne) {
    list("Rilevazione reale (Antenne)" = sistemi_lettura())
  } else {
    list()
  }
}

#' Quali sistemi hanno conferimenti del secco nelle letture
#'
#' Contesto: per scegliere gli scenari conta che un sistema abbia davvero
#' letture associate (`PIA`) del secco nel periodo, non solo che compaia nei file:
#' un'antenna che nel periodo ha letto solo altri rifiuti non dice niente sui
#' conferimenti del secco.
#' Funzionamento: guarda le letture `PIA` di bidoni con `servizio_transponder`
#' uguale a `SECCO`, viste da ogni sistema (una lettura «Entrambe» le conta tutte e
#' due).
#' Risultato: un vettore logico con nome, `antenne` e `saponette`.
#' @param unita Risultato di `unita_di_lettura()`.
#' @noRd
sistemi_con_conferimenti <- function(unita) {
  associate <- unita |>
    dplyr::filter(.data$stato %in% "PIA", .data$sistema %in% sistemi_lettura())
  associate <- dplyr::filter(associate, toupper(.data$servizio_transponder) %in% "SECCO")
  c(
    antenne = any(associate$sistema %in% c("Solo antenne", "Entrambe")),
    saponette = any(associate$sistema %in% c("Solo saponette", "Entrambe"))
  )
}

#' Conferimenti e litri di ogni fornitura in uno scenario
#'
#' Contesto: tappa di `analisi_economica()` e di `propensione_conferimento()`.
#' Funzionamento: tiene le letture `PIA` dei sistemi dello scenario, del servizio
#' indicato (`servizio_transponder`), e le somma per fornitura. Con
#' `escludi_gratuiti` toglie le letture dei contenitori gratuiti
#' (`contenitore_gratuito` `S`: conferimenti illimitati, non fatturati). I litri di
#' una lettura sono il volume del suo contenitore; se manca, quello più frequente
#' nella fornitura.
#' Risultato: `id_fornitura`, `conferimenti` e `litri`.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @param sistemi Sistemi dello scenario.
#' @param servizio Servizio dei contenitori, o `NULL` per tutti.
#' @param escludi_gratuiti Se togliere le letture dei contenitori gratuiti.
#' @noRd
conferimenti_per_fornitura <- function(
  unita,
  anagrafica,
  sistemi,
  servizio = NULL,
  escludi_gratuiti = FALSE
) {
  letture <- unita |>
    dplyr::filter(.data$stato %in% "PIA", .data$sistema %in% sistemi)
  if (escludi_gratuiti && "contenitore_gratuito" %in% names(letture)) {
    letture <- dplyr::filter(letture, !.data$contenitore_gratuito %in% "S")
  }
  if (!is.null(servizio)) {
    letture <- dplyr::filter(
      letture,
      toupper(.data$servizio_transponder) %in% toupper(servizio)
    )
  }
  letture |>
    dplyr::inner_join(
      dplyr::select(anagrafica, "id_fornitura", "volume_contenitore"),
      by = "id_fornitura"
    ) |>
    dplyr::summarise(
      conferimenti = dplyr::n(),
      litri = sum(dplyr::coalesce(.data$volume, .data$volume_contenitore)),
      .by = "id_fornitura"
    )
}

#' Propensione al conferimento delle forniture, per gruppo
#'
#' Contesto: gli indici `P_N` (letture per contenitore) e `P_V` (volume letto sul
#' volume massimo conferibile) del documento del proprietario. Dicono quanto dei
#' conferimenti possibili i contenitori hanno effettivamente fatto.
#' `numero_raccolte_annue` dell'associazione dei contenitori non è quanti
#' conferimenti ci si aspetta da una fornitura: è il **massimo** che quel bidone
#' potrebbe fare in un anno (26 con una raccolta ogni due settimane, 52 con una a
#' settimana). Molti clienti conferiscono meno, ed è normale: un valore sotto il
#' massimo non è un'anomalia.
#'
#' Funzionamento: per gruppo di forniture (casa vuota, potenziamento, 1 ... 6 o
#' più componenti, più le domestiche in totale) conta i contenitori `C` dell'anagrafica e le
#' letture associate (`PIA`) `L` dei due sistemi insieme di quelle forniture.
#' - `propensione_numerica` = `L / C`;
#' - `litri_massimi_annui` = volume massimo conferibile in un anno: per ogni
#'   contenitore, volume × massimo annuo di conferimenti, sommati;
#'   `volumetria_media_per_fornitura` e `volumetria_media_per_contenitore` lo
#'   dividono per le forniture e per i contenitori (di cui il massimo è noto);
#'   `soglia_fatturazione_annua` è la soglia massima di fatturazione della fascia
#'   (`soglia_fatturazione_del_gruppo()`), per un confronto rapido;
#' - `letture_per_fornitura` = `L` / forniture; `litri_per_fornitura` = litri
#'   letti / forniture; `litri_per_contenitore` = litri letti / contenitori;
#' - `conferimenti_massimi` = somma, sui contenitori, del massimo annuo di ognuno ×
#'   `fattore` (la parte di anno che occupa il periodo), e `propensione_sul_massimo` =
#'   `L` / conferimenti massimi;
#' - `propensione_volumetrica` = litri letti / `litri_massimi`, dove i litri massimi
#'   sono la somma, sui contenitori, di volume × massimo annuo × `fattore`.
#' Nei due rapporti col massimo entrano solo le forniture di cui il massimo è noto
#' (`letture_con_massimo`, `litri_con_massimo`).
#'
#' La base è l'`anagrafica` che si passa: nel report sono tutte le forniture e i
#' bidoni del secco attivi almeno un giorno nel periodo nell'associazione dei
#' contenitori, letti o no (`anagrafica_dei_bidoni_attivi()`; con i giri, solo
#' quelli letti, `anagrafica_dei_bidoni_letti()`). Una fornitura della base senza letture conta
#' con zero conferimenti. Le letture sono quelle della fornitura.
#'
#' Risultato: un tibble con una riga per gruppo.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @param servizio Servizio da considerare, o `NULL`.
#' @noRd
propensione_conferimento <- function(unita, anagrafica, servizio = NULL) {
  domestiche <- dplyr::filter(anagrafica, .data$tipo == "domestica")
  conferimenti <- conferimenti_per_fornitura(
    unita,
    domestiche,
    scenari_rilevazione()[[1]],
    servizio
  )
  per_fornitura <- domestiche |>
    dplyr::left_join(conferimenti, by = "id_fornitura") |>
    dplyr::mutate(
      conferimenti = dplyr::coalesce(.data$conferimenti, 0L),
      litri = dplyr::coalesce(.data$litri, 0),
      conferimenti_massimi = .data$conferimenti_massimi_annui * .data$fattore,
      litri_massimi = .data$litri_massimi_annui * .data$fattore,
      massimo_noto = !is.na(.data$conferimenti_massimi),
      # contenitori di cui il massimo è noto (senza il dato, tutti se il massimo è noto)
      contenitori_noti = if ("contenitori_con_massimo" %in% names(domestiche)) {
        .data$contenitori_con_massimo
      } else {
        ifelse(!is.na(.data$conferimenti_massimi), .data$contenitori, 0L)
      }
    )
  riassunto <- function(x, gruppo) {
    dplyr::summarise(
      x,
      forniture = dplyr::n(),
      contenitori = sum(.data$contenitori),
      letture = sum(.data$conferimenti),
      letture_con_massimo = sum(.data$conferimenti[.data$massimo_noto]),
      conferimenti_massimi = sum(.data$conferimenti_massimi, na.rm = TRUE),
      # prima di `litri`: una colonna appena creata nasconde l'originale
      litri_con_massimo = sum(.data$litri[.data$massimo_noto]),
      litri = sum(.data$litri),
      litri_massimi = sum(.data$litri_massimi, na.rm = TRUE),
      forniture_con_massimo = sum(.data$massimo_noto),
      contenitori_con_massimo = sum(.data$contenitori_noti),
      litri_massimi_annui = sum(.data$litri_massimi_annui, na.rm = TRUE)
    ) |>
      dplyr::mutate(gruppo = gruppo, .before = 1)
  }
  gruppi <- gruppi_forniture()[1:9]
  dplyr::bind_rows(
    lapply(gruppi, function(g) {
      riassunto(dplyr::filter(per_fornitura, .data$gruppo == g), g)
    }),
    riassunto(per_fornitura, "Domestiche (totale)")
  ) |>
    dplyr::mutate(
      propensione_numerica = rapporto(.data$letture, .data$contenitori),
      letture_per_fornitura = rapporto(.data$letture, .data$forniture),
      litri_per_fornitura = rapporto(.data$litri, .data$forniture),
      litri_per_contenitore = rapporto(.data$litri, .data$contenitori),
      propensione_sul_massimo = rapporto(.data$letture_con_massimo, .data$conferimenti_massimi),
      propensione_volumetrica = rapporto(.data$litri_con_massimo, .data$litri_massimi),
      volumetria_media_per_fornitura = rapporto(.data$litri_massimi_annui, .data$forniture_con_massimo),
      volumetria_media_per_contenitore = rapporto(.data$litri_massimi_annui, .data$contenitori_con_massimo),
      soglia_fatturazione_annua = soglia_fatturazione_del_gruppo(.data$gruppo)
    )
}

#' Forniture a pagamento dell'anagrafica
#'
#' Contesto: i contenitori gratuiti (`contenitore_gratuito` `S`) hanno un accordo
#' che permette di conferire senza limiti e senza fatturazione: una fornitura che ha
#' solo quelli non ha premio né eccedenza da perdere, quindi né l'analisi economica
#' né le forniture senza letture la considerano.
#'
#' Funzionamento: tiene le forniture con almeno un contenitore non gratuito
#' (`contenitori_non_gratuiti`). Un'anagrafica senza quella colonna (prima del
#' 10/11/2026) non ha contenitori gratuiti noti e resta com'è.
#'
#' Risultato: le righe di `anagrafica` con un contenitore a pagamento.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @noRd
con_contenitori_a_pagamento <- function(anagrafica) {
  if (!"contenitori_non_gratuiti" %in% names(anagrafica)) {
    return(anagrafica)
  }
  dplyr::filter(anagrafica, .data$contenitori_non_gratuiti > 0)
}

#' Forniture dell'analisi economica
#'
#' Contesto: l'analisi economica riguarda le sole domestiche del secco (condomini e
#' non domestiche hanno altre regole di fatturazione). La stessa popolazione serve
#' alla sezione «Forniture senza letture» del report, perché il numero delle senza
#' letture dell'economia e quello della sezione coincidano.
#'
#' Funzionamento: tiene le forniture domestiche del secco con i componenti noti e
#' almeno un contenitore a pagamento (`con_contenitori_a_pagamento()`); il volume
#' della tariffa è quello dei contenitori non gratuiti
#' (`volume_contenitore_non_gratuito`).
#'
#' Risultato: le righe di `anagrafica` dell'analisi economica.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @noRd
forniture_dell_economia <- function(anagrafica) {
  popolazione <- anagrafica |>
    dplyr::filter(
      .data$tipo == "domestica",
      .data$servizio %in% "SECCO",
      !is.na(.data$componenti_medi)
    ) |>
    con_contenitori_a_pagamento()
  if ("volume_contenitore_non_gratuito" %in% names(popolazione)) {
    popolazione <- dplyr::mutate(
      popolazione,
      volume_contenitore = dplyr::coalesce(
        .data$volume_contenitore_non_gratuito,
        .data$volume_contenitore
      )
    )
  }
  popolazione
}

#' Forniture dell'anagrafica senza conferimenti del secco nel periodo
#'
#' Contesto: il report segnala le forniture del secco che nel periodo non hanno
#' conferito, per fascia di componenti, e ne salva l'elenco
#' (`elenco_forniture_senza_letture()`). Una fornitura senza letture non è una
#' fornitura virtuosa: può avere il tag rotto o non letto, il contenitore non
#' esposto o lo smaltimento in un altro modo. Nell'analisi economica le domestiche
#' tra queste ricevono il premio massimo, proporzionale ai giorni di attività
#' (`analisi_economica()`). Storia: definizione del proprietario del 15-16/11/2026
#' (letture `PIA` di qualunque servizio, nessun altro controllo); il 20/11/2026 ha
#' chiesto di adottare il criterio dell'analisi economica, perché il numero della
#' sezione e quello del riepilogo per scenario coincidano.
#'
#' Funzionamento: una fornitura è «letta» se ha almeno un conferimento del secco a
#' pagamento, cioè una lettura associata (`PIA`) con `servizio_transponder`
#' `SECCO`, di un sistema qualunque, senza i contenitori gratuiti
#' (`conferimenti_per_fornitura()`, la stessa funzione dell'analisi economica; lo
#' scenario di riferimento «Saponette e antenne» ha tutti i sistemi). Una lettura
#' di un bidone di un altro servizio, o di un contenitore gratuito, non basta. Le
#' forniture con soli contenitori gratuiti non si considerano
#' (`con_contenitori_a_pagamento()`). `anagrafica` ha le forniture con almeno un
#' contenitore attivo almeno un giorno nel periodo (`anagrafica_forniture()`).
#'
#' Risultato: le righe di `anagrafica` a pagamento senza conferimenti, nello stesso
#' ordine. Le domestiche con componenti noti tra queste sono quelle che
#' `analisi_economica()` segnala come `senza_letture` nello scenario di riferimento
#' (le stesse che conta `forniture_dell_economia()`).
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @noRd
forniture_senza_letture <- function(unita, anagrafica) {
  a_pagamento <- con_contenitori_a_pagamento(anagrafica)
  lette <- conferimenti_per_fornitura(unita, a_pagamento, sistemi_lettura(), "SECCO", TRUE)$id_fornitura
  dplyr::filter(a_pagamento, !.data$id_fornitura %in% lette)
}

#' Elenco completo delle forniture senza letture, con i dati per capire perché
#'
#' Contesto: richiesta del proprietario (15/11/2026). Le forniture senza letture del
#' report sembravano più numerose di quelle reali: l'elenco salvato in
#' `data-raw/output` serve a guardarle una per una. Una fornitura può risultare
#' senza letture per motivi diversi: i suoi bidoni non sono mai stati letti; sono
#' stati letti ma la lettura non è associata (per esempio fuori dalle date di
#' attivazione e cessazione del contenitore, quindi `PIT` o `NP`); sono stati letti
#' e associati a un'altra fornitura (un contenitore che cambia fornitura, o due
#' forniture con il contenitore attivo nello stesso giorno); la fornitura è attiva
#' solo da pochi giorni del periodo o è cessata a database con il contenitore ancora
#' aperto.
#'
#' Funzionamento: parte dalle forniture senza letture
#' (`forniture_senza_letture()`) e per ognuna aggiunge: i codici dei bidoni del servizio attivi nel periodo
#' (`bidoni_delle_forniture()`); lo `stato_fornitura` dei contenitori; la prima
#' attivazione e l'ultima cessazione dei bidoni (cessazione `2099-12-31` se
#' aperta); le letture di quei codici nel periodo, di qualunque servizio, senza le
#' scartate: quante in tutto, quante non associate (`PIT` o `NP`, con la prima e
#' l'ultima data) e quante associate a un'altra fornitura; infine, delle letture
#' associate (`PIA`) alla fornitura, quante sono di un bidone di un altro servizio
#' e quante di un contenitore gratuito del servizio: non sono conferimenti, ma la
#' fornitura non è «mai letta» (`forniture_senza_letture()`).
#'
#' Risultato: una riga per fornitura con `id_fornitura`, `id_utenza`, `gruppo`,
#' `componenti_medi`, `potenziamento`, `contenitori`, `bidoni`, `volume_totale`,
#' `stato_fornitura`, `data_attivazione_bidoni`, `data_cessazione_bidoni`,
#' `giorni_attivi` (nel periodo), `letture_bidoni`, `letture_non_associate`,
#' `prima_lettura_non_associata`, `ultima_lettura_non_associata`,
#' `letture_altra_fornitura`, `letture_altri_servizi`,
#' `letture_contenitori_gratuiti`. Senza forniture, un tibble senza righe con le
#' stesse colonne.
#' @param senza Risultato di `forniture_senza_letture()`.
#' @param contenitori Tabella di `importa_contenitori()`.
#' @param unita Risultato di `unita_di_lettura()` su **tutte** le letture del
#'   periodo (non solo quelle del secco): serve a trovare le letture non associate.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @param servizio Servizio dei contenitori, o `NULL` per tutti.
#' @noRd
elenco_forniture_senza_letture <- function(senza, contenitori, unita, dal, al, servizio = "SECCO") {
  dal <- as.Date(dal)
  al <- as.Date(al)
  # Date estreme di un gruppo, mancanti se il gruppo e' vuoto (senza avvisi).
  prima <- function(x) if (length(x) > 0) min(x) else as.Date(NA)
  ultima <- function(x) if (length(x) > 0) max(x) else as.Date(NA)
  # Un'anagrafica senza il campo (file vecchi) lascia la colonna vuota.
  contenitori$stato_fornitura <- contenitori[["stato_fornitura"]] %||%
    rep(NA_character_, nrow(contenitori))
  bidoni <- contenitori |>
    bidoni_con_codice(servizio) |>
    dplyr::filter(
      .data$id_fornitura %in% senza$id_fornitura,
      (.data$data_attivazione_contenitore <= al &
        .data$data_cessazione_contenitore >= dal) %in% TRUE
    ) |>
    dplyr::distinct(.data$id_fornitura, .data$codice_transponder, .keep_all = TRUE)

  dei_bidoni <- bidoni |>
    dplyr::summarise(
      stato_fornitura = paste(sort(unique(stats::na.omit(.data$stato_fornitura))), collapse = ", "),
      data_attivazione_bidoni = prima(.data$data_attivazione_contenitore),
      data_cessazione_bidoni = ultima(.data$data_cessazione_contenitore),
      .by = "id_fornitura"
    )
  codici_testo <- bidoni_delle_forniture(contenitori, senza$id_fornitura, dal, al, servizio)

  # Le letture di quei codici nel periodo, ognuna con il suo stato e la fornitura
  # a cui è stata associata (le scartate non hanno una fornitura e non contano).
  codici <- dplyr::distinct(bidoni, .data$id_fornitura, codice = .data$codice_transponder)
  lette <- unita |>
    dplyr::filter(.data$RFID %in% codici$codice, as.character(.data$stato) %in% c("PIA", "PIT", "NP")) |>
    dplyr::transmute(
      codice = .data$RFID,
      .data$data_lettura,
      stato = as.character(.data$stato),
      fornitura_letta = as.character(.data$id_fornitura)
    )
  dei_codici <- dplyr::inner_join(codici, lette, by = "codice", relationship = "many-to-many")
  totali <- dei_codici |>
    dplyr::summarise(
      letture_bidoni = dplyr::n(),
      letture_non_associate = sum(.data$stato != "PIA"),
      letture_altra_fornitura = sum(.data$stato == "PIA" & !.data$fornitura_letta %in% .data$id_fornitura),
      .by = "id_fornitura"
    )
  non_associate <- dei_codici |>
    dplyr::filter(.data$stato != "PIA") |>
    dplyr::summarise(
      prima_lettura_non_associata = prima(.data$data_lettura),
      ultima_lettura_non_associata = ultima(.data$data_lettura),
      .by = "id_fornitura"
    )

  # Le letture associate alla fornitura che non sono conferimenti del servizio a
  # pagamento: di un bidone di un altro servizio o di un contenitore gratuito.
  associate <- unita |>
    dplyr::filter(
      as.character(.data$stato) %in% "PIA",
      .data$id_fornitura %in% senza$id_fornitura
    )
  servizio_letto <- toupper(as.character(
    associate[["servizio_transponder"]] %||% rep(NA_character_, nrow(associate))
  ))
  del_servizio <- if (is.null(servizio)) {
    rep(TRUE, nrow(associate))
  } else {
    servizio_letto %in% toupper(servizio)
  }
  gratuito <- (associate[["contenitore_gratuito"]] %||% rep(NA_character_, nrow(associate))) %in% "S"
  non_conferimenti <- dplyr::tibble(
    id_fornitura = as.character(associate$id_fornitura),
    altro_servizio = !del_servizio,
    gratuito_del_servizio = del_servizio & gratuito
  ) |>
    dplyr::summarise(
      letture_altri_servizi = sum(.data$altro_servizio),
      letture_contenitori_gratuiti = sum(.data$gratuito_del_servizio),
      .by = "id_fornitura"
    )

  senza |>
    dplyr::left_join(dei_bidoni, by = "id_fornitura") |>
    dplyr::left_join(codici_testo, by = "id_fornitura") |>
    dplyr::left_join(totali, by = "id_fornitura") |>
    dplyr::left_join(non_associate, by = "id_fornitura") |>
    dplyr::left_join(non_conferimenti, by = "id_fornitura") |>
    dplyr::mutate(
      letture_bidoni = dplyr::coalesce(.data$letture_bidoni, 0L),
      letture_non_associate = dplyr::coalesce(.data$letture_non_associate, 0L),
      letture_altra_fornitura = dplyr::coalesce(.data$letture_altra_fornitura, 0L),
      letture_altri_servizi = dplyr::coalesce(.data$letture_altri_servizi, 0L),
      letture_contenitori_gratuiti = dplyr::coalesce(.data$letture_contenitori_gratuiti, 0L)
    ) |>
    dplyr::arrange(.data$gruppo, .data$id_fornitura) |>
    dplyr::select(dplyr::any_of(c(
      "id_fornitura", "id_utenza", "gruppo", "componenti_medi", "potenziamento",
      "contenitori", "bidoni", "volume_totale", "stato_fornitura",
      "data_attivazione_bidoni", "data_cessazione_bidoni", "giorni_attivi",
      "letture_bidoni", "letture_non_associate", "prima_lettura_non_associata",
      "ultima_lettura_non_associata", "letture_altra_fornitura",
      "letture_altri_servizi", "letture_contenitori_gratuiti"
    )))
}

#' Premio, eccedenza e classe delle forniture domestiche del secco, per scenario
#'
#' Contesto: l'analisi economica del report. Applica `economia_forniture()` ai litri
#' conferiti in ognuno degli scenari di rilevazione (`scenari_rilevazione()`).
#'
#' Esclusione dei contenitori gratuiti: i bidoni con `contenitore_gratuito` `S`
#' hanno un accordo che permette di conferire senza limiti senza fatturazione, quindi
#' i loro conferimenti non contano e una fornitura con soli bidoni gratuiti non entra
#' nell'analisi.
#'
#' Funzionamento: gli scenari sono quelli validi per i sistemi che hanno letture
#' (`scenari_rilevazione()`): con un solo sistema c'è un solo scenario, la
#' situazione reale; solo con tutti e due si aggiungono i due a sistema singolo.
#' Il primo scenario è sempre il riferimento. **Ogni scenario è a sé stante**
#' (regola del proprietario, 16/11/2026, che sostituisce quella del 12/11): è come
#' se esistessero solo le letture dei suoi sistemi. **La popolazione è la stessa
#' in tutti gli scenari**: le forniture domestiche del secco attive nel periodo
#' (condomini e non domestiche hanno regole di fatturazione diverse), con i
#' componenti noti e almeno un contenitore a pagamento. I litri di ogni fornitura
#' sono quelli delle letture associate (`PIA`) dello scenario, sommate per
#' fornitura (due bidoni della stessa fornitura sommano il volume; le letture dello
#' stesso RFID sotto forniture diverse restano separate). **Una fornitura senza
#' letture nello scenario ha zero litri e riceve il premio massimo della sua
#' fascia, proporzionale ai giorni di attività** (`economia_forniture()`): è la
#' premialità per chi non risulta letto (`senza_letture` è vero). Con le letture di
#' un solo sistema, quindi, le forniture dei mezzi che quel sistema non copre
#' prendono il premio massimo: è quello che quel sistema da solo darebbe. Con i
#' giri indicati (`annualizza = FALSE`) il premio non si proporziona (le letture
#' sono un campione).
#'
#' Risultato: per fornitura e scenario, `conferimenti`, `litri`, `senza_letture` e
#' le colonne di `economia_forniture()` (litri ed euro annualizzati). Senza
#' scenari, un tibble senza righe.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @param con_antenne,con_saponette Quali sistemi hanno letture nel periodo
#'   (`scenari_rilevazione()`).
#' @param annualizza Se riportare i litri a un anno e usare le soglie annue
#'   (predefinito), o scalare le soglie al periodo (`economia_forniture()`).
#' @noRd
analisi_economica <- function(
  unita,
  anagrafica,
  con_antenne = TRUE,
  con_saponette = TRUE,
  annualizza = TRUE
) {
  # Condomini e non domestiche hanno altre regole; i contenitori gratuiti non si
  # fatturano: una fornitura che ha solo quelli esce dall'analisi, e delle altre
  # contano solo i contenitori a pagamento (`forniture_dell_economia()`).
  popolazione <- forniture_dell_economia(anagrafica)
  # Il premio è proporzionale ai giorni di attività solo con l'annualizzazione.
  popolazione$quota_attivita <- if (annualizza) {
    popolazione[["quota_attivita"]] %||% rep(1, nrow(popolazione))
  } else {
    rep(1, nrow(popolazione))
  }
  scenari <- scenari_rilevazione(con_antenne, con_saponette)
  if (length(scenari) == 0) {
    return(dplyr::tibble())
  }
  purrr::imap(scenari, function(sistemi, nome) {
    conferimenti <- conferimenti_per_fornitura(unita, popolazione, sistemi, "SECCO", TRUE)
    popolazione |>
      dplyr::left_join(conferimenti, by = "id_fornitura") |>
      dplyr::mutate(
        scenario = nome,
        senza_letture = is.na(.data$conferimenti),
        conferimenti = dplyr::coalesce(.data$conferimenti, 0L),
        litri = dplyr::coalesce(.data$litri, 0)
      ) |>
      economia_forniture(annualizza)
  }) |>
    dplyr::bind_rows() |>
    dplyr::mutate(scenario = factor(.data$scenario, names(scenari)))
}

#' Riepilogo economico per scenario (e per gruppo): classi, litri ed euro
#'
#' Contesto: gli indicatori economici del documento: numero e percentuale delle
#' forniture in premialità, in conferimento ordinario e eccedenti; litri in premialità e
#' eccedenti; euro di premi e di eccedenze.
#'
#' Funzionamento: raggruppa le righe di `analisi_economica()` per `scenario` (e,
#' con `per_gruppo`, per `gruppo`) e conta. I litri in premialità sono quelli
#' conferiti dalle forniture premiate; i litri sotto il massimo, la somma di
#' quanto hanno conferito in meno del massimo.
#'
#' Risultato: un tibble con `forniture`, `senza_letture` (le forniture senza letture
#' nello scenario), `premialita`, `ordinarie`, `eccedenti`, le quote
#' corrispondenti, `litri`, `litri_in_premialita`, `litri_sotto_massimo`,
#' `litri_eccedenti`, `premi_eur` (la premialità dovuta, proporzionale ai giorni di
#' attività), `premi_con_letture_eur` e `premi_senza_letture_eur` (le sue due
#' parti), `eccedenze_eur` e `importo_eur`. `litri_rilevati` sono i litri conferiti
#' nel periodo; `litri` e gli altri litri ed euro sono annualizzati
#' (`economia_forniture()`).
#' @param economia Risultato di `analisi_economica()`.
#' @param per_gruppo Se dividere anche per gruppo di forniture.
#' @noRd
riepilogo_economico <- function(economia, per_gruppo = FALSE) {
  chiavi <- if (per_gruppo) c("scenario", "gruppo") else "scenario"
  economia |>
    dplyr::summarise(
      forniture = dplyr::n(),
      premialita = sum(.data$classe %in% "Premialit\u00e0"),
      ordinarie = sum(.data$classe %in% "Ordinaria"),
      eccedenti = sum(.data$classe %in% "Eccedente"),
      litri_in_premialita = sum(.data$litri_annui[.data$classe %in% "Premialit\u00e0"]),
      litri_rilevati = sum(.data$litri),
      litri = sum(.data$litri_annui, na.rm = TRUE),
      litri_sotto_massimo = sum(.data$litri_sotto_massimo, na.rm = TRUE),
      litri_eccedenti = sum(.data$litri_eccedenti, na.rm = TRUE),
      premi_eur = sum(.data$premio_eur, na.rm = TRUE),
      premi_con_letture_eur = sum(.data$premio_eur[!.data$senza_letture], na.rm = TRUE),
      premi_senza_letture_eur = sum(.data$premio_eur[.data$senza_letture], na.rm = TRUE),
      # per ultimo: una colonna appena creata nasconde l'originale
      senza_letture = sum(.data$senza_letture),
      eccedenze_eur = sum(.data$eccedenza_eur, na.rm = TRUE),
      importo_eur = sum(.data$importo_eur, na.rm = TRUE),
      .by = dplyr::all_of(chiavi)
    ) |>
    dplyr::mutate(
      quota_premialita = rapporto(.data$premialita, .data$forniture),
      quota_ordinarie = rapporto(.data$ordinarie, .data$forniture),
      quota_eccedenti = rapporto(.data$eccedenti, .data$forniture),
      .after = "eccedenti"
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(chiavi)))
}

#' Forniture per fascia di eccedenza o per quota di premio, per scenario
#'
#' Contesto: «fasce di eccedenza» e «numero e percentuale delle forniture per quota di
#' premialità» del documento.
#' Funzionamento: taglia i litri eccedenti (`fasce_eccedenza()`) o gli euro di
#' premio (`quote_premio()`) delle forniture e le conta per scenario.
#' Risultato: per scenario e `intervallo`, `forniture` e `quota` sulle forniture
#' dello scenario.
#' @param economia Risultato di `analisi_economica()`.
#' @param su `"eccedenza"` o `"premio"`.
#' @param limiti Estremi delle fasce; senza, quelli predefiniti.
#' @noRd
forniture_per_fascia <- function(economia, su = c("eccedenza", "premio"), limiti = NULL) {
  su <- match.arg(su)
  tagliati <- if (su == "eccedenza") {
    fasce_eccedenza(economia$litri_eccedenti, limiti %||% c(120, 240, 480))
  } else {
    quote_premio(economia$premio_eur, limiti %||% c(5, 10, 15))
  }
  totali <- dplyr::count(economia, .data$scenario, name = "totale", .drop = FALSE)
  economia |>
    dplyr::mutate(intervallo = tagliati) |>
    dplyr::filter(!is.na(.data$intervallo)) |>
    dplyr::count(.data$scenario, .data$intervallo, name = "forniture", .drop = FALSE) |>
    dplyr::left_join(totali, by = "scenario") |>
    dplyr::mutate(quota = rapporto(.data$forniture, .data$totale)) |>
    dplyr::select(-"totale")
}

#' Estremi delle fasce di importo dell'eccedenza, dal parametro del report
#'
#' Contesto: il parametro `limiti_fasce_eccedenza` di `docs/indicatori_letture.qmd`
#' sceglie gli estremi superiori delle fasce di importo (50, 100 e 200 euro di
#' predefinito), per `forniture_eccedenti_per_fascia()`.
#' Funzionamento: accetta un vettore o una lista di numeri (come arriva dallo yaml)
#' o un testo con i numeri separati da virgole, punti e virgola o spazi
#' (`"50, 100, 200"`); il separatore dei decimali è il punto. Toglie i doppioni e
#' ordina. Ferma con un messaggio se non ci sono numeri, se uno non è leggibile o se
#' non è maggiore di zero.
#' Risultato: un vettore numerico crescente, senza doppioni.
#' @param x Valore del parametro.
#' @param nome Nome del parametro, per il messaggio d'errore.
#' @noRd
limiti_fasce_da_parametro <- function(x, nome = "limiti_fasce_eccedenza") {
  valori <- unlist(x)
  if (is.character(valori)) {
    valori <- unlist(strsplit(valori, "[,; ]+"))
    valori <- valori[nzchar(valori)]
  }
  numeri <- suppressWarnings(as.numeric(valori))
  if (length(numeri) == 0 || anyNA(numeri) || any(!is.finite(numeri)) || any(numeri <= 0)) {
    stop(
      "`", nome, "` deve contenere uno o pi\u00f9 importi in euro maggiori di zero, ",
      "per esempio 50, 100, 200.",
      call. = FALSE
    )
  }
  sort(unique(numeri))
}

#' Forniture eccedenti per fascia di componenti e per fascia di importo
#'
#' Contesto: riassume quante forniture pagherebbero un'eccedenza piccola o grande,
#' gruppo per gruppo (casa vuota, 1 ... 6 o più componenti, potenziamento), per
#' vedere dove l'eccedenza pesa di più.
#'
#' Funzionamento: nello scenario indicato (di norma il riferimento) conta, per
#' ogni gruppo, le forniture analizzate e quelle con un'eccedenza positiva
#' (`eccedenza_eur` > 0), e divide queste ultime in fasce di importo con estremi a
#' destra inclusi: con i `limiti` predefiniti 50, 100 e 200 euro sono «fino a 50
#' euro» (da zero escluso a 50 compreso), «oltre 50 e fino a 100», «oltre 100 e
#' fino a 200» e «oltre 200». In coda una riga «Totale». Il potenziamento compare con
#' zero eccedenti, perché non paga eccedenza. L'importo è quello di
#' `economia_forniture()`: annuo se i litri sono annualizzati, del periodo
#' altrimenti.
#'
#' Risultato: un tibble con `gruppo`, `forniture` (analizzate), `eccedenti`,
#' `eccedenza_totale_eur` e una colonna per fascia di importo, con il nome della
#' fascia (`attr(, "fasce")` ne elenca i nomi nell'ordine).
#' @param economia Risultato di `analisi_economica()`.
#' @param scenario Scenario da guardare; senza, il riferimento.
#' @param limiti Estremi delle fasce di importo, in euro, crescenti.
#' @noRd
forniture_eccedenti_per_fascia <- function(
  economia,
  scenario = scenario_riferimento(economia),
  limiti = c(50, 100, 200)
) {
  nome_scenario <- scenario
  limiti <- sort(unique(limiti))
  euro <- function(x) formatta_numero(x)
  intermedie <- if (length(limiti) > 1) {
    paste0("oltre ", euro(limiti[-length(limiti)]), " e fino a ", euro(limiti[-1]), " \u20ac")
  } else {
    character(0)
  }
  etichette <- c(
    paste0("fino a ", euro(limiti[1]), " \u20ac"),
    intermedie,
    paste0("oltre ", euro(limiti[length(limiti)]), " \u20ac")
  )
  forniture <- dplyr::filter(economia, .data$scenario == nome_scenario)
  fascia <- cut(
    forniture$eccedenza_eur,
    c(0, limiti, Inf),
    labels = etichette,
    right = TRUE
  )
  forniture$fascia_importo <- fascia
  riga <- function(x, gruppo) {
    conteggi <- vapply(etichette, function(e) sum(x$fascia_importo %in% e), numeric(1))
    esito <- dplyr::tibble(
      gruppo = gruppo,
      forniture = nrow(x),
      eccedenti = sum(!is.na(x$fascia_importo)),
      eccedenza_totale_eur = sum(x$eccedenza_eur[x$eccedenza_eur > 0], na.rm = TRUE)
    )
    esito[etichette] <- as.list(conteggi)
    esito
  }
  gruppi <- levels(forniture$gruppo)
  gruppi <- gruppi[gruppi %in% as.character(forniture$gruppo)]
  risultato <- dplyr::bind_rows(
    lapply(gruppi, function(g) riga(forniture[forniture$gruppo %in% g, ], g)),
    riga(forniture, "Totale")
  )
  attr(risultato, "fasce") <- etichette
  risultato
}

#' Le forniture con più eccedenza da pagare, per fascia di componenti
#'
#' Contesto: per ogni gruppo di forniture (casa vuota, 1 ... 6 o più componenti)
#' dice quali sono le utenze che, secondo la stima, pagherebbero di più in
#' eccedenza: i casi più gravi del gruppo. Il potenziamento non compare mai, perché
#' non paga eccedenza.
#'
#' Funzionamento: nello scenario indicato (di norma il riferimento) tiene le
#' forniture con un'eccedenza positiva e, per ogni gruppo, le `n` con
#' `eccedenza_eur` più alta (a pari eccedenza, più litri, poi l'ordine della
#' fornitura). I gruppi senza forniture eccedenti non compaiono e un gruppo con meno
#' di `n` eccedenti ne mostra meno. L'eccedenza è quella di `economia_forniture()`:
#' annua se i litri sono annualizzati, del periodo altrimenti.
#'
#' Risultato: per gruppo, al più `n` righe in ordine di eccedenza decrescente con
#' `gruppo`, `posizione` (1 è la più alta), `id_fornitura`, `id_utenza`,
#' `componenti_medi`, `litri` (rilevati nel periodo), `litri_annui`,
#' `litri_limite`, `litri_eccedenti` ed `eccedenza_eur`.
#' @param economia Risultato di `analisi_economica()`.
#' @param scenario Scenario da guardare; senza, il riferimento.
#' @param n Quante forniture tenere per gruppo.
#' @noRd
forniture_con_piu_eccedenza <- function(
  economia,
  scenario = scenario_riferimento(economia),
  n = 10
) {
  nome_scenario <- scenario
  economia |>
    dplyr::filter(.data$scenario == nome_scenario, .data$eccedenza_eur > 0) |>
    dplyr::arrange(
      .data$gruppo,
      dplyr::desc(.data$eccedenza_eur),
      dplyr::desc(.data$litri),
      .data$id_fornitura
    ) |>
    dplyr::mutate(posizione = dplyr::row_number(), .by = "gruppo") |>
    dplyr::filter(.data$posizione <= n) |>
    dplyr::select(
      "gruppo", "posizione", "id_fornitura", "id_utenza", "componenti_medi",
      "litri", "litri_annui", "litri_limite", "litri_eccedenti", "eccedenza_eur"
    )
}

#' Che cosa cambierebbe fatturando con un solo sistema
#'
#' Contesto: il confronto più vicino al costo reale tra saponette e antenne. Gli
#' scenari hanno le stesse forniture (`analisi_economica()`): per ognuna si
#' confronta la classe (premialità, ordinaria, eccedente) e l'importo dello
#' scenario con quelli del riferimento (il primo scenario, «Saponette e antenne»).
#'
#' Funzionamento: il riferimento è il primo livello di `scenario` (`scenario_riferimento()`).
#' Per ognuno degli altri scenari, a sistema singolo, conta le forniture che lo
#' scenario ha letto (`rilevate`) e quelle senza letture nello scenario
#' (`senza_letture`: prendono il premio massimo proporzionale), quelle con la stessa
#' classe e quelle con una classe diversa, i premi dati che il riferimento non
#' darebbe (premialità solo nello scenario, di solito per letture mancanti o per
#' una fornitura che il sistema non vede) e quelli mancati, e la differenza di
#' importo. L'importo è l'eccedenza meno il premio: una differenza positiva è più da
#' pagare del riferimento.
#'
#' Risultato: un tibble con una riga per scenario diverso dal riferimento (senza
#' righe se lo scenario è uno solo, cioè se c'è un solo sistema).
#' @param economia Risultato di `analisi_economica()`.
#' @noRd
impatto_dei_sistemi <- function(economia) {
  nome_riferimento <- scenario_riferimento(economia)
  riferimento <- economia |>
    dplyr::filter(.data$scenario == nome_riferimento) |>
    dplyr::select(
      "id_fornitura",
      classe_riferimento = "classe",
      importo_riferimento = "importo_eur"
    )
  economia |>
    dplyr::filter(.data$scenario != nome_riferimento) |>
    dplyr::inner_join(riferimento, by = "id_fornitura") |>
    dplyr::summarise(
      forniture = dplyr::n(),
      rilevate = sum(!.data$senza_letture),
      senza_letture = sum(.data$senza_letture),
      stessa_classe = sum(.data$classe == .data$classe_riferimento),
      classe_diversa = sum(.data$classe != .data$classe_riferimento),
      premi_in_piu = sum(
        .data$classe %in% "Premialit\u00e0" &
          !.data$classe_riferimento %in% "Premialit\u00e0"
      ),
      premi_in_meno = sum(
        !.data$classe %in% "Premialit\u00e0" &
          .data$classe_riferimento %in% "Premialit\u00e0"
      ),
      importo_riferimento_eur = sum(.data$importo_riferimento),
      importo_scenario_eur = sum(.data$importo_eur),
      differenza_eur = .data$importo_scenario_eur - .data$importo_riferimento_eur,
      .by = "scenario"
    )
}

#' Forniture di uno scenario in una classe, con conferimenti e volumetrie attesi a fine anno
#'
#' Contesto: base di `elenco_premialita()` e `elenco_eccedenza()` (richiesta del
#' proprietario, 16/11/2026). Per ogni fornitura di uno scenario in una classe
#' (premialità o eccedenza) servono: i componenti, i bidoni del secco attivi con
#' i loro codici e volumi, quanto è stato conferito nel periodo, quanto ci si
#' aspetta che manchi a fine anno e quanto ci si aspetta in tutto.
#'
#' Funzionamento: tiene le righe dello scenario (`analisi_economica()`) nella
#' classe indicata. **Stima lineare** con la stessa annualizzazione del resto del
#' report: il totale atteso è quello del periodo riportato a un anno
#' (`/ fattore`, uguale per tutte le forniture), il mancante è la differenza.
#' Con `dal` al 1 gennaio è esattamente la stima lineare dal giorno `al` al 31
#' dicembre; con un `dal` successivo è l'equivalente annuo (la stessa volumetria su
#' cui si decide la classe, quindi coerente con il premio o l'eccedenza). I
#' conferimenti e la volumetria sono quelli del solo scenario (`conferimenti` e
#' `litri` di `analisi_economica()`: letture associate dei bidoni del secco non
#' gratuiti, sommate per fornitura). I **bidoni** sono i contenitori del secco della
#' fornitura attivi almeno un giorno nel periodo con `stato_fornitura` `ATTIVA`
#' (`bidoni_delle_forniture()`): un contenitore gratuito compare con la parola
#' «gratuito» e non ha conferimenti; una fornitura cessata a database non ha bidoni
#' attivi.
#'
#' Risultato: un tibble con una riga per fornitura: `id_fornitura`,
#' `componenti_medi`, `fascia_componenti`, `bidoni_attivi`, `bidoni`,
#' `conferimenti_effettuati`, `conferimenti_mancanti_attesi`,
#' `conferimenti_attesi_fine_anno`, `volumetria_effettuata_l`,
#' `volumetria_mancante_attesa_l`, `volumetria_attesa_fine_anno_l`, poi le colonne
#' della classe (le aggiungono `elenco_premialita()` e `elenco_eccedenza()`) e
#' `id_utenza`, `stato_fornitura`, `giorni_attivi`, `quota_attivita`,
#' `senza_letture`.
#' @param economia Risultato di `analisi_economica()`.
#' @param scenario Nome dello scenario.
#' @param classe `Premialità` o `Eccedente`.
#' @param contenitori Tabella di `importa_contenitori()`.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @param servizio Servizio dei contenitori.
#' @noRd
forniture_di_classe_per_scenario <- function(economia, scenario, classe, contenitori, dal, al, servizio = "SECCO") {
  nome_scenario <- scenario
  nome_classe <- classe
  dal <- as.Date(dal)
  al <- as.Date(al)
  righe <- economia |>
    dplyr::filter(.data$scenario == nome_scenario, .data$classe %in% nome_classe) |>
    dplyr::arrange(.data$gruppo, .data$id_fornitura)
  bidoni <- bidoni_delle_forniture(contenitori, righe$id_fornitura, dal, al, servizio, stato = "ATTIVA")
  # Lo stato della fornitura, anche per chi non ha bidoni attivi.
  contenitori$stato_fornitura <- contenitori[["stato_fornitura"]] %||%
    rep(NA_character_, nrow(contenitori))
  stati <- contenitori |>
    bidoni_con_codice(servizio) |>
    dplyr::filter(
      .data$id_fornitura %in% righe$id_fornitura,
      (.data$data_attivazione_contenitore <= al &
        .data$data_cessazione_contenitore >= dal) %in% TRUE
    ) |>
    dplyr::summarise(
      stato_fornitura = paste(sort(unique(stats::na.omit(.data$stato_fornitura))), collapse = ", "),
      .by = "id_fornitura"
    )
  righe |>
    dplyr::left_join(bidoni, by = "id_fornitura") |>
    dplyr::left_join(stati, by = "id_fornitura") |>
    dplyr::mutate(
      conferimenti_attesi = riporta_ad_anno(.data$conferimenti, .data$fattore)
    ) |>
    dplyr::transmute(
      .data$id_fornitura,
      componenti_medi = round(.data$componenti_medi, 2),
      fascia_componenti = .data$fascia,
      bidoni_attivi = dplyr::coalesce(.data$numero_bidoni, 0L),
      bidoni = dplyr::coalesce(.data$bidoni, ""),
      conferimenti_effettuati = .data$conferimenti,
      conferimenti_mancanti_attesi = round(.data$conferimenti_attesi - .data$conferimenti, 1),
      conferimenti_attesi_fine_anno = round(.data$conferimenti_attesi, 1),
      volumetria_effettuata_l = .data$litri,
      volumetria_mancante_attesa_l = round(.data$litri_annui - .data$litri, 1),
      volumetria_attesa_fine_anno_l = round(.data$litri_annui, 1),
      .data$litri_minimi,
      .data$litri_premialita,
      .data$litri_limite,
      .data$litri_eccedenti,
      .data$premio_eur,
      .data$eccedenza_eur,
      .data$id_utenza,
      stato_fornitura = dplyr::coalesce(.data$stato_fornitura, ""),
      .data$giorni_attivi,
      quota_attivita = round(.data$quota_attivita, 4),
      senza_letture = dplyr::if_else(.data$senza_letture, "SI", "NO")
    )
}

#' Elenco delle forniture di uno scenario con diritto alla premialità
#'
#' Contesto: richiesta del proprietario (16/11/2026): per ogni scenario (saponette,
#' antenne, saponette e antenne) le forniture che hanno diritto alla premialità,
#' comprese quelle senza letture nello scenario (premio massimo).
#'
#' Funzionamento: `forniture_di_classe_per_scenario()` per la classe «Premialità»,
#' con in coda alle colonne della volumetria la **volumetria minima** e la
#' **volumetria massima** di riferimento per accedere alla premialità (i litri
#' annui della fascia: sotto il minimo il premio non cresce più, sopra il massimo
#' non c'è premio) e la **premialità attesa** a fine anno in euro: il premio annuo
#' sulla volumetria attesa, proporzionale ai giorni di attività
#' (`economia_forniture()`).
#'
#' Risultato: le colonne di `forniture_di_classe_per_scenario()` fino alla
#' volumetria attesa, `volumetria_minima_premialita_l`,
#' `volumetria_massima_premialita_l`, `premialita_attesa_eur`, e in coda
#' `id_utenza`, `stato_fornitura`, `giorni_attivi`, `quota_attivita` e
#' `senza_letture`.
#' @inheritParams forniture_di_classe_per_scenario
#' @noRd
elenco_premialita <- function(economia, scenario = scenario_riferimento(economia), contenitori, dal, al, servizio = "SECCO") {
  forniture_di_classe_per_scenario(economia, scenario, "Premialit\u00e0", contenitori, dal, al, servizio) |>
    dplyr::transmute(
      dplyr::across("id_fornitura":"volumetria_attesa_fine_anno_l"),
      volumetria_minima_premialita_l = .data$litri_minimi,
      volumetria_massima_premialita_l = .data$litri_premialita,
      premialita_attesa_eur = round(.data$premio_eur, 2),
      dplyr::across("id_utenza":"senza_letture")
    )
}

#' Elenco delle forniture di uno scenario in eccedenza
#'
#' Contesto: richiesta del proprietario (16/11/2026): l'equivalente di
#' `elenco_premialita()` per le forniture che pagherebbero un'eccedenza.
#'
#' Funzionamento: `forniture_di_classe_per_scenario()` per la classe «Eccedente»,
#' con in coda alle colonne della volumetria la **volumetria inclusa** nella tariffa
#' (i litri annui della fascia: oltre si paga), i **litri eccedenti attesi** a fine
#' anno e l'**eccedenza attesa** in euro (al litro, secondo il contenitore; non si
#' proporziona ai giorni di attività).
#'
#' Risultato: le colonne di `forniture_di_classe_per_scenario()` fino alla
#' volumetria attesa, `volumetria_inclusa_l`, `volumetria_eccedente_attesa_l`,
#' `eccedenza_attesa_eur`, e in coda `id_utenza`, `stato_fornitura`,
#' `giorni_attivi`, `quota_attivita` e `senza_letture`.
#' @inheritParams forniture_di_classe_per_scenario
#' @noRd
elenco_eccedenza <- function(economia, scenario = scenario_riferimento(economia), contenitori, dal, al, servizio = "SECCO") {
  forniture_di_classe_per_scenario(economia, scenario, "Eccedente", contenitori, dal, al, servizio) |>
    dplyr::transmute(
      dplyr::across("id_fornitura":"volumetria_attesa_fine_anno_l"),
      volumetria_inclusa_l = .data$litri_limite,
      volumetria_eccedente_attesa_l = round(.data$litri_eccedenti, 1),
      eccedenza_attesa_eur = round(.data$eccedenza_eur, 2),
      dplyr::across("id_utenza":"senza_letture")
    )
}

#' Nome dello scenario di riferimento di un'analisi economica
#'
#' Contesto: il riferimento è sempre il primo scenario (`scenari_rilevazione()`),
#' ma il suo nome dipende dai sistemi presenti («Saponette e antenne», oppure
#' «Rilevazione reale (Saponette)» con le sole saponette).
#' Risultato: il testo del primo livello di `scenario`; `NA` senza scenari.
#' @param economia Risultato di `analisi_economica()`.
#' @noRd
scenario_riferimento <- function(economia) {
  if (is.null(economia$scenario)) {
    return(NA_character_)
  }
  levels(economia$scenario)[1]
}

#' Forniture per fascia in colonne per scenario, o semplici se lo scenario è uno
#'
#' Contesto: le tabelle delle quote di premio e delle fasce di eccedenza. Con più
#' scenari ogni scenario ha le sue colonne; con uno solo i prefissi sarebbero
#' ridondanti e la tabella mostra solo la distribuzione delle forniture reali.
#' Funzionamento: con più scenari `tidyr::pivot_wider()` con colonne
#' «scenario: forniture» e «scenario: quota»; con uno, tiene `intervallo`,
#' `forniture` e `quota`.
#' Risultato: un tibble con `intervallo` e le colonne dei conteggi.
#' @param fasce Risultato di `forniture_per_fascia()`, con `quota` eventualmente
#'   già in testo.
#' @noRd
fasce_per_scenario <- function(fasce) {
  scenari <- unique(as.character(fasce$scenario))
  base <- dplyr::select(fasce, "scenario", "intervallo", "forniture", "quota")
  if (length(scenari) <= 1) {
    return(dplyr::select(base, "intervallo", "forniture", "quota"))
  }
  tidyr::pivot_wider(
    base,
    names_from = "scenario",
    values_from = c("forniture", "quota"),
    names_glue = "{scenario}: {.value}"
  )
}
