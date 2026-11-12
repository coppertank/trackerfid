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
#' tutte: un'informazione senza senso.
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
#' bidoni del secco attivi alla data `dal` nell'associazione dei contenitori, letti
#' o no (`anagrafica_dei_bidoni_attivi()`; con i giri, solo quelli letti,
#' `anagrafica_dei_bidoni_letti()`). Una fornitura della base senza letture conta
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

#' Forniture senza nessun conferimento nel periodo, pur potendone fare
#'
#' Contesto: una fornitura senza letture non è una fornitura virtuosa: può avere il
#' tag rotto o non letto, il contenitore non esposto o lo smaltimento in un altro
#' modo. Il report le segnala e non le premia. Si cercano solo quelle che nel
#' periodo potevano conferire almeno una volta, secondo il massimo annuo dei loro
#' contenitori (`numero_raccolte_annue`: il massimo possibile, non un'attesa).
#' Funzionamento: tiene le forniture con tag dell'anagrafica (di qualunque gruppo:
#' domestiche, condomini, non domestiche) che, nel periodo, non hanno nessuna
#' lettura associata dei due sistemi insieme e per cui i conferimenti massimi nel
#' periodo (massimo annuo dei contenitori × `fattore`) sono almeno uno. Le
#' forniture senza lettura che restano fuori non spariscono in silenzio: se ne
#' contano i due motivi nell'attributo `escluse`.
#' Risultato: l'anagrafica di quelle forniture con `conferimenti_massimi`. L'attributo
#' `escluse` è un vettore con `massimo_non_noto` (il massimo annuo manca nei dati)
#' e `massimo_sotto_uno` (nel periodo se ne potevano fare meno di uno).
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @noRd
forniture_senza_conferimenti <- function(unita, anagrafica) {
  lette <- unique(unita$id_fornitura[unita$stato %in% "PIA"])
  senza_letture <- anagrafica |>
    dplyr::mutate(conferimenti_massimi = .data$conferimenti_massimi_annui * .data$fattore) |>
    dplyr::filter(!.data$id_fornitura %in% lette)
  risultato <- dplyr::filter(senza_letture, (.data$conferimenti_massimi >= 1) %in% TRUE)
  attr(risultato, "escluse") <- c(
    massimo_non_noto = sum(is.na(senza_letture$conferimenti_massimi)),
    massimo_sotto_uno = sum(senza_letture$conferimenti_massimi < 1, na.rm = TRUE)
  )
  risultato
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
#' Il primo scenario è sempre il riferimento. La popolazione sono le forniture
#' domestiche del secco dell'anagrafica (condomini e non domestiche hanno regole di
#' fatturazione diverse), con i componenti noti, che hanno almeno un conferimento
#' nello scenario di riferimento. Una fornitura senza letture non si classifica:
#' sarebbe un premio dato per mancanza di dati (`forniture_senza_conferimenti()` la
#' segnala; con i giri le altre non si sanno nemmeno attribuire). Negli scenari a
#' sistema singolo una fornitura che il riferimento ha letto e lo scenario no ha zero
#' litri: è il premio che darebbe quel solo sistema, e `rilevata` è falso.
#'
#' Risultato: per fornitura e scenario, `conferimenti`, `litri`, `rilevata` e le
#' colonne di `economia_forniture()` (litri ed euro annualizzati). Senza
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
  popolazione <- anagrafica |>
    dplyr::filter(
      .data$tipo == "domestica",
      .data$servizio %in% "SECCO",
      !is.na(.data$componenti_medi)
    )
  # I contenitori gratuiti (`contenitore_gratuito` `S`) non si fatturano: una
  # fornitura che ha solo quelli esce dall'analisi, e delle altre contano solo i
  # contenitori a pagamento (il volume della tariffa è il loro).
  if ("contenitori_non_gratuiti" %in% names(popolazione)) {
    popolazione <- popolazione |>
      dplyr::filter(.data$contenitori_non_gratuiti > 0) |>
      dplyr::mutate(
        volume_contenitore = dplyr::coalesce(
          .data$volume_contenitore_non_gratuito,
          .data$volume_contenitore
        )
      )
  }
  scenari <- scenari_rilevazione(con_antenne, con_saponette)
  if (length(scenari) == 0) {
    return(dplyr::tibble())
  }
  letti <- conferimenti_per_fornitura(unita, popolazione, scenari[[1]], "SECCO", TRUE)
  popolazione <- dplyr::semi_join(popolazione, letti, by = "id_fornitura")
  purrr::imap(scenari, function(sistemi, nome) {
    conferimenti <- conferimenti_per_fornitura(unita, popolazione, sistemi, "SECCO", TRUE)
    popolazione |>
      dplyr::left_join(conferimenti, by = "id_fornitura") |>
      dplyr::mutate(
        scenario = nome,
        rilevata = !is.na(.data$conferimenti),
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
#' Risultato: un tibble con `forniture`, `premialita`, `ordinarie`, `eccedenti`, le
#' quote corrispondenti, `litri`, `litri_in_premialita`, `litri_sotto_massimo`,
#' `litri_eccedenti`, `premi_eur`, `eccedenze_eur` e `importo_eur`. `litri_rilevati`
#' sono i litri conferiti nel periodo; `litri` e gli altri litri ed euro sono
#' annualizzati (`economia_forniture()`).
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
#' Contesto: il confronto più vicino al costo reale tra saponette e antenne. Per
#' ogni fornitura si confronta la classe (premialità, ordinaria, eccedente) e
#' l'importo di uno scenario con quelli del riferimento (il primo scenario,
#' «Saponette e antenne»).
#'
#' Funzionamento: il riferimento è il primo livello di `scenario` (`scenario_riferimento()`).
#' Per ognuno degli altri scenari, a sistema singolo, conta le forniture che
#' lo scenario ha letto, quelle con la stessa classe e quelle con una classe diversa,
#' i premi dati che il riferimento non darebbe (premialità solo nello scenario, di
#' solito per letture mancanti) e quelli mancati, e la differenza di importo.
#' L'importo è l'eccedenza meno il premio: una differenza positiva è più da pagare
#' del riferimento.
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
      rilevate = sum(.data$rilevata),
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
