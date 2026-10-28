# Propensione al conferimento e analisi economica del secco per il report
# `docs/indicatori_letture.qmd`. Parte dalle letture (`unita_di_lettura()`) e dalle
# forniture del periodo (`anagrafica_forniture()`). L'app non usa queste funzioni.
#
# Un conferimento è una lettura compatibile (`PIA`): il bidone è stato svuotato quel
# giorno, e vale il volume del suo contenitore. Ha senso confrontare i tre scenari
# di rilevazione: le letture di entrambi i sistemi insieme, solo delle saponette e
# solo delle antenne.

#' Gli scenari di rilevazione: quali letture contano
#'
#' Contesto: per giudicare un sistema si guarda che cosa succederebbe fatturando
#' con quel solo sistema. «Saponette e antenne» è il riferimento: unisce le
#' letture dei due sistemi, ognuna una volta.
#' Risultato: una lista con nome di insiemi di `sistemi_lettura()`.
#' @noRd
scenari_rilevazione <- function() {
  list(
    "Saponette e antenne" = c("Solo saponette", "Solo antenne", "Entrambe"),
    "Solo saponette" = c("Solo saponette", "Entrambe"),
    "Solo antenne" = c("Solo antenne", "Entrambe")
  )
}

#' Conferimenti e litri di ogni fornitura in uno scenario
#'
#' Contesto: tappa di `analisi_economica()` e di `propensione_conferimento()`.
#' Funzionamento: tiene le letture `PIA` dei sistemi dello scenario, del servizio
#' indicato, e le somma per fornitura. I litri di una lettura sono il volume del suo
#' contenitore; se manca, quello più frequente nella fornitura.
#' Risultato: `id_fornitura`, `conferimenti` e `litri`.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @param sistemi Sistemi dello scenario.
#' @param servizio Servizio dei contenitori, o `NULL` per tutti.
#' @noRd
conferimenti_per_fornitura <- function(unita, anagrafica, sistemi, servizio = NULL) {
  letture <- unita |>
    dplyr::filter(.data$stato %in% "PIA", .data$sistema %in% sistemi)
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
#' volume dei contenitori) del documento del proprietario. Dicono quanto, rispetto
#' a quanto ci si aspettava, i contenitori sono stati conferiti.
#'
#' Funzionamento: per gruppo di forniture (casa vuota, potenziamento, 1 ... 6 o
#' più componenti, più le domestiche in totale) conta i contenitori `C` dell'anagrafica e le
#' letture compatibili `L` dei due sistemi insieme di quelle forniture.
#' - `propensione_numerica` = `L / C`;
#' - `raccolte_attese` = raccolte annue previste × `fattore` × contenitori, e
#'   `propensione_sulle_attese` = `L` / raccolte attese;
#' - `propensione_volumetrica` = litri letti / capacità, dove la capacità è il
#'   volume dei contenitori × le raccolte attese.
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
      raccolte_attese = .data$raccolte_annue * .data$fattore * .data$contenitori,
      capacita = .data$volume_totale * .data$raccolte_annue * .data$fattore
    )
  riassunto <- function(x, gruppo) {
    dplyr::summarise(
      x,
      forniture = dplyr::n(),
      contenitori = sum(.data$contenitori),
      letture = sum(.data$conferimenti),
      raccolte_attese = sum(.data$raccolte_attese, na.rm = TRUE),
      litri = sum(.data$litri),
      capacita = sum(.data$capacita, na.rm = TRUE)
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
      propensione_sulle_attese = rapporto(.data$letture, .data$raccolte_attese),
      propensione_volumetrica = rapporto(.data$litri, .data$capacita)
    )
}

#' Forniture senza nessun conferimento nel periodo in cui se ne aspettavano
#'
#' Contesto: una fornitura senza letture non è una fornitura virtuosa: può avere il
#' tag rotto o non letto, il contenitore non esposto o lo smaltimento in un altro
#' modo. Il report le segnala e non le premia.
#' Funzionamento: tiene le forniture con tag dell'anagrafica che, nel periodo,
#' non hanno nessuna lettura compatibile dei due sistemi insieme e per cui le raccolte
#' attese (raccolte annue × `fattore`) sono almeno una.
#' Risultato: l'anagrafica di quelle forniture con `raccolte_attese`.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @noRd
forniture_senza_conferimenti <- function(unita, anagrafica) {
  lette <- unique(unita$id_fornitura[unita$stato %in% "PIA"])
  anagrafica |>
    dplyr::mutate(raccolte_attese = .data$raccolte_annue * .data$fattore) |>
    dplyr::filter(
      !.data$id_fornitura %in% lette,
      (.data$raccolte_attese >= 1) %in% TRUE
    )
}

#' Premio, eccedenza e classe delle forniture domestiche del secco, per scenario
#'
#' Contesto: l'analisi economica del report. Applica `economia_forniture()` ai litri
#' conferiti in ognuno degli scenari di rilevazione (`scenari_rilevazione()`).
#'
#' Funzionamento: la popolazione sono le forniture domestiche del secco
#' dell'anagrafica (condomini e non domestiche hanno regole di fatturazione
#' diverse), con i componenti noti, che hanno almeno un conferimento nello scenario
#' di riferimento. Una fornitura senza letture non si classifica: sarebbe un premio
#' dato per mancanza di dati (`forniture_senza_conferimenti()` la segnala; con i giri
#' le altre non si sanno nemmeno attribuire). Negli altri scenari una fornitura che il
#' riferimento ha letto e lo scenario no ha zero litri: è il premio che darebbe
#' quel solo sistema, e `rilevata` è falso.
#'
#' Risultato: per fornitura e scenario, `conferimenti`, `litri`, `rilevata` e le
#' colonne di `economia_forniture()`.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @noRd
analisi_economica <- function(unita, anagrafica) {
  popolazione <- anagrafica |>
    dplyr::filter(
      .data$tipo == "domestica",
      .data$servizio %in% "SECCO",
      !is.na(.data$componenti_medi)
    )
  scenari <- scenari_rilevazione()
  letti <- conferimenti_per_fornitura(unita, popolazione, scenari[[1]], "SECCO")
  popolazione <- dplyr::semi_join(popolazione, letti, by = "id_fornitura")
  purrr::imap(scenari, function(sistemi, nome) {
    conferimenti <- conferimenti_per_fornitura(unita, popolazione, sistemi, "SECCO")
    popolazione |>
      dplyr::left_join(conferimenti, by = "id_fornitura") |>
      dplyr::mutate(
        scenario = nome,
        rilevata = !is.na(.data$conferimenti),
        conferimenti = dplyr::coalesce(.data$conferimenti, 0L),
        litri = dplyr::coalesce(.data$litri, 0)
      ) |>
      economia_forniture()
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
#' `litri_eccedenti`, `premi_eur`, `eccedenze_eur` e `importo_eur`.
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
      litri_in_premialita = sum(.data$litri[.data$classe %in% "Premialit\u00e0"]),
      litri = sum(.data$litri),
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

#' Che cosa cambierebbe fatturando con un solo sistema
#'
#' Contesto: il confronto più vicino al costo reale tra saponette e antenne. Per
#' ogni fornitura si confronta la classe (premialità, ordinaria, eccedente) e
#' l'importo di uno scenario con quelli di «Saponette e antenne».
#'
#' Funzionamento: per ognuno dei due scenari a sistema singolo conta le forniture che
#' lo scenario ha letto, quelle con la stessa classe e quelle con una classe diversa,
#' i premi dati che il riferimento non darebbe (premialità solo nello scenario, di
#' solito per letture mancanti) e quelli mancati, e la differenza di importo.
#' L'importo è l'eccedenza meno il premio: una differenza positiva è più da pagare
#' del riferimento.
#'
#' Risultato: un tibble con una riga per scenario.
#' @param economia Risultato di `analisi_economica()`.
#' @noRd
impatto_dei_sistemi <- function(economia) {
  riferimento <- economia |>
    dplyr::filter(.data$scenario == names(scenari_rilevazione())[1]) |>
    dplyr::select(
      "id_fornitura",
      classe_riferimento = "classe",
      importo_riferimento = "importo_eur"
    )
  economia |>
    dplyr::filter(.data$scenario != names(scenari_rilevazione())[1]) |>
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
