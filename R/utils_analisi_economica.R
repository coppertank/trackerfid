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
#' Funzionamento: guarda le letture `PIA` che contano per il secco (colonna
#' `secco` di `unita_di_lettura()`; senza la colonna, `servizio_transponder`
#' uguale a `SECCO`) viste da ogni sistema (una lettura «Entrambe» le conta tutte e
#' due).
#' Risultato: un vettore logico con nome, `antenne` e `saponette`.
#' @param unita Risultato di `unita_di_lettura()`.
#' @noRd
sistemi_con_conferimenti <- function(unita) {
  associate <- unita |>
    dplyr::filter(.data$stato %in% "PIA", .data$sistema %in% sistemi_lettura())
  associate <- if ("secco" %in% names(associate)) {
    dplyr::filter(associate, .data$secco %in% TRUE)
  } else {
    dplyr::filter(associate, toupper(.data$servizio_transponder) %in% "SECCO")
  }
  c(
    antenne = any(associate$sistema %in% c("Solo antenne", "Entrambe")),
    saponette = any(associate$sistema %in% c("Solo saponette", "Entrambe"))
  )
}

#' Conferimenti e litri di ogni fornitura in uno scenario
#'
#' Contesto: tappa di `analisi_economica()` e di `propensione_conferimento()`.
#' Funzionamento: tiene le letture `PIA` dei sistemi dello scenario, del servizio
#' indicato, e le somma per fornitura. Per il secco vale la regola delle saponette
#' e delle antenne (colonna `secco` di `unita_di_lettura()`); per un altro servizio
#' conta `servizio_transponder`. I litri di una lettura sono il volume del suo
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
    letture <- if (identical(toupper(servizio), "SECCO") && "secco" %in% names(letture)) {
      dplyr::filter(letture, .data$secco %in% TRUE)
    } else {
      dplyr::filter(
        letture,
        toupper(.data$servizio_transponder) %in% toupper(servizio)
      )
    }
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
#' letture associate (`PIA`) `L` dei due sistemi insieme di quelle forniture.
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
#' Funzionamento: tiene le forniture con tag dell'anagrafica (di qualunque gruppo:
#' domestiche, condomini, non domestiche) che, nel periodo, non hanno nessuna
#' lettura associata dei due sistemi insieme e per cui le raccolte attese
#' (raccolte annue × `fattore`) sono almeno una. Le forniture senza lettura che
#' restano fuori non spariscono in silenzio: se ne contano i due motivi
#' nell'attributo `escluse`.
#' Risultato: l'anagrafica di quelle forniture con `raccolte_attese`. L'attributo
#' `escluse` è un vettore con `raccolte_non_note` (le raccolte annue mancano nei
#' dati) e `meno_di_una_attesa` (nel periodo se ne aspetta meno di una).
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @noRd
forniture_senza_conferimenti <- function(unita, anagrafica) {
  lette <- unique(unita$id_fornitura[unita$stato %in% "PIA"])
  senza_letture <- anagrafica |>
    dplyr::mutate(raccolte_attese = .data$raccolte_annue * .data$fattore) |>
    dplyr::filter(!.data$id_fornitura %in% lette)
  risultato <- dplyr::filter(senza_letture, (.data$raccolte_attese >= 1) %in% TRUE)
  attr(risultato, "escluse") <- c(
    raccolte_non_note = sum(is.na(senza_letture$raccolte_attese)),
    meno_di_una_attesa = sum(senza_letture$raccolte_attese < 1, na.rm = TRUE)
  )
  risultato
}

#' Premio, eccedenza e classe delle forniture domestiche del secco, per scenario
#'
#' Contesto: l'analisi economica del report. Applica `economia_forniture()` ai litri
#' conferiti in ognuno degli scenari di rilevazione (`scenari_rilevazione()`).
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
#' colonne di `economia_forniture()` (comprese le proiezioni annue). Senza
#' scenari, un tibble senza righe.
#' @param unita Risultato di `unita_di_lettura()`.
#' @param anagrafica Risultato di `anagrafica_forniture()`.
#' @param con_antenne,con_saponette Quali sistemi hanno letture nel periodo
#'   (`scenari_rilevazione()`).
#' @noRd
analisi_economica <- function(
  unita,
  anagrafica,
  con_antenne = TRUE,
  con_saponette = TRUE
) {
  popolazione <- anagrafica |>
    dplyr::filter(
      .data$tipo == "domestica",
      .data$servizio %in% "SECCO",
      !is.na(.data$componenti_medi)
    )
  scenari <- scenari_rilevazione(con_antenne, con_saponette)
  if (length(scenari) == 0) {
    return(dplyr::tibble())
  }
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
#' `litri_eccedenti`, `premi_eur`, `eccedenze_eur` e `importo_eur`, più la
#' proiezione annua (`litri_proiettati`, `premi_proiettati_eur`,
#' `eccedenze_proiettate_eur`, `importo_proiettato_eur`: ogni fornitura riportata
#' a un anno intero con l'andamento del periodo, come in `economia_forniture()`).
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
      litri_proiettati = sum(.data$litri_proiettati, na.rm = TRUE),
      premi_proiettati_eur = sum(.data$premio_proiettato_eur, na.rm = TRUE),
      eccedenze_proiettate_eur = sum(.data$eccedenza_proiettata_eur, na.rm = TRUE),
      importo_proiettato_eur = sum(.data$importo_proiettato_eur, na.rm = TRUE),
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
