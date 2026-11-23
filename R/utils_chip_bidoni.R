# Tipo di chip dei bidoni dell'azienda e copertura delle letture nel periodo, per il
# report `docs/indicatori_letture.qmd`. L'app non usa queste funzioni.
#
# Contesto (richiesta del proprietario, 23/11/2026): le antenne dei mezzi leggono solo
# i chip UHF, le saponette anche gli LF. Una panoramica dei bidoni per tipo di chip,
# servizio e comune dice quanti chip le antenne non leggeranno quando i mezzi con
# l'antenna passeranno da un comune; i bidoni letti nel periodo dicono la copertura, e
# un bidone con chip UHF mai letto dalle antenne è un caso da controllare (chip
# manomesso o danneggiato).

#' Bidoni dell'associazione attivi nel periodo, con il tipo di chip
#'
#' Contesto: il punto di partenza della panoramica dei chip. «Tutti i bidoni
#' dell'azienda» sono quelli di `dv_cnt_associazione_contenitori`; per sapere se sono
#' stati letti nel periodo contano quelli attivi almeno un giorno tra `dal` e `al`,
#' anche solo in parte (come nelle altre tabelle del report): un bidone cessato prima
#' del periodo non poteva essere letto.
#'
#' Funzionamento: tiene i contenitori con un codice vero (non nullo, non di soli zeri)
#' attivi nel periodo e, per ogni codice, la riga con l'attivazione più recente (come
#' in `associa_contenitore_e_fornitura()`: una lettura va alla fornitura attivata più
#' di recente). Il tipo di chip è quello delle letture, `associa_tipo_supporto_chip()`:
#' dai tag se il codice c'è; altrimenti `POSSIBILE UHF` se l'attivazione di quella riga
#' è del 2016 o dopo; altrimenti vuoto. Il servizio è quello del contenitore e il comune
#' quello della fornitura (`(senza ...)` se manca). Il cantiere viene dal comune.
#'
#' Risultato: un bidone per riga con `codice_transponder`, `servizio`, `comune`,
#' `cantiere`, `id_fornitura`, `stato_fornitura`, `data_attivazione_contenitore`,
#' `descrizione_tipo_supporto_chip`, `tipo_chip` (la descrizione, `(nessun tipo)` se
#' manca, un fattore nell'ordine dei tipi di `tipi_supporto_chip()`, poi `POSSIBILE
#' UHF` e `(nessun tipo)`) e `categoria` (`categoria_chip()`).
#' @param contenitori Tabella di `importa_contenitori()`.
#' @param tag Tabella di `importa_tag_contenitori()`, con il tipo di supporto.
#' @param dal,al Primo e ultimo giorno del periodo.
#' @noRd
bidoni_con_chip <- function(contenitori, tag, dal, al) {
  dal <- as.Date(dal)
  al <- as.Date(al)
  contenitori$stato_fornitura <- contenitori[["stato_fornitura"]] %||%
    rep(NA_character_, nrow(contenitori))
  attivi <- contenitori |>
    dplyr::filter(
      !is.na(.data$codice_transponder),
      !grepl("^0*$", .data$codice_transponder),
      (.data$data_attivazione_contenitore <= al &
        .data$data_cessazione_contenitore >= dal) %in% TRUE
    ) |>
    dplyr::arrange(.data$codice_transponder, dplyr::desc(.data$data_attivazione_contenitore)) |>
    dplyr::distinct(.data$codice_transponder, .keep_all = TRUE)
  tipo <- attivi |>
    dplyr::transmute(
      RFID = .data$codice_transponder,
      data_attivazione_contenitore = .data$data_attivazione_contenitore
    ) |>
    associa_tipo_supporto_chip(tag) |>
    suppressMessages()
  attivi |>
    dplyr::transmute(
      .data$codice_transponder,
      servizio = dplyr::coalesce(
        toupper(trimws(as.character(.data$servizio_transponder))),
        "(senza servizio)"
      ),
      comune = dplyr::coalesce(as.character(.data$comune_fornitura), "(senza comune)"),
      cantiere = dplyr::coalesce(cantiere_di_comune(.data$comune_fornitura), "(fuori tabella)"),
      .data$id_fornitura,
      .data$stato_fornitura,
      .data$data_attivazione_contenitore,
      descrizione_tipo_supporto_chip = tipo$descrizione_tipo_supporto_chip,
      tipo_chip = factor(
        dplyr::coalesce(tipo$descrizione_tipo_supporto_chip, "(nessun tipo)"),
        c(tipi_supporto_chip()$descrizione_tipo_supporto_chip, "POSSIBILE UHF", "(nessun tipo)")
      ),
      categoria = categoria_chip(tipo$descrizione_tipo_supporto_chip)
    )
}

#' Stati di lettura di un bidone nel periodo
#'
#' Contesto: `bidoni_letti()`. Un bidone può essere letto dalle saponette, dalle
#' antenne, da tutti e due o da nessuno.
#' Risultato: i quattro stati, nell'ordine dei grafici.
#' @noRd
stati_di_lettura_bidone <- function() {
  c("Entrambi", "Solo saponette", "Solo antenne", "Nessuno")
}

#' Quali bidoni sono stati letti nel periodo, e da quale sistema
#'
#' Contesto: la copertura. Le letture di partenza sono quelle del periodo dell'analisi,
#' di ogni servizio, dei due sistemi (`unita_di_lettura()`).
#' Funzionamento: un bidone è letto se il suo codice compare in una lettura
#' compatibile (`PIA`, `PIT` o `NP`, non scartata) del periodo: anche una lettura non
#' associata a una fornitura (per esempio fuori dalle date del contenitore) dice che il
#' chip si legge. È letto dalle saponette se la lettura è di `Solo saponette` o
#' `Entrambe`, dalle antenne se di `Solo antenne` o `Entrambe`.
#' Risultato: i bidoni con `letto_saponette`, `letto_antenne` e `lettura` (fattore con
#' i livelli di `stati_di_lettura_bidone()`).
#' @param bidoni Risultato di `bidoni_con_chip()`.
#' @param unita Risultato di `unita_di_lettura()` su tutte le letture del periodo.
#' @noRd
bidoni_letti <- function(bidoni, unita) {
  compatibili <- unita[as.character(unita$stato) %in% c("PIA", "PIT", "NP"), ]
  sistema <- as.character(compatibili$sistema)
  da_saponette <- unique(compatibili$RFID[sistema %in% c("Solo saponette", "Entrambe")])
  da_antenne <- unique(compatibili$RFID[sistema %in% c("Solo antenne", "Entrambe")])
  stati <- stati_di_lettura_bidone()
  bidoni |>
    dplyr::mutate(
      letto_saponette = .data$codice_transponder %in% da_saponette,
      letto_antenne = .data$codice_transponder %in% da_antenne,
      lettura = factor(
        dplyr::case_when(
          .data$letto_saponette & .data$letto_antenne ~ stati[1],
          .data$letto_saponette ~ stati[2],
          .data$letto_antenne ~ stati[3],
          .default = stati[4]
        ),
        stati
      )
    )
}

#' Bidoni, letti e non letti, per i gruppi indicati
#'
#' Contesto: tutte le tabelle della panoramica (per comune, per tipo di chip, per
#' servizio) sono la stessa somma con chiavi diverse.
#' Funzionamento: raggruppa per `chiavi` e conta i bidoni e, per ognuno dei tre
#' scenari (saponette e antenne insieme, solo saponette, solo antenne), quanti sono
#' stati letti e quanti no.
#' Risultato: per gruppo `bidoni`; `letti` e `non_letti` (da uno dei due sistemi);
#' `letti_saponette` e `non_letti_saponette`; `letti_antenne` e `non_letti_antenne`;
#' e la divisione degli stati: `entrambi`, `solo_saponette`, `solo_antenne`. In ordine
#' delle chiavi.
#' @param bidoni Risultato di `bidoni_letti()`.
#' @param chiavi Nomi delle colonne di raggruppamento.
#' @noRd
copertura_bidoni <- function(bidoni, chiavi) {
  bidoni |>
    dplyr::summarise(
      bidoni = dplyr::n(),
      letti = sum(.data$lettura != "Nessuno"),
      non_letti = sum(.data$lettura == "Nessuno"),
      letti_saponette = sum(.data$letto_saponette),
      non_letti_saponette = sum(!.data$letto_saponette),
      letti_antenne = sum(.data$letto_antenne),
      non_letti_antenne = sum(!.data$letto_antenne),
      entrambi = sum(.data$lettura == "Entrambi"),
      solo_saponette = sum(.data$lettura == "Solo saponette"),
      solo_antenne = sum(.data$lettura == "Solo antenne"),
      .by = dplyr::all_of(chiavi)
    ) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(chiavi)))
}

#' Bidoni con un chip che le antenne potevano leggere e non hanno letto
#'
#' Contesto: il caso da spottare: un bidone con chip UHF (o possibile UHF) che le
#' antenne non hanno rilevato nel periodo può avere il chip manomesso o danneggiato.
#' Attenzione: le antenne sono su una parte dei mezzi; un bidone di un comune dove non è
#' passato nessun mezzo con l'antenna non è sospetto. Il segnale più forte è un bidone
#' letto dalle saponette (c'è, il chip si legge) e non dalle antenne.
#' Funzionamento: tiene i bidoni delle categorie `Chip con UHF` e `Possibile UHF` non
#' letti dalle antenne. In ordine: prima quelli letti dalle saponette, poi quelli mai
#' letti; per comune, servizio e codice.
#' Risultato: i bidoni con `codice_transponder`, `comune`, `cantiere`, `servizio`,
#' `id_fornitura`, `tipo_chip`, `categoria`, `lettura` (`Solo saponette` o `Nessuno`).
#' @param bidoni Risultato di `bidoni_letti()`.
#' @noRd
bidoni_uhf_non_letti_dalle_antenne <- function(bidoni) {
  bidoni |>
    dplyr::filter(
      .data$categoria %in% c("Chip con UHF", "Possibile UHF"),
      !.data$letto_antenne
    ) |>
    dplyr::arrange(
      dplyr::desc(.data$letto_saponette),
      .data$comune,
      .data$servizio,
      .data$codice_transponder
    ) |>
    dplyr::select(
      "codice_transponder", "comune", "cantiere", "servizio", "id_fornitura",
      "tipo_chip", "categoria", "lettura"
    )
}
