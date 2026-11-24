# Tariffe del secco residuo e calcolo di premi ed eccedenze di una fornitura, per
# il report `docs/indicatori_letture.qmd`. L'app non usa queste funzioni.
#
# Regole (tabella delle tariffe aggiornata del proprietario, 29/10/2026), per un
# anno intero e una fornitura domestica:
# - ogni fascia di componenti ha un volume annuo incluso nella tariffa; oltre si
#   paga un'eccedenza sul volume eccedente;
# - sotto il volume minimo addebitato non si riduce più niente;
# - fino al volume massimo per la premialità c'è un premio: (massimo - litri) ×
#   5/120 euro, dove i litri sono quelli conferiti o attesi a fine anno e non
#   scendono sotto il minimo (regola del proprietario, 25/11/2026: con un
#   componente il premio va da 0 euro a 480 litri a 10 euro a 240 litri o meno);
# - casa vuota non ha né minimo né premio; il potenziamento del secco non ha
#   premio e non paga eccedenza (litri infiniti: il beneficio è già pagato).
# I litri conferiti nel periodo si riportano alla fine dell'anno e per chi è
# attivo solo una parte dell'anno si ridimensionano tutte le soglie: minimo e
# massimo della premialità e limite dell'eccedenza (25/11/2026).

#' Tariffa annua del secco per fascia di componenti
#'
#' Contesto: tabella aggiornata dell'azienda. La colonna `componenti` è la fascia
#' (da 0 a 6, dove 6 vale «6 o più»). Gli anni passati usano la stessa tabella
#' (scelta del proprietario).
#' Risultato: un tibble con `componenti`, `fascia` (etichetta), `litri_inclusi`
#' (compresi nella tariffa variabile base annua), `litri_minimi` (minimo
#' addebitato) e `litri_premialita` (massimo per accedere alla premialità).
#' Minimo e premialità mancano per la casa vuota.
#' @noRd
tariffe_secco <- function() {
  dplyr::tibble(
    componenti = 0:6,
    fascia = c("0 (casa vuota)", "1", "2", "3", "4", "5", "6 o pi\u00f9"),
    litri_inclusi = c(240, 720, 960, 1200, 1440, 1680, 1680),
    litri_minimi = c(NA, 240, 360, 480, 600, 720, 840),
    litri_premialita = c(NA, 480, 600, 720, 840, 960, 1200)
  )
}

#' Penalità per l'eccedenza e premi, per volume del contenitore
#'
#' Contesto: tabella aggiornata dell'azienda. Un «eccedente» è un conferimento
#' oltre la soglia. Il premio non dipende più dal contenitore (regola del
#' proprietario, 25/11/2026): vedi `premio_al_litro()`.
#' Risultato: un tibble con `volume` (litri), `contenitore` ed `eccedenza` (euro
#' per conferimento eccedente).
#' @noRd
tariffe_contenitori <- function() {
  dplyr::tibble(
    volume = c(120, 60, 40, 30),
    contenitore = c(
      "carrellato",
      "press container",
      "bidoncino",
      "monoperatori stradali"
    ),
    eccedenza = c(10, 5, 3.34, 2.5)
  )
}

#' Euro di premio per ogni litro in meno del massimo per la premialità
#'
#' Contesto: regola del proprietario del 25/11/2026: il premio di una fornitura è
#' `(massimo per la premialità - litri) × 0,0416667`, il costo di un litro
#' conferito (5 euro ogni 120 litri). Vale per tutte le fasce e per ogni
#' contenitore, sacchetti compresi.
#' Funzionamento: 5/120, esatto (il proprietario scrive 0,0416667: con quel valore
#' arrotondato 240 litri sotto il massimo darebbero 10,000008 euro).
#' Risultato: un numero, euro per litro.
#' @noRd
premio_al_litro <- function() {
  5 / 120
}

#' Euro per litro di eccedenza di un contenitore
#'
#' Contesto: un volume si somma in litri anche quando i contenitori sono diversi,
#' quindi la tariffa per conferimento eccedente si porta a euro per litro. A un anno
#' intero il calcolo coincide con quello per conferimenti interi.
#' Funzionamento: euro per conferimento diviso il volume del contenitore. Un
#' volume che la tabella non ha (per esempio un contenitore da 240 litri) usa la
#' riga del carrellato da 120 litri.
#' Risultato: una lista con `eccedenza`, in euro per litro.
#' @param volume Volume del contenitore, in litri (un vettore).
#' @noRd
tariffa_al_litro <- function(volume) {
  tabella <- tariffe_contenitori()
  riga <- match(volume, tabella$volume)
  riga[is.na(riga)] <- 1L
  list(eccedenza = tabella$eccedenza[riga] / tabella$volume[riga])
}

#' Fascia di componenti dai componenti medi
#'
#' Contesto: i componenti di una fornitura possono cambiare nel periodo, quindi
#' la fascia si ricava dalla media pesata sul tempo (scelta del proprietario).
#' Funzionamento: arrotonda la media all'intero più vicino (0,5 va su) e riporta
#' a 6 tutto ciò che supera 6.
#' Risultato: un intero da 0 a 6; `NA` dove la media manca.
#' @param medi Componenti medi.
#' @noRd
fascia_componenti <- function(medi) {
  pmin(as.integer(floor(medi + 0.5)), 6L)
}

#' Premio, eccedenza e classe di ogni fornitura domestica del secco
#'
#' Contesto: cuore dell'analisi economica del report. Dai litri conferiti in un
#' periodo dice se la fornitura ha diritto a un premio, resta nella tariffa
#' ordinaria o paga un'eccedenza, e a quanto ammontano.
#'
#' Funzionamento:
#' 1. **litri attesi a fine anno** (regola del proprietario, 25/11/2026, che
#'    precisa l'annualizzazione dell'8/11): i litri del periodo si riportano alla
#'    fine dell'anno, `litri_annui = litri × quota_anno / (fattore ×
#'    quota_attivita)`, cioè i litri per giorno di attività della fornitura nel
#'    periodo, per i giorni di attività che la fornitura ha nell'anno. Il numero di
#'    bidoni non conta: i litri sono quelli della fornitura. Una fornitura attiva
#'    tutto l'anno ha `litri / fattore`, con `fattore` la parte di anno che occupa
#'    il periodo (giorni da `dal` a `al` / giorni dell'anno); una attivata o
#'    cessata durante l'anno prevede solo i litri dei suoi giorni di attività. È
#'    una stima lineare, senza stagionalità. Con `annualizza = FALSE` (giri
#'    indicati: litri di pochi giorni) si lascia `litri` com'è e si scalano le
#'    soglie per `fattore`;
#' 2. con il potenziamento non ci sono né premio né eccedenza (come se i litri
#'    consentiti fossero infiniti: la fornitura paga già il beneficio; il limite di
#'    6240 litri resta nel risultato solo come riferimento); altrimenti si usa la
#'    fascia di `componenti_medi` (`fascia_componenti()`); la casa vuota ha un
#'    limite di 240 litri, senza minimo né premio;
#' 3. se i litri attesi superano il limite la fornitura è «Eccedente» e paga
#'    `litri_eccedenti` per l'eccedenza al litro (`tariffa_al_litro()`);
#' 4. il **limite dell'eccedenza e il minimo e il massimo per la premialità si
#'    ridimensionano per i giorni di attività della fornitura nell'anno**
#'    (`quota_anno`, regola del proprietario, 25/11/2026: forniture attivate o
#'    cessate durante l'anno). La classe dipende così dal solo ritmo di
#'    conferimento della fornitura. Se i litri attesi
#'    sono al più il massimo ridimensionato la fornitura è in «Premialità» e il
#'    premio è `(massimo - litri addebitati) × 5/120` euro (`premio_al_litro()`):
#'    i litri addebitati sono quelli attesi ma non meno del minimo ridimensionato,
#'    quindi il premio cresce fino al minimo e sotto non cresce più. Per una
#'    fornitura attiva tutto l'anno con un componente (minimo 240, massimo 480):
#'    480 litri danno 0 euro, 360 danno 5, 240 o meno danno 10; con 6 o più
#'    (840-1200) il premio massimo è 15 euro. Non dipende dal volume del
#'    contenitore. Una fornitura senza letture ha zero litri e prende il premio
#'    massimo, ridimensionato come le soglie;
#' 5. nei casi in mezzo la classe è «Ordinaria».
#' L'importo è l'eccedenza meno il premio: positivo è da pagare, negativo da
#' accreditare. Euro e litri del risultato sono **annui** (con `annualizza = TRUE`),
#' quelli della premialità riferiti ai giorni di attività nell'anno; `litri`
#' resta il conferito nel periodo.
#'
#' Risultato: le stesse righe con `litri_annui` (i litri attesi a fine anno),
#' `fascia`, `regime` (`casa vuota`, `potenziamento` o `tariffa per fascia`),
#' `litri_limite`, `litri_minimi` e `litri_premialita` (limite dell'eccedenza,
#' minimo e massimo per la premialità, già ridimensionati), `litri_addebitati`, `classe`, `litri_eccedenti`,
#' `litri_sotto_massimo` (rispetto al massimo per la premialità), `eccedenza_eur`,
#' `quota_attivita`, `quota_anno`, `premio_eur` e `importo_eur`. Senza fattore (non
#' positivo o mancante) o senza giorni di attività nel periodo i litri attesi e la
#' classe mancano.
#' @param forniture Data frame con `litri` (conferiti nel periodo),
#'   `componenti_medi`, `potenziamento` (logico), `fattore` e, facoltative,
#'   `volume_contenitore` (litri, per l'eccedenza), `quota_attivita` (da 0 a 1,
#'   predefinita 1: la parte del periodo in cui la fornitura era attiva) e
#'   `quota_anno` (da 0 a 1, predefinita 1: la parte dell'anno in cui la fornitura
#'   è attiva).
#' @param annualizza Se riportare i litri alla fine dell'anno (predefinito) o
#'   scalare le soglie al periodo.
#' @noRd
economia_forniture <- function(forniture, annualizza = TRUE) {
  tariffe <- tariffe_secco()
  fattore <- forniture$fattore
  giorni_periodo <- quota_da_colonna(forniture[["quota_attivita"]], nrow(forniture))
  giorni_anno <- quota_da_colonna(forniture[["quota_anno"]], nrow(forniture))
  # Annualizzando, tutte le soglie (limite dell'eccedenza, minimo e massimo della
  # premialità) seguono i giorni di attività nell'anno; senza, seguono il periodo.
  if (annualizza) {
    litri <- riporta_ad_anno(forniture$litri, fattore, giorni_periodo, giorni_anno)
    scala_limite <- giorni_anno
    scala_premialita <- giorni_anno
  } else {
    litri <- forniture$litri
    scala_limite <- fattore
    scala_premialita <- fattore
    giorni_periodo <- rep(1, nrow(forniture))
    giorni_anno <- rep(1, nrow(forniture))
  }
  potenziamento <- forniture$potenziamento %in% TRUE
  fascia <- fascia_componenti(forniture$componenti_medi)
  riga <- match(fascia, tariffe$componenti)
  volume <- forniture[["volume_contenitore"]] %||%
    rep(120, nrow(forniture))
  al_litro <- tariffa_al_litro(volume)

  casa_vuota <- !potenziamento & fascia %in% 0L
  con_premio <- !potenziamento & fascia %in% 1:6
  regime <- dplyr::case_when(
    potenziamento ~ "potenziamento",
    casa_vuota ~ "casa vuota",
    con_premio ~ "tariffa per fascia",
    .default = NA_character_
  )
  limite <- dplyr::case_when(
    potenziamento ~ litri_potenziamento() * scala_limite,
    !is.na(riga) ~ tariffe$litri_inclusi[riga] * scala_limite,
    .default = NA_real_
  )
  minimi <- ifelse(con_premio, tariffe$litri_minimi[riga] * scala_premialita, NA_real_)
  premialita <- ifelse(
    con_premio,
    tariffe$litri_premialita[riga] * scala_premialita,
    NA_real_
  )
  litri_valutati <- litri
  addebitati <- ifelse(con_premio, pmax(litri, minimi), litri)

  eccedenti <- pmax(litri - limite, 0)
  eccedente <- !is.na(limite) & !potenziamento & litri > limite
  in_premialita <- con_premio & litri <= premialita
  classe <- dplyr::case_when(
    is.na(limite) | is.na(litri) ~ NA_character_,
    eccedente ~ "Eccedente",
    in_premialita ~ "Premialit\u00e0",
    .default = "Ordinaria"
  )
  sotto_massimo <- ifelse(in_premialita, premialita - addebitati, NA_real_)
  premio <- ifelse(in_premialita, premio_al_litro() * sotto_massimo, 0)
  eccedenza <- ifelse(eccedente, al_litro$eccedenza * eccedenti, 0)

  dplyr::mutate(
    forniture,
    litri_annui = litri_valutati,
    fascia = fascia,
    regime = regime,
    litri_limite = limite,
    litri_minimi = minimi,
    litri_premialita = premialita,
    litri_addebitati = addebitati,
    classe = factor(
      classe,
      c("Premialit\u00e0", "Ordinaria", "Eccedente")
    ),
    litri_eccedenti = ifelse(eccedente, eccedenti, 0),
    litri_sotto_massimo = sotto_massimo,
    eccedenza_eur = eccedenza,
    quota_attivita = giorni_periodo,
    quota_anno = giorni_anno,
    premio_eur = premio,
    importo_eur = eccedenza - premio
  )
}

#' Quota da 0 a 1 di una colonna facoltativa (mancante vale 1)
#' @noRd
quota_da_colonna <- function(x, n) {
  x <- x %||% rep(1, n)
  dplyr::coalesce(pmin(pmax(x, 0), 1), 1)
}

#' Riporta i litri del periodo alla fine dell'anno
#'
#' Contesto: annualizzazione dei litri (`economia_forniture()`) e dei
#' conferimenti attesi (`forniture_di_classe_per_scenario()`).
#' Funzionamento: `x × quota_anno / (fattore × quota_attivita)`. `fattore` è la
#' parte di anno che occupa il periodo (giorni da `dal` a `al` / giorni dell'anno),
#' `quota_attivita` la parte del periodo in cui la fornitura era attiva e
#' `quota_anno` la parte dell'anno in cui lo è: il rapporto è il consumo per
#' giorno di attività, moltiplicato per i giorni di attività dell'anno. Con
#' fornitura attiva tutto il periodo e tutto l'anno vale `x / fattore`.
#' Risultato: il valore atteso a fine anno; `NA` dove `fattore` o `quota_attivita`
#' non sono positivi o mancano.
#' @param x Valore del periodo (un vettore).
#' @param fattore Parte di anno, dello stesso vettore o di lunghezza 1.
#' @param quota_attivita,quota_anno Parti del periodo e dell'anno in cui la
#'   fornitura è attiva (predefinite 1).
#' @noRd
riporta_ad_anno <- function(x, fattore, quota_attivita = 1, quota_anno = 1) {
  giorni <- fattore * quota_attivita
  ifelse(giorni > 0, x * quota_anno / giorni, NA_real_)
}

#' Soglia massima di fatturazione di un gruppo di forniture, in litri all'anno
#'
#' Contesto: per confrontare i volumi di un gruppo con il limite oltre il quale la
#' fornitura paga l'eccedenza (i litri inclusi nella tariffa, `tariffe_secco()`).
#' Funzionamento: il gruppo «Casa vuota» ha la soglia della fascia 0, i gruppi da
#' «1 componente» a «6 o più» quella della loro fascia. Il potenziamento non paga
#' eccedenza (nessun limite), e «Componenti non noti», i condomini, le non
#' domestiche, «Altre» e i totali non hanno una fascia: mancante.
#' Risultato: un vettore numerico della lunghezza di `gruppo`.
#' @param gruppo Nomi dei gruppi (testo o fattore).
#' @noRd
soglia_fatturazione_del_gruppo <- function(gruppo) {
  tariffe <- tariffe_secco()
  gruppi <- gruppi_forniture()
  fascia <- c(0L, NA_integer_, 1:6, rep(NA_integer_, length(gruppi) - 8L))
  componenti <- fascia[match(as.character(gruppo), gruppi)]
  tariffe$litri_inclusi[match(componenti, tariffe$componenti)]
}

#' Fasce di litri eccedenti e quote di premio, per le tabelle del report
#'
#' Contesto: le tabelle del report contano le forniture per fascia di eccedenza
#' e per quota di premio. I limiti sono modificabili.
#' Funzionamento: `fasce_eccedenza()` taglia i litri eccedenti, `quote_premio()`
#' gli euro di premio, con gli estremi a destra inclusi.
#' Risultato: fattori ordinati con etichette leggibili; `NA` per chi non è
#' eccedente (o premiato).
#' @param litri_eccedenti,premio Valori da tagliare (zero e `NA` restano fuori).
#' @param limiti Estremi delle fasce, in litri o euro, crescenti.
#' @noRd
fasce_eccedenza <- function(litri_eccedenti, limiti = c(120, 240, 480)) {
  tagli_in_fasce(litri_eccedenti, limiti, " lt")
}

#' Quote di premio: come `fasce_eccedenza()`, sugli euro di premio
#' @noRd
quote_premio <- function(premio, limiti = c(5, 10, 15)) {
  tagli_in_fasce(premio, limiti, " \u20ac")
}

#' Taglia i valori positivi in fasce chiuse a destra, con l'ultima aperta
#' @noRd
tagli_in_fasce <- function(x, limiti, unita) {
  limiti <- sort(unique(limiti))
  etichette <- c(
    paste0("fino a ", formatta_numero(limiti[1]), unita),
    paste0(
      "da ",
      formatta_numero(limiti[-length(limiti)]),
      " a ",
      formatta_numero(limiti[-1]),
      unita
    ),
    paste0("oltre ", formatta_numero(limiti[length(limiti)]), unita)
  )
  x[!(x > 0)] <- NA_real_
  cut(x, c(0, limiti, Inf), labels = etichette, right = TRUE)
}
