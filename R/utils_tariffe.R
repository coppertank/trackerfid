# Tariffe del secco residuo e calcolo di premi ed eccedenze di una fornitura, per
# il report `docs/indicatori_letture.qmd`. L'app non usa queste funzioni.
#
# Regole (tabella delle tariffe aggiornata del proprietario, 29/10/2026), per un
# anno intero e una fornitura domestica:
# - ogni fascia di componenti ha un volume annuo incluso nella tariffa; oltre si
#   paga un'eccedenza sul volume eccedente;
# - sotto il volume minimo addebitato non si riduce più niente;
# - fino al volume massimo per la premialità c'è un premio, di 5 euro ogni 120 litri
#   (per un carrellato) che si aggiungono quanto meno si conferisce;
# - casa vuota e potenziamento del secco non hanno né minimo né premio.
# I volumi si riportano al periodo dell'analisi moltiplicandoli per il numero di
# giorni di attività diviso i giorni dell'anno.

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
#' oltre la soglia, e il premio è quello riconosciuto al volume massimo per la
#' premialità: cresce di pari importo per ogni conferimento in meno.
#' Risultato: un tibble con `volume` (litri), `contenitore`, `eccedenza` (euro per
#' conferimento eccedente) e `premio` (euro per conferimento in meno).
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
    eccedenza = c(10, 5, 3.34, 2.5),
    premio = c(5, 2.5, 1.67, 1.25)
  )
}

#' Euro per litro di eccedenza e di premio di un contenitore
#'
#' Contesto: un volume si somma in litri anche quando i contenitori sono diversi,
#' quindi le tariffe per conferimento si portano a euro per litro. A un anno
#' intero il calcolo coincide con quello per conferimenti interi.
#' Funzionamento: euro per conferimento diviso il volume del contenitore. Un
#' volume che la tabella non ha (per esempio un contenitore da 240 litri) usa la
#' riga del carrellato da 120 litri.
#' Risultato: una lista con `eccedenza` e `premio`, in euro per litro.
#' @param volume Volume del contenitore, in litri (un vettore).
#' @noRd
tariffa_al_litro <- function(volume) {
  tabella <- tariffe_contenitori()
  riga <- match(volume, tabella$volume)
  riga[is.na(riga)] <- 1L
  list(
    eccedenza = tabella$eccedenza[riga] / tabella$volume[riga],
    premio = tabella$premio[riga] / tabella$volume[riga]
  )
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
#' 1. i litri della tabella (inclusi, minimi, premialità) si moltiplicano per
#'    `fattore`, la parte di anno in cui la fornitura è attiva nel periodo;
#' 2. con il potenziamento vale il limite di 6240 litri, senza minimo né premio;
#'    altrimenti si usa la fascia di `componenti_medi` (`fascia_componenti()`);
#'    la casa vuota ha un limite di 240 litri, senza minimo né premio;
#' 3. se i litri superano il limite la fornitura è «Eccedente» e paga
#'    `litri_eccedenti` per l'eccedenza al litro (`tariffa_al_litro()`);
#' 4. se sono al più il volume massimo per la premialità è in «Premialità»: i
#'    litri contano almeno quanto il minimo addebitato, e il premio è quello di un
#'    conferimento (al volume massimo) più quello dei litri in meno, sempre al
#'    litro; fino al minimo aumenta, sotto non cresce più. Con un'annualità, un
#'    carrellato da 120 litri e un componente: 480 litri danno 5 euro, 360 danno
#'    10, 300 danno 12,50, 240 (il minimo) danno 15;
#' 5. nei casi in mezzo la classe è «Ordinaria».
#' L'importo è l'eccedenza meno il premio: positivo è da pagare, negativo da
#' accreditare. Con `fattore` minore di 1 premio ed eccedenza sono la quota del
#' periodo dell'importo annuo (il premio di un conferimento, 5 euro per un
#' carrellato, si riduce anch'esso di `fattore`).
#'
#' Risultato: le stesse righe con `fascia`, `regime` (`casa vuota`,
#' `potenziamento` o `tariffa per fascia`), `litri_limite`, `litri_minimi`,
#' `litri_premialita`, `litri_addebitati`, `classe`, `litri_eccedenti`,
#' `litri_sotto_massimo` (rispetto al massimo per la premialità), `eccedenza_eur`,
#' `premio_eur` e `importo_eur`.
#' @param forniture Data frame con `litri` (conferiti nel periodo),
#'   `componenti_medi`, `potenziamento` (logico), `fattore` e, facoltativa,
#'   `volume_contenitore` (litri).
#' @noRd
economia_forniture <- function(forniture) {
  tariffe <- tariffe_secco()
  fattore <- forniture$fattore
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
    potenziamento ~ litri_potenziamento() * fattore,
    !is.na(riga) ~ tariffe$litri_inclusi[riga] * fattore,
    .default = NA_real_
  )
  minimi <- ifelse(con_premio, tariffe$litri_minimi[riga] * fattore, NA_real_)
  premialita <- ifelse(
    con_premio,
    tariffe$litri_premialita[riga] * fattore,
    NA_real_
  )
  litri <- forniture$litri
  addebitati <- ifelse(con_premio, pmax(litri, minimi), litri)

  eccedenti <- pmax(litri - limite, 0)
  eccedente <- !is.na(limite) & litri > limite
  in_premialita <- con_premio & litri <= premialita
  classe <- dplyr::case_when(
    is.na(limite) ~ NA_character_,
    eccedente ~ "Eccedente",
    in_premialita ~ "Premialit\u00e0",
    .default = "Ordinaria"
  )
  sotto_massimo <- ifelse(in_premialita, premialita - addebitati, NA_real_)
  premio <- ifelse(
    in_premialita,
    al_litro$premio * (volume * fattore + sotto_massimo),
    0
  )
  eccedenza <- ifelse(eccedente, al_litro$eccedenza * eccedenti, 0)

  dplyr::mutate(
    forniture,
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
    premio_eur = premio,
    importo_eur = eccedenza - premio
  )
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
