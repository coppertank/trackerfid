# Importazione delle tabelle estratte dal database aziendale.
#
# Ogni funzione legge un CSV di data-raw/origine/, scritto dal passo 1, e lo
# restituisce ripulito: nomi delle colonne del progetto, date e numeri
# convertiti, nomi dei comuni corretti. Fino al 19 ottobre 2026 lo stesso
# lavoro lo faceva il codice commentato dello script del passo 2, che salvava
# le tabelle in data-raw/intermedi/: oggi le richiama la pipeline di
# {targets}, `data-raw/_targets.R`, attraverso `prepara_letture_app()`, e le
# tabelle ripulite non vengono salvate.
# L'app non usa queste funzioni.

#' Nomi dei file estratti dal passo 1
#'
#' I file hanno sempre lo stesso nome a ogni estrazione: la pipeline li
#' riconosce da qui. Quello dei test dei mezzi, scaricato da AppSheet, va
#' rinominato `AppSheet.ViewData.csv`.
#'
#' @param cartella Cartella dei file.
#' @return Lista con nome: il percorso di ogni file.
#' @noRd
file_origine <- function(cartella = "data-raw/origine") {
  nomi <- c(
    test_mezzi = "AppSheet.ViewData.csv",
    letture = "letture_antenne.csv",
    contenitori = "dv_cnt_associazione_contenitori.csv",
    gruppi_sacchetti = "dv_cnt_gruppi_sacchetti.csv",
    servizi_sacchetti = "dv_cnt_gruppi_sacchetti_servizi.csv",
    calendario = "iv_estrazione_giri_servizi_rifiuti_filtered.csv",
    componenti = "dv_associazione_componenti.csv",
    manovre = "dv_potenziamento_secco.csv",
    storiche = "dv_cnt_letture.csv"
  )
  as.list(stats::setNames(file.path(cartella, nomi), names(nomi)))
}

#' Legge un CSV estratto, tutto come testo
#'
#' Il separatore è quello del file, virgola o punto e virgola. I valori di
#' `na` diventano mancanti; gli spazi ai bordi si tolgono.
#'
#' @param percorso Percorso del file.
#' @param na Testi da leggere come mancanti.
#' @return Data frame di sole colonne di testo, con i nomi del file.
#' @noRd
leggi_tabella_origine <- function(
  percorso,
  na = c("", "NA", "NaN", "null", "NULL")
) {
  if (!file.exists(percorso)) {
    stop("File non trovato: ", percorso, call. = FALSE)
  }
  data.table::fread(
    percorso,
    sep = rileva_separatore(percorso),
    header = TRUE,
    colClasses = "character",
    na.strings = na,
    encoding = "UTF-8",
    strip.white = TRUE,
    check.names = FALSE,
    showProgress = FALSE,
    data.table = FALSE
  )
}

#' Numeri con la virgola o con il punto decimale
#' @noRd
numero_testo <- function(x) {
  suppressWarnings(as.numeric(sub(",", ".", x, fixed = TRUE)))
}

#' Date senza ora, con una data lontana per i periodi ancora aperti
#' @noRd
data_o_aperta <- function(x, aperta = TRUE) {
  data <- lubridate::as_date(x)
  if (aperta) {
    data[is.na(data)] <- as.Date("2099-12-31")
  }
  data
}

#' Test delle antenne dei mezzi
#'
#' Dalla tabella scaricata da AppSheet: targa, matricola, esito e giorno del
#' test della sezione B. Il trattino vale come mancante; l'ora del verbale ha
#' i punti al posto dei due punti.
#'
#' @param percorso CSV dei test.
#' @return Data frame con `targa_veicolo`, `matricola_veicolo`, `esito` e
#'   `data_test`.
#' @noRd
importa_test_mezzi <- function(percorso) {
  grezzo <- leggi_tabella_origine(percorso, na = c("", "NA", "-"))
  verbale <- gsub(
    ".",
    ":",
    grezzo[["ULTIMA DATA VERBALE SEZIONE B"]],
    fixed = TRUE
  )
  data.frame(
    targa_veicolo = grezzo[["TARGA_VEICOLO"]],
    matricola_veicolo = grezzo[["MATRICOLA_VEICOLO"]],
    esito = grezzo[["ESITO TEST SEZIONE B"]],
    data_test = lubridate::as_date(suppressWarnings(lubridate::ymd_hm(
      verbale
    ))),
    stringsAsFactors = FALSE
  )
}

#' Letture delle antenne
#'
#' Il comune di lettura è il testo dopo la prima virgola di
#' `IndirizzoLocalizzazione`, in maiuscolo: il sistema di localizzazione dei
#' mezzi scrive «indirizzo, comune». Il cantiere viene dalla tabella dei
#' comuni, per nome esatto. Restano fuori le letture senza posizione,
#' latitudine zero, e quelle senza comune.
#'
#' L'istante è letto nel fuso orario della sessione, come i turni del
#' calendario in `associa_servizio_atteso_da_calendario()`: i due devono
#' restare nello stesso fuso.
#'
#' @param percorso CSV delle letture delle antenne.
#' @param comuni Tabella dei comuni, con `comune` e `cantiere`.
#' @return Data frame con le colonne che `filtra_letture_post_test()` si
#'   aspetta.
#' @noRd
importa_letture <- function(percorso, comuni = comuni_cantieri()) {
  grezzo <- leggi_tabella_origine(percorso, na = c("", "NA"))
  indirizzo <- grezzo[["IndirizzoLocalizzazione"]]
  # Il secondo pezzo, tra la prima e l'eventuale seconda virgola.
  con_virgola <- grepl(",", indirizzo, fixed = TRUE)
  comune <- ifelse(
    con_virgola,
    sub("^[^,]*,\\s*([^,]*).*$", "\\1", indirizzo),
    NA_character_
  )
  istante <- grezzo[["DataeOraPrimaLettura"]]
  letture <- data.frame(
    giorno_lettura = as.POSIXct(istante, format = "%d/%m/%Y %H.%M.%S"),
    giorno = lubridate::as_date(suppressWarnings(lubridate::dmy_hms(istante))),
    targa_veicolo = grezzo[["Targa"]],
    RFID = grezzo[["RFID(EPC)"]],
    comune_lettura = toupper(stringr::str_trim(comune)),
    NumeroLetture = grezzo[["NumeroLetture"]],
    latitudine = numero_testo(grezzo[["Latitudine"]]),
    longitudine = numero_testo(grezzo[["Longitudine"]]),
    stringsAsFactors = FALSE
  )
  tenute <- letture$latitudine != 0 & letture$comune_lettura != ""
  letture <- letture[tenute %in% TRUE, , drop = FALSE]
  letture$cantiere <- comuni$cantiere[match(
    letture$comune_lettura,
    comuni$comune
  )]
  rownames(letture) <- NULL
  letture
}

#' Anagrafica dei contenitori e delle loro forniture
#'
#' Tiene i contenitori con un codice transponder. Una cessazione mancante
#' vuol dire ancora attivo. Corregge due nomi di comune scritti male
#' nell'anagrafica: `LUSINA CONCO` e `VALSTAGNA`, soppresso nel 2019 e oggi
#' parte di `VALBRENTA`.
#'
#' @param percorso CSV dell'associazione dei contenitori.
#' @return Data frame con i dati del contenitore e quelli della fornitura,
#'   con i nomi che `associa_servizio()` si aspetta.
#' @noRd
importa_contenitori <- function(percorso) {
  grezzo <- leggi_tabella_origine(percorso)
  grezzo <- grezzo[!is.na(grezzo$codice_transponder), , drop = FALSE]
  comune <- grezzo$comune_servizio
  comune[comune %in% "LUSINA CONCO"] <- "LUSIANA CONCO"
  comune[comune %in% "VALSTAGNA"] <- "VALBRENTA"
  data.frame(
    codice_transponder = grezzo$codice_transponder,
    servizio_transponder = grezzo$descrizione_rifiuto,
    volume = numero_testo(grezzo$volume),
    frequenza = grezzo$frequenza,
    numero_raccolte_annue = numero_testo(grezzo$numero_raccolte_annue),
    data_attivazione_contenitore = data_o_aperta(
      grezzo$data_attivazione_contenitore,
      aperta = FALSE
    ),
    data_cessazione_contenitore = data_o_aperta(
      grezzo$data_cessazione_contenitore
    ),
    id_fornitura = grezzo$id_servizio,
    stato_fornitura = grezzo$stato_servizio,
    id_utenza = grezzo$id_utenza,
    comune_fornitura = comune,
    stringsAsFactors = FALSE
  )
}

#' Sacchetti e loro fornitura
#'
#' Un gruppo di sacchetti può essere stato associato a più forniture nel
#' tempo: vale l'associazione con l'ultima variazione più recente, e a pari
#' data la prima della tabella.
#'
#' @param percorso_gruppi CSV dei gruppi di sacchetti, con `codice_uhf` e
#'   `codice_gruppo_sacchetti`.
#' @param percorso_servizi CSV delle associazioni tra gruppi e forniture.
#' @return Data frame con `codice_uhf` e `id_fornitura`.
#' @noRd
importa_sacchetti <- function(percorso_gruppi, percorso_servizi) {
  gruppi <- leggi_tabella_origine(percorso_gruppi)
  servizi <- leggi_tabella_origine(percorso_servizi)
  variazione <- as.POSIXct(servizi$ultima_variazione)
  ordine <- order(
    servizi$codice_gruppo_sacchetti,
    -as.numeric(variazione),
    method = "radix"
  )
  servizi <- servizi[ordine, , drop = FALSE]
  servizi <- servizi[
    !duplicated(servizi$codice_gruppo_sacchetti),
    ,
    drop = FALSE
  ]
  data.frame(
    codice_uhf = gruppi$codice_uhf,
    id_fornitura = servizi$id_servizio[match(
      gruppi$codice_gruppo_sacchetti,
      servizi$codice_gruppo_sacchetti
    )],
    stringsAsFactors = FALSE
  )
}

#' Calendario dei giri
#'
#' Tiene i turni con un mezzo, la cui matricola perde le lettere iniziali per
#' tornare quella delle letture. Il comune diventa `comune_giro`, con
#' `ROSA'` scritto `ROSÀ`, come nella tabella dei comuni.
#'
#' @param percorso CSV del calendario dei giri.
#' @return Data frame con le colonne del file, `giorno` come data e
#'   `comune_giro`.
#' @noRd
importa_calendario <- function(percorso) {
  calendario <- leggi_tabella_origine(percorso)
  calendario <- calendario[!is.na(calendario$matricola_mezzo), , drop = FALSE]
  calendario$matricola_mezzo <- sub("^[A-Za-z]", "", calendario$matricola_mezzo)
  calendario$giorno <- lubridate::as_date(calendario$giorno)
  calendario$comune[calendario$comune %in% "ROSA'"] <- "ROS\u00c0"
  names(calendario)[names(calendario) == "comune"] <- "comune_giro"
  rownames(calendario) <- NULL
  calendario
}

#' Composizione dei nuclei e categoria delle forniture
#'
#' Una riga per decorrenza: dal giorno e ora di inizio a quelli di fine. Le
#' colonne arrivano in maiuscolo, le date come `AAAA-MM-GG HH:MM:SS`.
#'
#' @param percorso CSV dell'associazione dei componenti.
#' @return Data frame con le colonne che `associa_componenti()` si
#'   aspetta.
#' @noRd
importa_componenti <- function(percorso) {
  grezzo <- leggi_tabella_origine(percorso)
  istante <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")
  intero <- function(x) suppressWarnings(as.integer(numero_testo(x)))
  data.frame(
    id_fornitura = grezzo$ID_SERVIZIO,
    data_inizio_decorrenza = istante(grezzo$DATA_INIZIO_DECORRENZA),
    data_fine_decorrenza = istante(grezzo$DATA_FINE_DECORRENZA),
    nucleo_residenti = intero(grezzo$NUCLEO_RESIDENTI),
    nucleo_non_residenti = intero(grezzo$NUCLEO_NON_RESIDENTI),
    nucleo_totale = intero(grezzo$NUCLEO_TOTALE),
    tipo_categoria = grezzo$TIPO_CATEGORIA,
    categoria = grezzo$CATEGORIA,
    sottocategoria = grezzo$SOTTOCATEGORIA,
    stringsAsFactors = FALSE
  )
}

#' Manovre sociali delle forniture
#'
#' La piattaforma ETL dell'azienda le estrae già filtrate: solo le
#' dichiarazioni che danno il potenziamento del secco, una riga per periodo.
#' Si tengono la fornitura e le date della dichiarazione; una dichiarazione
#' senza data di fine vale ancora. Le dichiarazioni che finiscono prima di
#' cominciare le segnala `associa_potenziamento()`.
#'
#' @param percorso CSV delle manovre sociali.
#' @return Data frame con `id_fornitura`, `data_inizio_dichiarazione` e
#'   `data_fine_dichiarazione`.
#' @noRd
importa_manovre <- function(percorso) {
  grezzo <- leggi_tabella_origine(percorso)
  data.frame(
    id_fornitura = grezzo$id_servizio,
    data_inizio_dichiarazione = data_o_aperta(
      grezzo$data_inizio_dichiarazione,
      aperta = FALSE
    ),
    data_fine_dichiarazione = data_o_aperta(grezzo$data_fine_dichiarazione),
    stringsAsFactors = FALSE
  )
}

#' Letture storiche, prima delle antenne
#'
#' Solo codice e istante, con i codici nel formato della pipeline, senza i
#' sacchetti e senza righe ripetute.
#'
#' @param percorso CSV delle letture storiche.
#' @param colonna_rfid,colonna_data Nomi delle due colonne nel file.
#' @return Data frame con `RFID` e `giorno_lettura`, come testo.
#' @noRd
importa_letture_storiche <- function(
  percorso,
  colonna_rfid = "codice_transponder",
  colonna_data = "data_lettura"
) {
  grezzo <- leggi_tabella_origine(percorso, na = c("", "NA"))
  mancanti <- setdiff(c(colonna_rfid, colonna_data), names(grezzo))
  if (length(mancanti) > 0) {
    stop(
      "Nel file delle letture storiche mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      call. = FALSE
    )
  }
  storiche <- data.frame(
    RFID = grezzo[[colonna_rfid]],
    giorno_lettura = grezzo[[colonna_data]],
    stringsAsFactors = FALSE
  )
  storiche <- storiche[
    !is.na(storiche$RFID) & !is.na(storiche$giorno_lettura),
    ,
    drop = FALSE
  ]
  storiche <- formatta_rfid(storiche)
  storiche <- storiche[!startsWith(storiche$RFID, "00BD"), , drop = FALSE]
  storiche <- unique(storiche)
  rownames(storiche) <- NULL
  storiche
}
