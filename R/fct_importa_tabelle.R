# Importazione dei file estratti dal database aziendale (passo 1), uno per
# funzione: nomi delle colonne del progetto, tipi convertiti, comuni
# corretti. Le chiama `prepara_letture_antenne()`; l'app non le usa.

#' Percorsi dei file estratti dal database aziendale, con i nomi fissi di ogni estrazione
#'
#' Contesto: la preparazione dei dati (`prepara_letture_antenne()`,
#' `prepara_letture_saponette()`) legge una decina di CSV che il passo 1
#' (`data-raw/01_estrai_tabelle.R`) o altri sistemi salvano in
#' `data-raw/origine/`. I nomi sono fissi, così la pipeline sa dove cercarli.
#' Due file non vengono dall'estrazione: i test dei mezzi (scaricati da AppSheet
#' e salvati come `AppSheet.ViewData.csv`) e `giri_aggiunti.csv`, scritto a mano.
#'
#' Funzionamento: associa a ogni nome logico (`test_mezzi`, `letture`,
#' `contenitori`, `calendario`, `non_pianificati`, `giri_aggiunti`, `saponette`...)
#' il suo file dentro `cartella`. `letture` sono le letture grezze delle antenne,
#' `saponette` quelle dei lettori UHF manuali.
#'
#' Risultato: una lista con nome, in cui ogni elemento è un percorso (il file può
#' non esistere: spetta a chi legge controllarlo).
#' @param cartella Cartella dei file estratti.
#' @noRd
file_origine <- function(cartella = "data-raw/origine") {
  nomi <- c(
    test_mezzi = "AppSheet.ViewData.csv",
    letture = "letture_grezze.csv",
    contenitori = "dv_cnt_associazione_contenitori.csv",
    gruppi_sacchetti = "dv_cnt_gruppi_sacchetti.csv",
    servizi_sacchetti = "dv_cnt_gruppi_sacchetti_servizi.csv",
    calendario = "iv_estrazione_giri_servizi_rifiuti_filtered.csv",
    non_pianificati = "iv_estrazione_giri_servizi_non_pianificati_filtered.csv",
    giri_aggiunti = "giri_aggiunti.csv",
    componenti = "dv_associazione_componenti.csv",
    manovre = "dv_potenziamento_secco.csv",
    saponette = "dv_cnt_letture.csv"
  )
  as.list(stats::setNames(file.path(cartella, nomi), names(nomi)))
}

#' Legge un CSV estratto: tutto testo, separatore del file, spazi ai bordi tolti
#'
#' Contesto: i file estratti hanno codici che iniziano con zeri (forniture,
#' matricole, RFID) e coordinate con la virgola. Leggerli con i tipi automatici
#' li rovinerebbe: la pipeline li legge sempre come testo e converte solo dove
#' serve (`numero_testo()`, `data_o_aperta()`).
#'
#' Funzionamento: riconosce il separatore (`rileva_separatore()`: `,` o `;`),
#' legge con `data.table::fread()` tutte le colonne come testo, toglie gli
#' spazi ai bordi e legge come mancanti i testi in `na`.
#'
#' Risultato: un data frame di sole colonne di testo, con i nomi del file.
#' Errore se il file non esiste o se manca il pacchetto `data.table`.
#' @param percorso Percorso del CSV.
#' @param na Testi da leggere come mancanti.
#' @noRd
leggi_tabella_origine <- function(
  percorso,
  na = c("", "NA", "NaN", "null", "NULL")
) {
  if (!file.exists(percorso)) {
    stop("File non trovato: ", percorso, call. = FALSE)
  }
  # data.table serve alla sola pipeline: sta tra i Suggests.
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop(
      "Per leggere i file estratti serve il pacchetto data.table: install.packages(\"data.table\").",
      call. = FALSE
    )
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

#' Numero da un testo con la virgola o il punto decimale
#'
#' Contesto: i file estratti scrivono i decimali all'italiana (`45,4700`).
#' Risultato: un vettore numerico; un testo non numerico dà `NA` senza avvisi.
#' @noRd
numero_testo <- function(x) {
  suppressWarnings(as.numeric(stringr::str_replace(
    x,
    stringr::fixed(","),
    "."
  )))
}

#' Data di fine di un periodo: una data mancante vuol dire periodo aperto
#'
#' Contesto: cessazioni di contenitori e fine delle dichiarazioni sono vuote
#' finché il periodo è in corso. Per poterle confrontare con una data si
#' sostituisce il vuoto con `2099-12-31`, la data aperta usata anche nel dataset
#' di esempio.
#' Risultato: un vettore di date senza mancanti.
#' @noRd
data_o_aperta <- function(x) {
  dplyr::coalesce(lubridate::as_date(x), as.Date("2099-12-31"))
}

#' Test delle antenne dei mezzi, da AppSheet
#'
#' Contesto: un mezzo entra nella preparazione solo dopo che l'antenna ha
#' superato il test (collaudo): si leggono solo le sue letture dal giorno dopo la
#' data del test (`filtra_letture_dopo_il_collaudo()`). I test li compila
#' l'azienda su AppSheet e si salvano come `AppSheet.ViewData.csv`.
#'
#' Funzionamento: legge il file (il trattino vale come mancante), rinomina le
#' colonne con i nomi del progetto e porta la data e ora del verbale, scritta con
#' i punti al posto dei due punti, a una data.
#'
#' Risultato: una riga per test con `targa_veicolo`, `matricola_veicolo`,
#' `esito` (`POSITIVO` o altro) e `data_test`.
#' @param percorso Percorso del CSV di AppSheet.
#' @return `targa_veicolo`, `matricola_veicolo`, `esito`, `data_test`.
#' @noRd
importa_test_mezzi <- function(percorso) {
  leggi_tabella_origine(percorso, na = c("", "NA", "-")) |>
    dplyr::transmute(
      targa_veicolo = .data$TARGA_VEICOLO,
      matricola_veicolo = .data$MATRICOLA_VEICOLO,
      esito = .data$`ESITO TEST SEZIONE B`,
      data_test = .data$`ULTIMA DATA VERBALE SEZIONE B` |>
        stringr::str_replace_all(stringr::fixed("."), ":") |>
        lubridate::ymd_hm(quiet = TRUE) |>
        lubridate::as_date()
    )
}

#' Letture delle antenne, dal file grezzo al formato della pipeline
#'
#' Contesto: le antenne montate sui mezzi sono il metodo nuovo con cui l'azienda
#' legge i bidoni; ogni lettura ha istante, mezzo (targa), RFID, indirizzo e
#' posizione GPS. Questa è la prima tappa della preparazione: dal file grezzo
#' (`letture_grezze.csv` in `data-raw/origine/`) alle colonne del progetto.
#'
#' Funzionamento: ricava il comune di lettura dal testo dopo la prima virgola di
#' `IndirizzoLocalizzazione` («indirizzo, comune», in maiuscolo) e il cantiere
#' dalla tabella dei comuni, per nome esatto. Scarta come dati sporchi le letture
#' con latitudine zero o senza comune. Poi, per ogni RFID, tiene la prima lettura
#' valida del giorno (prima i filtri, poi la deduplica, così resta la prima
#' lettura buona). L'istante è nel fuso della sessione di R, come i turni del
#' calendario: devono restare nello stesso fuso.
#'
#' Risultato: `data_ora_lettura`, `data_lettura`, `targa_veicolo`, `RFID`,
#' `comune_lettura` (quello del GPS, non ancora corretto con la posizione),
#' `NumeroLetture`, `latitudine`, `longitudine`, `cantiere`.
#' @param percorso Percorso del CSV delle letture.
#' @param comuni Tabella dei comuni serviti con il loro cantiere.
#' @noRd
importa_letture_antenne <- function(percorso, comuni = comuni_cantieri()) {
  leggi_tabella_origine(percorso, na = c("", "NA")) |>
    dplyr::transmute(
      data_ora_lettura = as.POSIXct(
        .data$DataeOraPrimaLettura,
        format = "%d/%m/%Y %H.%M.%S"
      ),
      data_lettura = lubridate::as_date(lubridate::dmy_hms(
        .data$DataeOraPrimaLettura,
        quiet = TRUE
      )),
      targa_veicolo = .data$Targa,
      RFID = .data$`RFID(EPC)`,
      comune_lettura = stringr::str_match(
        .data$IndirizzoLocalizzazione,
        "^[^,]*,\\s*([^,]*)"
      )[, 2] |>
        stringr::str_trim() |>
        toupper(),
      NumeroLetture = .data$NumeroLetture,
      latitudine = numero_testo(.data$Latitudine),
      longitudine = numero_testo(.data$Longitudine)
    ) |>
    # prima via i dati sporchi, poi la prima lettura valida di un transponder
    # nello stesso giorno, anche se letto più volte
    dplyr::filter(.data$latitudine != 0, .data$comune_lettura != "") |>
    dplyr::arrange(.data$data_ora_lettura) |>
    dplyr::distinct(.data$data_lettura, .data$RFID, .keep_all = TRUE) |>
    dplyr::left_join(
      dplyr::select(comuni, "comune", "cantiere"),
      by = c("comune_lettura" = "comune")
    )
}

#' Anagrafica dei contenitori con un codice transponder, e loro forniture
#'
#' Contesto: il database lega ogni contenitore (RFID) a una fornitura e a
#' un'utenza; con una fornitura l'RFID è «censito». Questa anagrafica serve
#' ad `associa_contenitore_e_fornitura()` per dire quale servizio, volume,
#' fornitura e utenza aveva il bidone nel giorno della lettura.
#'
#' Funzionamento: tiene le righe con un codice transponder, converte volume e
#' raccolte in numeri e le date in date (una cessazione mancante vuol dire
#' ancora attivo, `data_o_aperta()`), e rinomina i campi del database
#' (`id_servizio` diventa `id_fornitura`, `comune_servizio` `comune_fornitura`).
#' Corregge due nomi di comune: `LUSINA CONCO` in `LUSIANA CONCO` e `VALSTAGNA`,
#' soppresso nel 2019 e oggi parte di `VALBRENTA`, in `VALBRENTA`.
#'
#' Risultato: una riga per contenitore e periodo di attività, con le colonne di
#' `colonne_contenitore()` più `codice_transponder`.
#' @param percorso Percorso del CSV dei contenitori.
#' @noRd
importa_contenitori <- function(percorso) {
  leggi_tabella_origine(percorso) |>
    dplyr::filter(!is.na(.data$codice_transponder)) |>
    dplyr::transmute(
      codice_transponder = .data$codice_transponder,
      servizio_transponder = .data$descrizione_rifiuto,
      volume = numero_testo(.data$volume),
      frequenza = .data$frequenza,
      numero_raccolte_annue = numero_testo(.data$numero_raccolte_annue),
      data_attivazione_contenitore = lubridate::as_date(
        .data$data_attivazione_contenitore
      ),
      data_cessazione_contenitore = data_o_aperta(
        .data$data_cessazione_contenitore
      ),
      id_fornitura = .data$id_servizio,
      stato_fornitura = .data$stato_servizio,
      id_utenza = .data$id_utenza,
      comune_fornitura = dplyr::case_when(
        .data$comune_servizio == "LUSINA CONCO" ~ "LUSIANA CONCO",
        .data$comune_servizio == "VALSTAGNA" ~ "VALBRENTA",
        .default = .data$comune_servizio
      )
    )
}

#' Sacchetti e fornitura del loro gruppo
#'
#' Contesto: un sacchetto (codice di 24 caratteri con prefisso `00BD`) non ha un
#' contenitore in anagrafica: ha un gruppo di sacchetti, e il gruppo ha una
#' fornitura. Serve ad `associa_contenitore_e_fornitura()` per dire se un
#' sacchetto è censito. I sacchetti poi vengono scartati dai dataset finali
#' (`filtra_solo_bidoni()`).
#'
#' Funzionamento: per ogni gruppo vale l'associazione con l'ultima variazione più
#' recente (a pari data la prima della tabella); poi si uniscono i sacchetti al
#' loro gruppo.
#'
#' Risultato: `codice_uhf` (il codice del sacchetto) e `id_fornitura`.
#' @param percorso_gruppi,percorso_servizi CSV dei gruppi di sacchetti e della
#'   loro fornitura.
#' @return `codice_uhf`, `id_fornitura`.
#' @noRd
importa_sacchetti <- function(percorso_gruppi, percorso_servizi) {
  fornitura_del_gruppo <- leggi_tabella_origine(percorso_servizi) |>
    dplyr::mutate(variazione = as.POSIXct(.data$ultima_variazione)) |>
    dplyr::arrange(
      .data$codice_gruppo_sacchetti,
      dplyr::desc(.data$variazione)
    ) |>
    dplyr::distinct(.data$codice_gruppo_sacchetti, .keep_all = TRUE) |>
    dplyr::select("codice_gruppo_sacchetti", id_fornitura = "id_servizio")

  leggi_tabella_origine(percorso_gruppi) |>
    dplyr::left_join(fornitura_del_gruppo, by = "codice_gruppo_sacchetti") |>
    dplyr::select("codice_uhf", "id_fornitura")
}

#' Calendario dei giri pianificati
#'
#' Contesto: il calendario dice che cosa era previsto (quale mezzo, quale giorno,
#' quale servizio in quale comune, e in che orario), non che cosa è successo: è
#' la base con cui `scegli_giro()` assegna un giro a ogni lettura. I giri
#' pianificati hanno gli orari; quelli non pianificati no
#' (`importa_giri_non_pianificati()`).
#'
#' Funzionamento: legge il file (tutto testo, gli orari restano `HH:MM` o
#' `HH:MM:SS`) e applica le regole comuni dei calendari (`pulisci_giri()`).
#'
#' Risultato: una riga per giro con `data_giro`, `ora_inizio`, `ora_fine`,
#' `matricola_mezzo`, `id_giro`, `eseguito`, `descrizione_servizio` e `comune_giro`.
#' @param percorso Percorso del CSV del calendario.
#' @noRd
importa_calendario <- function(percorso) {
  calendario <- leggi_tabella_origine(percorso)
  pulisci_giri(calendario)
}

#' Giri non pianificati: data, mezzo, servizio e comune, senza orari
#'
#' Contesto: alcuni giri non stanno nel calendario degli orari (per esempio
#' servizi a chiamata); il database li elenca in un file a parte, senza orari né
#' conferma. Una lettura di un mezzo che ha solo giri non pianificati si associa
#' con altri criteri (comune, servizio unico, tratto...): vedi `scegli_giro()`.
#'
#' Funzionamento: legge il file, accetta il nome vecchio `giorno` per
#' `data_giro`, verifica che ci siano le colonne necessarie e applica le regole
#' comuni (`pulisci_giri()`). `id_giro`, se c'è, è il codice del giro.
#'
#' Risultato: `data_giro`, `matricola_mezzo`, `descrizione_servizio`,
#' `comune_giro` e, se il file la ha, `id_giro`.
#' @param percorso Percorso del CSV dei giri non pianificati.
#' @noRd
importa_giri_non_pianificati <- function(percorso) {
  giri <- leggi_tabella_origine(percorso) |>
    dplyr::rename(dplyr::any_of(c(data_giro = "giorno")))
  mancanti <- setdiff(
    c("data_giro", "matricola_mezzo", "descrizione_servizio", "comune_giro"),
    names(giri)
  )
  if (length(mancanti) > 0) {
    stop(
      "Nel file dei giri non pianificati mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      call. = FALSE
    )
  }
  pulisci_giri(giri)
}

#' Giri aggiunti a mano: quelli compiuti ma mancanti dal calendario estratto
#'
#' Contesto: il proprietario sa dai consuntivi che un giro è stato compiuto, ma
#' la query del calendario non lo ha. Il file `data-raw/origine/giri_aggiunti.csv`
#' li conserva: l'estrazione non lo scrive, quindi sopravvive a ogni
#' aggiornamento del calendario, e la preparazione li rimette tra i giri
#' pianificati (`aggiungi_giri_a_mano()`).
#'
#' Funzionamento: il file ha tutte le colonne del calendario pianificato
#' (`colonne_giri_aggiunti()`). Servono sempre `data_giro` (`AAAA-MM-GG` o
#' `GG/MM/AAAA`), `ora_inizio` e `ora_fine` (`HH:MM` o `HH:MM:SS`, l'inizio prima
#' della fine), `matricola_mezzo`, `descrizione_servizio` (il nome del calendario,
#' per esempio `SECCO PAP`, che la tabella di conversione deve conoscere) e
#' `comune_giro`; `id_giro` ed `eseguito` possono restare vuoti. Servizio, comune
#' ed `eseguito` si scrivono in maiuscolo. Righe vuote ignorate; righe a metà,
#' date, orari e servizi sconosciuti fermano la preparazione con un messaggio che
#' dice la riga del file.
#'
#' Risultato: le colonne di `colonne_giri_aggiunti()`, o `NULL` se il file manca
#' o non ha righe.
#' @param percorso Percorso del CSV.
#' @return Le colonne di `colonne_giri_aggiunti()`, o `NULL`.
#' @noRd
importa_giri_aggiunti <- function(percorso) {
  if (!file.exists(percorso %||% "")) {
    return(NULL)
  }
  colonne <- colonne_giri_aggiunti()
  obbligatorie <- setdiff(colonne, c("id_giro", "eseguito"))
  giri <- leggi_tabella_origine(percorso)
  names(giri) <- tolower(trimws(names(giri)))
  mancanti <- setdiff(colonne, names(giri))
  if (length(mancanti) > 0) {
    stop(
      "Nel file dei giri aggiunti mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  giri <- giri |>
    dplyr::select(dplyr::all_of(colonne)) |>
    dplyr::mutate(riga = dplyr::row_number() + 1L) |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(colonne), \(x) !is.na(x)))
  if (nrow(giri) == 0) {
    return(NULL)
  }

  incompleti <- giri$riga[!stats::complete.cases(giri[obbligatorie])]
  if (length(incompleti) > 0) {
    stop(
      "Righe incomplete nel file dei giri aggiunti (riga del file): ",
      paste(incompleti, collapse = ", "),
      ". Servono ",
      paste(obbligatorie, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  giri <- dplyr::mutate(
    giri,
    data_giro = lubridate::as_date(
      lubridate::parse_date_time(.data$data_giro, c("Ymd", "dmY"), quiet = TRUE)
    ),
    inizio = secondi_del_giorno(.data$ora_inizio),
    fine = secondi_del_giorno(.data$ora_fine),
    descrizione_servizio = stringr::str_to_upper(
      stringr::str_trim(.data$descrizione_servizio)
    ),
    comune_giro = stringr::str_to_upper(stringr::str_trim(.data$comune_giro)),
    eseguito = stringr::str_to_upper(stringr::str_trim(.data$eseguito))
  )
  if (anyNA(giri$data_giro)) {
    stop(
      "Date non valide nel file dei giri aggiunti (usare AAAA-MM-GG o GG/MM/AAAA), righe: ",
      paste(giri$riga[is.na(giri$data_giro)], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  orari_errati <- is.na(giri$inizio) | is.na(giri$fine) | giri$inizio >= giri$fine
  if (any(orari_errati)) {
    stop(
      "Orari non validi nel file dei giri aggiunti (HH:MM o HH:MM:SS, l'inizio prima della fine, niente turni oltre la mezzanotte), righe: ",
      paste(giri$riga[orari_errati], collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  ignoti <- unique(giri$descrizione_servizio[
    !giri$descrizione_servizio %in% conversione_servizi()$servizio_calendario
  ])
  if (length(ignoti) > 0) {
    stop(
      "Servizi del file dei giri aggiunti assenti dalla tabella di conversione: ",
      paste(ignoti, collapse = ", "),
      ". Correggi il nome o aggiungilo a `tipologie_servizio()`.",
      call. = FALSE
    )
  }
  giri |>
    dplyr::select(dplyr::all_of(colonne)) |>
    pulisci_giri()
}

#' Colonne del file dei giri aggiunti a mano: quelle del calendario pianificato
#'
#' Contesto: il proprietario ricava tutte le informazioni di un giro dal
#' consuntivo, quindi il file ha tutti i campi di un giro pianificato.
#' Risultato: i nomi, nell'ordine del calendario.
#' @noRd
colonne_giri_aggiunti <- function() {
  c(
    "data_giro",
    "ora_inizio",
    "ora_fine",
    "matricola_mezzo",
    "id_giro",
    "eseguito",
    "descrizione_servizio",
    "comune_giro"
  )
}

#' Regole comuni ai calendari dei giri
#'
#' Contesto: calendario pianificato, giri non pianificati e giri aggiunti a mano
#' arrivano con gli stessi difetti: giri senza mezzo, matricole con la lettera
#' iniziale del sistema di origine, date come testo e un comune scritto male.
#'
#' Funzionamento: toglie i giri senza mezzo, toglie la lettera iniziale dalla
#' matricola (`X1601` diventa `1601`), porta `data_giro` (in un file vecchio
#' `giorno`) a una data e corregge `ROSA'` in `ROSÀ`, come nella tabella dei
#' comuni.
#'
#' Risultato: lo stesso data frame, con questi campi puliti.
#' @param giri Un calendario con almeno `data_giro` (o `giorno`),
#'   `matricola_mezzo` e `comune_giro`.
#' @noRd
pulisci_giri <- function(giri) {
  giri |>
    dplyr::rename(dplyr::any_of(c(data_giro = "giorno"))) |>
    dplyr::filter(!is.na(.data$matricola_mezzo)) |>
    dplyr::mutate(
      matricola_mezzo = stringr::str_remove(.data$matricola_mezzo, "^[A-Za-z]"),
      data_giro = lubridate::as_date(.data$data_giro),
      comune_giro = dplyr::if_else(
        .data$comune_giro %in% "ROSA'",
        "ROS\u00c0",
        .data$comune_giro
      )
    )
}

#' Nucleo e categoria delle forniture, una riga per decorrenza
#'
#' Contesto: il limite annuo di litri del secco dipende dal nucleo familiare (e,
#' per alcune forniture, dal potenziamento); il report sui conferimenti e il
#' filtro per categoria dei report di confronto usano questi dati. Il database
#' li tiene a intervalli: ogni dichiarazione vale da un giorno a un altro.
#'
#' Funzionamento: legge le colonne in maiuscolo, converte le date
#' (`AAAA-MM-GG HH:MM:SS`) e i numeri; una fine mancante vuol dire ancora in
#' vigore. `associa_componenti()` sceglie poi la riga in vigore nel giorno della
#' lettura.
#'
#' Risultato: una riga per decorrenza con `id_fornitura`, le due date, i tre
#' numeri del nucleo, `tipo_categoria`, `categoria` e `sottocategoria`.
#' @param percorso Percorso del CSV dei componenti.
#' @noRd
importa_componenti <- function(percorso) {
  data_ora <- function(x) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S")
  intero <- function(x) suppressWarnings(as.integer(numero_testo(x)))

  leggi_tabella_origine(percorso) |>
    dplyr::transmute(
      id_fornitura = .data$ID_SERVIZIO,
      data_inizio_decorrenza = data_ora(.data$DATA_INIZIO_DECORRENZA),
      data_fine_decorrenza = data_ora(.data$DATA_FINE_DECORRENZA),
      nucleo_residenti = intero(.data$NUCLEO_RESIDENTI),
      nucleo_non_residenti = intero(.data$NUCLEO_NON_RESIDENTI),
      nucleo_totale = intero(.data$NUCLEO_TOTALE),
      tipo_categoria = .data$TIPO_CATEGORIA,
      categoria = .data$CATEGORIA,
      sottocategoria = .data$SOTTOCATEGORIA
    )
}

#' Manovre sociali del potenziamento del secco, già filtrate dall'ETL aziendale
#'
#' Contesto: una fornitura con una manovra sociale (neonati, anziani o disabili)
#' ha un limite annuo di litri più alto, il «potenziamento». L'estrazione
#' aziendale contiene già solo le dichiarazioni del potenziamento del secco.
#' `associa_potenziamento()` dice se in un giorno ce n'è una in vigore.
#'
#' Funzionamento: legge una riga per dichiarazione; una fine mancante vale
#' ancora attiva.
#' Risultato: `id_fornitura`, `data_inizio_dichiarazione`, `data_fine_dichiarazione`.
#' @param percorso Percorso del CSV delle manovre.
#' @noRd
importa_manovre <- function(percorso) {
  leggi_tabella_origine(percorso) |>
    dplyr::transmute(
      id_fornitura = .data$id_servizio,
      data_inizio_dichiarazione = lubridate::as_date(
        .data$data_inizio_dichiarazione
      ),
      data_fine_dichiarazione = data_o_aperta(.data$data_fine_dichiarazione)
    )
}

#' Data e ora da un testo `AAAA-MM-GG` o `GG/MM/AAAA`, con o senza ora
#'
#' Contesto: i file delle saponette non hanno un formato unico per l'istante.
#' Funzionamento: prova nell'ordine le forme con secondi, con minuti e senza
#' ora, nel fuso della sessione di R (lo stesso dei turni del calendario).
#' Risultato: un vettore di istanti; `NA` dove il testo non è leggibile.
#' @noRd
data_ora_da_testo <- function(x) {
  lubridate::parse_date_time(
    x,
    orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY"),
    tz = "",
    quiet = TRUE
  )
}

#' Letture delle saponette, i lettori UHF manuali usati prima delle antenne
#'
#' Contesto: le saponette sono il metodo di lettura che l'azienda usava finora:
#' un lettore UHF che l'operatore passa vicino a ogni bidone. Gli operatori sono
#' ancora obbligati a usarle accanto alle antenne. Il file ha tre campi:
#' l'istante, il codice transponder (l'RFID) e `id_trasmissione`, che raggruppa i
#' codici letti nello stesso giro in un giorno (una trasmissione è un giro delle
#' saponette) ma non corrisponde all'`id_giro` del calendario. Per questo non
#' hanno un servizio atteso dal giro e non hanno il mezzo.
#'
#' Funzionamento: rinomina le tre colonne (i nomi nel file si passano come
#' parametri), legge l'istante con `data_ora_da_testo()`, porta i codici al
#' formato delle antenne (`formatta_rfid()`) e toglie le righe ripetute. Le righe
#' con istante illeggibile si scartano, con un avviso che ne mostra qualcuna.
#'
#' Risultato: `data_ora_lettura`, `data_lettura`, `id_trasmissione`, `RFID`.
#' @param percorso Percorso del CSV delle letture delle saponette.
#' @param colonna_rfid,colonna_data_ora,colonna_trasmissione Nomi delle colonne
#'   nel file.
#' @return `data_ora_lettura`, `data_lettura`, `id_trasmissione`, `RFID`.
#' @noRd
importa_letture_saponette <- function(
  percorso,
  colonna_rfid = "codice_transponder",
  colonna_data_ora = "data_lettura",
  colonna_trasmissione = "id_trasmissione"
) {
  grezzo <- leggi_tabella_origine(percorso, na = c("", "NA"))
  mancanti <- setdiff(
    c(colonna_rfid, colonna_data_ora, colonna_trasmissione),
    names(grezzo)
  )
  if (length(mancanti) > 0) {
    stop(
      "Nel file delle letture delle saponette mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      call. = FALSE
    )
  }

  saponette <- grezzo |>
    dplyr::transmute(
      testo_data_ora = .data[[colonna_data_ora]],
      data_ora_lettura = data_ora_da_testo(.data$testo_data_ora),
      data_lettura = lubridate::as_date(.data$data_ora_lettura),
      id_trasmissione = .data[[colonna_trasmissione]],
      RFID = .data[[colonna_rfid]]
    )

  illeggibili <- dplyr::filter(
    saponette,
    !is.na(.data$testo_data_ora),
    is.na(.data$data_ora_lettura)
  )
  if (nrow(illeggibili) > 0) {
    warning(
      nrow(illeggibili),
      " letture delle saponette hanno un istante illeggibile e vengono scartate, ad esempio: ",
      paste(
        utils::head(unique(illeggibili$testo_data_ora), 3),
        collapse = ", "
      ),
      call. = FALSE
    )
  }

  saponette |>
    dplyr::select(-"testo_data_ora") |>
    dplyr::filter(!is.na(.data$RFID), !is.na(.data$data_ora_lettura)) |>
    formatta_rfid() |>
    dplyr::distinct()
}
