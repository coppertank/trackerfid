# Preparazione delle letture reali per l'app: dalle tabelle estratte al CSV
# che l'app carica. L'app non le usa; le chiama la pipeline di {targets},
# nell'ordine del file. Scritte per essere lette, non per la velocità.

#' Tiene le letture dei mezzi con il test delle antenne positivo, dal giorno dopo il collaudo
#'
#' Contesto: le antenne vengono installate e collaudate un mezzo alla volta
#' (il test lo compila l'azienda su AppSheet). Le letture di un mezzo il cui test
#' non è positivo, o fatte prima del collaudo, non sono affidabili e non entrano
#' nel dataset.
#'
#' Funzionamento: unisce le letture ai test positivi per targa e tiene quelle con
#' `data_lettura` successiva alla data del test. Il giorno del collaudo non conta.
#'
#' Risultato: le letture con le sole colonne che servono alle fasi seguenti
#' (istante, targa, matricola, RFID, comune, cantiere, posizione); i mezzi senza
#' test positivo spariscono.
#' @param df_letture Letture di `importa_letture_antenne()`.
#' @param df_test_mezzi Test di `importa_test_mezzi()`.
#' @noRd
filtra_letture_dopo_il_collaudo <- function(df_letture, df_test_mezzi) {
  test_positivi <- dplyr::filter(df_test_mezzi, .data$esito == "POSITIVO")

  df_letture |>
    dplyr::inner_join(test_positivi, by = "targa_veicolo") |>
    dplyr::filter(.data$data_lettura > .data$data_test) |>
    dplyr::select(
      "data_ora_lettura",
      "data_lettura",
      "targa_veicolo",
      "matricola_veicolo",
      "RFID",
      "comune_lettura",
      "cantiere",
      "NumeroLetture",
      "latitudine",
      "longitudine"
    )
}

#' Comune di lettura: quello del GPS se coincide con la fornitura, altrimenti quello della posizione
#'
#' Contesto: il sistema di localizzazione dei mezzi scrive il comune di una
#' lettura, ma vicino ai confini o per una frazione sbaglia. Il proprietario ha
#' verificato che il comune ricavato dalla posizione (dentro i confini dei comuni
#' serviti) coincide con quello della fornitura molto più spesso del comune del
#' GPS. Il comune di lettura decide cantiere, filtri per comune, bolle sulla
#' mappa e coerenza del comune. Spiegato nel dettaglio in `docs/comune_lettura.qmd`.
#'
#' Funzionamento (va applicata dopo `associa_contenitore_e_fornitura()`):
#' 1. porta il comune del GPS alla grafia della tabella dei comuni;
#' 2. se coincide con il comune della fornitura (`comune_fornitura`) la lettura lo
#'    tiene: vicino al confine la posizione può dare il comune accanto;
#' 3. altrimenti prende il comune servito che contiene il punto
#'    (`comune_del_punto()`);
#' 4. fuori da tutti i comuni serviti, o senza coordinate, resta il comune del GPS.
#' Il cantiere segue il comune finale. Un messaggio conta le letture il cui
#' comune cambia; l'elenco sta nell'attributo `comuni_corretti`
#' (vedi `comuni_corretti()`).
#'
#' Risultato: le letture con `comune_lettura` e `cantiere` aggiornati e
#' l'attributo `comuni_corretti`. Limite noto: per un censito la regola può
#' nascondere un bidone spostato in un comune confinante.
#' @param letture Letture con `comune_lettura`, `cantiere`, `latitudine`,
#'   `longitudine` e, se ci sono, `comune_fornitura`.
#' @noRd
correggi_comune_lettura <- function(letture) {
  tabella <- comuni_cantieri()
  fornitura <- letture[["comune_fornitura"]] %||%
    rep(NA_character_, nrow(letture))

  letture <- letture |>
    dplyr::mutate(
      riga_tabella = indice_comune(.data$comune_lettura),
      comune_gps = dplyr::if_else(
        is.na(.data$riga_tabella),
        .data$comune_lettura,
        tabella$comune[.data$riga_tabella]
      ),
      comune_fornitura_chiave = chiave_comune_o_na(fornitura),
      gps_come_fornitura = (chiave_comune_o_na(.data$comune_gps) ==
        .data$comune_fornitura_chiave) %in% TRUE,
      dalla_posizione = comune_del_punto(.data$latitudine, .data$longitudine),
      comune_finale = dplyr::if_else(
        .data$gps_come_fornitura,
        .data$comune_gps,
        dplyr::coalesce(.data$dalla_posizione, .data$comune_gps)
      ),
      cambiato = !is.na(.data$comune_finale) &
        (is.na(.data$comune_gps) | .data$comune_gps != .data$comune_finale)
    )

  cambiate <- dplyr::filter(letture, .data$cambiato)
  if (nrow(cambiate) > 0) {
    voci <- cambiate |>
      dplyr::count(
        da = dplyr::coalesce(.data$comune_gps, "(vuoto)"),
        a = .data$comune_finale,
        sort = TRUE
      ) |>
      dplyr::mutate(
        voce = sprintf("%s \u2192 %s (%d)", .data$da, .data$a, .data$n)
      ) |>
      dplyr::pull("voce")
    message(sprintf(
      "Comune di lettura preso dalla posizione per %d letture, diverso da quello del sistema di localizzazione: %s%s",
      nrow(cambiate),
      paste(utils::head(voci, 10), collapse = ", "),
      if (length(voci) > 10) ", \u2026" else ""
    ))
  }

  letture |>
    dplyr::mutate(
      comune_lettura = .data$comune_finale,
      cantiere = dplyr::coalesce(
        cantiere_di_comune(.data$comune_lettura),
        .data$cantiere
      )
    ) |>
    dplyr::select(
      -"riga_tabella",
      -"comune_gps",
      -"comune_fornitura_chiave",
      -"gps_come_fornitura",
      -"dalla_posizione",
      -"comune_finale",
      -"cambiato"
    ) |>
    structure(
      comuni_corretti = cambiate |>
        dplyr::select(
          dplyr::any_of(c(
            "data_ora_lettura", "matricola_veicolo", "targa_veicolo", "RFID"
          )),
          "latitudine", "longitudine", "comune_gps",
          dplyr::any_of("comune_fornitura"),
          comune_posizione = "dalla_posizione"
        )
    )
}

#' Letture il cui comune del GPS è stato sostituito da quello della posizione
#'
#' Contesto: serve a controllare quanto e come la correzione del comune cambia i
#' dati; la pipeline lo scrive in `comuni_corretti.csv`.
#' Funzionamento: legge l'attributo `comuni_corretti` lasciato da
#' `correggi_comune_lettura()` (o da `prepara_letture_antenne()`) e porta i
#' codici RFID al formato della pipeline.
#' Risultato: istante, mezzo, RFID, coordinate, `comune_gps`, `comune_fornitura`
#' e `comune_posizione`; una tabella vuota se l'attributo manca.
#' @noRd
comuni_corretti <- function(letture) {
  elenco <- attr(letture, "comuni_corretti") %||% dplyr::tibble(
    data_ora_lettura = as.POSIXct(character(0)),
    matricola_veicolo = character(0),
    targa_veicolo = character(0),
    RFID = character(0),
    latitudine = numeric(0),
    longitudine = numeric(0),
    comune_gps = character(0),
    comune_fornitura = character(0),
    comune_posizione = character(0)
  )
  if ("RFID" %in% names(elenco)) formatta_rfid(elenco) else elenco
}

#' Porta i codici RFID al formato comune, 10 o 24 caratteri in maiuscolo
#'
#' Contesto: antenne e saponette scrivono lo stesso codice in modi diversi (zeri
#' iniziali presenti o no, minuscole) e il lettore sbaglia a volte un carattere.
#' Per confrontare i due sistemi serve un formato unico: 10 caratteri per i
#' bidoni, 24 per i sacchetti (prefisso `00BD`).
#'
#' Funzionamento: mette in maiuscolo; corregge un carattere letto male tra i
#' primi 14, che dovrebbero essere zeri (bit flip); toglie gli zeri iniziali e
#' riempie a sinistra fino a 10 caratteri, o fino a 24 se ne restano più di 10.
#'
#' Risultato: le letture con `RFID` nel formato comune.
#' @param df Letture con la colonna `RFID`.
#' @noRd
formatta_rfid <- function(df) {
  dplyr::mutate(
    df,
    RFID = toupper(as.character(.data$RFID)),
    # 13 zeri su 14: il quattordicesimo carattere è letto male.
    RFID = dplyr::if_else(
      stringr::str_length(.data$RFID) >= 14 &
        stringr::str_count(stringr::str_sub(.data$RFID, 1, 14), "0") == 13,
      paste0("00000000000000", stringr::str_sub(.data$RFID, 15)),
      .data$RFID
    ),
    RFID = stringr::str_remove(.data$RFID, "^0+"),
    RFID = dplyr::if_else(
      stringr::str_length(.data$RFID) <= 10,
      stringr::str_pad(.data$RFID, width = 10, side = "left", pad = "0"),
      stringr::str_pad(.data$RFID, width = 24, side = "left", pad = "0")
    )
  )
}

#' Una lettura al giorno per RFID: la prima
#'
#' Contesto: un bidone può essere letto più volte nello stesso giorno (da mezzi
#' diversi o dallo stesso mezzo in momenti diversi). Per l'analisi basta sapere
#' che è stato letto, e la prima lettura del giorno è quella che conta.
#'
#' Funzionamento: tiene la prima lettura di ogni coppia giorno-RFID.
#' Va applicata dopo `formatta_rfid()`, perché due codici scritti diversi
#' possono diventare lo stesso RFID. Presuppone le letture in ordine di tempo.
#'
#' Risultato: le stesse colonne, con al più una riga per RFID e giorno.
#' @noRd
una_lettura_al_giorno <- function(letture) {
  dplyr::distinct(letture, .data$data_lettura, .data$RFID, .keep_all = TRUE)
}

#' Associa a ogni lettura il contenitore attivo nel suo giorno e la sua fornitura
#'
#' Contesto: la lettura di un'antenna ha solo il codice RFID. Dall'anagrafica
#' dei contenitori si sa quale servizio (secco, carta...), volume, fornitura e
#' utenza ha quel bidone: se ha una fornitura il bidone è «censito»
#' (`presente_a_database` uguale a `Presente`), altrimenti è non censito. I
#' sacchetti non hanno un contenitore: hanno solo la fornitura del loro gruppo.
#'
#' Funzionamento: per ogni lettura sceglie il contenitore con quel codice attivo
#' nel giorno della lettura (tra attivazione e cessazione); se non c'è cerca il
#' codice tra i sacchetti. I campi del contenitore diventano le colonne
#' `servizio_transponder`, `volume`, `frequenza`, `numero_raccolte_annue`,
#' `id_fornitura`, `stato_fornitura`, `id_utenza`, `comune_fornitura`.
#'
#' Risultato: le letture con queste colonne e `presente_a_database`.
#' @param df_letture Letture con `RFID` e `data_lettura`.
#' @param df_associazione_contenitori Tabella di `importa_contenitori()`.
#' @param df_associazione_sacchetti Tabella di `importa_sacchetti()`.
#' @noRd
associa_contenitore_e_fornitura <- function(
  df_letture,
  df_associazione_contenitori,
  df_associazione_sacchetti
) {
  sacchetti <- dplyr::select(
    df_associazione_sacchetti,
    "codice_uhf",
    fornitura_sacchetto = "id_fornitura"
  )

  df_letture |>
    dplyr::left_join(
      df_associazione_contenitori,
      by = dplyr::join_by(
        "RFID" == "codice_transponder",
        "data_lettura" >= "data_attivazione_contenitore",
        "data_lettura" <= "data_cessazione_contenitore"
      )
    ) |>
    dplyr::left_join(sacchetti, by = c("RFID" = "codice_uhf")) |>
    dplyr::mutate(
      # Le due tabelle possono scrivere il codice come numero o come testo.
      id_fornitura = dplyr::coalesce(
        as.character(.data$id_fornitura),
        as.character(.data$fornitura_sacchetto)
      ),
      presente_a_database = dplyr::if_else(
        is.na(.data$id_fornitura),
        "Non Presente",
        "Presente"
      )
    ) |>
    dplyr::select(-"fornitura_sacchetto")
}

#' Aggiunge ai giri pianificati quelli inseriti a mano e mancanti dal calendario
#'
#' Contesto: la query del calendario può non avere un giro che il proprietario sa
#' compiuto (da un consuntivo). Quei giri stanno in `giri_aggiunti.csv`, un file
#' che l'estrazione non tocca: a ogni aggiornamento del calendario vanno rimessi,
#' e questa funzione lo fa a ogni preparazione.
#'
#' Funzionamento: confronta i giri aggiunti con i pianificati per giorno, mezzo
#' (senza zeri iniziali), servizio e comune; quelli già presenti non si
#' ripetono. Gli altri si accodano ai pianificati, con i loro orari, `id_giro` ed
#' `eseguito`. Un messaggio dice quanti giri sono nuovi e quanti già presenti (che
#' si possono togliere dal file).
#'
#' Risultato: i giri pianificati, con quelli aggiunti; invariati se non ce ne
#' sono.
#' @param pianificati Calendario di `importa_calendario()`.
#' @param aggiunti Giri di `importa_giri_aggiunti()`, o `NULL`.
#' @return I giri pianificati, con quelli aggiunti.
#' @noRd
aggiungi_giri_a_mano <- function(pianificati, aggiunti) {
  if (is.null(aggiunti) || nrow(aggiunti) == 0) {
    return(pianificati)
  }
  chiave <- function(giri) {
    paste(
      as.character(giri$data_giro),
      chiave_matricola(giri$matricola_mezzo),
      stringr::str_to_upper(stringr::str_trim(giri$descrizione_servizio)),
      chiave_comune_o_na(giri$comune_giro),
      sep = "|"
    )
  }
  gia_presenti <- chiave(aggiunti) %in% chiave(pianificati)
  message(sprintf(
    "Giri aggiunti a mano: %d nuovi, %d gi\u00e0 nel calendario (si possono togliere dal file).",
    sum(!gia_presenti),
    sum(gia_presenti)
  ))
  dplyr::bind_rows(
    dplyr::mutate(pianificati, id_giro = as.character(.data$id_giro)),
    dplyr::mutate(aggiunti[!gia_presenti, ], id_giro = as.character(.data$id_giro))
  )
}

#' Un calendario solo, con i giri pianificati e i non pianificati
#'
#' Contesto: `scegli_giro()` lavora su un calendario unico. I giri pianificati
#' hanno gli orari, i non pianificati no; la colonna `pianificato` li distingue.
#'
#' Funzionamento: tiene le colonne del calendario pianificato (errore se
#' mancano), accoda i non pianificati senza orari né conferma e toglie le righe
#' ripetute. L'`id_giro` di un non pianificato è quello del suo file (servizio e
#' comune), se c'è: può coincidere con quello di un pianificato di un altro
#' giorno.
#'
#' Risultato: `data_giro`, `matricola_mezzo`, `id_giro`, `eseguito`,
#' `descrizione_servizio`, `comune_giro`, `ora_inizio`, `ora_fine`, `pianificato`.
#' @param pianificati Calendario di `importa_calendario()`.
#' @param non_pianificati Giri di `importa_giri_non_pianificati()`, o `NULL`.
#' @return Il calendario, con la colonna `pianificato`.
#' @noRd
unisci_calendari <- function(pianificati, non_pianificati = NULL) {
  colonne <- c(
    "data_giro",
    "matricola_mezzo",
    "id_giro",
    "eseguito",
    "descrizione_servizio",
    "comune_giro",
    "ora_inizio",
    "ora_fine"
  )
  mancanti <- setdiff(colonne, names(pianificati))
  if (length(mancanti) > 0) {
    stop(
      "Nel calendario dei giri mancano le colonne: ",
      paste(mancanti, collapse = ", "),
      call. = FALSE
    )
  }

  pianificati <- pianificati |>
    dplyr::select(dplyr::all_of(colonne)) |>
    dplyr::mutate(id_giro = as.character(.data$id_giro), pianificato = TRUE)
  if (is.null(non_pianificati) || nrow(non_pianificati) == 0) {
    return(dplyr::distinct(pianificati))
  }

  codici <- non_pianificati[["id_giro"]] %||%
    rep(NA_character_, nrow(non_pianificati))
  non_pianificati <- non_pianificati |>
    dplyr::transmute(
      data_giro = .data$data_giro,
      matricola_mezzo = as.character(.data$matricola_mezzo),
      id_giro = as.character(codici),
      descrizione_servizio = .data$descrizione_servizio,
      comune_giro = .data$comune_giro
    ) |>
    dplyr::mutate(
      eseguito = NA_character_,
      ora_inizio = NA_character_,
      ora_fine = NA_character_,
      pianificato = FALSE
    )

  dplyr::bind_rows(pianificati, non_pianificati) |>
    dplyr::distinct()
}

#' Secondi dalla mezzanotte di un orario `HH:MM` o `HH:MM:SS`
#'
#' Contesto: il calendario scrive gli orari come testo, in due forme; per
#' confrontare l'orario dei giri con quello delle letture serve un numero.
#' Risultato: i secondi dalla mezzanotte; `NA` se il testo non è leggibile.
#' @noRd
secondi_del_giorno <- function(ora) {
  stringr::str_split(as.character(ora), stringr::fixed(":")) |>
    purrr::map_dbl(function(parti) {
      numeri <- suppressWarnings(as.numeric(parti))
      if (!length(numeri) %in% 2:3 || anyNA(numeri)) {
        return(NA_real_)
      }
      sum(numeri * c(3600, 60, 1)[seq_along(numeri)])
    })
}

#' Secondi dalla mezzanotte di un istante
#'
#' Contesto: gemella di `secondi_del_giorno()` per gli istanti delle letture.
#' Risultato: ore, minuti e secondi dell'istante, in secondi.
#' @noRd
secondi_dalla_mezzanotte <- function(istante) {
  lubridate::hour(istante) *
    3600 +
    lubridate::minute(istante) * 60 +
    floor(lubridate::second(istante))
}

#' Matricola senza spazi, zeri iniziali e maiuscole, per confrontare "0933" con "933"
#'
#' Contesto: la stessa matricola arriva scritta in modi diversi dai file del
#' calendario, dei test e delle letture. Il confronto esatto farebbe perdere il
#' giro ai mezzi con lo zero iniziale.
#' Risultato: la chiave di confronto; non si usa per mostrare la matricola.
#' @noRd
chiave_matricola <- function(x) {
  toupper(sub("^0+", "", trimws(as.character(x))))
}

#' Chiave di confronto di un comune, `NA` se vuota (vedi `chiave_comune()`)
#'
#' Contesto: i comuni si confrontano sempre senza badare a maiuscole, accenti e
#' apostrofi. Un nome vuoto non deve risultare uguale a un altro nome vuoto.
#' Risultato: la chiave, o `NA`.
#' @noRd
chiave_comune_o_na <- function(nomi) {
  dplyr::na_if(chiave_comune(nomi), "")
}

#' Passi con cui una lettura riceve il suo giro, nell'ordine
#'
#' Contesto: l'associazione lettura-giro è una serie di regole, dalla più sicura
#' alla meno sicura (vedi `scegli_giro()` e `docs/associazione_giri.qmd`).
#' Funzionamento: ogni passo ha un nome in codice (`orario_comune`,
#' `comune_non_pianificato`, `orario`, `servizio_unico`, `tratto`,
#' `comune_vicino`, `vicino_orario`) e un testo, più `censiti` per i mezzi senza
#' giri. Il testo compare nel messaggio della pipeline che conta le letture di
#' ogni passo.
#' Risultato: un vettore con nome (codice) e valore (testo).
#' @noRd
passi_associazione <- function() {
  c(
    orario_comune = "giro pianificato, nell'orario e nel comune della lettura",
    comune_non_pianificato = "giro non pianificato nel comune della lettura",
    orario = "giro pianificato nel cui orario cade la lettura, in un altro comune",
    servizio_unico = "unico servizio non pianificato del mezzo nel giorno",
    tratto = "giro delle altre letture di seguito dello stesso mezzo",
    comune_vicino = "giro non pianificato del comune pi\u00f9 vicino",
    vicino_orario = "giro pianificato entro la tolleranza dall'orario",
    censiti = "servizio dei censiti letti dal mezzo, senza giri nel giorno"
  )
}

#' Minuti di ritardo ammessi dopo la fine di un giro pianificato
#'
#' Contesto: gli operatori finiscono spesso un giro dopo l'orario previsto.
#' Una lettura entro questo tempo dalla fine fa ancora parte del giro e, a
#' catena, diventa la nuova fine. È anche la pausa che separa due tratti.
#' @noRd
ritardo_giro_minuti <- function() 30

#' Minuti entro cui una lettura fuori orario prende ancora il giro pianificato più vicino
#'
#' Contesto: ultimo passo dell'associazione (`vicino_orario`): una lettura che
#' nessun altro criterio assegna prende il pianificato del mezzo più vicino nel
#' tempo, se dista meno di questa soglia dall'inizio o dalla fine (allungata).
#' @noRd
tolleranza_giro_minuti <- function() 60

#' Metri entro cui il comune più vicino decide il giro non pianificato
#'
#' Contesto: passo `comune_vicino` dell'associazione: una lettura in un comune
#' senza giri del mezzo prende il non pianificato del comune più vicino, se
#' dista meno di questa soglia; oltre, resta senza giro.
#' @noRd
soglia_comune_vicino_m <- function() 2000

#' Divide le letture di ogni mezzo in ogni giorno in tratti continui
#'
#' Contesto: un mezzo che lavora lascia letture ravvicinate; una pausa lunga
#' separa un periodo di lavoro dall'altro. Il tratto approssima «un giro» quando
#' il giro vero non si conosce: serve all'associazione delle letture orfane
#' (passo `tratto` di `scegli_giro()`), alla fine allungata dei giri
#' (`allunga_fine()`) e al servizio dei mezzi senza giri
#' (`servizio_dei_censiti_del_mezzo()`).
#'
#' Funzionamento: ordina per mezzo, giorno e ora; un tratto nuovo comincia dopo
#' una pausa più lunga di `ritardo_giro_minuti()`.
#'
#' Risultato: le letture in ordine di mezzo, giorno e ora, con la colonna `tratto`
#' (1, 2, ... in ogni mezzo e giorno).
#' @param letture Una riga per lettura: `mezzo`, `data_giro`, `ora` (secondi dalla
#'   mezzanotte).
#' @noRd
assegna_tratti <- function(letture) {
  ritardo <- ritardo_giro_minuti() * 60
  letture |>
    dplyr::arrange(.data$mezzo, .data$data_giro, .data$ora) |>
    dplyr::mutate(
      tratto = cumsum(c(TRUE, diff(.data$ora) > ritardo)),
      .by = c("mezzo", "data_giro")
    )
}

#' Allunga la fine dei giri pianificati con le letture in ritardo
#'
#' Contesto: un giro pianificato dalle 8 alle 12 spesso finisce dopo le 12. Le
#' letture di quel ritardo vanno al giro e non devono restare orfane.
#'
#' Funzionamento: la prima lettura del mezzo entro `ritardo_giro_minuti()` dalla
#' fine entra nel giro; con lei le seguenti, finché tra due letture passa al più
#' lo stesso tempo (catena). La catena si ferma all'inizio del giro pianificato
#' seguente dello stesso mezzo.
#'
#' Risultato: i giri con la `fine` allungata, usata dai passi che guardano
#' l'orario (`orario_comune`, `orario`, `vicino_orario`).
#' @param giri Una riga per giro: `riga`, `mezzo`, `data_giro`, `inizio`, `fine`,
#'   in secondi dalla mezzanotte.
#' @param letture Una riga per lettura: `mezzo`, `data_giro`, `ora`.
#' @return `giri`, con `fine` allungata.
#' @noRd
allunga_fine <- function(giri, letture) {
  ritardo <- ritardo_giro_minuti() * 60

  tratti <- letture |>
    assegna_tratti() |>
    dplyr::select("mezzo", "data_giro", "ora", "tratto")

  seguenti <- giri |>
    dplyr::filter(!is.na(.data$inizio)) |>
    dplyr::arrange(.data$inizio, .data$riga) |>
    dplyr::mutate(
      inizio_seguente = dplyr::lead(.data$inizio, default = Inf),
      .by = c("mezzo", "data_giro")
    ) |>
    dplyr::select("riga", "inizio_seguente")

  # Per ogni giro, le letture tra la fine e il giro seguente: la prima entra
  # se arriva entro il ritardo, e con lei il resto del suo tratto.
  fine_allungata <- giri |>
    dplyr::filter(!is.na(.data$fine)) |>
    dplyr::select("riga", "mezzo", "data_giro", "fine") |>
    dplyr::left_join(seguenti, by = "riga") |>
    dplyr::inner_join(
      tratti,
      by = c("mezzo", "data_giro"),
      relationship = "many-to-many"
    ) |>
    dplyr::filter(
      .data$ora > .data$fine,
      .data$ora < dplyr::coalesce(.data$inizio_seguente, Inf)
    ) |>
    dplyr::arrange(.data$ora) |>
    dplyr::filter(
      dplyr::first(.data$ora) - .data$fine <= ritardo,
      .data$tratto == dplyr::first(.data$tratto),
      .by = "riga"
    ) |>
    dplyr::summarise(fine_allungata = dplyr::last(.data$ora), .by = "riga")

  giri |>
    dplyr::left_join(fine_allungata, by = "riga") |>
    dplyr::mutate(fine = dplyr::coalesce(.data$fine_allungata, .data$fine)) |>
    dplyr::select(-"fine_allungata")
}

#' Giro del calendario di ogni lettura
#'
#' Contesto: una lettura esiste solo se un mezzo passa durante un giro, ma il
#' calendario dice che cosa era previsto, non che cosa è successo. Dare un giro
#' a una lettura serve a sapere quale servizio era atteso (e quindi se il bidone è
#' nel posto e nel servizio giusti) e in quale comune. Mai un giro di un altro
#' mezzo o di un altro giorno. Regole del proprietario, illustrate con
#' diagrammi in `docs/associazione_giri.qmd`.
#'
#' Funzionamento: ogni passo vale per le letture che i precedenti non hanno
#' associato:
#' 1. pianificato nel cui orario e nel cui comune cade la lettura;
#' 2. non pianificato nel comune della lettura, se in quel comune il mezzo ha un
#'    solo servizio;
#' 3. pianificato nel cui orario cade la lettura, in un altro comune;
#' 4. se tutti i non pianificati del mezzo hanno lo stesso servizio, quel servizio
#'    è certo: il giro è quello del comune più vicino;
#' 5. il giro più frequente tra le letture già associate dello stesso tratto
#'    continuo (`assegna_tratti()`);
#' 6. non pianificato del comune più vicino, entro `soglia_comune_vicino_m()`;
#' 7. pianificato più vicino nel tempo, entro `tolleranza_giro_minuti()`.
#' L'orario finisce alla fine allungata da `allunga_fine()`. Le matricole e i
#' comuni si confrontano senza badare a zeri iniziali, maiuscole, accenti e
#' apostrofi. Un pareggio tra servizi diversi non associa niente; a parità di
#' giro dello stesso servizio vale il primo del calendario.
#'
#' Risultato: una riga per lettura, con la riga del calendario scelta (`riga`,
#' mancante se nessun passo vale: la lettura resta senza giro), il `passo` che
#' l'ha scelta e `senza_giri`, vero se il mezzo quel giorno non ha nessun giro.
#' @param letture Letture con `matricola_veicolo`, `data_lettura`,
#'   `data_ora_lettura`, `comune_lettura`, `latitudine`, `longitudine`.
#' @param calendario Calendario di `unisci_calendari()`.
#' @return Una riga per lettura: `riga` del calendario, `passo` (testo di
#'   `passi_associazione()`), `senza_giri` se il mezzo quel giorno non ne ha.
#' @noRd
scegli_giro <- function(letture, calendario) {
  letture_ora <- letture |>
    dplyr::transmute(
      lettura = dplyr::row_number(),
      mezzo = chiave_matricola(.data$matricola_veicolo),
      data_giro = as.character(.data$data_lettura),
      ora = secondi_dalla_mezzanotte(.data$data_ora_lettura),
      comune_lettura = chiave_comune_o_na(.data$comune_lettura),
      latitudine = .data$latitudine,
      longitudine = .data$longitudine
    )
  tratti <- letture_ora |>
    assegna_tratti() |>
    dplyr::select("lettura", "tratto")

  giri <- calendario |>
    dplyr::transmute(
      riga = dplyr::row_number(),
      mezzo = chiave_matricola(.data$matricola_mezzo),
      data_giro = as.character(.data$data_giro),
      pianificato = .data$pianificato %in% TRUE,
      inizio = secondi_del_giorno(.data$ora_inizio),
      fine = secondi_del_giorno(.data$ora_fine),
      comune_nome = as.character(.data$comune_giro),
      comune_giro = chiave_comune_o_na(.data$comune_giro),
      servizio = stringr::str_to_upper(
        stringr::str_squish(as.character(.data$descrizione_servizio))
      )
    ) |>
    allunga_fine(letture_ora) |>
    # Quanti servizi hanno i non pianificati del mezzo nel giorno, e quanti
    # quelli dello stesso comune.
    dplyr::mutate(
      servizi_np = dplyr::n_distinct(.data$servizio[!.data$pianificato]),
      .by = c("mezzo", "data_giro")
    ) |>
    dplyr::mutate(
      servizi_np_comune = dplyr::n_distinct(.data$servizio),
      .by = c("mezzo", "data_giro", "pianificato", "comune_giro")
    )

  # Ogni lettura con ogni giro del suo mezzo nel suo giorno.
  coppie <- letture_ora |>
    dplyr::inner_join(
      giri,
      by = c("mezzo", "data_giro"),
      relationship = "many-to-many"
    ) |>
    dplyr::left_join(tratti, by = "lettura") |>
    dplyr::mutate(
      dentro = .data$pianificato &
        (dplyr::between(.data$ora, .data$inizio, .data$fine) %in% TRUE),
      distanza = dplyr::coalesce(
        pmax(.data$inizio - .data$ora, .data$ora - .data$fine, 0),
        Inf
      ),
      stesso_comune = (.data$comune_lettura == .data$comune_giro) %in% TRUE
    )

  # Per le letture ancora senza giro, la prima coppia ammessa secondo
  # `ordine` e, a parità, secondo il calendario. `prima` prepara le coppie
  # rimaste (ad esempio con la distanza dal comune).
  scegli <- function(scelte, passo, ammessa, ordine = 0, prima = identity) {
    nuove <- coppie |>
      dplyr::filter(!.data$lettura %in% scelte$lettura) |>
      prima() |>
      dplyr::filter({{ ammessa }}) |>
      dplyr::arrange(.data$lettura, {{ ordine }}, .data$riga) |>
      dplyr::distinct(.data$lettura, .keep_all = TRUE) |>
      dplyr::select("lettura", "riga") |>
      dplyr::mutate(passo = passi_associazione()[[passo]])
    dplyr::bind_rows(scelte, nuove)
  }

  # Metri dalla lettura al comune di ogni giro non pianificato.
  con_distanza <- function(coppie_rimaste) {
    coppie_rimaste |>
      dplyr::filter(!.data$pianificato) |>
      dplyr::mutate(
        distanza_m = distanza_dal_comune(
          .data$latitudine,
          .data$longitudine,
          .data$comune_nome
        )
      )
  }

  # Il non pianificato del comune più vicino, se i giri più vicini non hanno
  # servizi diversi.
  il_piu_vicino <- function(coppie_rimaste) {
    coppie_rimaste |>
      dplyr::filter(!.data$pianificato, !is.na(.data$latitudine)) |>
      con_distanza() |>
      dplyr::filter(!is.na(.data$distanza_m)) |>
      dplyr::slice_min(.data$distanza_m, by = "lettura", with_ties = TRUE) |>
      dplyr::filter(dplyr::n_distinct(.data$servizio) == 1, .by = "lettura")
  }

  # Il giro più frequente, tra le letture già associate, di ogni tratto.
  nel_tratto <- function(scelte) {
    vincitori <- scelte |>
      dplyr::inner_join(
        dplyr::distinct(coppie, .data$lettura, .data$mezzo, .data$data_giro, .data$tratto),
        by = "lettura"
      ) |>
      dplyr::count(.data$mezzo, .data$data_giro, .data$tratto, .data$riga) |>
      dplyr::slice_max(
        .data$n,
        by = c("mezzo", "data_giro", "tratto"),
        with_ties = TRUE
      ) |>
      dplyr::filter(dplyr::n() == 1, .by = c("mezzo", "data_giro", "tratto")) |>
      dplyr::select("mezzo", "data_giro", "tratto", "riga")
    nuove <- coppie |>
      dplyr::filter(!.data$lettura %in% scelte$lettura) |>
      dplyr::distinct(.data$lettura, .data$mezzo, .data$data_giro, .data$tratto) |>
      dplyr::inner_join(vincitori, by = c("mezzo", "data_giro", "tratto")) |>
      dplyr::select("lettura", "riga") |>
      dplyr::mutate(passo = passi_associazione()[["tratto"]])
    dplyr::bind_rows(scelte, nuove)
  }

  scelte <- dplyr::tibble(
    lettura = integer(),
    riga = integer(),
    passo = character()
  ) |>
    scegli("orario_comune", .data$dentro & .data$stesso_comune) |>
    scegli(
      "comune_non_pianificato",
      !.data$pianificato & .data$stesso_comune & .data$servizi_np_comune == 1
    ) |>
    scegli("orario", .data$dentro) |>
    scegli(
      "servizio_unico",
      .data$servizi_np == 1,
      ordine = .data$distanza_m,
      prima = \(rimaste) con_distanza(dplyr::filter(rimaste, .data$servizi_np == 1))
    ) |>
    nel_tratto() |>
    scegli(
      "comune_vicino",
      .data$distanza_m <= soglia_comune_vicino_m(),
      ordine = .data$distanza_m,
      prima = il_piu_vicino
    ) |>
    scegli(
      "vicino_orario",
      .data$pianificato & .data$distanza <= tolleranza_giro_minuti() * 60,
      ordine = .data$distanza
    )

  letture_ora |>
    dplyr::select("lettura") |>
    dplyr::left_join(scelte, by = "lettura") |>
    dplyr::mutate(senza_giri = !.data$lettura %in% coppie$lettura)
}

#' Valore più frequente, se è uno solo; mancanti e testi vuoti non contano
#'
#' Contesto: regola «a maggioranza» usata per dedurre un servizio da più bidoni.
#' Risultato: il valore più frequente, oppure `NA` se c'è un pareggio o non ci
#' sono valori.
#' @noRd
moda_unica <- function(valori) {
  valori <- valori[!is.na(valori) & valori != ""]
  if (length(valori) == 0) {
    return(NA_character_)
  }
  conteggi <- table(valori)
  piu_frequenti <- names(conteggi)[conteggi == max(conteggi)]
  if (length(piu_frequenti) == 1) piu_frequenti else NA_character_
}

#' Bidoni censiti diversi che servono in un tratto per dedurne il servizio
#'
#' Contesto: per un mezzo senza giri il servizio atteso si deduce dai bidoni
#' letti. Con meno bidoni il servizio più frequente sarebbe quasi quello di un
#' bidone solo, e la coerenza del servizio non segnalerebbe più niente.
#' Risultato: la soglia, 3.
#' @noRd
censiti_minimi_per_tratto <- function() 3L

#' Servizio più frequente tra i bidoni di un tratto; mancante sotto il minimo o con un pareggio
#'
#' Contesto: il cuore della regola per i mezzi senza giri
#' (`servizio_dei_censiti_del_mezzo()`).
#' Funzionamento: conta i bidoni diversi (`rfid`); sotto
#' `censiti_minimi_per_tratto()` non assegna niente, altrimenti applica
#' `moda_unica()`.
#' Risultato: un servizio o `NA`.
#' @param rfid,servizio Un bidone censito per riga, con il suo servizio.
#' @noRd
servizio_del_tratto <- function(rfid, servizio) {
  if (dplyr::n_distinct(rfid) < censiti_minimi_per_tratto()) {
    return(NA_character_)
  }
  moda_unica(servizio)
}

#' Servizio dei bidoni censiti letti da un mezzo senza giri, tratto per tratto
#'
#' Contesto: un mezzo che quel giorno non ha nessun giro nel calendario non ha
#' un `id_giro` (servizio e comune) da cui prendere il servizio atteso. Il giro
#' non si inventa: si deduce il solo servizio, dai bidoni che il mezzo ha letto.
#' Un mezzo non scarrabile raccoglie un rifiuto alla volta: il giro si approssima
#' con il tratto continuo (`assegna_tratti()`), non con il comune.
#'
#' Funzionamento: divide le letture dei mezzi senza giri in tratti; in ogni
#' tratto prende il servizio a database più frequente dei bidoni censiti, ognuno
#' contato una volta (`servizio_del_tratto()`): servono almeno
#' `censiti_minimi_per_tratto()` bidoni diversi e nessun pareggio. Nessun ripiego
#' sulla giornata intera.
#'
#' Risultato: un servizio per lettura, mancante se la lettura non è di un mezzo
#' senza giri o se il suo tratto non basta.
#' @param letture Letture con `matricola_veicolo`, `data_lettura`,
#'   `data_ora_lettura`, `RFID`, `presente_a_database`, `servizio_transponder` e
#'   `senza_giri`.
#' @return Un servizio per lettura, mancante se la lettura non è `senza_giri`.
#' @noRd
servizio_dei_censiti_del_mezzo <- function(letture) {
  senza_giri <- letture |>
    dplyr::transmute(
      lettura = dplyr::row_number(),
      mezzo = as.character(.data$matricola_veicolo),
      data_giro = as.character(.data$data_lettura),
      ora = secondi_dalla_mezzanotte(.data$data_ora_lettura),
      RFID = .data$RFID,
      presente = .data$presente_a_database,
      servizio = .data$servizio_transponder,
      senza_giri = .data$senza_giri
    ) |>
    dplyr::filter(.data$senza_giri %in% TRUE)
  tutte <- dplyr::tibble(lettura = seq_len(nrow(letture)))
  if (nrow(senza_giri) == 0) {
    return(rep(NA_character_, nrow(letture)))
  }

  con_tratto <- assegna_tratti(senza_giri)
  per_tratto <- con_tratto |>
    dplyr::filter(.data$presente %in% "Presente") |>
    dplyr::distinct(
      .data$mezzo,
      .data$data_giro,
      .data$tratto,
      .data$RFID,
      .data$servizio
    ) |>
    dplyr::summarise(
      servizio_dei_censiti = servizio_del_tratto(.data$RFID, .data$servizio),
      .by = c("mezzo", "data_giro", "tratto")
    )

  tutte |>
    dplyr::left_join(
      dplyr::left_join(
        con_tratto,
        per_tratto,
        by = c("mezzo", "data_giro", "tratto")
      ) |>
        dplyr::select("lettura", "servizio_dei_censiti"),
      by = "lettura"
    ) |>
    dplyr::pull("servizio_dei_censiti")
}

#' Associa a ogni lettura il giro del calendario, il servizio atteso e le coerenze
#'
#' Contesto: è il cuore della preparazione delle letture delle antenne. Per ogni
#' lettura dice quale giro la spiega (`id_giro`, `servizio_giro`, `comune_giro`,
#' `eseguito`), quale servizio era atteso (`servizio_atteso`) e se la lettura è
#' coerente con il database: `coerenza_servizio` (servizio a database contro
#' servizio atteso) e `coerenza_comune` (comune di lettura contro comune del
#' database o del giro, con la distanza dal comune dichiarato).
#'
#' Funzionamento:
#' 1. `scegli_giro()` sceglie il giro;
#' 2. il servizio atteso viene da `servizio_atteso_dal_giro()` (il nome del
#'    database del giro) o, per un mezzo senza giri, da
#'    `servizio_dei_censiti_del_mezzo()`;
#' 3. si calcolano `coerenza_servizio`, `distanza_comune_dichiarato` e
#'    `coerenza_comune` con le stesse funzioni dell'app.
#' Un messaggio conta le letture associate da ogni passo e quelle dei mezzi senza
#' giri rimaste senza servizio atteso.
#'
#' Risultato: le 28 colonne del CSV, con l'attributo `associazione`: le letture
#' di ogni passo e quelle senza giro.
#' @param df_letture Letture di `associa_contenitore_e_fornitura()`.
#' @param df_calendario Calendario di `unisci_calendari()`; senza la colonna
#'   `pianificato` vale come di soli giri pianificati.
#' @param servizi_applica_moda Giri dei servizi, vedi `giri_servizio_dai_censiti()`.
#' @return Le 28 colonne del CSV delle letture delle antenne, con l'attributo
#'   `associazione`.
#' @noRd
associa_giro_e_servizio_atteso <- function(
  df_letture,
  df_calendario,
  servizi_applica_moda = giri_servizio_dai_censiti()
) {
  calendario <- if ("pianificato" %in% names(df_calendario)) {
    df_calendario
  } else {
    unisci_calendari(df_calendario)
  }

  campi_del_giro <- calendario |>
    dplyr::transmute(
      riga = dplyr::row_number(),
      id_giro = as.character(.data$id_giro),
      eseguito = as.character(.data$eseguito),
      servizio_giro = as.character(.data$descrizione_servizio),
      comune_giro = as.character(.data$comune_giro)
    )
  scelta <- scegli_giro(df_letture, calendario)

  letture <- df_letture |>
    dplyr::select(-dplyr::any_of(names(campi_del_giro))) |>
    dplyr::bind_cols(dplyr::select(scelta, "riga", "passo", "senza_giri")) |>
    dplyr::left_join(campi_del_giro, by = "riga") |>
    dplyr::mutate(
      servizio_atteso = servizio_atteso_dal_giro(
        dplyr::pick(
          "data_lettura",
          "matricola_veicolo",
          "id_giro",
          "servizio_giro",
          "comune_giro",
          "servizio_transponder"
        ),
        servizi_applica_moda
      ),
      servizio_dei_censiti = servizio_dei_censiti_del_mezzo(dplyr::pick(
        "matricola_veicolo",
        "data_lettura",
        "data_ora_lettura",
        "RFID",
        "presente_a_database",
        "servizio_transponder",
        "senza_giri"
      )),
      servizio_atteso = dplyr::coalesce(
        .data$servizio_atteso,
        .data$servizio_dei_censiti
      ),
      passo = dplyr::if_else(
        is.na(.data$servizio_dei_censiti),
        .data$passo,
        passi_associazione()[["censiti"]]
      )
    )

  senza_giri <- letture$senza_giri %in% TRUE
  if (any(senza_giri)) {
    message(sprintf(
      "Mezzi senza giri nel giorno: %s letture, %s senza servizio atteso (tratti con meno di %d censiti o a pari voti).",
      formatta_numero(sum(senza_giri)),
      formatta_numero(sum(senza_giri & is.na(letture$servizio_atteso))),
      censiti_minimi_per_tratto()
    ))
  }

  associazione <- table(factor(
    dplyr::coalesce(letture$passo, "senza giro"),
    levels = c(unname(passi_associazione()), "senza giro")
  ))

  risultato <- letture |>
    dplyr::mutate(
      coerenza_servizio = coerenza_servizio(
        dplyr::pick("servizio_transponder", "servizio_atteso")
      ),
      distanza_comune_dichiarato = distanza_comune_dichiarato(dplyr::pick(
        "presente_a_database",
        "comune_lettura",
        "comune_fornitura",
        "comune_giro",
        "latitudine",
        "longitudine"
      )),
      coerenza_comune = coerenza_comune(dplyr::pick(
        "presente_a_database",
        "comune_lettura",
        "comune_fornitura",
        "comune_giro",
        "distanza_comune_dichiarato"
      ))
    ) |>
    dplyr::select(
      "data_ora_lettura",
      "data_lettura",
      "targa_veicolo",
      "matricola_veicolo",
      "id_giro",
      "eseguito",
      "RFID",
      "presente_a_database",
      "servizio_transponder",
      "servizio_atteso",
      "coerenza_servizio",
      "servizio_giro",
      "volume",
      "frequenza",
      "numero_raccolte_annue",
      "comune_fornitura",
      "comune_giro",
      "comune_lettura",
      "coerenza_comune",
      "distanza_comune_dichiarato",
      "cantiere",
      "data_attivazione_contenitore",
      "data_cessazione_contenitore",
      "id_fornitura",
      "stato_fornitura",
      "id_utenza",
      "latitudine",
      "longitudine"
    )
  structure(risultato, associazione = associazione)
}

#' Colonne del nucleo e della categoria, aggiunte da `associa_componenti()`
#'
#' Contesto: nucleo familiare e categoria servono al report sui conferimenti e
#' ai filtri per categoria dei report di confronto.
#' Risultato: i nomi delle colonne.
#' @noRd
colonne_componenti <- function() {
  c(
    "nucleo_residenti",
    "nucleo_non_residenti",
    "nucleo_totale",
    "tipo_categoria",
    "categoria",
    "sottocategoria"
  )
}

#' Le sette colonne in coda al CSV, per i conferimenti e i filtri dei report
#'
#' Risultato: nucleo residenti, non residenti e totale, tipo di categoria,
#' categoria, sottocategoria e `potenziamento_secco`. L'app non le carica.
#' @noRd
colonne_nucleo_manovra <- function() {
  c(colonne_componenti(), "potenziamento_secco")
}

#' Aggiunge nucleo e categoria della fornitura in vigore all'istante della lettura
#'
#' Contesto: il limite annuo di secco dipende dal nucleo e la categoria dice se
#' la fornitura è domestica o no. Le dichiarazioni cambiano nel tempo: serve
#' quella valida al momento della lettura.
#'
#' Funzionamento: tra le decorrenze della fornitura vale quella iniziata per
#' ultima, se non è già finita (fine mancante: ancora in vigore). Senza
#' fornitura o senza decorrenza i campi restano vuoti.
#'
#' Risultato: le letture con le colonne di `colonne_componenti()`.
#' @param df_letture Letture con `id_fornitura` e `data_ora_lettura`.
#' @param df_componenti Tabella di `importa_componenti()`.
#' @noRd
associa_componenti <- function(df_letture, df_componenti) {
  componenti <- dplyr::mutate(
    df_componenti,
    id_fornitura = as.character(.data$id_fornitura)
  )
  componenti[setdiff(colonne_componenti(), names(componenti))] <- NA

  in_vigore <- df_letture |>
    dplyr::distinct(
      id_fornitura = as.character(.data$id_fornitura),
      istante = .data$data_ora_lettura
    ) |>
    dplyr::filter(!is.na(.data$id_fornitura)) |>
    dplyr::inner_join(
      componenti,
      by = dplyr::join_by(
        "id_fornitura",
        "istante" >= "data_inizio_decorrenza"
      ),
      relationship = "many-to-many"
    ) |>
    dplyr::slice_max(
      .data$data_inizio_decorrenza,
      n = 1,
      with_ties = FALSE,
      by = c("id_fornitura", "istante")
    ) |>
    dplyr::filter(
      is.na(.data$data_fine_decorrenza) |
        .data$istante <= .data$data_fine_decorrenza
    ) |>
    dplyr::select(
      "id_fornitura",
      "istante",
      dplyr::all_of(colonne_componenti())
    )

  df_letture |>
    dplyr::select(-dplyr::any_of(colonne_componenti())) |>
    dplyr::mutate(fornitura_testo = as.character(.data$id_fornitura)) |>
    dplyr::left_join(
      in_vigore,
      by = c("fornitura_testo" = "id_fornitura", "data_ora_lettura" = "istante")
    ) |>
    dplyr::select(-"fornitura_testo")
}

#' Aggiunge `potenziamento_secco`: la fornitura ha una manovra sociale in vigore nel giorno
#'
#' Contesto: una fornitura con la manovra sociale ha un limite di litri più alto
#' (6240 l l'anno); il report sui conferimenti ne tiene conto.
#' Funzionamento: estremi compresi, fine mancante come ancora aperta. Una
#' dichiarazione che finisce prima di cominciare è annullata: non conta e dà un
#' avviso.
#' Risultato: la colonna `potenziamento_secco`, mancante senza fornitura.
#' @param df_letture Letture con `id_fornitura` e `data_lettura`.
#' @param df_manovra Tabella di `importa_manovre()`.
#' @noRd
associa_potenziamento <- function(df_letture, df_manovra) {
  dichiarazioni <- df_manovra |>
    dplyr::transmute(
      id_fornitura = as.character(.data$id_fornitura),
      inizio = as.Date(.data$data_inizio_dichiarazione),
      fine = dplyr::coalesce(
        as.Date(.data$data_fine_dichiarazione),
        as.Date("2099-12-31")
      ),
      annullata = (.data$fine < .data$inizio) %in% TRUE
    )

  annullate <- dplyr::filter(dichiarazioni, .data$annullata)
  if (nrow(annullate) > 0) {
    forniture <- unique(annullate$id_fornitura)
    warning(
      nrow(annullate),
      " dichiarazioni di manovra sociale finiscono prima di cominciare, ",
      "di solito perch\u00e9 annullate: non vengono considerate. Forniture: ",
      paste(utils::head(forniture, 10), collapse = ", "),
      if (length(forniture) > 10) ", ...",
      call. = FALSE
    )
  }

  con_potenziamento <- df_letture |>
    dplyr::distinct(
      id_fornitura = as.character(.data$id_fornitura),
      data_lettura = as.Date(.data$data_lettura)
    ) |>
    dplyr::filter(!is.na(.data$id_fornitura)) |>
    dplyr::semi_join(
      dplyr::filter(dichiarazioni, !.data$annullata),
      by = dplyr::join_by(
        "id_fornitura",
        "data_lettura" >= "inizio",
        "data_lettura" <= "fine"
      )
    ) |>
    dplyr::mutate(con_manovra = TRUE)

  df_letture |>
    dplyr::select(-dplyr::any_of("potenziamento_secco")) |>
    dplyr::mutate(
      fornitura_testo = as.character(.data$id_fornitura),
      data_chiave = as.Date(.data$data_lettura)
    ) |>
    dplyr::left_join(
      con_potenziamento,
      by = c("fornitura_testo" = "id_fornitura", "data_chiave" = "data_lettura")
    ) |>
    dplyr::mutate(
      potenziamento_secco = dplyr::if_else(
        is.na(.data$fornitura_testo),
        NA,
        .data$con_manovra %in% TRUE
      )
    ) |>
    dplyr::select(-"fornitura_testo", -"data_chiave", -"con_manovra")
}

#' Giri dei servizi: il servizio atteso viene dai censiti letti nel giro
#'
#' Contesto: giri come `SERVIZI MERCATI` o `SERV.MANIFESTAZIONE` non hanno un
#' rifiuto proprio: il rifiuto raccolto si capisce dai bidoni letti. L'elenco è
#' destinato a crescere. Un nome nuovo va aggiunto qui e alla tipologia
#' «Servizi» di `tipologie_servizio()`.
#' Risultato: i nomi dei giri, nel formato del calendario.
#' @noRd
giri_servizio_dai_censiti <- function() {
  c("SERVIZI MERCATI", "SERV.MANIFESTAZIONE")
}

#' Servizio atteso di ogni lettura, dal suo giro
#'
#' Contesto: il servizio atteso è quello che il giro lasciava aspettare, con il
#' nome della tipologia in maiuscolo (`SECCO`), mentre `servizio_giro` è il nome
#' del calendario (`SECCO PAP`).
#' Funzionamento: converte il giro con la tabella di conversione
#' (`servizio_da_giro()`). Nei giri di `servizi_applica_moda` prende invece il
#' servizio a database più frequente tra le letture del giro nel giorno, se è
#' uno solo. Il giro è `id_giro` per un pianificato; un non pianificato, senza
#' codice, è mezzo, servizio e comune.
#' Risultato: un servizio per lettura; mancante senza giro.
#' @param letture Letture con `data_lettura`, `id_giro`, `servizio_giro`,
#'   `servizio_transponder` e, se ci sono, `matricola_veicolo` e `comune_giro`.
#' @return Un servizio per lettura; mancante senza giro.
#' @noRd
servizio_atteso_dal_giro <- function(
  letture,
  servizi_applica_moda = giri_servizio_dai_censiti()
) {
  mezzo <- letture[["matricola_veicolo"]] %||% NA_character_
  comune_del_giro <- letture[["comune_giro"]] %||% NA_character_

  letture |>
    dplyr::mutate(
      giro_calendario = as.character(.data$servizio_giro),
      codice_giro = dplyr::coalesce(
        as.character(.data$id_giro),
        paste(mezzo, .data$giro_calendario, comune_del_giro, sep = "\r")
      ),
      dai_censiti = .data$giro_calendario %in% servizi_applica_moda
    ) |>
    dplyr::mutate(
      servizio_dei_censiti = if (isTRUE(dplyr::first(.data$dai_censiti))) {
        moda_unica(as.character(.data$servizio_transponder))
      } else {
        NA_character_
      },
      .by = c("data_lettura", "codice_giro", "dai_censiti")
    ) |>
    dplyr::mutate(
      atteso = dplyr::if_else(
        .data$dai_censiti,
        dplyr::coalesce(.data$servizio_dei_censiti, .data$giro_calendario),
        .data$giro_calendario
      )
    ) |>
    dplyr::pull("atteso") |>
    servizio_da_giro()
}

#' Servizio atteso di un giro: il nome del database, da `conversione_servizi()`
#'
#' Contesto: i nomi del calendario (`CARTA/CARTONE PAP`, `SECCO PAP`...) sono
#' molti; i servizi a database sono sei. La tabella `tipologie_servizio()` li
#' porta gli uni agli altri.
#' Risultato: i servizi attesi in maiuscolo. Un nome assente dalla tabella resta
#' com'è, con un avviso.
#' @noRd
servizio_da_giro <- function(servizio_giro) {
  nomi <- unique(stats::na.omit(as.character(servizio_giro)))
  ignoti <- nomi[
    !stringr::str_to_upper(stringr::str_trim(nomi)) %in%
      conversione_servizi()$servizio_calendario
  ]
  if (length(ignoti) > 0) {
    warning(
      "Servizi del calendario assenti dalla tabella di conversione, ",
      "lasciati come sono: ",
      paste(sort(ignoti), collapse = ", "),
      call. = FALSE
    )
  }
  uniforma_servizio(servizio_giro)
}

#' Metri di ogni lettura dal comune dichiarato, se fatta in un altro comune
#'
#' Contesto: un bidone letto a pochi metri dal confine del suo comune non è
#' spostato. La tolleranza (`tolleranza_confine_m()`, 100 m: portata dell'antenna
#' più errore GPS) la applica `coerenza_comune()` leggendo questa distanza.
#' Funzionamento: il comune dichiarato è quello del database per un censito,
#' quello del giro per un non censito. Calcola la distanza dal suo confine (zero
#' dentro).
#' Risultato: i metri; mancante per le letture nello stesso comune o se il
#' dichiarato non è un comune servito. Le colonne dei comuni possono mancare.
#' @noRd
distanza_comune_dichiarato <- function(letture) {
  colonna <- function(nome) {
    as.character(letture[[nome]] %||% rep(NA_character_, nrow(letture)))
  }
  comuni <- dplyr::tibble(
    latitudine = letture$latitudine,
    longitudine = letture$longitudine,
    comune_lettura = colonna("comune_lettura"),
    comune_dichiarato = dplyr::if_else(
      colonna("presente_a_database") %in% "Presente",
      colonna("comune_fornitura"),
      colonna("comune_giro")
    )
  ) |>
    dplyr::mutate(
      lettura = dplyr::row_number(),
      altro_comune = (chiave_comune_o_na(.data$comune_lettura) !=
        chiave_comune_o_na(.data$comune_dichiarato)) %in%
        TRUE
    )

  distanze <- comuni |>
    dplyr::filter(.data$altro_comune) |>
    dplyr::mutate(
      distanza = round(distanza_dal_comune(
        .data$latitudine,
        .data$longitudine,
        .data$comune_dichiarato
      ))
    )

  comuni |>
    dplyr::left_join(
      dplyr::select(distanze, "lettura", "distanza"),
      by = "lettura"
    ) |>
    dplyr::pull("distanza")
}

#' Giri del calendario da controllare
#'
#' Contesto: due anomalie vanno guardate a mano: un giro ancora «NO» (non
#' confermato) dopo una settimana, e un giro confermato («SI») di un mezzo con
#' l'antenna collaudata ma senza nessuna lettura (antenna spenta o guasta).
#' Funzionamento: sul periodo delle letture, cerca le letture per mezzo (senza
#' zeri iniziali), giorno e `id_giro`. Solo giri pianificati, compresi quelli
#' aggiunti a mano. Un avviso per tipo dice quanti sono.
#' Risultato: una riga per giro con `data_giro`, `matricola_veicolo`, `id_giro`,
#' `servizio_giro`, `comune_giro`, `eseguito` e `problema`; la pipeline la
#' scrive in `controllo_giri.csv`.
#' @noRd
controllo_giri <- function(
  letture,
  calendario,
  test_mezzi,
  giorni_conferma = 7
) {
  vuoto <- dplyr::tibble(
    data_giro = as.Date(character(0)),
    matricola_veicolo = character(0),
    id_giro = character(0),
    servizio_giro = character(0),
    comune_giro = character(0),
    eseguito = character(0),
    problema = character(0)
  )
  if (nrow(letture) == 0 || nrow(calendario) == 0) {
    return(vuoto)
  }
  if ("pianificato" %in% names(calendario)) {
    calendario <- dplyr::filter(calendario, .data$pianificato %in% TRUE)
  }
  periodo <- range(letture$data_lettura, na.rm = TRUE)

  # Le matricole si confrontano senza zeri iniziali, come in `scegli_giro()`.
  collaudi <- test_mezzi |>
    dplyr::filter(.data$esito %in% "POSITIVO") |>
    dplyr::mutate(chiave_mezzo = chiave_matricola(.data$matricola_veicolo)) |>
    dplyr::distinct(.data$chiave_mezzo, .keep_all = TRUE) |>
    dplyr::select("chiave_mezzo", "data_test")
  giri_letti <- letture |>
    dplyr::distinct(
      chiave_mezzo = chiave_matricola(.data$matricola_veicolo),
      data_giro = .data$data_lettura,
      id_giro = as.character(.data$id_giro)
    ) |>
    dplyr::mutate(letto = TRUE)

  giri <- calendario |>
    dplyr::filter(.data$data_giro >= periodo[1], .data$data_giro <= periodo[2]) |>
    dplyr::distinct(
      data_giro = .data$data_giro,
      matricola_veicolo = .data$matricola_mezzo,
      id_giro = as.character(.data$id_giro),
      servizio_giro = .data$descrizione_servizio,
      comune_giro = .data$comune_giro,
      eseguito = .data$eseguito
    ) |>
    dplyr::mutate(chiave_mezzo = chiave_matricola(.data$matricola_veicolo)) |>
    dplyr::left_join(collaudi, by = "chiave_mezzo") |>
    dplyr::left_join(
      giri_letti,
      by = c("chiave_mezzo", "data_giro", "id_giro")
    ) |>
    dplyr::mutate(
      conferma = toupper(stringr::str_squish(as.character(.data$eseguito))),
      non_confermato = .data$conferma %in%
        "NO" &
        .data$data_giro <= periodo[2] - giorni_conferma,
      con_antenna = (.data$data_giro > .data$data_test) %in% TRUE,
      senza_letture = .data$conferma %in%
        "SI" &
        .data$con_antenna &
        is.na(.data$letto)
    )

  if (sum(giri$non_confermato) > 0) {
    warning(
      sprintf(
        "%d giri sono ancora non confermati dopo pi\u00f9 di %d giorni: l'elenco \u00e8 in controllo_giri.csv.",
        sum(giri$non_confermato),
        giorni_conferma
      ),
      call. = FALSE
    )
  }
  if (sum(giri$senza_letture) > 0) {
    warning(
      sprintf(
        "%d giri confermati di mezzi con l'antenna non hanno nessuna lettura: l'antenna potrebbe essere spenta o guasta. L'elenco \u00e8 in controllo_giri.csv.",
        sum(giri$senza_letture)
      ),
      call. = FALSE
    )
  }

  dplyr::bind_rows(
    giri |>
      dplyr::filter(.data$non_confermato) |>
      dplyr::mutate(
        problema = sprintf("non confermato dopo %d giorni", giorni_conferma)
      ),
    giri |>
      dplyr::filter(.data$senza_letture) |>
      dplyr::mutate(problema = "confermato, senza letture dell'antenna")
  ) |>
    dplyr::select(dplyr::all_of(names(vuoto))) |>
    dplyr::arrange(.data$data_giro, .data$matricola_veicolo, .data$id_giro)
}

# --- La pipeline di preparazione --------------------------------------------------

#' Toglie sacchetti (`00BD...`) e codici che iniziano con dieci zeri
#'
#' Contesto: i dataset finali (antenne e saponette) riguardano i bidoni. I
#' sacchetti sono monouso e non hanno un servizio; i codici con dieci zeri
#' iniziali non sono bidoni. Togliere i sacchetti è una scelta del proprietario.
#' Risultato: le letture di soli bidoni.
#' @noRd
filtra_solo_bidoni <- function(letture) {
  dplyr::filter(
    letture,
    !startsWith(.data$RFID, "00BD"),
    !startsWith(.data$RFID, "0000000000")
  )
}

#' Letture delle antenne pronte per l'app, dai file estratti dal passo 1
#'
#' Contesto: è la funzione principale della pipeline per le antenne (il metodo
#' nuovo con cui l'azienda legge i bidoni). Da una decina di file grezzi produce
#' il CSV `letture_antenne.csv` che l'app carica e i report usano.
#'
#' Funzionamento, nell'ordine:
#' 1. `importa_letture_antenne()` e i test dei mezzi; teniamo i mezzi collaudati
#'    dal giorno dopo il collaudo (`filtra_letture_dopo_il_collaudo()`);
#' 2. RFID in formato comune (`formatta_rfid()`) e una lettura al giorno per RFID;
#' 3. contenitore e fornitura (`associa_contenitore_e_fornitura()`);
#' 4. comune di lettura corretto con la posizione (`correggi_comune_lettura()`);
#' 5. calendario completo: pianificati, giri aggiunti a mano
#'    (`aggiungi_giri_a_mano()`) e non pianificati;
#' 6. giro, servizio atteso e coerenze (`associa_giro_e_servizio_atteso()`);
#' 7. giri da controllare (`controllo_giri()`), nucleo e potenziamento, e solo i
#'    bidoni (`filtra_solo_bidoni()`).
#' Messaggi e avvisi dicono quante letture ha associato ogni passo e che cosa
#' controllare.
#'
#' Risultato: le 35 colonne del CSV, con gli attributi `controllo_giri` e
#' `comuni_corretti`; i file li scrivono i target della pipeline.
#' @param file Percorsi di `file_origine()`.
#' @return Le 35 colonne del CSV delle letture delle antenne, con gli attributi
#'   `controllo_giri` e `comuni_corretti`.
#' @noRd
prepara_letture_antenne <- function(file = file_origine()) {
  test_mezzi <- importa_test_mezzi(file$test_mezzi)

  non_pianificati <- if (file.exists(file$non_pianificati %||% "")) {
    importa_giri_non_pianificati(file$non_pianificati)
  } else {
    message(
      "Nessun file dei giri non pianificati: si usano i soli giri pianificati."
    )
    NULL
  }
  # I giri compiuti ma mancanti dal calendario, messi a mano dal proprietario,
  # entrano tra i pianificati.
  pianificati <- aggiungi_giri_a_mano(
    importa_calendario(file$calendario),
    importa_giri_aggiunti(file$giri_aggiunti)
  )
  calendario <- unisci_calendari(pianificati, non_pianificati)

  # Il comune di lettura si corregge dopo l'associazione al database: serve
  # il comune della fornitura.
  con_comune <- importa_letture_antenne(file$letture) |>
    filtra_letture_dopo_il_collaudo(test_mezzi) |>
    formatta_rfid() |>
    una_lettura_al_giorno() |>
    associa_contenitore_e_fornitura(
      importa_contenitori(file$contenitori),
      importa_sacchetti(file$gruppi_sacchetti, file$servizi_sacchetti)
    ) |>
    correggi_comune_lettura()

  con_giro <- associa_giro_e_servizio_atteso(con_comune, calendario)

  associazione <- attr(con_giro, "associazione")
  message(
    "Giro associato alle letture: ",
    paste(
      formatta_numero(as.integer(associazione)),
      names(associazione),
      collapse = "; "
    ),
    "."
  )

  da_controllare <- controllo_giri(con_giro, calendario, test_mezzi)

  con_giro |>
    associa_componenti(importa_componenti(file$componenti)) |>
    associa_potenziamento(importa_manovre(file$manovre)) |>
    filtra_solo_bidoni() |>
    structure(
      controllo_giri = da_controllare,
      comuni_corretti = attr(con_comune, "comuni_corretti")
    )
}

#' Scrive un data frame come CSV in `data-raw/output/`
#'
#' Contesto: i file finali della pipeline stanno tutti in `data-raw/output/`,
#' fuori da git perché contengono dati aziendali.
#' Risultato: il percorso del file scritto (così un target `format = "file"` lo
#' segue).
#' @param dati Il data frame.
#' @param nome Nome del file.
#' @param na Testo dei valori mancanti.
#' @noRd
scrivi_csv_output <- function(dati, nome, cartella = "data-raw/output", na = "NA") {
  dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
  percorso <- file.path(cartella, nome)
  utils::write.csv(dati, percorso, row.names = FALSE, na = na)
  percorso
}

#' Giri da controllare, dall'attributo di `prepara_letture_antenne()`
#'
#' Risultato: la tabella di `controllo_giri()`; una tabella vuota se l'attributo
#' manca (per esempio dopo aver riletto il CSV).
#' @noRd
giri_da_controllare <- function(letture) {
  attr(letture, "controllo_giri") %||%
    controllo_giri(letture[0, ], data.frame(), data.frame())
}

#' Colonne del CSV delle letture delle saponette: quelle delle antenne che hanno un senso, più `id_trasmissione`
#'
#' Contesto: le saponette hanno la stessa anagrafica delle antenne
#' (contenitore, fornitura, nucleo, potenziamento) ma non hanno mezzo, posizione,
#' giro, servizio atteso né comuni di lettura e del giro.
#' Risultato: i nomi, nello stesso ordine di `letture_antenne.csv`.
#' @noRd
colonne_letture_saponette <- function() {
  c(
    "data_ora_lettura",
    "data_lettura",
    "id_trasmissione",
    "RFID",
    "presente_a_database",
    "servizio_transponder",
    "volume",
    "frequenza",
    "numero_raccolte_annue",
    "comune_fornitura",
    "data_attivazione_contenitore",
    "data_cessazione_contenitore",
    "id_fornitura",
    "stato_fornitura",
    "id_utenza",
    colonne_nucleo_manovra()
  )
}

#' Letture delle saponette con contenitore, fornitura, nucleo e potenziamento
#'
#' Contesto: le saponette sono i lettori UHF manuali, il metodo usato prima delle
#' antenne e che gli operatori devono ancora usare. Il dataset serve ai report che
#' confrontano i due sistemi (`docs/confronto_antenne.qmd`,
#' `docs/controllo_giri_saponette.qmd`); l'app non lo carica.
#'
#' Funzionamento: legge il file (`importa_letture_saponette()`) e applica le stesse
#' associazioni della preparazione delle antenne: contenitore e fornitura, nucleo
#' e potenziamento. Non ha il giro: `id_trasmissione` non corrisponde all'`id_giro`
#' del calendario, quindi niente servizio atteso. Via i sacchetti e i codici con
#' dieci zeri, come per le antenne.
#'
#' Risultato: le colonne di `colonne_letture_saponette()`.
#' @param file Percorsi di `file_origine()`.
#' @param ... Nomi delle colonne, vedi `importa_letture_saponette()`.
#' @return Le colonne di `colonne_letture_saponette()`.
#' @noRd
prepara_letture_saponette <- function(file = file_origine(), ...) {
  importa_letture_saponette(file$saponette, ...) |>
    associa_contenitore_e_fornitura(
      importa_contenitori(file$contenitori),
      importa_sacchetti(file$gruppi_sacchetti, file$servizi_sacchetti)
    ) |>
    associa_componenti(importa_componenti(file$componenti)) |>
    associa_potenziamento(importa_manovre(file$manovre)) |>
    filtra_solo_bidoni() |>
    dplyr::select(dplyr::all_of(colonne_letture_saponette()))
}

#' Separatore di un CSV: `";"` se nell'intestazione supera le virgole, altrimenti `","`
#'
#' Contesto: i file estratti dal database aziendale usano la virgola o il punto
#' e virgola secondo l'esportazione. Funzionamento: conta i due caratteri nella
#' prima riga. Errore se il file è vuoto.
#' @param path Percorso del file.
#' @noRd
rileva_separatore <- function(path) {
  intestazione <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  if (length(intestazione) == 0 || !nzchar(stringr::str_trim(intestazione))) {
    errore_validazione("Il file \u00e8 vuoto.")
  }
  n_virgole <- stringr::str_count(intestazione, stringr::fixed(","))
  n_punti_virgola <- stringr::str_count(intestazione, stringr::fixed(";"))
  if (n_punti_virgola > n_virgole) ";" else ","
}
