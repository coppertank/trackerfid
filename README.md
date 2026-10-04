# trackerfid — Dashboard RFID Bidoni Rifiuti

Applicazione Shiny, costruita con il framework [golem](https://thinkr-open.github.io/golem/), per visualizzare su mappa le letture RFID dei bidoni per rifiuti raccolte dai mezzi aziendali.

Permette di:

- distinguere a colpo d'occhio i bidoni censiti nel database aziendale da quelli non censiti;
- vedere se la tipologia di rifiuto o l'utenza di un bidone sono cambiate nel tempo;
- seguire gli spostamenti di un singolo bidone;
- cercare i bidoni per RFID o per utenza proprietaria.

L'interfaccia è interamente in italiano.

## Avvio rapido

Servono R 4.1 o successivo e i pacchetti elencati in `DESCRIPTION`.

```r
# dalla cartella del progetto
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)

devtools::load_all()
run_app()             # si parte da un'app vuota e si carica un CSV
run_app(demo = TRUE)  # si parte con il dataset di esempio già caricato
```

In sviluppo si può usare anche `golem::run_dev()`.
Per installare il pacchetto: `devtools::install()`, poi `trackerfid::run_app()`.

## Formato del CSV

Il file deve contenere questi 10 campi. L'ordine delle colonne e le maiuscole nei nomi non contano, eventuali colonne in più vengono ignorate.

| Campo | Tipo | Contenuto |
|---|---|---|
| `giorno_lettura` | data e ora | Momento della lettura, `YYYY-MM-DD HH:MM:SS` |
| `targa_veicolo` | testo | Targa del mezzo che ha letto il chip |
| `matricola_veicolo` | testo | Matricola aziendale del mezzo, in relazione 1:1 con la targa |
| `RFID` | testo | Identificativo del chip, ripetuto a ogni lettura |
| `presente_a_database` | testo | `Presente` oppure `Non Presente` |
| `servizio_transponder` | testo | Tipologia secondo il database (`SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA`), vuoto se non censito |
| `servizio_atteso` | testo | Tipologia stimata dal calendario del mezzo, solo per i non censiti |
| `id_utenza` | testo | Utenza proprietaria, vuoto se non censito |
| `latitudine` | numero | Latitudine in gradi decimali |
| `longitudine` | numero | Longitudine in gradi decimali |

Cosa viene accettato:

- separatore virgola o punto e virgola, rilevato automaticamente;
- decimali con il punto o con la virgola;
- date anche senza secondi o in formato italiano (`DD/MM/YYYY HH:MM:SS`);
- valori mancanti scritti come `NA` o lasciati vuoti;
- file fino a 100 MB.

RFID e utenze sono sempre letti come testo, quindi gli zeri iniziali si conservano. Gli orari sono trattati come ora locale, senza conversioni di fuso.

### Validazione

Al caricamento l'app controlla il file e mostra l'esito sotto il campo di caricamento.

- **Errori, il file viene rifiutato**: colonne mancanti, date non riconoscibili, coordinate non numeriche o fuori scala, valori di `presente_a_database` diversi dai due ammessi. Il messaggio indica il campo e le righe del file coinvolte.
- **Avvisi, il file viene caricato**: righe prive di un campo essenziale (data, RFID, stato, coordinate), che vengono scartate; letture "Non Presente" che riportano comunque servizio o utenza; relazione targa/matricola non univoca.

## Come si usa

La barra laterale a sinistra, richiudibile dal pulsante in alto, contiene il caricamento del file, il dettaglio del bidone selezionato e i controlli della scheda aperta.

### Mappa Principale

Ogni bidone compare una sola volta, nella posizione della sua ultima lettura. I bidoni vicini sono raggruppati in cluster, che si aprono aumentando lo zoom.

I filtri agiscono su periodo, stato a database, servizio transponder e servizio atteso. Vengono applicati prima di scegliere l'ultima lettura: restringendo il periodo si vede la situazione a quella data. Le voci dei servizi elencano solo i valori presenti nel periodo scelto. Il filtro sul servizio atteso riguarda i soli bidoni non censiti. Sotto i filtri, il riquadro delle statistiche si aggiorna a ogni modifica.

### Ricerca RFID

Si inseriscono uno o più RFID, separati da virgola o a capo, e si preme «Cerca RFID». La mappa mostra tutte le letture dei bidoni cercati, senza cluster. L'ultima lettura ha il bordo spesso, le precedenti sono attenuate e tratteggiate. Un riquadro grigio racchiude tutte le posizioni dello stesso bidone.

### Ricerca Utenza

Si inseriscono uno o più ID utenza. La mappa mostra l'ultima lettura dei bidoni la cui utenza **attuale** è tra quelle cercate. Un bidone passato da un'utenza a un'altra compare solo cercando la nuova.

Entrambe le ricerche ignorano la differenza tra maiuscole e minuscole.

### Leggere i marker

| Elemento | Significato |
|---|---|
| Marker verde a goccia | Bidone presente a database |
| Marker rosso squadrato | Bidone non presente a database |
| Icona nel marker | Servizio: bidone (SECCO), foglio (CARTA), bottiglia (VETRO), foglia (UMIDO), riciclo (PLASTICA), punto di domanda (non censito) |
| Colore del cluster | Percentuale di censiti: rosso 0%, giallo 50%, verde 100% |
| Numeri nel cluster | Bidoni raggruppati e percentuale di censiti |

Lo stato è indicato sia dal colore sia dalla forma, così resta leggibile anche per chi non distingue rosso e verde.

### Dettaglio del bidone

Un click su un marker apre il popup con i dati della lettura e, nella barra laterale, l'analisi di tutta la storia del bidone:

- **censito con servizio coerente**: conferma della tipologia;
- **cambio di servizio**: cronologia dei passaggi, con il servizio attuale;
- **non censito**: stima percentuale del servizio ricavata dal calendario dei mezzi, oppure l'indicazione che non ci sono elementi per stimarlo;
- **cambio di proprietario**: cronologia delle utenze, mostrata in aggiunta ai casi precedenti.

## Dataset di esempio

`inst/extdata/sample_rfid_dataset.csv` contiene 736 letture di 236 bidoni, relative a settembre 2025, cinque mezzi e 50 utenze nell'area di Roma. Lo genera lo script `data-raw/genera_sample_dataset.R`, che ha un seed fisso e ne verifica la conformità prima di scrivere il file.

Casi utili da provare:

| RFID | Cosa mostra |
|---|---|
| `RFD20250901001` | Cambio di servizio: CARTA, poi SECCO, poi di nuovo CARTA |
| `RFD20250901050` | Cambio di utenza da `UTZ001` a `UTZ025` |
| `RFD20250915201` | Non censito con stima 100% SECCO |
| `RFD20250920250` | Non censito senza elementi per la stima |
| `RFD20250905100` | Bidone spostato di molto, con riquadro ampio |

## Struttura del progetto

```
R/
  app_ui.R, app_server.R        interfaccia e collegamento dei moduli
  run_app.R, app_config.R       avvio e configurazione golem
  mod_caricamento_*.R           caricamento e validazione del CSV
  mod_filtri_*.R                filtri laterali e statistiche
  mod_mappa_*.R                 mappa Leaflet, usata dalle tre schede
  mod_ricerca_*.R               ricerca per RFID e per utenza
  utils_data_processing.R       lettura, validazione, filtri, analisi dei bidoni
  utils_mapping.R               costruzione e aggiornamento delle mappe
  utils_styling.R               marker, cluster, legenda
  utils_popup.R                 popup, dettaglio del bidone, statistiche
inst/
  extdata/sample_rfid_dataset.csv
  app/www/custom_style.css, script.js
data-raw/genera_sample_dataset.R
tests/testthat/
```

Ogni modulo ha un file `_ui.R` e uno `_server.R`. La mappa di base viene disegnata una volta sola: filtri e ricerche aggiornano i marker senza ricaricare lo sfondo.

## Test

```r
devtools::test()    # funzioni di utilità, moduli e conformità del dataset di esempio
devtools::check()
```

## Differenze rispetto alla specifica

- **`leafletmarkercluster` e `shinyicon` non esistono su CRAN.** Il clustering usa il plugin Leaflet.markercluster già incluso in `leaflet`. Le icone Font Awesome arrivano dal pacchetto `fontawesome`.
- **Marker disegnati in SVG.** In questo modo hanno i colori esatti richiesti (`#2ecc71`, `#e74c3c`) e il bordo spesso o tratteggiato della ricerca RFID, che i marker standard non permettono. La cartella di icone PNG non serve.
- **Nomi delle utenze.** Il dataset non contiene i nomi dei proprietari, quindi il cambio di proprietario mostra solo gli ID utenza.
- **Filtri dei servizi.** Deselezionando tutte le voci restano visibili solo i non censiti (se inclusi). Lo pseudocodice della specifica in quel caso disattivava il filtro.
- **Coordinate del dataset.** I bidoni sono distribuiti nell'area indicata (lat 41,9 ± 0,05, lng 12,4 ± 0,05). Fanno eccezione le tre righe di esempio della specifica, in centro a Roma, e il bidone "spostato molto".

## Da completare

Il campo `License` di `DESCRIPTION` contiene ancora il segnaposto di golem. Finché non viene scelto, `devtools::check()` riporta un avviso.
