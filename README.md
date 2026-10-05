# trackerfid — Dashboard RFID Bidoni Rifiuti

Applicazione Shiny, costruita con il framework [golem](https://thinkr-open.github.io/golem/), per visualizzare su mappa le letture RFID dei bidoni per rifiuti raccolte dai mezzi aziendali.

Permette di:

- distinguere a colpo d'occhio i bidoni censiti nel database aziendale da quelli non censiti;
- vedere se la tipologia di rifiuto o l'utenza di un bidone sono cambiate nel tempo;
- seguire gli spostamenti di un singolo bidone;
- cercare i bidoni per RFID o per utenza proprietaria;
- analizzare per ogni bidone i luoghi in cui è stato letto nel periodo, per riconoscere spostamenti, tag smarriti o rimasti sui mezzi.

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

Il file deve contenere 10 campi obbligatori e può contenerne 2 facoltativi. L'ordine delle colonne e le maiuscole nei nomi non contano, eventuali altre colonne vengono ignorate.

| Campo | Tipo | Contenuto |
|---|---|---|
| `giorno_lettura` | data e ora | Momento della lettura, `YYYY-MM-DD HH:MM:SS` |
| `targa_veicolo` | testo | Targa del mezzo che ha letto il chip |
| `matricola_veicolo` | testo | Matricola aziendale del mezzo, in relazione 1:1 con la targa |
| `RFID` | testo | Identificativo del chip, ripetuto a ogni lettura |
| `presente_a_database` | testo | `Presente` oppure `Non Presente` |
| `servizio_transponder` | testo | Tipologia secondo il database (`SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE`), vuoto se non censito |
| `servizio_atteso` | testo | Nome del giro previsto dal calendario del mezzo (ad esempio `SECCO PAP`, `CARTA CONT.STRADALI`), solo per i non censiti |
| `id_utenza` | testo | Utenza proprietaria, vuoto se non censito |
| `latitudine` | numero | Latitudine in gradi decimali |
| `longitudine` | numero | Longitudine in gradi decimali |
| `volume_previsto` | numero | Facoltativo. Volume del contenitore in litri secondo il database, vuoto se non censito |
| `numero_raccolte_annue_previste` | numero | Facoltativo. Raccolte previste in un anno, vuoto se non censito |

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
- **Avvisi, il file viene caricato**: righe prive di un campo essenziale (data, RFID, stato, coordinate), che vengono scartate; letture "Non Presente" che riportano comunque servizio o utenza; relazione targa/matricola non univoca; colonne facoltative assenti, nel qual caso i campi dell'analisi dei cluster che ne dipendono restano vuoti.

## Come si usa

La barra laterale a sinistra, richiudibile dal pulsante in alto, contiene il caricamento del file, il periodo di analisi, il dettaglio del bidone selezionato e i controlli della scheda aperta.

### Periodo di analisi

Tutta l'app lavora sulle letture di un periodo: mappe, ricerche, statistiche, dettaglio del bidone e analisi dei cluster. Il periodo compare nel titolo, ad esempio «DASHBOARD RFID - ANNO 2025».

- **Anno.** L'elenco propone gli anni presenti nel file. All'apertura è selezionato il più recente, dal 1 gennaio al 31 dicembre.
- **Periodo personalizzato.** Spuntando la casella compaiono le due date, precompilate con l'anno scelto.

### Mappa Principale

Ogni bidone compare una sola volta, nella posizione della sua ultima lettura nel periodo. I bidoni vicini sono raggruppati in cluster, che si aprono aumentando lo zoom.

I filtri agiscono su stato a database, servizio transponder e servizio atteso. Vengono applicati prima di scegliere l'ultima lettura. Le voci dei servizi elencano solo i valori presenti nel periodo. Il filtro sul servizio atteso riguarda i soli bidoni non censiti. Sotto i filtri, il riquadro delle statistiche si aggiorna a ogni modifica.

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
| Icona nel marker, bidone censito | Servizio del transponder, vedi la tabella delle tipologie qui sotto |
| Icona nel marker, bidone non censito | Servizio atteso prevalente, se almeno l'80% delle letture con stima concorda. Altrimenti punto di domanda. Il marker resta rosso |
| Colore del cluster | Percentuale di censiti: rosso 0%, giallo 50%, verde 100% |
| Numeri nel cluster | Bidoni raggruppati e percentuale di censiti |

Lo stato è indicato sia dal colore sia dalla forma, così resta leggibile anche per chi non distingue rosso e verde.

Per i non censiti la quota dell'80% si calcola per tipologia, su tutte le letture del periodo: due giri della stessa tipologia, come `CARTA CONT.STRADALI` e `CARTA/CARTONE PAP`, contano insieme.

### Tipologie di servizio e icone

I nomi di `servizio_atteso` derivano dal giro dei mezzi (PAP = porta a porta) e sono diversi da quelli di `servizio_transponder`. I nomi della stessa tipologia condividono icona e colore.

| Tipologia | Icona | `servizio_transponder` | `servizio_atteso` |
|---|---|---|---|
| Secco | bidone | `SECCO` | `SECCO PAP` |
| Carta | foglio | `CARTA` | `CARTA CONT.STRADALI`, `CARTA/CARTONE PAP` |
| Vetro | bottiglia | `VETRO` | `VETRO PAP` |
| Umido | foglia | `UMIDO` | `UMIDO CONT.STRADALI`, `UMIDO PAP` |
| Plastica e metalli | riciclo | `PLASTICA E METALLI` | `PLAST.CONT.STRADALI`, `PLASTICA PAP` |
| Verde e ramaglie | albero | `VERDE E RAMAGLIE` | `VERDE PAP` |
| Assistente servizi | operatore | | `ASSISTENTE SERVIZI` |
| Pulizia territorio | scopa | | `PULIZIA TERRIT.` |
| Servizi mercati | negozio | | `SERVIZI MERCATI` |

Un nome non previsto viene comunque caricato e mostrato con il punto di domanda.

**Aggiungere una tipologia o cambiare un'icona.** Si modifica solo la funzione `tipologie_servizio()` in `R/utils_styling.R`. Per un nuovo nome basta aggiungerlo, in maiuscolo, al vettore `servizi` della tipologia giusta. Per una nuova tipologia si aggiunge una voce con nome, icona [Font Awesome](https://fontawesome.com/icons), colore ed emoji. Marker, legenda, filtri e statistiche si aggiornano da soli.

### Dettaglio del bidone

Un click su un marker apre il popup con i dati della lettura e, nella barra laterale, l'analisi di tutta la storia del bidone:

- **censito con servizio coerente**: conferma della tipologia;
- **cambio di servizio**: cronologia dei passaggi, con il servizio attuale;
- **non censito**: stima percentuale del servizio ricavata dal calendario dei mezzi, oppure l'indicazione che non ci sono elementi per stimarlo;
- **cambio di proprietario**: cronologia delle utenze, mostrata in aggiunta ai casi precedenti.

### Analisi Cluster Spaziale

La quarta scheda raggruppa le letture di ogni bidone in luoghi distinti e classifica ogni luogo. Si preme «Genera Analisi» nella barra laterale: l'analisi riguarda tutte le letture del periodo, a prescindere dai filtri della mappa. Se si cambia periodo o file il risultato viene scartato e va rigenerato.

Il risultato ha una riga per ogni cluster di ogni RFID e si esplora in tre viste:

- **Tabella**: tutte le colonne, ordinabili, con un filtro per colonna. I filtri della tabella valgono anche per le altre due viste.
- **Grafici**: numero di cluster per indicatore, e letture contro dispersione, con un riquadro per indicatore.
- **Mappa**: un punto sul baricentro di ogni cluster e un cerchio con il raggio della dispersione. Gli indicatori si accendono e spengono dal riquadro in alto a destra.

Un click su una riga o su un punto apre il dettaglio del bidone nella barra laterale. «Esporta CSV» salva l'intera tabella come `cluster_analysis_[dal]_[al].csv`.

| Indicatore | Significato |
|---|---|
| `VALID_TARGET` | Contenitore stabile nella sua posizione |
| `RELOCATED_BIN` | Contenitore spostato da un punto a un altro |
| `GHOST_TAG` | Poche letture: tag smarrito o letto per caso |
| `DEPOT_STUCK` | Tag fermo vicino a un lettore, ad esempio in deposito |
| `TRUCK_STOWAWAY` | Tag rimasto sul mezzo, letto lungo tutto il giro |
| `SCATTERED_READS / GPS_NOISE` | Letture sparse o GPS impreciso |
| `UNCATEGORIZED` | Nessuna delle categorie precedenti |

Regole, colonne, indice di fiducia, parametri e scelte di progetto sono descritti in [docs/analisi_cluster.md](docs/analisi_cluster.md).

L'indice di fiducia ha un report dedicato, [docs/indice_fiducia.Rmd](docs/indice_fiducia.Rmd): spiega come si calcola e perché il metodo è semplice ed efficace. Numeri e grafici sono calcolati sul dataset di esempio quando lo si compila in HTML:

```r
rmarkdown::render("docs/indice_fiducia.Rmd")   # crea docs/indice_fiducia.html
```

### Analisi senza l'app

La stessa analisi si esegue da R o da riga di comando.

```r
devtools::load_all()
risultato <- genera_analisi_cluster(
  csv_path = "data/letture_rfid_2025.csv",
  data_inizio = as.Date("2025-01-01"),
  data_fine = as.Date("2025-12-31")
)
scrivi_analisi_cluster(risultato, "output")   # output/cluster_analysis_2025-01-01_2025-12-31.csv
```

```sh
Rscript inst/scripts/generate_cluster_analysis.R letture.csv 2025-01-01 2025-12-31 output
```

Senza date si analizza l'anno più recente del file. Lo script si può anche caricare con `source()`, che rende disponibile `genera_analisi_cluster()`.

## Dataset di esempio

`inst/extdata/sample_rfid_dataset.csv` contiene 6.463 letture di 239 bidoni, da ottobre 2024 a dicembre 2025, con cinque mezzi e 50 utenze nell'area di Roma. Il 2025 è completo, il 2024 parziale: serve a provare la selezione dell'anno. Ogni bidone viene letto a cadenza fissa dal giro del proprio servizio, come con un calendario di raccolta reale.

Lo genera lo script `data-raw/genera_sample_dataset.R`, che ha un seed fisso e ne verifica la conformità prima di scrivere il file. Nella stessa cartella, `cluster_analysis_2025-01-01_2025-12-31.csv` è l'analisi dei cluster del 2025, prodotta con lo script descritto sopra.

Casi utili da provare:

| RFID | Cosa mostra |
|---|---|
| `RFD20250901001` | Cambio di servizio: CARTA, poi SECCO, poi di nuovo CARTA |
| `RFD20250901050` | Cambio di utenza da `UTZ001` a `UTZ025` |
| `RFD20250915201` | Non censito con stima 100% SECCO PAP, quindi con icona del secco. Poche letture: `GHOST_TAG` |
| `RFD20250920250` | Non censito senza elementi per la stima |
| `RFD20250905100` | Bidone spostato di oltre 20 km: `RELOCATED_BIN` con due cluster |
| `RFD20241001301` | Tag fermo in deposito, oltre 400 letture l'anno: `DEPOT_STUCK` |
| `RFD20250203302` | Tag rimasto sul camion, letto lungo tutto il giro: `TRUCK_STOWAWAY` |
| `RFD20241003303` | GPS impreciso, 17 cluster: `SCATTERED_READS / GPS_NOISE` |

## Struttura del progetto

```
R/
  app_ui.R, app_server.R        interfaccia e collegamento dei moduli
  run_app.R, app_config.R       avvio e configurazione golem
  mod_caricamento_*.R           caricamento e validazione del CSV
  mod_periodo_*.R               anno o periodo personalizzato, valido per tutta l'app
  mod_filtri_*.R                filtri laterali e statistiche
  mod_mappa_*.R                 mappa Leaflet, usata dalle tre schede con mappa
  mod_ricerca_*.R               ricerca per RFID e per utenza
  mod_cluster_analysis_*.R      scheda di analisi dei cluster spaziali
  utils_data_processing.R       lettura, validazione, filtri, analisi dei bidoni
  utils_mapping.R               costruzione e aggiornamento delle mappe
  utils_styling.R               marker, cluster, legenda, tipologie di servizio
  utils_popup.R                 popup, dettaglio del bidone, statistiche
  utils_dbscan_clustering.R     clustering DBSCAN e distanze
  utils_cluster_output.R        tabella di analisi, indicatori, indice di fiducia
  utils_cluster_viste.R         tabella interattiva, grafici e mappa dei cluster
  generate_cluster_analysis.R   funzioni esportate per l'analisi senza app
inst/
  extdata/                      dataset di esempio e analisi dei cluster di esempio
  scripts/generate_cluster_analysis.R   script eseguibile da riga di comando
  app/www/custom_style.css, script.js
data-raw/genera_sample_dataset.R
docs/
  analisi_cluster.md            analisi dei cluster: regole, colonne, scelte
  indice_fiducia.Rmd            report sull'indice di fiducia, da compilare in HTML
tests/testthat/
```

Il file [CLAUDE.md](CLAUDE.md) descrive il progetto per gli assistenti LLM: contesto, flusso dei dati, indice delle funzioni e dove intervenire per ogni tipo di modifica.

Ogni modulo ha un file `_ui.R` e uno `_server.R`. Le mappe di base vengono disegnate una volta sola: filtri e ricerche aggiornano i marker senza ricaricare lo sfondo.

## Test

```r
devtools::test()    # funzioni di utilità, moduli, analisi dei cluster e dataset di esempio
devtools::check()
```

## Differenze rispetto alle specifiche

Prima specifica:

- **`leafletmarkercluster` e `shinyicon` non esistono su CRAN.** Il clustering usa il plugin Leaflet.markercluster già incluso in `leaflet`. Le icone Font Awesome arrivano dal pacchetto `fontawesome`.
- **Marker disegnati in SVG.** In questo modo hanno i colori esatti richiesti (`#2ecc71`, `#e74c3c`) e il bordo spesso o tratteggiato della ricerca RFID, che i marker standard non permettono. La cartella di icone PNG non serve.
- **Nomi delle utenze.** Il dataset non contiene i nomi dei proprietari, quindi il cambio di proprietario mostra solo gli ID utenza.
- **Filtri dei servizi.** Deselezionando tutte le voci restano visibili solo i non censiti (se inclusi). Lo pseudocodice della specifica in quel caso disattivava il filtro.

Specifica delle correzioni e dell'analisi dei cluster:

- **Dataset di esempio su base annuale.** La prima versione copriva 30 giorni, con una o due letture per la maggior parte dei bidoni. Raccolte annue e soglie dell'analisi dei cluster non erano dimostrabili su quei dati. I casi speciali della prima versione sono rimasti. Il file ha mantenuto il nome `sample_rfid_dataset.csv`.
- **Periodo valido per tutta l'app.** La selezione dell'anno e il periodo personalizzato stanno in un solo controllo nella barra laterale, usato anche dall'analisi dei cluster.
- **Tre colonne in più nell'output.** La specifica parlava di 25 campi e ne definiva 22. Sono stati aggiunti `presente_a_database`, `servizio_transponder_cronologia` e `servizio_atteso_dettaglio`.
- **Classificazione.** Tre correzioni alle regole, descritte in [docs/analisi_cluster.md](docs/analisi_cluster.md).
- **Distanze per DBSCAN.** Proiezione locale in metri al posto della matrice di Haversine, per reggere tag con migliaia di letture. I cluster coincidono.
- **Script autonomo.** Le funzioni stanno nel pacchetto, lo script in `inst/scripts` le richiama. Un file con codice duplicato sarebbe rimasto disallineato alla prima modifica.

## Da completare

Il campo `License` di `DESCRIPTION` contiene ancora il segnaposto di golem. Finché non viene scelto, `devtools::check()` riporta un avviso.
