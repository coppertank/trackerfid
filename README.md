# trackerfid — Dashboard RFID Bidoni Rifiuti

Applicazione Shiny, costruita con il framework [golem](https://thinkr-open.github.io/golem/), per visualizzare su mappa le letture RFID dei bidoni per rifiuti raccolte dai mezzi aziendali.

Permette di:

- distinguere a colpo d'occhio i bidoni censiti nel database aziendale da quelli non censiti;
- vedere se la tipologia di rifiuto o l'utenza di un bidone sono cambiate nel tempo;
- seguire gli spostamenti di un singolo bidone;
- filtrare i bidoni per cantiere e per comune;
- cercare i bidoni per RFID o per utenza proprietaria;
- analizzare per ogni bidone i luoghi in cui è stato letto nel periodo, per riconoscere spostamenti, tag smarriti o rimasti sui mezzi;
- vedere quante volte un bidone è stato letto anno per anno, anche prima dell'installazione delle antenne sui mezzi.

L'interfaccia è interamente in italiano.

## Avvio rapido

Servono R 4.1 o successivo e i pacchetti elencati in `DESCRIPTION`.

```r
# dalla cartella del progetto
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)

devtools::load_all()
run_app()             # si parte da un'app vuota e si carica un file
run_app(demo = TRUE)  # si parte con i dati di esempio già caricati
run_app(letture = "letture.csv.gz")   # si parte con un file letto dal disco: vedi «File grandi»
```

In sviluppo si può usare anche `golem::run_dev()`.
Per installare il pacchetto: `devtools::install()`, poi `trackerfid::run_app()`.

## Formato del file

Il file deve contenere 10 campi obbligatori e può contenerne 5 facoltativi. L'ordine delle colonne e le maiuscole nei nomi non contano, eventuali altre colonne vengono ignorate.

| Campo | Tipo | Contenuto |
|---|---|---|
| `giorno_lettura` | data e ora | Momento della lettura, `YYYY-MM-DD HH:MM:SS` |
| `targa_veicolo` | testo | Targa del mezzo che ha letto il chip |
| `matricola_veicolo` | testo | Matricola aziendale del mezzo, in relazione 1:1 con la targa |
| `RFID` | testo | Identificativo del chip, ripetuto a ogni lettura |
| `presente_a_database` | testo | `Presente` oppure `Non Presente` |
| `servizio_transponder` | testo | Tipologia secondo il database (`SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE`), vuoto se non censito |
| `servizio_atteso` | testo | Nome del giro che il mezzo stava facendo al momento della lettura, dal calendario dei mezzi (ad esempio `SECCO PAP`, `CARTA CONT.STRADALI`). C'è per ogni lettura |
| `id_utenza` | testo | Utenza proprietaria, vuoto se non censito |
| `latitudine` | numero | Latitudine in gradi decimali |
| `longitudine` | numero | Longitudine in gradi decimali |
| `volume_previsto` | numero | Facoltativo. Volume del contenitore in litri secondo il database, vuoto se non censito |
| `numero_raccolte_annue_previste` | numero | Facoltativo. Raccolte previste in un anno, vuoto se non censito |
| `comune_da_database` | testo | Facoltativo. Comune di servizio secondo il database, vuoto se non censito |
| `comune_lettura` | testo | Facoltativo. Comune atteso: quello del giro che il mezzo stava facendo al momento della lettura. C'è per ogni lettura |
| `cantiere` | testo | Facoltativo. Cantiere del comune del database, se c'è, altrimenti di quello della lettura |

**Giro e comune del giro.** Una lettura esiste solo se un mezzo passa durante un giro, e dal calendario dei mezzi si ricavano il giro e il suo comune. Per questo `servizio_atteso` e `comune_lettura` sono compilati per ogni lettura, anche per i bidoni censiti. Per un bidone non censito sono le sole informazioni disponibili: l'app ne ricava il servizio stimato e il comune. Per un bidone censito valgono il servizio e il comune del database.

**Comuni e cantieri.** La tabella `inst/extdata/comuni_cantieri.csv` elenca i 57 comuni serviti e il cantiere di ognuno: `ASIAGO`, `BASSANO`, `CAMPOSAMPIERO`, `RUBANO`. I nomi dei comuni del file sono riportati alla grafia della tabella, senza badare a maiuscole, accenti e apostrofi. Il comune usato da filtri e report è quello del database, se c'è, altrimenti quello della lettura. Il cantiere viene ricalcolato dal comune secondo la tabella: quello scritto nel file vale solo per i comuni che la tabella non contiene, segnalati con un avviso. La colonna `comune` dei file meno recenti vale come `comune_da_database`.

Cosa viene accettato:

- file CSV, CSV compresso con gzip (`.csv.gz`) oppure Parquet (`.parquet`, con il pacchetto `nanoparquet`);
- separatore virgola o punto e virgola, rilevato automaticamente;
- decimali con il punto o con la virgola;
- date anche senza secondi o in formato italiano (`DD/MM/YYYY HH:MM:SS`);
- valori mancanti scritti come `NA` o lasciati vuoti;
- file fino a 2 GB dal browser. Per i file con milioni di letture vedi [File grandi](#file-grandi).

RFID e utenze sono sempre letti come testo, quindi gli zeri iniziali si conservano. Gli orari sono trattati come ora locale, senza conversioni di fuso.

### Validazione

Al caricamento l'app controlla il file e mostra l'esito sotto il campo di caricamento.

- **Errori, il file viene rifiutato**: colonne mancanti, date non riconoscibili, coordinate non numeriche o fuori scala, valori di `presente_a_database` diversi dai due ammessi. Il messaggio indica il campo e le righe del file coinvolte.
- **Avvisi, il file viene caricato**: righe prive di un campo essenziale (data, RFID, stato, coordinate), che vengono scartate; letture "Non Presente" che riportano comunque servizio o utenza; relazione targa/matricola non univoca; colonne facoltative assenti, nel qual caso le informazioni che ne dipendono restano vuote.

### Letture storiche

Le letture fatte prima dell'installazione delle antenne sui mezzi stanno in un secondo file, facoltativo, con due sole colonne. Sono accettati gli stessi formati dell'altro file.

| Campo | Tipo | Contenuto |
|---|---|---|
| `RFID` | testo | Identificativo del chip, scritto come nel file delle letture con le antenne |
| `giorno_lettura` | data e ora | Momento della lettura, negli stessi formati accettati per l'altro file |

Si carica dal secondo campo della sezione di caricamento. Eventuali altre colonne vengono ignorate, le righe incomplete e quelle ripetute vengono scartate con un avviso. L'app usa questo file nel dettaglio del bidone, per l'istogramma delle letture per anno. Il confronto completo tra i due sistemi è nel [report sul beneficio delle antenne](#report-sul-beneficio-delle-antenne).

## Come si usa

La barra laterale a sinistra, richiudibile dal pulsante in alto, contiene il caricamento dei file, il periodo di analisi, il dettaglio del bidone selezionato e i controlli della scheda aperta.

### Periodo di analisi

Tutta l'app lavora sulle letture di un periodo: mappe, ricerche, statistiche, dettaglio del bidone e analisi dei cluster. Il periodo compare nel titolo, ad esempio «DASHBOARD RFID - ANNO 2025».

- **Anno.** L'elenco propone gli anni presenti nel file. All'apertura è selezionato il più recente, dal 1 gennaio al 31 dicembre.
- **Periodo personalizzato.** Spuntando la casella compaiono le due date, precompilate con l'anno scelto.

### Mappa Principale

Ogni bidone compare una sola volta, nella posizione della sua ultima lettura nel periodo. I bidoni vicini sono raggruppati in cluster, che si aprono aumentando lo zoom.

I filtri agiscono su cantiere, comune, stato a database e servizio. Vengono applicati prima di scegliere l'ultima lettura. Le voci elencano solo i valori presenti nel periodo. Sotto i filtri, il riquadro delle statistiche si aggiorna a ogni modifica e riporta anche RFID e letture di ogni cantiere.

**Stato a database e servizi.** Lo stato a database toglie o rimette tutti i bidoni «Presente» o tutti i «Non Presente». I servizi stanno in due elenchi, uno per stato.

- **Servizio transponder**: riguarda i bidoni «Presente». È il servizio del database.
- **Servizio atteso**: riguarda i bidoni «Non Presente». È il servizio stimato dai giri dei mezzi che hanno letto il bidone, lo stesso che dà l'icona al marker.
- **Includi Non Censiti con stima incerta (?)**: riguarda i bidoni non censiti per cui nessuna tipologia di rifiuto raggiunge l'80% delle letture. Sono i marker con il punto di domanda.

Accanto al titolo di ogni elenco, «Tutti» e «Nessuno» spuntano o tolgono in un colpo tutte le voci: per vedere un solo servizio basta «Nessuno» e poi la voce che interessa. «Nessuno» sul servizio atteso toglie anche i non censiti con la stima incerta. Le statistiche contano con gli stessi criteri: i censiti per servizio transponder, i non censiti per servizio stimato, e a parte quelli con la stima incerta.

**Cantiere e comune.** I cantieri si spuntano come i servizi. La voce «Non assegnato» raccoglie le letture senza cantiere. I comuni si scelgono da un elenco, che propone quelli dei cantieri spuntati, divisi per cantiere: se ne possono scegliere più di uno, anche di cantieri diversi. Senza comuni scelti valgono tutti. Un comune scelto si toglie con la crocetta accanto al nome. Vale il comune assegnato alla lettura: quello del database, oppure quello del giro per i non censiti. Quando si cambiano cantieri o comuni la mappa si sposta sull'area scelta. I due filtri compaiono solo se il file ha le colonne geografiche.

**Con molti bidoni.** Fino a 10.000 bidoni la mappa li riceve tutti e li raggruppa in cluster. Oltre, per restare veloce, mostra una bolla per cantiere, poi una per comune, poi una per zona man mano che si ingrandisce, e i singoli bidoni quando quelli dell'area inquadrata tornano sotto il limite. Le bolle hanno lo stesso aspetto dei cluster: numero di bidoni e percentuale di censiti. Un click su una bolla ingrandisce sui suoi bidoni. L'intestazione della mappa dice quale vista è attiva.

### Ricerca RFID

Si inseriscono uno o più RFID, separati da virgola o a capo, e si preme «Cerca RFID». La mappa mostra tutte le letture dei bidoni cercati, senza cluster. L'ultima lettura ha il bordo spesso, le precedenti sono attenuate e tratteggiate. Un riquadro grigio racchiude tutte le posizioni dello stesso bidone. Se le letture trovate sono più di 3.000 la mappa disegna le più recenti: il riquadro copre comunque tutte le posizioni.

### Ricerca Utenza

Si inseriscono uno o più ID utenza. La mappa mostra l'ultima lettura dei bidoni la cui utenza **attuale** è tra quelle cercate. Un bidone passato da un'utenza a un'altra compare solo cercando la nuova.

Entrambe le ricerche ignorano la differenza tra maiuscole e minuscole.

### Leggere i marker

| Elemento | Significato |
|---|---|
| Marker verde a goccia | Bidone presente a database |
| Marker rosso squadrato | Bidone non presente a database |
| Icona nel marker, bidone censito | Servizio del transponder, vedi la tabella delle tipologie qui sotto |
| Icona nel marker, bidone non censito | Servizio stimato dai giri: quello prevalente, se almeno l'80% delle letture concorda sulla tipologia. Altrimenti punto di domanda: la stima è incerta. Il marker resta rosso |
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
| Sacchetti | sacco | `SACCHETTI`, assegnato dall'app | |
| Assistente servizi | operatore | | `ASSISTENTE SERVIZI` |
| Pulizia territorio | scopa | | `PULIZIA TERRIT.` |
| Servizi mercati | negozio | | `SERVIZI MERCATI` |

Un nome non previsto viene comunque caricato e mostrato con il punto di domanda.

**Sacchetti.** I sacchetti non hanno una tipologia di rifiuto a database. L'app li riconosce dal codice RFID, che ha 24 caratteri e inizia per `00BD`, mentre quello di un bidone ne ha 10, e li mette nella tipologia «Sacchetti», con la sua icona. Un sacchetto censito compare tra i servizi dei bidoni «Presente», uno non censito tra quelli dei «Non Presente»: per un sacchetto non viene stimato nessun servizio dai giri. Il dettaglio riporta i giri che lo hanno letto. Un sacchetto è monouso: viene letto una volta sola, quando il mezzo lo raccoglie. Per questo si vede sulla mappa e nelle ricerche, ma resta fuori dall'analisi dei cluster e dal report sul beneficio delle antenne, che riguardano i bidoni. Il dataset di esempio contiene 12 sacchetti.

**Aggiungere una tipologia o cambiare un'icona.** Si modifica solo la funzione `tipologie_servizio()` in `R/utils_styling.R`. Per un nuovo nome basta aggiungerlo, in maiuscolo, al vettore `servizi` della tipologia giusta. Per una nuova tipologia si aggiunge una voce con nome, icona [Font Awesome](https://fontawesome.com/icons), colore ed emoji. Marker, legenda, filtri e statistiche si aggiornano da soli.

### Dettaglio del bidone

Un click su un marker apre il popup con i dati della lettura e, nella barra laterale, l'analisi di tutta la storia del bidone:

- **censito con servizio coerente**: conferma della tipologia;
- **cambio di servizio**: cronologia dei passaggi, con il servizio attuale;
- **non censito**: stima percentuale del servizio, ricavata dai giri dei mezzi che lo hanno letto. Se il file non riporta nessun giro, l'indicazione che non ci sono elementi per stimarlo;
- **cambio di proprietario**: cronologia delle utenze, mostrata in aggiunta ai casi precedenti;
- **letture per anno**: un istogramma con una colonna per anno, dalla prima lettura del bidone all'ultimo anno dei dati. Non dipende dal periodo di analisi. Se sono state caricate le letture storiche, le colonne distinguono il sistema precedente dalle antenne e un anno senza letture resta vuoto, con lo zero in rosso.

Il popup riporta anche comune e cantiere, quando il file li contiene, e il comune di lettura se è diverso da quello del database.

### Analisi Cluster Spaziale

La quarta scheda raggruppa le letture di ogni bidone in luoghi distinti e classifica ogni luogo. Si preme «Genera Analisi» nella barra laterale: l'analisi riguarda tutte le letture del periodo, a prescindere dai filtri della mappa. Fanno eccezione i sacchetti, che restano fuori: il risultato è quello di un file che non li contiene. Se si cambia periodo o file il risultato viene scartato e va rigenerato.

Il risultato ha una riga per ogni cluster di ogni RFID e si esplora in tre viste:

- **Tabella**: tutte le colonne, ordinabili, con un filtro per colonna. I filtri della tabella valgono anche per le altre due viste.
- **Grafici**: numero di cluster per indicatore, e letture contro dispersione, con un riquadro per indicatore.
- **Mappa**: un punto sul baricentro di ogni cluster e un cerchio con il raggio della dispersione. Gli indicatori si accendono e spengono dal riquadro in alto a destra.

Un click su una riga o su un punto apre il dettaglio del bidone nella barra laterale. «Esporta CSV» salva l'intera tabella come `cluster_analysis_[dal]_[al].csv`.

Con decine di migliaia di cluster il grafico a dispersione e la mappa ne disegnano una parte: tutti gli anomali e un campione dei regolari. Un avviso dice quanti sono mostrati. La tabella e il CSV li contengono sempre tutti.

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

## Report sul beneficio delle antenne

[docs/beneficio_antenne.Rmd](docs/beneficio_antenne.Rmd) confronta le letture del sistema precedente con quelle delle antenne, sugli stessi contenitori. I sacchetti, se i file ne contengono, sono tolti prima di ogni conto. Gli stessi conti sono fatti a tre livelli.

- **Intero territorio.** Le letture anno per anno, vecchie e nuove insieme, e la tabella per RFID. Gli anni senza letture: quanti contenitori già in servizio non risultano mai letti in un anno intero. La stima del guadagno: letture per contenitore, quota delle raccolte previste che risulta letta, letture recuperate ogni anno, con un intervallo di confidenza.
- **Cantiere per cantiere.** Le letture utili di ogni cantiere anno per anno, e la tabella con anni vuoti, tasso di lettura prima e dopo, punti guadagnati e letture recuperate. L'ultima riga riporta l'intero territorio.
- **Comune per comune.** Il tasso di lettura di ogni comune prima e dopo, la mappa dei comuni e la tabella ordinabile. I comuni con meno di cinque contenitori confrontabili sono segnalati: lì il dato è indicativo.

In fondo, gli RFID letti in passato che le antenne non hanno ancora letto.

Senza parametri usa i due dataset di esempio. Sui dati reali si indicano i due file, con il percorso a partire dalla radice del progetto:

```r
rmarkdown::render("docs/beneficio_antenne.Rmd")   # dati di esempio

rmarkdown::render(
  "docs/beneficio_antenne.Rmd",
  params = list(
    letture = "data-raw/output/letture_app.csv",
    storico = "data-raw/output/letture_storiche.csv"
  ),
  output_dir = "data-raw/output"
)
```

L'HTML compilato sui dati reali contiene dati aziendali: `output_dir` lo scrive in una cartella esclusa da git. Con 2 milioni di letture e 2,5 milioni di letture storiche la compilazione richiede circa mezzo minuto. La tabella per RFID riporta al massimo 2.000 righe, quelle con più anni senza letture.

### Confini dei comuni

La mappa dei comuni usa i confini di `inst/extdata/comuni_confini.geojson`: sono quelli dell'ISTAT, presi dall'ultimo rilascio della raccolta [geojson-italy](https://github.com/guglielmo/geojson-italy), con licenza CC-BY 4.0. Il file ha un confine per ogni riga della tabella dei comuni, nello stesso ordine.

Per aggiornarli, o dopo aver aggiunto un comune alla tabella:

```sh
Rscript data-raw/prepara_comuni.R
```

Lo script scarica il file nazionale dei comuni e ne tiene quelli serviti. I nomi si confrontano senza badare a maiuscole, accenti e apostrofi: `ROSA'` trova `Rosà`. Se anche un solo comune non trova il suo confine, o se il centro abitato indicato in tabella cade fuori dal confine trovato, lo script si ferma senza scrivere niente e dice quali comuni controllare.

Le regole del confronto sono funzioni del pacchetto, in `R/utils_storico.R`, e hanno i loro test. Un contenitore conta dall'anno successivo a quello della sua prima lettura, così un contenitore nuovo non passa per un contenitore mai letto. Le letture utili di un contenitore non superano le raccolte previste, così un tag fermo in deposito non gonfia il risultato.

## Preparare i dati reali

Il CSV con le letture vere si produce dentro il repository, con gli script elencati qui sotto, da eseguire in ordine dalla radice del progetto.

| Passo | Script | Cosa fa |
|---|---|---|
| 1 | `data-raw/01_estrai_tabelle.R` | Estrae le tabelle dal database aziendale e le salva in `data-raw/origine/` |
| 2 | `data-raw/02_importa_tabelle.R` | Carica le tabelle ripulite da `data-raw/intermedi/`. Il codice commentato le ricostruisce dalle estrazioni |
| 3 | `data-raw/03_crea_letture_app.R` | Applica le regole di preparazione e scrive `data-raw/output/letture_app.csv`, da caricare nell'app |
| 4, facoltativo | `data-raw/04_crea_letture_storiche.R` | Legge le letture precedenti alle antenne da `data-raw/origine/letture_storiche.csv`, porta i codici RFID allo stesso formato del passo 3 e scrive `data-raw/output/letture_storiche.csv` |

Il passo 3 esegue da solo anche il passo 2. Le regole sono funzioni del pacchetto, in `R/fct_preparazione_letture.R`, e hanno i loro test. Il comune del database arriva dalla colonna `comune_servizio` dell'anagrafica dei contenitori. Il comune di lettura arriva dal calendario dei giri: il nome della sua colonna si indica nello script, con l'argomento `colonna_comune`. Il cantiere viene dalla tabella dei comuni del pacchetto.

Il passo 4 va adattato al file di origine: in cima allo script si indicano il nome del file e i nomi delle due colonne. Alla fine riporta quanti RFID delle antenne compaiono anche nelle letture storiche: se sono pochi, i codici dei due file hanno con ogni probabilità formati diversi.

Le tre cartelle dei dati sono escluse da git: i dati aziendali restano sul computer di chi li prepara. Il passo 1 richiede i pacchetti `odbc` e `DBI` e la sorgente dati ODBC configurata.

## File grandi

Un anno di letture con le antenne può arrivare a un paio di milioni di righe. L'app è costruita per restare veloce a quella scala.

**Caricare il file.** Tre modi, dal più semplice al più rapido.

- Dal browser, come un file piccolo. Un CSV di 2 milioni di righe pesa circa 300 MB.
- Dal browser, compresso. Lo stesso file in `.csv.gz` pesa circa un sesto, in Parquet circa un quinto, e arriva al server in meno tempo.
- Dal disco, senza passare dal browser: `run_app(letture = "percorso", storico = "percorso")`. Il file viene letto una volta sola e resta in memoria per tutte le sessioni dello stesso processo. È la scelta giusta quando l'app è usata da più persone.

**Tempi misurati** su 2 milioni di letture di 100.000 bidoni, su un portatile.

| Operazione | Tempo |
|---|---|
| Lettura e controllo del file | circa 5 s, 3 s da Parquet |
| Cambio di un filtro, con la mappa ridisegnata | da 0,3 a 0,5 s |
| Zoom o spostamento della mappa | da 0,3 a 0,5 s, compreso un quarto di secondo di attesa che il movimento finisca |
| Dettaglio di un bidone | 0,1 s |
| Ricerca di un RFID | 0,2 s |
| Analisi dei cluster | circa 2 s |

R usa circa 250 MB per le letture e arriva a 1,3 GB durante il caricamento e l'analisi dei cluster.

**Limiti regolabili** con `options()`, prima di avviare l'app.

| Opzione | Predefinito | Cosa regola |
|---|---|---|
| `trackerfid.max_marker` | 10.000 | Bidoni oltre i quali la mappa principale passa alle bolle |
| `trackerfid.max_letture_ricerca` | 3.000 | Letture disegnate sulla mappa dalla ricerca RFID |
| `trackerfid.max_upload_mb` | 2.048 | Dimensione massima di un file caricato dal browser |

Per ripetere le misure sul proprio computer: `Rscript dev/misura_prestazioni.R <file delle letture> [file storico]`. Per un file di prova sintetico delle dimensioni volute: `Rscript data-raw/genera_dataset_grande.R <cartella>`.

## Dataset di esempio

`inst/extdata/sample_rfid_dataset.csv` contiene 6.475 letture di 251 RFID, cioè 239 bidoni e 12 sacchetti, da ottobre 2024 a dicembre 2025, con cinque mezzi e 50 utenze. I bidoni stanno nel territorio servito, tra l'Altopiano di Asiago e i Colli Euganei, dentro i confini dei loro comuni. Il 2025 è completo, il 2024 parziale: serve a provare la selezione dell'anno. Ogni bidone viene letto a cadenza fissa dal giro del proprio servizio, come con un calendario di raccolta reale.

Come nei dati reali, ogni lettura ha il giro del mezzo che l'ha fatta e il comune di quel giro. I bidoni censiti hanno anche il comune del database, e il cantiere segue dal comune. I comuni sono 54, nei quattro cantieri. I bidoni non censiti sono 37: nel 2025, 18 hanno una stima netta del servizio e 19 una stima incerta, con il punto di domanda sulla mappa.

I sacchetti servono a vedere come l'app li tratta: nei dati reali, oggi, la preparazione dei dati li scarta. Sono 12: 8 censiti, di quattro utenze da `SAC001` a `SAC004`, e 4 non censiti. Un sacchetto è monouso, quindi ognuno ha una lettura sola: quella della raccolta. Hanno il codice di 24 caratteri che inizia per `00BD`, da `00BD00000000000000000001` in su i censiti e da `00BD00000000000000000101` i non censiti. Come nei dati reali, a database un sacchetto ha solo l'utenza: niente servizio, volume, raccolte previste o comune. Li leggono i giri del secco, nei quattro cantieri. Due sono letti solo nel 2024, gli altri dieci nel 2025.

`inst/extdata/sample_letture_storiche.csv` contiene le letture del sistema precedente alle antenne: 7.025 letture di 227 RFID, da gennaio 2020 a settembre 2024, con le sole colonne `RFID` e `giorno_lettura`. Il sistema precedente registra solo una parte dei passaggi, in misura diversa da cantiere a cantiere e da comune a comune, e lascia anni interi senza letture. Dei 227 RFID, 183 sono letti anche dalle antenne e 44 no: contenitori ritirati, oppure non ancora raggiunti. Altri 68 RFID sono letti solo dalle antenne: 56 bidoni e i 12 sacchetti.

Li genera entrambi lo script `data-raw/genera_sample_dataset.R`, che ha un seed fisso e ne verifica la conformità prima di scrivere i file. Nella stessa cartella, `cluster_analysis_2025-01-01_2025-12-31.csv` è l'analisi dei cluster del 2025, prodotta con lo script descritto sopra.

Casi utili da provare:

| RFID | Cosa mostra |
|---|---|
| `RFD20250901001` | Cambio di servizio: CARTA, poi SECCO, poi di nuovo CARTA |
| `RFD20250901050` | Cambio di utenza da `UTZ001` a `UTZ025` |
| `RFD20250915201` | Non censito con stima 100% SECCO PAP, quindi con icona del secco. Poche letture: `GHOST_TAG` |
| `RFD20250920250` | Non censito con stima incerta: due letture, una dal giro del secco e una da quello della carta. Sulla mappa ha il punto di domanda |
| `RFD20250905100` | Bidone spostato di 20 km, da Limena a Cittadella: `RELOCATED_BIN` con due cluster. Il popup mostra il comune del database e quello di lettura |
| `RFD20241001301` | Tag fermo in deposito, oltre 400 letture l'anno: `DEPOT_STUCK`. Non censito, letto dai giri di tutti i mezzi: stima incerta |
| `RFD20250203302` | Tag rimasto sul camion, letto lungo tutto il giro: `TRUCK_STOWAWAY` |
| `RFD20241003303` | GPS impreciso, 17 cluster: `SCATTERED_READS / GPS_NOISE` |
| `RFD20241004003` | Letto con regolarità dalle antenne, ma senza nessuna lettura nel 2021 e nel 2023: l'istogramma del dettaglio mostra i due anni vuoti |

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
  utils_storico.R               letture storiche: lettura, conteggi per anno, confronto con le antenne
  utils_geografia.R             comuni, cantieri e confini del territorio servito
  utils_gruppi.R                calcoli per gruppo senza cicli, per i dataset grandi
  utils_mappa_aggregata.R       vista della mappa principale quando i bidoni sono troppi
  generate_cluster_analysis.R   funzioni esportate per l'analisi senza app
  fct_preparazione_letture.R    regole che trasformano i dati aziendali nel CSV dell'app
  fct_confini_comuni.R          scarica i confini dei comuni e sceglie quelli serviti
inst/
  extdata/                      dataset di esempio, letture storiche di esempio, analisi dei cluster di esempio
  extdata/comuni_cantieri.csv   i 57 comuni serviti, con cantiere e coordinate del centro
  extdata/comuni_confini.geojson   confini dei comuni serviti: ISTAT, dalla raccolta geojson-italy
  scripts/generate_cluster_analysis.R   script eseguibile da riga di comando
  app/www/custom_style.css, script.js
  app/www/favicon.png           favicon: il simbolo aziendale
data-raw/
  genera_sample_dataset.R       dataset di esempio, simulati
  genera_dataset_grande.R       dataset sintetico di milioni di letture, per misurare i tempi
  prepara_comuni.R              scarica i confini aggiornati dei comuni serviti
  01_estrai_tabelle.R, 02_importa_tabelle.R, 03_crea_letture_app.R   dati reali, tre passi
  04_crea_letture_storiche.R    dati reali: letture precedenti alle antenne, facoltativo
  origine/, intermedi/, output/ dati aziendali, esclusi da git
dev/
  misura_prestazioni.R          tempi e memoria di ogni passaggio dell'app su un file
docs/
  analisi_cluster.md            analisi dei cluster: regole, colonne, scelte
  indice_fiducia.Rmd            report sull'indice di fiducia, da compilare in HTML
  beneficio_antenne.Rmd         report sul beneficio delle antenne, da compilare in HTML
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
- **Filtri dei servizi.** Deselezionando tutte le voci di un elenco spariscono i bidoni di quello stato. Lo pseudocodice della specifica in quel caso disattivava il filtro.

Specifica delle correzioni e dell'analisi dei cluster:

- **Dataset di esempio su base annuale.** La prima versione copriva 30 giorni, con una o due letture per la maggior parte dei bidoni. Raccolte annue e soglie dell'analisi dei cluster non erano dimostrabili su quei dati. I casi speciali della prima versione sono rimasti. Il file ha mantenuto il nome `sample_rfid_dataset.csv`.
- **Periodo valido per tutta l'app.** La selezione dell'anno e il periodo personalizzato stanno in un solo controllo nella barra laterale, usato anche dall'analisi dei cluster.
- **Tre colonne in più nell'output.** La specifica parlava di 25 campi e ne definiva 22. Sono stati aggiunti `presente_a_database`, `servizio_transponder_cronologia` e `servizio_atteso_dettaglio`.
- **Classificazione.** Tre correzioni alle regole, descritte in [docs/analisi_cluster.md](docs/analisi_cluster.md).
- **Distanze per DBSCAN.** Proiezione locale in metri al posto della matrice di Haversine, per reggere tag con migliaia di letture. I cluster coincidono.
- **Script autonomo.** Le funzioni stanno nel pacchetto, lo script in `inst/scripts` le richiama. Un file con codice duplicato sarebbe rimasto disallineato alla prima modifica.

Specifica su geografia e prestazioni:

- **Nell'app solo i filtri.** Le analisi per territorio, cantiere e comune stanno nel report sul beneficio delle antenne, non in schede dell'app.
- **Comuni e cantieri.** Il file dei comuni ne elenca 57 in quattro cantieri: il testo della specifica parlava di 55 comuni e cinque cantieri. Le coordinate indicate per i cantieri erano approssimative: quelle usate sono i centri dei comuni, da fonti aperte.
- **Colonne geografiche.** Sono `comune_da_database`, `comune_lettura` e `cantiere`. Il comune assegnato a una lettura lo calcola l'app.
- **Dataset di esempio.** Copre un anno e tre mesi, con 6.475 letture: le analisi annuali richiedono un anno completo. I bidoni sono stati spostati nel territorio servito e sono stati aggiunti 12 sacchetti, il resto non è cambiato.
- **Mappa con molti bidoni.** Al posto del campionamento dei marker o di una mappa di calore, il server raggruppa i bidoni per cantiere, comune o zona secondo lo zoom. Nessun bidone viene escluso dai conteggi, e il browser riceve poche centinaia di elementi.
- **Niente database.** Le letture restano in memoria: su 2 milioni di righe ogni filtro richiede decimi di secondo, quindi un database o un indice spaziale non darebbero vantaggi.

Correzioni alla logica dei non censiti:

- **Giro su ogni lettura.** `servizio_atteso` e `comune_lettura` ci sono per ogni lettura, non solo per i non censiti: così è nei dati reali, e così è ora il dataset di esempio. La prima specifica li voleva vuoti per i censiti.
- **Un elenco di servizi per stato.** Il servizio transponder filtra i soli bidoni «Presente», il servizio atteso i soli «Non Presente». La casella «Includi Non Censiti» è stata tolta: era un doppione dello stato «Non Presente».
- **Servizio atteso come stima del bidone.** Il filtro non guarda più il giro della singola lettura, ma il servizio stimato del bidone, lo stesso dell'icona. Un bidone non censito sta quindi sotto una voce sola: il suo servizio stimato, oppure la stima incerta.
- **Tipologia dei sacchetti.** I sacchetti censiti arrivavano senza servizio, e sulla mappa avevano il punto di domanda. Ora hanno una tipologia e un'icona loro, riconosciuti dal codice RFID.
- **Stima incerta al posto di «senza stima».** La casella riguardava i non censiti senza nessun giro, che nei dati reali non esistono. Ora riguarda quelli sotto l'80%, cioè i marker con il punto di domanda.

## Da completare

Il campo `License` di `DESCRIPTION` contiene ancora il segnaposto di golem. Finché non viene scelto, `devtools::check()` riporta un avviso.
