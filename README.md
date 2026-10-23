# trackerfid — Dashboard RFID Bidoni Rifiuti

Applicazione Shiny, costruita con il framework [golem](https://thinkr-open.github.io/golem/), per visualizzare su mappa le letture RFID dei bidoni per rifiuti raccolte dai mezzi aziendali.

Permette di:

- distinguere a colpo d'occhio i bidoni censiti nel database aziendale da quelli non censiti;
- vedere se la tipologia di rifiuto o l'utenza di un bidone sono cambiate nel tempo;
- seguire gli spostamenti di un singolo bidone;
- filtrare i bidoni per cantiere e per comune, per servizio atteso e per giro del calendario dei mezzi;
- trovare le letture in cui il servizio del contenitore, quello atteso e il giro non sono dello stesso rifiuto;
- cercare i bidoni per RFID o per utenza proprietaria;
- analizzare per ogni bidone i luoghi in cui è stato letto nel periodo, per riconoscere spostamenti, tag smarriti o rimasti sui mezzi.

L'interfaccia è interamente in italiano.

## Avvio rapido

Servono R 4.1 o successivo e i pacchetti elencati in `DESCRIPTION`. Le letture stanno in un database [DuckDB](https://duckdb.org) in memoria, con il pacchetto `duckdb`; le mappe sono disegnate in WebGL da [MapLibre GL JS](https://maplibre.org), con il pacchetto `mapgl`. `mapgl` porta con sé `sf` e `terra`: su macOS e Windows i pacchetti binari di CRAN bastano, su Linux servono le librerie di sistema GDAL, GEOS e PROJ.

```r
# dalla cartella del progetto
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)

devtools::load_all()
run_app()             # si parte da un'app vuota e si carica un file
run_app(demo = TRUE)  # si parte con i dati di esempio già caricati
run_app(letture = "letture_app.duckdb")   # si parte con un file letto dal disco: vedi «File grandi»
```

In sviluppo si può usare anche `golem::run_dev()`.
Per installare il pacchetto: `devtools::install()`, poi `trackerfid::run_app()`.

## Formato del file

Il file è quello che scrive la pipeline della [preparazione dei dati reali](#preparare-i-dati-reali). Deve contenere 9 campi obbligatori e può contenerne altri facoltativi, segnati nella tabella. L'ordine delle colonne e le maiuscole nei nomi non contano. Le colonne della pipeline che l'app non usa vengono ignorate, perché il caricamento sia il più veloce possibile: `data_lettura`, `id_giro`, `frequenza`, `data_attivazione_contenitore`, `data_cessazione_contenitore`, `id_fornitura`, `stato_fornitura` e le sette della fornitura: nucleo, categoria e potenziamento del secco. Le legge il report sui conferimenti, dal CSV, con `carica_dataset(percorso, colonne = "tutte")`.

| Campo | Tipo | Contenuto |
|---|---|---|
| `data_ora_lettura` | data e ora | Momento della lettura, `YYYY-MM-DD HH:MM:SS` |
| `targa_veicolo` | testo | Targa del mezzo che ha letto il chip |
| `matricola_veicolo` | testo | Matricola aziendale del mezzo, in relazione 1:1 con la targa |
| `RFID` | testo | Identificativo del chip, ripetuto a ogni lettura |
| `presente_a_database` | testo | `Presente` oppure `Non Presente` |
| `servizio_transponder` | testo | Facoltativo. Servizio del contenitore secondo il database (`SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE`), vuoto per i non censiti |
| `servizio_atteso` | testo | Servizio che il giro del mezzo lasciava aspettare, con il nome del database: il giro `SECCO PAP` dà `SECCO`, vedi [la tabella di conversione](#tipologie-di-servizio-e-icone). Nei giri in cui non è chiaro quale rifiuto si raccolga, come `SERVIZI MERCATI`, è il servizio a database più frequente tra i censiti letti nel giro. C'è per ogni lettura |
| `servizio_giro` | testo | Facoltativo. Giro che il calendario dava al mezzo, scritto come nel calendario, senza conversioni: `SECCO PAP`, non `SECCO`. Se manca vale `servizio_atteso` |
| `coerenza_servizio` | testo | Facoltativo. `SI` se il servizio a database e quello atteso sono dello stesso rifiuto, `NO` altrimenti; vuoto se uno dei due manca, come per ogni non censito. L'app lo ricalcola sempre, anche se il file lo contiene già |
| `id_utenza` | testo | Utenza proprietaria, vuoto se non censito |
| `latitudine` | numero | Latitudine in gradi decimali |
| `longitudine` | numero | Longitudine in gradi decimali |
| `volume` | numero | Facoltativo. Volume del contenitore in litri secondo il database, vuoto se non censito. Vale anche il nome `volume_previsto` |
| `numero_raccolte_annue` | numero | Facoltativo. Raccolte previste in un anno, vuoto se non censito. Vale anche il nome `numero_raccolte_annue_previste` |
| `comune_fornitura` | testo | Facoltativo. Comune della fornitura a cui è legato il contenitore, secondo il database; vuoto se non censito. Valgono anche i nomi `comune_da_database`, dei file preparati fino al 17 ottobre 2026, e `comune` |
| `comune_giro` | testo | Facoltativo. Comune in cui il calendario mandava il mezzo che ha fatto la lettura. C'è per ogni lettura |
| `comune_lettura` | testo | Facoltativo. Comune in cui è stata fatta la lettura: nei dati reali la pipeline lo ricava dalla posizione dentro i comuni serviti, e fuori lascia quello del sistema di localizzazione dei mezzi. Se manca, la lettura non ha un comune: l'app non lo ricava dalle coordinate |
| `coerenza_comune` | testo | Facoltativo. Il comune di lettura a confronto con quello dichiarato: `Coerente`, `Fuori comune`, `Fuori giro` oppure `Coerenza ignota`, vedi [Comune di lettura](#formato-del-file). L'app lo ricalcola sempre, anche se il file lo contiene già |
| `distanza_comune_dichiarato` | numero | Facoltativo. Per una lettura fatta in un comune diverso da quello dichiarato, i metri dal confine del comune dichiarato, zero se il punto ci cade dentro. Lo scrive la pipeline di preparazione |
| `eseguito` | testo | Facoltativo. Conferma del giro nel calendario: `SI`, `NO` se non ancora confermato, oppure vuoto |
| `cantiere` | testo | Facoltativo. Cantiere del comune di lettura; fuori dai comuni serviti, quello del comune del database |

**Giro e comune del giro.** Una lettura esiste solo se un mezzo passa durante un giro, e dal calendario dei mezzi si ricavano il servizio del giro e il suo comune. Per questo `servizio_giro`, `servizio_atteso` e `comune_giro` sono compilati per quasi ogni lettura, anche per i bidoni censiti. La preparazione dei dati unisce i giri pianificati e quelli non pianificati (questi ultimi senza orari) e cerca sempre un giro dello stesso mezzo e dello stesso giorno, con una serie di regole dalla più sicura alla meno sicura: prima l'orario e il comune dei pianificati, poi il comune dei non pianificati, poi il servizio se il mezzo ne ha uno solo, poi le letture di seguito dello stesso mezzo, poi il comune più vicino e infine un pianificato a meno di un'ora. Una lettura che nessuna regola associa resta senza giro, e un messaggio della pipeline dice quante sono per ogni regola. Le regole sono spiegate con diagrammi in `docs/associazione_giri.qmd` (documento Quarto, si compila con `quarto::quarto_render()`, non con la pipeline). `servizio_giro` è il giro scelto, con il nome del calendario: resta `SECCO PAP`, perché il secco porta a porta va distinto dagli altri giri del secco. Il calendario dice però che cosa era previsto, non che cosa è successo: un mezzo può sostituirne un altro e fare un giro che non era il suo.

**Tre servizi per ogni lettura.** `servizio_transponder` è il servizio del contenitore secondo il database: c'è solo per i censiti. `servizio_atteso` è il servizio che il giro lasciava aspettare, con il nome del database, e c'è per tutti. `servizio_giro` è il giro com'è scritto nel calendario. Per un bidone non censito il servizio si stima dal servizio atteso delle sue letture: vedi [la mappa principale](#mappa-principale).

**Giri dei servizi.** Nei giri `SERVIZI MERCATI` e `SERV.MANIFESTAZIONE` non è chiaro quale rifiuto si raccolga. Un mezzo che non è uno scarrabile ne raccoglie però uno solo alla volta: il servizio atteso di quelle letture è il servizio a database più frequente tra i censiti letti nello stesso giro, nello stesso giorno. Se nessun censito ha un servizio, o due servizi sono ugualmente frequenti, resta il giro, con il nome della tipologia: `SERVIZI`. L'elenco di questi giri sta in `giri_servizio_dai_censiti()`.

**Coerenza.** Il campo `coerenza_servizio` dice se il servizio a database e il servizio atteso di una lettura sono dello stesso rifiuto. Si confrontano le tipologie, non i nomi. Un contenitore della carta letto in un giro del secco non è coerente: è stato letto in mezzo a contenitori di un altro rifiuto, oppure il mezzo non stava facendo il giro del calendario. Un non censito non ha un servizio a database, quindi la sua coerenza manca. I giri che non raccolgono un rifiuto, come `PULIZIA TERRIT.`, fanno tipologia a sé: un contenitore censito letto in uno di quei giri risulta non coerente.

**Comune di lettura.** Il comune in cui una lettura è stata fatta arriva nel file. La preparazione dei dati reali lo ricava dalla posizione per ogni lettura che cade in un comune servito, con i confini della Regione; fuori dai comuni serviti lascia quello del sistema di localizzazione dei mezzi. L'app non lo ricalcola dalle coordinate, e una lettura senza comune non ne ha uno. Un comune di lettura che la tabella dei comuni non contiene conta come fuori dai comuni serviti, e l'app lo segnala con un avviso. Per un bidone non censito il comune è quello in cui viene letto più spesso. Per un bidone censito vale il comune del database.

**Coerenza del comune.** Il campo `coerenza_comune` confronta il comune di lettura con il comune dichiarato. Per un bidone censito il comune dichiarato è quello del database: se il comune di lettura è diverso la lettura è `Fuori comune`, di solito un bidone spostato. Per un non censito è il comune del giro: se il comune di lettura è diverso la lettura è `Fuori giro`, di solito un mezzo che non faceva il giro del calendario. Se i due comuni coincidono la lettura è `Coerente`. Se manca uno dei due la coerenza è `Coerenza ignota`: succede ai sacchetti censiti, che a database non hanno un comune, e alle letture senza comune di lettura. I nomi si confrontano senza badare a maiuscole, accenti e apostrofi. Vicino al confine il sistema di localizzazione dà spesso il comune accanto: una lettura fatta in un altro comune, ma entro 100 metri dal confine del comune dichiarato, è coerente. La distanza la calcola la pipeline di preparazione, nel campo `distanza_comune_dichiarato`; un file senza quel campo confronta i soli nomi. Un non censito letto in un giro il cui comune non è un comune servito, come una zona del calendario («Massiccio del Grappa»), ha la coerenza ignota: non c'è un confine con cui confrontare.

**Comuni e cantieri.** La tabella `inst/extdata/comuni_cantieri.csv` elenca i 66 comuni serviti e il cantiere di ognuno: `ASIAGO`, `BASSANO`, `CAMPOSAMPIERO`, `RUBANO`. I nomi sono scritti come nei dati aziendali, con l'accento: `ROSÀ`. I nomi dei comuni del file sono riportati alla grafia della tabella, senza badare a maiuscole, accenti e apostrofi. Il comune usato da filtri, bolle e statistiche è il comune di lettura, se è tra i comuni serviti; altrimenti quello del database, per i censiti. Un non censito letto fuori dai comuni serviti non ha un comune né un cantiere: nei filtri sta sotto «Non assegnato». I report e la tabella dei cluster danno a ogni bidone un comune solo: quello del database per un censito, quello in cui è letto più spesso per un non censito. Il cantiere viene ricalcolato dal comune secondo la tabella: quello scritto nel file vale solo per i comuni che la tabella non contiene, segnalati con un avviso. La colonna `comune` dei file meno recenti vale come `comune_fornitura`.

Cosa viene accettato:

- file CSV, CSV compresso con gzip (`.csv.gz`), Parquet (`.parquet`) oppure l'archivio DuckDB (`.duckdb`) che scrive la pipeline di preparazione dei dati, già controllato: si apre in un istante, vedi [File grandi](#file-grandi);
- separatore virgola o punto e virgola, rilevato automaticamente;
- decimali con il punto o con la virgola;
- date anche senza secondi o in formato italiano (`DD/MM/YYYY HH:MM:SS`);
- valori mancanti scritti come `NA` o lasciati vuoti;
- file fino a 2 GB dal browser. Per i file con milioni di letture vedi [File grandi](#file-grandi).

RFID e utenze sono sempre letti come testo, quindi gli zeri iniziali si conservano. Gli orari sono trattati come ora locale, senza conversioni di fuso.

### Validazione

Al caricamento l'app controlla il file e mostra l'esito sotto il campo di caricamento. Il controllo lo fa DuckDB, sul file appena letto. Un file `.duckdb` è già stato controllato quando è stato scritto: l'app ne mostra gli avvisi di allora.

- **Errori, il file viene rifiutato**: colonne mancanti, date non riconoscibili, coordinate non numeriche o fuori scala, valori di `presente_a_database` diversi dai due ammessi. Il messaggio indica il campo e le righe del file coinvolte.
- **Avvisi, il file viene caricato**: righe prive di un campo essenziale (data, RFID, stato, coordinate), che vengono scartate; letture "Non Presente" che riportano comunque `servizio_transponder` o un'utenza; comuni di lettura, del database o dei giri che la tabella dei comuni non contiene; relazione targa/matricola non univoca; colonne facoltative assenti, nel qual caso le informazioni che ne dipendono restano vuote.

## Come si usa

La barra verde in alto riporta il titolo e, a destra, il periodo di analisi. La barra laterale a sinistra, richiudibile dal pulsante in alto, contiene il caricamento del file delle letture, il dettaglio del bidone selezionato e i controlli della scheda aperta. La sezione del caricamento si chiude con un click sul titolo, una volta caricati i dati: chiusa occupa una riga e riporta il nome del file. I filtri della mappa principale stanno invece sopra la mappa.

### Periodo di analisi

Tutta l'app lavora sulle letture di un periodo: mappe, ricerche, statistiche, dettaglio del bidone e analisi dei cluster. Per questo il periodo si sceglie nella barra verde in alto, all'estremità destra, quando un file è caricato. Se l'intervallo non è valido, o il periodo non ha letture, un avviso compare accanto alle date. Il periodo compare anche nel titolo, ad esempio «DASHBOARD RFID - ANNO 2025».

Il periodo si sceglie con due date, dal / al. All'apertura vale l'anno più recente del file, dal 1 gennaio al 31 dicembre; il calendario propone i giorni dal 1 gennaio dell'anno più vecchio al 31 dicembre del più recente. Un anno intero compare nel titolo come «ANNO 2025», un altro intervallo con le sue date.

### Mappa Principale

Ogni bidone compare una sola volta, nella posizione della sua ultima lettura nel periodo. Sopra la mappa, accanto al badge della visualizzazione, si legge quante letture superano i filtri e di quanti RFID, ad esempio «5.360 letture di 248 RFID (ultima lettura registrata)», e che cosa si vede allo zoom attuale. Quello che si vede dipende dallo zoom.

- **Da lontano**, fino a zoom 10: una bolla per cantiere, con il nome.
- **Avvicinandosi**, da zoom 11 a zoom 13: una bolla per comune, nel baricentro del comune. La bolla resta ferma al variare dei filtri.
- **Da vicino**, da zoom 14: i singoli bidoni, nella loro posizione esatta. Quelli vicini restano raggruppati in un cluster, che si apre con un click o ingrandendo ancora. Un cluster di bidoni che stanno nello stesso punto non si apre più: un click mostra l'elenco dei suoi bidoni, e un click su un nome apre il dettaglio.

Gli zoom sono quelli di MapLibre, che disegna la mappa a tessere di 512 pixel: lo zoom 10 di MapLibre ha la scala dello zoom 11 della mappa precedente, Leaflet. Le fasce, alla stessa scala sullo schermo, sono quelle di prima.

Una bolla riporta il numero dei suoi bidoni e la percentuale di censiti, con gli stessi colori dei cluster. Un click su una bolla la apre: da un cantiere si passa ai suoi comuni, da un comune ai suoi bidoni. Se i bidoni della bolla entrano nella mappa a uno zoom che non basta, la mappa si porta sulla bolla allo zoom del livello successivo. L'intestazione della mappa dice quale vista è attiva. Passando con il mouse su una bolla, un cluster o un bidone compaiono i suoi numeri o il suo RFID.

**Mappa WebGL.** La mappa è disegnata dalla scheda grafica, con MapLibre GL JS, su uno sfondo vettoriale di [OpenFreeMap](https://openfreemap.org), lo stile «bright», con i colori della mappa di OpenStreetMap: serve la connessione a internet. Un altro sfondo si sceglie con l'opzione `trackerfid.stile_mappa`, ad esempio `options(trackerfid.stile_mappa = "https://tiles.openfreemap.org/styles/liberty")`.

**Legenda.** Sta in un riquadro sopra la mappa, in basso a destra. A ogni avvio dell'app è chiusa: è un piccolo pulsante nell'angolo, «Legenda», che si apre con un click e con un altro si richiude. La scelta vale per tutte le mappe. A ogni cambio dei filtri il browser riceve tutti i bidoni e le bolle di cantieri e comuni, e da lì zoom, spostamenti, cluster e passaggio da una fascia all'altra restano nel browser, senza chiedere niente al server. Le bolle arrivano prima dei bidoni: da lontano si aggiornano subito, mentre il browser raggruppa i bidoni.

**Confini dei comuni.** Tutte le mappe disegnano i confini dei comuni serviti, con un bordo tratteggiato e un riempimento appena accennato nel colore del cantiere. La legenda riporta il colore di ogni cantiere. I confini non rispondono al mouse: click e passaggi arrivano alla mappa e ai marker.

**Filtri.** Sopra la mappa, una riga di menu a discesa: Cantiere, Comune, Coerenza comune, Stato database, Servizio, Giro, Coerenza servizio e il pulsante Reset. Un click su un menu apre le sue caselle, che restano visibili mentre si spuntano; un click fuori lo chiude. Quando qualche voce di un menu è tolta, il pulsante diventa verde e dice quante voci restano, ad esempio «Servizio 7/8». I filtri vengono applicati prima di scegliere l'ultima lettura. Le voci elencano solo i valori presenti nel periodo. Il riquadro delle statistiche, nella barra laterale, si aggiorna a ogni modifica e riporta anche RFID e letture di ogni cantiere.

**Stato a database e servizi.** Lo stato a database toglie o rimette tutti i bidoni «Presente» o tutti i «Non Presente», e vale insieme a tutti gli altri filtri: con ogni elenco si possono guardare i soli censiti, i soli non censiti, o tutti. I servizi stanno in due menu.

- **Servizio**: il servizio del contenitore, lo stesso che dà l'icona al marker. Per un bidone censito è quello del database, `servizio_transponder`; per un bidone non censito è il servizio stimato dal servizio atteso delle sue letture. Per vedere i contenitori della carta basta la voce `CARTA`; per i soli censiti, in più, lo stato «Presente».
- **Non censiti con stima incerta (?)**: nello stesso menu, riguarda i bidoni non censiti per cui nessuna tipologia di rifiuto raggiunge l'80% delle letture. Sono i marker con il punto di domanda.
- **Giro**: il giro del calendario dei mezzi, con il suo nome. Per vedere i soli bidoni letti dai giri del secco porta a porta basta «Nessuno» e poi la voce `SECCO PAP`.

In cima a ogni menu dei servizi, «Tutti» e «Nessuno» spuntano o tolgono in un colpo tutte le voci: per vedere un solo servizio basta «Nessuno» e poi la voce che interessa. «Nessuno» sul servizio toglie anche i non censiti con la stima incerta. Le statistiche contano con gli stessi criteri: i censiti per servizio a database, i non censiti per servizio stimato e a parte quelli con la stima incerta, poi i bidoni per giro.

**Coerenza del servizio.** Il menu «Coerenza servizio» ha due voci, «Coerente» e «Non coerente»: per vedere i soli bidoni con letture non coerenti si toglie la prima. Le letture di cui la coerenza non si può dire, come quelle dei non censiti, stanno con quelle coerenti. Le statistiche contano i bidoni non coerenti. Il popup di una lettura non coerente riporta il servizio a database e quello atteso, ad esempio «a database CARTA, atteso SECCO», e il dettaglio del bidone dice quante letture non sono coerenti e quali sono le più recenti. La regola è descritta in [Formato del file](#formato-del-file).

**Cantiere e comune.** I cantieri si spuntano come i servizi. La voce «Non assegnato» raccoglie le letture senza cantiere. I comuni si scelgono da un elenco a discesa nella stessa riga, che propone quelli dei cantieri spuntati, divisi per cantiere: se ne possono scegliere più di uno, anche di cantieri diversi. Senza comuni scelti valgono tutti. Un comune scelto si toglie con la crocetta accanto al nome. Vale il comune in cui è stata fatta la lettura. Quando si cambiano cantieri o comuni la mappa si sposta sull'area scelta. I due filtri compaiono solo se il file ha le colonne geografiche.

**Con molti bidoni.** La mappa riceve sempre tutti i bidoni: con 100.000 bidoni il messaggio al browser pesa da 3 a 5 MB, e il browser li raggruppa in circa mezzo secondo. Non c'è più un limite oltre il quale la mappa passa a bolle per zona.

**Coerenza del comune.** Il menu «Coerenza comune» segue il campo `coerenza_comune` e ha quattro voci: «Coerente», «Fuori comune», «Fuori giro» e «Coerenza ignota».

- **Fuori comune**: un censito letto in un comune diverso da quello del database. Di solito è un bidone spostato.
- **Fuori giro**: un non censito letto in un comune diverso da quello del giro. Di solito il mezzo non stava facendo il giro del calendario. Un censito non è mai fuori giro: il suo comune si confronta con quello del database.

Per vedere le sole letture fuori giro si tolgono le altre tre voci. Le statistiche contano i bidoni fuori comune, fuori giro e di coerenza ignota. Sul marker le due incongruenze compaiono come bollini scuri con una lettera, `C` e `G`, e il popup dice dove era dichiarata la lettura e dove è stata fatta.

### Ricerca RFID

Si inseriscono uno o più RFID, separati da virgola o a capo, e si preme «Cerca RFID».

- **Mappa.** Le ultime 10 letture di ogni RFID cercato, senza cluster: un bidone letto centinaia di volte nello stesso punto avrebbe centinaia di marker uno sopra l'altro. L'ultima lettura ha il bordo spesso e sta sopra le altre, le precedenti sono attenuate e tratteggiate. Un riquadro grigio racchiude tutte le posizioni dello stesso bidone nel periodo, anche quelle che la mappa non disegna.
- **Tabella delle letture.** Sopra la mappa, in basso a sinistra, in un riquadro come quello della legenda che si chiude con un click sull'intestazione. Riporta tutte le letture del periodo degli RFID cercati, raggruppate per RFID, dalla più recente: data e ora, mezzo e servizio del giro. La riga di ogni RFID dice quante letture ha, il primo e l'ultimo giorno, e con quali mezzi e in quali giri è stato letto; un click sulla freccia mostra le letture. Con un solo RFID cercato sono già in vista.

### Ricerca Giro

Mostra che cosa ha letto un mezzo in un giorno, e dove. Nella barra laterale si scelgono il mezzo, oppure tutti, e un giorno o un intervallo di giorni; all'apertura la ricerca è sull'ultimo giorno con letture. Due caselle restringono la ricerca alle sole letture fuori giro o fuori comune, secondo il campo `coerenza_comune`.

- **Tabella dei giri.** Sta sopra la mappa, in basso a sinistra, in un riquadro come quello della legenda che si chiude con un click sull'intestazione. Una riga per giorno, mezzo, giro previsto e comune del giro: che cosa doveva fare il mezzo secondo il calendario, se il giro è stato confermato, a che ora ha letto, quante letture e quanti bidoni, quante letture fuori giro e fuori comune, e in quale comune sono state fatte davvero le letture. I giri con più letture fuori giro stanno in cima. Sotto i titoli ogni colonna ha un filtro: i testi cercano una parte del valore, senza badare alle maiuscole, ad esempio `1601` nel mezzo o `lim` nel comune; la data si scrive come si vede, `24/09`; nei numeri si scrive un valore oppure un confronto, `>0`, `<5`, `>=2`. Un click su una riga mostra sulla mappa le letture di quel giro; un secondo click toglie la scelta. La colonna «Confermato» riporta il campo `eseguito` del calendario: `SI`, `NO` in arancio, o un trattino senza l'informazione. Un giro `NO` non vuol dire che non sia stato fatto: di solito viene confermato nei giorni seguenti.
- **Mappa.** Le letture del giro scelto nella tabella, una per marker, senza cluster. Finché non si sceglie un giro la mappa non ha marker e inquadra la zona di tutti i giri trovati: così una ricerca su molti giorni o su tutti i mezzi non la riempie di letture. Il confine del comune in cui il giro doveva lavorare è messo in evidenza con un contorno scuro, e la mappa inquadra insieme letture e comune: se il mezzo ha letto altrove si vede subito. Oltre 50.000 letture la mappa disegna per prime quelle fuori giro o fuori comune.

Per trovare i giri sbagliati di un periodo si confrontano, nella tabella, il comune del giro e quello in cui sono state fatte le letture. La casella «Fuori giro» trova i non censiti letti fuori dal comune del giro: i censiti si confrontano con il comune del database, e un mezzo che ha fatto un giro diverso dal previsto tra contenitori censiti non ha letture fuori giro. Un click su un marker apre il dettaglio del bidone.

### Ricerca Utenza

Si inseriscono uno o più ID utenza. La mappa mostra l'ultima lettura dei bidoni la cui utenza **attuale** è tra quelle cercate. Un bidone passato da un'utenza a un'altra compare solo cercando la nuova.

Entrambe le ricerche ignorano la differenza tra maiuscole e minuscole.

### Leggere i marker

| Elemento | Significato |
|---|---|
| Marker verde a goccia | Bidone presente a database |
| Marker rosso squadrato | Bidone non presente a database |
| Icona nel marker, bidone censito | Servizio del contenitore a database, vedi la tabella delle tipologie qui sotto. Non cambia con il giro che lo legge |
| Icona nel marker, bidone non censito | Servizio stimato dai giri: quello prevalente, se almeno l'80% delle letture concorda sulla tipologia. Altrimenti punto di domanda: la stima è incerta. Il marker resta rosso |
| Colore del cluster | Percentuale di censiti: rosso 0%, giallo 50%, verde 100% |
| Numeri nel cluster | Bidoni raggruppati e percentuale di censiti |
| Bollino scuro con `C` | Lettura fuori comune: censito letto in un comune diverso da quello del database |
| Bollino scuro con `G` | Lettura fuori giro: non censito letto in un comune diverso da quello del giro del mezzo |

Lo stato è indicato sia dal colore sia dalla forma, così resta leggibile anche per chi non distingue rosso e verde.

Per i non censiti la quota dell'80% si calcola per tipologia, su tutte le letture del periodo.

### Tipologie di servizio e icone

Il calendario dei mezzi chiama i giri con nomi suoi (PAP = porta a porta), diversi da quelli del database. La tabella qui sotto è la **tabella di conversione**: con questa la preparazione dei dati ricava dal giro il servizio atteso, con il nome della sua tipologia. Il giro resta in `servizio_giro` con il nome del calendario, senza conversioni. La stessa tabella serve alla coerenza, che confronta le tipologie.

| Tipologia | Icona | Nome uniforme | Nomi convertiti |
|---|---|---|---|
| Secco | bidone | `SECCO` | `SECCO PAP`, `SCARICO SECCO`, `SECCO CONT.STRADALI` |
| Carta | foglio | `CARTA` | `CARTA CONT.STRADALI`, `CARTA/CARTONE PAP`, `SCARICO CARTA-CART.` |
| Vetro | bottiglia | `VETRO` | `VETRO PAP`, `VETRO CONT.STRADALI` |
| Umido | foglia | `UMIDO` | `UMIDO CONT.STRADALI`, `UMIDO PAP` |
| Plastica e metalli | riciclo | `PLASTICA E METALLI` | `PLAST.CONT.STRADALI`, `PLASTICA PAP`, `SCARICO PLASTICA` |
| Verde e ramaglie | albero | `VERDE E RAMAGLIE` | `VERDE PAP`, `SCARICO VERDE` |
| Sacchetti | sacco | `SACCHETTI`, assegnato dall'app | |
| Pulizia territorio | scopa | `PULIZIA TERRITORIO` | `PULIZIA TERRIT.` |
| Servizi | negozio | `SERVIZI` | `SERVIZI MERCATI`, `SERV.MANIFESTAZIONE` |

La tabella sta in un punto solo, `tipologie_servizio()` in `R/utils_styling.R`: il nome uniforme è il nome della tipologia, in maiuscolo; per le sei tipologie di rifiuto è anche il nome del database. La restituisce `conversione_servizi()` e la applica `uniforma_servizio()`. Un nome del calendario che la tabella non contiene resta com'è, e la preparazione dei dati lo segnala con un avviso: va aggiunto all'elenco della tipologia giusta. L'app riconosce i nomi del calendario e dà loro l'icona della tipologia.

Un nome non previsto viene comunque caricato e mostrato con il punto di domanda. È il caso di `ASSISTENTE SERVIZI`, che non ha più una tipologia.

**Sacchetti.** I sacchetti non hanno una tipologia di rifiuto a database. L'app li riconosce dal codice RFID, che ha 24 caratteri e inizia per `00BD`, mentre quello di un bidone ne ha 10, e li mette nella tipologia «Sacchetti», con la sua icona. Nel filtro sul servizio i sacchetti hanno la loro voce, censiti o no: per un sacchetto non viene stimato nessun servizio dai giri. Il dettaglio riporta i giri che lo hanno letto. Un sacchetto è monouso: viene letto una volta sola, quando il mezzo lo raccoglie. Per questo si vede sulla mappa e nelle ricerche, ma resta fuori dall'analisi dei cluster, che riguarda i bidoni. Il dataset di esempio contiene 12 sacchetti.

**Aggiungere una tipologia o cambiare un'icona.** Si modifica solo la funzione `tipologie_servizio()` in `R/utils_styling.R`. Per un nuovo nome basta aggiungerlo, in maiuscolo, al vettore `servizi` della tipologia giusta. Per una nuova tipologia si aggiunge una voce con nome, icona [Font Awesome](https://fontawesome.com/icons), colore ed emoji. Marker, legenda, filtri e statistiche si aggiornano da soli.

### Dettaglio del bidone

Un click su un marker apre il popup con i dati della lettura e, nella barra laterale, l'analisi di tutta la storia del bidone:

- **censito con servizio stabile**: conferma della tipologia;
- **cambio di servizio**: cronologia dei passaggi del servizio a database, con il servizio attuale;
- **non censito**: stima percentuale del servizio, ricavata dal servizio atteso delle sue letture, cioè dai giri che lo hanno letto. Se nessuna lettura ha un servizio atteso, l'indicazione che non ci sono elementi per stimarlo;
- **cambio di proprietario**: cronologia delle utenze, mostrata in aggiunta ai casi precedenti;
- **letture non coerenti**: quante letture hanno il servizio a database diverso da quello atteso dal giro, con le più recenti.

Il dettaglio segnala anche le letture fuori dai comuni dichiarati: quante sono fuori dal comune del database e dove sono state fatte, quante sono fuori giro, con le cinque più recenti.

Il popup riporta il servizio a database, per un censito, il servizio atteso dal giro e il giro del calendario, il comune a database, per un censito, il comune di lettura, il comune del giro se è diverso da quello di lettura, il cantiere, e una riga per ogni incongruenza: «a database LIMENA, letto a CITTADELLA».

### Analisi Cluster Spaziale

La quarta scheda raggruppa le letture di ogni bidone in luoghi distinti e classifica ogni luogo. Si preme «Genera Analisi» nella barra laterale: l'analisi riguarda tutte le letture del periodo, a prescindere dai filtri della mappa. Fanno eccezione i sacchetti, che restano fuori: il risultato è quello di un file che non li contiene. Se si cambia periodo o file il risultato viene scartato e va rigenerato.

Il risultato ha una riga per ogni cluster di ogni RFID e si esplora in tre viste:

- **Tabella**: tutte le colonne, ordinabili, con un filtro per colonna. I filtri della tabella valgono anche per le altre due viste.
- **Grafici**: numero di cluster per indicatore, e letture contro dispersione, con un riquadro per indicatore.
- **Mappa**: un punto sul baricentro di ogni cluster e un cerchio con il raggio della dispersione, in metri a ogni zoom. Gli indicatori si accendono e spengono dal riquadro in alto a destra.

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

L'indice di fiducia ha un report dedicato, [docs/indice_fiducia.qmd](docs/indice_fiducia.qmd), in Quarto: spiega come si calcola e perché il metodo è semplice ed efficace. Numeri e grafici sono calcolati sul dataset di esempio quando lo si compila in HTML (serve il programma [Quarto](https://quarto.org); quello incluso in Positron va bene):

```r
quarto::quarto_render("docs/indice_fiducia.qmd")   # crea docs/indice_fiducia.html
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

## Confini dei comuni

Le mappe dell'app usano i confini di `inst/extdata/comuni_confini.geojson`: sono quelli della Regione del Veneto, dallo strato dei comuni del suo geoportale, con licenza IODL 2.0. Non sono semplificati: seguono il confine ufficiale vertice per vertice. Il file ha un confine per ogni riga della tabella dei comuni, nello stesso ordine, e pesa 1,25 MB: il browser lo scarica una volta, e non a ogni mappa. Le mappe citano la fonte in basso, come chiede la licenza.

Lo shapefile della Regione sta in `data-raw/rvccomuni/`, con la richiesta da cui viene in `wfsrequest.txt`. Quando la Regione pubblica confini nuovi basta sostituire il contenuto della cartella, con un solo file `.shp`, e rieseguire lo script; lo stesso dopo aver aggiunto un comune alla tabella:

```sh
Rscript data-raw/prepara_comuni.R
```

Lo script legge lo shapefile, lo riporta in longitudine e latitudine e ne tiene i comuni serviti. Serve il pacchetto `sf`. I nomi si confrontano senza badare a maiuscole, accenti e apostrofi: `ROSÀ` della tabella trova `Rosa'` della Regione. Dopo aver cambiato i confini va rigenerato il dataset di esempio, con `Rscript data-raw/genera_sample_dataset.R`. Se anche un solo comune non trova il suo confine, o se il centro abitato indicato in tabella cade fuori dal confine trovato, lo script si ferma senza scrivere niente e dice quali comuni controllare.

## Preparare i dati reali

Il CSV con le letture vere si produce dentro il repository, in due passi, dalla radice del progetto.

| Passo | Come | Cosa fa |
|---|---|---|
| 1, estrazione | `source("data-raw/01_estrai_tabelle.R")`, a mano | Estrae le tabelle dal database aziendale e le salva in `data-raw/origine/` |
| 2, pipeline di preparazione | `targets::tar_make()` | Dai file estratti scrive in `data-raw/output/` il CSV e l'archivio `.duckdb` da caricare nell'app, l'elenco dei giri da controllare, il CSV delle letture storiche e quello dei comuni corretti. Fa solo la pulizia dei dati: i report, come quello sui [conferimenti del secco oltre il limite](#conferimenti-del-secco-oltre-il-limite), si compilano a mano |

La pipeline usa il pacchetto [{targets}](https://docs.ropensci.org/targets/): sta in `data-raw/_targets.R` e rifà solo quello che è cambiato. Dopo una nuova estrazione riconosce i file cambiati; dopo una modifica a una regola del pacchetto rifà i passi che la usano. Le tabelle intermedie restano in memoria; i due dataset finali si leggono in R con `targets::tar_load(letture_app)` e `targets::tar_load(letture_storiche)`, e i file in `data-raw/output/` li scrivono passi a sé, l'archivio `.duckdb` dal CSV. Altri comandi utili: `targets::tar_make(names = duckdb_letture_app)` per i soli file dell'app, `targets::tar_outdated()` per sapere che cosa è da rifare, `targets::tar_visnetwork()` per vedere la catena dei passi. Servono i pacchetti `targets` e `pkgload`.

I file di `data-raw/origine/` hanno sempre lo stesso nome, elencato in `file_origine()`. Le letture delle antenne, `letture_antenne.csv`, e i test dei mezzi, `AppSheet.ViewData.csv`, arrivano da altri sistemi e vanno salvati con quel nome. In cima a `data-raw/_targets.R` si indicano i nomi delle colonne del file delle letture storiche.

Il file `.duckdb` contiene le stesse letture del CSV, già controllate come le controlla l'app e con le sole colonne che l'app usa, con gli avvisi del controllo: l'app lo apre senza rileggerle. Lo scrive `scrivi_archivio()`, che si può usare anche da sola su un CSV già preparato: `scrivi_archivio("letture_app.csv", "letture_app.duckdb")`. Le regole sono funzioni del pacchetto, in `R/fct_importa_tabelle.R`, una per file estratto, e in `R/fct_preparazione_letture.R`, e hanno i loro test. Il comune di lettura arriva con le letture delle antenne: è il testo dopo la prima virgola del campo `IndirizzoLocalizzazione`, e con la tabella dei comuni del pacchetto se ne trova il cantiere. Se il comune del sistema di localizzazione coincide con quello della fornitura del contenitore, la lettura lo tiene; altrimenti ogni lettura che cade nel confine di un comune servito prende quel comune, dalla posizione, con i confini della Regione: il sistema di localizzazione scrive a volte una frazione, o il comune accanto. Un messaggio conta le letture cambiate, e `comuni_corretti.csv` le elenca tutte: istante, mezzo, RFID, coordinate, comune del sistema di localizzazione, comune della fornitura e comune dalla posizione. Una lettura fuori dai comuni serviti, o senza coordinate, tiene il nome del sistema di localizzazione. Il percorso è spiegato con diagrammi in `docs/comune_lettura.qmd` (si compila come l'altro, non con la pipeline). Il servizio e gli altri dati del contenitore vengono dall'anagrafica dei contenitori attiva nel giorno della lettura, insieme alla fornitura a cui il contenitore è legato: `id_fornitura`, il suo stato, `stato_fornitura`, l'utenza che la possiede, `id_utenza`, e il suo comune, `comune_fornitura`. Un'utenza può avere più forniture. Una lettura è «Presente» a database se ha una fornitura: quella del contenitore o, per un sacchetto, quella del suo gruppo di sacchetti; un sacchetto non ha un'utenza. Il giro, il suo comune, `comune_giro`, e i campi `id_giro` ed `eseguito` vengono dal giro associato alla lettura; un giro non pianificato ha l'`id_giro` del suo file, se c'è, ma non ha `eseguito`, che vale solo per il calendario dei pianificati. La coerenza del comune, `coerenza_comune`, confronta il comune di lettura con quello del database o del giro, con la stessa funzione che l'app usa al caricamento; per una lettura fatta in un altro comune la pipeline scrive anche la distanza dal comune dichiarato, `distanza_comune_dichiarato`, e una lettura entro 100 metri è coerente. I nomi dei giri sono riportati a quelli del database con la [tabella di conversione](#tipologie-di-servizio-e-icone), e danno il servizio atteso. I nomi dei comuni si correggono all'importazione: `VALSTAGNA`, soppresso nel 2019 e oggi parte di `VALBRENTA`, viene riportato a `VALBRENTA`. Un nome che la tabella dei comuni non contiene arriva comunque all'app, che lo segnala con un avviso.

La pipeline controlla anche il calendario, con il campo `eseguito`, e scrive in `data-raw/output/controllo_giri.csv` i giri da guardare, con un avviso che ne dice il numero: i giri ancora `NO` più di sette giorni prima dell'ultima lettura, probabilmente conferme dimenticate, e i giri confermati di un mezzo con l'antenna collaudata, dopo il collaudo, senza nessuna lettura, segno di un'antenna spenta o guasta. Un giro che non raccoglie contenitori, come la pulizia del territorio, può non avere letture. Il file c'è sempre, anche senza righe.

La pipeline aggiunge in coda sette colonne della fornitura, che l'app non carica: `nucleo_residenti`, `nucleo_non_residenti`, `nucleo_totale`, `tipo_categoria`, `categoria`, `sottocategoria`, in vigore al momento della lettura, e `potenziamento_secco`, vero se nel giorno della lettura la fornitura ha almeno una manovra sociale in vigore. Vengono dalle tabelle della composizione dei nuclei e delle manovre sociali, che l'estrazione aziendale filtra già alle sole dichiarazioni del potenziamento; servono al report sui conferimenti. Una dichiarazione che finisce prima di cominciare, di solito annullata, non conta e dà un avviso.

Le letture storiche, fatte con i lettori manuali, servono ai report che confrontano il sistema precedente con le antenne: l'app non le carica. Il file estratto ha tre campi: `data_lettura`, l'istante, `codice_transponder` e `id_trasmissione`, che raggruppa i codici letti nello stesso giro in un giorno ma non corrisponde al codice dei giri del calendario. La pipeline porta i codici nel formato delle letture delle antenne e associa a ogni lettura la stessa anagrafica di `letture_app.csv`: contenitore attivo nel giorno, fornitura, utenza, nucleo e potenziamento del secco. Senza un giro non c'è il servizio atteso. Toglie i sacchetti, come per le letture dell'app, e scrive `letture_storiche.csv` con i campi in comune nello stesso ordine, più `id_trasmissione`.

Le cartelle dei dati, `data-raw/origine/`, `data-raw/output/` e l'archivio della pipeline, `data-raw/_targets/`, sono escluse da git: i dati aziendali restano sul computer di chi li prepara. Il passo 1 richiede i pacchetti `odbc` e `DBI` e la sorgente dati ODBC configurata.

## Conferimenti del secco oltre il limite

Il report `docs/conferimenti_secco.qmd` trova le forniture domestiche che, al ritmo attuale, conferiranno in un anno più secco di quanto consente la tariffa: i litri del nucleo residente, da 720 per un componente a 1680 per cinque o più, oppure 6240 con il potenziamento del secco di una manovra sociale per neonati, anziani o disabili. Per ogni fornitura stima i conferimenti dell'anno, quelli consentiti, quelli eccedenti e la probabilità di superare il limite, e la mette in una classe: «Oltre il limite», «Probabile superamento», «Vicino al limite», «Entro il limite», oppure il motivo per cui non si può dire. Per i bidoni non censiti del secco stima i conferimenti, il nucleo più piccolo che li giustificherebbe, quanto è comune il potenziamento tra le forniture censite che conferiscono così e il contenitore censito che potrebbero aver sostituito.

Le letture delle antenne non vedono tutti gli svuotamenti: le antenne sono recenti e solo su una parte dei mezzi. Per questo il report non conta le letture, ma le confronta con le occasioni, cioè le volte in cui un mezzo con l'antenna è passato davanti al bidone, che si riconoscono dalle letture dei bidoni vicini. Il report spiega il metodo, le ragioni delle scelte e la prova su un territorio simulato.

- Sul dataset di esempio: `quarto::quarto_render("docs/conferimenti_secco.qmd")`. Nell'esempio i bidoni del secco sono grandi ed esposti quasi sempre: i risultati servono solo a vedere il report.
- Sui dati aziendali: lo compili tu, la pipeline `targets::tar_make()` non lo fa. `compila_report()` lo compila con i parametri e mette l'HTML in `data-raw/output/`, fuori dal repository; `letture` è il CSV della pipeline, `dal` e `al` il periodo (vuoti: l'anno dell'ultima lettura), `cartella_csv` dove scrivere i tre CSV:

```r
source("data-raw/compila_report.R")
compila_report(
  "docs/conferimenti_secco.qmd",
  list(letture = "data-raw/output/letture_app.csv", cartella_csv = "data-raw/output")
)
```

L'app non usa ancora questa analisi.

## Confronto tra letture storiche e letture con le antenne

Il report `docs/confronto_antenne.qmd` confronta le letture dei lettori manuali (le saponette) con quelle delle antenne. Si compila a mano, come gli altri, e ha questi parametri:

| Parametro | Che cosa fa |
|---|---|
| `letture_antenne` | CSV delle letture con le antenne, di norma `data-raw/output/letture_app.csv` |
| `letture_storiche` | CSV delle letture storiche, di norma `data-raw/output/letture_storiche.csv` |
| `servizio` | tiene le letture con `servizio_atteso` **oppure** `servizio_transponder` uguale al valore, per esempio `"SECCO"` (le storiche hanno solo il secondo) |
| `categorie_escluse` | toglie le letture di quella `categoria`, per esempio `"34"`; i non censiti, senza categoria, restano |
| `sottocategoria` | tiene solo le letture di quella `sottocategoria`, per esempio `"D"`; i non censiti, senza sottocategoria, escono |
| `mezzi_totali` | mezzi della flotta, per la proiezione (analisi H); vuoto: niente proiezione |
| `chip_condivisi_minimi` | RFID letti dai due sistemi perché una trasmissione delle saponette sia abbinata a un mezzo (analisi C), 3 |
| `finestra_giorni` | giorni della finestra mobile della copertura (analisi B), 28 |

Un parametro di filtro vuoto non filtra; per più valori si passa un elenco (`c("SECCO", "CARTA")`). I percorsi relativi partono dalla radice del progetto.

Il report comincia dal confronto iniziale: degli RFID letti dalle antenne dopo i filtri si prendono le coppie di `data_lettura` e `id_trasmissione` in cui compaiono nelle letture storiche, e si tengono tutte le letture storiche di quelle coppie. Questo confronto resta, ma non è del tutto equo: un giro delle saponette comprende RFID che le antenne non hanno mai letto (un mezzo senza antenna, un bidone non raccolto da un mezzo attrezzato, un bidone non letto dall'antenna), quindi è a svantaggio delle antenne, e il primo e l'ultimo mese sono parziali. Il report lo spiega con diagrammi e statistiche, poi lo corregge e aggiunge otto analisi:

| | Analisi |
|---|---|
| Correzione 1 | letture al giorno per mese, non totali del mese |
| Correzione 2 | le saponette contano solo gli RFID delle antenne |
| A | stesso periodo dell'anno prima (364 giorni), per giorno, con un intervallo al 95% |
| B | quota dei contenitori attivi letti almeno una volta in ogni finestra mobile |
| C | mezzi e giorni in cui i due sistemi hanno lavorato insieme: RFID letti da entrambi, solo dalle saponette, solo dalle antenne |
| D | giri registrati al giorno e RFID per giro, nel tempo |
| E | letture a settimana al crescere dei mezzi con antenna |
| F | RFID delle antenne che nessuna saponetta ha mai letto |
| G | coerenza delle letture con le antenne con il servizio e con il comune |
| H | proiezione a tutta la flotta, come stima |

```r
source("data-raw/compila_report.R")
compila_report(
  "docs/confronto_antenne.qmd",
  list(servizio = "SECCO", categorie_escluse = "34", sottocategoria = "D", mezzi_totali = 40)
)   # crea data-raw/output/confronto_antenne.html
```

## Controllo dei giri con antenne e saponette

Il report `docs/controllo_giri_saponette.qmd` confronta i due sistemi su una lista di giri in cui i mezzi hanno già le antenne e gli operatori dovevano usare anche le saponette. La lista dà un denominatore che non dipende da nessuno dei due: si sa quali giri dovevano esserci. Se le saponette fossero complete, ogni RFID letto da un sistema sarebbe letto anche dall'altro.

Le antenne sono le letture del giorno dei mezzi della lista con il servizio del giro indicato (per esempio `SECCO PaP`, senza distinguere le maiuscole; gli zeri iniziali delle matricole non contano). Le saponette sono tutte le letture delle trasmissioni dello stesso giorno che hanno letto almeno uno di quegli RFID. Il report mostra quanti RFID sono letti da entrambi, solo dalle antenne, solo dalle saponette, mezzo per mezzo; come ogni trasmissione è stata abbinata a un mezzo (le letture storiche non hanno il mezzo); dove si trovano gli RFID letti da un solo sistema (per esempio letti da un'antenna fuori lista lo stesso giorno), con il servizio a database e la coerenza delle letture. Quando i due sistemi differiscono non dice quale sbaglia.

I giri da controllare stanno in un file, `data-raw/origine/giri_controllo.csv` (parametro `giri_controllo`, fuori da git come gli altri dati aziendali): per un nuovo confronto si modifica il file e si ricompila, senza toccare il report. Il file ha una riga per mezzo e giorno:

| Colonna | Contenuto |
|---|---|
| `data_giro` | giorno del giro, `AAAA-MM-GG` oppure `GG/MM/AAAA` |
| `matricola_veicolo` | matricola del mezzo; gli zeri iniziali non contano (`0101` e `101` sono lo stesso mezzo) |
| `servizio_giro` | servizio del giro come nel calendario, per esempio `SECCO PaP`; le maiuscole non contano |

Il separatore può essere la virgola o il punto e virgola; le righe vuote si ignorano e una riga a metà dà un errore con il numero di riga. Con più giorni, mezzi o servizi nello stesso file il confronto si fa su tutti insieme. I parametri `letture_antenne` e `letture_storiche` sono i CSV della pipeline.

```r
source("data-raw/compila_report.R")
compila_report("docs/controllo_giri_saponette.qmd")
# con un altro file di giri
compila_report(
  "docs/controllo_giri_saponette.qmd",
  list(giri_controllo = "data-raw/origine/altri_giri.csv")
)
```

## File grandi

Un anno di letture con le antenne può arrivare a un paio di milioni di righe. L'app è costruita per restare veloce a quella scala.

**Dove stanno le letture.** In un database DuckDB in memoria, non in R. Filtri, ultima lettura di ogni bidone, ricerche e conteggi sono interrogazioni SQL: in R arrivano solo le righe che servono, ad esempio una per bidone per la mappa. Il periodo di analisi non copia le letture.

**Caricare il file.** Dal più semplice al più rapido.

- Dal browser, come un file piccolo. Un CSV di 2 milioni di righe con le colonne della pipeline pesa circa 550 MB.
- Dal browser, compresso. Lo stesso file in `.csv.gz` pesa circa un ottavo, in Parquet circa un settimo, e arriva al server in meno tempo.
- Il file `.duckdb` della pipeline, dal browser o dal disco: le letture sono già controllate e l'app le apre in meno di un decimo di secondo, invece di qualche secondo. Pesa circa un decimo del CSV, e la memoria usata è un quinto.
- Dal disco, senza passare dal browser: `run_app(letture = "percorso")`. Il file viene letto una volta sola e resta aperto per tutte le sessioni dello stesso processo. È la scelta giusta quando l'app è usata da più persone, meglio ancora con il file `.duckdb`.

**Tempi misurati** su 2 milioni di letture di 100.000 bidoni, su un portatile, a ottobre 2026. I tempi nel browser sono misurati senza scheda grafica: con una scheda grafica sono più brevi.

| Operazione | Tempo |
|---|---|
| Lettura e controllo del file | circa 4 s da CSV, anche compresso; 1,4 s da Parquet; meno di 0,1 s dal file `.duckdb` |
| Cambio di un filtro, sul server | da 0,1 a 0,2 s |
| Cambio di un filtro, nel browser | le bolle cambiano da 0,3 a 0,5 s dopo il click, i bidoni raggruppati entro 1,5 s |
| Zoom o spostamento della mappa | nel browser, senza passare dal server |
| Dettaglio di un bidone | meno di 0,1 s |
| Ricerca di un RFID | meno di 0,1 s |
| Ricerca dei giri di un mezzo in un giorno; di tutti i mezzi in un mese | meno di 0,1 s; circa 1 s |
| Analisi dei cluster | circa 6 s, di cui 1,7 per estrarre le letture del periodo |

Il processo di R usa circa 0,5 GB con il file `.duckdb`. Con un CSV arriva a circa 2,5 GB, perché la memoria usata durante il controllo non torna subito al sistema; durante l'analisi dei cluster serve un altro GB.

**Limiti regolabili** con `options()`, prima di avviare l'app.

| Opzione | Predefinito | Cosa regola |
|---|---|---|
| `trackerfid.max_letture_ricerca` | 50.000 | Letture disegnate sulla mappa dalla ricerca RFID e dalla ricerca dei giri |
| `trackerfid.letture_per_rfid` | 10 | Ultime letture di ogni RFID disegnate sulla mappa della ricerca RFID |
| `trackerfid.max_upload_mb` | 2.048 | Dimensione massima di un file caricato dal browser |

Per ripetere le misure sul proprio computer: `Rscript dev/misura_prestazioni.R <file delle letture>`. Per un file di prova sintetico delle dimensioni volute: `Rscript data-raw/genera_dataset_grande.R <cartella>`.

## Dataset di esempio

`inst/extdata/sample_rfid_dataset.csv` contiene 6.475 letture di 251 RFID, cioè 239 bidoni e 12 sacchetti, da ottobre 2024 a dicembre 2025, con cinque mezzi e 50 utenze. I bidoni stanno nel territorio servito, tra l'Altopiano di Asiago e i Colli Euganei, dentro i confini dei loro comuni. Il 2025 è completo, il 2024 parziale: serve a provare la selezione dell'anno. Ogni bidone viene letto a cadenza fissa dal giro del proprio servizio, come con un calendario di raccolta reale. Ogni giro del calendario, cioè un turno in un giorno della settimana, lavora in un comune, e i suoi bidoni stanno lì.

Le colonne sono le 35 che scrive la pipeline di preparazione dei dati reali, con i formati dei dati aziendali: mezzi, giri, forniture e utenze hanno codici numerici, gli RFID dei bidoni dieci caratteri esadecimali. Quelle del contenitore e della fornitura che l'app non usa hanno valori inventati con le regole dei dati aziendali: volumi da 40, 60, 120 e 1.100 litri; forniture `ATTIVA` o `CESSATA`, con la data di cessazione `2099-12-31` se ancora aperte; categoria `31` e sottocategoria `D` per le domestiche, altre categorie, sottocategoria `ND` e nucleo a zero per le non domestiche; giri confermati, `SI`, tranne quelli degli ultimi sette giorni, `NO`, e qualcuno senza l'informazione. Ogni contenitore censito ha una fornitura, che lo lega alla sua utenza per il suo servizio: un contenitore che cambia utenza o servizio cambia fornitura. Come nei dati reali, ogni lettura ha il giro del mezzo che l'ha fatta, con il suo numero, `id_giro`, il servizio atteso dal giro, con il nome del database, e il comune del giro, `comune_giro`. I bidoni censiti hanno il servizio a database, `servizio_transponder`: 23 letture di quattro censiti, letti nei giri del secco porta a porta, non sono coerenti. Il comune di lettura, `comune_lettura`, è quello il cui confine contiene il punto: nell'esempio nessuna lettura è fuori dai comuni serviti, e una su cinque è fatta in un comune diverso da quello del giro, quasi sempre per i due tag anomali e per un giro di zona: il giro dell'umido di POVE DEL GRAPPA ha come comune la zona MASSICCIO DEL GRAPPA, come certi giri del calendario aziendale, e il caricamento lo segnala con un avviso. I bidoni censiti hanno anche il comune della fornitura, e il cantiere segue dal comune di lettura. Per coerenza del comune le letture sono 5.460 coerenti, 389 fuori comune, 605 fuori giro e 21 di coerenza ignota: quelle dei sacchetti censiti e dei non censiti letti dal giro di zona. 45 letture fatte in un altro comune, ma entro 100 metri dal confine di quello dichiarato, sono coerenti. I comuni in cui lavorano i giri sono 32, nei quattro cantieri. I bidoni non censiti sono 37: nel 2025, 18 hanno una stima netta del servizio e 19 una stima incerta, con il punto di domanda sulla mappa.

Le incongruenze dell'esempio: il contenitore `D250905100` è registrato a LIMENA e letto a CITTADELLA, quindi fuori comune; il tag rimasto su un mezzo, censito a MAROSTICA, è fuori comune nelle letture fatte a NOVE e a BASSANO DEL GRAPPA; il tag fermo in deposito, non censito, è letto da giri di ogni comune, quindi fuori giro. Qualche censito letto appena oltre il confine del suo comune è coerente; due, letti più lontano, risultano fuori comune. Mercoledì 24 settembre 2025 il mezzo `1601` doveva lavorare a LIMENA e ha letto dieci bidoni a BORGORICCO: nella scheda «Ricerca Giro» il comune del giro e quello di lettura sono diversi, ma è fuori giro solo il non censito, perché i nove censiti sono nel comune del loro database.

I sacchetti servono a vedere come l'app li tratta: nei dati reali, oggi, la preparazione dei dati li scarta. Sono 12: 8 censiti, di quattro forniture da `200000001` a `200000004`, e 4 non censiti. Un sacchetto è monouso, quindi ognuno ha una lettura sola: quella della raccolta. Hanno il codice di 24 caratteri che inizia per `00BD`, da `00BD00000000000000000001` in su i censiti e da `00BD00000000000000000101` i non censiti. Come nei dati reali, a database un sacchetto ha solo la fornitura del suo gruppo di sacchetti: niente utenza, servizio, volume, raccolte previste o comune. Nel popup l'utenza di un sacchetto censito è «N/A», non «Non Censito». Il suo servizio atteso è quello del giro che lo ha letto. Li leggono i giri del secco, nel comune del giro. Due sono letti solo nel 2024, gli altri dieci nel 2025.

Lo genera lo script `data-raw/genera_sample_dataset.R`, che ha un seed fisso e ne verifica la conformità prima di scrivere il file. Nella stessa cartella, `cluster_analysis_2025-01-01_2025-12-31.csv` è l'analisi dei cluster del 2025, prodotta con lo script descritto sopra.

Casi utili da provare:

| RFID | Cosa mostra |
|---|---|
| `D250901001` | Cambio di servizio: CARTA, poi SECCO, poi di nuovo CARTA |
| `D250901050` | Cambio di utenza da `58590001` a `58590025` |
| `D250915201` | Non censito con stima 100% SECCO, quindi con icona del secco. Poche letture: `GHOST_TAG` |
| `D250920250` | Non censito con stima incerta: due letture, una dal giro del secco e una da quello della carta. Sulla mappa ha il punto di domanda |
| `D250905100` | Bidone spostato di 20 km, da Limena a Cittadella: `RELOCATED_BIN` con due cluster. Il popup mostra il comune del database e quello di lettura |
| `D241001301` | Tag fermo in deposito, oltre 400 letture l'anno: `DEPOT_STUCK`. Non censito, letto dai giri di tutti i mezzi: stima incerta |
| `D250203302` | Tag rimasto sul camion, letto lungo tutto il giro: `TRUCK_STOWAWAY` |
| `D241003303` | GPS impreciso, 17 cluster: `SCATTERED_READS / GPS_NOISE` |

## Struttura del progetto

```
R/
  app_ui.R, app_server.R        interfaccia e collegamento dei moduli
  run_app.R, app_config.R       avvio e configurazione golem
  mod_caricamento_*.R           caricamento e validazione del file delle letture
  mod_periodo_*.R               periodo di analisi, due date valide per tutta l'app
  mod_filtri_*.R                filtri sopra la mappa principale e statistiche
  mod_mappa_*.R                 mappa WebGL, con mapgl, usata dalle quattro schede con mappa
  mod_ricerca_*.R               ricerca per RFID e per utenza, tabella delle letture della ricerca RFID
  mod_giro_*.R                  ricerca dei giri di un mezzo
  mod_cluster_analysis_*.R      scheda di analisi dei cluster spaziali
  utils_archivio.R              archivio DuckDB delle letture: lettura, validazione in SQL, file .duckdb
  utils_selezione.R             letture del periodo in DuckDB: filtri, ultima lettura, ricerche, giri
  utils_data_processing.R       schema del file, regole dei servizi e delle coerenze, analisi dei bidoni
  utils_mapping.R               configurazione delle mappe e dati per il browser
  utils_styling.R               marker, cluster, legenda, tipologie di servizio
  utils_popup.R                 popup, dettaglio del bidone, statistiche
  utils_dbscan_clustering.R     clustering DBSCAN e distanze
  utils_cluster_output.R        tabella di analisi, indicatori, indice di fiducia
  utils_cluster_viste.R         tabella interattiva, grafici e mappa dei cluster
  utils_geografia.R             comuni, cantieri e confini del territorio servito
  utils_gruppi.R                calcoli per gruppo senza cicli, per i dataset grandi
  utils_tabelle_mappa.R         tabelle delle ricerche sopra le mappe
  utils_conferimenti.R          conferimenti del secco oltre il limite, per il report
  utils_mappa_aggregata.R       bolle di cantieri e comuni, fasce di zoom
  utils_giri.R                  giri dei mezzi: ricerca e riepilogo
  generate_cluster_analysis.R   funzioni esportate per l'analisi senza app
  fct_importa_tabelle.R         importazione dei file estratti dal database aziendale
  fct_preparazione_letture.R    regole che trasformano i dati aziendali nel CSV dell'app
  fct_preparazione_comuni.R     confini dei comuni serviti dallo shapefile della Regione del Veneto
inst/
  extdata/                      dataset di esempio e analisi dei cluster di esempio
  extdata/comuni_cantieri.csv   i 66 comuni serviti, con cantiere e coordinate del centro
  extdata/comuni_confini.geojson   confini dei comuni serviti, della Regione del Veneto, non semplificati
  scripts/generate_cluster_analysis.R   script eseguibile da riga di comando
  app/www/custom_style.css, script.js
  app/www/mappa.js              mappe WebGL nel browser: dati, cluster, bolle, click, popup
  app/www/favicon.png           favicon: il simbolo aziendale
data-raw/
  genera_sample_dataset.R       dataset di esempio, simulati
  genera_dataset_grande.R       dataset sintetico di milioni di letture, per misurare i tempi
  prepara_comuni.R              scrive i confini dei comuni serviti dallo shapefile della Regione
  rvccomuni/                    shapefile dei comuni della Regione del Veneto
  01_estrai_tabelle.R           dati reali, passo 1: estrazione dal database aziendale
  _targets.R                    dati reali, passo 2: pipeline di preparazione con {targets}
  origine/, output/, _targets/  dati aziendali, esclusi da git
dev/
  misura_prestazioni.R          tempi e memoria di ogni passaggio dell'app su un file
docs/
  analisi_cluster.md            analisi dei cluster: regole, colonne, scelte
  indice_fiducia.qmd            report sull'indice di fiducia, da compilare in HTML
  conferimenti_secco.qmd        report sui conferimenti del secco oltre il limite
  confronto_antenne.qmd         confronto tra letture storiche e letture con le antenne, con i filtri
  controllo_giri_saponette.qmd  controllo di una lista di giri con antenne e saponette insieme
  associazione_giri.qmd, comune_lettura.qmd   come si ricavano il giro e il comune di una lettura
tests/testthat/
```

Il file [CLAUDE.md](CLAUDE.md) descrive il progetto per gli assistenti LLM: contesto, flusso dei dati, indice delle funzioni e dove intervenire per ogni tipo di modifica.

Ogni modulo ha un file `_ui.R` e uno `_server.R`. Le mappe di base vengono disegnate una volta sola: filtri e ricerche mandano al browser i nuovi dati, in colonne compatte, senza ricaricare lo sfondo.

## Test

```r
devtools::test()    # funzioni di utilità, moduli, analisi dei cluster e dataset di esempio
devtools::check()
```

## Differenze rispetto alle specifiche

Prima specifica:

- **`leafletmarkercluster` e `shinyicon` non esistono su CRAN.** Il clustering lo faceva il plugin Leaflet.markercluster incluso in `leaflet`; oggi lo fa MapLibre, vedi sotto. Le icone Font Awesome arrivano dal pacchetto `fontawesome`.
- **Marker disegnati in SVG.** In questo modo hanno i colori esatti richiesti (`#2ecc71`, `#e74c3c`) e il bordo spesso o tratteggiato della ricerca RFID, che i marker standard non permettono. La cartella di icone PNG non serve.
- **Nomi delle utenze.** Il dataset non contiene i nomi dei proprietari, quindi il cambio di proprietario mostra solo gli ID utenza.
- **Filtri dei servizi.** Deselezionando tutte le voci di un elenco spariscono i bidoni di quello stato. Lo pseudocodice della specifica in quel caso disattivava il filtro.

Specifica delle correzioni e dell'analisi dei cluster:

- **Dataset di esempio su base annuale.** La prima versione copriva 30 giorni, con una o due letture per la maggior parte dei bidoni. Raccolte annue e soglie dell'analisi dei cluster non erano dimostrabili su quei dati. I casi speciali della prima versione sono rimasti. Il file ha mantenuto il nome `sample_rfid_dataset.csv`.
- **Periodo valido per tutta l'app.** Il periodo si sceglie con due date, nella barra verde in alto, e vale anche per l'analisi dei cluster. Non c'è più un elenco dell'anno a parte.
- **Colonne dell'output.** La specifica parlava di 25 campi e ne definiva 22. Sono stati aggiunti `presente_a_database`, la cronologia dei cambi di servizio e la composizione del servizio stimato. Le colonne del servizio sono `servizio_transponder`, con la sua cronologia `servizio_transponder_cronologia`, e, per i non censiti, `servizio_atteso` con la composizione `servizio_atteso_dettaglio`.
- **Classificazione.** Tre correzioni alle regole, descritte in [docs/analisi_cluster.md](docs/analisi_cluster.md).
- **Distanze per DBSCAN.** Proiezione locale in metri al posto della matrice di Haversine, per reggere tag con migliaia di letture. I cluster coincidono.
- **Script autonomo.** Le funzioni stanno nel pacchetto, lo script in `inst/scripts` le richiama. Un file con codice duplicato sarebbe rimasto disallineato alla prima modifica.

Specifica su geografia e prestazioni:

- **Nell'app solo i filtri.** L'app non ha schede di analisi per territorio, cantiere e comune.
- **Comuni e cantieri.** Il file dei comuni ne elenca 66 in quattro cantieri: il testo della specifica parlava di 55 comuni e cinque cantieri. Le coordinate indicate per i cantieri erano approssimative: quelle usate sono i centri dei comuni, da fonti aperte.
- **Colonne geografiche.** Sono `comune_fornitura`, `comune_giro`, `comune_lettura` e `cantiere`. Il comune di lettura arriva nel file; il comune assegnato e la coerenza del comune li calcola l'app.
- **Dataset di esempio.** Copre un anno e tre mesi, con 6.475 letture: le analisi annuali richiedono un anno completo. I bidoni sono stati spostati nel territorio servito e sono stati aggiunti 12 sacchetti, il resto non è cambiato.
- **Comune di lettura e comuni dichiarati.** Il comune di lettura si confronta con quello del database, per i censiti, e con quello del giro, per i non censiti: le letture in un comune diverso sono segnalate, e una quinta scheda, «Ricerca Giro», mostra che cosa ha letto ogni mezzo in ogni giorno.
- **Gruppi per territorio.** Sulla mappa principale i gruppi seguono il territorio: una bolla per cantiere da lontano, una per comune avvicinandosi, i singoli bidoni da vicino. I confini dei comuni sono disegnati su tutte le mappe, colorati per cantiere.
- **Mappa con molti bidoni.** Al posto del campionamento dei marker o di una mappa di calore, i bidoni si raggruppano per cantiere e per comune secondo lo zoom. Nessun bidone viene escluso dai conteggi. Con Leaflet il server mandava al browser poche centinaia di elementi alla volta; con la mappa WebGL li manda tutti, vedi sotto.

Correzioni alla logica dei non censiti:

- **Giro su ogni lettura.** `servizio_atteso` e `comune_giro` ci sono per ogni lettura, non solo per i non censiti: così è nei dati reali, e così è ora il dataset di esempio. La prima specifica li voleva vuoti per i censiti.
- **Comune dalla posizione, non dal calendario.** Il calendario dice dove un mezzo doveva andare, non dove è andato: un mezzo può sostituirne un altro. `comune_lettura` è quindi il comune in cui è stata fatta la lettura, e il comune del calendario sta in una colonna sua, `comune_giro`, che serve solo a riconoscere le letture fuori giro. Un file preparato prima di questa modifica non ha `comune_giro`: va rigenerato con la pipeline, altrimenti le letture fuori giro non si possono riconoscere.
- **Giro con il nome del calendario, servizio atteso con quello del database.** `servizio_giro` resta scritto come nel calendario dei mezzi, così il secco porta a porta, `SECCO PAP`, si distingue dagli altri giri del secco. `servizio_atteso` ne è la versione con il nome del database, ricavata con una tabella di conversione nella preparazione dei dati.
- **Tre servizi per ogni lettura.** Il servizio a database, `servizio_transponder`, il servizio atteso dal giro, `servizio_atteso`, con il nome del database, e il giro del calendario, `servizio_giro`. La coerenza, `coerenza_servizio`, confronta i primi due, con `SI` e `NO`. Per due giorni, a ottobre 2026, servizio a database e servizio atteso sono stati una colonna sola, con un campo sulla sua origine, e il servizio atteso dei non censiti veniva dai censiti letti dallo stesso mezzo: il proprietario del progetto ha poi riscritto la preparazione dei dati con le tre colonne separate. I file di quei giorni, con la colonna `origine_servizio`, si caricano ancora.
- **Giri dei servizi.** Nei giri in cui non è chiaro quale rifiuto si raccolga, come `SERVIZI MERCATI`, il servizio atteso è il servizio a database più frequente tra i censiti letti nel giro.
- **Filtri su servizio, giro e coerenza.** Il filtro sul servizio riguarda il servizio del contenitore: per un censito quello a database, per un non censito quello stimato. Si aggiungono un elenco sul giro del calendario e un filtro sulla coerenza. Tutti valgono per censiti e non censiti: lo stato a database si sceglie a parte. La casella «Includi Non Censiti» è stata tolta: era un doppione dello stato «Non Presente».
- **Servizio stimato del bidone, per i non censiti.** Per un non censito il filtro non guarda il servizio atteso della singola lettura, ma il servizio stimato del bidone, lo stesso dell'icona: sta quindi sotto una voce sola, il suo servizio stimato oppure la stima incerta.
- **Filtri sopra la mappa.** I filtri della mappa principale sono menu a discesa in una riga sopra la mappa, non più elenchi nella barra laterale, dove restano le statistiche.
- **Comune di lettura dal file.** Il comune in cui è stata fatta una lettura arriva con le letture: la pipeline lo ricava dalla posizione dentro i comuni serviti, e fuori lascia quello del sistema di localizzazione dei mezzi. L'app lo usa così com'è e non lo ricava mai dalla posizione.
- **Coerenza del comune al posto delle distanze.** Fuori comune e fuori giro si misuravano dalla posizione, con una tolleranza di un chilometro dal confine del comune dichiarato, e i punti appena fuori dal territorio prendevano il comune servito più vicino entro 100 metri. Ora il campo `coerenza_comune` confronta i nomi: comune di lettura contro comune del database per i censiti, contro comune del giro per i non censiti. Un censito non è più fuori giro, e la tabella dei cluster non ha più la distanza dal comune dichiarato. Dal 19 ottobre 2026 una lettura entro 100 metri dal confine del comune dichiarato è coerente: la distanza la calcola la pipeline di preparazione, non l'app.
- **Due coerenze.** La coerenza del servizio confronta il servizio a database e quello atteso, la coerenza del comune il comune di lettura e quello dichiarato: hanno un menu ciascuno, «Coerenza servizio» e «Coerenza comune». Nell'indice di fiducia dei cluster la verifica sul servizio si chiama «Stabilità del servizio».
- **Tipologia dei sacchetti.** I sacchetti censiti arrivavano senza servizio, e sulla mappa avevano il punto di domanda. Ora hanno una tipologia e un'icona loro, riconosciuti dal codice RFID.
- **Stima incerta al posto di «senza stima».** La casella riguardava i non censiti senza nessun giro, che nei dati reali non esistono. Ora riguarda quelli sotto l'80%, cioè i marker con il punto di domanda.

Salto architetturale, ottobre 2026:

- **Letture in DuckDB.** Le letture non stanno più in un data frame di R ma in un database DuckDB in memoria. Il controllo del file è scritto in SQL, con le stesse regole e gli stessi messaggi di prima, e su 2 milioni di letture passa da circa 10 s a meno di 4 s. Filtri, ultima lettura per bidone, ricerche e giri sono interrogazioni: in R arrivano solo le righe che servono. Il passo 3 scrive anche un archivio `.duckdb` già controllato, che l'app apre in meno di un decimo di secondo.
- **Mappe WebGL.** Tutte le mappe sono disegnate da MapLibre GL JS, con il pacchetto `mapgl`, al posto di Leaflet. Marker, icone dei servizi, segnali `C` e `G`, colori dei cluster secondo la quota di censiti, bolle di cantieri e comuni, confini colorati per cantiere e legenda sono rimasti gli stessi. Lo sfondo è OpenFreeMap, stile «bright».
- **Zoom nel browser.** Il browser riceve tutti i bidoni e le bolle a ogni cambio dei filtri, e zoom e spostamenti non passano più dal server. Non c'è più l'attesa di un quarto di secondo dopo uno spostamento, né il limite di 10.000 bidoni oltre il quale la mappa passava a bolle per zona.
- **Bidoni nello stesso punto.** Leaflet li apriva a raggiera. MapLibre non lo fa: un click sul loro cluster mostra l'elenco dei bidoni.
- **Numeri di zoom.** MapLibre disegna tessere di 512 pixel: i suoi zoom sono quelli di Leaflet meno uno, alla stessa scala sullo schermo. Le fasce di cantieri, comuni e bidoni sono rimaste le stesse sullo schermo.

Letture storiche, ottobre 2026:

- **Letture storiche tolte dall'app.** L'app caricava, in un secondo campo, le letture del sistema precedente alle antenne, e il dettaglio del bidone ne mostrava un istogramma per anno. Il confronto tra i due sistemi si fa ora nei report: la pipeline di preparazione dei dati prepara ancora le letture storiche, ma l'app non le carica più, e l'istogramma non c'è più.

## Da completare

Il campo `License` di `DESCRIPTION` contiene ancora il segnaposto di golem. Finché non viene scelto, `devtools::check()` riporta un avviso.
