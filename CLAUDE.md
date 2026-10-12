# trackerfid: contesto per assistenti LLM

Questo file descrive il progetto a un assistente che deve leggerlo o modificarlo. Va letto prima di aprire il codice. Le istruzioni per chi usa l'app sono in `README.md`.

Quando aggiungi o sposti file e funzioni, o cambi il flusso dei dati, aggiorna anche questo file.

## In breve

`trackerfid` è un pacchetto R che contiene un'app Shiny, costruita con il framework golem. Un'azienda di raccolta rifiuti la usa per esplorare su mappa le letture RFID dei propri bidoni e per capire dove ogni bidone è stato letto nel tempo.

- Linguaggio: R (>= 4.1), Shiny, golem, shinydashboard, leaflet, dplyr.
- Lingua del progetto: italiano, per interfaccia, nomi di funzioni e variabili, commenti, test e documenti.
- Avvio: `run_app()`, oppure `run_app(demo = TRUE)` per partire con il dataset di esempio, oppure `run_app(letture = "percorso")` per leggere un file dal disco.
- Dimensioni attese: circa 2 milioni di letture l'anno. L'app deve restare veloce a quella scala: vedi "File grandi".

## Contesto del dominio

- Ogni bidone porta un chip, il transponder, con un codice `RFID`.
- I mezzi di raccolta hanno un lettore e un GPS. A ogni passaggio registrano una **lettura**: data e ora, mezzo, RFID, coordinate.
- Il database aziendale associa a un RFID un servizio, cioè il tipo di rifiuto, e un'utenza, cioè il proprietario. Un RFID in anagrafica è **censito**, gli altri sono **non censiti**.
- Una lettura esiste solo se un mezzo passa durante un **giro**. Dal calendario dei mezzi si ricavano il servizio del giro e il suo comune: per questo ogni lettura, anche di un bidone censito, ha il **servizio atteso** e il **comune del giro**, che nel file è `comune_giro`.
- **Il calendario dice che cosa era previsto, non che cosa è successo.** Un mezzo può rompersi ed essere sostituito da un altro, che fa un giro non suo: il servizio e il comune del calendario, per quelle letture, sono sbagliati. Per questo il proprietario del progetto ha voluto che il comune di una lettura venga dalla sua posizione, e che il calendario non assegni comuni a nessun contenitore.
- Per un non censito servizio, utenza e comune non sono a database. Il comune è quello in cui viene letto, dalla posizione. Il servizio viene dai contenitori censiti letti dallo stesso mezzo: un giro raccoglie un rifiuto solo, quindi i censiti letti poco prima e poco dopo dicono quale. Il proprietario del progetto ha approvato la regola e le soglie: censiti nei 30 minuti prima e dopo, almeno 5, almeno l'80% dello stesso servizio; se non bastano, vale il calendario. Uno stesso mezzo può fare più giri in un giorno, di rifiuti diversi, ma non ci sono mezzi a doppia vasca: ogni giro raccoglie una sola tipologia.
- I nomi dei servizi del calendario sono diversi da quelli del database: `CARTA/CARTONE PAP` e `CARTA CONT.STRADALI` contro `CARTA`. Una **tabella di conversione** li riporta a quelli del database nella preparazione dei dati, così `servizio_atteso` e `servizio_transponder` hanno gli stessi valori.
- Oltre ai bidoni le antenne leggono i **sacchetti**. Non hanno una tipologia di rifiuto, ma si riconoscono dal codice RFID: 24 caratteri con il prefisso `00BD`, contro i 10 caratteri di un bidone. Un sacchetto è **monouso**: il mezzo lo legge quando lo raccoglie e, una volta svuotato, non può più essere letto. Ha quindi una lettura sola. Il proprietario del progetto vuole poterli vedere sulla mappa, ma li tiene fuori dall'analisi dei cluster e dal report sul beneficio delle antenne, e il più delle volte li toglierà dal file delle letture.
- L'obiettivo finale dell'analisi è **censire a mano i bidoni che oggi non sono a database**. Gli operatori cercano: bidoni non censiti, cambi di servizio o di utenza, bidoni spostati, tag anomali (smarriti, fermi in deposito, rimasti su un mezzo).
- Ogni lettura ha una posizione, da cui viene il **comune di lettura**, e fino a due **comuni dichiarati**: quello del database, per i censiti, e quello del giro. Il proprietario del progetto vuole che il comune di una lettura venga dalla posizione, e che le differenze con i comuni dichiarati saltino all'occhio. Le letture fuori da tutti i comuni serviti hanno come comune di lettura `FUORI SERVIZIO AZIENDA`: un valore solo, da filtrare via. Un censito letto lontano dal comune del database è **fuori comune**: di solito un bidone spostato. Una lettura lontana dal comune del giro è **fuori giro**: di solito il mezzo ha fatto un giro diverso da quello del calendario, e quel giorno ha molte letture fuori giro. La tolleranza, scelta dal proprietario del progetto, è di un chilometro dal confine del comune.
- Il territorio servito è diviso in quattro **cantieri**: `ASIAGO`, `BASSANO`, `CAMPOSAMPIERO`, `RUBANO`. Ogni comune appartiene a un cantiere. Gli operatori filtrano le letture per cantiere e per comune.
- Le antenne sui mezzi sono recenti. Prima un altro sistema registrava solo RFID e istante della lettura, senza mezzo e senza posizione: sono le **letture storiche**. L'operatore leggeva a mano il tag di ogni contenitore. L'azienda vuole misurare quanto le antenne leggono in più, anche per la fatturazione.
- Nei dati reali, a ottobre 2026, le letture storiche vanno dal primo gennaio 2025 a oggi e quelle con le antenne dal primo agosto 2026: le antenne sono montate solo su una parte dei mezzi, e i due sistemi lavorano insieme. Per questo il report sul beneficio delle antenne non confronta i totali: parte dai contenitori che le antenne hanno letto, la **coorte**, e guarda quante volte li aveva letti il sistema precedente.
- Il report serve a mostrare ai soci dell'azienda il beneficio delle antenne, di oggi e atteso. **Non deve contenere stime in euro**: l'ha chiesto il proprietario del progetto. L'analisi parte dal **secco**, l'unico servizio che l'azienda fattura, e si estende poi agli altri servizi. Per il secco le raccolte previste sono di norma 26 l'anno; un'utenza ne chiede di solito una decina.
- Il **bit flip**: un contenitore tenuto sotto controllo compariva nelle letture storiche con due codici, quello giusto e uno diverso per un carattere, in parti quasi uguali. Con il codice sbagliato il database non lo riconosce. La preparazione dei dati corregge già l'errore quando cade negli zeri iniziali del codice, vedi `formatta_rfid()`; nelle cifre che contano non si riconosce da solo.

## Glossario

| Termine | Significato | Nel codice |
|---|---|---|
| Lettura | Una riga del CSV: un RFID letto da un mezzo in un istante e in un punto | righe del dataset |
| Censito, non censito | RFID presente o assente nel database aziendale | `presente_a_database`: `"Presente"` o `"Non Presente"` |
| Servizio transponder | Tipo di rifiuto secondo il database. Sei valori: `SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE` | `servizio_transponder`, vuoto per i non censiti |
| Servizio atteso | Servizio che il mezzo stava raccogliendo al momento della lettura. Per un non censito viene dai censiti letti dallo stesso mezzo nella mezz'ora attorno e, se sono pochi o misti, dal calendario; per un censito è quello del calendario. Ha i nomi del database, ad esempio `SECCO`. C'è su ogni lettura: l'app lo usa per i soli non censiti | `servizio_atteso`, `associa_servizio_atteso_dai_censiti()` |
| Servizio del giro, giro previsto | Servizio che il calendario dava al mezzo al momento della lettura, con i nomi del database: nel calendario il giro si chiama `SECCO PAP`, dove PAP significa porta a porta. Descrive il giro nella scheda «Ricerca Giro». In un file che non lo riporta vale il servizio atteso | `servizio_giro`, `servizio_del_giro()` |
| Provenienza del servizio atteso | Dice se il servizio atteso di una lettura viene dai censiti o dal calendario. Sta nel CSV, per chi lo analizza: l'app non la carica | `origine_servizio_atteso`: `"censiti"` o `"calendario"` |
| Tabella di conversione | Riporta i nomi dei giri del calendario a quelli del database: dodici nomi diventano nove. Il nome uniforme di una tipologia è il primo dei suoi `servizi` | `conversione_servizi()`, `uniforma_servizio()`, `uniforma_servizi_calendario()` |
| Servizio stimato | Per un non censito: il giro prevalente, se una tipologia raggiunge l'80% delle sue letture non censite del periodo. Dà l'icona al marker ed è il valore usato da filtri e statistiche | `servizio_icona`, `servizio_prevalente_non_censiti()`, `servizio_mostrato()` |
| Stima incerta | Non censito senza servizio stimato: nessuna tipologia arriva all'80%, oppure il file non riporta nessun giro. Sulla mappa ha il punto di domanda | `servizio_icona` mancante, `includi_stima_incerta`, `stima_incerta` nelle statistiche |
| Tipologia | Gruppo di nomi di servizio che condividono icona e colore | `tipologie_servizio()` |
| Sacchetto | Sacco monouso con un chip, riconosciuto dal codice RFID: 24 caratteri che iniziano per `00BD`. Viene letto una volta sola. Non ha una tipologia di rifiuto: l'app gli dà il servizio `SACCHETTI`, con una tipologia e un'icona sue | `rfid_sacchetto()`, `servizio_sacchetti()`, `senza_sacchetti()` |
| Utenza | Proprietario del bidone secondo il database | `id_utenza` |
| Periodo di analisi | Anno solare o intervallo di date. Vale per tutta l'app | modulo `mod_periodo` |
| Ultima lettura | La lettura più recente di un RFID. La mappa principale ne mostra una per RFID | `deduplica_ultimo_rfid()` |
| Cluster di marker | Raggruppamento visivo dei marker vicini sulla mappa principale | modalità `"cluster"`, `opzioni_cluster()` |
| Cluster spaziale | Gruppo di letture vicine dello stesso RFID, trovato con DBSCAN | `cluster_id`, `R/utils_dbscan_clustering.R` |
| Dispersione | 90° percentile della distanza delle letture dal baricentro del cluster spaziale, in metri | `cluster_dispersione_90th_m` |
| Giorni osservati | Parte del periodo di analisi coperta dal dataset. Serve a riportare i conteggi a un anno | attributo `parametri` del risultato |
| Indicatore | Categoria di un cluster spaziale. Sette valori, ad esempio `VALID_TARGET` | `indicatore_cluster`, `classifica_cluster()` |
| Indice di fiducia | Voto da 0 a 1 sull'affidabilità di un cluster spaziale: somma pesata di sei verifiche | `cluster_indice_fiducia`, `indice_fiducia()` |
| Cantiere | Sede operativa che serve un gruppo di comuni. Quattro valori | `cantiere`, `cantieri()`, `inst/extdata/comuni_cantieri.csv` |
| Comune del database | Comune di servizio del contenitore secondo il database | `comune_da_database`, vuoto per i non censiti |
| Comune del giro, comune atteso | Comune in cui il calendario mandava il mezzo che ha fatto la lettura. C'è su ogni lettura. Serve solo al confronto con la posizione: non assegna un comune a nessun contenitore | `comune_giro` |
| Comune di lettura | Comune servito il cui confine contiene il punto della lettura, dalla posizione. Fuori dai comuni serviti, o senza coordinate, vale `FUORI SERVIZIO AZIENDA` | `comune_lettura`, `fuori_servizio()`, calcolato da `aggiungi_geografia()` |
| Comune geografico | Lo stesso comune di `comune_lettura`, ma vuoto fuori dai comuni serviti: lo usano i conti, che non devono trattare la voce dei fuori servizio come un comune | `comune_geografico`, `comune_geografico()`, `R/utils_comune_geografico.R` |
| Comune dichiarato | Comune del database, per un censito, o comune del giro: quello che i dati dicono, a confronto con la posizione | `comune_da_database`, `comune_giro` |
| Comune assegnato | Comune geografico della lettura; se il punto è fuori dai comuni serviti, quello del database, per i censiti. Un non censito letto fuori dai comuni serviti non ne ha. È il comune usato da filtri, bolle e statistiche | `comune_assegnato`, calcolato da `aggiungi_geografia()` |
| Fuori comune | Lettura di un censito oltre la tolleranza dal comune del database | `fuori_comune`, segnale `C` sul marker, `FUORI_COMUNE` nella tabella dei cluster |
| Fuori giro | Lettura, di un censito o no, oltre la tolleranza dal comune del giro | `fuori_giro`, segnale `G` sul marker, `FUORI_GIRO` nella tabella dei cluster |
| Tolleranza | Distanza dal confine del comune dichiarato oltre la quale una lettura è incongruente: un chilometro | `tolleranza_confine_m()` |
| Giro | Lavoro di un mezzo in un giorno, per un servizio e in un comune, secondo il calendario. Nel file sta su ogni lettura | `servizio_giro` e `comune_giro`, `riepilogo_giri()`, `R/utils_giri.R` |
| Comune di un RFID | L'ultimo comune indicato dal database, per un censito; per un non censito quello in cui cade il maggior numero delle sue letture, secondo la posizione. Lo usano i report e la tabella dei cluster | `comune` in `anagrafica_rfid()` e nella tabella dei cluster, `geografia_per_gruppo()` |
| Bolla | Sulla mappa principale, da lontano: un gruppo di bidoni per cantiere o per comune, calcolato dal server. Da vicino, oltre il limite dei marker, anche per riquadro. Ha l'aspetto di un cluster di marker. La bolla di un comune sta nel baricentro del comune | `scegli_vista()`, `aggrega_marker()`, `baricentri_comuni()`, `R/utils_mappa_aggregata.R` |
| Confini dei comuni | Livello disegnato su tutte le mappe: il confine di ogni comune servito, tratteggiato, con il colore del suo cantiere | `aggiungi_confini()`, `geojson_confini()`, `colori_cantieri()` |
| Letture storiche, sistema precedente | Letture fatte prima dell'installazione delle antenne: solo RFID e istante | secondo file CSV, `fonte == "storico"`, `R/utils_storico.R` |
| Fonte | Sistema che ha fatto una lettura | `fonte`: `"storico"` o `"antenne"`, aggiunta da `unisci_letture()` |
| Anno misto, anno parziale | Anno coperto da tutti e due i sistemi. Anno coperto per meno del 95% dei giorni, riportato a dodici mesi | `sistema`, `completo`, `fattore` in `copertura_annuale()` |
| Coorte | I contenitori letti dalle antenne: su questi si fa il confronto tra i due sistemi. Entrano anche quelli che il sistema precedente non ha mai letto | righe di `anagrafica_rfid()` e di `confronto_sistemi()` |
| Una lettura al giorno | Nel confronto due letture dello stesso contenitore nello stesso giorno contano una volta: sono lo stesso svuotamento | `una_lettura_al_giorno()` |
| Periodo di riferimento | I 365 giorni che precedono la prima lettura con le antenne, o la parte coperta dalle letture storiche: lì il sistema precedente lavorava da solo | `riferimento`, `giorni_riferimento` in `periodi_confronto()` |
| Già in servizio | Contenitore con almeno una lettura storica prima del periodo di riferimento: non può essere un contenitore nuovo | `letture_prima`, `gia_in_servizio` |
| Giorni osservati | Per le antenne, i giorni dalla prima lettura di un contenitore all'ultimo giorno dei dati. Contano le sole letture successive alla prima | `giorni_dopo`, `letture_dopo` |
| Tipologia di un RFID | Il rifiuto del contenitore: dal servizio del database per un censito, dal servizio stimato dai giri per un non censito | `tipologia` in `anagrafica_rfid()` |
| Raccolte previste di un non censito | Le più frequenti tra i censiti dello stesso servizio nello stesso comune, o dello stesso servizio | `raccolte_previste`, `raccolte_stimate`, `raccolte_di_riferimento()` |
| Letture utili | Letture di un contenitore, mai più delle raccolte previste nei giorni osservati | `utili_storico`, `utili_antenne` |
| Tasso di lettura | Somma delle letture utili divisa per la somma delle raccolte previste nei giorni osservati | `tasso_storico`, `tasso_antenne` in `riepilogo_confronto()` |
| Codici gemelli | Due codici RFID che differiscono per un solo carattere | `codici_gemelli()` |
| Gemello sospetto | Coppia di codici gemelli che sembra un contenitore solo, letto male: uno censito e l'altro no, mai nello stesso giorno, stessi giorni della settimana, un bit di differenza | `sospetto` in `gemelli_sospetti()` |

**Attenzione: la parola "cluster" ha due significati che non c'entrano l'uno con l'altro.** Sulla mappa principale è un effetto grafico. Nell'analisi è il risultato di DBSCAN. Le bolle per cantiere, per comune e per riquadro hanno l'aspetto dei cluster di marker ma sono calcolate dal server.

## Funzionalità

L'interfaccia è una dashboard. La barra laterale contiene, dall'alto: caricamento dei due file (letture con le antenne e, facoltative, letture storiche), periodo di analisi, dettaglio del bidone selezionato, controlli della scheda aperta. Il corpo ha cinque schede, in un `tabBox` con id `scheda_attiva`.

| Scheda | Valore di `scheda_attiva` | Cosa mostra | Controlli nella barra laterale |
|---|---|---|---|
| Mappa Principale | `mappa` | Un marker per RFID, nella posizione dell'ultima lettura del periodo. Da lontano una bolla per cantiere, poi una per comune; da vicino i singoli marker, con quelli vicini raggruppati in cluster colorati secondo la quota di censiti. Confini dei comuni colorati per cantiere | Filtri per cantiere, comune, stato a database, servizio transponder, servizio atteso. Statistiche |
| Ricerca RFID | `rfid` | Tutte le letture degli RFID cercati. Ultima lettura in evidenza, riquadro attorno alle posizioni di ogni RFID | Campo di testo, pulsanti Cerca e Pulisci |
| Ricerca Giro | `giro` | Che cosa ha letto un mezzo in un giorno: tabella dei giri, con servizio e comune previsti dal calendario e letture fuori giro e fuori comune, e mappa di tutte le letture, con il confine del comune del giro in evidenza | Mezzo, giorno o intervallo di giorni, caselle «Fuori giro» e «Fuori comune» |
| Ricerca Utenza | `utenza` | Ultima lettura dei bidoni la cui utenza attuale è tra quelle cercate | Campo di testo, pulsanti Cerca e Pulisci |
| Analisi Cluster Spaziale | `cluster` | Tabella, grafici e mappa dei cluster spaziali di ogni RFID, con indicatore e indice di fiducia. Esportazione in CSV | Parametri DBSCAN, pulsante Genera Analisi |

**Le schede sono cinque, per scelta del proprietario del progetto**, che ha chiesto la «Ricerca Giro» dopo aver fissato le prime quattro. Le analisi per territorio, cantiere e comune stanno nel report `docs/beneficio_antenne.Rmd`: non aggiungere all'app altre schede di analisi, di report o di prestazioni.

Altri comportamenti:

- **Filtri per cantiere e comune.** I cantieri si spuntano: la voce «Non assegnato» raccoglie le letture senza cantiere, cioè i non censiti letti fuori dai comuni serviti. I comuni si scelgono da un elenco a scelta multipla, con i comuni dei cantieri spuntati divisi per cantiere: se ne possono scegliere più di uno, anche di cantieri diversi. Nessun comune scelto vale per tutti. Vale il comune assegnato alla lettura, cioè quello in cui cade il suo punto. Cambiando cantieri o comuni la mappa si sposta sull'area scelta. I due filtri compaiono solo se almeno una lettura ha un cantiere.
- **Filtro sulla posizione.** «Filtro Posizione» ha tre voci, tutte spuntate all'avvio: «Coerente», «Fuori comune», «Fuori giro». Una lettura fuori comune e fuori giro insieme resta se è spuntata almeno una delle due voci. Compare con gli altri filtri geografici. Le statistiche riportano i bidoni fuori comune e fuori giro.
- **Filtri dei servizi.** Lo stato a database toglie o rimette tutti i bidoni di uno stato. I servizi stanno in due elenchi, uno per stato. "Filtro Servizio Transponder" riguarda i bidoni "Presente": servizio del database. "Filtro Servizio Atteso" riguarda i bidoni "Non Presente": servizio stimato dai giri, lo stesso dell'icona. La casella "Includi Non Censiti con stima incerta (?)" riguarda i non censiti con il punto di domanda. Accanto a ogni titolo, "Tutti" e "Nessuno" spuntano o tolgono tutte le voci: "Nessuno" sul servizio atteso toglie anche la stima incerta. Non c'è una casella "Includi Non Censiti": il proprietario del progetto l'ha fatta togliere perché doppione dello stato "Non Presente".
- **Elenchi a discesa.** L'elenco dei comuni e quello dell'anno di analisi hanno lo stesso aspetto, per richiesta del proprietario del progetto: stesso riquadro, stesso testo, stessa freccia.
- **Vista per zoom.** La mappa principale mostra una bolla per cantiere fino a zoom 11, una per comune a zoom 12 e 13, i singoli bidoni da zoom 14: l'ha chiesto il proprietario del progetto, perché i gruppi che si vedono siano quelli del territorio. I cantieri arrivavano a zoom 10 e i comuni a zoom 12: il proprietario del progetto li ha voluti, in due richieste, per un livello di zoom in più ciascuno. Vale per qualsiasi numero di bidoni. Da vicino i bidoni che si sovrappongono restano in un cluster di marker. Con più bidoni del limite, da zoom 14 la mappa mostra una bolla per riquadro e i singoli bidoni dell'area inquadrata solo quando tornano sotto il limite. La bolla di un comune sta nel baricentro del comune, per richiesta del proprietario del progetto: così resta ferma al variare dei filtri, in mezzo al confine disegnato. Le bolle dei cantieri e dei riquadri stanno nel punto medio dei loro bidoni. Un click su una bolla la apre: ingrandisce sui suoi bidoni e arriva almeno al livello successivo, i comuni per un cantiere e i bidoni per un comune. Un file senza cantieri né comuni non ha bolle del territorio.
- **Confini dei comuni.** Tutte le mappe, anche quella dei cluster spaziali, disegnano i confini dei comuni serviti: bordo tratteggiato e riempimento appena accennato, nel colore del cantiere. Il proprietario del progetto li ha chiesti poco saturi e poco invasivi. La legenda riporta i quattro colori. I confini non rispondono al mouse e stanno sotto marker, cluster e riquadri.
- **Marker.** Verde a goccia se censito, rosso squadrato se non censito. L'icona interna indica il servizio. Per un non censito è il servizio stimato: il giro prevalente, se una tipologia raggiunge l'80% delle letture, altrimenti un punto di domanda. Un sacchetto ha sempre l'icona dei sacchetti, censito o no. Due bollini scuri con una lettera, negli angoli in alto, segnalano le incongruenze: `C` per una lettura fuori comune, `G` per una fuori giro. Non cambiano colore, forma né icona.
- **Sacchetti.** Si vedono sulla mappa principale e nelle due ricerche, con la loro icona, la loro voce nei filtri e nelle statistiche e un dettaglio loro. Restano fuori dall'analisi dei cluster spaziali: la scheda riguarda i soli contenitori.
- **Dettaglio del bidone.** Un click su un marker, su una riga della tabella dei cluster o su un baricentro apre nella barra laterale la storia dell'RFID: servizio coerente, cambio di servizio, stima per i non censiti, cambio di utenza, letture fuori dal comune del database e fuori dal giro, con dove sono state fatte. In fondo c'è l'istogramma delle letture per anno: usa tutte le letture caricate, non solo quelle del periodo, e distingue le letture storiche da quelle delle antenne. Un anno senza letture resta vuoto, con lo zero in rosso.
- **Popup.** Riporta i dati della lettura, il comune a database, il comune di lettura, il comune del giro se è diverso da quello di lettura, il cantiere e, per una lettura fuori comune o fuori giro, una riga che dice dove era dichiarata e dove è stata fatta.
- **Titolo.** Riporta il periodo, ad esempio "DASHBOARD RFID - ANNO 2025".
- **Favicon.** È il simbolo aziendale. Nell'intestazione non c'è nessun logo, per scelta del proprietario del progetto.

## Dati

Il file delle letture con le antenne ha dieci colonne obbligatorie e sei facoltative. L'ordine e le maiuscole dei nomi non contano. I formati accettati sono CSV, CSV compresso con gzip e Parquet: li riconosce `leggi_tabella()` dall'estensione.

| Colonna | Tipo dopo la validazione | Note |
|---|---|---|
| `giorno_lettura` | POSIXct, fuso UTC | Formato `YYYY-MM-DD HH:MM:SS`. Accettate anche date italiane |
| `targa_veicolo`, `matricola_veicolo` | testo | Relazione 1:1 |
| `RFID` | testo | Si ripete a ogni lettura |
| `presente_a_database` | testo | Solo `Presente` o `Non Presente` |
| `servizio_transponder` | testo maiuscolo | Vuoto per i non censiti. Per un sacchetto censito, che a database non ha servizio, la validazione scrive `SACCHETTI` |
| `servizio_atteso` | testo maiuscolo | Servizio che il mezzo stava raccogliendo, con i nomi del database: dai censiti letti dallo stesso mezzo, per i non censiti, oppure dal calendario. Compilato per ogni lettura, anche dei censiti. L'app accetta anche i nomi del calendario dei file preparati in passato |
| `servizio_giro` | testo maiuscolo, facoltativa | Servizio che il calendario dava al mezzo. Se il file non la riporta la validazione la riempie con `servizio_atteso`, senza avvisi: nei file preparati in passato le due cose coincidevano |
| `id_utenza` | testo | Vuoto per i non censiti |
| `latitudine`, `longitudine` | numerico | Accettata la virgola decimale |
| `volume_previsto` | numerico, facoltativa | Litri. Usata dall'analisi dei cluster |
| `numero_raccolte_annue_previste` | numerico, facoltativa | Usata dall'analisi dei cluster e dal confronto con le letture storiche |
| `comune_da_database` | testo, facoltativa | Vuoto per i non censiti. La colonna `comune` dei file meno recenti vale come questa |
| `comune_giro` | testo, facoltativa | Comune in cui il calendario mandava il mezzo. Compilato per ogni lettura |
| `cantiere` | testo maiuscolo, facoltativa | Ricalcolato dal comune assegnato, cioè dalla posizione, secondo la tabella dei comuni. Quello del file vale solo per le letture fuori dai comuni serviti con un comune del database fuori tabella |
| `comune_lettura` | testo | Comune in cui cade il punto, oppure `FUORI SERVIZIO AZIENDA`. La validazione lo calcola sempre dalle coordinate: se il file lo contiene già, viene ricalcolato. Il CSV dei dati reali lo riporta, calcolato dalla stessa funzione |
| `comune_geografico` | testo | Non sta nel file: lo aggiunge la validazione, dalle coordinate. Vuoto fuori dai comuni serviti |
| `comune_assegnato` | testo | Non sta nel file. Comune geografico, oppure quello del database per le letture dei censiti fuori dai comuni serviti |
| `fuori_comune`, `fuori_giro` | logico | Non stanno nel file. Lettura oltre la tolleranza dal comune del database, per i censiti, e dal comune del giro |

La validazione rifiuta il file se mancano colonne o se ci sono valori non interpretabili. Scarta, con un avviso, le righe prive di un campo essenziale. Riporta i nomi dei comuni alla grafia della tabella dei comuni, senza badare a maiuscole, accenti e apostrofi, e segnala con un avviso i comuni che la tabella non contiene: quelli assegnati alle letture e, con un avviso a parte, quelli dei giri.

**Un file preparato prima di ottobre 2026 ha `comune_lettura` con il comune del calendario e non ha `comune_giro`.** L'app non tenta di riconoscerlo: ricalcola `comune_lettura` dalla posizione e, senza `comune_giro`, non segnala letture fuori giro. Il file va rigenerato con il passo 3.

La geografia sta in due file del pacchetto.

- `inst/extdata/comuni_cantieri.csv`: i 66 comuni serviti, con cantiere e coordinate del centro abitato. È compilato a mano ed è l'unico elenco di comuni e cantieri: 7 comuni per `ASIAGO`, 17 per `BASSANO`, 27 per `CAMPOSAMPIERO`, 15 per `RUBANO`. `VIGONZA` sta sotto `CAMPOSAMPIERO`: l'ha precisato il proprietario del progetto, dopo un primo elenco che lo metteva sotto `RUBANO`. I nomi sono in maiuscolo, con l'apostrofo al posto dell'accento, come nel file dell'azienda: `ROSA'`. L'elenco dell'azienda aveva anche `VALSTAGNA`, comune soppresso nel 2019 e oggi parte di `VALBRENTA`: non ha più un confine suo e non sta in tabella. Il proprietario del progetto lo riporta a `VALBRENTA` nel passo 2 della preparazione dei dati.
- `inst/extdata/comuni_confini.geojson`: i confini degli stessi comuni, uno per ogni riga della tabella e nello stesso ordine. Sono quelli dell'ISTAT, dall'ultimo rilascio della raccolta `guglielmo/geojson-italy`, licenza CC-BY 4.0: ora il rilascio del 14 maggio 2026. Li scarica `scarica_confini_comuni()`, richiamata dallo script `data-raw/prepara_comuni.R`. Ogni comune sta su una riga del file e porta `comune` e `cantiere`, come nella tabella, e `nome_istat`, `codice_istat`, `codice_catastale`, `provincia`, come nella fonte. Servono ai confini disegnati sulle mappe dell'app, alla loro vista iniziale, alla mappa dei comuni del report e al dataset di esempio. `VALBRENTA` ha due poligoni.

I nomi della tabella e quelli della fonte si associano con `chiave_comune()`: `ROSA'` trova `Rosà`. Tra due comuni italiani con lo stesso nome vale quello che contiene il centro abitato della tabella. Se un comune non trova il confine, o il suo centro cade fuori dal confine trovato, `scarica_confini_comuni()` si ferma senza scrivere il file.

Il CSV delle letture storiche è un secondo file, facoltativo, con due colonne: `RFID` e `giorno_lettura`. Lo valida `valida_storico()`: una data non interpretabile ferma il caricamento, le righe incomplete e quelle ripetute vengono scartate con un avviso. I codici RFID devono avere lo stesso formato dell'altro file.

I dataset di esempio sono due, simulati dallo script `data-raw/genera_sample_dataset.R` con un seme fisso.

- `inst/extdata/sample_rfid_dataset.csv`: 6.475 letture con le antenne di 251 RFID, cioè 239 bidoni e 12 sacchetti, da ottobre 2024 a dicembre 2025, cinque mezzi, 50 utenze dei bidoni. Come nei dati reali, il calendario dà un comune a ogni giro: nell'esempio un giro è un turno in un giorno della settimana, i giri sono 41 e lavorano in 32 comuni dei quattro cantieri. Ogni bidone sta nel comune del proprio giro, dentro il confine. Ogni lettura ha servizio atteso, comune del giro, comune di lettura e cantiere. Il servizio atteso ha i nomi del database: i dodici giri del calendario simulato diventano nove servizi. Il comune di lettura viene dalla posizione: nessuna lettura è fuori dai comuni serviti, e 1.218 sono fatte in un comune diverso da quello del giro. I bidoni non censiti sono 37: nel 2025, 18 hanno il servizio stimato e 19 la stima incerta. I sacchetti hanno una lettura ciascuno, perché sono monouso: 8 sono censiti, con le utenze da `SAC001` a `SAC004`, e 4 non censiti. A database hanno solo l'utenza, come escono dal passo 3: niente servizio, volume, raccolte o comune. Li leggono i giri del secco, nel comune del giro. Due sono letti nel 2024, dieci nel 2025. Le letture fuori comune sono 77, di due RFID; quelle fuori giro sono circa 1.200, quasi tutte dei due tag anomali, quello in deposito e quello rimasto su un mezzo.
- `inst/extdata/sample_letture_storiche.csv`: 6.893 letture storiche di 229 RFID, da gennaio 2020 a settembre 2024. 184 RFID sono letti da entrambi i sistemi, 67 solo dalle antenne, cioè 55 bidoni e i 12 sacchetti, 45 solo dal sistema precedente: 44 contenitori e un codice letto male. Il sistema precedente registra una parte dei passaggi, diversa per cantiere, per comune e per anno, e lascia anni interi senza letture. Un contenitore ha metà delle letture storiche sotto un codice alterato: è il caso del bit flip.

Il CSV dei dati reali e quello di esempio hanno anche `origine_servizio_atteso`, che l'app non carica: `censiti` o `calendario`. Nell'esempio le letture sono rade, pochi contenitori per mezzo e per giorno: il servizio atteso viene dai censiti in 2 letture su 6.475, e in tutte coincide con quello del calendario.

Lo script genera prima le letture dei bidoni, poi la geografia e i sacchetti, con semi a parte, poi le letture storiche: una modifica alla geografia, ai sacchetti o alla parte storica non cambia il resto. Anche la quota di passaggi registrata in ogni comune dal sistema precedente ha un seme suo, e così la scelta delle letture storiche con il codice alterato. Aggiungere comuni alla tabella cambia invece il comune dei giri, e con quello posizioni e letture storiche: date, mezzi e servizi delle letture con le antenne restano gli stessi. Il giro di ogni lettura è ricavato senza numeri casuali: `giro_del_mezzo()` dà il servizio e `indice_turno_del_mezzo()` il turno, cioè quello in cui cade la lettura o, fuori orario, il più vicino del mezzo in quel giorno, come fa sui dati reali `associa_servizio_atteso_da_calendario()`. Il comune del giro viene dalla tabella `giri` dello script: una lettura fatta altrove risulta fuori giro. Comune di lettura e cantiere li calcola `aggiungi_geografia()`, dalle coordinate scritte nel file; il nome del servizio lo uniforma `uniforma_servizio()`. Test e report fanno riferimento a questi RFID:

| RFID | Caso |
|---|---|
| `RFD20250901001` | Servizio cambiato: CARTA, SECCO, di nuovo CARTA |
| `RFD20250901050` | Utenza cambiata da `UTZ001` a `UTZ025` |
| `RFD20250915201` | Non censito, quattro letture, stima 100% `SECCO`. Indicatore `GHOST_TAG` |
| `RFD20250920250` | Non censito con stima incerta: due letture, `SECCO` e `CARTA` |
| `RFD20250905100` | Spostato di 17 km: censito a `LIMENA`, prima lettura lì, poi otto letture a `CITTADELLA`, dove lo legge il giro di Cittadella. Fuori comune, mai fuori giro. Indicatore `RELOCATED_BIN`, secondo cluster `FUORI_COMUNE` |
| `RFD20241001301` | Tag fermo in deposito, a `CAMPOSAMPIERO`. Indicatore `DEPOT_STUCK`. Non censito, letto dai giri di tutti i mezzi: stima incerta, e quasi tutte le letture fuori giro |
| `RFD20250203302` | Tag rimasto sul mezzo `VEH004`, letto lungo lo stesso percorso tra `MAROSTICA`, `BASSANO DEL GRAPPA` e `NOVE` qualunque sia il giro. Indicatore `TRUCK_STOWAWAY`. Tutte le letture fuori giro, 69 anche fuori comune |
| `RFD20241003303` | GPS impreciso, 17 cluster. Indicatore `SCATTERED_READS / GPS_NOISE` |
| `RFD20241004003` | Letto dalle antenne con regolarità, senza letture storiche nel 2021 e nel 2023. Comune `LIMENA`, cantiere `RUBANO`. Letture per anno dal 2020: 13, 0, 16, 0, 12, 25 |
| `RFD20241003061` | Codice letto male dal sistema precedente. Censito del secco: 39 letture storiche con il suo codice e 37 con `RFD20241003063`, che differisce per un bit dell'ultima cifra, non è a database e non è mai letto dalle antenne. I due codici non compaiono mai nello stesso giorno. È l'unica coppia sospetta di `gemelli_sospetti()` |
| `00BD00000000000000000005` | Sacchetto censito, utenza `SAC002`: una lettura a `ROANA` nel 2025. I censiti vanno da `…0001` a `…0008` |
| `00BD00000000000000000102` | Sacchetto non censito: una lettura a `ROANA` nel 2025. I non censiti vanno da `…0101` a `…0104` |
| `VEH001`, 24 settembre 2025 | Giro sbagliato: il calendario dava il secco a `LIMENA`, il mezzo ha letto dieci bidoni a `BORGORICCO`: `comune_giro` è `LIMENA`, `comune_lettura` è `BORGORICCO`. Dieci letture fuori giro, nessuna fuori comune. È il giro con più letture fuori giro dell'anno |

Numeri del confronto tra i due sistemi sul dataset di esempio, usati dai test. Periodo di riferimento dal 2 ottobre 2023 al 30 settembre 2024, 457 giorni di antenne, nessuna sovrapposizione. La coorte ha 239 contenitori: 20 senza tipologia, 17 con le raccolte previste stimate. I contenitori del secco sono 84, di cui 79 censiti: 21 mai letti nel periodo di riferimento, 26 con al più una lettura; 65 già in servizio, di cui 7 con al più una lettura. Tasso di lettura del secco dal 23% all'82%. Per cantiere, sul secco, tasso precedente e con le antenne: `ASIAGO` 11% e 90%, `BASSANO` 17% e 67%, `CAMPOSAMPIERO` 27% e 85%, `RUBANO` 19% e 76%. Per ultima lettura, su tutto il file: 251 RFID, 1 fuori comune e 20 fuori giro; per cantiere 33, 66, 92 e 60. Nel solo 2025 gli RFID sono 248, e per cantiere 33, 64, 91 e 60.

### Dati reali

I dati veri dell'azienda non sono nel repository. Gli script in `data-raw/` li preparano, in ordine, e producono i CSV che l'app carica.

| Passo | Script | Legge | Scrive |
|---|---|---|---|
| 1 | `data-raw/01_estrai_tabelle.R` | database aziendale, via ODBC | un CSV per tabella in `data-raw/origine/` |
| 2 | `data-raw/02_importa_tabelle.R` | `data-raw/intermedi/*.rds`. Il codice commentato li ricostruisce da `data-raw/origine/` | tabelle ripulite nell'ambiente di lavoro |
| 3 | `data-raw/03_crea_letture_app.R` | le tabelle del passo 2 | `data-raw/output/letture_app.csv` |
| 4, facoltativo | `data-raw/04_crea_letture_storiche.R` | `data-raw/origine/letture_storiche.csv`, le letture precedenti alle antenne | `data-raw/output/letture_storiche.csv` |

Le regole di trasformazione sono funzioni del pacchetto, in `R/fct_preparazione_letture.R`. Il passo 3 le applica in questo ordine: `filtra_letture_post_test()`, `formatta_rfid()`, `associa_servizio()`, `associa_servizio_atteso_da_calendario()`, `associa_servizio_atteso_dai_censiti()`. La penultima restituisce le colonne del CSV dell'app, l'ultima ne aggiunge due. `comune_da_database` lo scrive `associa_servizio()`, dalla colonna `comune_servizio` dell'anagrafica dei contenitori. `comune_giro` viene dalla colonna `comune` del calendario. Le due colonne di origine devono esserci: il proprietario del progetto ha tolto l'argomento `colonna_comune` e reso obbligatoria la colonna del calendario. `comune_lettura` e `cantiere` vengono dalla posizione, con `aggiungi_geografia()`, la stessa funzione dell'app: le coordinate possono essere ancora testo con la virgola, e `completa_geografia()` le legge come numeri solo per il conto. I nomi dei servizi del calendario sono riportati a quelli del database all'inizio di `associa_servizio_atteso_da_calendario()`, con `uniforma_servizi_calendario()`: un nome che la tabella di conversione non contiene resta com'è e dà un avviso. Per ultima viene `associa_servizio_atteso_dai_censiti()`, che il passo 3 chiama con le tre soglie scritte per esteso: dà ai non censiti il servizio dei censiti letti dallo stesso mezzo e aggiunge `servizio_giro` e `origine_servizio_atteso`. I nomi dei comuni si correggono nel codice commentato del passo 2, sulla colonna `comune_servizio` dell'anagrafica dei contenitori: `LUSINA CONCO` diventa `LUSIANA CONCO`, `VALSTAGNA` diventa `VALBRENTA`. L'app non corregge i nomi: un comune che la tabella non contiene viene segnalato tra i comuni assenti. Il passo 3 compila `servizio_atteso` e `comune_giro` per ogni lettura, anche dei censiti: restano vuoti solo se il mezzo non ha turni in quel giorno, cosa che secondo il proprietario del progetto nei dati reali non accade.

**I passi 3 e 4 oggi scartano i codici che iniziano per `00BD`, cioè i sacchetti**: finché quella riga resta negli script, nel CSV dell'app non ce ne sono. Toglierla è una scelta del proprietario del progetto. Il passo 3 scarta anche i codici che iniziano con dieci zeri.

Il passo 4 non dipende dagli altri. In cima allo script si indicano il nome del file di origine e i nomi delle sue due colonne, che non sono noti a priori. Applica `formatta_rfid()`, così i codici hanno lo stesso formato del passo 3, e la stessa esclusione dei codici che iniziano per `00BD`.

Le cartelle `origine/`, `intermedi/` e `output/` sono escluse da git: contengono solo un file `.gitkeep`. **Non eseguire il passo 1 se non viene chiesto: si collega al database aziendale.** Su questo computer i dati reali possono mancare del tutto: le funzioni si provano con le tabelle sintetiche dei test.

## Architettura

### Flusso dei dati

```
file delle letture              file delle letture storiche (facoltativo)
dal browser o da run_app(letture = , storico = )
   │                               │
   ▼                               ▼
mod_caricamento ──► dati_caricati: tutte le letture validate, con comune_lettura, comune_geografico, comune_assegnato, cantiere, fuori_comune e fuori_giro
   │            └─► storico: letture storiche validate ──► solo al dettaglio del bidone
   │
   ├──► mod_periodo ──► dati: letture del periodo, con la colonna servizio_icona
   │        │
   │        ├──► mod_filtri ──► dati_filtrati: una riga per RFID ──► mod_mappa "mappa_principale"
   │        │              └─► area: coordinate del cantiere e del comune scelti ──► inquadratura della mappa
   │        ├──► mod_ricerca "ricerca_rfid" ──► risultati ──► mod_mappa "mappa_rfid"
   │        ├──► mod_giro "giro" ──► risultati ──► mod_mappa "mappa_giro"
   │        ├──► mod_ricerca "ricerca_utenza" ──► risultati ──► mod_mappa "mappa_utenza"
   │        └──► dettaglio del bidone: output$info_panel in app_server.
   │             L'istogramma per anno usa invece dati_caricati e storico
   │
   └──► mod_cluster_analysis: riceve dati_caricati e il periodo ──► tabella, grafici, mappa, CSV
```

`R/app_server.R` contiene solo questi collegamenti, il titolo e il dettaglio del bidone.

Le funzioni di `R/fct_preparazione_letture.R` stanno fuori da questo flusso: l'app non le richiama mai. Servono a produrre il file CSV in ingresso.

Di `R/utils_storico.R` l'app usa solo la lettura del file e `andamento_rfid()`. Il confronto tra i due sistemi sta in `R/utils_confronto.R` e serve al report `docs/beneficio_antenne.Rmd`: l'app non lo richiama. Il report è uno solo, con parametri: si compila per l'intero territorio, per un cantiere o per un comune, e per un servizio. Lo script `docs/compila_report.R` li compila tutti.

### Moduli

Ogni modulo ha due file: `R/mod_<nome>_ui.R` e `R/mod_<nome>_server.R`.

| Modulo e id usati | Riceve | Restituisce | Input principali |
|---|---|---|---|
| `mod_caricamento` (`caricamento`) | `demo`, `percorsi`: di norma le opzioni di `run_app()` | lista di reactive: `letture` e `storico`, ciascuno con il dataset validato o `NULL` | `file_upload`, `file_storico`, `carica_esempio` |
| `mod_periodo` (`periodo`) | `dati` | lista di reactive: `periodo`, `etichetta`, `dati` | `anno_filtro`, `personalizzato`, `intervallo` |
| `mod_filtri` (`filtri`) | `dati`, `azzera` | lista di reactive: `dati_filtrati` e `area` | `cantiere_check`, `comuni`, `coerenza_check`, `presente_filter`, `servizio_transponder_check`, `transponder_tutti`, `transponder_nessuno`, `servizio_atteso_check`, `atteso_tutti`, `atteso_nessuno`, `includi_stima_incerta`, `reset` |
| `mod_ricerca` (`ricerca_rfid`, `ricerca_utenza`) | `dati`, `tipo` | lista di reactive: `risultati`, `codici` | `testo`, `cerca`, `pulisci` |
| `mod_giro` (`giro`) | `dati` | lista di reactive: `risultati`, `riepilogo`, `messaggio` | `mezzo`, `giorni`, `solo`, `tabella_rows_selected` |
| `mod_mappa` (`mappa_principale`, `mappa_rfid`, `mappa_giro`, `mappa_utenza`) | `dati`, `modalita`, `dati_vista`, `attiva`, `messaggio` | reactive con il marker cliccato: lista con `rfid` e `quando` | `mappa_marker_click`, `mappa_zoom`, `mappa_bounds` |
| `mod_cluster_analysis` (`cluster`) | `dati`, `periodo`, `etichetta` | reactive con il cluster scelto: lista con `rfid` e `quando` | `eps_m`, `min_pts`, `genera`, `tabella_rows_all`, `tabella_rows_selected`, `mappa_marker_click` |

`mod_caricamento` tiene i due file separati: un file storico non valido non tocca le letture con le antenne. `carica_esempio` e `run_app(demo = TRUE)` caricano entrambi i dataset di esempio. Caricare a mano un file di letture toglie le letture storiche di esempio, non quelle caricate a mano. I file indicati in `run_app(letture = , storico = )` sono letti da `leggi_una_volta()`: una volta sola per processo, condivisi tra le sessioni.

`mod_mappa` ha quattro modalità: `"cluster"`, `"rfid"`, `"utenza"`, `"giro"`. La modalità decide come `disegna_marker()` disegna i dati. In modalità `"cluster"`, con più bidoni del limite, il modulo segue `mappa_bounds` e `mappa_zoom` e ridisegna la vista scelta da `scegli_vista()`. Lo stato sta in tre `reactiveVal` interni: `disegnati`, `bolle`, `vista`. In modalità `"rfid"` disegna al massimo le letture di `limita_letture_ricerca()`, in modalità `"giro"` quelle di `limita_letture_giro()`. `mod_giro` ha due funzioni di interfaccia, `mod_giro_sidebar_ui()` e `mod_giro_ui()` con la tabella dei giri; tiene lo stato sul server e torna all'ultimo giorno con letture a ogni cambio delle letture del periodo. Il giro scelto in tabella vale finché la ricerca non cambia. La mappa dei giri riceve come `dati_vista` l'area di `area_giri()`, che comprende i comuni dei giri. `mod_cluster_analysis` ha due funzioni di interfaccia: `mod_cluster_analysis_ui()` per il corpo e `mod_cluster_analysis_sidebar_ui()` per la barra laterale. Grafici e mappa seguono le righe mostrate dalla tabella, tenute nel `reactiveVal` interno `righe_tabella`: `NULL` vuol dire tutte, un vettore vuoto nessuna. Una nuova analisi lo azzera. Le righe comunicate dal browser valgono solo dopo l'attesa di `attesa_righe_tabella()`.

### Scelte di progetto da rispettare

1. **Logica fuori dai moduli.** Calcoli e costruzione di HTML, mappe e grafici stanno nei file `R/utils_*.R`, come funzioni senza reattività e coperte da test. I moduli collegano input e output. Il prefisso `fct_` è riservato alla preparazione dei dati, che non fa parte dell'app: i dati reali e i confini dei comuni.
2. **Stato sul server.** `mod_filtri` e `mod_periodo` tengono lo stato in un `reactiveValues` e lo riallineano agli input. Al caricamento di un nuovo dataset lo stato torna subito ai valori predefiniti, senza attendere il browser. Gli `observeEvent` sugli input usano `ignoreInit = TRUE`.
3. **Servizi filtrati per esclusione.** I filtri memorizzano i servizi deselezionati, non quelli selezionati. Così le scelte restano valide quando l'elenco dei servizi cambia con il periodo. "Nessuno" esclude le voci in elenco e tutti i servizi noti a `servizi_config()`: resta "nessuno" anche in un periodo con voci diverse.
4. **Filtri prima della deduplica.** `maschera_filtri()` agisce su tutte le letture, poi `deduplica_ultimo_rfid()` tiene l'ultima rimasta per ogni RFID.
5. **Mappe aggiornate con proxy.** La mappa di base si disegna una volta. I marker si aggiornano con `leafletProxy()`, solo quando la mappa è pronta e la sua scheda è visibile. Gli aggiornamenti arrivati nel frattempo restano in attesa.
6. **Identificativo dei marker.** Il `layerId` è il numero di riga dei dati disegnati. Il click risale così alla lettura anche quando lo stesso RFID ha più marker.
7. **Marker in SVG.** Ogni marker è un'immagine SVG generata in R e passata come data URI. Colore e forma indicano lo stato, il glifo Font Awesome il servizio.
8. **Icona dei non censiti.** `mod_periodo` aggiunge alle letture la colonna `servizio_icona`. `aggiungi_marker()` la usa al posto di `servizio_transponder`. Per i non censiti è il servizio stimato: lo usano anche filtri e statistiche, tramite `servizio_mostrato()`.
9. **Analisi dei cluster in una funzione sola.** `calcola_analisi_cluster()` è usata dall'app, dalla funzione esportata `genera_analisi_cluster()` e dallo script a riga di comando.
10. **Orari senza fuso.** Date e ore sono lette e scritte sempre in UTC, come se fossero ora locale. Non convertire i fusi orari.
11. **Filtri come maschera.** `maschera_filtri()` restituisce un vettore logico, `deduplica_ultimo_rfid()` sceglie l'ultima lettura tra le righe indicate. Non si crea mai una copia filtrata di tutte le letture: con milioni di righe è la parte più costosa.
12. **Cantieri e posizione filtrati per esclusione, comuni per scelta.** Anche le voci del filtro sulla posizione si memorizzano come escluse, in `coerenza_escluse`. I cantieri seguono la regola dei servizi. I comuni scelti stanno in un vettore: vuoto vuol dire tutti. I comuni di un cantiere deselezionato escono dalla scelta, gli altri restano.
13. **Vista della mappa principale calcolata dal server.** `scegli_vista()` decide dallo zoom: bolle per cantiere, bolle per comune, poi i bidoni. Le bolle le calcola il server, con qualsiasi numero di bidoni. Da vicino il browser riceve tutti i bidoni fino al limite di `limiti_mappa()`; oltre riceve i soli bidoni dell'area inquadrata o una bolla per riquadro, e il messaggio resta sotto il megabyte. Dopo uno spostamento o uno zoom la mappa si ridisegna solo se `vista_da_rifare()` lo dice: sotto il limite succede solo quando lo zoom cambia fascia. Le bolle di cantieri e comuni si disegnano subito, appena lo zoom entra nella loro fascia, secondo `vista_immediata()`: sono poche e non dipendono dall'area inquadrata. Le altre viste aspettano 250 millisecondi, perché una serie di spostamenti dia un ridisegno solo: i bidoni possono essere migliaia. L'ha chiesto il proprietario del progetto, a cui il passaggio tra cantieri e comuni sembrava lento: con l'attesa le bolle vecchie restavano sulla mappa 350 millisecondi dopo la fine dello zoom, senza circa 80. L'area caricata è più larga del 30% di quella inquadrata, così i piccoli spostamenti non ridisegnano niente.
14. **Calcoli per gruppo senza cicli.** Con centinaia di migliaia di RFID un `summarise()` valutato gruppo per gruppo costa decine di secondi. L'analisi dei cluster, l'anagrafica e il confronto tra i sistemi usano le funzioni di `R/utils_gruppi.R`, che lavorano su vettori interi.
15. **Valori ripetuti trattati una volta.** I campi di testo hanno pochi valori ripetuti milioni di volte: maiuscole, pulizia e ricerca del cantiere passano da `per_valori_distinti()`.
16. **Una sola tabella dei comuni.** Comuni e cantieri stanno solo in `inst/extdata/comuni_cantieri.csv`. Il confronto dei nomi passa sempre da `chiave_comune()`.
17. **Elenchi a discesa con lo stesso aspetto.** L'elenco dell'anno è un elenco del browser, quello dei comuni è un selectize a scelta multipla, dentro un `div` con classe `elenco-comuni`. La sezione "Elenchi a discesa della barra laterale" del CSS dà a tutti e due lo stesso riquadro e la stessa freccia, disegnata nel CSS perché quella del browser cambia da un sistema all'altro. Un elenco nuovo nella barra laterale deve usare gli stessi stili.
18. **Un elenco di servizi per stato a database.** I servizi transponder filtrano le sole letture "Presente". I servizi attesi filtrano le sole letture "Non Presente", e per il servizio stimato dell'RFID, non per il giro della singola lettura: un non censito sta sotto una voce sola, il suo servizio stimato oppure la stima incerta. Così voci dei filtri, icone dei marker e statistiche coincidono. Nessuna regola dell'app deve leggere `servizio_atteso` senza guardare lo stato a database: nei dati reali la colonna è compilata anche per i censiti.
19. **Sacchetti riconosciuti dal codice.** `rfid_sacchetto()` è l'unica regola. La validazione dà ai sacchetti censiti senza servizio il servizio `SACCHETTI`; `aggiungi_servizio_icona()` lo dà anche ai non censiti, al posto della stima dai giri, perché un sacchetto non ha una tipologia di rifiuto da stimare. Da lì in poi è un servizio come gli altri: voce nei filtri del suo stato, riga nelle statistiche, icona, legenda. Fanno eccezione l'analisi dei cluster e il report sul beneficio delle antenne, che riguardano i contenitori: `calcola_analisi_cluster()` e il blocco `dati` del report tolgono i sacchetti con `senza_sacchetti()`, prima di ogni conto. Il risultato deve essere quello di un file che non li contiene.
20. **Confini come livello GeoJSON a parte.** `aggiungi_confini()` li disegna con `addGeoJSON()` in un riquadro Leaflet suo, `confini`, sotto marker e forme, senza interazione. `pulisci_mappa()` usa `clearShapes()`, che non tocca i livelli GeoJSON: i confini restano a ogni ridisegno. Il testo GeoJSON lo prepara `geojson_confini()` una volta per sessione, con cinque decimali: pesa circa 125 kB. Colore e stile stanno in `colori_cantieri()` e `stile_confini()`.
21. **Colore del cantiere legato al nome.** `colori_cantieri()` dà un colore a ogni cantiere per nome, non per posizione nell'elenco. Le quattro tinte sono state scelte perché si distinguano a coppie anche con un difetto della visione dei colori, e stiano lontane dal verde, dal rosso e dall'arancio di stato e cluster. Un cantiere nuovo va aggiunto lì, altrimenti è grigio.
22. **Bolla di un comune nel baricentro del comune.** `aggrega_marker()` mette la bolla di un comune nel punto dato da `baricentri_comuni()`: il baricentro dell'area del confine, calcolato una volta per sessione da `baricentro_poligoni()`. La posizione non dipende dai bidoni filtrati. Un comune fuori tabella, o mancante, non ha baricentro: la sua bolla sta nel punto medio dei bidoni, come quelle di cantieri e riquadri.
23. **Il comune di una lettura viene dalla posizione.** `aggiungi_geografia()` trova il comune geografico di ogni lettura e da quello ricava comune di lettura, comune assegnato e cantiere: filtri, bolle e statistiche lavorano sul territorio, non sulle colonne del file. I comuni dichiarati restano nelle loro colonne, per il confronto. La usano la validazione dell'app e la preparazione dei dati: il CSV riporta gli stessi valori che l'app ricalcola al caricamento.
24. **Griglia dei comuni.** Trovare il comune di milioni di punti contro 66 confini, punto per punto, costerebbe decine di secondi. `griglia_comuni()` prepara una volta per sessione una griglia di celle di 55 metri: una cella che nessun confine attraversa vale per intero il comune del suo centro, le altre sono marcate. `indice_comune_geografico()` cerca la cella e fa il controllo esatto, con `indice_comune_esatto()`, solo per i punti delle celle marcate. Il risultato è identico al controllo punto per punto: lo verificano i test.
25. **Incongruenze calcolate una volta, con la tolleranza.** `fuori_comune` e `fuori_giro` sono colonne delle letture, scritte dalla validazione con `oltre_il_comune()`: filtri, marker, popup, dettaglio, statistiche e ricerca dei giri le leggono, senza ricalcolare distanze. La distanza esatta dal confine si calcola solo per i punti vicini al comune dichiarato; per quelli lontani basta la distanza dal suo riquadro. Le due incongruenze sono indipendenti: una lettura può averle tutte e due.
26. **Ricerca dei giri sulle colonne delle letture.** Un giro non è una tabella a parte: è il gruppo delle letture con lo stesso giorno, mezzo, `servizio_giro` e `comune_giro`, cioè con lo stesso giro del calendario. `riepilogo_giri()` li raggruppa con le funzioni di `R/utils_gruppi.R`. La scheda non ha un pulsante «Cerca»: segue gli input, e all'avvio cerca un giorno solo, così la mappa non riceve mai un anno di letture senza che l'utente lo chieda.
27. **Confronto sulla coorte, non sui totali.** Le antenne coprono pochi mesi e una parte dei mezzi: il report misura i due sistemi sugli stessi contenitori, quelli letti dalle antenne, e ognuno sui giorni in cui lo si è potuto osservare. Per il sistema precedente il periodo di riferimento, per le antenne i giorni successivi alla prima lettura di ogni contenitore. La prima lettura non si conta: fa entrare il contenitore nella coorte, e contarla gonfierebbe il ritmo delle antenne. Tassi e ritmi sono rapporti tra somme, non medie di rapporti: un contenitore osservato per pochi giorni pesa poco.
28. **Un report solo, con parametri.** Territorio, cantiere, comune e servizio sono parametri di `docs/beneficio_antenne.Rmd`, non documenti diversi: le regole restano le stesse a ogni livello. Le sezioni che a un livello non hanno senso, come i cantieri nel report di un comune, non vengono scritte. I periodi dei due sistemi si calcolano sempre su tutte le letture, prima di scegliere l'ambito.
29. **Indizi sui codici letti male, non correzioni.** `codici_gemelli()` e `gemelli_sospetti()` segnalano le coppie di codici: nessuna regola dell'app o della preparazione dei dati unisce due codici da sola. La verifica si fa sul posto.
30. **Il calendario non assegna comuni.** `comune_giro` serve a due cose sole: riconoscere le letture fuori giro e descrivere un giro nella scheda «Ricerca Giro». Il comune di una lettura, quello di un non censito e il comune assegnato vengono dalla posizione o dal database, mai dal calendario: un mezzo può sostituirne un altro. Una regola nuova che legga `comune_giro` per dire dove sta un contenitore rimette l'errore che il proprietario del progetto ha fatto togliere.
31. **Due colonne per il comune della posizione.** `comune_lettura` ha la voce `FUORI SERVIZIO AZIENDA`, perché il proprietario del progetto la vuole per filtrare; `comune_geografico` ha il valore mancante. I conti usano `comune_geografico`: con la voce dei fuori servizio al posto del mancante, un «comune più frequente» o una ricerca del cantiere la tratterebbero come un comune. `comune_lettura` si usa dove il valore viene mostrato o esportato.
32. **Servizi uniformati nella preparazione, non nell'app.** `uniforma_servizi_calendario()` converte i nomi del calendario prima che il servizio sia associato alle letture: l'ha chiesto così il proprietario del progetto. La validazione dell'app non converte niente; riconosce comunque i nomi del calendario, perché `tipologie_servizio()` li elenca, e dà loro la stessa icona. La tabella di conversione non è un file a parte: è `tipologie_servizio()`, dove il primo dei `servizi` di una tipologia è il nome uniforme.
33. **Servizio dei non censiti dai censiti dello stesso mezzo.** `associa_servizio_atteso_dai_censiti()` non usa gli orari del calendario per delimitare un giro: quando un mezzo ne sostituisce un altro sono proprio quelli a essere sbagliati. Usa una finestra di tempo attorno a ogni lettura. Vicino al passaggio da un giro all'altro i censiti sono misti, il voto non arriva alla soglia e resta il calendario: la regola non ha bisogno di sapere dove un giro finisce. Ogni censito vota una volta al giorno, così un contenitore letto più volte non pesa di più. La regola sta nella preparazione dei dati: l'app legge `servizio_atteso` com'è.
34. **Servizio atteso e servizio del giro sono due colonne.** `servizio_atteso` è la stima per la lettura e dà icona, filtri e statistiche dei non censiti. `servizio_giro` è il calendario e descrive il giro nella scheda «Ricerca Giro»: se le due cose stessero nella stessa colonna, un non censito con il servizio preso dai censiti finirebbe in un giro a parte, con un «giro previsto» che il calendario non prevedeva. È lo stesso schema di `comune_lettura` e `comune_giro`.

## Mappa del repository

```
trackerfid/
├── CLAUDE.md                     questo file
├── README.md                     guida per chi usa l'app
├── DESCRIPTION                   metadati e dipendenze del pacchetto
├── NAMESPACE                     generato da roxygen2: non modificare a mano
├── .gitignore                    esclude i dati aziendali, le sessioni di R e i file .DS_Store
├── R/                            codice del pacchetto
│   ├── run_app.R                 run_app(): avvio dell'app. Esportata
│   ├── app_config.R              app_sys(), get_golem_config(): generati da golem
│   ├── app_ui.R                  struttura della pagina, schede, barra laterale
│   ├── app_server.R              collegamento dei moduli, titolo, dettaglio del bidone
│   ├── mod_caricamento_ui.R / _server.R        caricamento e validazione dei due CSV
│   ├── mod_periodo_ui.R / _server.R            anno o periodo personalizzato
│   ├── mod_filtri_ui.R / _server.R             filtri laterali e statistiche
│   ├── mod_ricerca_ui.R / _server.R            ricerca per RFID o per utenza
│   ├── mod_giro_ui.R / _server.R               ricerca dei giri di un mezzo: pannello laterale e tabella dei giri
│   ├── mod_mappa_ui.R / _server.R              mappa Leaflet, tre modalità
│   ├── mod_cluster_analysis_ui.R / _server.R   scheda di analisi dei cluster spaziali
│   ├── utils_data_processing.R   lettura, validazione, periodo, filtri, ricerche, storia di un RFID
│   ├── utils_styling.R           tipologie di servizio, colori, marker SVG, cluster di marker, legenda
│   ├── utils_mapping.R           mappa di base, confini dei comuni e disegno dei marker
│   ├── utils_popup.R             popup, pannello di dettaglio, riquadro statistiche
│   ├── utils_dbscan_clustering.R DBSCAN, distanze, dispersione
│   ├── utils_cluster_output.R    tabella di analisi, soglie, indicatori, indice di fiducia
│   ├── utils_cluster_viste.R     tabella interattiva, grafici e mappa dei cluster spaziali
│   ├── utils_storico.R           letture storiche: lettura, conteggi per anno, anagrafica degli RFID
│   ├── utils_confronto.R         confronto tra sistema precedente e antenne sulla coorte, codici gemelli, contenitori fuori comune
│   ├── utils_geografia.R         comuni, cantieri e confini del territorio servito
│   ├── utils_gruppi.R            calcoli per gruppo su vettori, senza cicli
│   ├── utils_mappa_aggregata.R   limiti delle mappe e vista della mappa principale secondo lo zoom
│   ├── utils_comune_geografico.R comune in cui cade una lettura, distanza da un comune, fuori comune e fuori giro
│   ├── utils_giri.R              giri dei mezzi: ricerca, riepilogo, tabella
│   ├── fct_preparazione_letture.R   regole che trasformano i dati aziendali nel CSV dell'app
│   ├── fct_confini_comuni.R      scarica i confini dei comuni e sceglie quelli serviti
│   └── generate_cluster_analysis.R   genera_analisi_cluster(), scrivi_analisi_cluster(). Esportate
├── inst/
│   ├── app/www/custom_style.css  tutti gli stili dell'app
│   ├── app/www/script.js         due gestori: porta in vista il dettaglio del bidone, fa disegnare le mappe tornate visibili
│   ├── app/www/favicon.png       favicon: il simbolo aziendale
│   ├── extdata/                  dataset di esempio, letture storiche di esempio, analisi dei cluster di esempio
│   ├── extdata/comuni_cantieri.csv   comuni serviti, con cantiere e coordinate del centro
│   ├── extdata/comuni_confini.geojson   confini dei comuni serviti, dalla raccolta geojson-italy
│   ├── scripts/generate_cluster_analysis.R   analisi dei cluster da riga di comando
│   └── golem-config.yml          configurazione golem
├── data-raw/
│   ├── genera_sample_dataset.R   genera i due dataset di esempio, simulati
│   ├── genera_dataset_grande.R   genera un dataset sintetico di milioni di letture, per misurare i tempi
│   ├── prepara_comuni.R          scarica i confini aggiornati dei comuni serviti. Richiede la rete
│   ├── 01_estrai_tabelle.R       dati reali, passo 1: estrazione dal database aziendale
│   ├── 02_importa_tabelle.R      dati reali, passo 2: caricamento delle tabelle ripulite
│   ├── 03_crea_letture_app.R     dati reali, passo 3: CSV da caricare nell'app
│   ├── 04_crea_letture_storiche.R   dati reali, passo 4 facoltativo: CSV delle letture storiche
│   ├── origine/                  estrazioni grezze. Esclusa da git
│   ├── intermedi/                tabelle ripulite in .rds. Esclusa da git
│   └── output/                   letture_app.csv, letture_storiche.csv, report compilati sui dati reali. Esclusa da git
├── docs/
│   ├── analisi_cluster.md        regole, colonne, soglie e scelte dell'analisi dei cluster
│   ├── indice_fiducia.Rmd        report sull'indice di fiducia, da compilare in HTML
│   ├── beneficio_antenne.Rmd     report sul beneficio delle antenne: parametri per i file, l'ambito e il servizio
│   └── compila_report.R          compila il report per l'intero territorio, per ogni cantiere e per ogni comune
├── tests/testthat/
│   ├── helper-dati.R             dati di prova condivisi tra i test
│   └── test-*.R                  un file per ogni file utils e fct, più moduli, dataset e script
├── dev/
│   ├── 01_start.R, 02_dev.R, 03_deploy.R   modelli di golem, tenuti come promemoria: nessuno li esegue
│   ├── run_dev.R                 avvia l'app in sviluppo
│   └── misura_prestazioni.R      tempi e memoria di ogni passaggio dell'app su un file
└── man/                          pagine di aiuto generate: non modificare a mano
```

`data-raw/`, `dev/`, `docs/` e questo file sono esclusi dalla costruzione del pacchetto tramite `.Rbuildignore`.

## Indice delle funzioni

Le funzioni esportate sono tre: `run_app()`, `genera_analisi_cluster()`, `scrivi_analisi_cluster()`. Tutte le altre sono interne.

### `R/utils_data_processing.R`

| Gruppo | Funzioni |
|---|---|
| Schema del dataset | `colonne_obbligatorie()`, `colonne_facoltative()`, `colonne_quantita()` per le facoltative numeriche, `colonne_geografiche()`, `colonne_derivate()`, `colonne_dataset()`, `colonne_calendario()` per il servizio del giro, `stati_database()`, `stati_coerenza()`, `fuori_servizio()` per la voce delle letture fuori dai comuni serviti, `percorso_dataset_esempio()` |
| Sacchetti | `rfid_sacchetto()` li riconosce dal codice, `servizio_sacchetti()` dà il nome del loro servizio, `senza_sacchetti()` li toglie dalle letture |
| Lettura e validazione | `carica_dataset()` è il punto di ingresso: chiama `leggi_tabella()` e `validate_dataset()`. `leggi_tabella()` sceglie il formato tra quelli di `formati_file()`: `read_csv_auto()`, `decomprimi_gz()`, Parquet. `leggi_una_volta()` tiene in memoria i file letti da percorso. Ausiliarie: `rileva_separatore()`, `converti_data_ora()`, `converti_numero()`, `pulisci_testo()`, `celle_da_pulire()`, `elenco_righe()`, `errore_validazione()` |
| Periodo | `anni_disponibili()`, `periodo_anno()`, `etichetta_periodo()`, `filtra_periodo()` |
| Servizio stimato dei non censiti | `servizio_prevalente_non_censiti()` applica la regola dell'80%, `aggiungi_servizio_icona()` scrive la colonna `servizio_icona`, `servizio_mostrato()` la legge o, se manca, la calcola |
| Filtri e statistiche | `maschera_filtri()` dice quali letture superano i filtri, `coerenza_letture()` quali sono fuori comune, fuori giro o coerenti, `deduplica_ultimo_rfid()` tiene l'ultima per RFID tra quelle. `ordina_servizi()`, `conta_servizi()`, `ordina_cantieri()`, `conta_cantieri()`, `senza_cantiere()`, `calcola_statistiche()` |
| Ricerche | `analizza_input_ricerca()`, `codice_in()`, `cerca_per_rfid()`, `filtra_ultimo_per_utenza()`, `calcola_bbox()` |
| Storia di un RFID | `analizza_rfid()` decide il caso da mostrare nel dettaglio. Ausiliarie: `posizione_rfid()` per le letture fuori comune e fuori giro, `cronologia_transizioni()`, `stima_servizio_atteso()` |

### `R/utils_styling.R`

| Gruppo | Funzioni |
|---|---|
| Servizi | `tipologie_servizio()` è l'unico elenco di servizi, icone, colori ed emoji. Da lì derivano `servizi_config()`, `info_servizio()`, `tipologia_servizio()`, e la tabella di conversione: `conversione_servizi()` la restituisce, `uniforma_servizio()` la applica |
| Colori | `colori_stato()`, `get_color()`, `genera_colore_cluster()`. Per i cantieri: `colori_cantieri()`, `colore_cantiere()`, e `stile_confini()` per bordo e riempimento dei confini |
| Marker | `svg_marker()` disegna il marker, `get_leaflet_icon()` lo trasforma in icone Leaflet. `svg_segnale()` disegna i bollini `C` e `G`. Ausiliarie: `glifo_fa()`, `svg_glifo()`, `svg_icona_servizio()`, `uri_svg()` |
| Cluster di marker | `js_icona_cluster()` restituisce il JavaScript che colora i cluster |
| Bolle della mappa principale | `svg_bolla()` disegna una bolla con lo stesso aspetto di un cluster, `icone_bolle()` la trasforma in icone Leaflet, `lato_bolla()` ne dà la dimensione |
| Legenda | `html_legenda()` |

### `R/utils_mapping.R`

`mappa_base()`, `geojson_confini()`, `aggiungi_confini()`, `opzioni_cluster()`, `pulisci_mappa()`, `aggiungi_marker()`, `aggiungi_bolle()`, `aggiungi_comuni_giro()` per il contorno dei comuni dei giri, `add_rfid_bounds()`, `disegna_marker()`, `adatta_vista()`. Funzionano sia su una mappa `leaflet()` sia su un `leafletProxy()`. `mappa_base()` inquadra la zona servita e disegna i confini dei comuni.

### `R/utils_popup.R`

| Gruppo | Funzioni |
|---|---|
| Formati | `formatta_data_ora()`, `formatta_numero()`, `conta()` per singolari e plurali, ad esempio "1 bidone" |
| Popup del marker | `create_popup_html()` |
| Dettaglio del bidone | `crea_info_panel()` sceglie tra `blocco_bidone()` e `blocco_sacchetto()`. `blocco_posizione()` aggiunge le letture fuori comune e fuori giro. Ausiliarie: `blocco_info()`, `elenco_cronologia()`, `barre_stima()`, `istogramma_annuale()` |
| Statistiche | `crea_box_statistiche()`, `righe_servizi()`, `righe_cantieri()` |

### `R/utils_dbscan_clustering.R`

`cluster_dbscan()` raggruppa le letture di un RFID. `assegna_cluster()` lo applica a tutti gli RFID e aggiunge `cluster_id`: salta DBSCAN per gli RFID letti in un solo punto. `ordina_cluster()` numera i cluster in ordine di prima lettura. `soglia_arrotondamento_cluster()` dà il numero di letture oltre il quale le coordinate sono arrotondate a 2 metri. Distanze: `raggio_terra_m()`, `proietta_metri()`, `distanza_haversine_m()`, scritta in R base. La dispersione di un cluster è calcolata in `calcola_analisi_cluster()` con `quantile_per_gruppo()`.

### `R/utils_cluster_output.R`

| Gruppo | Funzioni |
|---|---|
| Parametri | `soglie_indicatore()`, `pesi_fiducia()`, `indicatori_config()`, `colonne_analisi_cluster()` |
| Calcolo | `calcola_analisi_cluster()` produce la tabella con una riga per cluster. Usa `classifica_cluster()`, `indice_fiducia()` e `posizione_cluster()` per comune geografico, incongruenza e distanza dal comune dichiarato |
| Esportazione | `nome_file_analisi_cluster()`, `esporta_analisi_cluster()` |
| Ausiliarie | `analisi_cluster_vuota()`, `sequenza_valori()`, `tra_zero_e_uno()` |

### `R/utils_cluster_viste.R`

`tabella_cluster_dt()`, `grafico_indicatori()`, `grafico_dispersione()`, `mappa_cluster()`, `popup_cluster()`. Con troppi cluster grafico a dispersione e mappa disegnano un campione: `limiti_viste_cluster()`, `campiona_cluster()`, `nota_campione()`. `attesa_righe_tabella()` dà l'attesa prima che grafici e mappa seguano le righe della tabella. Ausiliarie: `lingua_dt()`, `inchiostro_grafici()`.

### `R/utils_storico.R`

| Gruppo | Funzioni |
|---|---|
| Lettura e validazione | `carica_storico()` chiama `read_csv_auto()` e `valida_storico()`. `colonne_storico()`, `percorso_storico_esempio()` |
| Letture per anno | `unisci_letture()` mette insieme i due sistemi e aggiunge `fonte`. Da lì: `conta_letture_annuali()`, `griglia_annuale()` con gli zeri dalla prima lettura in poi, `andamento_rfid()` per l'istogramma dell'app, `copertura_annuale()` |
| Anagrafica | `anagrafica_rfid()` dà stato, tipologia, cantiere, comune e raccolte previste di ogni RFID letto dalle antenne |
| RFID non letti dalle antenne | `rfid_solo_storico()` |

### `R/utils_confronto.R`

Confronto tra il sistema precedente e le antenne, per il report. L'app non le richiama.

| Gruppo | Funzioni |
|---|---|
| Letture e periodi | `una_lettura_al_giorno()` tiene una lettura al giorno per contenitore e per sistema. `periodi_confronto()` dà i periodi dei due sistemi e il periodo di riferimento |
| Confronto sulla coorte | `confronto_sistemi()` produce una riga per RFID dell'anagrafica, con le letture dei due sistemi nei giorni osservati. `raccolte_di_riferimento()` completa le raccolte previste dei non censiti. `riepilogo_confronto()` dà contenitori mai letti, ritmo e tasso di lettura, con l'argomento `per` per raggruppare per tipologia, cantiere o comune. `intervallo_jackknife()` dà l'intervallo di confidenza di una statistica fatta di somme |
| Andamento nel tempo | `letture_mensili()` per i due sistemi, `copertura_settimanale()` per le antenne: mezzi attivi, contenitori letti, contenitori nuovi |
| Codici letti male | `codici_gemelli()` trova le coppie di codici che differiscono per un carattere. `gemelli_sospetti()` tiene quelle con un solo codice censito e raccoglie gli indizi: giorni in comune, giorni della settimana, bit, distanza tra i punti di lettura |
| Contenitori lontani dal comune | `rfid_fuori_comune()`: una riga per ogni censito con letture fuori comune |

### `R/utils_geografia.R`

| Gruppo | Funzioni |
|---|---|
| Tabelle | `comuni_cantieri()`, `cantieri()`, `poligoni_comuni()` con i vertici dei confini, `riquadro_zona()`. Lette una volta per sessione di R |
| Confini | `percorso_confini_comuni()`. `anelli_geometria()` e `anelli_geojson()` trasformano un GeoJSON in una tabella di vertici, con poligoni e fori. `dentro_poligoni()` dice se un punto è dentro un confine. `baricentro_poligoni()` dà baricentro e superficie di uno o più poligoni, `baricentri_comuni()` il baricentro di ogni comune, sempre dentro il suo confine |
| Nomi dei comuni | `chiave_comune()` per il confronto, `normalizza_comune()` per la grafia della tabella, `cantiere_di_comune()` |
| Geografia delle letture | `aggiungi_geografia()` compila `comune_lettura`, `comune_geografico`, `comune_assegnato`, `cantiere`, `fuori_comune` e `fuori_giro` di ogni lettura. `geografia_per_gruppo()` dà comune e cantiere di ogni RFID |
| Ausiliaria | `per_valori_distinti()` applica una funzione ai soli valori distinti di un vettore |

### `R/utils_gruppi.R`

Calcoli per gruppo su vettori interi. Il gruppo è un intero da 1 al numero dei gruppi, dato da `indice_gruppi()`. `primo_per_gruppo()`, `ultimo_per_gruppo()`, `ultimo_valido_per_gruppo()`, `somma_per_gruppo()`, `media_per_gruppo()`, `estremi_per_gruppo()`, `quantile_per_gruppo()`, `n_distinti_per_gruppo()`, `piu_frequente_per_gruppo()`. I test li confrontano con il calcolo gruppo per gruppo.

### `R/utils_comune_geografico.R`

| Gruppo | Funzioni |
|---|---|
| Parametri | `tolleranza_confine_m()`, `metri_per_grado()` |
| Confini pronti per i conti | `lati_comuni()`: per ogni comune i vertici, quelli che li precedono, l'anello e il riquadro |
| Comune di un punto | `comune_geografico()` dà il nome, `indice_comune_geografico()` la riga della tabella dei comuni. Dietro: `griglia_comuni()`, `indice_comune_esatto()`, `punti_nel_comune()` |
| Confronto con un comune | `indice_comune()` trova un comune per nome, `distanza_dal_comune_m()` misura la distanza dal suo confine, `oltre_il_comune()` applica la tolleranza |

### `R/utils_giri.R`

`mezzi_presenti()` e `giorni_letture()` danno le scelte della ricerca. `cerca_giri()` trova le letture di un mezzo in un intervallo di giorni. `servizio_del_giro()` dà il servizio del calendario di ogni lettura. `riepilogo_giri()` le riassume, una riga per giorno, mezzo, servizio e comune del giro: colonne in `colonne_riepilogo_giri()`. `letture_del_giro()` dà le letture di una riga del riepilogo. Per la mappa: `limita_letture_giro()`, `area_giri()`, `descrizione_giri()`. `tabella_giri_dt()` costruisce la tabella interattiva.

### `R/utils_mappa_aggregata.R`

| Gruppo | Funzioni |
|---|---|
| Limiti | `limiti_mappa()`: bidoni disegnati uno per uno e letture disegnate dalla ricerca RFID. `limita_letture_ricerca()` |
| Scelta della vista | `scegli_vista()` restituisce `"cantiere"` o `"comune"` da lontano, poi `"tutti"`, `"singoli"` o `"griglia"`. `soglie_zoom()` dà gli zoom a cui cambiano le bolle, `livello_aggregazione()` la fascia di uno zoom, `vista_da_rifare()`, `vista_immediata()` per le viste che si disegnano senza attesa, `descrizione_vista()` |
| Click su una bolla | `zoom_minimo_bolla()` dà lo zoom a cui una bolla si apre, `zoom_del_riquadro()` quello a cui i suoi bidoni entrano nella mappa |
| Bolle | `aggrega_marker()` raggruppa i bidoni e ne conta i censiti. `passo_griglia()` |
| Riquadri | `allarga_riquadro()`, `riquadro_contiene()`, `nel_riquadro()` |

### `R/fct_confini_comuni.R`

Preparazione del file dei confini. L'app non le richiama.

| Funzione | Cosa fa |
|---|---|
| `scarica_confini_comuni()` | Punto di ingresso: trova il rilascio in vigore, scarica il file nazionale dei comuni, sceglie i comuni serviti e scrive `inst/extdata/comuni_confini.geojson`. Con `sorgente` usa un file nazionale già scaricato |
| `origine_confini()` | Indirizzi della raccolta `guglielmo/geojson-italy`: indice dei rilasci e file nazionale dei comuni |
| `rilascio_confini()` | Dall'indice della raccolta, il rilascio in vigore in una data |
| `seleziona_confini_comuni()` | Associa ogni comune della tabella al suo confine e si ferma se uno manca. Il risultato ha tanti comuni quante sono le righe della tabella |
| `scrivi_confini_comuni()` | Scrive il GeoJSON, un comune per riga |

### `R/fct_preparazione_letture.R`

Preparazione dei dati reali, nell'ordine in cui le funzioni vengono applicate.

| Funzione | Cosa fa |
|---|---|
| `filtra_letture_post_test()` | Tiene le letture dei mezzi con test delle antenne positivo, fatte dopo la data del test |
| `formatta_rfid()` | Porta i codici RFID a 10 o 24 caratteri e corregge un carattere letto male tra gli zeri iniziali |
| `associa_servizio()` | Associa servizio, utenza, volume e raccolte del contenitore attivo nel giorno della lettura. In mancanza cerca tra i sacchetti. Calcola `presente_a_database` e rinomina `comune_servizio` in `comune_da_database` |
| `associa_servizio_atteso_da_calendario()` | Sceglie per ogni lettura il turno del mezzo in cui cade, o il più vicino. Un turno che finisce dopo mezzanotte vale anche per il giorno successivo. Dal turno prende il servizio atteso e il comune del giro, dalla colonna `comune` del calendario. Restituisce le colonne del CSV dell'app |
| `uniforma_servizi_calendario()` | Riporta i nomi dei servizi del calendario a quelli del database, con la tabella di conversione. Segnala con un avviso i nomi che la tabella non contiene |
| `completa_geografia()` | Aggiunge `comune_lettura` e `cantiere`, dalla posizione, con `aggiungi_geografia()`. Legge le coordinate come numeri anche se sono testo |
| `associa_servizio_atteso_dai_censiti()` | Dà alle letture dei non censiti il servizio dei censiti letti dallo stesso mezzo nei minuti attorno, se sono abbastanza e concordi; altrimenti lascia quello del calendario. Aggiunge `servizio_giro` e `origine_servizio_atteso` |
| `estrai_rfid()` | Ricava i codici RFID dal testo degli eventi. Non usata dagli script attuali |

### Altri file

| File | Funzioni |
|---|---|
| `R/app_ui.R` | `app_ui()`, `sezione_sidebar()` per i riquadri della barra laterale, `golem_add_external_resources()` |
| `R/app_server.R` | `app_server()` |
| `R/mod_giro_ui.R` | `mod_giro_sidebar_ui()` per la barra laterale, `mod_giro_ui()` per la tabella dei giri |
| `R/mod_ricerca_ui.R` | `mod_ricerca_ui()`, `testi_ricerca()` con le etichette delle due ricerche |
| `R/mod_caricamento_ui.R` | `mod_caricamento_ui()`, `estensioni_accettate()` per i campi di caricamento |
| `R/mod_filtri_ui.R` | `mod_filtri_ui()`, `intestazione_filtro()` per il titolo di un elenco di servizi con "Tutti" e "Nessuno" |
| `R/generate_cluster_analysis.R` | `genera_analisi_cluster()`, `scrivi_analisi_cluster()` |

## Dove intervenire

| Per cambiare | Vai in | Test collegati |
|---|---|---|
| Servizi e giri riconosciuti, icone, colori, emoji | `tipologie_servizio()` in `R/utils_styling.R`. È l'unico punto | `test-utils_styling.R` |
| Tabella di conversione dei servizi: un nome nuovo del calendario, o il nome a cui viene riportato | `tipologie_servizio()`: il nome nuovo va aggiunto ai `servizi` della sua tipologia, dopo il primo, che è il nome uniforme | `test-utils_styling.R`, `test-fct_preparazione_letture.R` |
| Dove i nomi dei servizi vengono uniformati | `uniforma_servizi_calendario()`, chiamata all'inizio di `associa_servizio_atteso_da_calendario()` | `test-fct_preparazione_letture.R` |
| Soglie del servizio atteso dei non censiti: minuti attorno alla lettura, numero minimo di censiti, quota dello stesso servizio | argomenti `minuti`, `minimo_censiti` e `soglia` di `associa_servizio_atteso_dai_censiti()`, scritti nella chiamata di `data-raw/03_crea_letture_app.R`. Sono scelte del proprietario del progetto | `test-fct_preparazione_letture.R` |
| Chi vota per il servizio di un non censito, e quante volte | `associa_servizio_atteso_dai_censiti()`: il vettore `votanti` | `test-fct_preparazione_letture.R` |
| Voce delle letture fuori dai comuni serviti | `fuori_servizio()` in `R/utils_data_processing.R`. Cambiarla cambia il valore scritto nel CSV dei dati reali | `test-utils_comune_geografico.R`, `test-fct_preparazione_letture.R` |
| Verde e rosso dello stato, colori dei cluster di marker | `colori_stato()`. Il JavaScript in `js_icona_cluster()` li legge da lì | `test-utils_styling.R` |
| Forma, bordo o dimensione dei marker | `svg_marker()` e `get_leaflet_icon()` | `test-utils_styling.R` |
| Legenda della mappa | `html_legenda()` | `test-utils_styling.R` |
| Soglia dell'80% per il servizio stimato dei non censiti: icona, filtri, statistiche | argomento `soglia` di `aggiungi_servizio_icona()`, chiamata in `R/mod_periodo_server.R` | `test-utils_data_processing.R` |
| A quali bidoni si applica ogni elenco di servizi, regola della stima incerta | `maschera_filtri()`, e gli stessi criteri in `calcola_statistiche()` | `test-utils_data_processing.R`, `test-moduli.R` |
| Scelte rapide "Tutti" e "Nessuno" dei filtri dei servizi | `intestazione_filtro()` in `R/mod_filtri_ui.R`, `spunta_elenco()` in `R/mod_filtri_server.R`, classi `titolo-filtro`, `azioni-filtro`, `nota-filtro` e `gruppo-servizi` nel CSS | `test-moduli.R`, poi guardarle nel browser |
| Regola che riconosce un sacchetto: prefisso e lunghezza del codice | `rfid_sacchetto()` in `R/utils_data_processing.R` | `test-utils_data_processing.R` |
| Dove i sacchetti restano fuori: analisi dei cluster, report sul beneficio delle antenne | `senza_sacchetti()` in `R/utils_data_processing.R`, chiamata all'inizio di `calcola_analisi_cluster()` e nel blocco `dati` di `docs/beneficio_antenne.Rmd` | `test-utils_data_processing.R`, `test-utils_cluster_output.R`, poi compilare il report |
| Nome, icona, colore ed emoji della tipologia dei sacchetti | `servizio_sacchetti()` per il nome, la voce "Sacchetti" di `tipologie_servizio()` per il resto | `test-utils_styling.R` |
| Testi del dettaglio di un sacchetto | `blocco_sacchetto()` in `R/utils_popup.R` | `test-utils_popup.R` |
| Testo del popup dei marker | `create_popup_html()` | `test-utils_popup.R` |
| Comuni serviti, cantiere di un comune | `inst/extdata/comuni_cantieri.csv`. Per un comune nuovo: una riga con cantiere e coordinate del centro abitato, poi `Rscript data-raw/prepara_comuni.R` per i confini e `Rscript data-raw/genera_sample_dataset.R` per il dataset di esempio | `test-utils_geografia.R`, `test-sample_dataset.R` |
| Confini dei comuni: aggiornarli, cambiare la fonte o le regole con cui i nomi si associano | `scarica_confini_comuni()`, `origine_confini()`, `seleziona_confini_comuni()` in `R/fct_confini_comuni.R` | `test-fct_confini_comuni.R`, `test-utils_geografia.R` |
| Regola del comune di lettura, del comune assegnato e del cantiere di una lettura, regola del fuori comune e del fuori giro | `aggiungi_geografia()` in `R/utils_geografia.R`. Vale per l'app e per il CSV dei dati reali | `test-utils_comune_geografico.R`, `test-fct_preparazione_letture.R`, `test-sample_dataset.R` |
| Tolleranza dal confine oltre la quale una lettura è fuori comune o fuori giro | `tolleranza_confine_m()` in `R/utils_comune_geografico.R`. Vale per letture e cluster; il testo del dettaglio la legge da lì. Poi rigenerare l'analisi dei cluster di esempio | `test-utils_comune_geografico.R`, `test-sample_dataset.R` |
| Come si trova il comune di un punto: passo della griglia, controllo esatto | `griglia_comuni()`, `indice_comune_geografico()` | `test-utils_comune_geografico.R`, che confronta con il controllo punto per punto |
| Voci del filtro sulla posizione, regola con cui si combinano | `stati_coerenza()`, `maschera_filtri()`, interfaccia in `R/mod_filtri_ui.R` | `test-utils_data_processing.R`, `test-moduli.R` |
| Segnali `C` e `G` sui marker | `svg_segnale()` e `svg_marker()` in `R/utils_styling.R`, voci in `html_legenda()` | `test-utils_styling.R` |
| Righe del popup e blocchi del dettaglio sulle incongruenze | `create_popup_html()`, `posizione_rfid()`, `blocco_posizione()`, classi `popup-incongruenza` e `info-nota` nel CSS | `test-utils_popup.R`, `test-utils_data_processing.R` |
| Scheda «Ricerca Giro»: che cosa cerca, colonne e ordine della tabella dei giri | `cerca_giri()`, `riepilogo_giri()`, `tabella_giri_dt()` in `R/utils_giri.R`; modulo in `R/mod_giro_ui.R` e `R/mod_giro_server.R` | `test-utils_giri.R`, `test-moduli.R` |
| Mappa dei giri: contorno dei comuni dei giri, area inquadrata, numero massimo di comuni in evidenza | `aggiungi_comuni_giro()` in `R/utils_mapping.R`, `area_giri()` in `R/utils_giri.R`, classi `scheda-giro` e `tabella-giri` nel CSS | `test-utils_mapping.R`, `test-utils_giri.R`, poi guardarla nel browser |
| Colonne sulla posizione nella tabella dei cluster | `posizione_cluster()` in `R/utils_cluster_output.R` | `test-utils_cluster_output.R` |
| Regola del comune e del cantiere di un RFID | `geografia_per_gruppo()` | `test-utils_geografia.R`, `test-utils_storico.R` |
| Filtri per cantiere, comune e posizione | interfaccia in `R/mod_filtri_ui.R`, stato e scelte in `R/mod_filtri_server.R`, regola in `maschera_filtri()` | `test-moduli.R`, `test-utils_data_processing.R` |
| Aspetto degli elenchi a discesa della barra laterale: riquadro, freccia, comuni scelti | sezione "Elenchi a discesa della barra laterale" di `inst/app/www/custom_style.css` | nessuno: guardarli nel browser |
| Numero di bidoni oltre il quale, da vicino, la mappa passa ai soli bidoni inquadrati o alle bolle per riquadro; letture disegnate dalla ricerca RFID | `limiti_mappa()`, oppure le opzioni `trackerfid.max_marker` e `trackerfid.max_letture_ricerca` | `test-utils_mappa_aggregata.R` |
| Zoom a cui la mappa passa dalle bolle per cantiere a quelle per comune e ai singoli bidoni, dimensione dei riquadri | `soglie_zoom()`: è l'unico punto, lo leggono `livello_aggregazione()` e `zoom_minimo_bolla()`. `passo_griglia()` per i riquadri | `test-utils_mappa_aggregata.R`, `test-moduli.R`, poi guardarla nel browser |
| Che cosa fa il click su una bolla | `zoom_minimo_bolla()`, `zoom_del_riquadro()`, e l'osservatore di `mappa_marker_click` in `R/mod_mappa_server.R` | `test-utils_mappa_aggregata.R`, poi provarlo nel browser |
| Punto in cui sta la bolla di un comune, o di un cantiere | `aggrega_marker()` in `R/utils_mappa_aggregata.R`. Per i comuni il punto viene da `baricentri_comuni()` in `R/utils_geografia.R` | `test-utils_mappa_aggregata.R`, `test-utils_geografia.R` |
| Colori dei cantieri, bordo e riempimento dei confini dei comuni | `colori_cantieri()` e `stile_confini()` in `R/utils_styling.R`, classe `legenda-cantiere` nel CSS. Una palette nuova va controllata a coppie anche per chi non distingue i colori | `test-utils_styling.R`, `test-utils_mapping.R`, poi guardarli nel browser |
| Su quali mappe compaiono i confini, in che ordine rispetto ai marker | `aggiungi_confini()` in `R/utils_mapping.R`, chiamata da `mappa_base()` e da `mappa_cluster()` | `test-utils_mapping.R` |
| Aspetto delle bolle | `svg_bolla()`, `lato_bolla()`, `aggiungi_bolle()`, classe `etichetta-bolla` nel CSS | `test-utils_styling.R`, `test-utils_mapping.R` |
| Attesa prima di ridisegnare dopo uno spostamento, e viste che non la rispettano | `debounce()` in `R/mod_mappa_server.R` per la durata, `vista_immediata()` in `R/utils_mappa_aggregata.R` per le viste disegnate subito | `test-utils_mappa_aggregata.R`, `test-moduli.R`, poi misurarla nel browser |
| Formati di file accettati | `formati_file()`, `leggi_tabella()` | `test-utils_data_processing.R` |
| Cluster disegnati al massimo da grafico e mappa dell'analisi | `limiti_viste_cluster()` | `test-utils_cluster_viste.R` |
| Attesa prima che grafici e mappa seguano i filtri della tabella dei cluster | `attesa_righe_tabella()`, usata in `R/mod_cluster_analysis_server.R` | `test-utils_cluster_viste.R`, `test-moduli.R` |
| Ridisegno delle mappe quando una scheda torna visibile | secondo gestore di `inst/app/www/script.js` | nessuno: provarlo nel browser |
| Istogramma delle letture per anno nel dettaglio | `andamento_rfid()` per i conteggi, `istogramma_annuale()` per l'HTML, classi `anni-*` nel CSS | `test-utils_storico.R`, `test-utils_popup.R` |
| Colonne o regole del file delle letture storiche | `colonne_storico()`, `valida_storico()` | `test-utils_storico.R` |
| Regole del confronto tra sistema precedente e antenne: letture contate, giorni osservati, letture utili, tasso | `confronto_sistemi()`, `riepilogo_confronto()` in `R/utils_confronto.R` | `test-utils_confronto.R` |
| Durata del periodo di riferimento | argomento `giorni` di `periodi_confronto()`; `minimo_giorni_riferimento` nel blocco `dati` del report dice sotto quanti giorni il report si ferma | `test-utils_confronto.R` |
| Raccolte previste dei contenitori non censiti | `raccolte_di_riferimento()` | `test-utils_confronto.R` |
| Tipologia di un RFID nel report | `anagrafica_rfid()` in `R/utils_storico.R`; la soglia dell'80% è quella di `servizio_prevalente_non_censiti()` | `test-utils_storico.R` |
| Ricerca dei codici letti male: quando una coppia è sospetta | `gemelli_sospetti()`: argomenti `quota_giorni` e `distanza_massima_m`, e la riga che calcola `sospetto` | `test-utils_confronto.R` |
| Quali contenitori risultano fuori comune nel report | `rfid_fuori_comune()`; la regola della singola lettura resta in `aggiungi_geografia()` | `test-utils_confronto.R` |
| Soglia del 95% per un anno completo | argomento `quota_completo` di `copertura_annuale()` | `test-utils_storico.R` |
| Testi e grafici del report sul beneficio delle antenne | `docs/beneficio_antenne.Rmd` | nessuno: compilarlo, anche per un cantiere e per un comune |
| Ambito e servizio del report | parametri `cantiere`, `comune`, `servizio`; blocco `dati` del report: `contenitori` è l'ambito, `coorte` l'ambito ristretto al servizio. Le sezioni di cantieri e comuni dipendono da `mostra_cantieri`, `mostra_comuni`, `mostra_mappa` | nessuno: compilarlo ai tre livelli |
| Quali report compila lo script, nomi dei file, cartella | `docs/compila_report.R` | nessuno: eseguirlo sui dati di esempio |
| Soglie del report: contenitori letti con regolarità, comuni con pochi contenitori, letture minime di un giro, righe massime delle tabelle | `soglia_regolari`, `minimo_contenitori`, `minimo_letture_giro`, `massimo_righe_tabella` nel blocco `dati` del report | nessuno: compilarlo |
| Linea degli svuotamenti che un'utenza chiede di norma | parametro `conferimenti_attesi` del report | nessuno: compilarlo |
| Casi e testi del dettaglio del bidone | `analizza_rfid()` per la logica, `blocco_bidone()` per i testi | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Riquadro delle statistiche | `calcola_statistiche()` e `crea_box_statistiche()` | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Colonne del CSV, formati di data, regole di validazione | `colonne_obbligatorie()`, `colonne_facoltative()`, `converti_data_ora()`, `validate_dataset()` | `test-utils_data_processing.R` |
| Dimensione massima del file caricato | opzione `trackerfid.max_upload_mb`, letta in `R/app_server.R`. Predefinito 2 GB | nessuno |
| Un filtro laterale nuovo o modificato | interfaccia in `R/mod_filtri_ui.R`, stato in `R/mod_filtri_server.R`, regola in `maschera_filtri()` | `test-moduli.R`, `test-utils_data_processing.R` |
| Selezione del periodo, preimpostazioni | `R/mod_periodo_ui.R`, `R/mod_periodo_server.R`, `periodo_anno()`, `etichetta_periodo()` | `test-moduli.R` |
| Titolo in alto | `output$titolo` in `R/app_server.R` | nessuno |
| Favicon | `favicon(ext = "png")` in `golem_add_external_resources()`. File `inst/app/www/favicon.png`: quadrato di 192 pixel con il simbolo aziendale su fondo bianco. L'immagine di origine non è nel repository | `test-moduli.R` |
| Aggiungere una scheda | `tabBox` e `conditionalPanel` in `R/app_ui.R`, collegamenti in `R/app_server.R`. Solo se il proprietario del progetto la chiede | `test-moduli.R` |
| Regole di ricerca: separatori, maiuscole | `analizza_input_ricerca()`, `codice_in()`, `cerca_per_rfid()`, `filtra_ultimo_per_utenza()` | `test-utils_data_processing.R` |
| Aspetto della ricerca RFID: evidenza, opacità, riquadri | ramo `rfid` di `disegna_marker()`, `add_rfid_bounds()` | `test-utils_mapping.R` |
| Raggio dei cluster di marker | `opzioni_cluster()` | `test-utils_mapping.R` |
| Sfondo e vista iniziale delle mappe | `mappa_base()` | `test-utils_mapping.R` |
| Parametri predefiniti di DBSCAN | `numericInput` in `R/mod_cluster_analysis_ui.R` e argomenti predefiniti di `calcola_analisi_cluster()` e `genera_analisi_cluster()` | `test-utils_cluster_output.R` |
| Soglie degli indicatori | `soglie_indicatore()` | `test-utils_cluster_output.R` |
| Regole degli indicatori | `classifica_cluster()` | `test-utils_cluster_output.R` |
| Pesi o formula dell'indice di fiducia | `pesi_fiducia()`, `indice_fiducia()` | `test-utils_cluster_output.R` |
| Colonne della tabella dei cluster | `colonne_analisi_cluster()`, `analisi_cluster_vuota()` e l'ultimo `mutate` di `calcola_analisi_cluster()`: i tre devono coincidere. Oggi sono 30 | `test-utils_cluster_output.R` |
| Nomi e colori degli indicatori | `indicatori_config()` | `test-utils_cluster_viste.R` |
| Tabella, grafici o mappa dei cluster spaziali | `R/utils_cluster_viste.R` | `test-utils_cluster_viste.R` |
| Stili grafici dell'app | `inst/app/www/custom_style.css` | nessuno |
| Dataset di esempio | `data-raw/genera_sample_dataset.R`, poi rigenerare i file in `inst/extdata/` | `test-sample_dataset.R`, `test-utils_storico.R`, `test-utils_confronto.R` |
| Velocità di un passaggio su file grandi | misurare prima e dopo con `dev/misura_prestazioni.R` | i test del file modificato |
| Regole di preparazione dei dati reali: mezzi validi, formato degli RFID, associazione al database, servizio atteso | `R/fct_preparazione_letture.R` | `test-fct_preparazione_letture.R` |
| Tabelle estratte dal database, filtri sull'estrazione | `data-raw/01_estrai_tabelle.R` | nessuno |
| Pulizia delle singole tabelle estratte | codice commentato in `data-raw/02_importa_tabelle.R` | nessuno |
| Filtri finali e nome del CSV per l'app | `data-raw/03_crea_letture_app.R`: oggi scarta i codici che iniziano per `00BD` e quelli che iniziano con dieci zeri | nessuno |
| Dipendenze | `DESCRIPTION`, poi `devtools::document()` | `devtools::check()` |

Dopo una modifica alle regole dell'analisi dei cluster vanno aggiornati anche `docs/analisi_cluster.md` e `README.md`. Il report `docs/indice_fiducia.Rmd` contiene controlli che fermano la compilazione se il testo non corrisponde più ai dati.

Il report `docs/beneficio_antenne.Rmd` gira anche sui dati reali, quindi nel testo non ha numeri scritti a mano: ogni numero è calcolato. Una frase nuova deve restare vera con qualsiasi dato, anche se le antenne non leggessero di più: per questo i giudizi sono scelti con una condizione, e i confronti tra comuni usano solo quelli con abbastanza contenitori. Le ipotesi sulle cause e sulle conseguenze sono scritte come ipotesi. Il report non contiene stime in euro. Dopo una modifica alle regole del confronto vanno aggiornati anche il report, la sezione "Come sono fatti i conti" compresa, e `README.md`.

Il report va provato in tre condizioni: sui dati di esempio, per un cantiere e per un comune, e su uno scenario simile ai dati reali, con le antenne attive da poche settimane su una parte dei mezzi e il sistema precedente ancora in uso. Lo scenario si ricava dai dataset di esempio, tenendo le letture con le antenne degli ultimi mesi di tre mezzi e trasformando le altre in letture storiche.

## File grandi

Il dataset reale atteso è di circa 2 milioni di letture l'anno. Ogni modifica deve lasciare l'app veloce a quella scala. Misure su 2 milioni di letture di 100.000 RFID, su un portatile:

| Passaggio | Tempo |
|---|---|
| Lettura del CSV da 320 MB, e validazione | 2,3 s e 3,8 s. Da Parquet 1,3 s e 3 s. Nella validazione 1,3 s sono del comune geografico e delle incongruenze |
| Filtro del periodo e icone dei non censiti | 0,2 s |
| Cambio di un filtro: maschera e ultima lettura per RFID | 0,15 s |
| Disegno della mappa, lato server | meno di 0,1 s, messaggio sotto il megabyte |
| Nel browser, dal click al ridisegno finito | da 0,3 a 0,5 s per filtri, zoom e spostamenti |
| Ricerca di un RFID, dettaglio di un bidone | 0,1 s, 0,03 s |
| Ricerca dei giri: un mezzo in un giorno, tutti i mezzi in un mese | 0,01 s, 0,3 s con il riepilogo. Su tutto l'anno e tutti i mezzi qualche secondo |
| Analisi dei cluster | circa 2 s |
| Report sul beneficio delle antenne, con 2,5 milioni di letture storiche | circa 35 s. Il confronto sulla coorte 1,5 s; la ricerca dei codici gemelli 8 s su codici consecutivi, che sono il caso peggiore |

I tempi dipendono dallo stato del computer: da una sessione all'altra gli stessi passaggi, a codice invariato, sono cambiati fino a una volta e mezza. Per confrontare due versioni vanno misurate di seguito.

Regole da rispettare quando si tocca il codice dei dati:

- Niente `dplyr::summarise()`, `mutate()` o `slice_min()` per gruppo sugli RFID o sulle letture: usare `R/utils_gruppi.R`, oppure ordinare e tenere la prima riga di ogni gruppo.
- Niente `ifelse()` su tutte le letture dove basta applicare un filtro solo quando è attivo: vedi `maschera_filtri()`.
- Niente copia filtrata di tutte le letture a ogni cambio di filtro: maschera logica e indici.
- Niente espressioni regolari con alternative su milioni di celle: vedi `celle_da_pulire()`.
- Niente decine di migliaia di marker al browser in un solo messaggio: vedi `limiti_mappa()` e `limiti_viste_cluster()`.
- Per provare: `Rscript data-raw/genera_dataset_grande.R <cartella>` crea i file, `Rscript dev/misura_prestazioni.R <file>` misura. I file di prova non vanno sotto git.

## Comandi

```r
devtools::load_all()                        # carica il pacchetto dai sorgenti
run_app(demo = TRUE)                        # avvia l'app con i due dataset di esempio
run_app(letture = "percorso", storico = "percorso")   # avvia l'app con i file letti dal disco
source("dev/run_dev.R")                     # avvio usato in sviluppo: rigenera la documentazione e avvia
devtools::test()                            # tutti i test
devtools::test(filter = "utils_cluster")    # solo i file di test che contengono quel testo
devtools::document()                        # rigenera NAMESPACE e man/
devtools::check()                           # controllo completo del pacchetto
rmarkdown::render("docs/indice_fiducia.Rmd")   # compila il report in HTML
rmarkdown::render("docs/beneficio_antenne.Rmd")   # report sul beneficio delle antenne, sui dati di esempio
```

```r
# report sul beneficio delle antenne sui dati reali: l'HTML va in una cartella esclusa da git
rmarkdown::render(
  "docs/beneficio_antenne.Rmd",
  params = list(
    letture = "data-raw/output/letture_app.csv",
    storico = "data-raw/output/letture_storiche.csv"
  ),
  output_dir = "data-raw/output"
)
```

```r
# lo stesso report per un cantiere, per un comune o per un altro servizio
rmarkdown::render(
  "docs/beneficio_antenne.Rmd",
  params = list(cantiere = "BASSANO", servizio = "Carta"),
  output_dir = "data-raw/output"
)
```

```sh
# tutti i report in una volta: territorio, cantieri, comuni. Senza file usa i dati di esempio
Rscript docs/compila_report.R data-raw/output/letture_app.csv data-raw/output/letture_storiche.csv
```

```r
# dati reali: i passi, dalla radice del progetto
source("data-raw/01_estrai_tabelle.R")     # si collega al database aziendale
source("data-raw/03_crea_letture_app.R")   # esegue anche il passo 2
source("data-raw/04_crea_letture_storiche.R")   # facoltativo: letture precedenti alle antenne
```

```sh
# rigenera i due dataset di esempio
Rscript data-raw/genera_sample_dataset.R
# dataset sintetico di 2 milioni di letture, in una cartella fuori da git, e misura dei tempi
Rscript data-raw/genera_dataset_grande.R /percorso/della/cartella
Rscript dev/misura_prestazioni.R /percorso/della/cartella/letture_prova.csv
# scarica i confini aggiornati dei comuni serviti: richiede la rete
Rscript data-raw/prepara_comuni.R
# rigenera l'analisi dei cluster di esempio
Rscript inst/scripts/generate_cluster_analysis.R inst/extdata/sample_rfid_dataset.csv 2025-01-01 2025-12-31 inst/extdata
```

## Convenzioni e vincoli

- **Italiano ovunque.** Nomi, commenti, messaggi, test e documenti sono in italiano.
- **Solo ASCII nel codice R.** Lettere accentate, emoji e simboli nelle stringhe di `R/` vanno scritti come escape Unicode, ad esempio `"\u00e8"` per la e accentata. Nei commenti gli accenti sono ammessi. Verifica: `tools:::.check_package_ASCII_code(".")` deve restituire `character(0)`.
- **Documentazione roxygen.** Le funzioni interne hanno `@noRd`. Nei commenti roxygen i nomi delle funzioni interne vanno tra accenti gravi, mai tra parentesi quadre: un collegamento a una funzione senza pagina di aiuto produce avvisi a ogni avvio dell'app.
- **Stile del codice.** Pipe nativa, colonne riferite con `.data$colonna` dentro dplyr, due spazi di rientro.
- **Test.** Ogni funzione di utilità ha i suoi test. I moduli si provano con `shiny::testServer()`. Una modifica al comportamento va accompagnata dall'aggiornamento dei test.
- **Controllo del pacchetto.** L'esito atteso di `devtools::check()` è 0 errori, 0 note e 1 avviso sul campo `License`.
- **Dipendenze facoltative.** `nanoparquet` sta tra i `Suggests`: serve solo a leggere i file Parquet, e il codice controlla che ci sia prima di usarlo. Anche `geosphere` è tra i `Suggests`: serve solo a due test, che confrontano le distanze del pacchetto con le sue e vengono saltati se manca.
- **File GeoJSON senza pacchetti geografici.** I confini si leggono con `jsonlite` e si disegnano con `ggplot2::geom_polygon()`: il progetto non dipende da `sf`.
- **Licenza.** Il campo `License` di `DESCRIPTION` contiene un segnaposto. La scelta spetta al proprietario del progetto: non compilarlo.
- **Git.** I commit li fa il proprietario del progetto. Non committare se non viene chiesto.
- **Dati aziendali.** Estrazioni, tabelle intermedie e CSV reali stanno solo in `data-raw/origine/`, `data-raw/intermedi/` e `data-raw/output/`, escluse da git. Non copiarli in `inst/` o in `data/`, che entrano nel pacchetto, e non riportarne il contenuto in test, documenti o messaggi. Vale anche per i report compilati sui dati reali: vanno scritti in `data-raw/output/` con `output_dir`, mai lasciati in `docs/`. Lo script `docs/compila_report.R` li scrive in `data-raw/output/report/`.

## Trappole note

- **Mappe in schede nascoste.** Leaflet non disegna bene in un contenitore invisibile. Per questo `mod_mappa` aspetta che la scheda sia attiva. Una mappa nuova deve seguire lo stesso schema, oppure essere ridisegnata per intero con `renderLeaflet()` come fa la mappa dei cluster spaziali.
- **Altezza del contenuto e barra laterale.** A ogni ridimensionamento e a ogni click sul pulsante che chiude la barra, il tema scrive su `.content-wrapper` un'altezza minima pari a quella del contenuto della barra laterale. Qui la barra scorre per conto suo, quindi il CSS la riporta all'altezza della finestra con `!important`. Senza quella regola, chiudendo la barra con i filtri aperti la pagina scorre nel vuoto sotto la mappa.
- **`summarise()` e nomi riusati.** Dentro un `dplyr::summarise()` una colonna appena creata nasconde quella originale nelle espressioni successive. I riepiloghi che servono la colonna originale vanno calcolati prima di sovrascriverla.
- **`dbscan::dbscan()` e matrici.** Una matrice viene letta come coordinate dei punti, non come distanze. Il progetto passa coordinate proiettate in metri.
- **Date nella tabella interattiva.** Le colonne di tipo `Date` slittano di un giorno con i formati data del browser. In `tabella_cluster_dt()` sono convertite in testo.
- **Confini e dataset di esempio.** Lo script del dataset di esempio mette ogni contenitore dentro il confine del suo comune e ricava il comune di lettura dal punto. Dopo un aggiornamento dei confini il dataset va rigenerato: possono cambiare le coordinate di qualche contenitore vicino a un confine e il comune di lettura di qualche lettura. Vanno poi rigenerata l'analisi dei cluster di esempio e rieseguiti i test. Lo stesso dopo aver aggiunto un comune alla tabella o averne cambiato il cantiere: i bidoni dell'esempio vengono redistribuiti tra i comuni del loro cantiere, e cambiano i conteggi per comune e i numeri del confronto tra i sistemi.
- **Confini e pulizia della mappa.** I confini restano sulla mappa perché Leaflet li tiene tra i livelli GeoJSON, che `clearShapes()` non tocca. Disegnati con `addPolygons()` sparirebbero al primo ridisegno dei marker. Per lo stesso motivo `pulisci_mappa()` non deve chiamare `clearGeoJSON()`.
- **Marker assenti da lontano.** All'avvio la mappa principale inquadra tutta la zona, a zoom 10 o meno, e disegna quattro bolle: i marker dei bidoni non ci sono finché non si arriva a zoom 14. Una prova nel browser che conta i marker deve prima ingrandire. Nei test del modulo lo zoom si imposta con `session$setInputs(mappa_zoom = )`.
- **`VALSTAGNA` non è in tabella.** È un comune soppresso: il passo 2 della preparazione lo riporta a `VALBRENTA`, ma solo nella colonna `comune_servizio` dei contenitori. Se un file lo contiene ancora, ad esempio perché compare nel calendario dei giri, l'app lo tratta come ogni comune fuori tabella: avviso al caricamento, nessun confine e nessun baricentro, e per un comune del giro nessuna lettura fuori giro. Il riconoscimento nell'app, aggiunto e poi tolto, non va rimesso senza che il proprietario del progetto lo chieda.
- **Baricentro fuori dal confine.** Il baricentro dell'area di un comune dalla forma irregolare può cadere fuori dal suo confine: succede a `POVE DEL GRAPPA` e a `TEOLO`. `baricentri_comuni()` usa allora il baricentro della parte più grande, se il comune ha più poligoni, e altrimenti il centro abitato della tabella dei comuni. Dopo aver aggiunto un comune conviene guardare dove cade la sua bolla.
- **Dati di esempio citati per nome.** Test e report fanno riferimento a RFID e conteggi precisi. Cambiare il seme o le regole dello script del dataset li fa fallire: vanno aggiornati insieme.
- **Compilazione dei report.** Vanno compilati dalla cartella `docs/`, senza l'opzione `intermediates_dir`, che produce avvisi spuri. `output_dir` invece si può usare.
- **Confronto tra i sistemi e coorte.** Il confronto parte dagli RFID letti dalle antenne, quindi per le antenne la quota di contenitori letti è il 100% per costruzione: non è un risultato e non va scritto come tale. Per lo stesso motivo le letture delle antenne si contano dal giorno dopo la prima: una regola nuova che conti anche la prima gonfia il risultato, tanto più quanto più breve è il periodo delle antenne.
- **Contenitori nuovi e contenitori mai letti.** Un contenitore della coorte senza letture storiche può essere stato consegnato da poco. Il report non lo dà per mai letto: accanto a ogni conto riporta quello sui contenitori già in servizio, che hanno letture storiche prima del periodo di riferimento. Una frase sui contenitori mai letti deve tenere conto di tutti e due i numeri.
- **Ordine dell'anagrafica.** `anagrafica_rfid()` e `confronto_sistemi()` danno gli RFID nell'ordine della prima lettura con le antenne, non in ordine alfabetico. Nei test i valori attesi vanno cercati per codice.
- **Codici consecutivi.** Con codici RFID consecutivi, come nei file di prova sintetici, le coppie a un carattere di distanza sono milioni: `gemelli_sospetti()` lavora per questo su vettori, senza unire tabelle. Nei dataset di esempio i codici sono quasi tutti lontani tra loro.
- **Titolo del report e parametri.** Il sottotitolo del report è scritto nell'intestazione YAML, che viene valutata prima dei blocchi di codice: può usare solo `params`, non gli oggetti calcolati dopo.
- **Numeri preceduti da un articolo.** "I 8 comuni" e "dei 84 contenitori" sono sbagliati, e il numero cambia con i dati. Nel report un numero non segue mai un articolo o una preposizione articolata: "comuni in questa condizione: 8". Per le percentuali c'è `con_articolo()`.
- **Codice nei report e `highlight.js`.** Con l'evidenziazione predefinita di R Markdown un blocco di codice nel testo fa incorporare `highlight.js`, che su questo computer arriva troncato e dà un errore JavaScript nella pagina. `beneficio_antenne.Rmd` usa `highlight: tango`, che non richiede script.
- **`favicon()` e test dell'interfaccia.** golem mette il favicon nella sezione `head`, che `as.character(app_ui(NULL))` non riporta. Per controllarla nei test serve `htmltools::renderTags(app_ui(NULL))$head`.
- **Intestazioni delle tabelle DT.** L'argomento `colnames` di `DT::datatable()` cambia i nomi che `formatStyle()` deve usare. Nel report le colonne sono rinominate prima, nel data frame.
- **Output di tipo `reactive()` nei test.** Un output assegnato con `reactive()`, come quelli che pilotano un `conditionalPanel`, non si legge da `shiny::testServer()`: dà errore. Nei test si legge il reactive interno, ad esempio `con_geografia()` in `mod_filtri`.
- **Elenco a scelta multipla senza scelte.** Con nessun comune scelto `input$comuni` è `NULL`, non un vettore vuoto: l'osservatore usa `ignoreNULL = FALSE`.
- **Attese nei test della mappa.** Il ridisegno dopo uno spostamento parte dopo 250 millisecondi: nei test va fatto passare il tempo con `session$elapse()`. Fanno eccezione le bolle di cantieri e comuni, disegnate appena cambia `mappa_zoom`: lì il test controlla la vista subito dopo `session$setInputs()`.
- **Bolle troppo grandi da lontano.** Con numeri alti le bolle arrivano a 72 pixel a qualsiasi zoom, e da lontano coprono buona parte del territorio. Il proprietario del progetto lo sa e ha scelto di lasciarle così: `lato_bolla()` non va legato allo zoom senza che lo chieda.
- **Numeri tondi.** `format(1e5, big.mark = ".")` dà `"1e+05"`. `formatta_numero()` usa `scientific = FALSE`: per i conteggi usare sempre quella.
- **Servizio atteso e servizio del giro.** `servizio_atteso` di un non censito può non essere quello del calendario: per dire che cosa era previsto serve `servizio_giro`, oppure `servizio_del_giro()` su dati che possono non avere la colonna. Raggruppare i giri per `servizio_atteso` li spezza.
- **Dataset di esempio e servizio dai censiti.** Nell'esempio i mezzi leggono due o tre contenitori al giorno: cinque censiti in mezz'ora non ci sono quasi mai, e la regola scatta in 2 letture. Per provarla servono le letture sintetiche dei test, con un giro fitto. Un caso dimostrativo nell'esempio cambierebbe letture e conteggi: va aggiunto solo se il proprietario del progetto lo chiede.
- **Servizio atteso sui censiti.** Nei dati reali `servizio_atteso` è compilato per ogni lettura. Una regola che legge quella colonna senza guardare `presente_a_database` tratta i censiti come non censiti: è l'errore che avevano i filtri e le statistiche. Filtri, statistiche, stima del pannello di dettaglio e composizione dei giri nell'analisi dei cluster contano le sole letture "Non Presente".
- **Sacchetti nell'esempio, non nei dati reali.** Il dataset di esempio contiene 12 sacchetti, per scelta del proprietario del progetto, che vuole vedere come l'app li tratta. Nei dati reali i passi 3 e 4 li scartano ancora. Per questo i conteggi dell'esempio vanno letti come 239 bidoni più 12 sacchetti: i test sulle regole dei bidoni, come volume, raccolte e comune del database, li escludono con `rfid_sacchetto()`.
- **Sacchetti fuori dalle analisi.** L'analisi dei cluster e il report sul beneficio delle antenne tolgono i sacchetti con `senza_sacchetti()`. Le funzioni di `R/utils_storico.R` non lo fanno da sole: `anagrafica_rfid()`, `unisci_letture()` e `andamento_rfid()` servono anche al dettaglio di un sacchetto nell'app, che ne mostra l'istogramma. Un'analisi nuova sui contenitori deve togliere i sacchetti all'inizio, come fa il report. Mappa, ricerche, filtri e statistiche della barra laterale invece li comprendono: per questo sulla mappa del 2025 ci sono 248 RFID e nell'analisi dei cluster 238.
- **Due comuni dichiarati e tre dalla posizione.** `comune_da_database` e `comune_giro` sono dichiarati. `comune_lettura` e `comune_geografico` vengono dalla posizione e differiscono solo fuori dai comuni serviti: voce `FUORI SERVIZIO AZIENDA` il primo, valore mancante il secondo. `comune_assegnato` è quello che usano filtri e bolle. Una regola nuova deve scegliere quale le serve: per dire dove sta un bidone il geografico, per confrontare con l'anagrafica o con il calendario i dichiarati. `geografia_per_gruppo()` usa il database per i censiti e la posizione per i non censiti.
- **`comune_lettura` ha cambiato significato.** Fino a ottobre 2026 era il comune del calendario, oggi è quello della posizione; il comune del calendario è `comune_giro`. In una versione vecchia del codice, o in un file vecchio, il nome inganna.
- **Coordinate come testo nella preparazione dei dati.** Nelle tabelle aziendali latitudine e longitudine possono essere testo con la virgola decimale fino alla scrittura del CSV. Una funzione di preparazione che fa conti sulla posizione deve prima passare da `converti_numero()`, come fa `completa_geografia()`.
- **Cantiere di una lettura e cantiere di un RFID.** Il `cantiere` di una lettura è quello del comune in cui cade; quello di un RFID in `anagrafica_rfid()` e nella tabella dei cluster è quello del comune dichiarato. Per un bidone spostato in un altro cantiere i due sono diversi: per ultima lettura i bidoni per cantiere sono 33, 66, 92 e 60, in anagrafica 33, 66, 91 e 61.
- **Colonna `letture` dentro `tibble()`.** In `riepilogo_giri()` il conteggio delle letture si chiama `letture`, come il data frame: dentro `dplyr::tibble()` la colonna appena creata nasconde il data frame nelle espressioni successive. I valori che servono le letture sono calcolati prima.
- **Click sulle righe di una tabella DT nelle prove.** La selezione di una riga parte dagli eventi del mouse: un `click()` di jQuery non basta, servono `mousedown`, `mouseup` e `click` veri.
- **Giri del dataset di esempio.** Nell'esempio il comune del giro viene dal calendario simulato: le letture fatte da un giro in un altro comune, come quelle dei due tag anomali e di qualche non censito letto di passaggio, sono fuori giro. È voluto. Il giro sbagliato dimostrativo è scritto a mano nello script: cambiarne giorno o turno richiede di aggiornare test e documenti.
- **Comuni con più poligoni nel contorno del giro.** `aggiungi_comuni_giro()` separa gli anelli con un valore mancante, usando il numero dell'anello di `lati_comuni()`: dedurre la fine di un anello dai vertici non basta, `VALBRENTA` ha due parti.
- **Statistiche e servizio stimato.** `calcola_statistiche()` riceve una riga per RFID: il servizio stimato va calcolato prima, su tutte le letture del periodo, con `aggiungi_servizio_icona()`. Se la colonna `servizio_icona` manca, `servizio_mostrato()` la calcola sulle sole righe ricevute, e la stima può risultare diversa.
- **Elenco di caselle senza etichetta.** Con `label = NULL` Shiny lascia un'etichetta nascosta e sposta le voci 10 pixel più in alto: finirebbero sopra la riga che sta sotto il titolo. La regola CSS su `.gruppo-servizi .shiny-options-group` lo corregge.
- **Righe della tabella dei cluster dopo una nuova analisi.** `input$tabella_rows_all` riporta le righe dell'ultima tabella disegnata dal browser, e la tabella non si ridisegna mentre la sua vista è nascosta. Dopo una nuova analisi quegli indici sono di un'altra tabella: usati così come sono danno righe vuote, un avviso di Leaflet e l'errore "Bounds are not valid" nel browser. Per questo `righe_tabella` viene azzerato a ogni nuova analisi.
- **Tabella dei cluster senza righe.** Se il filtro della tabella non lascia righe, `input$tabella_rows_all` arriva come `NULL`, lo stesso valore di quando la tabella non è ancora stata disegnata. Lo stesso elenco vuoto arriva per qualche centesimo di secondo a ogni ridisegno della tabella: 50 millisecondi sul file da 2 milioni di letture. Per questo il modulo legge l'input dopo un `debounce()` di `attesa_righe_tabella()` millisecondi e tratta come "nessuna riga" solo l'elenco vuoto che dura. Nei test del modulo va fatto passare il tempo con `session$elapse()`.
- **Mappa ridisegnata mentre è nascosta.** Se una nuova analisi finisce mentre la vista "Mappa" dei cluster è nascosta, perché l'utente ha cambiato vista durante il calcolo, Leaflet tiene i dati in attesa di un ridimensionamento. Al ritorno sulla vista il ridimensionamento non arriva, perché la misura è la stessa di prima, e la mappa resterebbe grigia. Il secondo gestore di `script.js` lo chiede a ogni scheda che torna visibile. Va legato a `shown.bs.tab`: Bootstrap lancia l'evento con quello spazio dei nomi, e un gestore legato al solo `shown` non lo riceverebbe. Legato così riceve anche lo `shown` senza spazio dei nomi con cui Shiny mostra un `conditionalPanel`.
- **Eccezione di Leaflet nella console sulla mappa dei cluster.** A ogni ridisegno la mappa dei cluster scrive nella console del browser un'eccezione `clearRect`: viene dal disegno su canvas della mappa precedente, appena rimossa, e non ha effetti visibili.
- **Layer Leaflet senza righe.** `addCircleMarkers()` con vettori vuoti dà errore. La mappa dei cluster sceglie gli indicatori da disegnare dopo il campione, non prima.
- **Mappe con ggplot nei report.** Con `coord_quickmap()` il grafico ha proporzioni fisse e viene centrato nella figura: se la figura è più larga del necessario il titolo esce dal bordo. L'altezza della figura della mappa è calcolata dalle proporzioni della zona.
- **File di `testthat` dopo un test fallito.** Un test fallito lascia `tests/testthat/_problems/` e `tests/testthat/testthat-problems.rds`: vanno eliminati, non fanno parte del progetto.

## Documenti collegati

| Documento | Contenuto |
|---|---|
| `README.md` | Uso dell'app, formato del file, file grandi, differenze rispetto alle specifiche |
| `docs/analisi_cluster.md` | Analisi dei cluster spaziali: colonne, indicatori, indice di fiducia, posizione e comuni dichiarati, scelte e limiti |
| `docs/indice_fiducia.Rmd` | Report sull'indice di fiducia, con esempi svolti e prove di efficacia |
| `docs/beneficio_antenne.Rmd` | Report sul beneficio delle antenne: quanto leggeva il sistema precedente i contenitori che le antenne leggono oggi, tasso di lettura a parità di tempo, ipotesi sulle cause, contenitori fuori comune, non censiti, codici letti male, giri diversi dal calendario. Per l'intero territorio, un cantiere o un comune, e per servizio |
| `docs/compila_report.R` | Script che compila il report a tutti i livelli |
