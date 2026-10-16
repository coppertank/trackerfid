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
- Per un non censito servizio, utenza e comune non sono a database. Il comune è quello in cui viene letto. Il servizio si stima dal servizio atteso delle sue letture, cioè dai giri che lo hanno letto. Uno stesso mezzo può fare più giri in un giorno, di rifiuti diversi, ma non ci sono mezzi a doppia vasca: ogni giro raccoglie una sola tipologia. Per due giorni, a ottobre 2026, il servizio atteso di un non censito è venuto dai censiti letti dallo stesso mezzo nella mezz'ora attorno: il proprietario del progetto ha poi riscritto la preparazione dei dati senza quella regola, e la funzione `associa_servizio_atteso_dai_censiti()` non c'è più.
- Ogni lettura ha tre servizi. Il **servizio a database**, `servizio_transponder`, è il servizio del contenitore secondo l'anagrafica: c'è solo per i censiti. Il **servizio atteso**, `servizio_atteso`, è quello che il giro del mezzo lasciava aspettare, con il nome del database. Il **giro**, `servizio_giro`, è quello che il calendario dava al mezzo, con il nome del calendario: `CARTA/CARTONE PAP` e `CARTA CONT.STRADALI` contro `CARTA`. Il giro resta scritto come nel calendario: il secco porta a porta, `SECCO PAP`, per l'azienda si comporta in modo diverso dagli altri giri del secco, e il proprietario del progetto vuole poterlo filtrare. Il servizio atteso ha il nome della tipologia del giro, in maiuscolo: una **tabella di conversione** lo ricava dal nome del calendario.
- Nei **giri dei servizi**, quelli in cui non è chiaro quale rifiuto si raccolga, oggi `SERVIZI MERCATI` e `SERV.MANIFESTAZIONE`, il servizio atteso è il servizio a database più frequente tra le letture dei censiti di quel giro, in quel giorno: un mezzo che non è uno scarrabile raccoglie un rifiuto solo alla volta. Con nessun censito, o con due servizi ugualmente frequenti, resta il giro. La regola e l'elenco, destinato a crescere, sono del proprietario del progetto.
- Il proprietario del progetto ha annunciato che passerà la struttura del calendario dei giri non pianificati, quelli a chiamata e quelli dei mezzi scarrabili: fino ad allora la regola del servizio atteso resta quella descritta qui.
- Una lettura è **coerente** se il servizio a database e il servizio atteso sono dello stesso rifiuto: `coerenza_servizio` vale `SI` o `NO`. Un contenitore della carta letto in un giro del secco non è coerente: è stato letto in mezzo a contenitori di un altro rifiuto, oppure il mezzo non faceva il giro del calendario. Un non censito non ha un servizio a database: la sua coerenza manca. Un censito letto in un giro `PULIZIA TERRIT.` non è coerente, per scelta del proprietario del progetto.
- Oltre ai bidoni le antenne leggono i **sacchetti**. Non hanno una tipologia di rifiuto, ma si riconoscono dal codice RFID: 24 caratteri con il prefisso `00BD`, contro i 10 caratteri di un bidone. Un sacchetto è **monouso**: il mezzo lo legge quando lo raccoglie e, una volta svuotato, non può più essere letto. Ha quindi una lettura sola. Il proprietario del progetto vuole poterli vedere sulla mappa, ma li tiene fuori dall'analisi dei cluster, e il più delle volte li toglierà dal file delle letture.
- L'obiettivo finale dell'analisi è **censire a mano i bidoni che oggi non sono a database**. Gli operatori cercano: bidoni non censiti, cambi di servizio o di utenza, bidoni spostati, tag anomali (smarriti, fermi in deposito, rimasti su un mezzo).
- Ogni lettura ha un **comune di lettura**, il comune in cui è stata fatta, e fino a due **comuni dichiarati**: quello del database, per i censiti, e quello del giro. Il proprietario del progetto vuole che il comune di una lettura venga da dove è stata fatta, non dal calendario, e che le differenze con i comuni dichiarati saltino all'occhio. Le dice la **coerenza del comune**, `coerenza_comune`, con la regola che ha dato il proprietario del progetto: per un censito il comune di lettura si confronta con quello del database, e se differiscono la lettura è **fuori comune**, di solito un bidone spostato; per un non censito si confronta con il comune del giro, e se differiscono la lettura è **fuori giro**, di solito un mezzo che non faceva il giro del calendario. Se i due comuni coincidono la lettura è coerente; se manca il comune dichiarato, o quello di lettura, la coerenza è ignota. Un censito non è mai fuori giro.
- Nei dati reali il comune di lettura arriva con le letture, dal campo `IndirizzoLocalizzazione` del sistema di localizzazione dei mezzi, già calcolato dalla posizione: il proprietario del progetto l'ha detto a ottobre 2026. Il 15 ottobre 2026 ha fatto togliere ogni calcolo sulla posizione: l'app non ricava più il comune dai confini, fuori comune e fuori giro non si misurano più con la distanza dal confine, e non ci sono più la tolleranza di un chilometro né il margine di 100 metri che dava il comune più vicino ai punti appena fuori dal territorio. I nomi dei comuni arrivano con l'accento, `ROSÀ`, e la tabella dei comuni del pacchetto li scrive allo stesso modo.
- Il territorio servito è diviso in quattro **cantieri**: `ASIAGO`, `BASSANO`, `CAMPOSAMPIERO`, `RUBANO`. Ogni comune appartiene a un cantiere. Gli operatori filtrano le letture per cantiere e per comune.
- Le antenne sui mezzi sono recenti. Prima un altro sistema registrava solo RFID e istante della lettura, senza mezzo e senza posizione: sono le **letture storiche**. L'operatore leggeva a mano il tag di ogni contenitore. L'azienda vuole misurare quanto le antenne leggono in più, anche per la fatturazione.
- Nei dati reali, a ottobre 2026, le letture storiche vanno dal primo gennaio 2025 a oggi e quelle con le antenne dal primo agosto 2026: le antenne sono montate solo su una parte dei mezzi, e i due sistemi lavorano insieme. Molti contenitori letti dalle antenne non hanno nessuna lettura precedente con lo stesso codice.
- Il proprietario del progetto vuole mostrare ai soci dell'azienda il beneficio delle antenne con un report che scrive lui. Il report `docs/beneficio_antenne.Rmd`, con lo script che lo compilava e le funzioni del confronto tra i due sistemi, è stato tolto dal repository su sua richiesta: non va ricreato, e al pacchetto non vanno aggiunte funzioni pensate per un report se non le chiede. Il **secco** è l'unico servizio che l'azienda fattura. Per il secco le raccolte previste sono di norma 26 l'anno; un'utenza ne chiede di solito una decina.
- Il **bit flip**: un contenitore tenuto sotto controllo compariva nelle letture storiche con due codici, quello giusto e uno diverso per un carattere, in parti quasi uguali. Con il codice sbagliato il database non lo riconosce. Il dataset di esempio non contiene più un caso così: il proprietario del progetto l'ha fatto togliere insieme al vecchio report. La preparazione dei dati corregge già l'errore quando cade negli zeri iniziali del codice, vedi `formatta_rfid()`; nelle cifre che contano non si riconosce da solo.

## Glossario

| Termine | Significato | Nel codice |
|---|---|---|
| Lettura | Una riga del CSV: un RFID letto da un mezzo in un istante e in un punto | righe del dataset |
| Censito, non censito | RFID presente o assente nel database aziendale | `presente_a_database`: `"Presente"` o `"Non Presente"` |
| Servizio a database, servizio transponder | Tipo di rifiuto del contenitore secondo il database. Sei valori: `SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE`. Vuoto per i non censiti e per i sacchetti. Dà icona, filtro e statistiche dei censiti, la cronologia del servizio nel dettaglio e i cambi di servizio nei cluster | `servizio_transponder`, `colonne_contenitore()` |
| Servizio atteso | Servizio che il giro del mezzo lasciava aspettare, con il nome della tipologia del giro: per le sei tipologie di rifiuto il nome del database, ad esempio `SECCO`. Nei giri dei servizi è il servizio a database più frequente tra i censiti letti nel giro. C'è su ogni lettura. Per un non censito dà il servizio stimato | `servizio_atteso`, `associa_servizio_atteso_da_calendario()`, `servizio_atteso_dal_giro()` |
| Servizio del giro, giro previsto | Giro che il calendario dava al mezzo al momento della lettura, con il nome del calendario, senza conversioni: `SECCO PAP`, dove PAP significa porta a porta, è una voce diversa da `SECCO CONT.STRADALI`. Ha un filtro suo e descrive il giro nella scheda «Ricerca Giro». In un file che non lo riporta vale il servizio atteso | `servizio_giro`, `servizio_del_giro()` |
| Giri dei servizi | Giri in cui non è chiaro quale rifiuto si raccolga: `SERVIZI MERCATI` e `SERV.MANIFESTAZIONE`. Il servizio atteso viene dai censiti letti nel giro | `giri_servizio_dai_censiti()`, tipologia «Servizi» |
| Coerenza del servizio | Dice se servizio a database e servizio atteso di una lettura sono dello stesso rifiuto. Due valori, `SI` e `NO`, mostrati come «Coerente» e «Non coerente»; manca se manca uno dei due servizi, come per ogni non censito. Sta nel CSV, e l'app la ricalcola | `coerenza_servizio`, `coerenza_servizio()`, `stati_coerenza()`, `etichette_coerenza()` |
| Coerenza del comune | Comune di lettura a confronto con il comune dichiarato: quello del database per un censito, quello del giro per un non censito. Quattro valori: `Coerente`, `Fuori comune`, `Fuori giro`, `Coerenza ignota`. I nomi si confrontano con `chiave_comune()`. Sta nel CSV, subito dopo `comune_lettura`, e l'app la ricalcola | `coerenza_comune`, `coerenza_comune()`, `stati_coerenza_comune()`, `incongruenze_comune()` |
| Tabella di conversione | Riporta i nomi dei giri del calendario al nome della loro tipologia, in maiuscolo: `SECCO PAP` diventa `SECCO`, `SERVIZI MERCATI` diventa `SERVIZI`, `PULIZIA TERRIT.` diventa `PULIZIA TERRITORIO`. Serve al servizio atteso e alla coerenza: il giro resta con il suo nome. L'ha voluta così il proprietario del progetto, che prima aveva come nome uniforme il primo dei `servizi` | `conversione_servizi()`, `uniforma_servizio()`, `servizio_da_giro()` |
| Servizio stimato | Per un non censito: il giro prevalente, se una tipologia raggiunge l'80% delle sue letture non censite del periodo. Dà l'icona al marker ed è il valore usato da filtri e statistiche per i non censiti | `servizio_icona`, `servizio_prevalente_non_censiti()`, `servizio_mostrato()` |
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
| Indice di fiducia | Voto da 0 a 1 sull'affidabilità di un cluster spaziale: somma pesata di sei verifiche. Quella sul servizio si chiama «stabilità del servizio» | `cluster_indice_fiducia`, `indice_fiducia()` |
| Cantiere | Sede operativa che serve un gruppo di comuni. Quattro valori | `cantiere`, `cantieri()`, `inst/extdata/comuni_cantieri.csv` |
| Comune del database | Comune di servizio del contenitore secondo il database | `comune_da_database`, vuoto per i non censiti |
| Comune del giro, comune atteso | Comune in cui il calendario mandava il mezzo che ha fatto la lettura. C'è su ogni lettura. Serve solo al confronto con il comune di lettura dei non censiti e alla scheda «Ricerca Giro»: non assegna un comune a nessun contenitore | `comune_giro` |
| Comune di lettura | Comune in cui è stata fatta la lettura. Nei dati reali arriva nel file, dal sistema di localizzazione dei mezzi; nel dataset di esempio è il comune il cui confine contiene il punto. L'app non lo ricava dalle coordinate: una lettura senza comune di lettura non ne ha uno | `comune_lettura`, riportato alla grafia della tabella da `aggiungi_geografia()` |
| Comune geografico | Lo stesso comune di `comune_lettura`, ma vuoto se non è tra i comuni serviti: lo usano i conti, che non devono trattare un comune fuori tabella come un comune servito | `comune_geografico` |
| Comune dichiarato | Comune del database, per un censito, o comune del giro, per un non censito: quello che i dati dicono, a confronto con il comune di lettura | `comune_da_database`, `comune_giro` |
| Comune assegnato | Comune geografico della lettura; se la lettura è fuori dai comuni serviti, o non ha un comune di lettura, quello del database, per i censiti. Un non censito letto fuori dai comuni serviti non ne ha. È il comune usato da filtri, bolle e statistiche | `comune_assegnato`, calcolato da `aggiungi_geografia()` |
| Fuori comune | Lettura di un censito fatta in un comune diverso da quello del database | voce `Fuori comune` di `coerenza_comune`, segnale `C` sul marker, `FUORI_COMUNE` nella tabella dei cluster |
| Fuori giro | Lettura di un non censito fatta in un comune diverso da quello del giro | voce `Fuori giro` di `coerenza_comune`, segnale `G` sul marker, `FUORI_GIRO` nella tabella dei cluster |
| Giro | Lavoro di un mezzo in un giorno, per un servizio e in un comune, secondo il calendario. Nel file sta su ogni lettura | `servizio_giro` e `comune_giro`, `riepilogo_giri()`, `R/utils_giri.R` |
| Comune di un RFID | L'ultimo comune indicato dal database, per un censito; per un non censito quello in cui è stato fatto il maggior numero delle sue letture, secondo il comune di lettura. Lo usa la tabella dei cluster | `comune` nella tabella dei cluster, `geografia_per_gruppo()` |
| Bolla | Sulla mappa principale, da lontano: un gruppo di bidoni per cantiere o per comune, calcolato dal server. Da vicino, oltre il limite dei marker, anche per riquadro. Ha l'aspetto di un cluster di marker. La bolla di un comune sta nel baricentro del comune | `scegli_vista()`, `aggrega_marker()`, `baricentri_comuni()`, `R/utils_mappa_aggregata.R` |
| Confini dei comuni | Livello disegnato su tutte le mappe: il confine di ogni comune servito, tratteggiato, con il colore del suo cantiere | `aggiungi_confini()`, `geojson_confini()`, `colori_cantieri()` |
| Letture storiche, sistema precedente | Letture fatte prima dell'installazione delle antenne: solo RFID e istante | secondo file CSV, `fonte == "storico"`, `R/utils_storico.R` |
| Fonte | Sistema che ha fatto una lettura | `fonte`: `"storico"` o `"antenne"`, aggiunta da `unisci_letture()` |

**Attenzione: la parola "cluster" ha due significati che non c'entrano l'uno con l'altro.** Sulla mappa principale è un effetto grafico. Nell'analisi è il risultato di DBSCAN. Le bolle per cantiere, per comune e per riquadro hanno l'aspetto dei cluster di marker ma sono calcolate dal server.

## Funzionalità

L'interfaccia è una dashboard. La barra laterale contiene, dall'alto: caricamento dei due file (letture con le antenne e, facoltative, letture storiche), periodo di analisi, dettaglio del bidone selezionato, controlli della scheda aperta. I filtri della mappa principale stanno invece sopra la mappa. Il corpo ha cinque schede, in un `tabBox` con id `scheda_attiva`.

| Scheda | Valore di `scheda_attiva` | Cosa mostra | Controlli |
|---|---|---|---|
| Mappa Principale | `mappa` | Un marker per RFID, nella posizione dell'ultima lettura del periodo. Da lontano una bolla per cantiere, poi una per comune; da vicino i singoli marker, con quelli vicini raggruppati in cluster colorati secondo la quota di censiti. Confini dei comuni colorati per cantiere | Sopra la mappa, menu a discesa: cantiere, comune, coerenza del comune, stato a database, servizio, giro, coerenza dei servizi, reset. Nella barra laterale le statistiche |
| Ricerca RFID | `rfid` | Tutte le letture degli RFID cercati. Ultima lettura in evidenza, riquadro attorno alle posizioni di ogni RFID | Campo di testo, pulsanti Cerca e Pulisci |
| Ricerca Giro | `giro` | Che cosa ha letto un mezzo in un giorno: tabella dei giri, con servizio e comune previsti dal calendario e letture fuori giro e fuori comune, e mappa di tutte le letture, con il confine del comune del giro in evidenza | Mezzo, giorno o intervallo di giorni, caselle «Fuori giro» e «Fuori comune» |
| Ricerca Utenza | `utenza` | Ultima lettura dei bidoni la cui utenza attuale è tra quelle cercate | Campo di testo, pulsanti Cerca e Pulisci |
| Analisi Cluster Spaziale | `cluster` | Tabella, grafici e mappa dei cluster spaziali di ogni RFID, con indicatore e indice di fiducia. Esportazione in CSV | Parametri DBSCAN, pulsante Genera Analisi |

**Le schede sono cinque, per scelta del proprietario del progetto**, che ha chiesto la «Ricerca Giro» dopo aver fissato le prime quattro. Non aggiungere all'app altre schede di analisi, di report o di prestazioni.

Altri comportamenti:

- **Filtri per cantiere e comune.** I cantieri si spuntano: la voce «Non assegnato» raccoglie le letture senza cantiere, cioè i non censiti letti fuori dai comuni serviti. I comuni si scelgono da un elenco a scelta multipla, nella riga dei filtri, con i comuni dei cantieri spuntati divisi per cantiere: se ne possono scegliere più di uno, anche di cantieri diversi. Nessun comune scelto vale per tutti. Vale il comune assegnato alla lettura, cioè quello in cui è stata fatta. Cambiando cantieri o comuni la mappa si sposta sull'area scelta. I due filtri compaiono solo se almeno una lettura ha un cantiere.
- **Filtro sulla coerenza del comune.** Il menu «Coerenza comune» segue la colonna `coerenza_comune` e ha quattro voci, tutte spuntate all'avvio: «Coerente», «Fuori comune», «Fuori giro», «Coerenza ignota». Compare con gli altri filtri geografici. Le statistiche riportano i bidoni fuori comune, fuori giro e di coerenza ignota. Fino al 15 ottobre 2026 era il filtro «Posizione», con le voci «Regolare», «Fuori comune» e «Fuori giro» misurate dalla distanza dal confine: il proprietario del progetto l'ha fatto puntare a questo campo.
- **Filtri dei servizi.** Lo stato a database toglie o rimette tutti i bidoni di uno stato, e vale insieme agli altri filtri. I servizi stanno in due menu, che riguardano censiti e non censiti. «Servizio»: il servizio del contenitore, cioè per un censito quello a database, `servizio_transponder`, per un non censito quello stimato dai giri. È lo stesso dell'icona: la voce `CARTA` mostra i contenitori della carta, e con lo stato «Presente» i soli censiti. La casella «Non censiti con stima incerta (?)» riguarda i non censiti con il punto di domanda. «Giro»: il giro del calendario, con il suo nome, così si vedono i soli bidoni letti dai giri `SECCO PAP`. In cima ai due menu, «Tutti» e «Nessuno» spuntano o tolgono tutte le voci: «Nessuno» sul servizio toglie anche la stima incerta. Fino a ottobre 2026 il filtro sul servizio si chiamava «Servizio Atteso», quando il servizio atteso di un censito era il servizio del database: oggi il servizio atteso è quello del giro, e il filtro guarda il servizio del contenitore. Non c'è una casella «Includi Non Censiti»: il proprietario del progetto l'ha fatta togliere perché doppione dello stato «Non Presente».
- **Filtro sulla coerenza dei servizi.** Il menu «Coerenza servizio» ha due voci, spuntate all'avvio: «Coerente» e «Non coerente», per i valori `SI` e `NO`. Le letture di cui la coerenza non si può dire, come quelle dei non censiti, stanno con quelle coerenti. Le statistiche riportano i bidoni non coerenti, e i bidoni per giro.
- **Filtri sopra la mappa.** L'ha chiesto il proprietario del progetto: i filtri della mappa principale sono una riga di menu a discesa sopra la mappa, non elenchi nella barra laterale. Nell'ordine: Cantiere, Comune, Coerenza comune, Stato database, Servizio, Giro, Coerenza servizio, Reset. Un menu resta aperto mentre si spuntano le voci e si chiude con un click fuori. Un menu con qualche voce tolta ha il pulsante verde, con il numero delle voci rimaste, ad esempio «Servizio 7/8». Le statistiche restano nella barra laterale.
- **Elenchi a discesa.** L'elenco dei comuni, sopra la mappa, e quello dell'anno di analisi, nella barra laterale, hanno lo stesso aspetto, per richiesta del proprietario del progetto: stesso riquadro, stesso testo, stessa freccia.
- **Vista per zoom.** La mappa principale mostra una bolla per cantiere fino a zoom 11, una per comune da zoom 12 a zoom 14, i singoli bidoni da zoom 15: l'ha chiesto il proprietario del progetto, perché i gruppi che si vedono siano quelli del territorio. I cantieri arrivavano a zoom 10 e i comuni a zoom 12: il proprietario del progetto ha allargato le due fasce un livello di zoom alla volta, in tre richieste. Vale per qualsiasi numero di bidoni. Da vicino i bidoni che si sovrappongono restano in un cluster di marker. Con più bidoni del limite, da zoom 15 la mappa mostra una bolla per riquadro e i singoli bidoni dell'area inquadrata solo quando tornano sotto il limite. La bolla di un comune sta nel baricentro del comune, per richiesta del proprietario del progetto: così resta ferma al variare dei filtri, in mezzo al confine disegnato. Le bolle dei cantieri e dei riquadri stanno nel punto medio dei loro bidoni. Un click su una bolla la apre: ingrandisce sui suoi bidoni e arriva almeno al livello successivo, i comuni per un cantiere e i bidoni per un comune. Un file senza cantieri né comuni non ha bolle del territorio.
- **Confini dei comuni.** Tutte le mappe, anche quella dei cluster spaziali, disegnano i confini dei comuni serviti: bordo tratteggiato e riempimento appena accennato, nel colore del cantiere. Il proprietario del progetto li ha chiesti poco saturi e poco invasivi. La legenda riporta i quattro colori. I confini non rispondono al mouse e stanno sotto marker, cluster e riquadri.
- **Marker.** Verde a goccia se censito, rosso squadrato se non censito. L'icona interna indica il servizio. Per un non censito è il servizio stimato: il giro prevalente, se una tipologia raggiunge l'80% delle letture, altrimenti un punto di domanda. Un sacchetto ha sempre l'icona dei sacchetti, censito o no. Due bollini scuri con una lettera, negli angoli in alto, segnalano le incongruenze della coerenza del comune: `C` per una lettura fuori comune, `G` per una fuori giro. Non cambiano colore, forma né icona.
- **Sacchetti.** Si vedono sulla mappa principale e nelle due ricerche, con la loro icona, la loro voce nei filtri e nelle statistiche e un dettaglio loro. Restano fuori dall'analisi dei cluster spaziali: la scheda riguarda i soli contenitori.
- **Dettaglio del bidone.** Un click su un marker, su una riga della tabella dei cluster o su un baricentro apre nella barra laterale la storia dell'RFID: servizio stabile, cambio di servizio, stima per i non censiti, cambio di utenza, letture fuori dal comune del database e fuori dal giro, con dove sono state fatte, letture non coerenti, con servizio atteso e giro. In fondo c'è l'istogramma delle letture per anno: usa tutte le letture caricate, non solo quelle del periodo, e distingue le letture storiche da quelle delle antenne. Un anno senza letture resta vuoto, con lo zero in rosso.
- **Popup.** Riporta i dati della lettura, con il servizio a database, per un censito, il servizio atteso e il giro del calendario, il comune a database, il comune di lettura, il comune del giro se è diverso da quello di lettura, il cantiere e, per una lettura fuori comune o fuori giro, una riga che dice dove era dichiarata e dove è stata fatta. Una lettura non coerente ha una riga con servizio a database e servizio atteso, ad esempio «a database CARTA, atteso SECCO».
- **Titolo.** Riporta il periodo, ad esempio "DASHBOARD RFID - ANNO 2025".
- **Favicon.** È il simbolo aziendale. Nell'intestazione non c'è nessun logo, per scelta del proprietario del progetto.

## Dati

Il file delle letture con le antenne è quello che scrive il passo 3 della preparazione dei dati: 27 colonne. Per l'app nove sono obbligatorie e dieci facoltative, tra cui `coerenza_servizio` e `coerenza_comune`, che l'app ricalcola. Le altre otto, `giorno`, `id_giro`, `eseguito`, `frequenza`, `codice_categoria`, `stato_servizio`, `data_attivazione_contenitore` e `data_cessazione_contenitore`, vengono ignorate: la validazione non le tiene. L'ordine e le maiuscole dei nomi non contano. I formati accettati sono CSV, CSV compresso con gzip e Parquet: li riconosce `leggi_tabella()` dall'estensione.

| Colonna | Tipo dopo la validazione | Note |
|---|---|---|
| `giorno_lettura` | POSIXct, fuso UTC | Formato `YYYY-MM-DD HH:MM:SS`. Accettate anche date italiane |
| `targa_veicolo`, `matricola_veicolo` | testo | Relazione 1:1 |
| `RFID` | testo | Si ripete a ogni lettura |
| `presente_a_database` | testo | Solo `Presente` o `Non Presente` |
| `servizio_transponder` | testo maiuscolo, facoltativa | Servizio del contenitore a database, vuoto per i non censiti e per i sacchetti |
| `servizio_atteso` | testo maiuscolo | Servizio atteso dal giro, con il nome della tipologia; nei giri dei servizi quello dei censiti letti nel giro. Compilato per ogni lettura. Un file preparato tra il 13 e il 15 ottobre 2026 ha qui il servizio del database dei censiti, con `origine_servizio` uguale a `Database`, e non ha `servizio_transponder`: la validazione riporta quel servizio in `servizio_transponder` e mette nel servizio atteso il giro convertito |
| `servizio_giro` | testo maiuscolo, facoltativa | Giro che il calendario dava al mezzo, con il nome del calendario: `SECCO PAP`, non `SECCO`. Se il file non la riporta la validazione la riempie con `servizio_atteso`, senza avvisi |
| `id_utenza` | testo | Vuoto per i non censiti |
| `latitudine`, `longitudine` | numerico | Accettata la virgola decimale |
| `volume_previsto` | numerico, facoltativa | Litri. Nel file del passo 3 si chiama `volume`: vedi `alias_colonne()`. Usata dall'analisi dei cluster |
| `numero_raccolte_annue_previste` | numerico, facoltativa | Nel file del passo 3 si chiama `numero_raccolte_annue`. Usata dall'analisi dei cluster |
| `comune_da_database` | testo, facoltativa | Vuoto per i non censiti. La colonna `comune` dei file meno recenti vale come questa |
| `comune_giro` | testo, facoltativa | Comune in cui il calendario mandava il mezzo. Compilato per ogni lettura |
| `cantiere` | testo maiuscolo, facoltativa | Ricalcolato dal comune assegnato, cioè dal comune di lettura, secondo la tabella dei comuni. Quello del file vale solo per le letture fuori dai comuni serviti con un comune del database fuori tabella |
| `comune_lettura` | testo, facoltativa | Comune in cui è stata fatta la lettura: nel file del passo 3 arriva dal sistema di localizzazione dei mezzi, e la validazione lo tiene, nella grafia della tabella dei comuni. Se manca, la lettura non ha un comune: l'app non lo calcola dalle coordinate |
| `coerenza_comune` | testo, facoltativa | `Coerente`, `Fuori comune`, `Fuori giro` o `Coerenza ignota`, vedi `coerenza_comune()`. Nel file del passo 3 sta subito dopo `comune_lettura`. La validazione la calcola sempre: se il file la contiene già, viene ricalcolata |
| `comune_geografico` | testo | Non sta nel file: lo aggiunge la validazione. È il comune di lettura, se è tra i comuni serviti; altrimenti è vuoto |
| `comune_assegnato` | testo | Non sta nel file. Comune geografico, oppure quello del database per le letture dei censiti fuori dai comuni serviti |
| `coerenza_servizio` | testo, facoltativa | `SI` o `NO`, vuoto se manca il servizio a database o il servizio atteso. La validazione la calcola sempre da quei due servizi: se il file la contiene già, viene ricalcolata. Il CSV dei dati reali la riporta, calcolata dalla stessa funzione |

La validazione rifiuta il file se mancano colonne o se ci sono valori non interpretabili. Scarta, con un avviso, le righe prive di un campo essenziale. Riporta i nomi dei comuni alla grafia della tabella dei comuni, senza badare a maiuscole, accenti e apostrofi, e segnala con un avviso i comuni che la tabella non contiene: quelli assegnati alle letture e, con avvisi a parte, quelli dei giri e i comuni di lettura del file. Un comune del giro scritto male fa risultare fuori giro i non censiti letti in quel giro: per questo ha il suo avviso.

**Un file preparato prima di ottobre 2026 ha `comune_lettura` con il comune del calendario e non ha `comune_giro`.** L'app non tenta di riconoscerlo: prende `comune_lettura` come comune di lettura e, senza `comune_giro`, la coerenza dei non censiti è ignota. Un file preparato tra metà e fine ottobre 2026 può avere in `comune_lettura` la voce `FUORI SERVIZIO AZIENDA`, che il passo 3 di allora scriveva per i punti fuori dai comuni serviti: l'app la tratta come un comune fuori tabella, con un avviso. In tutti e due i casi il file va rigenerato con il passo 3.

La geografia sta in due file del pacchetto.

- `inst/extdata/comuni_cantieri.csv`: i 66 comuni serviti, con cantiere e coordinate del centro abitato. È compilato a mano ed è l'unico elenco di comuni e cantieri: 7 comuni per `ASIAGO`, 17 per `BASSANO`, 27 per `CAMPOSAMPIERO`, 15 per `RUBANO`. `VIGONZA` sta sotto `CAMPOSAMPIERO`: l'ha precisato il proprietario del progetto, dopo un primo elenco che lo metteva sotto `RUBANO`. I nomi sono in maiuscolo, con l'accento come nei dati aziendali: `ROSÀ`. Fino a ottobre 2026 era scritto `ROSA'`; il proprietario del progetto ha corretto i nomi nei dati aziendali, e la tabella e il file dei confini li seguono. Il passo 2 trova il cantiere di una lettura unendo il suo comune a questa tabella per nome esatto: un nome scritto in un altro modo resta senza cantiere. L'elenco dell'azienda aveva anche `VALSTAGNA`, comune soppresso nel 2019 e oggi parte di `VALBRENTA`: non ha più un confine suo e non sta in tabella. Il proprietario del progetto lo riporta a `VALBRENTA` nel passo 2 della preparazione dei dati.
- `inst/extdata/comuni_confini.geojson`: i confini degli stessi comuni, uno per ogni riga della tabella e nello stesso ordine. Sono quelli dell'ISTAT, dall'ultimo rilascio della raccolta `guglielmo/geojson-italy`, licenza CC-BY 4.0: ora il rilascio del 14 maggio 2026. Li scarica `scarica_confini_comuni()`, richiamata dallo script `data-raw/prepara_comuni.R`. Ogni comune sta su una riga del file e porta `comune` e `cantiere`, come nella tabella, e `nome_istat`, `codice_istat`, `codice_catastale`, `provincia`, come nella fonte. Servono ai confini disegnati sulle mappe dell'app, alla loro vista iniziale e al dataset di esempio. `VALBRENTA` ha due poligoni.

I nomi della tabella e quelli della fonte si associano con `chiave_comune()`: `ROSÀ` trova `Rosà`, e così farebbe `ROSA'`. Tra due comuni italiani con lo stesso nome vale quello che contiene il centro abitato della tabella. Se un comune non trova il confine, o il suo centro cade fuori dal confine trovato, `scarica_confini_comuni()` si ferma senza scrivere il file.

Il CSV delle letture storiche è un secondo file, facoltativo, con due colonne: `RFID` e `giorno_lettura`. Lo valida `valida_storico()`: una data non interpretabile ferma il caricamento, le righe incomplete e quelle ripetute vengono scartate con un avviso. I codici RFID devono avere lo stesso formato dell'altro file.

I dataset di esempio sono due, simulati dallo script `data-raw/genera_sample_dataset.R` con un seme fisso.

- `inst/extdata/sample_rfid_dataset.csv`: 6.475 letture con le antenne di 251 RFID, cioè 239 bidoni e 12 sacchetti, da ottobre 2024 a dicembre 2025, cinque mezzi, 50 utenze dei bidoni. Come nei dati reali, il calendario dà un comune a ogni giro: nell'esempio un giro è un turno in un giorno della settimana, i giri sono 41 e lavorano in 32 comuni dei quattro cantieri. Ogni bidone sta nel comune del proprio giro, dentro il confine. Il file ha le 27 colonne del passo 3: le colonne del contenitore che l'app non usa, come frequenza, categoria e stato del servizio, hanno valori inventati, e `id_giro` è il numero della riga della tabella `giri` dello script. Il giro ha il nome del calendario simulato, dodici in tutto; il servizio atteso ha il nome della tipologia, e i dodici giri diventano otto servizi. Nei giri dei servizi non passa nessun censito: il loro servizio atteso resta `SERVIZI`. Le letture non coerenti sono 23, di quattro censiti letti in un giro del secco. Il comune di lettura è quello il cui confine contiene il punto, come lo darebbe il sistema di localizzazione dei mezzi: nessuna lettura è fuori dai comuni serviti, e 1.218 sono fatte in un comune diverso da quello del giro. I bidoni non censiti sono 37: nel 2025, 18 hanno il servizio stimato e 19 la stima incerta. I sacchetti hanno una lettura ciascuno, perché sono monouso: 8 sono censiti, con le utenze da `SAC001` a `SAC004`, e 4 non censiti. A database hanno solo l'utenza, come escono dal passo 3: niente servizio, volume, raccolte o comune. Il loro servizio atteso è quello del giro che li ha letti. Li leggono i giri del secco, nel comune del giro. Due sono letti nel 2024, dieci nel 2025. Per coerenza del comune le letture sono 5.460 coerenti, 389 fuori comune, di cinque RFID, 618 fuori giro, di 36 RFID, e 8 di coerenza ignota, quelle dei sacchetti censiti. Le letture fuori comune sono per lo più del tag rimasto su un mezzo, 360, e del contenitore spostato, 8; le altre 21 sono di tre censiti letti appena oltre il confine del loro comune, che il confronto senza tolleranza segnala. Quelle fuori giro sono per lo più del tag fermo in deposito, 475.
- `inst/extdata/sample_letture_storiche.csv`: 6.855 letture storiche di 222 RFID, da gennaio 2020 a settembre 2024. Nell'app servono all'istogramma per anno del dettaglio. 179 RFID sono letti da entrambi i sistemi, 72 solo dalle antenne, cioè 60 bidoni e i 12 sacchetti, 43 solo dal sistema precedente. Il sistema precedente registra una parte dei passaggi, diversa per cantiere e per anno, e lascia anni interi senza letture.

Il CSV dei dati reali e quello di esempio hanno anche `coerenza_servizio` e `coerenza_comune`, che l'app ricalcola. Nell'esempio le letture sono 5.376 `SI`, 23 `NO` e 1.076 senza coerenza del servizio: quelle dei non censiti e dei sacchetti.

Lo script genera prima le letture dei bidoni, poi la geografia e i sacchetti, con semi a parte, poi le letture storiche: una modifica alla geografia, ai sacchetti o alla parte storica non cambia il resto. Aggiungere comuni alla tabella cambia invece il comune dei giri, e con quello posizioni e letture storiche: date, mezzi e servizi delle letture con le antenne restano gli stessi. Il giro di ogni lettura è ricavato senza numeri casuali: `giro_del_mezzo()` dà il servizio e `indice_turno_del_mezzo()` il turno, e quindi `id_giro`, cioè quello in cui cade la lettura o, fuori orario, il più vicino del mezzo in quel giorno, come fa sui dati reali `associa_servizio_atteso_da_calendario()`. Il comune del giro viene dalla tabella `giri` dello script: un non censito letto altrove risulta fuori giro. Il comune di lettura lo trova `comune_del_punto()` dello script, dai confini dei comuni e dalle coordinate scritte nel file, come farebbe il sistema di localizzazione dei mezzi; cantiere e coerenza del comune li calcola `aggiungi_geografia()`; il servizio atteso lo dà `servizio_atteso_dal_giro()`, la stessa regola del passo 3. I test fanno riferimento a questi RFID:

| RFID | Caso |
|---|---|
| `RFD20250901001` | Servizio cambiato: CARTA, SECCO, di nuovo CARTA |
| `RFD20250901050` | Utenza cambiata da `UTZ001` a `UTZ025` |
| `RFD20250915201` | Non censito, quattro letture, stima 100% `SECCO`. Indicatore `GHOST_TAG` |
| `RFD20250920250` | Non censito con stima incerta: due letture, `SECCO` e `CARTA` |
| `RFD20250905100` | Spostato di 17 km: censito a `LIMENA`, prima lettura lì, poi otto letture a `CITTADELLA`, dove lo legge il giro di Cittadella. Otto letture fuori comune; da censito non è mai fuori giro. Indicatore `RELOCATED_BIN`, secondo cluster `FUORI_COMUNE` |
| `RFD20241001301` | Tag fermo in deposito, a `CAMPOSAMPIERO`. Indicatore `DEPOT_STUCK`. Non censito, letto dai giri di tutti i mezzi: stima incerta, e 475 letture fuori giro su 514 |
| `RFD20250203302` | Tag rimasto sul mezzo `VEH004`, letto lungo lo stesso percorso tra `MAROSTICA`, `BASSANO DEL GRAPPA` e `NOVE` qualunque sia il giro. Indicatore `TRUCK_STOWAWAY`. Censito a `MAROSTICA`: 360 letture fuori comune, quelle fatte a `NOVE` e a `BASSANO DEL GRAPPA` |
| `RFD20241003303` | GPS impreciso, 17 cluster. Indicatore `SCATTERED_READS / GPS_NOISE` |
| `RFD20241004003` | Letto dalle antenne con regolarità, senza letture storiche nel 2021 e nel 2023. Comune `LIMENA`, cantiere `RUBANO`. Letture per anno dal 2020: 11, 0, 15, 0, 12, 25 |
| `00BD00000000000000000005` | Sacchetto censito, utenza `SAC002`: una lettura a `ROANA` nel 2025. I censiti vanno da `…0001` a `…0008` |
| `00BD00000000000000000102` | Sacchetto non censito: una lettura a `ROANA` nel 2025. I non censiti vanno da `…0101` a `…0104` |
| `VEH001`, 24 settembre 2025 | Giro sbagliato: il calendario dava il secco a `LIMENA`, il mezzo ha letto dieci bidoni a `BORGORICCO`: `comune_giro` è `LIMENA`, `comune_lettura` è `BORGORICCO`. Nove sono censiti a `BORGORICCO` e risultano coerenti; è fuori giro solo il non censito `RFD20241009227`. Nella scheda «Ricerca Giro» si riconosce dal comune del giro diverso da quello di lettura |

Numeri del dataset di esempio usati dai test. Per ultima lettura, su tutto il file: 251 RFID, 3 fuori comune, 19 fuori giro, 8 di coerenza del comune ignota e 2 non coerenti; per cantiere 33, 66, 92 e 60. Nel solo 2025 gli RFID sono 248, e per cantiere 33, 64, 91 e 60; per ultima lettura 3 sono fuori comune, 19 fuori giro e 7 di coerenza ignota. Nell'analisi dei cluster del 2025, 4 cluster sono `FUORI_COMUNE` e 20 `FUORI_GIRO`. Le letture non coerenti sono 23, tutte di censiti letti in un giro `SECCO PAP`: `RFD20241007190` 10, `RFD20241011121` 11, `RFD20250901001` 1, `RFD20250905100` 1. Gli RFID con almeno una lettura del giro `SECCO PAP` sono 115. Nel 2025 il servizio atteso `SECCO` raccoglie 87 RFID, di cui 5 non censiti.

### Dati reali

I dati veri dell'azienda non sono nel repository. Gli script in `data-raw/` li preparano, in ordine, e producono i CSV che l'app carica.

| Passo | Script | Legge | Scrive |
|---|---|---|---|
| 1 | `data-raw/01_estrai_tabelle.R` | database aziendale, via ODBC | un CSV per tabella in `data-raw/origine/` |
| 2 | `data-raw/02_importa_tabelle.R` | `data-raw/intermedi/*.rds`. Il codice commentato li ricostruisce da `data-raw/origine/` | tabelle ripulite nell'ambiente di lavoro |
| 3 | `data-raw/03_crea_letture_app.R` | le tabelle del passo 2 | `data-raw/output/letture_app.csv` |
| 4, facoltativo | `data-raw/04_crea_letture_storiche.R` | `data-raw/origine/letture_storiche.csv`, le letture precedenti alle antenne | `data-raw/output/letture_storiche.csv` |

Le regole di trasformazione sono funzioni del pacchetto, in `R/fct_preparazione_letture.R`. Il passo 3 le applica in questo ordine: `filtra_letture_post_test()`, `formatta_rfid()`, `associa_servizio()`, `associa_servizio_atteso_da_calendario()`. L'ultima restituisce le 27 colonne del CSV dell'app. Le letture arrivano dal passo 2 con `comune_lettura`, ricavato dal campo `IndirizzoLocalizzazione`, e con `cantiere`, dall'unione con `inst/extdata/comuni_cantieri.csv`. `associa_servizio()` aggiunge i dati del contenitore attivo nel giorno della lettura: `servizio_transponder`, che il passo 2 ricava da `descrizione_rifiuto`, volume, frequenza, raccolte, stato del servizio, date, categoria, utenza, e `comune_da_database`, dalla colonna `comune_servizio`. `associa_servizio_atteso_da_calendario()` sceglie il turno del mezzo, anche a cavallo della mezzanotte, e ne prende `id_giro`, `eseguito`, `servizio_giro`, il giro con il nome del calendario, senza conversioni, e `comune_giro`, la colonna del calendario che il passo 2 ricava da `comune`. Scrive poi `servizio_atteso`, con `servizio_atteso_dal_giro()`: il giro con il nome della tipologia, ricavato con `servizio_da_giro()`, oppure, nei giri dei servizi, il servizio a database più frequente tra i censiti letti nel giro in quel giorno. Un nome che la tabella di conversione non contiene resta com'è e dà un avviso. Scrive poi `coerenza_servizio`, con `coerenza_servizio()`, e per ultima `coerenza_comune`, subito dopo `comune_lettura`, con `coerenza_comune()`: sono le stesse funzioni dell'app. I nomi dei comuni si correggono nel codice commentato del passo 2, sulla colonna `comune_servizio` dell'anagrafica dei contenitori: `LUSINA CONCO` diventa `LUSIANA CONCO`, `VALSTAGNA` diventa `VALBRENTA`; nel calendario `ROSA'` diventa `ROSÀ`. L'app non corregge i nomi: un comune che la tabella non contiene viene segnalato tra i comuni assenti. Il passo 3 compila `servizio_atteso` e `comune_giro` per ogni lettura, anche dei censiti: restano vuoti solo se il mezzo non ha turni in quel giorno, cosa che secondo il proprietario del progetto nei dati reali non accade.

Fino al 15 ottobre 2026 il comune di lettura e il cantiere li calcolava il passo 3 dalla posizione, con `completa_geografia()`, e il servizio atteso dei non censiti veniva dai censiti letti dallo stesso mezzo, con `associa_servizio_atteso_dai_censiti()`: il proprietario del progetto ha tolto tutte e due le funzioni, e il parametro `df_tag` di `associa_servizio()`. Il passo 2 carica ancora la tabella dei tag dei contenitori, che il passo 3 non usa più.

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
mod_caricamento ──► dati_caricati: tutte le letture validate, con comune_lettura, comune_geografico, comune_assegnato, cantiere, coerenza_comune e coerenza_servizio
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

Di `R/utils_storico.R` l'app usa la lettura del file e `andamento_rfid()`, per l'istogramma del dettaglio.

### Moduli

Ogni modulo ha due file: `R/mod_<nome>_ui.R` e `R/mod_<nome>_server.R`.

| Modulo e id usati | Riceve | Restituisce | Input principali |
|---|---|---|---|
| `mod_caricamento` (`caricamento`) | `demo`, `percorsi`: di norma le opzioni di `run_app()` | lista di reactive: `letture` e `storico`, ciascuno con il dataset validato o `NULL` | `file_upload`, `file_storico`, `carica_esempio` |
| `mod_periodo` (`periodo`) | `dati` | lista di reactive: `periodo`, `etichetta`, `dati` | `anno_filtro`, `personalizzato`, `intervallo` |
| `mod_filtri` (`filtri`) | `dati`, `azzera` | lista di reactive: `dati_filtrati` e `area` | `cantiere_check`, `comuni`, `coerenza_comune_check`, `presente_filter`, `servizio_check`, `servizio_tutti`, `servizio_nessuno`, `includi_stima_incerta`, `servizio_giro_check`, `giro_tutti`, `giro_nessuno`, `coerenza_check`, `reset` |
| `mod_ricerca` (`ricerca_rfid`, `ricerca_utenza`) | `dati`, `tipo` | lista di reactive: `risultati`, `codici` | `testo`, `cerca`, `pulisci` |
| `mod_giro` (`giro`) | `dati` | lista di reactive: `risultati`, `riepilogo`, `messaggio` | `mezzo`, `giorni`, `solo`, `tabella_rows_selected` |
| `mod_mappa` (`mappa_principale`, `mappa_rfid`, `mappa_giro`, `mappa_utenza`) | `dati`, `modalita`, `dati_vista`, `attiva`, `messaggio` | reactive con il marker cliccato: lista con `rfid` e `quando` | `mappa_marker_click`, `mappa_zoom`, `mappa_bounds` |
| `mod_cluster_analysis` (`cluster`) | `dati`, `periodo`, `etichetta` | reactive con il cluster scelto: lista con `rfid` e `quando` | `eps_m`, `min_pts`, `genera`, `tabella_rows_all`, `tabella_rows_selected`, `mappa_marker_click` |

`mod_filtri` ha due funzioni di interfaccia, con lo stesso id: `mod_filtri_ui()` per la riga dei filtri sopra la mappa principale, `mod_filtri_sidebar_ui()` per le statistiche nella barra laterale. `mod_caricamento` tiene i due file separati: un file storico non valido non tocca le letture con le antenne. `carica_esempio` e `run_app(demo = TRUE)` caricano entrambi i dataset di esempio. Caricare a mano un file di letture toglie le letture storiche di esempio, non quelle caricate a mano. I file indicati in `run_app(letture = , storico = )` sono letti da `leggi_una_volta()`: una volta sola per processo, condivisi tra le sessioni.

`mod_mappa` ha quattro modalità: `"cluster"`, `"rfid"`, `"utenza"`, `"giro"`. La modalità decide come `disegna_marker()` disegna i dati. In modalità `"cluster"`, con più bidoni del limite, il modulo segue `mappa_bounds` e `mappa_zoom` e ridisegna la vista scelta da `scegli_vista()`. Lo stato sta in tre `reactiveVal` interni: `disegnati`, `bolle`, `vista`. In modalità `"rfid"` disegna al massimo le letture di `limita_letture_ricerca()`, in modalità `"giro"` quelle di `limita_letture_giro()`. `mod_giro` ha due funzioni di interfaccia, `mod_giro_sidebar_ui()` e `mod_giro_ui()` con la tabella dei giri; tiene lo stato sul server e torna all'ultimo giorno con letture a ogni cambio delle letture del periodo. Il giro scelto in tabella vale finché la ricerca non cambia. La mappa dei giri riceve come `dati_vista` l'area di `area_giri()`, che comprende i comuni dei giri. `mod_cluster_analysis` ha due funzioni di interfaccia: `mod_cluster_analysis_ui()` per il corpo e `mod_cluster_analysis_sidebar_ui()` per la barra laterale. Grafici e mappa seguono le righe mostrate dalla tabella, tenute nel `reactiveVal` interno `righe_tabella`: `NULL` vuol dire tutte, un vettore vuoto nessuna. Una nuova analisi lo azzera. Le righe comunicate dal browser valgono solo dopo l'attesa di `attesa_righe_tabella()`.

### Scelte di progetto da rispettare

1. **Logica fuori dai moduli.** Calcoli e costruzione di HTML, mappe e grafici stanno nei file `R/utils_*.R`, come funzioni senza reattività e coperte da test. I moduli collegano input e output. Il prefisso `fct_` è riservato alla preparazione dei dati, che non fa parte dell'app: i dati reali e i confini dei comuni.
2. **Stato sul server.** `mod_filtri` e `mod_periodo` tengono lo stato in un `reactiveValues` e lo riallineano agli input. Al caricamento di un nuovo dataset lo stato torna subito ai valori predefiniti, senza attendere il browser. Gli `observeEvent` sugli input usano `ignoreInit = TRUE`.
3. **Servizi filtrati per esclusione.** I filtri memorizzano i servizi deselezionati, non quelli selezionati. Così le scelte restano valide quando l'elenco dei servizi cambia con il periodo. "Nessuno" esclude le voci in elenco e tutti i servizi noti a `servizi_config()`: resta "nessuno" anche in un periodo con voci diverse.
4. **Filtri prima della deduplica.** `maschera_filtri()` agisce su tutte le letture, poi `deduplica_ultimo_rfid()` tiene l'ultima rimasta per ogni RFID.
5. **Mappe aggiornate con proxy.** La mappa di base si disegna una volta. I marker si aggiornano con `leafletProxy()`, solo quando la mappa è pronta e la sua scheda è visibile. Gli aggiornamenti arrivati nel frattempo restano in attesa.
6. **Identificativo dei marker.** Il `layerId` è il numero di riga dei dati disegnati. Il click risale così alla lettura anche quando lo stesso RFID ha più marker.
7. **Marker in SVG.** Ogni marker è un'immagine SVG generata in R e passata come data URI. Colore e forma indicano lo stato, il glifo Font Awesome il servizio.
8. **Icona dei marker.** `mod_periodo` aggiunge alle letture la colonna `servizio_icona`, con `aggiungi_servizio_icona()`. `aggiungi_marker()` la legge con `servizio_mostrato()`. Per un censito è il servizio a database della lettura, per un non censito il servizio stimato dell'RFID, per un sacchetto il servizio dei sacchetti. La usano anche filtri e statistiche.
9. **Analisi dei cluster in una funzione sola.** `calcola_analisi_cluster()` è usata dall'app, dalla funzione esportata `genera_analisi_cluster()` e dallo script a riga di comando.
10. **Orari senza fuso.** Date e ore sono lette e scritte sempre in UTC, come se fossero ora locale. Non convertire i fusi orari.
11. **Filtri come maschera.** `maschera_filtri()` restituisce un vettore logico, `deduplica_ultimo_rfid()` sceglie l'ultima lettura tra le righe indicate. Non si crea mai una copia filtrata di tutte le letture: con milioni di righe è la parte più costosa.
12. **Cantieri e coerenze filtrati per esclusione, comuni per scelta.** Anche le voci del filtro sulla coerenza del comune si memorizzano come escluse, in `coerenza_comune_escluse`, e così quelle della coerenza dei servizi, in `coerenza_escluse`. I cantieri seguono la regola dei servizi. I comuni scelti stanno in un vettore: vuoto vuol dire tutti. I comuni di un cantiere deselezionato escono dalla scelta, gli altri restano.
13. **Vista della mappa principale calcolata dal server.** `scegli_vista()` decide dallo zoom: bolle per cantiere, bolle per comune, poi i bidoni. Le bolle le calcola il server, con qualsiasi numero di bidoni. Da vicino il browser riceve tutti i bidoni fino al limite di `limiti_mappa()`; oltre riceve i soli bidoni dell'area inquadrata o una bolla per riquadro, e il messaggio resta sotto il megabyte. Dopo uno spostamento o uno zoom la mappa si ridisegna solo se `vista_da_rifare()` lo dice: sotto il limite succede solo quando lo zoom cambia fascia. Le bolle di cantieri e comuni si disegnano subito, appena lo zoom entra nella loro fascia, secondo `vista_immediata()`: sono poche e non dipendono dall'area inquadrata. Le altre viste aspettano 250 millisecondi, perché una serie di spostamenti dia un ridisegno solo: i bidoni possono essere migliaia. L'ha chiesto il proprietario del progetto, a cui il passaggio tra cantieri e comuni sembrava lento: con l'attesa le bolle vecchie restavano sulla mappa 350 millisecondi dopo la fine dello zoom, senza circa 80. L'area caricata è più larga del 30% di quella inquadrata, così i piccoli spostamenti non ridisegnano niente.
14. **Calcoli per gruppo senza cicli.** Con centinaia di migliaia di RFID un `summarise()` valutato gruppo per gruppo costa decine di secondi. L'analisi dei cluster, la ricerca dei giri e la geografia degli RFID usano le funzioni di `R/utils_gruppi.R`, che lavorano su vettori interi.
15. **Valori ripetuti trattati una volta.** I campi di testo hanno pochi valori ripetuti milioni di volte: maiuscole, pulizia e ricerca del cantiere passano da `per_valori_distinti()`.
16. **Una sola tabella dei comuni.** Comuni e cantieri stanno solo in `inst/extdata/comuni_cantieri.csv`. Il confronto dei nomi passa sempre da `chiave_comune()`.
17. **Elenchi a discesa con lo stesso aspetto.** L'elenco dell'anno, nella barra laterale, è un elenco del browser; quello dei comuni, nella riga dei filtri sopra la mappa, è un selectize a scelta multipla, dentro un `div` con classe `elenco-comuni`. La sezione "Elenchi a discesa" del CSS dà a tutti e due lo stesso riquadro e la stessa freccia, disegnata nel CSS perché quella del browser cambia da un sistema all'altro. Un elenco nuovo deve usare gli stessi stili.
18. **Un servizio solo per icona, filtri e statistiche.** `servizio_mostrato()` dà il valore con cui una lettura compare sul marker, nel filtro sul servizio atteso e nelle statistiche: è la colonna `servizio_icona`. Per un censito è il `servizio_transponder` della lettura, cioè il servizio del contenitore a database. Per un non censito è il servizio stimato dell'RFID, non il servizio atteso della singola lettura: un non censito sta sotto una voce sola, il suo servizio stimato oppure la stima incerta. Un sacchetto sta sempre sotto la voce dei sacchetti. Così voci dei filtri, statistiche e icone dei marker coincidono per costruzione: la funzione `servizio_dei_filtri()`, che dava lo stesso valore per un'altra strada, è stata tolta con la fusione dei due servizi. Lo stato a database non è legato a un elenco: si sceglie a parte.
19. **Sacchetti riconosciuti dal codice.** `rfid_sacchetto()` è l'unica regola. `aggiungi_servizio_icona()` dà ai sacchetti, censiti o no, il servizio `SACCHETTI`, al posto del servizio atteso e della stima dai giri, perché un sacchetto non ha una tipologia di rifiuto. `SACCHETTI` è una voce di icona, filtri e statistiche, non un valore del file: nella colonna `servizio_atteso` un sacchetto tiene il servizio del giro che lo ha letto. Nei filtri e nelle statistiche è un servizio come gli altri: voce, riga, icona, legenda. Fa eccezione l'analisi dei cluster, che riguarda i contenitori: `calcola_analisi_cluster()` toglie i sacchetti con `senza_sacchetti()`, prima di ogni conto. Il risultato deve essere quello di un file che non li contiene.
20. **Confini come livello GeoJSON a parte.** `aggiungi_confini()` li disegna con `addGeoJSON()` in un riquadro Leaflet suo, `confini`, sotto marker e forme, senza interazione. `pulisci_mappa()` usa `clearShapes()`, che non tocca i livelli GeoJSON: i confini restano a ogni ridisegno. Il testo GeoJSON lo prepara `geojson_confini()` una volta per sessione, con cinque decimali: pesa circa 125 kB. Colore e stile stanno in `colori_cantieri()` e `stile_confini()`.
21. **Colore del cantiere legato al nome.** `colori_cantieri()` dà un colore a ogni cantiere per nome, non per posizione nell'elenco. Le quattro tinte sono state scelte perché si distinguano a coppie anche con un difetto della visione dei colori, e stiano lontane dal verde, dal rosso e dall'arancio di stato e cluster. Un cantiere nuovo va aggiunto lì, altrimenti è grigio.
22. **Bolla di un comune nel baricentro del comune.** `aggrega_marker()` mette la bolla di un comune nel punto dato da `baricentri_comuni()`: il baricentro dell'area del confine, calcolato una volta per sessione da `baricentro_poligoni()`. La posizione non dipende dai bidoni filtrati. Un comune fuori tabella, o mancante, non ha baricentro: la sua bolla sta nel punto medio dei bidoni, come quelle di cantieri e riquadri.
23. **Il comune di una lettura viene dal file.** Nei dati reali arriva dal sistema di localizzazione dei mezzi, già calcolato dalla posizione: `aggiungi_geografia()` lo tiene, nella grafia della tabella dei comuni, e non lo ricava mai dalle coordinate. Da quello vengono comune geografico, comune assegnato e cantiere: filtri, bolle e statistiche lavorano sul territorio. I comuni dichiarati restano nelle loro colonne, per il confronto. Un comune di lettura del file che la tabella non contiene vale come fuori dai comuni serviti, con un avviso.
24. **Coerenza del comune per nome, senza distanze.** L'ha chiesto il proprietario del progetto il 15 ottobre 2026: il comune di lettura è già quello esatto del punto, e misurare la distanza dal confine era ridondante. `coerenza_comune()` confronta le chiavi di `chiave_comune()`, una volta per valore distinto con `per_valori_distinti()`: su milioni di letture costa decimi di secondo. Prima una griglia di celle di 55 metri trovava il comune di ogni punto e una tolleranza di un chilometro decideva le incongruenze: `R/utils_comune_geografico.R`, che conteneva tutto questo, è stato tolto. Non va rimesso senza che il proprietario del progetto lo chieda.
25. **Incongruenze calcolate una volta.** `coerenza_comune` è una colonna delle letture, scritta dalla validazione: filtri, marker, popup, dettaglio, statistiche, ricerca dei giri e analisi dei cluster la leggono. Chi ha bisogno di due vettori logici usa `incongruenze_comune()`, che dà `fuori_comune` e `fuori_giro`. Una lettura non può essere fuori comune e fuori giro insieme: un censito si confronta con il database, un non censito con il giro.
26. **Ricerca dei giri sulle colonne delle letture.** Un giro non è una tabella a parte: è il gruppo delle letture con lo stesso giorno, mezzo, `servizio_giro` e `comune_giro`, cioè con lo stesso giro del calendario. `riepilogo_giri()` li raggruppa con le funzioni di `R/utils_gruppi.R`. La scheda non ha un pulsante «Cerca»: segue gli input, e all'avvio cerca un giorno solo, così la mappa non riceve mai un anno di letture senza che l'utente lo chieda.
27. **Il calendario non assegna comuni.** `comune_giro` serve a due cose sole: riconoscere le letture fuori giro dei non censiti e descrivere un giro nella scheda «Ricerca Giro». Il comune di una lettura, quello di un non censito e il comune assegnato vengono dal comune di lettura o dal database, mai dal calendario: un mezzo può sostituirne un altro. Una regola nuova che legga `comune_giro` per dire dove sta un contenitore rimette l'errore che il proprietario del progetto ha fatto togliere.
28. **Due colonne per il comune di lettura.** `comune_lettura` riporta il comune del file, anche se non è tra i comuni serviti; `comune_geografico` è lo stesso comune, ma vuoto fuori dai comuni serviti. I conti usano `comune_geografico`: un «comune più frequente» o una ricerca del cantiere non devono trattare un comune fuori tabella come un comune servito. `comune_lettura` si usa dove il valore viene mostrato, esportato o confrontato con i comuni dichiarati.
29. **Giro con il nome del calendario, servizio atteso con quello della tipologia.** `associa_servizio_atteso_da_calendario()` non converte il calendario: scrive il giro com'è in `servizio_giro`. L'ha chiesto il proprietario del progetto, che deve poter filtrare i giri `SECCO PAP`: una regola nuova non deve uniformare `servizio_giro`. Il servizio atteso viene dal giro, per ogni lettura, con `servizio_atteso_dal_giro()` e `servizio_da_giro()`. La validazione dell'app non converte niente; riconosce i nomi del calendario, perché `tipologie_servizio()` li elenca, e dà loro l'icona della tipologia. La tabella di conversione non è un file a parte: è `tipologie_servizio()`, dove il nome uniforme è il nome della tipologia in maiuscolo. `servizi_config()` lo aggiunge sempre ai nomi riconosciuti, così `SERVIZI` e `PULIZIA TERRITORIO` hanno icona e tipologia.
30. **Giri dei servizi: il servizio dai censiti del giro.** `servizio_atteso_dal_giro()` applica la regola del proprietario del progetto: nei giri di `giri_servizio_dai_censiti()` il servizio atteso è il servizio a database più frequente tra le letture dello stesso giro, `id_giro`, nello stesso giorno, contate lettura per lettura; con un pareggio, o senza censiti, resta il giro. La moda la dà `moda_unica_per_gruppo()`, su vettori, solo per le letture dei giri dei servizi. La usano il passo 3 e lo script del dataset di esempio. Un nome nuovo va aggiunto a `giri_servizio_dai_censiti()` e, per l'icona, alla tipologia «Servizi».
31. **Tre colonne per i servizi.** `servizio_transponder` è il servizio del contenitore: dà icona, filtri e statistiche dei censiti, la cronologia del servizio e i cambi di servizio nei cluster. `servizio_atteso` è quello del giro, con il nome della tipologia: dà, attraverso il servizio stimato, icona, filtri e statistiche dei non censiti, e la coerenza. `servizio_giro` è il calendario, con il suo nome, e descrive il giro nella scheda «Ricerca Giro». Per due giorni, a ottobre 2026, servizio a database e servizio atteso sono stati una colonna sola, con una colonna `origine_servizio`: il proprietario del progetto ha poi rimesso le tre colonne nella preparazione dei dati, e l'app le segue. È lo stesso schema di `comune_da_database`, `comune_lettura` e `comune_giro`.
32. **I confini dei comuni servono solo al disegno.** Dopo il 15 ottobre 2026 l'app non decide più niente con la posizione di una lettura rispetto ai confini: li usa per disegnarli sulle mappe, per il baricentro delle bolle dei comuni, per il contorno e l'inquadratura dei comuni dei giri, con `lati_comuni()` e `indice_comune()`, e il dataset di esempio per mettere i bidoni dentro il loro comune. Non c'è più un margine attorno al territorio né una tolleranza dal confine dichiarato.
33. **Coerenza calcolata una volta, per tipologia, tra servizio a database e servizio atteso.** `coerenza_servizio()` confronta le tipologie di `tipologie_servizio()`, non i nomi: un servizio atteso scritto ancora con il nome del calendario, `SECCO PAP`, concorda con `SECCO`. Guarda due sole colonne, `servizio_transponder` e `servizio_atteso`, e scrive `SI` o `NO`, i valori della preparazione dei dati del proprietario del progetto; per un non censito la coerenza manca. La usano la preparazione e la validazione: filtri, popup, dettaglio e statistiche leggono la colonna `coerenza_servizio`, senza rifare il confronto. L'interfaccia mostra i due valori con le parole di `etichette_coerenza()`.
34. **Volume e raccolte con due nomi.** Il passo 3 scrive `volume` e `numero_raccolte_annue`, come l'anagrafica; l'app e la tabella dei cluster li chiamano `volume_previsto` e `numero_raccolte_annue_previste`. `alias_colonne()` porta i nomi del file a quelli dell'app, come faceva già per la colonna `comune` dei file meno recenti. Un file con tutti e due i nomi tiene quello dell'app.
35. **Due coerenze, con nomi distinti.** Il proprietario del progetto ha chiamato `coerenza_comune` il confronto dei comuni: la parola coerenza vale così per i servizi e per i comuni, e ogni nome dice quale. Servizi: colonna `coerenza_servizio`, `stati_coerenza()`, `coerenza_servizio()`, input `coerenza_check`, stato `coerenza_escluse`, menu «Coerenza servizio». Comuni: colonna `coerenza_comune`, `stati_coerenza_comune()`, `coerenza_comune()`, input `coerenza_comune_check`, stato `coerenza_comune_escluse`, menu «Coerenza comune». Nell'indice di fiducia dei cluster la verifica sul servizio si chiama stabilità: `stabilita` in `pesi_fiducia()`, soglia `prevalenza_atteso`. Il caso del dettaglio di un censito con il servizio mai cambiato è `censito_stabile`.
36. **Filtri in menu a discesa sopra la mappa.** Ogni filtro della mappa principale è un menu Bootstrap, costruito da `menu_filtro()`, con dentro gli stessi input di prima: gli identificativi, lo stato sul server e le regole non cambiano. Il comune è un selectize nella riga, fuori dai menu. Il terzo gestore di `script.js` ferma i click dentro un menu, che altrimenti lo chiuderebbero; il quarto conta le caselle spuntate a ogni `change`, che Shiny lancia anche quando il server aggiorna le voci, e scrive sul pulsante quante ne restano. La riga ha uno z-index più alto dei controlli di Leaflet, perché i menu aperti coprono la mappa.

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
│   ├── mod_filtri_ui.R / _server.R             filtri sopra la mappa principale e statistiche
│   ├── mod_ricerca_ui.R / _server.R            ricerca per RFID o per utenza
│   ├── mod_giro_ui.R / _server.R               ricerca dei giri di un mezzo: pannello laterale e tabella dei giri
│   ├── mod_mappa_ui.R / _server.R              mappa Leaflet, quattro modalità
│   ├── mod_cluster_analysis_ui.R / _server.R   scheda di analisi dei cluster spaziali
│   ├── utils_data_processing.R   lettura, validazione, periodo, filtri, ricerche, storia di un RFID
│   ├── utils_styling.R           tipologie di servizio, colori, marker SVG, cluster di marker, legenda
│   ├── utils_mapping.R           mappa di base, confini dei comuni e disegno dei marker
│   ├── utils_popup.R             popup, pannello di dettaglio, riquadro statistiche
│   ├── utils_dbscan_clustering.R DBSCAN, distanze, dispersione
│   ├── utils_cluster_output.R    tabella di analisi, soglie, indicatori, indice di fiducia
│   ├── utils_cluster_viste.R     tabella interattiva, grafici e mappa dei cluster spaziali
│   ├── utils_storico.R           letture storiche: lettura e conteggi per anno
│   ├── utils_geografia.R         comuni, cantieri e confini del territorio servito
│   ├── utils_gruppi.R            calcoli per gruppo su vettori, senza cicli
│   ├── utils_mappa_aggregata.R   limiti delle mappe e vista della mappa principale secondo lo zoom
│   ├── utils_giri.R              giri dei mezzi: ricerca, riepilogo, tabella
│   ├── fct_preparazione_letture.R   regole che trasformano i dati aziendali nel CSV dell'app
│   ├── fct_confini_comuni.R      scarica i confini dei comuni e sceglie quelli serviti
│   └── generate_cluster_analysis.R   genera_analisi_cluster(), scrivi_analisi_cluster(). Esportate
├── inst/
│   ├── app/www/custom_style.css  tutti gli stili dell'app
│   ├── app/www/script.js         quattro gestori: porta in vista il dettaglio del bidone, fa disegnare le mappe tornate visibili, tiene aperti i menu dei filtri e ne segna lo stato
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
│   └── output/                   letture_app.csv, letture_storiche.csv. Esclusa da git
├── docs/
│   ├── analisi_cluster.md        regole, colonne, soglie e scelte dell'analisi dei cluster
│   └── indice_fiducia.Rmd        report sull'indice di fiducia, da compilare in HTML
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
| Schema del dataset | `colonne_obbligatorie()`, `colonne_facoltative()`, `colonne_quantita()` per le facoltative numeriche, `colonne_geografiche()`, `colonne_derivate()`, `colonne_dataset()`, `colonne_contenitore()` per il servizio a database, `alias_colonne()` per i nomi alternativi delle colonne, `colonne_calendario()` per il servizio del giro, `stati_database()`, `stati_coerenza()`, `etichette_coerenza()` e `coerenza_servizio()` per la coerenza tra servizio a database e servizio atteso, `stati_coerenza_comune()` e `coerenza_comune()` per la coerenza tra comune di lettura e comune dichiarato, `percorso_dataset_esempio()` |
| Sacchetti | `rfid_sacchetto()` li riconosce dal codice, `servizio_sacchetti()` dà il nome del loro servizio, `senza_sacchetti()` li toglie dalle letture |
| Lettura e validazione | `carica_dataset()` è il punto di ingresso: chiama `leggi_tabella()` e `validate_dataset()`. `leggi_tabella()` sceglie il formato tra quelli di `formati_file()`: `read_csv_auto()`, `decomprimi_gz()`, Parquet. `leggi_una_volta()` tiene in memoria i file letti da percorso. Ausiliarie: `rileva_separatore()`, `converti_data_ora()`, `converti_numero()`, `pulisci_testo()`, `celle_da_pulire()`, `elenco_righe()`, `errore_validazione()` |
| Periodo | `anni_disponibili()`, `periodo_anno()`, `etichetta_periodo()`, `filtra_periodo()` |
| Servizio stimato dei non censiti | `servizio_prevalente_non_censiti()` applica la regola dell'80%, `aggiungi_servizio_icona()` scrive la colonna `servizio_icona`, `servizio_mostrato()` la legge o, se manca, la calcola |
| Filtri e statistiche | `maschera_filtri()` dice quali letture superano i filtri, `servizio_mostrato()` con quale servizio atteso compaiono, `incongruenze_comune()` quali sono fuori comune o fuori giro, `deduplica_ultimo_rfid()` tiene l'ultima per RFID tra quelle. `ordina_servizi()`, `conta_servizi()`, `ordina_cantieri()`, `conta_cantieri()`, `senza_cantiere()`, `calcola_statistiche()` |
| Ricerche | `analizza_input_ricerca()`, `codice_in()`, `cerca_per_rfid()`, `filtra_ultimo_per_utenza()`, `calcola_bbox()` |
| Storia di un RFID | `analizza_rfid()` decide il caso da mostrare nel dettaglio. Ausiliarie: `coerenza_comune_rfid()` per le letture fuori comune e fuori giro, `coerenza_rfid()` per quelle non coerenti, `cronologia_transizioni()`, `stima_servizio_atteso()` |

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
| Popup del marker | `create_popup_html()`, `descrivi_servizi()` per servizio a database, servizio atteso e giro di una lettura |
| Dettaglio del bidone | `crea_info_panel()` sceglie tra `blocco_bidone()` e `blocco_sacchetto()`. `blocco_coerenza_comune()` aggiunge le letture fuori comune e fuori giro, `blocco_coerenza()` quelle non coerenti. Ausiliarie: `blocco_info()`, `elenco_cronologia()`, `barre_stima()`, `istogramma_annuale()` |
| Statistiche | `crea_box_statistiche()`, `righe_servizi()`, `righe_cantieri()` |

### `R/utils_dbscan_clustering.R`

`cluster_dbscan()` raggruppa le letture di un RFID. `assegna_cluster()` lo applica a tutti gli RFID e aggiunge `cluster_id`: salta DBSCAN per gli RFID letti in un solo punto. `ordina_cluster()` numera i cluster in ordine di prima lettura. `soglia_arrotondamento_cluster()` dà il numero di letture oltre il quale le coordinate sono arrotondate a 2 metri. Distanze: `raggio_terra_m()`, `proietta_metri()`, `distanza_haversine_m()`, scritta in R base. La dispersione di un cluster è calcolata in `calcola_analisi_cluster()` con `quantile_per_gruppo()`.

### `R/utils_cluster_output.R`

| Gruppo | Funzioni |
|---|---|
| Parametri | `soglie_indicatore()`, `pesi_fiducia()`, `indicatori_config()`, `colonne_analisi_cluster()` |
| Calcolo | `calcola_analisi_cluster()` produce la tabella con una riga per cluster. Usa `classifica_cluster()`, `indice_fiducia()` e `comune_cluster()` per comune geografico e incongruenza |
| Esportazione | `nome_file_analisi_cluster()`, `esporta_analisi_cluster()` |
| Ausiliarie | `analisi_cluster_vuota()`, `sequenza_valori()`, `tra_zero_e_uno()` |

### `R/utils_cluster_viste.R`

`tabella_cluster_dt()`, `grafico_indicatori()`, `grafico_dispersione()`, `mappa_cluster()`, `popup_cluster()`. Con troppi cluster grafico a dispersione e mappa disegnano un campione: `limiti_viste_cluster()`, `campiona_cluster()`, `nota_campione()`. `attesa_righe_tabella()` dà l'attesa prima che grafici e mappa seguano le righe della tabella. Ausiliarie: `lingua_dt()`, `inchiostro_grafici()`.

### `R/utils_storico.R`

| Gruppo | Funzioni |
|---|---|
| Lettura e validazione | `carica_storico()` chiama `read_csv_auto()` e `valida_storico()`. `colonne_storico()`, `percorso_storico_esempio()` |
| Letture per anno | `unisci_letture()` mette insieme i due sistemi e aggiunge `fonte`. Da lì: `conta_letture_annuali()`, `griglia_annuale()` con gli zeri dalla prima lettura in poi, `andamento_rfid()` per l'istogramma dell'app |

### `R/utils_geografia.R`

| Gruppo | Funzioni |
|---|---|
| Tabelle | `comuni_cantieri()`, `cantieri()`, `poligoni_comuni()` con i vertici dei confini, `riquadro_zona()`. Lette una volta per sessione di R |
| Confini | `percorso_confini_comuni()`. `anelli_geometria()` e `anelli_geojson()` trasformano un GeoJSON in una tabella di vertici, con poligoni e fori. `dentro_poligoni()` dice se un punto è dentro un confine. `baricentro_poligoni()` dà baricentro e superficie di uno o più poligoni, `baricentri_comuni()` il baricentro di ogni comune, sempre dentro il suo confine. `lati_comuni()` dà i vertici di ogni comune, con l'anello e il riquadro, per il contorno e l'inquadratura dei comuni dei giri; `indice_comune()` trova la riga di un comune per nome |
| Nomi dei comuni | `chiave_comune()` per il confronto, `normalizza_comune()` per la grafia della tabella, `cantiere_di_comune()` |
| Geografia delle letture | `aggiungi_geografia()` tiene il comune di lettura del file e compila `comune_lettura`, `comune_geografico`, `comune_assegnato`, `cantiere` e `coerenza_comune` di ogni lettura. `geografia_per_gruppo()` dà comune e cantiere di ogni RFID |
| Ausiliaria | `per_valori_distinti()` applica una funzione ai soli valori distinti di un vettore |

### `R/utils_gruppi.R`

Calcoli per gruppo su vettori interi. Il gruppo è un intero da 1 al numero dei gruppi, dato da `indice_gruppi()`. `primo_per_gruppo()`, `ultimo_per_gruppo()`, `ultimo_valido_per_gruppo()`, `somma_per_gruppo()`, `media_per_gruppo()`, `estremi_per_gruppo()`, `quantile_per_gruppo()`, `n_distinti_per_gruppo()`, `piu_frequente_per_gruppo()`. I test li confrontano con il calcolo gruppo per gruppo.

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
| `filtra_letture_post_test()` | Tiene le letture dei mezzi con test delle antenne positivo, fatte dopo la data del test. Le letture hanno già comune di lettura e cantiere |
| `formatta_rfid()` | Porta i codici RFID a 10 o 24 caratteri e corregge un carattere letto male tra gli zeri iniziali |
| `associa_servizio()` | Associa servizio a database, utenza, volume, frequenza, raccolte, stato, date e categoria del contenitore attivo nel giorno della lettura. In mancanza cerca tra i sacchetti. Calcola `presente_a_database` e rinomina `comune_servizio` in `comune_da_database` |
| `associa_servizio_atteso_da_calendario()` | Sceglie per ogni lettura il turno del mezzo in cui cade, o il più vicino; a parità di distanza il primo del calendario. Un turno che finisce dopo mezzanotte vale anche per il giorno successivo. Dal turno prende `id_giro`, `eseguito`, il giro, con il nome del calendario, e il comune del giro. Poi il servizio atteso, con `servizio_atteso_dal_giro()`, la coerenza del servizio e la coerenza del comune. Restituisce le 27 colonne del CSV dell'app |
| `giri_servizio_dai_censiti()` | L'elenco dei giri dei servizi, il valore predefinito dell'argomento `servizi_applica_moda` |
| `servizio_atteso_dal_giro()` | Il servizio atteso di ogni lettura: il giro convertito, oppure nei giri dei servizi il servizio dei censiti letti nel giro |
| `moda_unica_per_gruppo()` | Il valore più frequente di ogni gruppo, se è uno solo |
| `servizio_da_giro()` | Dà il servizio atteso di un giro: il nome del database, secondo la tabella di conversione. Segnala con un avviso i nomi che la tabella non contiene. Il giro non viene toccato |
| `estrai_rfid()` | Ricava i codici RFID dal testo degli eventi. Non usata dagli script attuali |

### Altri file

| File | Funzioni |
|---|---|
| `R/app_ui.R` | `app_ui()`, `sezione_sidebar()` per i riquadri della barra laterale, `golem_add_external_resources()` |
| `R/app_server.R` | `app_server()` |
| `R/mod_giro_ui.R` | `mod_giro_sidebar_ui()` per la barra laterale, `mod_giro_ui()` per la tabella dei giri |
| `R/mod_ricerca_ui.R` | `mod_ricerca_ui()`, `testi_ricerca()` con le etichette delle due ricerche |
| `R/mod_caricamento_ui.R` | `mod_caricamento_ui()`, `estensioni_accettate()` per i campi di caricamento |
| `R/mod_filtri_ui.R` | `mod_filtri_ui()` per la riga dei filtri, `mod_filtri_sidebar_ui()` per le statistiche, `menu_filtro()` per un menu a discesa, `intestazione_filtro()` per il titolo di un elenco di servizi con "Tutti" e "Nessuno" |
| `R/generate_cluster_analysis.R` | `genera_analisi_cluster()`, `scrivi_analisi_cluster()` |

## Dove intervenire

| Per cambiare | Vai in | Test collegati |
|---|---|---|
| Servizi e giri riconosciuti, icone, colori, emoji | `tipologie_servizio()` in `R/utils_styling.R`. È l'unico punto | `test-utils_styling.R` |
| Tabella di conversione dei servizi: un nome nuovo del calendario, o il nome a cui viene riportato | `tipologie_servizio()`: il nome nuovo va aggiunto ai `servizi` della sua tipologia; il nome uniforme è quello della tipologia, in maiuscolo | `test-utils_styling.R`, `test-fct_preparazione_letture.R` |
| Dove il giro diventa servizio atteso, e regola dei giri dei servizi | `servizio_atteso_dal_giro()` e `giri_servizio_dai_censiti()`, chiamate da `associa_servizio_atteso_da_calendario()` dopo la scelta del turno | `test-fct_preparazione_letture.R`, `test-sample_dataset.R` |
| Regola della coerenza: servizio a database contro servizio atteso, nomi sconosciuti, valori `SI` e `NO` | `coerenza_servizio()` in `R/utils_data_processing.R`. Vale per l'app e per il CSV dei dati reali | `test-utils_data_processing.R`, `test-fct_preparazione_letture.R`, `test-sample_dataset.R` |
| Colonne del file con un altro nome: `volume`, `numero_raccolte_annue`, `comune` | `alias_colonne()` in `R/utils_data_processing.R` | `test-utils_data_processing.R` |
| File preparati tra il 13 e il 15 ottobre 2026, con `origine_servizio` | il blocco sul servizio atteso in `validate_dataset()` | `test-utils_data_processing.R` |
| Verde e rosso dello stato, colori dei cluster di marker | `colori_stato()`. Il JavaScript in `js_icona_cluster()` li legge da lì | `test-utils_styling.R` |
| Forma, bordo o dimensione dei marker | `svg_marker()` e `get_leaflet_icon()` | `test-utils_styling.R` |
| Legenda della mappa | `html_legenda()` | `test-utils_styling.R` |
| Soglia dell'80% per il servizio stimato dei non censiti: icona, filtri, statistiche | argomento `soglia` di `aggiungi_servizio_icona()`, chiamata in `R/mod_periodo_server.R` | `test-utils_data_processing.R` |
| Con quale servizio una lettura compare su marker, filtro sul servizio e statistiche; regola della stima incerta | `aggiungi_servizio_icona()`, letta da `servizio_mostrato()`, e `maschera_filtri()`, e gli stessi criteri in `calcola_statistiche()` | `test-utils_data_processing.R`, `test-moduli.R` |
| Filtri sul giro e sulla coerenza | interfaccia in `R/mod_filtri_ui.R`, stato in `R/mod_filtri_server.R`, regola in `maschera_filtri()` | `test-moduli.R`, `test-utils_data_processing.R` |
| Riga del popup e blocco del dettaglio sulle letture non coerenti | `create_popup_html()`, `descrivi_servizi()`, `coerenza_rfid()`, `blocco_coerenza()` | `test-utils_popup.R`, `test-utils_data_processing.R` |
| Scelte rapide "Tutti" e "Nessuno" dei filtri dei servizi | `intestazione_filtro()` in `R/mod_filtri_ui.R`, `spunta_elenco()` in `R/mod_filtri_server.R`, classi `titolo-filtro`, `azioni-filtro` e `nota-filtro` nel CSS | `test-moduli.R`, poi guardarle nel browser |
| Menu dei filtri sopra la mappa: ordine, aspetto, segnale dei filtri attivi, chiusura | `mod_filtri_ui()` e `menu_filtro()` in `R/mod_filtri_ui.R`, sezione "Filtri sopra la mappa principale" del CSS, terzo e quarto gestore di `inst/app/www/script.js` | `test-moduli.R`, poi provarli nel browser |
| Regola che riconosce un sacchetto: prefisso e lunghezza del codice | `rfid_sacchetto()` in `R/utils_data_processing.R` | `test-utils_data_processing.R` |
| Dove i sacchetti restano fuori: analisi dei cluster | `senza_sacchetti()` in `R/utils_data_processing.R`, chiamata all'inizio di `calcola_analisi_cluster()` | `test-utils_data_processing.R`, `test-utils_cluster_output.R` |
| Nome, icona, colore ed emoji della tipologia dei sacchetti | `servizio_sacchetti()` per il nome, la voce "Sacchetti" di `tipologie_servizio()` per il resto | `test-utils_styling.R` |
| Testi del dettaglio di un sacchetto | `blocco_sacchetto()` in `R/utils_popup.R` | `test-utils_popup.R` |
| Testo del popup dei marker | `create_popup_html()`; `descrivi_servizi()` per la riga dei servizi non coerenti | `test-utils_popup.R` |
| Comuni serviti, cantiere di un comune | `inst/extdata/comuni_cantieri.csv`. Per un comune nuovo: una riga con cantiere e coordinate del centro abitato, poi `Rscript data-raw/prepara_comuni.R` per i confini e `Rscript data-raw/genera_sample_dataset.R` per il dataset di esempio | `test-utils_geografia.R`, `test-sample_dataset.R` |
| Confini dei comuni: aggiornarli, cambiare la fonte o le regole con cui i nomi si associano | `scarica_confini_comuni()`, `origine_confini()`, `seleziona_confini_comuni()` in `R/fct_confini_comuni.R` | `test-fct_confini_comuni.R`, `test-utils_geografia.R` |
| Regola del comune di lettura, del comune assegnato e del cantiere di una lettura, uso del comune di lettura del file | `aggiungi_geografia()` in `R/utils_geografia.R` | `test-utils_geografia.R`, `test-utils_data_processing.R`, `test-sample_dataset.R` |
| Regola della coerenza del comune: fuori comune, fuori giro, coerenza ignota | `coerenza_comune()` in `R/utils_data_processing.R`. Vale per l'app e per il CSV dei dati reali. Poi rigenerare il dataset di esempio e l'analisi dei cluster di esempio | `test-utils_data_processing.R`, `test-fct_preparazione_letture.R`, `test-sample_dataset.R` |
| Voci del filtro sulla coerenza del comune | `stati_coerenza_comune()`, `maschera_filtri()`, interfaccia in `R/mod_filtri_ui.R` | `test-utils_data_processing.R`, `test-moduli.R` |
| Segnali `C` e `G` sui marker | `svg_segnale()` e `svg_marker()` in `R/utils_styling.R`, voci in `html_legenda()` | `test-utils_styling.R` |
| Righe del popup e blocchi del dettaglio sulle incongruenze | `create_popup_html()`, `coerenza_comune_rfid()`, `blocco_coerenza_comune()`, classi `popup-incongruenza` e `info-nota` nel CSS | `test-utils_popup.R`, `test-utils_data_processing.R` |
| Scheda «Ricerca Giro»: che cosa cerca, colonne e ordine della tabella dei giri | `cerca_giri()`, `riepilogo_giri()`, `tabella_giri_dt()` in `R/utils_giri.R`; modulo in `R/mod_giro_ui.R` e `R/mod_giro_server.R` | `test-utils_giri.R`, `test-moduli.R` |
| Mappa dei giri: contorno dei comuni dei giri, area inquadrata, numero massimo di comuni in evidenza | `aggiungi_comuni_giro()` in `R/utils_mapping.R`, `area_giri()` in `R/utils_giri.R`, classi `scheda-giro` e `tabella-giri` nel CSS | `test-utils_mapping.R`, `test-utils_giri.R`, poi guardarla nel browser |
| Comune e incongruenza nella tabella dei cluster | `comune_cluster()` in `R/utils_cluster_output.R` | `test-utils_cluster_output.R` |
| Regola del comune e del cantiere di un RFID | `geografia_per_gruppo()` | `test-utils_geografia.R`, `test-utils_storico.R` |
| Filtri per cantiere, comune e coerenza del comune | interfaccia in `R/mod_filtri_ui.R`, stato e scelte in `R/mod_filtri_server.R`, regola in `maschera_filtri()` | `test-moduli.R`, `test-utils_data_processing.R` |
| Aspetto degli elenchi a discesa dell'anno e dei comuni: riquadro, freccia, comuni scelti | sezione "Elenchi a discesa" di `inst/app/www/custom_style.css` | nessuno: guardarli nel browser |
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
| Casi e testi del dettaglio del bidone | `analizza_rfid()` per la logica, `blocco_bidone()` per i testi | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Riquadro delle statistiche | `calcola_statistiche()` e `crea_box_statistiche()` | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Colonne del CSV, formati di data, regole di validazione | `colonne_obbligatorie()`, `colonne_facoltative()`, `converti_data_ora()`, `validate_dataset()` | `test-utils_data_processing.R` |
| Dimensione massima del file caricato | opzione `trackerfid.max_upload_mb`, letta in `R/app_server.R`. Predefinito 2 GB | nessuno |
| Un filtro nuovo o modificato della mappa principale | interfaccia in `R/mod_filtri_ui.R`, con `menu_filtro()`, stato in `R/mod_filtri_server.R`, regola in `maschera_filtri()` | `test-moduli.R`, `test-utils_data_processing.R` |
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
| Colonne della tabella dei cluster | `colonne_analisi_cluster()`, `analisi_cluster_vuota()` e l'ultimo `mutate` di `calcola_analisi_cluster()`: i tre devono coincidere. Oggi sono 29 | `test-utils_cluster_output.R` |
| Nomi e colori degli indicatori | `indicatori_config()` | `test-utils_cluster_viste.R` |
| Tabella, grafici o mappa dei cluster spaziali | `R/utils_cluster_viste.R` | `test-utils_cluster_viste.R` |
| Stili grafici dell'app | `inst/app/www/custom_style.css` | nessuno |
| Dataset di esempio | `data-raw/genera_sample_dataset.R`, poi rigenerare i file in `inst/extdata/` | `test-sample_dataset.R`, `test-utils_storico.R` |
| Velocità di un passaggio su file grandi | misurare prima e dopo con `dev/misura_prestazioni.R` | i test del file modificato |
| Regole di preparazione dei dati reali: mezzi validi, formato degli RFID, associazione al database, servizio atteso | `R/fct_preparazione_letture.R` | `test-fct_preparazione_letture.R` |
| Tabelle estratte dal database, filtri sull'estrazione | `data-raw/01_estrai_tabelle.R` | nessuno |
| Pulizia delle singole tabelle estratte | codice commentato in `data-raw/02_importa_tabelle.R` | nessuno |
| Filtri finali e nome del CSV per l'app | `data-raw/03_crea_letture_app.R`: oggi scarta i codici che iniziano per `00BD` e quelli che iniziano con dieci zeri | nessuno |
| Dipendenze | `DESCRIPTION`, poi `devtools::document()` | `devtools::check()` |

Dopo una modifica alle regole dell'analisi dei cluster vanno aggiornati anche `docs/analisi_cluster.md` e `README.md`. Il report `docs/indice_fiducia.Rmd` contiene controlli che fermano la compilazione se il testo non corrisponde più ai dati.

## File grandi

Il dataset reale atteso è di circa 2 milioni di letture l'anno. Ogni modifica deve lasciare l'app veloce a quella scala. Misure su 2 milioni di letture di 100.000 RFID, su un portatile:

| Passaggio | Tempo |
|---|---|
| Lettura del CSV da 320 MB, e validazione | 2,3 s e 3,8 s. Da Parquet 1,3 s e 3 s. Nella validazione 0,4 s sono della coerenza dei servizi e 0,8 s del comune di lettura e della coerenza del comune: il calcolo dalla posizione che c'era prima, misurato di seguito sugli stessi dati il 16 ottobre 2026, ne prendeva circa 2. La parte più lunga è la pulizia dei campi di testo |
| Filtro del periodo e icone dei non censiti | 0,2 s |
| Cambio di un filtro: maschera e ultima lettura per RFID | 0,15 s. Con il filtro sul servizio 0,3 s |
| Disegno della mappa, lato server | meno di 0,1 s, messaggio sotto il megabyte |
| Nel browser, dal click al ridisegno finito | da 0,3 a 0,5 s per filtri, zoom e spostamenti |
| Ricerca di un RFID, dettaglio di un bidone | 0,1 s, 0,03 s |
| Ricerca dei giri: un mezzo in un giorno, tutti i mezzi in un mese | 0,01 s, 0,3 s con il riepilogo. Su tutto l'anno e tutti i mezzi qualche secondo |
| Analisi dei cluster | circa 2 s |

Il file del passo 3, con le sue 27 colonne, è più grande: 540 MB per 2 milioni di letture del dataset sintetico, contro 375 MB del formato precedente, e un secondo in più per leggerlo. Le otto colonne che l'app non usa si possono togliere dal file, oppure si può usare Parquet. La validazione tiene il comune di lettura del file e non calcola niente dalla posizione.

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
- **Dati aziendali.** Estrazioni, tabelle intermedie e CSV reali stanno solo in `data-raw/origine/`, `data-raw/intermedi/` e `data-raw/output/`, escluse da git. Non copiarli in `inst/` o in `data/`, che entrano nel pacchetto, e non riportarne il contenuto in test, documenti o messaggi. Vale anche per i report compilati sui dati reali: vanno scritti in `data-raw/output/` con `output_dir`, mai lasciati in `docs/`.

## Trappole note

- **Mappe in schede nascoste.** Leaflet non disegna bene in un contenitore invisibile. Per questo `mod_mappa` aspetta che la scheda sia attiva. Una mappa nuova deve seguire lo stesso schema, oppure essere ridisegnata per intero con `renderLeaflet()` come fa la mappa dei cluster spaziali.
- **Altezza del contenuto e barra laterale.** A ogni ridimensionamento e a ogni click sul pulsante che chiude la barra, il tema scrive su `.content-wrapper` un'altezza minima pari a quella del contenuto della barra laterale. Qui la barra scorre per conto suo, quindi il CSS la riporta all'altezza della finestra con `!important`. Senza quella regola, chiudendo la barra con i filtri aperti la pagina scorre nel vuoto sotto la mappa.
- **`summarise()` e nomi riusati.** Dentro un `dplyr::summarise()` una colonna appena creata nasconde quella originale nelle espressioni successive. I riepiloghi che servono la colonna originale vanno calcolati prima di sovrascriverla.
- **`dbscan::dbscan()` e matrici.** Una matrice viene letta come coordinate dei punti, non come distanze. Il progetto passa coordinate proiettate in metri.
- **Date nella tabella interattiva.** Le colonne di tipo `Date` slittano di un giorno con i formati data del browser. In `tabella_cluster_dt()` sono convertite in testo.
- **Confini e dataset di esempio.** Lo script del dataset di esempio mette ogni contenitore dentro il confine del suo comune e ricava il comune di lettura dal punto. Dopo un aggiornamento dei confini il dataset va rigenerato: possono cambiare le coordinate di qualche contenitore vicino a un confine e il comune di lettura di qualche lettura. Vanno poi rigenerata l'analisi dei cluster di esempio e rieseguiti i test. Lo stesso dopo aver aggiunto un comune alla tabella o averne cambiato il cantiere: i bidoni dell'esempio vengono redistribuiti tra i comuni del loro cantiere, e cambiano i conteggi per comune e i numeri del confronto tra i sistemi.
- **Confini e pulizia della mappa.** I confini restano sulla mappa perché Leaflet li tiene tra i livelli GeoJSON, che `clearShapes()` non tocca. Disegnati con `addPolygons()` sparirebbero al primo ridisegno dei marker. Per lo stesso motivo `pulisci_mappa()` non deve chiamare `clearGeoJSON()`.
- **Marker assenti da lontano.** All'avvio la mappa principale inquadra tutta la zona, a zoom 10 o meno, e disegna quattro bolle: i marker dei bidoni non ci sono finché non si arriva a zoom 15. Una prova nel browser che conta i marker deve prima ingrandire. Nei test del modulo lo zoom si imposta con `session$setInputs(mappa_zoom = )`.
- **`VALSTAGNA` non è in tabella.** È un comune soppresso: il passo 2 della preparazione lo riporta a `VALBRENTA`, ma solo nella colonna `comune_servizio` dei contenitori. Se un file lo contiene ancora, ad esempio perché compare nel calendario dei giri, l'app lo tratta come ogni comune fuori tabella: avviso al caricamento, nessun confine e nessun baricentro. Un non censito letto a `VALBRENTA` da un giro di `VALSTAGNA` risulta fuori giro: i nomi sono diversi. Il riconoscimento nell'app, aggiunto e poi tolto, non va rimesso senza che il proprietario del progetto lo chieda.
- **Baricentro fuori dal confine.** Il baricentro dell'area di un comune dalla forma irregolare può cadere fuori dal suo confine: succede a `POVE DEL GRAPPA` e a `TEOLO`. `baricentri_comuni()` usa allora il baricentro della parte più grande, se il comune ha più poligoni, e altrimenti il centro abitato della tabella dei comuni. Dopo aver aggiunto un comune conviene guardare dove cade la sua bolla.
- **Dati di esempio citati per nome.** I test fanno riferimento a RFID e conteggi precisi. Cambiare il seme o le regole dello script del dataset li fa fallire: vanno aggiornati insieme.
- **Compilazione dei report.** Vanno compilati dalla cartella `docs/`, senza l'opzione `intermediates_dir`, che produce avvisi spuri. `output_dir` invece si può usare.
- **`favicon()` e test dell'interfaccia.** golem mette il favicon nella sezione `head`, che `as.character(app_ui(NULL))` non riporta. Per controllarla nei test serve `htmltools::renderTags(app_ui(NULL))$head`.
- **Output di tipo `reactive()` nei test.** Un output assegnato con `reactive()`, come quelli che pilotano un `conditionalPanel`, non si legge da `shiny::testServer()`: dà errore. Nei test si legge il reactive interno, ad esempio `con_geografia()` in `mod_filtri`.
- **Elenco a scelta multipla senza scelte.** Con nessun comune scelto `input$comuni` è `NULL`, non un vettore vuoto: l'osservatore usa `ignoreNULL = FALSE`.
- **Attese nei test della mappa.** Il ridisegno dopo uno spostamento parte dopo 250 millisecondi: nei test va fatto passare il tempo con `session$elapse()`. Fanno eccezione le bolle di cantieri e comuni, disegnate appena cambia `mappa_zoom`: lì il test controlla la vista subito dopo `session$setInputs()`.
- **Bolle troppo grandi da lontano.** Con numeri alti le bolle arrivano a 72 pixel a qualsiasi zoom, e da lontano coprono buona parte del territorio. Il proprietario del progetto lo sa e ha scelto di lasciarle così: `lato_bolla()` non va legato allo zoom senza che lo chieda.
- **Numeri tondi.** `format(1e5, big.mark = ".")` dà `"1e+05"`. `formatta_numero()` usa `scientific = FALSE`: per i conteggi usare sempre quella.
- **Servizio atteso e servizio del giro.** `servizio_atteso` ha il nome della tipologia, `servizio_giro` quello del calendario: non si confrontano per uguaglianza ma per tipologia, con `tipologia_servizio()` o `uniforma_servizio()`. Nei giri dei servizi il servizio atteso può non essere quello del calendario: per dire che cosa era previsto serve `servizio_giro`, oppure `servizio_del_giro()` su dati che possono non avere la colonna. Raggruppare i giri per `servizio_atteso` li spezza.
- **Dataset di esempio e giri dei servizi.** Nell'esempio nei giri `SERVIZI MERCATI` e `SERV.MANIFESTAZIONE` passano solo i tag anomali e qualche non censito: il servizio atteso resta `SERVIZI`. Per provare la regola dei censiti servono le letture sintetiche dei test. Un caso dimostrativo nell'esempio cambierebbe letture e conteggi: va aggiunto solo se il proprietario del progetto lo chiede.
- **Servizio atteso, servizio a database e servizio stimato.** `servizio_atteso` è compilato per ogni lettura e viene dal giro, per censiti e non censiti. Il servizio di un censito è `servizio_transponder`; quello di un non censito è il servizio stimato, il servizio atteso prevalente tra le sue letture «Non Presente». La stima del pannello di dettaglio e la composizione dei giri nell'analisi dei cluster contano le stesse letture. Filtri e statistiche passano da `servizio_mostrato()`.
- **File preparati tra il 13 e il 15 ottobre 2026.** Hanno `origine_servizio` e non `servizio_transponder`: il servizio del database dei censiti sta nel servizio atteso. La validazione lo riporta in `servizio_transponder` e mette nel servizio atteso il giro convertito; per i non censiti il servizio atteso resta quello del file, che può venire dai censiti letti dallo stesso mezzo. I file ancora più vecchi hanno il servizio atteso dei censiti preso dal calendario: con `servizio_transponder` si caricano come quelli di oggi. Per avere `id_giro`, la regola dei giri dei servizi e il comune di lettura del sistema di localizzazione, il file va rigenerato con il passo 3.
- **Giri che non raccolgono un rifiuto.** `PULIZIA TERRIT.` fa tipologia a sé, «Pulizia territorio»; `SERVIZI MERCATI` e `SERV.MANIFESTAZIONE` stanno nella tipologia «Servizi». Un contenitore censito letto in uno di questi giri risulta non coerente, salvo che nei giri dei servizi il suo servizio sia il più frequente del giro. `ASSISTENTE SERVIZI` non ha più una tipologia, per scelta del proprietario del progetto: ha il punto di domanda, e `servizio_da_giro()` lo segnala con un avviso.
- **Nome uniforme di una tipologia.** È il nome della tipologia in maiuscolo: rinominare una tipologia in `tipologie_servizio()` cambia il servizio atteso scritto nel CSV dei dati reali e in quello di esempio, e i nomi nella composizione dei cluster.
- **Sacchetti nell'esempio, non nei dati reali.** Il dataset di esempio contiene 12 sacchetti, per scelta del proprietario del progetto, che vuole vedere come l'app li tratta. Nei dati reali i passi 3 e 4 li scartano ancora. Per questo i conteggi dell'esempio vanno letti come 239 bidoni più 12 sacchetti: i test sulle regole dei bidoni, come volume, raccolte e comune del database, li escludono con `rfid_sacchetto()`.
- **Sacchetti fuori dalle analisi.** L'analisi dei cluster toglie i sacchetti con `senza_sacchetti()`. Le funzioni di `R/utils_storico.R` non lo fanno: `unisci_letture()` e `andamento_rfid()` servono anche al dettaglio di un sacchetto nell'app, che ne mostra l'istogramma. Un'analisi nuova sui contenitori deve togliere i sacchetti all'inizio. Mappa, ricerche, filtri e statistiche della barra laterale invece li comprendono: per questo sulla mappa del 2025 ci sono 248 RFID e nell'analisi dei cluster 238.
- **Due comuni dichiarati e tre dal comune di lettura.** `comune_da_database` e `comune_giro` sono dichiarati. `comune_lettura` e `comune_geografico` vengono dal file e differiscono solo per i comuni fuori tabella: il primo li riporta, il secondo è vuoto. `comune_assegnato` è quello che usano filtri e bolle. Una regola nuova deve scegliere quale le serve: per dire dove sta un bidone il geografico, per confrontare con l'anagrafica o con il calendario i dichiarati. `geografia_per_gruppo()` usa il database per i censiti e il comune di lettura per i non censiti.
- **Coerenza del comune senza tolleranza.** Il confronto è tra nomi: una lettura fatta pochi metri oltre il confine del comune del database è fuori comune. Nel dataset di esempio succede a tre censiti vicini a un confine, 21 letture; nei dati reali conviene aspettarsi molti fuori comune lungo i confini. Un comune scritto in due modi diversi, invece, non conta: `chiave_comune()` ignora maiuscole, accenti e apostrofi.
- **Soglie dello zoom nei test.** Le fasce di zoom sono cambiate tre volte: i test del modulo della mappa e di `scegli_vista()` usano numeri di zoom scritti a mano, e vanno ritoccati a ogni cambio di `soglie_zoom()`.
- **`comune_lettura` ha cambiato significato.** Fino a ottobre 2026 era il comune del calendario, oggi è quello in cui è stata fatta la lettura; il comune del calendario è `comune_giro`. In una versione vecchia del codice, o in un file vecchio, il nome inganna. Da metà ottobre 2026 nei dati reali arriva dal sistema di localizzazione dei mezzi, e dal 15 ottobre l'app lo usa così com'è, anche per la coerenza del comune.
- **Coordinate come testo nella preparazione dei dati.** Nelle tabelle aziendali latitudine e longitudine sono testo con la virgola decimale: il codice commentato del passo 2 le converte in numeri. Una funzione di preparazione che riceve coordinate da un'altra fonte deve prima passare da `converti_numero()`.
- **Cantiere di una lettura e cantiere di un RFID.** Il `cantiere` di una lettura è quello del suo comune di lettura; quello di un RFID nella tabella dei cluster, dato da `geografia_per_gruppo()`, è quello del comune dichiarato. Per un bidone spostato in un altro cantiere i due sono diversi: per ultima lettura i bidoni per cantiere sono 33, 66, 92 e 60, per RFID 33, 66, 91 e 61.
- **Colonna `letture` dentro `tibble()`.** In `riepilogo_giri()` il conteggio delle letture si chiama `letture`, come il data frame: dentro `dplyr::tibble()` la colonna appena creata nasconde il data frame nelle espressioni successive. I valori che servono le letture sono calcolati prima.
- **Click sulle righe di una tabella DT nelle prove.** La selezione di una riga parte dagli eventi del mouse: un `click()` di jQuery non basta, servono `mousedown`, `mouseup` e `click` veri.
- **Giri del dataset di esempio.** Nell'esempio il comune del giro viene dal calendario simulato: i non censiti letti da un giro in un altro comune, come il tag fermo in deposito e qualche non censito letto di passaggio, sono fuori giro. È voluto. Il giro sbagliato dimostrativo è scritto a mano nello script: cambiarne giorno o turno richiede di aggiornare test e documenti. Con la coerenza del comune dà una sola lettura fuori giro, perché i nove censiti letti a `BORGORICCO` sono a database proprio lì.
- **Comuni con più poligoni nel contorno del giro.** `aggiungi_comuni_giro()` separa gli anelli con un valore mancante, usando il numero dell'anello di `lati_comuni()`: dedurre la fine di un anello dai vertici non basta, `VALBRENTA` ha due parti.
- **Statistiche e servizio stimato.** `calcola_statistiche()` riceve una riga per RFID: il servizio stimato va calcolato prima, su tutte le letture del periodo, con `aggiungi_servizio_icona()`. Se la colonna `servizio_icona` manca, `servizio_mostrato()` la calcola sulle sole righe ricevute, e la stima può risultare diversa.
- **Elenco di caselle senza etichetta.** Con `label = NULL` Shiny lascia un'etichetta nascosta e sposta le voci 10 pixel più in alto: nei menu dei filtri finirebbero sopra la nota. La regola CSS `.filtro-menu .shiny-input-checkboxgroup label ~ .shiny-options-group` lo corregge: deve essere più specifica di quella di Shiny.
- **Menu dei filtri e click.** Bootstrap chiude un menu a discesa a ogni click sul documento: `script.js` ferma i click dentro `.filtro-menu`. Un widget dentro un menu che apre una sua tendina attaccata al `body`, come un selectize con `dropdownParent: "body"`, la chiuderebbe: per questo l'elenco dei comuni sta nella riga, fuori dai menu.
- **Righe della tabella dei cluster dopo una nuova analisi.** `input$tabella_rows_all` riporta le righe dell'ultima tabella disegnata dal browser, e la tabella non si ridisegna mentre la sua vista è nascosta. Dopo una nuova analisi quegli indici sono di un'altra tabella: usati così come sono danno righe vuote, un avviso di Leaflet e l'errore "Bounds are not valid" nel browser. Per questo `righe_tabella` viene azzerato a ogni nuova analisi.
- **Tabella dei cluster senza righe.** Se il filtro della tabella non lascia righe, `input$tabella_rows_all` arriva come `NULL`, lo stesso valore di quando la tabella non è ancora stata disegnata. Lo stesso elenco vuoto arriva per qualche centesimo di secondo a ogni ridisegno della tabella: 50 millisecondi sul file da 2 milioni di letture. Per questo il modulo legge l'input dopo un `debounce()` di `attesa_righe_tabella()` millisecondi e tratta come "nessuna riga" solo l'elenco vuoto che dura. Nei test del modulo va fatto passare il tempo con `session$elapse()`.
- **Mappa ridisegnata mentre è nascosta.** Se una nuova analisi finisce mentre la vista "Mappa" dei cluster è nascosta, perché l'utente ha cambiato vista durante il calcolo, Leaflet tiene i dati in attesa di un ridimensionamento. Al ritorno sulla vista il ridimensionamento non arriva, perché la misura è la stessa di prima, e la mappa resterebbe grigia. Il secondo gestore di `script.js` lo chiede a ogni scheda che torna visibile. Va legato a `shown.bs.tab`: Bootstrap lancia l'evento con quello spazio dei nomi, e un gestore legato al solo `shown` non lo riceverebbe. Legato così riceve anche lo `shown` senza spazio dei nomi con cui Shiny mostra un `conditionalPanel`.
- **Eccezione di Leaflet nella console sulla mappa dei cluster.** A ogni ridisegno la mappa dei cluster scrive nella console del browser un'eccezione `clearRect`: viene dal disegno su canvas della mappa precedente, appena rimossa, e non ha effetti visibili.
- **Layer Leaflet senza righe.** `addCircleMarkers()` con vettori vuoti dà errore. La mappa dei cluster sceglie gli indicatori da disegnare dopo il campione, non prima.
- **File di `testthat` dopo un test fallito.** Un test fallito lascia `tests/testthat/_problems/` e `tests/testthat/testthat-problems.rds`: vanno eliminati, non fanno parte del progetto.

## Documenti collegati

| Documento | Contenuto |
|---|---|
| `README.md` | Uso dell'app, formato del file, file grandi, differenze rispetto alle specifiche |
| `docs/analisi_cluster.md` | Analisi dei cluster spaziali: colonne, indicatori, indice di fiducia, comune di lettura e comuni dichiarati, scelte e limiti |
| `docs/indice_fiducia.Rmd` | Report sull'indice di fiducia, con esempi svolti e prove di efficacia |
