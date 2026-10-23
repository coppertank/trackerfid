# trackerfid: contesto per assistenti LLM

Da leggere prima del codice; la guida per chi usa l'app è `README.md`. Quando aggiungi o sposti file e funzioni, o cambi il flusso dei dati, aggiorna questo file. «Il proprietario» è il proprietario del progetto: le sue scelte, qui annotate, non vanno ribaltate senza che lo chieda.

## In breve

- Pacchetto R con un'app Shiny (golem, shinydashboard, dplyr). Un'azienda di raccolta rifiuti esplora su mappa le letture RFID dei bidoni e dove ogni bidone è stato letto nel tempo.
- Letture in DuckDB in memoria (`duckdb`), mappe WebGL con MapLibre GL JS (`mapgl`): scelti dal proprietario il 16/10/2026 per la velocità.
- Tutto in italiano: interfaccia, nomi, commenti, test, documenti.
- Avvio: `run_app()`, `run_app(demo = TRUE)` (dataset di esempio), `run_app(letture = "percorso")` (anche il `.duckdb` della pipeline).
- Scala attesa: circa 2 milioni di letture l'anno; l'app deve restare veloce (vedi «File grandi»).

## Dominio

- **Lettura**: un mezzo con lettore e GPS legge il chip (`RFID`) di un bidone: data e ora, mezzo, RFID, coordinate.
- **Fornitura e utenza**: il database lega un RFID a una fornitura (`id_servizio` a database, `id_fornitura` dopo la preparazione) e la fornitura a un'utenza, il proprietario; un'utenza può avere più forniture. Con una fornitura l'RFID è **censito**, altrimenti **non censito**. Il suffisso `_fornitura` indica la fornitura; «servizio» è il tipo di rifiuto.
- **Giro**: una lettura esiste solo se un mezzo passa durante un giro del calendario, che dà servizio e comune del giro (`servizio_giro`, `comune_giro`). **Il calendario dice che cosa era previsto, non che cosa è successo**: un mezzo può sostituirne un altro. Per questo il comune di una lettura viene dalla sua posizione e il calendario non assegna comuni a nessun contenitore.
- **Tre servizi per lettura**: `servizio_transponder`, del contenitore a database, solo per i censiti; `servizio_atteso`, quello che il giro lasciava aspettare, con il nome della tipologia in maiuscolo (`SECCO`); `servizio_giro`, il nome del calendario senza conversioni (`SECCO PAP`, `CARTA CONT.STRADALI`). Il secco porta a porta va filtrato a parte: `servizio_giro` non si uniforma mai. La **tabella di conversione** (`tipologie_servizio()`) porta il nome del calendario a quello della tipologia.
- **Giri dei servizi** (`SERVIZI MERCATI`, `SERV.MANIFESTAZIONE`, elenco destinato a crescere): il servizio atteso è il servizio a database più frequente tra le letture dei censiti di quel giro in quel giorno; con nessun censito o un pareggio resta il giro. Un mezzo non scarrabile raccoglie un rifiuto alla volta.
- **Associazione lettura-giro** (regole del proprietario, 21-23/10/2026, in `scegli_giro()`; spiegata con diagrammi in `docs/associazione_giri.qmd`, Quarto (formato scelto dal proprietario il 23/10/2026 al posto di R Markdown): **non fa parte della pipeline, si compila a mano con `quarto::quarto_render()` quando la logica cambia**; tabella dei passi, grafici e soglie sono calcolati dal codice e la compilazione si ferma se i passi non corrispondono). Due calendari: pianificati (`data_giro, ora_inizio, ora_fine HH:MM, matricola_mezzo, id_giro, eseguito, descrizione_servizio, comune_giro`) e non pianificati (`data_giro, matricola_mezzo, id_giro, descrizione_servizio, comune_giro`, senza orari né conferma), uniti da `unisci_calendari()`. L'`id_giro` dei non pianificati è un codice proprio del loro file (servizio più comune), con la stessa logica di quello dei pianificati; può coincidere con uno di un pianificato di un altro giorno; se il file non lo ha resta vuoto. Mai un giro di un altro mezzo o di un altro giorno; niente turni a cavallo della mezzanotte né pianificati sovrapposti dello stesso mezzo. Passi, ognuno per le letture non ancora associate (nomi di `passi_associazione()`):
  1. `orario_comune`: pianificato nel cui orario e nel cui comune cade la lettura;
  2. `comune_non_pianificato`: non pianificato nel comune della lettura, se in quel comune il mezzo ha un solo servizio (vale anche dentro l'orario di un pianificato di un altro comune: confermato il 21/10);
  3. `orario`: pianificato nel cui orario cade la lettura, in un altro comune;
  4. `servizio_unico`: se tutti i non pianificati del mezzo nel giorno hanno lo stesso servizio (`distinct()` sul servizio, anche in comuni diversi), quel servizio è certo; il giro è quello del comune più vicino (`distanza_dal_comune()`), proposto dal proprietario il 23/10;
  5. `tratto`: le letture del mezzo in un giorno si dividono in tratti continui (pausa > 30 minuti, `assegna_tratti()`); le orfane prendono il giro più frequente tra le letture già associate del tratto (a pari voti nessuno);
  6. `comune_vicino`: non pianificato del comune più vicino, entro `soglia_comune_vicino_m()` = 2 km, se i giri più vicini non hanno servizi diversi;
  7. `vicino_orario`: pianificato più vicino nel tempo, entro `tolleranza_giro_minuti()` = 60 minuti.

  Una lettura che nessun passo risolve **resta senza giro** e senza servizio atteso (il 23/10 il proprietario ha accettato di togliere «l'unico giro del mezzo» e «il primo del calendario», che assegnavano anche a ore di distanza). Un pareggio tra servizi diversi non assegna niente; a parità di giro dello stesso servizio vale il primo del calendario. **Ritardo**: una lettura entro 30 minuti (`ritardo_giro_minuti()`) dalla fine di un pianificato ne fa parte e diventa la nuova fine, a catena; la catena si ferma all'inizio del pianificato seguente (`allunga_fine()`), e l'orario dei passi 1, 3 e 7 conta la fine allungata. **Mezzo senza giri nel giorno**: servizio atteso = servizio più frequente tra i bidoni censiti, contati una volta ciascuno, letti quel giorno dal mezzo (`servizio_dei_censiti_del_mezzo()`); con un pareggio resta vuoto; giro, comune del giro e `id_giro` vuoti. Nei giri dei servizi il giro di un non pianificato senza codice è mezzo, servizio e comune. Per verificare un'associazione, per i bidoni censiti la quota di `coerenza_servizio` `SI` per passo dice quanto è giusto. Scarrabili e giri a chiamata: il proprietario passerà la struttura.
- **Coerenza del servizio** `coerenza_servizio`: `SI` se servizio a database e servizio atteso sono della stessa tipologia, `NO` altrimenti, mancante se manca uno dei due (ogni non censito). Un censito letto in `PULIZIA TERRIT.` non è coerente.
- **Sacchetti**: codice di 24 caratteri con prefisso `00BD` (i bidoni ne hanno 10); monouso, una lettura sola; nessuna tipologia di rifiuto. Visibili sulla mappa, fuori dall'analisi dei cluster; nei dati reali la pipeline li scarta.
- **Scopo finale**: censire a mano i bidoni non a database. Gli operatori cercano non censiti, cambi di servizio o utenza, bidoni spostati, tag anomali (smarriti, fermi in deposito, rimasti su un mezzo).
- **Comuni**: comune di lettura `comune_lettura` (nei dati reali dalla posizione dentro i comuni serviti, altrimenti dal sistema di localizzazione dei mezzi, vedi sotto) e comuni dichiarati: `comune_fornitura` (database) e `comune_giro`. **Coerenza del comune** `coerenza_comune`: un censito si confronta con il database (diverso: **fuori comune**, di solito un bidone spostato), un non censito con il giro (diverso: **fuori giro**, di solito un mezzo fuori calendario); uguali: `Coerente`; manca uno dei due: `Coerenza ignota`. Un censito non è mai fuori giro. Nomi con l'accento, `ROSÀ`, come nella tabella dei comuni.
- **Calcoli sulla posizione solo nella pipeline** (scelte del 15, 19 e 21/10/2026): l'app non ricava comuni dalle coordinate né usa i confini per decidere (niente più griglia, tolleranza di 1 km, margine di 100 m). La pipeline (`correggi_comune_lettura()`, dopo `associa_servizio()`): se il comune del sistema di localizzazione (`IndirizzoLocalizzazione`) coincide con `comune_fornitura`, la lettura lo tiene (regola del proprietario del 23/10/2026: vicino ai confini la posizione può dare il comune accanto); altrimenti prende il comune servito che contiene il punto, qualunque nome scriva il sistema (a volte una frazione o il comune accanto); fuori dai comuni serviti, o senza coordinate, tiene il nome del sistema. Il proprietario ha verificato che il comune dalla posizione coincide molto più spesso con quello della fornitura. Spiegato con diagrammi in `docs/comune_lettura.qmd` (Quarto, come `docs/associazione_giri.qmd`: fuori dalla pipeline, da ricompilare a mano; i sei casi sono calcolati da `correggi_comune_lettura()`, il testo va aggiornato a mano). Per i censiti la regola può nascondere un bidone spostato in un comune confinante (GPS ancora uguale alla fornitura): limite noto. Una lettura al giorno per RFID anche dopo `formatta_rfid()` (`una_lettura_al_giorno()`). Scrive poi `distanza_comune_dichiarato`: una lettura entro `tolleranza_confine_m()` = 100 m dal comune dichiarato è coerente (portata dell'antenna più errore GPS). Un comune del giro che è una zona (Massiccio del Grappa) dà coerenza ignota a un non censito; una tabella zona → comuni la farà il proprietario.
- **Conferma del giro** `eseguito`: `SI`, `NO` (non ancora: di solito diventa `SI`), a volte vuota. Nessun conto la usa; la tabella dei giri la mostra («Confermato»); la pipeline segnala in `controllo_giri.csv` i `NO` dopo una settimana e i `SI` di mezzi con antenna senza letture.
- **Cantieri**: `ASIAGO`, `BASSANO`, `CAMPOSAMPIERO`, `RUBANO`; ogni comune ne ha uno.
- **Letture storiche** (lettori manuali, le «saponette»): tre campi, `data_lettura` (istante, nome grezzo del database; per noi `data_ora_lettura`), `codice_transponder` (l'RFID) e `id_trasmissione` (codici con la stessa trasmissione in un giorno sono letti nello stesso giro, ma non corrisponde a `id_giro`: niente servizio atteso dal giro). **L'app non le usa** (tolte il 16/10/2026: non rimetterle senza richiesta). Dal 21/10/2026 la pipeline dà loro la stessa anagrafica di `letture_app.csv`, contenitore, fornitura, nucleo e potenziamento, senza sacchetti, con i campi in comune nello stesso ordine (`prepara_letture_storiche()`, `colonne_letture_storiche()`). A ottobre 2026: storiche dal 1/1/2025, antenne dal 1/8/2026, su una parte dei mezzi.
- **Report** (23/10/2026): `docs/conferimenti_secco.qmd`, `docs/indice_fiducia.qmd` e `docs/confronto_antenne.qmd`, Quarto, **fuori dalla pipeline**: `tar_make()` fa solo la pulizia dei dati (richiesta del proprietario, la compilazione richiedeva troppo tempo), i report si compilano a mano con `compila_report()` di `data-raw/compila_report.R`, che mette l'HTML in `data-raw/output/`. Il report sul beneficio delle antenne lo scrive il proprietario; `docs/beneficio_antenne.Rmd` e le vecchie funzioni del confronto sono stati tolti e non vanno ricreati; non aggiungere funzioni pensate per un report se non chieste. Il 22/10/2026 ha chiesto `confronta_letture()` in `R/utils_confronto.R`; il 23/10 `docs/confronto_antenne.qmd` con cinque parametri (`letture_antenne`, `letture_storiche`, `servizio`, `categorie_escluse`, `sottocategoria`; un parametro vuoto non filtra, i percorsi relativi partono dalla radice del progetto) e le funzioni `filtra_letture()` (vale anche per le storiche: queste hanno solo `servizio_transponder`) e `storiche_di_confronto()`. Le letture con le antenne si filtrano così: `servizio_atteso` **oppure** `servizio_transponder` uguale al servizio; fuori le `categoria` escluse (i non censiti, senza categoria, restano); solo la `sottocategoria` indicata (i non censiti escono). Dalle letture filtrate si prendono gli RFID; delle letture storiche si tengono le coppie `data_lettura` e `id_trasmissione` in cui compare uno di quegli RFID, e **tutte** le letture storiche di quelle coppie; poi `confronta_letture()` unisce le due serie, un RFID al più una volta al giorno, con `origine` `Saponette`, `Antenne` o `Entrambe`. **Il confronto iniziale resta com'è** (scelta del proprietario, 23/10/2026: `storiche_di_confronto()` non va cambiata): non è del tutto equo, ma è a svantaggio delle antenne, perché i giri delle saponette comprendono RFID mai letti dalle antenne; il report lo spiega con diagrammi e poi lo corregge. Dopo il confronto iniziale il report elenca i tre punti da verificare (mesi incompleti, insiemi di RFID diversi sui due lati, stagionalità) e le analisi A-H, tutte in `R/utils_confronto_analisi.R` con test: correzione 1 per giorno (`riepilogo_mensile()`), correzione 2 stessi RFID (`storiche_stessi_chip()`, `unisci_origini()`), A anno su anno a 364 giorni con intervallo bootstrap (`anno_su_anno()`), B copertura dei contenitori attivi in finestre mobili (`copertura_contenitori()`, denominatore ricavato dalle letture, per difetto per entrambi i sistemi), C giri svolti con entrambi i sistemi (`giri_in_comune()`: trasmissione abbinata al mezzo con più RFID in comune, almeno `chip_condivisi_minimi`; stessi filtri e solo censiti, non gli stessi RFID, che gonfierebbero la quota), D giri e RFID per giro (`produttivita_giri()`), E settimane e mezzi attrezzati (`serie_settimanale()`; un mezzo è attrezzato dalla prima lettura con le antenne), F RFID mai letti dalle saponette (`chip_senza_saponette()`), G coerenza delle letture con le antenne (`qualita_letture_antenne()`), H proiezione alla flotta (`proiezione_flotta()`, solo con il parametro `mezzi_totali`, è una stima). Le storiche non hanno il mezzo: un confronto prima-dopo tra mezzi con e senza antenna richiede un campo che oggi manca. Il testo del report non fa affermazioni che dipendano dai dati: spiega come leggere i numeri.
- **Controllo dei giri con antenne e saponette** (23/10/2026, `docs/controllo_giri_saponette.qmd`, funzioni in `R/utils_controllo_giri.R`): il proprietario ha avuto una lista di giri pianificati di un giorno (20/10/2026), con mezzi già attrezzati e collaudati, in cui gli operatori dovevano usare anche le saponette. Si aspetta, se le saponette sono efficaci, gli stessi RFID letti da entrambi i sistemi. Metodo scelto da lui: antenne = letture del giorno con `matricola_veicolo` in lista e `servizio_giro` uguale (`SECCO PaP`, **senza distinguere maiuscole**; matricole senza zeri iniziali, `chiave_matricola()`); saponette = tutte le letture delle trasmissioni (`id_trasmissione` dello stesso giorno) che hanno letto almeno un RFID delle antenne (basta uno). La lista è il denominatore indipendente dai due sistemi (un mezzo senza letture resta nel conteggio). Le storiche non hanno il mezzo: una trasmissione va al mezzo con più RFID in comune (`abbina_trasmissioni()`). Un RFID letto da un solo sistema non dice quale sbaglia (l'antenna può leggere bidoni vicini non svuotati, la saponetta non scansionare): il report guarda se un'antenna fuori lista lo ha letto lo stesso giorno, il servizio a database e la coerenza. **I giri stanno in un file, non nel report**: `data-raw/origine/giri_controllo.csv` (fuori da git come i dati aziendali; creato il 23/10/2026 con i 19 mezzi del 20/10, `SECCO PaP`), parametro `giri_controllo`; per un nuovo confronto il proprietario modifica il file e ricompila, il report ricava da solo antenne e trasmissioni (`giri_di_controllo()`). Colonne `data_giro` (`AAAA-MM-GG` o `GG/MM/AAAA`), `matricola_veicolo`, `servizio_giro`; separatore virgola o punto e virgola; righe vuote ignorate, righe a metà e date illeggibili danno un errore con il numero di riga; più giorni, mezzi e servizi nello stesso file valgono tutti, ognuno con il servizio della sua riga. Non sostituisce l'analisi C del report di confronto: la completa con un denominatore noto.
- **Conferimenti del secco** (l'unico rifiuto fatturato; di norma 26 raccolte previste l'anno, un'utenza ne usa una decina). Litri annui per fornitura domestica secondo `nucleo_residenti`: 720, 960, 1200, 1440, 1680 da cinque in su; con il **potenziamento** (manovra sociale `NEONATI`, `ANZIANI` o `DISABILI` in vigore, estratta già filtrata come `POTENZIAMENTO SECCO`) 6240, al posto del limite del nucleo. Il limite vale per fornitura; un conferimento è uno svuotamento e vale il volume del contenitore; le non domestiche restano fuori. Interessa il numero dei conferimenti eccedenti, non gli euro, e se i non censiti possono avere la manovra. È un report (`docs/conferimenti_secco.qmd`, funzioni in `R/utils_conferimenti.R`, compilato a mano, parametro `letture` con il CSV della pipeline): **non nell'app** finché il proprietario non lo approva. Dichiarazioni con fine prima dell'inizio sono annullate: scartate con un avviso.
- **Bit flip**: un carattere letto male; `formatta_rfid()` lo corregge solo negli zeri iniziali. L'esempio non ne contiene.

## Glossario

**«Cluster» ha due significati**: sulla mappa principale è un raggruppamento grafico di marker (MapLibre); nell'analisi è un gruppo DBSCAN di letture dello stesso RFID. Le bolle di cantieri e comuni sembrano cluster ma le calcola il server.

| Termine | Nel codice |
|---|---|
| Censito / non censito | `presente_a_database`: `"Presente"` / `"Non Presente"` |
| Servizio a database (sei valori: `SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE`) | `servizio_transponder`, `colonne_contenitore()` |
| Servizio atteso, dal giro | `servizio_atteso`, `servizio_atteso_dal_giro()` |
| Giro previsto, nome del calendario | `servizio_giro`, `servizio_del_giro()` |
| Tabella di conversione (es. `PULIZIA TERRIT.` → `PULIZIA TERRITORIO`, `SERVIZI MERCATI` → `SERVIZI`) | `conversione_servizi()`, `uniforma_servizio()`, `servizio_da_giro()` |
| Servizio stimato di un non censito: tipologia prevalente se ≥ 80% delle sue letture non censite del periodo; altrimenti **stima incerta** (punto di domanda) | `servizio_icona`, `servizio_prevalente_non_censiti()`, `tabella_stima()` |
| Tipologia: nomi che condividono icona e colore | `tipologie_servizio()` |
| Sacchetto, servizio `SACCHETTI` | `rfid_sacchetto()`, `servizio_sacchetti()`, `senza_sacchetti()` |
| Coerenza del servizio `SI`/`NO` («Coerente», «Non coerente») | `coerenza_servizio()`, `stati_coerenza()`, `etichette_coerenza()` |
| Coerenza del comune: `Coerente`, `Fuori comune`, `Fuori giro`, `Coerenza ignota` | `coerenza_comune()`, `stati_coerenza_comune()`, `incongruenze_comune()`, segnali `C` e `G` sul marker |
| Distanza dal comune dichiarato (metri, zero dentro il confine) | `distanza_comune_dichiarato`, `distanza_dal_comune()` |
| Comune geografico: comune di lettura, vuoto fuori dai comuni serviti; lo usano i conti | `comune_geografico` |
| Comune assegnato: geografico o, per un censito fuori tabella, quello del database; usato da filtri, bolle, statistiche | `comune_assegnato`, `aggiungi_geografia()` |
| Comune di un RFID: ultimo del database per un censito, geografico più frequente per un non censito | `geografia_per_gruppo()` |
| Periodo di analisi, all'avvio l'anno più recente del file | `mod_periodo` |
| Ultima lettura di un RFID (la mappa principale ne mostra una) | `ultime_letture()`, `sql_ultime()` |
| Archivio: letture validate nella tabella DuckDB `letture`, con riepilogo | `archivio_letture`, `nuovo_archivio()` |
| Selezione: archivio più condizione SQL del periodo, con la tabella della stima | `selezione_letture`, `seleziona_periodo()` |
| Riga: posizione della lettura nel file | colonna `riga` |
| Bolla: gruppo per cantiere o comune; quella di un comune sta nel baricentro del comune | `aggrega_marker()`, `baricentri_comuni()` |
| Dispersione: 90° percentile della distanza dal baricentro del cluster | `cluster_dispersione_90th_m` |
| Giorni osservati: parte del periodo coperta dal dataset, per i conteggi annui | attributo `parametri` |
| Indicatore (sette valori, es. `VALID_TARGET`) e indice di fiducia (0-1, sei verifiche pesate; quella sul servizio si chiama stabilità) | `classifica_cluster()`, `indice_fiducia()` |
| Nucleo, categoria, sottocategoria (`D` domestica, `ND` non domestica, vuota per non censiti), potenziamento | `colonne_nucleo_manovra()`, `associa_componenti()`, `associa_potenziamento()` |
| Esposizione (giorno in cui un contenitore del secco è letto in un giro del secco), occasione (giorno in cui un mezzo con antenna è passato), tasso di esposizione | `analisi_conferimenti()`, `occasioni_conferimento()`, `stima_tasso()` |

## Funzionalità

Dashboard: barra verde in alto con titolo e periodo; barra laterale con caricamento, dettaglio del bidone, controlli della scheda; cinque schede in un `tabBox` con id `scheda_attiva`. **Le schede sono cinque per scelta del proprietario: non aggiungere schede di analisi, report o prestazioni.**

| Scheda (`scheda_attiva`) | Contenuto | Controlli |
|---|---|---|
| Mappa Principale (`mappa`) | Un marker per RFID nell'ultima lettura del periodo; da lontano bolle per cantiere, poi per comune, da vicino marker in cluster colorati per quota di censiti; confini colorati per cantiere; legenda | Menu sopra la mappa: Cantiere, Comune, Coerenza comune, Stato database, Servizio, Giro, Coerenza servizio, Reset. Statistiche nella barra laterale |
| Ricerca RFID (`rfid`) | Ultime 10 letture di ogni RFID cercato, l'ultima in evidenza, riquadro dei suoi spostamenti; tabella di tutte le letture del periodo per RFID | Testo, Cerca, Pulisci |
| Ricerca Giro (`giro`) | Tabella dei giri (servizio e comune previsti, conferma, fuori giro e fuori comune, filtro per colonna); sulla mappa solo le letture del giro scelto, con il confine del comune del giro; senza scelta nessun marker | Mezzo, giorni, caselle «Fuori giro» e «Fuori comune» |
| Ricerca Utenza (`utenza`) | Ultima lettura dei bidoni la cui utenza attuale è tra quelle cercate | Testo, Cerca, Pulisci |
| Analisi Cluster Spaziale (`cluster`) | Tabella, grafici e mappa dei cluster DBSCAN, export CSV | Parametri DBSCAN, Genera Analisi |

Comportamenti (richiesti dal proprietario):

- **Filtri sopra la mappa**, menu a discesa Bootstrap, non elenchi nella barra laterale. Un menu resta aperto mentre si spunta e si chiude con un click fuori; con voci tolte il pulsante è verde con le voci rimaste («Servizio 7/8»).
- **Cantiere e comune**: cantieri a spunta, «Non assegnato» per le letture senza cantiere; comuni da un selectize multiplo, divisi per cantiere, nessuno = tutti. Vale il comune assegnato. La mappa si sposta sull'area scelta. Compaiono solo se una lettura ha un cantiere.
- **Coerenza comune**: quattro voci, tutte spuntate all'avvio (prima del 15/10/2026 era il filtro «Posizione» sulle distanze).
- **Servizi**: stato a database a parte. «Servizio»: per un censito `servizio_transponder`, per un non censito il servizio stimato (lo stesso dell'icona), più la casella «Non censiti con stima incerta (?)». «Giro»: il giro del calendario con il suo nome. «Tutti» e «Nessuno» in cima; «Nessuno» toglie anche la stima incerta. Niente casella «Includi Non Censiti» (doppione dello stato).
- **Coerenza servizio**: «Coerente» e «Non coerente»; le letture senza coerenza stanno con le coerenti.
- **Periodo nella barra in alto** (18/10/2026): due date dal/al a destra; all'avvio l'anno più recente, calendario dal 1 gennaio del più vecchio al 31 dicembre del più recente; un anno intero dà «ANNO 2025» nel titolo. Tolti l'elenco dell'anno e «Periodo personalizzato»: non rimetterli. A sinistra delle date solo un avviso. z-index: barra sopra i filtri, calendario sopra la barra.
- **Intestazione della mappa principale**: «5.360 letture di 248 RFID (ultima lettura registrata) · fascia di zoom»; le letture le conta `conta_letture()`, attributo `letture` di `dati_filtrati`.
- **Caricamento collassabile**: nasce aperto; chiuso mostra il nome del file.
- **Elenchi a discesa** (mezzo e comuni) con lo stesso aspetto, freccia disegnata nel CSS.
- **Vista per zoom** (zoom di MapLibre): bolle per cantiere fino a 10, per comune da 11 a 13, bidoni da 14 (le fasce Leaflet 11 e 14, allargate dal proprietario in tre richieste). Zoom e spostamenti non passano dal server. Click su una bolla: ingrandisce almeno al livello successivo. Un cluster che non si divide più mostra l'elenco dei suoi bidoni (massimo 50). Senza cantieri né comuni, bidoni a ogni zoom.
- **Mouse**: RFID sul bidone, bidoni e censiti su cluster e bolle.
- **Sfondo** `bright` di OpenFreeMap (scelto al posto di `positron`), senza chiave, serve la rete; deve avere il carattere `Noto Sans Bold` (`carattere_mappa()`). Senza rete: sfondo chiaro di ripiego, con confini, bolle, marker.
- **Legenda** in basso a destra, collassabile, **chiusa a ogni avvio** (niente `localStorage`, tolto il 18/10/2026).
- **Tabelle delle ricerche** (`reactable`) in un riquadro in basso a sinistra sopra la mappa, collassabile, nato aperto. Ricerca RFID: tutte le letture del periodo per RFID, dalla più recente; la mappa ne disegna solo le ultime 10 per RFID. Tabella dei giri: click sceglie un giro, secondo click lo toglie; filtri: testo parziale, data come si vede (`24/09`), numeri con confronto (`>0`).
- **Confini** dei comuni su tutte le mappe: tratteggiati, riempimento lieve, colore del cantiere, sotto etichette e marker, non rispondono al mouse.
- **Marker**: verde a goccia censito, rosso squadrato non censito, glifo del servizio (stimato o `?` per un non censito; sacchetti con la loro icona); bollini `C` (fuori comune) e `G` (fuori giro) negli angoli.
- **Dettaglio del bidone** (click su marker, riga o baricentro): servizio stabile o cambiato, stima, cambio di utenza, fuori comune e fuori giro, letture non coerenti.
- **Popup**: dati della lettura, servizi, comuni (del giro solo se diverso da quello di lettura), cantiere; righe per fuori comune o giro, «Al confine» con la distanza, servizi non coerenti («a database CARTA, atteso SECCO»).
- **Titolo** con il periodo; **favicon** con il simbolo aziendale; nessun logo nell'intestazione.

## Dati

**File delle letture** = CSV della pipeline, 35 colonne (il 19/10/2026 tolta `codice_categoria`, doppione di `categoria`, aggiunta `distanza_comune_dichiarato`). Per l'app 9 obbligatorie e 12 facoltative (`colonne_app()`); le altre 14 (`data_lettura`, `id_giro`, `frequenza`, date del contenitore, `id_fornitura`, `stato_fornitura`, le sette di `colonne_nucleo_manovra()`) l'app non le usa. `carica_archivio()`, `carica_dataset()` e `scrivi_archivio()` hanno `colonne = "app"` (predefinito, più veloce) o `"tutte"` (le tiene, tipizzate da `tipi_colonne_preparazione()`; usato dall'analisi dei conferimenti sul CSV). Formati: CSV, CSV gzip (riconosciuto dai primi due byte), Parquet, `.duckdb` della pipeline. Ordine e maiuscole dei nomi non contano.

| Colonna | Note |
|---|---|
| `data_ora_lettura` | TIMESTAMP letto come UTC; `YYYY-MM-DD HH:MM:SS` o date italiane. Nome vecchio `giorno_lettura` (alias) |
| `targa_veicolo`, `matricola_veicolo` | 1:1 |
| `RFID`, `presente_a_database` (`Presente`/`Non Presente`), `id_utenza` (vuota per i non censiti), `latitudine`, `longitudine` (anche virgola decimale), `servizio_atteso` | obbligatorie |
| `servizio_transponder` | facoltativa. Un file del 13-15/10/2026 ha invece `origine_servizio`: la validazione sposta il servizio del database in `servizio_transponder` e rimette il giro convertito in `servizio_atteso` |
| `servizio_giro` | facoltativa; se manca vale `servizio_atteso` |
| `volume_previsto`, `numero_raccolte_annue_previste` | nel file `volume`, `numero_raccolte_annue` (`alias_colonne()`); per l'analisi dei cluster |
| `comune_fornitura` | alias `comune_da_database` (fino al 17/10/2026) e `comune` (più vecchi) |
| `comune_giro`, `comune_lettura`, `cantiere` | il cantiere si ricalcola dal comune assegnato; quello del file vale solo fuori tabella |
| `coerenza_comune`, `coerenza_servizio` | sempre ricalcolate dalla validazione |
| `distanza_comune_dichiarato` | facoltativa; non numerica ferma il caricamento; se manca contano i soli nomi |
| `eseguito` | facoltativa, `SI`/`NO`/vuota |
| `comune_geografico`, `comune_assegnato` | aggiunte dalla validazione |

La validazione rifiuta colonne mancanti o valori non interpretabili (indicando le righe), scarta con avviso le righe senza un campo essenziale, porta i comuni alla grafia della tabella (senza badare a maiuscole, accenti, apostrofi) e avvisa, separatamente, dei comuni assegnati, dei giri e di lettura che la tabella non contiene (un comune del giro scritto male rende fuori giro i non censiti). File vecchi: prima di ottobre 2026 `comune_lettura` era il comune del calendario e mancava `comune_giro`; metà-fine ottobre poteva contenere `FUORI SERVIZIO AZIENDA`. L'app non li riconosce: vanno rigenerati con la pipeline.

**Geografia** (unici elenchi, confronto dei nomi sempre con `chiave_comune()`):

- `inst/extdata/comuni_cantieri.csv`: 66 comuni serviti con cantiere e centro abitato, a mano: 7 `ASIAGO`, 17 `BASSANO`, 27 `CAMPOSAMPIERO` (con `VIGONZA`, precisato dal proprietario), 15 `RUBANO`. `ROSÀ` con l'accento. `VALSTAGNA` (soppresso nel 2019) non c'è: `importa_contenitori()` lo porta a `VALBRENTA`.
- `inst/extdata/comuni_confini.geojson`: confini della Regione del Veneto (strato `c0104011_comuni`, IODL 2.0: la fonte va citata nell'attribuzione), non semplificati, scelti il 17/10/2026 al posto di quelli ISTAT; un comune per riga, stesso ordine della tabella, con `comune`, `cantiere`, `nome_istat`, `codice_istat` (6 cifre), `provincia`; 1,25 MB, circa 57.000 vertici. Scritto da `prepara_confini_comuni()` (`data-raw/prepara_comuni.R`) dallo shapefile in `data-raw/rvccomuni/` (`wfsrequest.txt` dice come scaricarlo); si ferma se un comune non trova il confine o il centro ne cade fuori; tra omonimi vale quello che contiene il centro.

**Dataset di esempio** `inst/extdata/sample_rfid_dataset.csv`, simulato da `data-raw/genera_sample_dataset.R` con seme fisso. I test citano questi numeri: cambiare lo script richiede di aggiornarli.

- 6.475 letture di 251 RFID (239 bidoni, 12 sacchetti), ottobre 2024 - dicembre 2025, 5 mezzi, 50 utenze; 41 giri in 32 comuni dei quattro cantieri; le 35 colonne della pipeline nei formati aziendali (matricola, codice del giro, fornitura, utenza numerici; RFID dei bidoni di 10 caratteri esadecimali, `D` + AAMMGG della prima lettura + progressivo; `id_giro` = 1800 + riga della tabella `giri` dello script).
- Anagrafica inventata con le regole aziendali: volumi 40, 60, 120 (domestiche) e 1.100 (non domestiche, salvo l'umido); `ATTIVA`, o `CESSATA` per la fornitura lasciata da un cambio di utenza o servizio; cessazione aperta `2099-12-31`; `eseguito` `SI`, `NO` negli ultimi sette giorni, a volte vuoto; forniture `116000001`-`116000212`, una per contenitore, utenza e servizio. Domestiche: categoria `31`, `IA_31`, `D`; non domestiche: `44`, `68` o `71`, `IA_50`, `ND`, nucleo 0; qualche domestica senza decorrenza; una domestica del secco cambia nucleo il 1/7/2025, un'altra ha il potenziamento dal 1/6/2025. Semi a parte (2031 nucleo e categoria, 2032 volume, 2033 conferma): le letture non cambiano. Per i conferimenti l'esempio non è realistico (bidoni esposti quasi sempre, senza vicini).
- Dodici giri del calendario diventano otto servizi. Nei giri dei servizi non passa nessun censito (servizio atteso `SERVIZI`). Il giro dell'umido di `POVE DEL GRAPPA` ha come comune la zona `MASSICCIO DEL GRAPPA` (avviso al caricamento).
- Comune di lettura dal confine (`comune_del_punto()`): nessuna lettura fuori dai comuni serviti, 1.326 in un comune diverso dal giro. Coerenza del comune: 5.460 coerenti, 389 fuori comune (4 RFID: 362 del tag sul mezzo, 8 del bidone spostato, 19 di due censiti oltre il confine), 605 fuori giro (36 RFID, 465 del tag in deposito), 21 ignote (8 sacchetti censiti, 13 non censiti del giro di zona). 45 letture in un altro comune entro la tolleranza sono coerenti (38 di censiti, 28 del tag sul mezzo; 7 di non censiti).
- Coerenza del servizio: 5.376 `SI`, 23 `NO` (tutte in `SECCO PAP`: `D241007190` 10, `D241011121` 11, `D250901001` 1, `D250905100` 1), 1.076 vuote (non censiti e sacchetti).
- 37 bidoni non censiti: nel 2025 18 con servizio stimato, 19 incerti. Sacchetti: 8 censiti (forniture `200000001`-`200000004`, senza utenza, servizio, volume, comune) e 4 no; letti dai giri del secco; 2 nel 2024, 10 nel 2025.
- Per ultima lettura, tutto il file: 251 RFID, 3 fuori comune, 18 fuori giro, 9 ignoti, 2 non coerenti; per cantiere 33, 66, 93, 59 (per RFID, dal comune dichiarato: 33, 66, 91, 61). Solo 2025: 248 RFID, cantieri 33, 64, 92, 59, 3 fuori comune, 18 fuori giro, 8 ignoti. Cluster 2025: 238 RFID (senza sacchetti), 4 cluster `FUORI_COMUNE`, 18 `FUORI_GIRO`. RFID con letture `SECCO PAP`: 115. Servizio atteso `SECCO` nel 2025: 87 RFID, 5 non censiti.
- Lo script genera prima le letture dei bidoni, poi geografia e sacchetti con semi a parte. Il giro di ogni lettura viene senza numeri casuali da `giro_del_mezzo()` e `indice_turno_del_mezzo()`; il servizio atteso da `servizio_atteso_dal_giro()`; la distanza da `distanza_comune_dichiarato()`; cantiere e coerenza da `aggiungi_geografia()`. Aggiungere comuni ridistribuisce i bidoni e cambia i conteggi per comune.

| RFID dell'esempio | Caso |
|---|---|
| `D250901001` | Servizio CARTA, SECCO, di nuovo CARTA |
| `D250901050` | Utenza da `58590001` a `58590025` |
| `D250915201` | Non censito, 4 letture, stima 100% `SECCO`, `GHOST_TAG` |
| `D250920250` | Non censito, stima incerta: `SECCO` e `CARTA` |
| `D250905100` | Spostato di 17 km: censito a `LIMENA`, poi 8 letture a `CITTADELLA` (fuori comune); `RELOCATED_BIN`, secondo cluster `FUORI_COMUNE` |
| `D241001301` | Non censito fermo in deposito a `CAMPOSAMPIERO`, letto da tutti i mezzi: stima incerta, 465 letture fuori giro su 514; `DEPOT_STUCK` |
| `D250203302` | Tag sul mezzo `1604`, censito a `MAROSTICA`, letto tra `MAROSTICA`, `BASSANO DEL GRAPPA` e `NOVE`: 362 fuori comune, 28 coerenti al confine; `TRUCK_STOWAWAY` |
| `D241003303` | GPS impreciso, 17 cluster; `SCATTERED_READS / GPS_NOISE` |
| `00BD00000000000000000005` | Sacchetto censito, fornitura `200000002`, letto a `ROANA` nel 2025 (censiti `…0001`-`…0008`) |
| `00BD00000000000000000102` | Sacchetto non censito, `ROANA`, 2025 (non censiti `…0101`-`…0104`) |
| mezzo `1601`, 24/9/2025 | Giro sbagliato: calendario secco a `LIMENA`, dieci bidoni letti a `BORGORICCO`; nove censiti lì e coerenti, fuori giro solo `D241009227` |

### Dati reali e pipeline

I dati aziendali non sono nel repository. Due passi:

| Passo | Dove | Scrive |
|---|---|---|
| 1, estrazione, a mano | `data-raw/01_estrai_tabelle.R`, via ODBC. **Non eseguirlo se non richiesto: si collega al database aziendale** | un CSV per tabella in `data-raw/origine/`, nomi fissi di `file_origine()` |
| 2, pipeline {targets} | `data-raw/_targets.R` (`_targets.yaml` nella radice; archivio in `data-raw/_targets/`) | in `data-raw/output/`: `letture_app.csv`, `letture_app.duckdb`, `controllo_giri.csv`, `letture_storiche.csv`, `comuni_corretti.csv` |

- Pipeline chiesta il 19/10/2026 al posto degli script dei passi 2-5. Niente tabelle intermedie; dal 22/10/2026 i due dataset sono target leggibili con `tar_load(letture_app)` e `tar_load(letture_storiche)` (salvati in `data-raw/_targets/objects/`), e i file si scrivono in target a sé. Target: i file di partenza `file_antenne` (test, letture, calendari), `file_anagrafica` (contenitori, sacchetti, componenti, manovre: comune ai due dataset), `file_storiche`; i dataset `letture_app` (con gli attributi `controllo_giri` e `comuni_corretti`) e `letture_storiche`; i file `csv_letture_app`, `duckdb_letture_app` (da quel CSV, con `scrivi_archivio()`), `csv_controllo_giri`, `csv_comuni_corretti` (letture il cui comune del GPS è stato sostituito da quello della posizione: istante, mezzo, RFID, coordinate, `comune_gps`, `comune_fornitura`, `comune_posizione`), `csv_letture_storiche` (tutti con `scrivi_csv_output()`, `giri_da_controllare()`, `comuni_corretti()`). I report non sono target (dal 23/10/2026). Con `pkgload::load_all()` e `tar_option_set(imports = "trackerfid")` una funzione cambiata rifà i target che la usano (e tutta la preparazione delle letture). In cima allo script: colonne delle letture storiche.
- `prepara_letture_app()`: `importa_*()` di `R/fct_importa_tabelle.R`, poi `filtra_letture_post_test()`, `formatta_rfid()`, `associa_servizio()`, `correggi_comune_lettura()`, `unisci_calendari()` + `associa_servizio_atteso_da_calendario()` (giro, servizio atteso, coerenze, distanza; 28 colonne; un messaggio conta le letture di ogni passo), `controllo_giri()`, `associa_componenti()`, `associa_potenziamento()` (35 colonne), `filtra_codici_app()`. I file li scrivono i target.
- `importa_letture()` prende il comune dal testo dopo la prima virgola di `IndirizzoLocalizzazione` (scartando le letture con latitudine zero o senza comune: per il proprietario sono dati sporchi) e tiene, per ogni RFID, la prima lettura del giorno (scelta del proprietario del 22/10/2026: prima i filtri, poi la deduplica, così resta la prima lettura valida); `correggi_comune_lettura()` lo sostituisce con il comune servito che contiene il punto; il cantiere viene dalla tabella per nome esatto. `associa_servizio()` prende contenitore attivo nel giorno e fornitura (`id_servizio`, `stato_servizio`, `comune_servizio` diventano `id_fornitura`, `stato_fornitura`, `comune_fornitura`); un sacchetto prende solo la fornitura del gruppo; `Presente` se c'è una fornitura. Correzioni dei nomi solo nell'importazione: `LUSINA CONCO` → `LUSIANA CONCO`, `VALSTAGNA` → `VALBRENTA` (contenitori), `ROSA'` → `ROSÀ` (calendari). Un nome assente dalla conversione dei servizi resta com'è con un avviso.
- `filtra_codici_app()` scarta, in tutti e due i dataset, i sacchetti (`00BD`, scelta del proprietario) e i codici con dieci zeri iniziali. `importa_letture_storiche()` applica `formatta_rfid()`, toglie le righe ripetute e scarta con un avviso gli istanti illeggibili (`AAAA-MM-GG` o `GG/MM/AAAA`, nel fuso della sessione); i nomi delle tre colonne stanno in cima a `data-raw/_targets.R`.
- Date e ore di letture e turni nel fuso della sessione di R, come il codice del proprietario: devono restare nello stesso fuso.
- Tolte e da non rimettere: `completa_geografia()`, `associa_servizio_atteso_dai_censiti()` (servizio dei non censiti dai censiti dello stesso mezzo nella mezz'ora), il parametro `df_tag`, la tabella dei tag dei contenitori, gli script numerati, `data-raw/intermedi/` (cartella superstite).
- Senza dati reali si prova con i file sintetici di `scrivi_origine_prova()` (`tests/testthat/helper-dati.R`).

## Architettura

```
file (browser o run_app(letture = ))
  └─ mod_caricamento ─► archivio DuckDB validato (comuni, cantiere, coerenze, riga)
       ├─ mod_periodo ─► selezione del periodo, con la tabella della stima
       │    ├─ mod_filtri ─► dati_filtrati (una riga per RFID) e area ─► mod_mappa "mappa_principale"
       │    ├─ mod_ricerca "ricerca_rfid" / "ricerca_utenza" ─► mod_mappa "mappa_rfid" / "mappa_utenza"
       │    ├─ mod_giro "giro" ─► mod_mappa "mappa_giro"
       │    └─ dettaglio del bidone (app_server, letture_del_rfid())
       └─ mod_cluster_analysis: letture_periodo() al click di «Genera Analisi»
```

In R arrivano solo data frame piccoli, salvo l'analisi dei cluster. `R/app_server.R` ha solo collegamenti, titolo e dettaglio. Le funzioni `R/fct_*.R` stanno fuori dall'app: preparano il CSV.

### Moduli (`R/mod_<nome>_ui.R` e `_server.R`)

| Modulo (id) | Riceve → restituisce | Input |
|---|---|---|
| `mod_caricamento` (`caricamento`) | `demo`, `percorso` (da `run_app()`) → reactive con l'archivio o `NULL`; `output$riassunto` | `file_upload`, `carica_esempio` |
| `mod_periodo` (`periodo`) | archivio → `periodo`, `etichetta`, `dati` (selezione); UI = un `li` per `dashboardHeader()` | `intervallo` |
| `mod_filtri` (`filtri`) | selezione, `azzera` → `dati_filtrati` (attributo `letture`), `area`. Due UI con lo stesso id: `mod_filtri_ui()` (riga dei filtri), `mod_filtri_sidebar_ui()` (statistiche) | `cantiere_check`, `comuni`, `coerenza_comune_check`, `presente_filter`, `servizio_check`, `servizio_tutti`, `servizio_nessuno`, `includi_stima_incerta`, `servizio_giro_check`, `giro_tutti`, `giro_nessuno`, `coerenza_check`, `reset` |
| `mod_ricerca` (`ricerca_rfid`, `ricerca_utenza`) | selezione, `tipo` → `risultati`, `codici`; per RFID `output$letture` (`mod_ricerca_tabella_ui()`) | `testo`, `cerca`, `pulisci` |
| `mod_giro` (`giro`) | selezione → `risultati` (letture del giro scelto o `NULL`), `riepilogo`, `messaggio`, `vista` (`area_giri()`). UI: `mod_giro_sidebar_ui()`, `mod_giro_ui()` (tabella) | `mezzo`, `giorni`, `solo`, `tabella__reactable__selected` |
| `mod_mappa` (`mappa_principale`, `mappa_rfid`, `mappa_giro`, `mappa_utenza`) | `dati`, `modalita`, `dati_vista`, `attiva`, `messaggio`; UI con `sovrapposto` (la tabella di una ricerca) → marker cliccato (`rfid`, `quando`) | `mappa_clic` (da `mappa.js`), `mappa_zoom` (solo intestazione) |
| `mod_cluster_analysis` (`cluster`) | archivio, `periodo`, `etichetta` → cluster scelto. UI: `mod_cluster_analysis_ui()`, `_sidebar_ui()` | `eps_m`, `min_pts`, `genera`, `tabella_rows_all`, `tabella_rows_selected`, `mappa_clic` |

Note: un file da `run_app(letture = )` è letto da `leggi_una_volta()` una volta per processo e condiviso (`condiviso = TRUE`, mai chiuso); un file dal browser è della sessione e `chiudi_archivio()` lo chiude. `mod_mappa` ha le modalità `cluster`, `rfid`, `utenza`, `giro` (più `analisi` per la mappa dei cluster, senza modulo); base disegnata una volta con `renderMaplibre()`, poi messaggi di `invia_mappa()`: in `cluster` `bolle`, `dati`, `inquadra`; nelle altre `dati`, `inquadra`; al click il server risponde con `popup`. Stato nel `reactiveVal` `disegnati`. `rfid` disegna `limita_letture_ricerca()`, `giro` `limita_letture_giro()`. `mod_giro` torna all'ultimo giorno a ogni cambio delle letture; il giro scelto vale finché la ricerca non cambia (reactive interno `trovate`). Nell'analisi dei cluster grafici e mappa seguono le righe della tabella (`righe_tabella`: `NULL` tutte, vuoto nessuna; azzerato a ogni analisi; letto dopo `attesa_righe_tabella()`).

### Scelte di progetto da rispettare

1. **Logica fuori dai moduli**: calcoli, HTML, mappe e grafici in `R/utils_*.R`, senza reattività e testati. Prefisso `fct_` solo per la preparazione dei dati (fuori dall'app).
2. **Stato sul server** in `reactiveValues`, riallineato agli input; al nuovo dataset torna subito ai predefiniti. `observeEvent` sugli input con `ignoreInit = TRUE`.
3. **Esclusione**: servizi, giri, cantieri e le due coerenze si memorizzano come esclusi (`coerenza_escluse`, `coerenza_comune_escluse`), così restano validi se l'elenco cambia; «Nessuno» esclude anche tutti i servizi di `servizi_config()`. I comuni si memorizzano come scelti (vuoto = tutti); quelli di un cantiere deselezionato escono.
4. **Filtri in SQL prima della deduplica**: `condizione_filtri()` su tutte le letture del periodo, poi `sql_ultime()` (`arg_max()` con `GROUP BY`) tiene l'ultima per RFID; mai estrarre il periodo in R per filtrarlo.
5. **Un servizio solo** per icona, filtri e statistiche: `servizio_icona`, calcolato da `sql_con_icona()` con `tabella_stima()` (una tabella temporanea per periodo, condivisa, ricalcolarla a ogni filtro rallentava); in R lo legge `servizio_mostrato()`. Censito: `servizio_transponder`; non censito: servizio stimato dell'RFID; sacchetto: `SACCHETTI`. `aggiungi_servizio_icona()` e `servizio_prevalente_non_censiti()` restano come regola di riferimento nei test.
6. **Validazione in SQL, regole in R come riferimento**: `valida_archivio()` applica le regole di `coerenza_servizio()`, `coerenza_comune()`, `aggiungi_geografia()`; le funzioni in R le usano pipeline, dataset di esempio e test, che confrontano i risultati. Una regola nuova va scritta nei due posti. Così `rfid_sacchetto()` e `sql_sacchetto()`.
7. **DuckDB**: archivio in memoria, tabella `letture` con le colonne di `colonne_app()` più `riga` (schema 5, `versione_archivio()`: va aumentata se cambiano le colonne, e un `.duckdb` di altra versione viene rifiutato). Connessione con `allow_extensions = FALSE` e `shared_home = FALSE`: niente ICU (niente fusi, `SET TimeZone` darebbe errore), niente download. Il `.duckdb` della pipeline (`scrivi_archivio()`: tabelle `letture` e `informazioni`) si apre in sola lettura senza copie (`apri_file_archivio()`).
8. **Orari senza fuso**: date e ore sempre lette e scritte in UTC come ora locale; non convertire.
9. **Mappe WebGL**: R prepara la configurazione (`configurazione_mappa()`: icone, sorgenti, livelli, interattivi, confini), `mappa_base()` la passa a `mapgl::maplibre()` con `htmlwidgets::onRender()`, `inst/app/www/mappa.js` crea livelli e applica i messaggi. Dati in colonne compatte (coordinate intere in centomillesimi, numero dell'icona), non GeoJSON; di mapgl solo `maplibre()`, `renderMaplibre()`, `maplibreOutput()` (il proxy vuole `sf` e GeoJSON pesante). I messaggi partono solo con la scheda visibile; `mappa.js` tiene l'ultimo di ogni tipo finché la mappa non è pronta.
10. **Marker SVG** da `svg_marker()`; `icone_mappa()` manda tutte le combinazioni (stato, glifo, stile, segnali), registrate al doppio della risoluzione; ogni punto porta `indice_icona()` e la sua posizione `i` da 0, con cui il click risale alla lettura.
11. **Bolle dal server, zoom nel browser**: `aggrega_marker()` a ogni filtro; il browser mostra ogni livello nella sua fascia (`minzoom`/`maxzoom` di `livelli_mappa()`). Le bolle partono prima dei bidoni (raggruppare 100.000 bidoni costa mezzo secondo). Bolla di un comune nel baricentro (`baricentri_comuni()`), ferma al variare dei filtri; cantieri e comuni fuori tabella nel punto medio dei bidoni. Con Leaflet il server sceglieva la vista: `scegli_vista()` e simili non ci sono più.
12. **Zoom di MapLibre** (tessere da 512 px, zoom `z` = Leaflet `z + 1`): `soglie_zoom()` 10 e 13, unico punto; zoom massimo di un inquadramento 16.
13. **Confini come sorgente a parte** `confini`, livelli `confini-riempimento` e `confini-bordo` sotto le etichette (`sotto_etichette`), non interattivi. Il file non viaggia nella configurazione (1,25 MB per cinque mappe): è una risorsa statica (`risorsa_confini()`), la configurazione porta l'indirizzo con la data (`indirizzo_confini()`), MapLibre lo scarica nel worker. Colore dall'espressione di `espressione_colore_cantiere()`; colori per nome in `colori_cantieri()` (distinguibili a coppie anche con un difetto della visione, lontani da verde, rosso e arancio): un cantiere nuovo va aggiunto lì, altrimenti è grigio.
14. **Calcoli per gruppo senza cicli nell'app**: `R/utils_gruppi.R` su vettori interi; testi ripetuti trattati una volta con `per_valori_distinti()` (non serve in SQL).
15. **Il comune di una lettura viene dal file** (`aggiungi_geografia()` lo porta solo alla grafia della tabella); da lì comune geografico, assegnato e cantiere. `comune_lettura` si mostra e si confronta; `comune_geografico` (vuoto fuori tabella) si usa nei conti. **Il calendario non assegna comuni**: `comune_giro` serve solo al fuori giro dei non censiti e alla scheda dei giri.
16. **Coerenza del comune per nome**: `coerenza_comune()` confronta `chiave_comune()`; l'app legge `distanza_comune_dichiarato` e la confronta con `tolleranza_confine_m()`, in R e in SQL, senza toccare i confini. `R/utils_comune_geografico.R` (griglia di 55 m, tolleranza di 1 km) è stato tolto: non rimetterlo. Nell'app i confini servono solo al disegno, ai baricentri e a contorno e inquadratura dei giri (`lati_comuni()`, `indice_comune()`); `comune_del_punto()` e `distanza_dal_comune()` (con `misura_comune()`) servono a pipeline ed esempio.
17. **Incongruenze calcolate una volta**: la colonna `coerenza_comune`; chi vuole vettori logici usa `incongruenze_comune()`. Una lettura non è mai fuori comune e fuori giro insieme.
18. **Giri sulle colonne delle letture**: un giro è il gruppo con stesso giorno, mezzo, `servizio_giro`, `comune_giro` (`riepilogo_giri()`); niente pulsante «Cerca»; all'avvio un giorno solo.
19. **Servizio atteso con il nome della tipologia, giro con il nome del calendario**: la conversione è `tipologie_servizio()` (nome uniforme = tipologia in maiuscolo, sempre riconosciuto da `servizi_config()`); la validazione non converte. Tre colonne dei servizi come tre dei comuni (per due giorni a ottobre 2026 erano una, con `origine_servizio`).
20. **Coerenza del servizio per tipologia**, `SI`/`NO` come nei dati del proprietario, calcolata una volta (pipeline e validazione); l'interfaccia usa `etichette_coerenza()`.
21. **Due coerenze, nomi distinti**: servizi `coerenza_servizio`, `stati_coerenza()`, `coerenza_check`, «Coerenza servizio»; comuni `coerenza_comune`, `stati_coerenza_comune()`, `coerenza_comune_check`, «Coerenza comune». Nell'indice di fiducia la verifica sul servizio è `stabilita` (soglia `prevalenza_atteso`); il caso del dettaglio è `censito_stabile`.
22. **Sacchetti**: riconosciuti dal codice; servizio `SACCHETTI` per icona, filtri, statistiche (non è un valore del file); `calcola_analisi_cluster()` li toglie con `senza_sacchetti()` prima di ogni conto. Un'analisi nuova sui contenitori deve fare lo stesso.
23. **Filtri in menu** costruiti da `menu_filtro()`; il comune è un selectize nella riga, fuori dai menu. `script.js`: il terzo gestore ferma i click dentro un menu, il quarto conta le spunte a ogni `change`. La riga ha uno z-index sopra i controlli della mappa.
24. **Elenchi a discesa uguali**: sezione "Elenchi a discesa" del CSS (classe `elenco-comuni`); un elenco nuovo usa gli stessi stili.
25. **Analisi dei cluster in una funzione**: `calcola_analisi_cluster()`, usata da app, `genera_analisi_cluster()` e script a riga di comando. Volume e raccolte con due nomi (`alias_colonne()`).
26. **Bidoni nello stesso punto**: niente raggiera; elenco al click (`elencoCluster()`).
27. **Pipeline leggibile** (21/10/2026): le funzioni di `R/fct_*.R` sono scritte con dplyr e pipe, il più leggibili possibile, senza badare alla velocità; le regole di «File grandi» valgono per l'app.

## Mappa del repository

```
R/  run_app.R, app_config.R, app_ui.R, app_server.R
    mod_{caricamento,periodo,filtri,ricerca,giro,mappa,cluster_analysis}_{ui,server}.R
    utils_archivio.R        archivio DuckDB: lettura, validazione SQL, .duckdb
    utils_selezione.R       periodo, filtri, ultima lettura, ricerche, giri, dettaglio in SQL
    utils_data_processing.R schema, servizi, coerenze, statistiche, storia di un RFID
    utils_styling.R         tipologie, colori, marker SVG, legenda
    utils_mapping.R         configurazione WebGL, dati per il browser, popup
    utils_popup.R           popup, dettaglio, statistiche
    utils_geografia.R       comuni, cantieri, confini, geografia delle letture
    utils_gruppi.R          calcoli per gruppo su vettori
    utils_mappa_aggregata.R bolle, fasce di zoom, limiti delle ricerche
    utils_giri.R            riepilogo, tabella e mappa dei giri
    utils_tabelle_mappa.R   tabelle delle ricerche sopra le mappe
    utils_dbscan_clustering.R, utils_cluster_output.R, utils_cluster_viste.R   analisi dei cluster
    utils_conferimenti.R    conferimenti del secco (solo report)
    utils_confronto.R       filtri e confronto saponette-antenne (solo report)
    utils_confronto_analisi.R  analisi del report confronto_antenne (solo report)
    utils_controllo_giri.R  giri di controllo con antenne e saponette (solo report)
    fct_importa_tabelle.R, fct_preparazione_letture.R   pipeline dei dati reali
    fct_preparazione_comuni.R   confini dallo shapefile della Regione
    generate_cluster_analysis.R genera_analisi_cluster(), scrivi_analisi_cluster() (esportate)
inst/app/www/  custom_style.css; script.js (cinque gestori: dettaglio in vista, ridimensiona le mappe
               tornate visibili, menu aperti, stato dei menu, legende); mappa.js; favicon.png
inst/extdata/  dataset di esempio, analisi dei cluster di esempio, comuni_cantieri.csv, comuni_confini.geojson
inst/scripts/generate_cluster_analysis.R   analisi da riga di comando
data-raw/      genera_sample_dataset.R, genera_dataset_grande.R, prepara_comuni.R, rvccomuni/,
               01_estrai_tabelle.R, _targets.R; origine/, intermedi/, output/, _targets/ fuori da git
docs/          analisi_cluster.md; Quarto fuori dalla pipeline: indice_fiducia.qmd, conferimenti_secco.qmd, confronto_antenne.qmd, controllo_giri_saponette.qmd, associazione_giri.qmd, comune_lettura.qmd, stile.css (gli HTML dei documenti stanno accanto, quelli dei report sui dati reali in data-raw/output/)
tests/testthat helper-dati.R (dati di prova, archivio da data frame) e un test-*.R per file
dev/           run_dev.R, misura_prestazioni.R; 01-03 di golem, mai eseguiti
man/, NAMESPACE generati da roxygen2
```

`data-raw/`, `dev/`, `docs/`, `_targets.yaml`, `.claude/` (file degli strumenti dell'assistente) e questo file sono in `.Rbuildignore`. Esportate solo `run_app()`, `genera_analisi_cluster()`, `scrivi_analisi_cluster()`.

## Dove intervenire

Le funzioni hanno un titolo roxygen breve: per trovarle, `grep` sul nome. Punti unici e collegamenti non ovvi:

| Per cambiare | Dove (test) |
|---|---|
| Servizi, giri riconosciuti, conversione, icone, colori, emoji | `tipologie_servizio()` (`test-utils_styling.R`) |
| Passi dell'associazione al giro, ritardo, tolleranza, soglia dei 2 km, mezzi senza giri | `scegli_giro()`, `passi_associazione()`, `allunga_fine()`, `assegna_tratti()`, `ritardo_giro_minuti()`, `tolleranza_giro_minuti()`, `soglia_comune_vicino_m()`, `servizio_dei_censiti_del_mezzo()`, `unisci_calendari()` (`test-fct_preparazione_letture.R`, `test-fct_importa_tabelle.R`); **poi aggiornare e ricompilare `docs/associazione_giri.qmd`** |
| Giri dei servizi | `giri_servizio_dai_censiti()` e la tipologia «Servizi»; regola in `servizio_atteso_dal_giro()` |
| Coerenza del servizio o del comune, geografia di una lettura | funzione in R e stessa regola SQL in `valida_archivio()`; poi rigenerare esempio e analisi di esempio (`test-utils_data_processing.R`, `test-utils_geografia.R`, `test-sample_dataset.R`) |
| Tolleranza dal confine | `tolleranza_confine_m()`; rigenerare esempio, analisi di esempio e `.duckdb` |
| Comune di lettura (GPS, posizione, fornitura), distanza, controllo dei giri | `correggi_comune_lettura()`, `una_lettura_al_giorno()`, `distanza_comune_dichiarato()`, `controllo_giri()`; **poi aggiornare e ricompilare `docs/comune_lettura.qmd`** |
| Soglia dell'80% | `soglia` di `tabella_stima()` e di `aggiungi_servizio_icona()` |
| Filtri della mappa principale | UI `mod_filtri_ui.R` con `menu_filtro()`, stato `mod_filtri_server.R`, regola `condizione_filtri()`, voci `scelte_periodo()`, area `area_filtri()`; CSS "Filtri sopra la mappa principale"; gestori 3 e 4 di `script.js` (`test-moduli.R`) |
| Sacchetti | `rfid_sacchetto()` e `sql_sacchetto()` insieme; `senza_sacchetti()`; testi `blocco_sacchetto()` |
| Popup, dettaglio, statistiche | `create_popup_html()`, `descrivi_servizi()`; `analizza_rfid()` e `blocco_*()`; `calcola_statistiche()` e `crea_box_statistiche()` |
| Comuni e cantieri | `inst/extdata/comuni_cantieri.csv`, poi `Rscript data-raw/prepara_comuni.R` e `Rscript data-raw/genera_sample_dataset.R` |
| Confini | shapefile in `data-raw/rvccomuni/`, `R/fct_preparazione_comuni.R`, poi i due script sopra |
| Colori di stato, cantieri, confini, cluster e bolle | `colori_stato()`, `colori_cantieri()`, `stile_confini()`, `espressioni_gruppo()`, `livelli_gruppo()`, `lato_bolla()` |
| Marker, segnali, legenda | `svg_marker()`, `svg_segnale()`, `icone_mappa()`, `html_legenda()` (classi `legenda-dettagli`, `legenda-intestazione`, gestore 5) |
| Fasce di zoom | solo `soglie_zoom()`; i test usano zoom scritti a mano |
| Click e passaggio del mouse sulla mappa | `apriBolla()`, `apriCluster()`, `scegliPunto()`, `testoAlPassaggio()`, `elencoCluster()` in `mappa.js`; `zoom_minimo_bolla()`, `popup_lettura()`, `testo` in `livelli_mappa()` |
| Sfondo | `stile_mappa()` o opzione `trackerfid.stile_mappa`; `stile_ripiego()` e gestore `error` in `inizia()` |
| Tabelle delle ricerche | `riquadro_tabella_mappa()`, `tema_tabelle_mappa()`, `tabella_letture_rfid()`, `tabella_giri()`, `filtro_numerico_js()`; CSS "Tabelle delle ricerche sopra la mappa" |
| Scheda dei giri | `cerca_giri()`, `riepilogo_giri()`, `area_giri()`, `contorni_comuni_giro()` (livello `comuni-giro`) |
| Limiti dei disegni | `limiti_mappa()` (opzioni `trackerfid.max_letture_ricerca`, `trackerfid.letture_per_rfid`), `limiti_viste_cluster()` |
| Periodo nella barra | `mod_periodo_ui()` in `dashboardHeader()`; CSS con `.datepicker.dropdown-menu` e z-index di `.main-header` |
| Sezioni collassabili | `sezione_sidebar()` (`collassabile`, `riassunto`) |
| Colonne, formati, validazione | `colonne_obbligatorie()`, `colonne_facoltative()`, `formati_data_ora()`, `valida_archivio()`; con colonne nuove `versione_archivio()` (`test-utils_archivio.R`) |
| Analisi dei cluster | `soglie_indicatore()`, `classifica_cluster()`, `pesi_fiducia()`, `indice_fiducia()`, `indicatori_config()`; colonne in `colonne_analisi_cluster()`, `analisi_cluster_vuota()` e ultimo `mutate` di `calcola_analisi_cluster()` (29, devono coincidere). Aggiornare `docs/analisi_cluster.md` e `README.md`; `docs/indice_fiducia.qmd` si ferma se il testo non torna |
| Confronto saponette-antenne | `filtra_letture()`, `storiche_di_confronto()`, `confronta_letture()` in `R/utils_confronto.R`; le analisi in `R/utils_confronto_analisi.R` (`test-utils_confronto.R`, `test-utils_confronto_analisi.R`) |
| Controllo dei giri con antenne e saponette | `R/utils_controllo_giri.R` (`test-utils_controllo_giri.R`) e `docs/controllo_giri_saponette.qmd` |
| Conferimenti | `R/utils_conferimenti.R` e il report, che si ferma se la simulazione non conferma il testo; periodo con i parametri `dal` e `al` del report |
| Pipeline: file, target, filtri finali, letture storiche | `data-raw/_targets.R`, `file_origine()`, `filtra_codici_app()`, `scrivi_csv_output()`, `prepara_letture_storiche()`, `colonne_letture_storiche()` |
| Upload massimo | opzione `trackerfid.max_upload_mb` (2 GB) |
| Favicon | `inst/app/www/favicon.png`, 192 px; l'immagine di origine non è nel repository |
| Una scheda nuova | solo se il proprietario la chiede |
| Dipendenze | `DESCRIPTION`, poi `devtools::document()` e `devtools::check()` |

## File grandi

Misure del 16/10/2026 su 2 milioni di letture di 100.000 RFID (`dev/misura_prestazioni.R`; Brave senza scheda grafica): lettura e validazione 3,7 s da CSV (534 MB), 4 s gzip, 1,4 s Parquet; `.duckdb` scritto in 0,75 s, aperto in 0,06 s (60 MB); selezione del periodo 0,03 s; voci dei filtri 0,05 s; ultima lettura 0,17 s; cambio di filtro 0,1-0,2 s; dati della mappa 0,1 s più 0,1 s di serializzazione (3-5 MB); nel browser bolle dopo 0,3-0,5 s, bidoni dopo 1,2-1,7 s; ricerca RFID 0,01 s, dettaglio 0,04 s; giri: un mezzo-giorno 0,01 s, tutti i mezzi in un mese 1 s; analisi dei cluster 1,7 s di estrazione più 4-4,7 s. Con tutte le colonne della pipeline la validazione passa da 3,7 a 5 s e il `.duckdb` da 46 a 63 MB: per questo l'app tiene le sue. Memoria: 0,55 GB con il `.duckdb`, 2,5 GB dopo un CSV (DuckDB ne usa 0,65; il resto l'allocatore non lo rende subito). Nella pipeline: distanza su 316.000 letture circa 10 s, `comune_del_punto()` su 1,9 milioni meno di 2 s; associazione ai giri circa 18 s su 2 milioni (23/10/2026, dati sintetici). I tempi variano fino a una volta e mezza tra sessioni: confrontare versioni misurandole di seguito.

Regole per il codice dell'app:

- Le letture restano in DuckDB: filtri, ultima lettura, conteggi e ricerche in SQL in `R/utils_selezione.R`; niente `letture_archivio()` o `letture_periodo()` a ogni filtro.
- `arg_max()` con `GROUP BY`, non funzioni finestra. Quello che non dipende dai filtri, una volta per periodo.
- Sui data frame grandi niente `summarise()`, `mutate()` o `slice_min()` per gruppo: `R/utils_gruppi.R`, o ordinare e tenere la prima riga. Niente regex con alternative su milioni di celle: `stringr::fixed()`.
- Al browser colonne compatte, non GeoJSON; grafico e mappa dei cluster sotto `limiti_viste_cluster()`.
- Prove: `Rscript data-raw/genera_dataset_grande.R <cartella>`, `Rscript dev/misura_prestazioni.R <file>`; i file di prova fuori da git.

## Comandi

```r
devtools::load_all(); run_app(demo = TRUE); run_app(letture = "percorso")
scrivi_archivio("letture_app.csv", "letture_app.duckdb")
source("dev/run_dev.R")                       # documenta e avvia
devtools::test(); devtools::test(filter = "utils_cluster"); devtools::document(); devtools::check()
quarto::quarto_render("docs/indice_fiducia.qmd"); quarto::quarto_render("docs/associazione_giri.qmd"); quarto::quarto_render("docs/comune_lettura.qmd")   # documenti sul dataset di esempio, fuori dalla pipeline; serve il programma quarto (quello di Positron va bene)
source("data-raw/compila_report.R"); compila_report("docs/confronto_antenne.qmd", list(servizio = "SECCO", mezzi_totali = 40))   # report sui dati reali: HTML in data-raw/output/
targets::tar_make(); targets::tar_load(letture_app); targets::tar_make(names = duckdb_letture_app); targets::tar_outdated(); targets::tar_visnetwork()
```

```sh
Rscript data-raw/genera_sample_dataset.R
Rscript data-raw/prepara_comuni.R
Rscript inst/scripts/generate_cluster_analysis.R inst/extdata/sample_rfid_dataset.csv 2025-01-01 2025-12-31 inst/extdata
```

## Convenzioni e vincoli

- **Italiano ovunque.**
- **Solo ASCII nel codice R**: nelle stringhe di `R/` escape `"è"`; nei commenti gli accenti vanno bene. Verifica: `tools:::.check_package_ASCII_code(".")` = `character(0)`.
- **Commenti e roxygen brevi** (richiesta del 21/10/2026): titolo e solo ciò che il codice non dice; la storia delle decisioni sta in questo file. Funzioni interne con `@noRd`; nei roxygen i nomi delle funzioni interne tra accenti gravi, mai tra parentesi quadre (avvisi a ogni avvio).
- **Nomi dei campi data** (22/10/2026, scelta del proprietario): `data_...` per una data `AAAA-MM-GG`, `data_ora_...` per un istante (data e ora, con minuti o secondi). Letture: `data_ora_lettura` (era `giorno_lettura`) e `data_lettura` (era `giorno`); calendari, giri e controllo dei giri: `data_giro`; riepilogo dei giri: `data_giro`, `data_ora_prima_lettura`, `data_ora_ultima_lettura`; conferimenti dei non censiti: `data_prima_lettura`, `data_ultima_lettura`, `data_ultima_lettura_candidato`; confronto saponette-antenne: `data_lettura`. Restano i nomi grezzi del database e le colonne senza data (`ora_inizio`, `ora_fine`). `alias_colonne()` porta i nomi vecchi a quelli nuovi; la tabella dell'analisi dei cluster (`globale_prima_lettura`, `analisi_dal`...) non è stata rinominata. Un campo data o istante nuovo segue la regola.
- **Stile**: pipe nativa, `.data$colonna` dentro dplyr, due spazi.
- **Test**: ogni funzione di utilità ha i suoi; moduli con `shiny::testServer()`; un comportamento nuovo aggiorna i test. Una funzione che serve solo ai test va in `helper-dati.R`, non in `R/`.
- **`devtools::check()`**: 0 errori, 0 note, 1 avviso sul campo `License`.
- **Dipendenze facoltative** (`Suggests`): `nanoparquet` (Parquet di prova), `geosphere` (due test), `sf` (solo `leggi_confini_regione()`), e dal 21/10/2026 le dipendenze della sola pipeline, `data.table`, `targets`, `odbc`, con `requireNamespace()` dove servono. L'app non usa `sf` (i confini si leggono con `jsonlite`), ma `sf` e `terra` arrivano con `mapgl` (su Linux GDAL, GEOS, PROJ).
- **Licenza**: il campo `License` è un segnaposto; la sceglie il proprietario.
- **Git**: i commit li fa il proprietario; non committare se non richiesto.
- **Dati aziendali** solo in `data-raw/origine/`, `intermedi/`, `_targets/`, `output/` (fuori da git): mai in `inst/` o `data/`, mai il loro contenuto in test, documenti o messaggi; i report compilati sui dati reali vanno in `data-raw/output/` con `output_dir`, mai in `docs/`.

## Trappole note

- **Mappe nascoste**: MapLibre non conosce la sua misura in un contenitore invisibile. `mod_mappa` manda dati solo a scheda attiva, `mappa.js` rimanda l'inquadratura, il secondo gestore di `script.js` chiama `TrackerfidMappa.ridimensiona()` su `shown.bs.tab` (riceve anche lo `shown` dei `conditionalPanel`; il solo `shown` no). Vale anche per la mappa dei cluster, nata nella vista nascosta «Mappa».
- **Altezza di `.content-wrapper`**: il tema la imposta all'altezza della barra laterale; il CSS la riporta alla finestra con `!important`, altrimenti la pagina scorre nel vuoto.
- **`summarise()` e `tibble()`**: una colonna appena creata nasconde l'originale (e in `riepilogo_giri()` la colonna `letture` il data frame): calcolare prima.
- **`dbscan::dbscan()`** legge una matrice come coordinate, non distanze: si passano coordinate in metri.
- **Date nelle tabelle DT** slittano di un giorno: convertite in testo in `tabella_cluster_dt()`.
- **Esempio vicino al confine del territorio**: GPS e arrotondamento spostano le letture di una decina di metri; un contenitore deve avere attorno, a circa 50 m, solo comuni serviti (`nel_territorio()` nello script). Dopo un cambio di confini, comuni o cantieri vanno rigenerati esempio, analisi di esempio e test.
- **Sfondo senza rete**: senza stile MapLibre non lancia `load`; il gestore di `error` riconosce l'errore dall'indirizzo dello stile (`stile_url`), non da una tessera.
- **Prove nel browser**: in MapLibre 5 i dati si leggono con `getSource(id).getData()` (promessa); `querySourceFeatures()` dà solo le tessere caricate; la mappa con `HTMLWidgets.find('#<id>').getMap()`; all'avvio si vedono solo bolle, i marker da zoom 14. Click veri (DevTools Protocol) sulle righe di DT e reactable.
- **`VALSTAGNA`** corretto solo nei contenitori: altrove è un comune fuori tabella (giro → coerenza ignota). Il riconoscimento nell'app è stato tolto: non rimetterlo.
- **Baricentro fuori dal confine** (`POVE DEL GRAPPA`, `TEOLO`): `baricentri_comuni()` ripiega sulla parte più grande o sul centro abitato; dopo un comune nuovo guardare la sua bolla.
- **Errori dei report**: `compila_report()` chiama `quarto_render()` senza `quiet`, altrimenti un errore del report si vede solo come «Error running quarto CLI» e il motivo (per esempio «Nessuna lettura con le antenne...») si perde.
- **Report Quarto** compilati in `docs/` (il `.qmd` cerca `../DESCRIPTION`): un percorso dei parametri parte dalla radice del progetto e il `.qmd` lo risolve con `percorso_progetto()`; `compila_report()` sposta l'HTML in `data-raw/output/`. Il codice inline `{r}` non fa HTML: una nota in HTML va in un blocco `results: asis`; due `{r}` attaccati vanno uniti in uno. Il pacchetto `quarto` non basta, serve il programma (in Positron c'è, non nel PATH del terminale).
- **Report di confronto su dati reali** (1,65 milioni di letture storiche): un `%in%` dentro un raggruppamento si rifà per ogni gruppo (`composizione_giri()` passò da 326 s a 2 s calcolandolo prima) e un `summarise()` per RFID su milioni di righe è lento: ridurre prima con `distinct()` o usare i numeri dei contenitori (`copertura_contenitori()`, 65 s → 1 s). Il report intero, su dati di quella scala, compila in circa 20 s: se risale, cercare il passaggio con `system.time()` sulle funzioni di `R/utils_confronto_analisi.R`.
- **Test dell'interfaccia**: il favicon sta in `htmltools::renderTags(app_ui(NULL))$head`; gli output `reactive()` non si leggono da `testServer()` (leggere il reactive interno, es. `con_geografia()`); i messaggi di `sendCustomMessage()` si catturano sostituendo `invia_mappa()` con `local_mocked_bindings()` (`registra_messaggi()`); click con `session$setInputs(mappa_clic = list(i = , nonce = ))`, riga dei giri con `tabella__reactable__selected`, tempo con `session$elapse()`.
- **Selectize senza scelte**: `input$comuni` è `NULL`; l'osservatore usa `ignoreNULL = FALSE`.
- **Bolle grandi da lontano** (fino a 72 px): il proprietario le vuole così; `lato_bolla()` non va legato allo zoom.
- **Numeri**: `format(1e5, big.mark = ".")` dà `"1e+05"`; usare `formatta_numero()`.
- **Servizio atteso, giro, stimato**: atteso (tipologia) e giro (calendario) si confrontano per tipologia, mai per uguaglianza; raggruppare i giri per `servizio_atteso` li spezza; nei giri dei servizi l'atteso può non essere il calendario. Il servizio di un non censito è lo stimato, dalle sue letture «Non Presente». `calcola_statistiche()` riceve una riga per RFID: `servizio_icona` va calcolato prima, su tutto il periodo.
- **Giri dei servizi nell'esempio**: ci passano solo tag anomali e non censiti; la regola dei censiti si prova con letture sintetiche. Un caso dimostrativo cambierebbe i conteggi: solo su richiesta.
- **Giri senza rifiuto**: `PULIZIA TERRIT.` (tipologia «Pulizia territorio») e i giri dei servizi rendono non coerente un censito (salvo il servizio più frequente nei giri dei servizi). `ASSISTENTE SERVIZI` non ha tipologia (scelta del proprietario): punto di domanda e avviso.
- **Rinominare una tipologia** cambia il servizio atteso nei CSV e i nomi nella composizione dei cluster.
- **Sacchetti** solo nell'esempio (12, voluti dal proprietario); i test sulle regole dei bidoni li escludono con `rfid_sacchetto()`.
- **Due comuni dichiarati, tre dal comune di lettura**: scegliere quello giusto; per dove sta un bidone il geografico, per confrontare i dichiarati. Cantiere di una lettura (comune di lettura) ≠ cantiere di un RFID (comune dichiarato).
- **Tolleranza e file**: un file senza `distanza_comune_dichiarato` (prima del 19/10/2026) confronta i soli nomi e lungo i confini dà molti fuori comune: rigenerarlo. Il `.duckdb` conserva la coerenza calcolata: dopo un cambio di `tolleranza_confine_m()` va riscritto (la pipeline lo fa da sé).
- **`comune_lettura` ha cambiato significato** a ottobre 2026 (prima era il comune del calendario).
- **Pipeline**: ogni colonna dei file estratti si legge come testo (`leggi_tabella_origine()`): codici della fornitura con gli zeri iniziali, coordinate con la virgola convertite da `importa_letture()`. `controllo_giri()` cerca le letture per mezzo, giorno e `id_giro`. Lettura e turni nello stesso fuso. Nel `.duckdb` della pipeline mancano le colonne dei conferimenti: l'analisi legge il CSV con `colonne = "tutte"`; un CSV prima del 18/10/2026 non ha la fornitura, un `.duckdb` con schema < 5 va rigenerato.
- **Conferimenti**: la finestra di un contenitore parte dal giorno dopo la prima lettura (contarla gonfia il tasso); con meno di `vicini_minimi` vicini le occasioni vengono dalle raccolte previste e il tasso esce basso (succede a tutto l'esempio).
- **Elenco di caselle con `label = NULL`**: Shiny sposta le voci in su; la regola CSS `.filtro-menu .shiny-input-checkboxgroup label ~ .shiny-options-group` corregge.
- **Menu e tendine**: Bootstrap chiude i menu a ogni click; un widget con tendina attaccata al `body` dentro un menu la chiuderebbe: per questo i comuni stanno fuori dai menu.
- **Tabella dei cluster**: dopo una nuova analisi `input$tabella_rows_all` riporta righe della tabella vecchia (da qui l'azzeramento); senza righe arriva `NULL`, come a tabella non disegnata, e per 50 ms a ogni ridisegno: da qui il `debounce()`.
- **Riga scelta nella tabella dei giri** = riga dei dati: `tabella_giri()` non riordina il riepilogo (ordine da `defaultSorted`).
- **Riquadri sopra la mappa**: classi proprie, non quelle della legenda, altrimenti si aprono e chiudono con lei.
- **Comuni con più poligoni**: `contorni_comuni_giro()` usa il numero dell'anello di `lati_comuni()`; i test lo provano con un poligono aggiunto.
- **JSON di un elemento**: Shiny scrive un vettore di lunghezza 1 come scalare; `mappa.js` passa ogni colonna da `elenco()`.
- **Nomi in `helper-dati.R`**: `letture_rfid()` dei test sarebbe nascosta da una funzione del pacchetto con lo stesso nome: per questo esiste `letture_del_rfid()`.
- **Tabelle della stima** restano finché l'archivio è aperto, una per periodo guardato (piccole).
- **`.duckdb` riscritto con l'app aperta**: l'app legge le letture di prima fino al riavvio; su sistemi che bloccano i file la scrittura può fallire.
- **Datepicker**: `dashboardHeader()` accetta solo `li` con classe `dropdown`; senza `.datepicker.dropdown-menu` con `!important` il calendario starebbe sotto la riga dei filtri (z-index 1050).
- **Test falliti** lasciano `tests/testthat/_problems/` e `testthat-problems.rds`: eliminarli.

## Documenti collegati

`README.md` (uso dell'app, formato del file, file grandi); `docs/analisi_cluster.md` (colonne, indicatori, indice di fiducia, scelte); `docs/indice_fiducia.qmd` (esempi e prove dell'indice); `docs/conferimenti_secco.qmd` (metodo, simulazione, risultati dei conferimenti); `docs/confronto_antenne.qmd` (confronto saponette-antenne con i filtri, spiegazione dei limiti e analisi A-H); `docs/controllo_giri_saponette.qmd` (un giorno di giri con entrambi i sistemi, lista dei giri come denominatore); `docs/associazione_giri.qmd` (come una lettura riceve il suo giro, con diagrammi) e `docs/comune_lettura.qmd` (come si ricava il comune di lettura): Quarto fuori dalla pipeline (`targets::tar_make()` non li compila), da ricompilare con `quarto::quarto_render("docs/associazione_giri.qmd")` quando la logica cambia; servono il programma `quarto` e i pacchetti `quarto`, `DiagrammeR` e `ggplot2` (Suggests); gli HTML compilati (autonomi) stanno in `docs/`. Il diagramma di DiagrammeR ha l'altezza di `fig-height` per 96 dpi: non alzare `fig-dpi` (lascia spazio bianco), la nitidezza viene da `fig.retina`.
