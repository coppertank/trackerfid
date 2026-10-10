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
- Una lettura esiste solo se un mezzo passa durante un **giro**. Dal calendario dei mezzi si ricavano il giro e il suo comune: per questo ogni lettura, anche di un bidone censito, ha il **servizio atteso** e il **comune atteso**, che nel file è `comune_lettura`.
- Per un non censito servizio, utenza e comune non sono noti. Valgono quelli dei giri che lo hanno letto: il servizio viene stimato dai giri, il comune è quello del giro.
- Oltre ai bidoni le antenne leggono i **sacchetti**. Non hanno una tipologia di rifiuto, ma si riconoscono dal codice RFID: 24 caratteri con il prefisso `00BD`, contro i 10 caratteri di un bidone. Un sacchetto è **monouso**: il mezzo lo legge quando lo raccoglie e, una volta svuotato, non può più essere letto. Ha quindi una lettura sola. Il proprietario del progetto vuole poterli vedere sulla mappa, ma li tiene fuori dall'analisi dei cluster e dal report sul beneficio delle antenne, e il più delle volte li toglierà dal file delle letture.
- L'obiettivo finale dell'analisi è **censire a mano i bidoni che oggi non sono a database**. Gli operatori cercano: bidoni non censiti, cambi di servizio o di utenza, bidoni spostati, tag anomali (smarriti, fermi in deposito, rimasti su un mezzo).
- Il territorio servito è diviso in quattro **cantieri**: `ASIAGO`, `BASSANO`, `CAMPOSAMPIERO`, `RUBANO`. Ogni comune appartiene a un cantiere. Gli operatori filtrano le letture per cantiere e per comune.
- Le antenne sui mezzi sono recenti. Prima un altro sistema registrava solo RFID e istante della lettura, senza mezzo e senza posizione: sono le **letture storiche**. L'azienda vuole misurare quanto le antenne leggono in più, anche per la fatturazione.

## Glossario

| Termine | Significato | Nel codice |
|---|---|---|
| Lettura | Una riga del CSV: un RFID letto da un mezzo in un istante e in un punto | righe del dataset |
| Censito, non censito | RFID presente o assente nel database aziendale | `presente_a_database`: `"Presente"` o `"Non Presente"` |
| Servizio transponder | Tipo di rifiuto secondo il database. Sei valori: `SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE` | `servizio_transponder`, vuoto per i non censiti |
| Servizio atteso, giro | Nome del giro che il mezzo stava facendo al momento della lettura. Dodici valori, ad esempio `SECCO PAP`. PAP significa porta a porta. C'è su ogni lettura: l'app lo usa per i soli non censiti | `servizio_atteso` |
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
| Comune di lettura, comune atteso | Comune del giro che il mezzo stava facendo al momento della lettura. C'è su ogni lettura | `comune_lettura` |
| Comune assegnato | Comune del database, se c'è, altrimenti quello di lettura. È il comune usato da filtri e report | `comune_assegnato`, calcolato da `aggiungi_geografia()` |
| Comune di un RFID | Ultimo comune indicato dal database, oppure, per un non censito, quello in cui è stato letto più spesso | `comune` in `anagrafica_rfid()` e nella tabella dei cluster, `geografia_per_gruppo()` |
| Vista aggregata, bolla | Sulla mappa principale, oltre il limite dei marker: un gruppo di bidoni per cantiere, comune o riquadro, calcolato dal server | `scegli_vista()`, `aggrega_marker()`, `R/utils_mappa_aggregata.R` |
| Letture storiche, sistema precedente | Letture fatte prima dell'installazione delle antenne: solo RFID e istante | secondo file CSV, `fonte == "storico"`, `R/utils_storico.R` |
| Fonte | Sistema che ha fatto una lettura | `fonte`: `"storico"` o `"antenne"`, aggiunta da `unisci_letture()` |
| Anno misto, anno parziale | Anno coperto da tutti e due i sistemi. Anno coperto per meno del 95% dei giorni, riportato a dodici mesi | `sistema`, `completo`, `fattore` in `copertura_annuale()` |
| Anni di servizio | Anni interi del sistema precedente successivi all'anno della prima lettura di un RFID: lì il contenitore era di sicuro in servizio | `anni_servizio` in `confronto_sistemi()` |
| Anno vuoto | Anno di servizio senza nemmeno una lettura | `anni_vuoti` |
| Letture utili | Letture di un contenitore in un anno, mai più delle raccolte previste | `letture_utili_storico`, `letture_utili_antenne` |
| Tasso di lettura | Letture utili divise per le raccolte previste | `tasso_storico`, `tasso_antenne` |

**Attenzione: la parola "cluster" ha due significati che non c'entrano l'uno con l'altro.** Sulla mappa principale è un effetto grafico. Nell'analisi è il risultato di DBSCAN. Le bolle della vista aggregata hanno l'aspetto dei cluster di marker ma sono calcolate dal server.

## Funzionalità

L'interfaccia è una dashboard. La barra laterale contiene, dall'alto: caricamento dei due file (letture con le antenne e, facoltative, letture storiche), periodo di analisi, dettaglio del bidone selezionato, controlli della scheda aperta. Il corpo ha quattro schede, in un `tabBox` con id `scheda_attiva`.

| Scheda | Valore di `scheda_attiva` | Cosa mostra | Controlli nella barra laterale |
|---|---|---|---|
| Mappa Principale | `mappa` | Un marker per RFID, nella posizione dell'ultima lettura del periodo. Marker raggruppati in cluster colorati secondo la quota di censiti. Oltre 10.000 bidoni, vista aggregata | Filtri per cantiere, comune, stato a database, servizio transponder, servizio atteso. Statistiche |
| Ricerca RFID | `rfid` | Tutte le letture degli RFID cercati. Ultima lettura in evidenza, riquadro attorno alle posizioni di ogni RFID | Campo di testo, pulsanti Cerca e Pulisci |
| Ricerca Utenza | `utenza` | Ultima lettura dei bidoni la cui utenza attuale è tra quelle cercate | Campo di testo, pulsanti Cerca e Pulisci |
| Analisi Cluster Spaziale | `cluster` | Tabella, grafici e mappa dei cluster spaziali di ogni RFID, con indicatore e indice di fiducia. Esportazione in CSV | Parametri DBSCAN, pulsante Genera Analisi |

**Le schede sono quattro, per scelta del proprietario del progetto.** Le analisi per territorio, cantiere e comune stanno nel report `docs/beneficio_antenne.Rmd`: non aggiungere all'app schede di analisi, di report o di prestazioni.

Altri comportamenti:

- **Filtri per cantiere e comune.** I cantieri si spuntano. I comuni si scelgono da un elenco a scelta multipla, con i comuni dei cantieri spuntati divisi per cantiere: se ne possono scegliere più di uno, anche di cantieri diversi. Nessun comune scelto vale per tutti. Vale il comune assegnato alla lettura. Cambiando cantieri o comuni la mappa si sposta sull'area scelta. I due filtri compaiono solo se almeno una lettura ha un cantiere.
- **Filtri dei servizi.** Lo stato a database toglie o rimette tutti i bidoni di uno stato. I servizi stanno in due elenchi, uno per stato. "Filtro Servizio Transponder" riguarda i bidoni "Presente": servizio del database. "Filtro Servizio Atteso" riguarda i bidoni "Non Presente": servizio stimato dai giri, lo stesso dell'icona. La casella "Includi Non Censiti con stima incerta (?)" riguarda i non censiti con il punto di domanda. Accanto a ogni titolo, "Tutti" e "Nessuno" spuntano o tolgono tutte le voci: "Nessuno" sul servizio atteso toglie anche la stima incerta. Non c'è una casella "Includi Non Censiti": il proprietario del progetto l'ha fatta togliere perché doppione dello stato "Non Presente".
- **Elenchi a discesa.** L'elenco dei comuni e quello dell'anno di analisi hanno lo stesso aspetto, per richiesta del proprietario del progetto: stesso riquadro, stesso testo, stessa freccia.
- **Vista aggregata.** Con più bidoni del limite la mappa principale mostra una bolla per cantiere fino a zoom 10, per comune a zoom 11 e 12, per riquadro da zoom 13, e i singoli bidoni dell'area inquadrata quando tornano sotto il limite. Un click su una bolla ingrandisce sui suoi bidoni.
- **Marker.** Verde a goccia se censito, rosso squadrato se non censito. L'icona interna indica il servizio. Per un non censito è il servizio stimato: il giro prevalente, se una tipologia raggiunge l'80% delle letture, altrimenti un punto di domanda. Un sacchetto ha sempre l'icona dei sacchetti, censito o no.
- **Sacchetti.** Si vedono sulla mappa principale e nelle due ricerche, con la loro icona, la loro voce nei filtri e nelle statistiche e un dettaglio loro. Restano fuori dall'analisi dei cluster spaziali: la scheda riguarda i soli contenitori.
- **Dettaglio del bidone.** Un click su un marker, su una riga della tabella dei cluster o su un baricentro apre nella barra laterale la storia dell'RFID: servizio coerente, cambio di servizio, stima per i non censiti, cambio di utenza. In fondo c'è l'istogramma delle letture per anno: usa tutte le letture caricate, non solo quelle del periodo, e distingue le letture storiche da quelle delle antenne. Un anno senza letture resta vuoto, con lo zero in rosso.
- **Popup.** Riporta i dati della lettura, il comune del database, il comune di lettura se è diverso, e il cantiere, se il file ha le colonne.
- **Titolo.** Riporta il periodo, ad esempio "DASHBOARD RFID - ANNO 2025".
- **Favicon.** È il simbolo aziendale. Nell'intestazione non c'è nessun logo, per scelta del proprietario del progetto.

## Dati

Il file delle letture con le antenne ha dieci colonne obbligatorie e cinque facoltative. L'ordine e le maiuscole dei nomi non contano. I formati accettati sono CSV, CSV compresso con gzip e Parquet: li riconosce `leggi_tabella()` dall'estensione.

| Colonna | Tipo dopo la validazione | Note |
|---|---|---|
| `giorno_lettura` | POSIXct, fuso UTC | Formato `YYYY-MM-DD HH:MM:SS`. Accettate anche date italiane |
| `targa_veicolo`, `matricola_veicolo` | testo | Relazione 1:1 |
| `RFID` | testo | Si ripete a ogni lettura |
| `presente_a_database` | testo | Solo `Presente` o `Non Presente` |
| `servizio_transponder` | testo maiuscolo | Vuoto per i non censiti. Per un sacchetto censito, che a database non ha servizio, la validazione scrive `SACCHETTI` |
| `servizio_atteso` | testo maiuscolo | Giro del mezzo al momento della lettura. Compilato per ogni lettura, anche dei censiti |
| `id_utenza` | testo | Vuoto per i non censiti |
| `latitudine`, `longitudine` | numerico | Accettata la virgola decimale |
| `volume_previsto` | numerico, facoltativa | Litri. Usata dall'analisi dei cluster |
| `numero_raccolte_annue_previste` | numerico, facoltativa | Usata dall'analisi dei cluster e dal confronto con le letture storiche |
| `comune_da_database` | testo, facoltativa | Vuoto per i non censiti. La colonna `comune` dei file meno recenti vale come questa |
| `comune_lettura` | testo, facoltativa | Comune del giro del mezzo: il comune atteso. Compilato per ogni lettura |
| `cantiere` | testo maiuscolo, facoltativa | Ricalcolato dal comune assegnato secondo la tabella dei comuni. Quello del file vale solo per i comuni fuori tabella |
| `comune_assegnato` | testo | Non sta nel file: lo aggiunge la validazione |

La validazione rifiuta il file se mancano colonne o se ci sono valori non interpretabili. Scarta, con un avviso, le righe prive di un campo essenziale. Riporta i nomi dei comuni alla grafia della tabella dei comuni, senza badare a maiuscole, accenti e apostrofi, e segnala con un avviso i comuni che la tabella non contiene.

La geografia sta in due file del pacchetto.

- `inst/extdata/comuni_cantieri.csv`: i 57 comuni serviti, con cantiere e coordinate del centro abitato. È compilato a mano ed è l'unico elenco di comuni e cantieri: 7 comuni per `ASIAGO`, 15 per `BASSANO`, 23 per `CAMPOSAMPIERO`, 12 per `RUBANO`. I nomi sono in maiuscolo, con l'apostrofo al posto dell'accento, come nel file dell'azienda: `ROSA'`.
- `inst/extdata/comuni_confini.geojson`: i confini degli stessi comuni, uno per ogni riga della tabella e nello stesso ordine. Sono quelli dell'ISTAT, dall'ultimo rilascio della raccolta `guglielmo/geojson-italy`, licenza CC-BY 4.0: ora il rilascio del 14 maggio 2026. Li scarica `scarica_confini_comuni()`, richiamata dallo script `data-raw/prepara_comuni.R`. Ogni comune sta su una riga del file e porta `comune` e `cantiere`, come nella tabella, e `nome_istat`, `codice_istat`, `codice_catastale`, `provincia`, come nella fonte. Servono alla mappa dei comuni del report, alla vista iniziale delle mappe dell'app e al dataset di esempio.

I nomi della tabella e quelli della fonte si associano con `chiave_comune()`: `ROSA'` trova `Rosà`. Tra due comuni italiani con lo stesso nome vale quello che contiene il centro abitato della tabella. Se un comune non trova il confine, o il suo centro cade fuori dal confine trovato, `scarica_confini_comuni()` si ferma senza scrivere il file.

Il CSV delle letture storiche è un secondo file, facoltativo, con due colonne: `RFID` e `giorno_lettura`. Lo valida `valida_storico()`: una data non interpretabile ferma il caricamento, le righe incomplete e quelle ripetute vengono scartate con un avviso. I codici RFID devono avere lo stesso formato dell'altro file.

I dataset di esempio sono due, simulati dallo script `data-raw/genera_sample_dataset.R` con un seme fisso.

- `inst/extdata/sample_rfid_dataset.csv`: 6.475 letture con le antenne di 251 RFID, cioè 239 bidoni e 12 sacchetti, da ottobre 2024 a dicembre 2025, cinque mezzi, 50 utenze dei bidoni. I bidoni stanno nel territorio servito, dentro i confini dei loro comuni: 54 comuni nei quattro cantieri. Come nei dati reali, ogni lettura ha servizio atteso, comune di lettura e cantiere. I bidoni non censiti sono 37: nel 2025, 18 hanno il servizio stimato e 19 la stima incerta. I sacchetti hanno una lettura ciascuno, perché sono monouso: 8 sono censiti, con le utenze da `SAC001` a `SAC004`, e 4 non censiti. A database hanno solo l'utenza, come escono dal passo 3: niente servizio, volume, raccolte o comune. Li leggono i giri `SECCO PAP`. Due sono letti nel 2024, dieci nel 2025.
- `inst/extdata/sample_letture_storiche.csv`: 7.025 letture storiche di 227 RFID, da gennaio 2020 a settembre 2024. 183 RFID sono letti da entrambi i sistemi, 68 solo dalle antenne, cioè 56 bidoni e i 12 sacchetti, 44 solo dal sistema precedente. Il sistema precedente registra una parte dei passaggi, diversa per cantiere, per comune e per anno, e lascia anni interi senza letture.

Lo script genera prima le letture dei bidoni, poi la geografia e i sacchetti, con semi a parte, poi le letture storiche: una modifica alla geografia, ai sacchetti o alla parte storica non cambia il resto. Giro, comune di lettura e cantiere di ogni lettura sono ricavati senza numeri casuali, con `giro_del_mezzo()`: il turno in cui cade la lettura o, fuori orario, il più vicino del mezzo in quel giorno, come fa sui dati reali `associa_servizio_atteso_da_calendario()`. Test e report fanno riferimento a questi RFID:

| RFID | Caso |
|---|---|
| `RFD20250901001` | Servizio cambiato: CARTA, SECCO, di nuovo CARTA |
| `RFD20250901050` | Utenza cambiata da `UTZ001` a `UTZ025` |
| `RFD20250915201` | Non censito, quattro letture, stima 100% `SECCO PAP`. Indicatore `GHOST_TAG` |
| `RFD20250920250` | Non censito con stima incerta: due letture, `SECCO PAP` e `CARTA/CARTONE PAP` |
| `RFD20250905100` | Spostato di 20 km: censito a `LIMENA`, cantiere `RUBANO`, poi letto a `CITTADELLA`. Indicatore `RELOCATED_BIN` |
| `RFD20241001301` | Tag fermo in deposito. Indicatore `DEPOT_STUCK`. Non censito, letto dai giri di tutti i mezzi: stima incerta |
| `RFD20250203302` | Tag rimasto sul mezzo. Indicatore `TRUCK_STOWAWAY` |
| `RFD20241003303` | GPS impreciso, 17 cluster. Indicatore `SCATTERED_READS / GPS_NOISE` |
| `RFD20241004003` | Letto dalle antenne con regolarità, senza letture storiche nel 2021 e nel 2023. Comune e cantiere `RUBANO`. Letture per anno dal 2020: 16, 0, 6, 0, 13, 25 |
| `00BD00000000000000000005` | Sacchetto censito, utenza `SAC002`: una lettura a `CAMPOSAMPIERO` nel 2025. I censiti vanno da `…0001` a `…0008` |
| `00BD00000000000000000102` | Sacchetto non censito: una lettura a `CITTADELLA` nel 2025. I non censiti vanno da `…0101` a `…0104` |

Numeri del confronto tra i due sistemi sul dataset di esempio, usati dai test: 162 RFID nel confronto, 415 anni di servizio di cui 90 vuoti, tasso di lettura dal 31% all'82%. Per cantiere, tasso precedente e con le antenne: `ASIAGO` 19% e 84%, `BASSANO` 46% e 77%, `CAMPOSAMPIERO` 30% e 83%, `RUBANO` 21% e 85%.

### Dati reali

I dati veri dell'azienda non sono nel repository. Gli script in `data-raw/` li preparano, in ordine, e producono i CSV che l'app carica.

| Passo | Script | Legge | Scrive |
|---|---|---|---|
| 1 | `data-raw/01_estrai_tabelle.R` | database aziendale, via ODBC | un CSV per tabella in `data-raw/origine/` |
| 2 | `data-raw/02_importa_tabelle.R` | `data-raw/intermedi/*.rds`. Il codice commentato li ricostruisce da `data-raw/origine/` | tabelle ripulite nell'ambiente di lavoro |
| 3 | `data-raw/03_crea_letture_app.R` | le tabelle del passo 2 | `data-raw/output/letture_app.csv` |
| 4, facoltativo | `data-raw/04_crea_letture_storiche.R` | `data-raw/origine/letture_storiche.csv`, le letture precedenti alle antenne | `data-raw/output/letture_storiche.csv` |

Le regole di trasformazione sono funzioni del pacchetto, in `R/fct_preparazione_letture.R`. Il passo 3 le applica in questo ordine: `filtra_letture_post_test()`, `formatta_rfid()`, `associa_servizio()`, `associa_servizio_atteso_da_calendario()`. L'ultima restituisce le colonne del CSV dell'app. `comune_da_database` viene da `comune_servizio` dell'anagrafica dei contenitori. `comune_lettura` viene dalla colonna del calendario indicata con l'argomento `colonna_comune`: lo script usa `"comune"`, e il proprietario del progetto lo ha lasciato così. Se nei dati veri la colonna ha un altro nome basta cambiare l'argomento. `cantiere` viene dalla tabella dei comuni. Ogni colonna geografica manca se manca la sua origine. Il passo 3 compila `servizio_atteso` e `comune_lettura` per ogni lettura, anche dei censiti: restano vuoti solo se il mezzo non ha turni in quel giorno, cosa che secondo il proprietario del progetto nei dati reali non accade.

**I passi 3 e 4 oggi scartano i codici che iniziano per `00BD`, cioè i sacchetti**: finché quella riga resta negli script, nel CSV dell'app non ce ne sono. Toglierla è una scelta del proprietario del progetto.

Il passo 4 non dipende dagli altri. In cima allo script si indicano il nome del file di origine e i nomi delle sue due colonne, che non sono noti a priori. Applica `formatta_rfid()`, così i codici hanno lo stesso formato del passo 3, e la stessa esclusione dei codici che iniziano per `00BD`.

Le cartelle `origine/`, `intermedi/` e `output/` sono escluse da git: contengono solo un file `.gitkeep`. **Non eseguire il passo 1 se non viene chiesto: si collega al database aziendale.** Su questo computer i dati reali possono mancare del tutto: le funzioni si provano con le tabelle sintetiche dei test.

## Architettura

### Flusso dei dati

```
file delle letture              file delle letture storiche (facoltativo)
dal browser o da run_app(letture = , storico = )
   │                               │
   ▼                               ▼
mod_caricamento ──► dati_caricati: tutte le letture validate, con comune_assegnato e cantiere
   │            └─► storico: letture storiche validate ──► solo al dettaglio del bidone
   │
   ├──► mod_periodo ──► dati: letture del periodo, con la colonna servizio_icona
   │        │
   │        ├──► mod_filtri ──► dati_filtrati: una riga per RFID ──► mod_mappa "mappa_principale"
   │        │              └─► area: coordinate del cantiere e del comune scelti ──► inquadratura della mappa
   │        ├──► mod_ricerca "ricerca_rfid" ──► risultati ──► mod_mappa "mappa_rfid"
   │        ├──► mod_ricerca "ricerca_utenza" ──► risultati ──► mod_mappa "mappa_utenza"
   │        └──► dettaglio del bidone: output$info_panel in app_server.
   │             L'istogramma per anno usa invece dati_caricati e storico
   │
   └──► mod_cluster_analysis: riceve dati_caricati e il periodo ──► tabella, grafici, mappa, CSV
```

`R/app_server.R` contiene solo questi collegamenti, il titolo e il dettaglio del bidone.

Le funzioni di `R/fct_preparazione_letture.R` stanno fuori da questo flusso: l'app non le richiama mai. Servono a produrre il file CSV in ingresso.

Di `R/utils_storico.R` l'app usa solo la lettura del file e `andamento_rfid()`. Le funzioni del confronto tra i due sistemi servono al report `docs/beneficio_antenne.Rmd`, che le applica a tre livelli: intero territorio, cantiere, comune.

### Moduli

Ogni modulo ha due file: `R/mod_<nome>_ui.R` e `R/mod_<nome>_server.R`.

| Modulo e id usati | Riceve | Restituisce | Input principali |
|---|---|---|---|
| `mod_caricamento` (`caricamento`) | `demo`, `percorsi`: di norma le opzioni di `run_app()` | lista di reactive: `letture` e `storico`, ciascuno con il dataset validato o `NULL` | `file_upload`, `file_storico`, `carica_esempio` |
| `mod_periodo` (`periodo`) | `dati` | lista di reactive: `periodo`, `etichetta`, `dati` | `anno_filtro`, `personalizzato`, `intervallo` |
| `mod_filtri` (`filtri`) | `dati`, `azzera` | lista di reactive: `dati_filtrati` e `area` | `cantiere_check`, `comuni`, `presente_filter`, `servizio_transponder_check`, `transponder_tutti`, `transponder_nessuno`, `servizio_atteso_check`, `atteso_tutti`, `atteso_nessuno`, `includi_stima_incerta`, `reset` |
| `mod_ricerca` (`ricerca_rfid`, `ricerca_utenza`) | `dati`, `tipo` | lista di reactive: `risultati`, `codici` | `testo`, `cerca`, `pulisci` |
| `mod_mappa` (`mappa_principale`, `mappa_rfid`, `mappa_utenza`) | `dati`, `modalita`, `dati_vista`, `attiva`, `messaggio` | reactive con il marker cliccato: lista con `rfid` e `quando` | `mappa_marker_click`, `mappa_zoom`, `mappa_bounds` |
| `mod_cluster_analysis` (`cluster`) | `dati`, `periodo`, `etichetta` | reactive con il cluster scelto: lista con `rfid` e `quando` | `eps_m`, `min_pts`, `genera`, `tabella_rows_all`, `tabella_rows_selected`, `mappa_marker_click` |

`mod_caricamento` tiene i due file separati: un file storico non valido non tocca le letture con le antenne. `carica_esempio` e `run_app(demo = TRUE)` caricano entrambi i dataset di esempio. Caricare a mano un file di letture toglie le letture storiche di esempio, non quelle caricate a mano. I file indicati in `run_app(letture = , storico = )` sono letti da `leggi_una_volta()`: una volta sola per processo, condivisi tra le sessioni.

`mod_mappa` ha tre modalità: `"cluster"`, `"rfid"`, `"utenza"`. La modalità decide come `disegna_marker()` disegna i dati. In modalità `"cluster"`, con più bidoni del limite, il modulo segue `mappa_bounds` e `mappa_zoom` e ridisegna la vista scelta da `scegli_vista()`. Lo stato sta in tre `reactiveVal` interni: `disegnati`, `bolle`, `vista`. In modalità `"rfid"` disegna al massimo le letture di `limita_letture_ricerca()`. `mod_cluster_analysis` ha due funzioni di interfaccia: `mod_cluster_analysis_ui()` per il corpo e `mod_cluster_analysis_sidebar_ui()` per la barra laterale. Grafici e mappa seguono le righe mostrate dalla tabella, tenute nel `reactiveVal` interno `righe_tabella`: `NULL` vuol dire tutte, un vettore vuoto nessuna. Una nuova analisi lo azzera. Le righe comunicate dal browser valgono solo dopo l'attesa di `attesa_righe_tabella()`.

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
12. **Cantieri filtrati per esclusione, comuni per scelta.** I cantieri seguono la regola dei servizi. I comuni scelti stanno in un vettore: vuoto vuol dire tutti. I comuni di un cantiere deselezionato escono dalla scelta, gli altri restano.
13. **Vista aggregata calcolata dal server.** Sopra il limite di `limiti_mappa()` il browser riceve bolle, non marker: il messaggio resta sotto il megabyte. Dopo uno spostamento o uno zoom la mappa si ridisegna solo se `vista_da_rifare()` lo dice, con un'attesa di 250 millisecondi. L'area caricata è più larga del 30% di quella inquadrata, così i piccoli spostamenti non ridisegnano niente.
14. **Calcoli per gruppo senza cicli.** Con centinaia di migliaia di RFID un `summarise()` valutato gruppo per gruppo costa decine di secondi. L'analisi dei cluster, l'anagrafica e il confronto tra i sistemi usano le funzioni di `R/utils_gruppi.R`, che lavorano su vettori interi.
15. **Valori ripetuti trattati una volta.** I campi di testo hanno pochi valori ripetuti milioni di volte: maiuscole, pulizia e ricerca del cantiere passano da `per_valori_distinti()`.
16. **Una sola tabella dei comuni.** Comuni e cantieri stanno solo in `inst/extdata/comuni_cantieri.csv`. Il confronto dei nomi passa sempre da `chiave_comune()`.
17. **Elenchi a discesa con lo stesso aspetto.** L'elenco dell'anno è un elenco del browser, quello dei comuni è un selectize a scelta multipla, dentro un `div` con classe `elenco-comuni`. La sezione "Elenchi a discesa della barra laterale" del CSS dà a tutti e due lo stesso riquadro e la stessa freccia, disegnata nel CSS perché quella del browser cambia da un sistema all'altro. Un elenco nuovo nella barra laterale deve usare gli stessi stili.
18. **Un elenco di servizi per stato a database.** I servizi transponder filtrano le sole letture "Presente". I servizi attesi filtrano le sole letture "Non Presente", e per il servizio stimato dell'RFID, non per il giro della singola lettura: un non censito sta sotto una voce sola, il suo servizio stimato oppure la stima incerta. Così voci dei filtri, icone dei marker e statistiche coincidono. Nessuna regola dell'app deve leggere `servizio_atteso` senza guardare lo stato a database: nei dati reali la colonna è compilata anche per i censiti.
19. **Sacchetti riconosciuti dal codice.** `rfid_sacchetto()` è l'unica regola. La validazione dà ai sacchetti censiti senza servizio il servizio `SACCHETTI`; `aggiungi_servizio_icona()` lo dà anche ai non censiti, al posto della stima dai giri, perché un sacchetto non ha una tipologia di rifiuto da stimare. Da lì in poi è un servizio come gli altri: voce nei filtri del suo stato, riga nelle statistiche, icona, legenda. Fanno eccezione l'analisi dei cluster e il report sul beneficio delle antenne, che riguardano i contenitori: `calcola_analisi_cluster()` e il blocco `dati` del report tolgono i sacchetti con `senza_sacchetti()`, prima di ogni conto. Il risultato deve essere quello di un file che non li contiene.

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
│   ├── mod_mappa_ui.R / _server.R              mappa Leaflet, tre modalità
│   ├── mod_cluster_analysis_ui.R / _server.R   scheda di analisi dei cluster spaziali
│   ├── utils_data_processing.R   lettura, validazione, periodo, filtri, ricerche, storia di un RFID
│   ├── utils_styling.R           tipologie di servizio, colori, marker SVG, cluster di marker, legenda
│   ├── utils_mapping.R           mappa di base e disegno dei marker
│   ├── utils_popup.R             popup, pannello di dettaglio, riquadro statistiche
│   ├── utils_dbscan_clustering.R DBSCAN, distanze, dispersione
│   ├── utils_cluster_output.R    tabella di analisi, soglie, indicatori, indice di fiducia
│   ├── utils_cluster_viste.R     tabella interattiva, grafici e mappa dei cluster spaziali
│   ├── utils_storico.R           letture storiche: lettura, conteggi per anno, confronto con le antenne
│   ├── utils_geografia.R         comuni, cantieri e confini del territorio servito
│   ├── utils_gruppi.R            calcoli per gruppo su vettori, senza cicli
│   ├── utils_mappa_aggregata.R   limiti delle mappe e vista aggregata della mappa principale
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
│   └── beneficio_antenne.Rmd     report sul beneficio delle antenne, con parametri per i dati reali
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
| Schema del dataset | `colonne_obbligatorie()`, `colonne_facoltative()`, `colonne_quantita()` per le facoltative numeriche, `colonne_geografiche()`, `colonne_derivate()`, `colonne_dataset()`, `stati_database()`, `percorso_dataset_esempio()` |
| Sacchetti | `rfid_sacchetto()` li riconosce dal codice, `servizio_sacchetti()` dà il nome del loro servizio, `senza_sacchetti()` li toglie dalle letture |
| Lettura e validazione | `carica_dataset()` è il punto di ingresso: chiama `leggi_tabella()` e `validate_dataset()`. `leggi_tabella()` sceglie il formato tra quelli di `formati_file()`: `read_csv_auto()`, `decomprimi_gz()`, Parquet. `leggi_una_volta()` tiene in memoria i file letti da percorso. Ausiliarie: `rileva_separatore()`, `converti_data_ora()`, `converti_numero()`, `pulisci_testo()`, `celle_da_pulire()`, `elenco_righe()`, `errore_validazione()` |
| Periodo | `anni_disponibili()`, `periodo_anno()`, `etichetta_periodo()`, `filtra_periodo()` |
| Servizio stimato dei non censiti | `servizio_prevalente_non_censiti()` applica la regola dell'80%, `aggiungi_servizio_icona()` scrive la colonna `servizio_icona`, `servizio_mostrato()` la legge o, se manca, la calcola |
| Filtri e statistiche | `maschera_filtri()` dice quali letture superano i filtri, `deduplica_ultimo_rfid()` tiene l'ultima per RFID tra quelle. `ordina_servizi()`, `conta_servizi()`, `ordina_cantieri()`, `conta_cantieri()`, `senza_cantiere()`, `calcola_statistiche()` |
| Ricerche | `analizza_input_ricerca()`, `codice_in()`, `cerca_per_rfid()`, `filtra_ultimo_per_utenza()`, `calcola_bbox()` |
| Storia di un RFID | `analizza_rfid()` decide il caso da mostrare nel dettaglio. Ausiliarie: `cronologia_transizioni()`, `stima_servizio_atteso()` |

### `R/utils_styling.R`

| Gruppo | Funzioni |
|---|---|
| Servizi | `tipologie_servizio()` è l'unico elenco di servizi, icone, colori ed emoji. Da lì derivano `servizi_config()`, `info_servizio()`, `tipologia_servizio()` |
| Colori | `colori_stato()`, `get_color()`, `genera_colore_cluster()` |
| Marker | `svg_marker()` disegna il marker, `get_leaflet_icon()` lo trasforma in icone Leaflet. Ausiliarie: `glifo_fa()`, `svg_glifo()`, `svg_icona_servizio()`, `uri_svg()` |
| Cluster di marker | `js_icona_cluster()` restituisce il JavaScript che colora i cluster |
| Bolle della vista aggregata | `svg_bolla()` disegna una bolla con lo stesso aspetto di un cluster, `icone_bolle()` la trasforma in icone Leaflet, `lato_bolla()` ne dà la dimensione |
| Legenda | `html_legenda()` |

### `R/utils_mapping.R`

`mappa_base()`, `opzioni_cluster()`, `pulisci_mappa()`, `aggiungi_marker()`, `aggiungi_bolle()`, `add_rfid_bounds()`, `disegna_marker()`, `adatta_vista()`. Funzionano sia su una mappa `leaflet()` sia su un `leafletProxy()`. `mappa_base()` inquadra la zona servita.

### `R/utils_popup.R`

| Gruppo | Funzioni |
|---|---|
| Formati | `formatta_data_ora()`, `formatta_numero()`, `conta()` per singolari e plurali, ad esempio "1 bidone" |
| Popup del marker | `create_popup_html()` |
| Dettaglio del bidone | `crea_info_panel()` sceglie tra `blocco_bidone()` e `blocco_sacchetto()`. Ausiliarie: `blocco_info()`, `elenco_cronologia()`, `barre_stima()`, `istogramma_annuale()` |
| Statistiche | `crea_box_statistiche()`, `righe_servizi()`, `righe_cantieri()` |

### `R/utils_dbscan_clustering.R`

`cluster_dbscan()` raggruppa le letture di un RFID. `assegna_cluster()` lo applica a tutti gli RFID e aggiunge `cluster_id`: salta DBSCAN per gli RFID letti in un solo punto. `ordina_cluster()` numera i cluster in ordine di prima lettura. `soglia_arrotondamento_cluster()` dà il numero di letture oltre il quale le coordinate sono arrotondate a 2 metri. Distanze: `raggio_terra_m()`, `proietta_metri()`, `distanza_haversine_m()`, scritta in R base. La dispersione di un cluster è calcolata in `calcola_analisi_cluster()` con `quantile_per_gruppo()`.

### `R/utils_cluster_output.R`

| Gruppo | Funzioni |
|---|---|
| Parametri | `soglie_indicatore()`, `pesi_fiducia()`, `indicatori_config()`, `colonne_analisi_cluster()` |
| Calcolo | `calcola_analisi_cluster()` produce la tabella con una riga per cluster. Usa `classifica_cluster()` e `indice_fiducia()` |
| Esportazione | `nome_file_analisi_cluster()`, `esporta_analisi_cluster()` |
| Ausiliarie | `analisi_cluster_vuota()`, `sequenza_valori()`, `tra_zero_e_uno()` |

### `R/utils_cluster_viste.R`

`tabella_cluster_dt()`, `grafico_indicatori()`, `grafico_dispersione()`, `mappa_cluster()`, `popup_cluster()`. Con troppi cluster grafico a dispersione e mappa disegnano un campione: `limiti_viste_cluster()`, `campiona_cluster()`, `nota_campione()`. `attesa_righe_tabella()` dà l'attesa prima che grafici e mappa seguano le righe della tabella. Ausiliarie: `lingua_dt()`, `inchiostro_grafici()`.

### `R/utils_storico.R`

| Gruppo | Funzioni |
|---|---|
| Lettura e validazione | `carica_storico()` chiama `read_csv_auto()` e `valida_storico()`. `colonne_storico()`, `percorso_storico_esempio()` |
| Letture per anno | `unisci_letture()` mette insieme i due sistemi e aggiunge `fonte`. Da lì: `conta_letture_annuali()`, `griglia_annuale()` con gli zeri dalla prima lettura in poi, `andamento_rfid()` per l'istogramma dell'app, `copertura_annuale()` |
| Tabella per RFID | `anagrafica_rfid()` dà cantiere, comune e raccolte previste di ogni RFID, `tabella_letture_annuali()` una colonna per anno |
| Confronto tra i sistemi | `confronto_sistemi()` produce una riga per RFID letto da entrambi. `riepilogo_confronto()` dà i totali e `media_annuale()` le letture medie per contenitore anno per anno: tutte e due con l'argomento `per`, per raggruppare per cantiere o per comune. Ausiliarie: `intervallo_media()`, `rfid_solo_storico()` |

### `R/utils_geografia.R`

| Gruppo | Funzioni |
|---|---|
| Tabelle | `comuni_cantieri()`, `cantieri()`, `poligoni_comuni()` con i vertici dei confini, `riquadro_zona()`. Lette una volta per sessione di R |
| Confini | `percorso_confini_comuni()`. `anelli_geometria()` e `anelli_geojson()` trasformano un GeoJSON in una tabella di vertici, con poligoni e fori. `dentro_poligoni()` dice se un punto è dentro un confine |
| Nomi dei comuni | `chiave_comune()` per il confronto, `normalizza_comune()` per la grafia della tabella, `cantiere_di_comune()` |
| Geografia delle letture | `aggiungi_geografia()` compila `comune_assegnato` e `cantiere` di ogni lettura. `geografia_per_gruppo()` dà comune e cantiere di ogni RFID |
| Ausiliaria | `per_valori_distinti()` applica una funzione ai soli valori distinti di un vettore |

### `R/utils_gruppi.R`

Calcoli per gruppo su vettori interi. Il gruppo è un intero da 1 al numero dei gruppi, dato da `indice_gruppi()`. `primo_per_gruppo()`, `ultimo_per_gruppo()`, `ultimo_valido_per_gruppo()`, `somma_per_gruppo()`, `media_per_gruppo()`, `estremi_per_gruppo()`, `quantile_per_gruppo()`, `n_distinti_per_gruppo()`, `piu_frequente_per_gruppo()`. I test li confrontano con il calcolo gruppo per gruppo.

### `R/utils_mappa_aggregata.R`

| Gruppo | Funzioni |
|---|---|
| Limiti | `limiti_mappa()`: bidoni disegnati uno per uno e letture disegnate dalla ricerca RFID. `limita_letture_ricerca()` |
| Scelta della vista | `scegli_vista()` restituisce `"tutti"`, `"singoli"`, `"cantiere"`, `"comune"` o `"griglia"`. `livello_aggregazione()`, `vista_da_rifare()`, `descrizione_vista()` |
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
| `associa_servizio()` | Associa servizio, utenza, volume e raccolte del contenitore attivo nel giorno della lettura. In mancanza cerca tra i sacchetti. Calcola `presente_a_database` |
| `associa_servizio_atteso_da_calendario()` | Sceglie per ogni lettura il turno del mezzo in cui cade, o il più vicino. Un turno che finisce dopo mezzanotte vale anche per il giorno successivo. Dal turno prende anche il comune di lettura. Restituisce le colonne del CSV dell'app. Ausiliaria: `completa_cantiere()` |
| `estrai_rfid()` | Ricava i codici RFID dal testo degli eventi. Non usata dagli script attuali |

### Altri file

| File | Funzioni |
|---|---|
| `R/app_ui.R` | `app_ui()`, `sezione_sidebar()` per i riquadri della barra laterale, `golem_add_external_resources()` |
| `R/app_server.R` | `app_server()` |
| `R/mod_ricerca_ui.R` | `mod_ricerca_ui()`, `testi_ricerca()` con le etichette delle due ricerche |
| `R/mod_caricamento_ui.R` | `mod_caricamento_ui()`, `estensioni_accettate()` per i campi di caricamento |
| `R/mod_filtri_ui.R` | `mod_filtri_ui()`, `intestazione_filtro()` per il titolo di un elenco di servizi con "Tutti" e "Nessuno" |
| `R/generate_cluster_analysis.R` | `genera_analisi_cluster()`, `scrivi_analisi_cluster()` |

## Dove intervenire

| Per cambiare | Vai in | Test collegati |
|---|---|---|
| Servizi e giri riconosciuti, icone, colori, emoji | `tipologie_servizio()` in `R/utils_styling.R`. È l'unico punto | `test-utils_styling.R` |
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
| Comuni serviti, cantiere di un comune | `inst/extdata/comuni_cantieri.csv`. Per un comune nuovo: una riga con cantiere e coordinate del centro abitato, poi `Rscript data-raw/prepara_comuni.R` per i confini | `test-utils_geografia.R`, `test-sample_dataset.R` |
| Confini dei comuni: aggiornarli, cambiare la fonte o le regole con cui i nomi si associano | `scarica_confini_comuni()`, `origine_confini()`, `seleziona_confini_comuni()` in `R/fct_confini_comuni.R` | `test-fct_confini_comuni.R`, `test-utils_geografia.R` |
| Regola del comune assegnato e del cantiere di una lettura | `aggiungi_geografia()` | `test-utils_geografia.R` |
| Regola del comune e del cantiere di un RFID | `geografia_per_gruppo()` | `test-utils_geografia.R`, `test-utils_storico.R` |
| Filtri per cantiere e comune | interfaccia in `R/mod_filtri_ui.R`, stato e scelte in `R/mod_filtri_server.R`, regola in `maschera_filtri()` | `test-moduli.R`, `test-utils_data_processing.R` |
| Aspetto degli elenchi a discesa della barra laterale: riquadro, freccia, comuni scelti | sezione "Elenchi a discesa della barra laterale" di `inst/app/www/custom_style.css` | nessuno: guardarli nel browser |
| Numero di bidoni oltre il quale la mappa passa alle bolle, letture disegnate dalla ricerca RFID | `limiti_mappa()`, oppure le opzioni `trackerfid.max_marker` e `trackerfid.max_letture_ricerca` | `test-utils_mappa_aggregata.R` |
| Zoom a cui le bolle passano da cantiere a comune a riquadro, dimensione dei riquadri | `livello_aggregazione()`, `passo_griglia()` | `test-utils_mappa_aggregata.R` |
| Aspetto delle bolle | `svg_bolla()`, `lato_bolla()`, `aggiungi_bolle()`, classe `etichetta-bolla` nel CSS | `test-utils_styling.R`, `test-utils_mapping.R` |
| Attesa prima di ridisegnare dopo uno spostamento | `debounce()` in `R/mod_mappa_server.R` | `test-moduli.R` |
| Formati di file accettati | `formati_file()`, `leggi_tabella()` | `test-utils_data_processing.R` |
| Cluster disegnati al massimo da grafico e mappa dell'analisi | `limiti_viste_cluster()` | `test-utils_cluster_viste.R` |
| Attesa prima che grafici e mappa seguano i filtri della tabella dei cluster | `attesa_righe_tabella()`, usata in `R/mod_cluster_analysis_server.R` | `test-utils_cluster_viste.R`, `test-moduli.R` |
| Ridisegno delle mappe quando una scheda torna visibile | secondo gestore di `inst/app/www/script.js` | nessuno: provarlo nel browser |
| Istogramma delle letture per anno nel dettaglio | `andamento_rfid()` per i conteggi, `istogramma_annuale()` per l'HTML, classi `anni-*` nel CSS | `test-utils_storico.R`, `test-utils_popup.R` |
| Colonne o regole del file delle letture storiche | `colonne_storico()`, `valida_storico()` | `test-utils_storico.R` |
| Regole del confronto tra sistema precedente e antenne: anni di servizio, letture utili, tasso | `confronto_sistemi()`, `media_annuale()`, `riepilogo_confronto()` | `test-utils_storico.R` |
| Soglia del 95% per un anno completo | argomento `quota_completo` di `copertura_annuale()` | `test-utils_storico.R` |
| Testi e grafici del report sul beneficio delle antenne | `docs/beneficio_antenne.Rmd` | nessuno: compilarlo |
| Livelli del report: territorio, cantiere, comune | blocco `dati` del report: `per_cantiere`, `comuni`, `consistenza()`. Le regole restano in `riepilogo_confronto()` e `media_annuale()` | `test-utils_storico.R` per le regole, poi compilarlo |
| Soglia sotto cui il tasso di un comune è indicativo | `minimo_contenitori` nel blocco `dati` del report | nessuno: compilarlo |
| Righe massime della tabella per RFID del report | `massimo_righe_tabella` nel blocco `dati` del report | nessuno: compilarlo |
| Casi e testi del dettaglio del bidone | `analizza_rfid()` per la logica, `blocco_bidone()` per i testi | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Riquadro delle statistiche | `calcola_statistiche()` e `crea_box_statistiche()` | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Colonne del CSV, formati di data, regole di validazione | `colonne_obbligatorie()`, `colonne_facoltative()`, `converti_data_ora()`, `validate_dataset()` | `test-utils_data_processing.R` |
| Dimensione massima del file caricato | opzione `trackerfid.max_upload_mb`, letta in `R/app_server.R`. Predefinito 2 GB | nessuno |
| Un filtro laterale nuovo o modificato | interfaccia in `R/mod_filtri_ui.R`, stato in `R/mod_filtri_server.R`, regola in `maschera_filtri()` | `test-moduli.R`, `test-utils_data_processing.R` |
| Selezione del periodo, preimpostazioni | `R/mod_periodo_ui.R`, `R/mod_periodo_server.R`, `periodo_anno()`, `etichetta_periodo()` | `test-moduli.R` |
| Titolo in alto | `output$titolo` in `R/app_server.R` | nessuno |
| Favicon | `favicon(ext = "png")` in `golem_add_external_resources()`. File `inst/app/www/favicon.png`: quadrato di 192 pixel con il simbolo aziendale su fondo bianco. L'immagine di origine non è nel repository | `test-moduli.R` |
| Aggiungere una scheda | `tabBox` e `conditionalPanel` in `R/app_ui.R`, collegamenti in `R/app_server.R` | `test-moduli.R` |
| Regole di ricerca: separatori, maiuscole | `analizza_input_ricerca()`, `codice_in()`, `cerca_per_rfid()`, `filtra_ultimo_per_utenza()` | `test-utils_data_processing.R` |
| Aspetto della ricerca RFID: evidenza, opacità, riquadri | ramo `rfid` di `disegna_marker()`, `add_rfid_bounds()` | `test-utils_mapping.R` |
| Raggio dei cluster di marker | `opzioni_cluster()` | `test-utils_mapping.R` |
| Sfondo e vista iniziale delle mappe | `mappa_base()` | `test-utils_mapping.R` |
| Parametri predefiniti di DBSCAN | `numericInput` in `R/mod_cluster_analysis_ui.R` e argomenti predefiniti di `calcola_analisi_cluster()` e `genera_analisi_cluster()` | `test-utils_cluster_output.R` |
| Soglie degli indicatori | `soglie_indicatore()` | `test-utils_cluster_output.R` |
| Regole degli indicatori | `classifica_cluster()` | `test-utils_cluster_output.R` |
| Pesi o formula dell'indice di fiducia | `pesi_fiducia()`, `indice_fiducia()` | `test-utils_cluster_output.R` |
| Colonne della tabella dei cluster | `colonne_analisi_cluster()`, `analisi_cluster_vuota()` e l'ultimo `mutate` di `calcola_analisi_cluster()`: i tre devono coincidere | `test-utils_cluster_output.R` |
| Nomi e colori degli indicatori | `indicatori_config()` | `test-utils_cluster_viste.R` |
| Tabella, grafici o mappa dei cluster spaziali | `R/utils_cluster_viste.R` | `test-utils_cluster_viste.R` |
| Stili grafici dell'app | `inst/app/www/custom_style.css` | nessuno |
| Dataset di esempio | `data-raw/genera_sample_dataset.R`, poi rigenerare i file in `inst/extdata/` | `test-sample_dataset.R`, `test-utils_storico.R` |
| Velocità di un passaggio su file grandi | misurare prima e dopo con `dev/misura_prestazioni.R` | i test del file modificato |
| Regole di preparazione dei dati reali: mezzi validi, formato degli RFID, associazione al database, servizio atteso | `R/fct_preparazione_letture.R` | `test-fct_preparazione_letture.R` |
| Tabelle estratte dal database, filtri sull'estrazione | `data-raw/01_estrai_tabelle.R` | nessuno |
| Pulizia delle singole tabelle estratte | codice commentato in `data-raw/02_importa_tabelle.R` | nessuno |
| Filtri finali e nome del CSV per l'app | `data-raw/03_crea_letture_app.R` | nessuno |
| Dipendenze | `DESCRIPTION`, poi `devtools::document()` | `devtools::check()` |

Dopo una modifica alle regole dell'analisi dei cluster vanno aggiornati anche `docs/analisi_cluster.md` e `README.md`. Il report `docs/indice_fiducia.Rmd` contiene controlli che fermano la compilazione se il testo non corrisponde più ai dati.

Il report `docs/beneficio_antenne.Rmd` gira anche sui dati reali, quindi nel testo non ha numeri scritti a mano: ogni numero è calcolato. Una frase nuova deve restare vera con qualsiasi dato: per questo i giudizi sono scelti con una condizione, e i confronti tra comuni usano solo quelli con abbastanza contenitori. Dopo una modifica alle regole del confronto vanno aggiornati anche il report, la sezione "Come sono fatti i conti" compresa, e `README.md`. Il report ha tre livelli: "L'intero territorio", "Cantiere per cantiere", "Comune per comune".

## File grandi

Il dataset reale atteso è di circa 2 milioni di letture l'anno. Ogni modifica deve lasciare l'app veloce a quella scala. Misure su 2 milioni di letture di 100.000 RFID, su un portatile:

| Passaggio | Tempo |
|---|---|
| Lettura del CSV da 320 MB, e validazione | 2,3 s e 2,5 s. Da Parquet 1,3 s e 1,7 s |
| Filtro del periodo e icone dei non censiti | 0,2 s |
| Cambio di un filtro: maschera e ultima lettura per RFID | 0,15 s |
| Disegno della mappa, lato server | meno di 0,1 s, messaggio sotto il megabyte |
| Nel browser, dal click al ridisegno finito | da 0,3 a 0,5 s per filtri, zoom e spostamenti |
| Ricerca di un RFID, dettaglio di un bidone | 0,1 s, 0,03 s |
| Analisi dei cluster | 1,8 s |

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
- **Confini e dataset di esempio.** Lo script del dataset di esempio mette ogni contenitore dentro il confine del suo comune e ricava il comune di lettura dal punto. Dopo un aggiornamento dei confini il dataset va rigenerato: possono cambiare le coordinate di qualche contenitore vicino a un confine e il comune di lettura di qualche lettura. Vanno poi rigenerata l'analisi dei cluster di esempio e rieseguiti i test.
- **Dati di esempio citati per nome.** Test e report fanno riferimento a RFID e conteggi precisi. Cambiare il seme o le regole dello script del dataset li fa fallire: vanno aggiornati insieme.
- **Compilazione dei report.** Vanno compilati dalla cartella `docs/`, senza l'opzione `intermediates_dir`, che produce avvisi spuri. `output_dir` invece si può usare.
- **Confronto tra i sistemi e coorte.** Il confronto parte dagli RFID letti dalle antenne, quindi per le antenne la quota di contenitori letti è il 100% per costruzione: non è un risultato e non va scritto come tale. Per lo stesso motivo un contenitore conta solo dall'anno successivo a quello della sua prima lettura.
- **Codice nei report e `highlight.js`.** Con l'evidenziazione predefinita di R Markdown un blocco di codice nel testo fa incorporare `highlight.js`, che su questo computer arriva troncato e dà un errore JavaScript nella pagina. `beneficio_antenne.Rmd` usa `highlight: tango`, che non richiede script.
- **`favicon()` e test dell'interfaccia.** golem mette il favicon nella sezione `head`, che `as.character(app_ui(NULL))` non riporta. Per controllarla nei test serve `htmltools::renderTags(app_ui(NULL))$head`.
- **Intestazioni delle tabelle DT.** L'argomento `colnames` di `DT::datatable()` cambia i nomi che `formatStyle()` deve usare. Nel report le colonne sono rinominate prima, nel data frame.
- **Output di tipo `reactive()` nei test.** Un output assegnato con `reactive()`, come quelli che pilotano un `conditionalPanel`, non si legge da `shiny::testServer()`: dà errore. Nei test si legge il reactive interno, ad esempio `con_geografia()` in `mod_filtri`.
- **Elenco a scelta multipla senza scelte.** Con nessun comune scelto `input$comuni` è `NULL`, non un vettore vuoto: l'osservatore usa `ignoreNULL = FALSE`.
- **Attese nei test della mappa.** Il ridisegno dopo uno spostamento parte dopo 250 millisecondi: nei test va fatto passare il tempo con `session$elapse()`.
- **Numeri tondi.** `format(1e5, big.mark = ".")` dà `"1e+05"`. `formatta_numero()` usa `scientific = FALSE`: per i conteggi usare sempre quella.
- **Servizio atteso sui censiti.** Nei dati reali `servizio_atteso` è compilato per ogni lettura. Una regola che legge quella colonna senza guardare `presente_a_database` tratta i censiti come non censiti: è l'errore che avevano i filtri e le statistiche. Filtri, statistiche, stima del pannello di dettaglio e composizione dei giri nell'analisi dei cluster contano le sole letture "Non Presente".
- **Sacchetti nell'esempio, non nei dati reali.** Il dataset di esempio contiene 12 sacchetti, per scelta del proprietario del progetto, che vuole vedere come l'app li tratta. Nei dati reali i passi 3 e 4 li scartano ancora. Per questo i conteggi dell'esempio vanno letti come 239 bidoni più 12 sacchetti: i test sulle regole dei bidoni, come volume, raccolte e comune del database, li escludono con `rfid_sacchetto()`.
- **Sacchetti fuori dalle analisi.** L'analisi dei cluster e il report sul beneficio delle antenne tolgono i sacchetti con `senza_sacchetti()`. Le funzioni di `R/utils_storico.R` non lo fanno da sole: `anagrafica_rfid()`, `unisci_letture()` e `andamento_rfid()` servono anche al dettaglio di un sacchetto nell'app, che ne mostra l'istogramma. Un'analisi nuova sui contenitori deve togliere i sacchetti all'inizio, come fa il report. Mappa, ricerche, filtri e statistiche della barra laterale invece li comprendono: per questo sulla mappa del 2025 ci sono 248 RFID e nell'analisi dei cluster 238.
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
| `docs/analisi_cluster.md` | Analisi dei cluster spaziali: colonne, indicatori, indice di fiducia, scelte e limiti |
| `docs/indice_fiducia.Rmd` | Report sull'indice di fiducia, con esempi svolti e prove di efficacia |
| `docs/beneficio_antenne.Rmd` | Report sul beneficio delle antenne: letture per anno, anni vuoti, tasso di lettura prima e dopo, a tre livelli: intero territorio, cantiere, comune |
