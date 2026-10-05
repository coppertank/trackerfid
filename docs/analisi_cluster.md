# Analisi dei cluster spaziali

Questo documento spiega come l'app raggruppa le letture di ogni contenitore in luoghi distinti, come leggere la tabella che ne risulta e quali scelte sono state fatte dove la specifica chiedeva un consiglio.

L'analisi risponde a una domanda: nel periodo scelto, ogni tag è rimasto in un posto, è stato spostato, oppure si comporta in modo anomalo?

## Come funziona

1. **Periodo.** Si considerano le letture comprese tra `analisi_dal` e `analisi_al`, estremi inclusi.
2. **Cluster.** Per ogni RFID, DBSCAN raggruppa le letture vicine. Con i parametri predefiniti due letture stanno nello stesso cluster se sono collegate da una catena di letture distanti al massimo 100 metri. I cluster sono numerati in ordine di prima lettura: il cluster 1 è il luogo in cui il contenitore è stato visto per primo.
3. **Descrizione.** Per ogni cluster si calcolano tempi, numero di letture, baricentro e dispersione. La dispersione è il 90° percentile della distanza di Haversine delle letture dal baricentro.
4. **Valutazione.** Ogni cluster riceve un indicatore e un indice di fiducia.

Il clustering è indipendente per ogni RFID. Lo stesso `cluster_id` su due RFID diversi non indica lo stesso luogo.

### Giorni osservati

Le quantità annue non usano la lunghezza del periodo, ma i **giorni osservati**: la parte del periodo effettivamente coperta dal dataset. Se si analizza l'anno intero con un file che arriva a marzo, i giorni osservati sono 90 e non 365. In questo modo frequenze e raccolte annue non vengono sottostimate. Con un anno di dati completo i due valori coincidono.

## La tabella di output

Una riga per ogni cluster di ogni RFID. La specifica parlava di 25 campi ma ne definiva 22: i tre aggiunti sono segnati con un asterisco.

| Campo | Contenuto |
|---|---|
| `RFID` | Codice del contenitore |
| `cluster_id` | Numero del cluster per quell'RFID, in ordine di prima lettura. Vale 0 per le letture isolate quando le letture minime sono più di 1 |
| `globale_conteggio_cluster` | Numero di cluster dell'RFID nel periodo |
| `globale_prima_lettura`, `globale_ultima_lettura` | Prima e ultima lettura dell'RFID nel periodo |
| `cluster_prima_lettura`, `cluster_ultima_lettura` | Prima e ultima lettura del cluster |
| `presente_a_database` * | Stato dell'ultima lettura del periodo. Stabilisce quali campi sono compilati |
| `servizio_transponder` | Servizio dell'ultima lettura del periodo. Vuoto per i non censiti |
| `servizio_transponder_cronologia` * | Sequenza dei servizi quando è cambiato, ad esempio `CARTA > SECCO > CARTA`. Vuoto se non è cambiato |
| `servizio_atteso` | Solo per i non censiti: il giro prevalente nel cluster se copre almeno l'80% delle letture con stima, altrimenti vuoto |
| `servizio_atteso_dettaglio` * | Solo per i non censiti: composizione completa, ad esempio `CARTA CONT.STRADALI (40%); SECCO PAP (35%); UMIDO PAP (25%)` |
| `volume_previsto` | Volume a database, solo per i censiti |
| `volume_atteso` | Copia di `volume_previsto`, riservata a sviluppi futuri |
| `analisi_dal`, `analisi_al` | Periodo di analisi |
| `numero_raccolte_annue_previste` | Raccolte annue a database, solo per i censiti |
| `numero_raccolte_annue_presunte` | Raccolte annue stimate dalle letture del cluster |
| `globale_numero_letture` | Letture dell'RFID nel periodo |
| `cluster_numero_letture` | Letture del cluster |
| `lat_baricentro_cluster`, `lon_baricentro_cluster` | Media delle coordinate del cluster |
| `cluster_dispersione_90th_m` | Dispersione del cluster in metri |
| `cluster_indice_fiducia` | Affidabilità del cluster, da 0 a 1 |
| `indicatore_cluster` | Categoria del cluster |

### Raccolte annue presunte

La formula è quella proposta nella specifica: il maggiore tra due stime.

```
max( floor(letture_cluster × 365 / giorni_osservati),
     floor(letture_cluster × 365 / durata_cluster_in_giorni) )
```

La seconda stima evita di sottostimare un contenitore attivo solo per una parte dell'anno. Viene usata solo se il cluster dura almeno 30 giorni. Senza questa soglia due letture a un giorno di distanza darebbero 730 raccolte l'anno, e una lettura sola una divisione per zero.

## Indicatori

Le regole si valutano in ordine: vale la prima che si applica. La frequenza è quella annua dell'RFID, cioè `letture × 365 / giorni_osservati`. La dispersione è quella del singolo cluster.

| Ordine | Indicatore | Regola | Significato |
|---|---|---|---|
| 1 | `GHOST_TAG` | Frequenza sotto 5 | Tag smarrito o letto per caso |
| 2 | `RELOCATED_BIN` | Frequenza 5-300, 2 o 3 cluster, tutti sotto 50 m | Contenitore spostato |
| 3 | `DEPOT_STUCK` | Frequenza sopra 300, 1 cluster, sotto 50 m | Tag fermo vicino a un lettore |
| 4 | `TRUCK_STOWAWAY` | Frequenza sopra 300, dispersione sopra 500 m | Tag rimasto sul mezzo |
| 5 | `SCATTERED_READS / GPS_NOISE` | Frequenza 5-300, più di 3 cluster oppure dispersione sopra 1000 m | Letture sparse o GPS impreciso |
| 6 | `VALID_TARGET` | Frequenza 5-300, 1 cluster, sotto 50 m | Contenitore al suo posto |
| 7 | `UNCATEGORIZED` | Tutti gli altri casi | Da verificare a mano |

### Differenze rispetto allo pseudocodice della specifica

- **Frequenza annua invece del conteggio.** Su un anno di dati completo il risultato è identico. Su periodi o dataset più corti le soglie 5 e 300 restano valide senza doverle ricalcolare.
- **`RELOCATED_BIN` limitato a 3 cluster.** Nello pseudocodice bastavano 2 o più cluster compatti. Ma un tag frammentato in dieci gruppetti compatti sarebbe risultato "spostato", e la regola dei "più di 3 cluster" di `SCATTERED_READS` non sarebbe mai scattata per i frammenti compatti.
- **`DEPOT_STUCK` richiede 1 cluster.** La tabella della specifica lo prevedeva, lo pseudocodice no. Senza questa condizione un tag con molte letture sparse in tanti gruppetti compatti sarebbe risultato "fermo in deposito".

## Indice di fiducia

Somma pesata di sei componenti, ognuna tra 0 e 1. Il report [indice_fiducia.Rmd](indice_fiducia.Rmd) lo spiega per esteso, con esempi svolti e prove di efficacia.

| Componente | Peso | Vale 1 quando | Vale 0 quando |
|---|---|---|---|
| Censimento | 0,15 | Il contenitore è a database | Non è censito |
| Letture | 0,25 | Il cluster ha almeno 10 letture | Cresce in proporzione da 0 a 10 |
| Compattezza | 0,25 | Dispersione fino a 50 m | Dispersione da 500 m in su, scala logaritmica |
| Coerenza | 0,15 | Censito con servizio mai cambiato, oppure non censito con giro prevalente dall'80% | Non censito con giro prevalente fino al 50%. Vale 0,5 per un censito che ha cambiato servizio |
| Raccolte | 0,10 | Presunte uguali alle previste | Presunte lontanissime dalle previste. Vale 0,5 se le previste non sono note |
| Unicità | 0,10 | Tutte le letture dell'RFID sono nel cluster | Il cluster contiene una quota minima delle letture |

La componente delle raccolte è il rapporto tra il valore minore e il maggiore: 26 previste e 52 presunte danno 0,5, 26 previste e 200 presunte danno 0,13.

Si è scelta la somma pesata perché si spiega facilmente e ogni punto perso è riconducibile a una causa. Compattezza e numero di letture pesano di più perché sono misurati direttamente, mentre censimento e raccolte dipendono dalla qualità dell'anagrafica.

## Personalizzare i parametri

| Cosa | Dove | Predefinito |
|---|---|---|
| Raggio di DBSCAN | Scheda dell'analisi, «Parametri DBSCAN». Nello script: argomento `eps_m` | 100 m |
| Letture minime per cluster | Come sopra. Nello script: `min_pts` | 1 |
| Soglie degli indicatori | Funzione `soglie_indicatore()` in `R/utils_cluster_output.R` | 5 e 300 letture annue, 50, 500 e 1000 m, 3 cluster, 80% |
| Pesi dell'indice di fiducia | Funzione `pesi_fiducia()` nello stesso file | Vedi tabella sopra |
| Colori e descrizioni degli indicatori | Funzione `indicatori_config()` nello stesso file | |

Un raggio più ampio unisce luoghi vicini e riduce i cluster. Con letture minime maggiori di 1 le letture isolate non formano un cluster: finiscono nel cluster 0, che le raccoglie tutte.

### Nota tecnica sulle distanze

Per trovare i cluster le coordinate vengono proiettate in metri su un piano locale centrato sulle letture dell'RFID. DBSCAN può così usare un indice spaziale invece della matrice di tutte le distanze, che per un tag con 20.000 letture occuperebbe più di 3 GB. Lo scarto rispetto alla distanza di Haversine è inferiore all'1% su estensioni di pochi chilometri. Un test verifica che sul dataset di esempio i cluster coincidano con quelli ottenuti dalla matrice di Haversine. Baricentro e dispersione usano Haversine.

Attenzione a un dettaglio del codice proposto nella specifica: `dbscan::dbscan()` interpreta una matrice come coordinate dei punti. Per passargli distanze già calcolate servirebbe `as.dist()`.

## Scelte e consigli

Risposte alle richieste di consiglio della specifica. Per ognuna: cosa è stato fatto, perché, e cosa si potrebbe fare in più.

### Servizio transponder quando cambia nell'anno

**Fatto.** `servizio_transponder` riporta il servizio dell'ultima lettura. La colonna aggiuntiva `servizio_transponder_cronologia` riporta la sequenza dei cambi.

**Perché.** L'ultimo valore è lo stato attuale del database, quello con cui confrontarsi. La moda nasconderebbe un cambio recente, il primo valore descriverebbe una situazione superata. La cronologia in una colonna a parte tiene pulito il campo principale e permette di filtrare con un solo criterio tutti i contenitori che hanno cambiato servizio. Il cambio abbassa anche l'indice di fiducia.

**In più.** Una sequenza del tipo A, B, A con B presente in una sola lettura fa pensare a un errore di inserimento più che a un vero cambio. Si potrebbe segnalarla con un campo dedicato.

### Servizio atteso quando le letture non concordano

**Fatto.** Sotto l'80% `servizio_atteso` resta vuoto. `servizio_atteso_dettaglio` riporta sempre la composizione completa.

**Perché.** Il campo principale contiene solo valori affidabili, quindi si presta a filtri e tabelle pivot. L'informazione non va persa perché è nella colonna di dettaglio. Con 50% e 50%, o con 40%, 35% e 25%, il campo resta vuoto.

**Sulla soglia.** L'80% è ragionevole con una ventina di letture l'anno: tollera quattro letture fuori giro. Con meno di cinque letture la quota è instabile, perché bastano due letture concordi per avere il 100%. In quei casi conviene guardare anche il numero di letture o l'indice di fiducia.

**Una differenza da conoscere.** Per l'icona sulla mappa la quota si calcola per tipologia: `CARTA CONT.STRADALI` e `CARTA/CARTONE PAP` contano insieme, perché l'icona è la stessa. Nella tabella dei cluster si calcola sul nome esatto del giro, perché stradale e porta a porta sono servizi diversi.

### Periodo di analisi

**Fatto.** Un solo controllo nella barra laterale, valido per tutta l'app: selezione dell'anno, con l'anno più recente come predefinito, e una casella «Periodo personalizzato» che mostra le due date.

**Perché.** L'analisi annuale standard richiede un solo click. Chi vuole un periodo diverso ha le due date, precompilate con l'anno scelto. Un controllo unico garantisce che mappe, ricerche e analisi dei cluster parlino sempre dello stesso periodo. Un secondo selettore dentro la scheda dei cluster avrebbe permesso di avere due periodi diversi a schermo.

**In più.** Preimpostazioni come l'anno fiscale si possono aggiungere come voci dell'elenco degli anni.

### Classificazione dei cluster

Le tre correzioni applicate sono descritte sopra. Restano alcuni punti deboli, lasciati come nella specifica.

- **Casi non coperti.** Finiscono in `UNCATEGORIZED`: un solo cluster con dispersione tra 50 e 1000 m, 2 o 3 cluster non tutti compatti, più di 300 letture con dispersione tra 50 e 500 m o con più cluster. Sui dati reali il primo caso sarà frequente. Si suggerisce una categoria per i cluster larghi, ad esempio `WIDE_TARGET` tra 50 e 300 m.
- **Spostamento o lettura anomala.** Un contenitore regolare con una sola lettura GPS sbagliata a 150 m ha 2 cluster compatti e risulta `RELOCATED_BIN`. Uno spostamento vero ha due proprietà in più: ogni cluster ha diverse letture e i cluster si succedono nel tempo senza alternarsi. Si suggerisce di richiederle entrambe.
- **Tag sul mezzo.** Con un raggio di 100 m, un tag che viaggia viene spezzato in tanti gruppetti compatti, a meno che le letture siano così fitte da formare una catena. La regola sulla dispersione del singolo cluster rischia quindi di non scattare. Si suggerisce di valutarla sulla dispersione di tutte le letture dell'RFID. Aiuterebbe anche il numero di mezzi diversi che leggono il tag: uno solo per un tag sul camion, molti per un tag fermo in deposito.
- **Soglia delle 300 letture.** Un contenitore svuotato sei giorni la settimana supera le 300 letture l'anno e verrebbe trattato come anomalo. Dove le raccolte previste sono note, la soglia potrebbe essere un multiplo di quelle, ad esempio il triplo.
- **Soglia dei 50 m.** È realistica per il GPS di bordo, che sbaglia di 5-15 m. Un contenitore stradale letto dai due lati della strada, o in vie strette tra edifici alti, può arrivare a 50-80 m.
- **Letture o raccolte.** Due letture nello stesso giorno contano come due. Contare i giorni distinti di lettura darebbe una stima più fedele delle raccolte.

### Casi limite

| Caso | Comportamento |
|---|---|
| RFID con una sola lettura | Un cluster con dispersione 0. Raccolte presunte dalla sola stima sul periodo |
| Letture senza coordinate | Scartate al caricamento del file, con avviso |
| RFID letto solo a inizio e fine anno | La durata del cluster copre l'anno, la stima resta bassa |
| Dataset di pochi mesi | Frequenze e raccolte rapportate ai giorni osservati |
| Censito senza servizio a database | Analizzato normalmente, `servizio_transponder` vuoto, punto di domanda sulla mappa |
| Stato a database che cambia nel periodo | Vale lo stato dell'ultima lettura |
| Periodo senza letture | Tabella vuota e messaggio nell'app |
| File senza volume e raccolte previste | Analisi eseguita, campi vuoti, avviso al caricamento |

Un limite della formula delle raccolte presunte: divide le letture per la durata, mentre gli intervalli tra le letture sono uno in meno. Con poche letture la stima sulla durata è quindi abbondante: tre letture in 30 giorni danno 36 raccolte l'anno invece di 24.

### Prestazioni

Misure su un portatile, con un dataset di prova ottenuto replicando quello di esempio.

| Operazione | Dati | Tempo |
|---|---|---|
| Analisi dei cluster | 142.000 letture, 5.200 RFID | 1,6 s |
| Periodo e icone dei non censiti | 142.000 letture | 0,03 s |
| Un solo tag | 20.000 letture | 0,06 s |

Cosa rende veloce il calcolo: l'indice spaziale al posto delle matrici di distanze, e i riepiloghi calcolati per gruppi su tutta la tabella, con una sola chiamata alla distanza di Haversine per tutte le letture.

Con questi volumi non servono `data.table`, parallelizzazione o cache. Nell'app il risultato resta in memoria finché non cambiano dataset o periodo. Oltre il milione di letture il primo collo di bottiglia sarebbe la mappa principale, non l'analisi: converrebbe disegnare i marker solo per l'area visibile.
