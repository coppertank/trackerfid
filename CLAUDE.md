# trackerfid: contesto per assistenti LLM

Questo file descrive il progetto a un assistente che deve leggerlo o modificarlo. Va letto prima di aprire il codice. Le istruzioni per chi usa l'app sono in `README.md`.

Quando aggiungi o sposti file e funzioni, o cambi il flusso dei dati, aggiorna anche questo file.

## In breve

`trackerfid` è un pacchetto R che contiene un'app Shiny, costruita con il framework golem. Un'azienda di raccolta rifiuti la usa per esplorare su mappa le letture RFID dei propri bidoni e per capire dove ogni bidone è stato letto nel tempo.

- Linguaggio: R (>= 4.1), Shiny, golem, shinydashboard, leaflet, dplyr.
- Lingua del progetto: italiano, per interfaccia, nomi di funzioni e variabili, commenti, test e documenti.
- Avvio: `run_app()`, oppure `run_app(demo = TRUE)` per partire con il dataset di esempio.

## Contesto del dominio

- Ogni bidone porta un chip, il transponder, con un codice `RFID`.
- I mezzi di raccolta hanno un lettore e un GPS. A ogni passaggio registrano una **lettura**: data e ora, mezzo, RFID, coordinate.
- Il database aziendale associa a un RFID un servizio, cioè il tipo di rifiuto, e un'utenza, cioè il proprietario. Un RFID in anagrafica è **censito**, gli altri sono **non censiti**.
- Per un non censito il servizio non è noto. Viene stimato dal **giro** che il mezzo stava facendo al momento della lettura.
- Gli operatori cercano: bidoni non censiti, cambi di servizio o di utenza, bidoni spostati, tag anomali (smarriti, fermi in deposito, rimasti su un mezzo).

## Glossario

| Termine | Significato | Nel codice |
|---|---|---|
| Lettura | Una riga del CSV: un RFID letto da un mezzo in un istante e in un punto | righe del dataset |
| Censito, non censito | RFID presente o assente nel database aziendale | `presente_a_database`: `"Presente"` o `"Non Presente"` |
| Servizio transponder | Tipo di rifiuto secondo il database. Sei valori: `SECCO`, `CARTA`, `VETRO`, `UMIDO`, `PLASTICA E METALLI`, `VERDE E RAMAGLIE` | `servizio_transponder`, vuoto per i non censiti |
| Servizio atteso, giro | Nome del giro del mezzo, usato come stima per i non censiti. Dodici valori, ad esempio `SECCO PAP`. PAP significa porta a porta | `servizio_atteso`, vuoto per i censiti |
| Tipologia | Gruppo di nomi di servizio che condividono icona e colore | `tipologie_servizio()` |
| Utenza | Proprietario del bidone secondo il database | `id_utenza` |
| Periodo di analisi | Anno solare o intervallo di date. Vale per tutta l'app | modulo `mod_periodo` |
| Ultima lettura | La lettura più recente di un RFID. La mappa principale ne mostra una per RFID | `deduplica_ultimo_rfid()` |
| Cluster di marker | Raggruppamento visivo dei marker vicini sulla mappa principale | modalità `"cluster"`, `opzioni_cluster()` |
| Cluster spaziale | Gruppo di letture vicine dello stesso RFID, trovato con DBSCAN | `cluster_id`, `R/utils_dbscan_clustering.R` |
| Dispersione | 90° percentile della distanza delle letture dal baricentro del cluster spaziale, in metri | `cluster_dispersione_90th_m` |
| Giorni osservati | Parte del periodo di analisi coperta dal dataset. Serve a riportare i conteggi a un anno | attributo `parametri` del risultato |
| Indicatore | Categoria di un cluster spaziale. Sette valori, ad esempio `VALID_TARGET` | `indicatore_cluster`, `classifica_cluster()` |
| Indice di fiducia | Voto da 0 a 1 sull'affidabilità di un cluster spaziale: somma pesata di sei verifiche | `cluster_indice_fiducia`, `indice_fiducia()` |

**Attenzione: la parola "cluster" ha due significati che non c'entrano l'uno con l'altro.** Sulla mappa principale è un effetto grafico. Nell'analisi è il risultato di DBSCAN.

## Funzionalità

L'interfaccia è una dashboard. La barra laterale contiene, dall'alto: caricamento del file, periodo di analisi, dettaglio del bidone selezionato, controlli della scheda aperta. Il corpo ha quattro schede, in un `tabBox` con id `scheda_attiva`.

| Scheda | Valore di `scheda_attiva` | Cosa mostra | Controlli nella barra laterale |
|---|---|---|---|
| Mappa Principale | `mappa` | Un marker per RFID, nella posizione dell'ultima lettura del periodo. Marker raggruppati in cluster colorati secondo la quota di censiti | Filtri per stato a database, servizio transponder, servizio atteso. Statistiche |
| Ricerca RFID | `rfid` | Tutte le letture degli RFID cercati. Ultima lettura in evidenza, riquadro attorno alle posizioni di ogni RFID | Campo di testo, pulsanti Cerca e Pulisci |
| Ricerca Utenza | `utenza` | Ultima lettura dei bidoni la cui utenza attuale è tra quelle cercate | Campo di testo, pulsanti Cerca e Pulisci |
| Analisi Cluster Spaziale | `cluster` | Tabella, grafici e mappa dei cluster spaziali di ogni RFID, con indicatore e indice di fiducia. Esportazione in CSV | Parametri DBSCAN, pulsante Genera Analisi |

Altri comportamenti:

- **Marker.** Verde a goccia se censito, rosso squadrato se non censito. L'icona interna indica il servizio. Per un non censito è il servizio atteso prevalente, se raggiunge l'80% delle letture con stima, altrimenti un punto di domanda.
- **Dettaglio del bidone.** Un click su un marker, su una riga della tabella dei cluster o su un baricentro apre nella barra laterale la storia dell'RFID: servizio coerente, cambio di servizio, stima per i non censiti, cambio di utenza.
- **Titolo.** Riporta il periodo, ad esempio "DASHBOARD RFID - ANNO 2025".

## Dati

Il CSV ha dieci colonne obbligatorie e due facoltative. L'ordine e le maiuscole dei nomi non contano.

| Colonna | Tipo dopo la validazione | Note |
|---|---|---|
| `giorno_lettura` | POSIXct, fuso UTC | Formato `YYYY-MM-DD HH:MM:SS`. Accettate anche date italiane |
| `targa_veicolo`, `matricola_veicolo` | testo | Relazione 1:1 |
| `RFID` | testo | Si ripete a ogni lettura |
| `presente_a_database` | testo | Solo `Presente` o `Non Presente` |
| `servizio_transponder` | testo maiuscolo | Vuoto per i non censiti |
| `servizio_atteso` | testo maiuscolo | Vuoto per i censiti |
| `id_utenza` | testo | Vuoto per i non censiti |
| `latitudine`, `longitudine` | numerico | Accettata la virgola decimale |
| `volume_previsto` | numerico, facoltativa | Litri. Usata dall'analisi dei cluster |
| `numero_raccolte_annue_previste` | numerico, facoltativa | Usata dall'analisi dei cluster |

La validazione rifiuta il file se mancano colonne o se ci sono valori non interpretabili. Scarta, con un avviso, le righe prive di un campo essenziale.

Il dataset di esempio è `inst/extdata/sample_rfid_dataset.csv`: 6.463 letture di 239 RFID, da ottobre 2024 a dicembre 2025, cinque mezzi, 50 utenze, area di Roma. È simulato dallo script `data-raw/genera_sample_dataset.R` con un seme fisso. Test e report fanno riferimento a questi RFID:

| RFID | Caso |
|---|---|
| `RFD20250901001` | Servizio cambiato: CARTA, SECCO, di nuovo CARTA |
| `RFD20250901050` | Utenza cambiata da `UTZ001` a `UTZ025` |
| `RFD20250915201` | Non censito, quattro letture, stima 100% `SECCO PAP`. Indicatore `GHOST_TAG` |
| `RFD20250920250` | Non censito senza stima |
| `RFD20250905100` | Spostato di circa 21 km. Indicatore `RELOCATED_BIN` |
| `RFD20241001301` | Tag fermo in deposito. Indicatore `DEPOT_STUCK` |
| `RFD20250203302` | Tag rimasto sul mezzo. Indicatore `TRUCK_STOWAWAY` |
| `RFD20241003303` | GPS impreciso, 17 cluster. Indicatore `SCATTERED_READS / GPS_NOISE` |

## Architettura

### Flusso dei dati

```
file CSV
   │
   ▼
mod_caricamento ──► dati_caricati: tutte le letture validate
   │
   ├──► mod_periodo ──► dati: letture del periodo, con la colonna servizio_icona
   │        │
   │        ├──► mod_filtri ──► dati_filtrati: una riga per RFID ──► mod_mappa "mappa_principale"
   │        ├──► mod_ricerca "ricerca_rfid" ──► risultati ──► mod_mappa "mappa_rfid"
   │        ├──► mod_ricerca "ricerca_utenza" ──► risultati ──► mod_mappa "mappa_utenza"
   │        └──► dettaglio del bidone: output$info_panel in app_server
   │
   └──► mod_cluster_analysis: riceve dati_caricati e il periodo ──► tabella, grafici, mappa, CSV
```

`R/app_server.R` contiene solo questi collegamenti, il titolo e il dettaglio del bidone.

### Moduli

Ogni modulo ha due file: `R/mod_<nome>_ui.R` e `R/mod_<nome>_server.R`.

| Modulo e id usati | Riceve | Restituisce | Input principali |
|---|---|---|---|
| `mod_caricamento` (`caricamento`) | niente | reactive con il dataset validato, o `NULL` | `file_upload`, `carica_esempio` |
| `mod_periodo` (`periodo`) | `dati` | lista di reactive: `periodo`, `etichetta`, `dati` | `anno_filtro`, `personalizzato`, `intervallo` |
| `mod_filtri` (`filtri`) | `dati`, `azzera` | lista con il reactive `dati_filtrati` | `presente_filter`, `servizio_transponder_check`, `includi_non_censiti`, `servizio_atteso_check`, `includi_senza_stima`, `reset` |
| `mod_ricerca` (`ricerca_rfid`, `ricerca_utenza`) | `dati`, `tipo` | lista di reactive: `risultati`, `codici` | `testo`, `cerca`, `pulisci` |
| `mod_mappa` (`mappa_principale`, `mappa_rfid`, `mappa_utenza`) | `dati`, `modalita`, `dati_vista`, `attiva`, `messaggio` | reactive con il marker cliccato: lista con `rfid` e `quando` | `mappa_marker_click`, `mappa_zoom` |
| `mod_cluster_analysis` (`cluster`) | `dati`, `periodo`, `etichetta` | reactive con il cluster scelto: lista con `rfid` e `quando` | `eps_m`, `min_pts`, `genera`, `tabella_rows_all`, `tabella_rows_selected`, `mappa_marker_click` |

`mod_mappa` ha tre modalità: `"cluster"`, `"rfid"`, `"utenza"`. La modalità decide come `disegna_marker()` disegna i dati. `mod_cluster_analysis` ha due funzioni di interfaccia: `mod_cluster_analysis_ui()` per il corpo e `mod_cluster_analysis_sidebar_ui()` per la barra laterale.

### Scelte di progetto da rispettare

1. **Logica fuori dai moduli.** Calcoli e costruzione di HTML, mappe e grafici stanno nei file `R/utils_*.R`, come funzioni senza reattività e coperte da test. I moduli collegano input e output.
2. **Stato sul server.** `mod_filtri` e `mod_periodo` tengono lo stato in un `reactiveValues` e lo riallineano agli input. Al caricamento di un nuovo dataset lo stato torna subito ai valori predefiniti, senza attendere il browser. Gli `observeEvent` sugli input usano `ignoreInit = TRUE`.
3. **Servizi filtrati per esclusione.** I filtri memorizzano i servizi deselezionati, non quelli selezionati. Così le scelte restano valide quando l'elenco dei servizi cambia con il periodo.
4. **Filtri prima della deduplica.** `filtra_letture()` agisce su tutte le letture, poi `deduplica_ultimo_rfid()` tiene l'ultima rimasta per ogni RFID.
5. **Mappe aggiornate con proxy.** La mappa di base si disegna una volta. I marker si aggiornano con `leafletProxy()`, solo quando la mappa è pronta e la sua scheda è visibile. Gli aggiornamenti arrivati nel frattempo restano in attesa.
6. **Identificativo dei marker.** Il `layerId` è il numero di riga dei dati disegnati. Il click risale così alla lettura anche quando lo stesso RFID ha più marker.
7. **Marker in SVG.** Ogni marker è un'immagine SVG generata in R e passata come data URI. Colore e forma indicano lo stato, il glifo Font Awesome il servizio.
8. **Icona dei non censiti.** `mod_periodo` aggiunge alle letture la colonna `servizio_icona`. `aggiungi_marker()` la usa al posto di `servizio_transponder`.
9. **Analisi dei cluster in una funzione sola.** `calcola_analisi_cluster()` è usata dall'app, dalla funzione esportata `genera_analisi_cluster()` e dallo script a riga di comando.
10. **Orari senza fuso.** Date e ore sono lette e scritte sempre in UTC, come se fossero ora locale. Non convertire i fusi orari.

## Mappa del repository

```
trackerfid/
├── CLAUDE.md                     questo file
├── README.md                     guida per chi usa l'app
├── DESCRIPTION                   metadati e dipendenze del pacchetto
├── NAMESPACE                     generato da roxygen2: non modificare a mano
├── R/                            codice del pacchetto
│   ├── run_app.R                 run_app(): avvio dell'app. Esportata
│   ├── app_config.R              app_sys(), get_golem_config(): generati da golem
│   ├── app_ui.R                  struttura della pagina, schede, barra laterale
│   ├── app_server.R              collegamento dei moduli, titolo, dettaglio del bidone
│   ├── mod_caricamento_ui.R / _server.R        caricamento e validazione del CSV
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
│   └── generate_cluster_analysis.R   genera_analisi_cluster(), scrivi_analisi_cluster(). Esportate
├── inst/
│   ├── app/www/custom_style.css  tutti gli stili dell'app
│   ├── app/www/script.js         un solo gestore: porta in vista il dettaglio del bidone
│   ├── extdata/                  dataset di esempio e analisi dei cluster di esempio
│   ├── scripts/generate_cluster_analysis.R   analisi dei cluster da riga di comando
│   └── golem-config.yml          configurazione golem
├── data-raw/
│   ├── genera_sample_dataset.R   genera il dataset di esempio
│   └── sample_rfid_dataset_precedente.csv   copia del primo dataset: non usata
├── docs/
│   ├── analisi_cluster.md        regole, colonne, soglie e scelte dell'analisi dei cluster
│   └── indice_fiducia.Rmd        report sull'indice di fiducia, da compilare in HTML
├── tests/testthat/
│   ├── helper-dati.R             dati di prova condivisi tra i test
│   └── test-*.R                  un file per ogni file utils, più moduli, dataset e script
├── dev/                          script golem. run_dev.R avvia l'app in sviluppo
└── man/                          pagine di aiuto generate: non modificare a mano
```

`data-raw/`, `dev/`, `docs/` e questo file sono esclusi dalla costruzione del pacchetto tramite `.Rbuildignore`.

## Indice delle funzioni

Le funzioni esportate sono tre: `run_app()`, `genera_analisi_cluster()`, `scrivi_analisi_cluster()`. Tutte le altre sono interne.

### `R/utils_data_processing.R`

| Gruppo | Funzioni |
|---|---|
| Schema del dataset | `colonne_obbligatorie()`, `colonne_facoltative()`, `colonne_dataset()`, `stati_database()`, `percorso_dataset_esempio()` |
| Lettura e validazione | `carica_dataset()` è il punto di ingresso: chiama `read_csv_auto()` e `validate_dataset()`. Ausiliarie: `rileva_separatore()`, `converti_data_ora()`, `converti_numero()`, `elenco_righe()`, `errore_validazione()` |
| Periodo | `anni_disponibili()`, `periodo_anno()`, `etichetta_periodo()`, `filtra_periodo()` |
| Icona dei non censiti | `servizio_prevalente_non_censiti()`, `get_icon_for_uncensored_rfid()`, `aggiungi_servizio_icona()` |
| Filtri e statistiche | `filtra_letture()`, `deduplica_ultimo_rfid()`, `calcola_pct_presente()`, `ordina_servizi()`, `conta_servizi()`, `calcola_statistiche()` |
| Ricerche | `analizza_input_ricerca()`, `codice_in()`, `cerca_per_rfid()`, `filtra_ultimo_per_utenza()`, `calcola_bbox()` |
| Storia di un RFID | `analizza_rfid()` decide il caso da mostrare nel dettaglio. Ausiliarie: `cronologia_transizioni()`, `stima_servizio_atteso()` |

### `R/utils_styling.R`

| Gruppo | Funzioni |
|---|---|
| Servizi | `tipologie_servizio()` è l'unico elenco di servizi, icone, colori ed emoji. Da lì derivano `servizi_config()`, `info_servizio()`, `tipologia_servizio()` |
| Colori | `colori_stato()`, `get_color()`, `genera_colore_cluster()` |
| Marker | `svg_marker()` disegna il marker, `get_leaflet_icon()` lo trasforma in icone Leaflet. Ausiliarie: `glifo_fa()`, `svg_glifo()`, `svg_icona_servizio()`, `uri_svg()` |
| Cluster di marker | `js_icona_cluster()` restituisce il JavaScript che colora i cluster |
| Legenda | `html_legenda()` |

### `R/utils_mapping.R`

`mappa_base()`, `opzioni_cluster()`, `pulisci_mappa()`, `aggiungi_marker()`, `add_rfid_bounds()`, `disegna_marker()`, `adatta_vista()`. Funzionano sia su una mappa `leaflet()` sia su un `leafletProxy()`.

### `R/utils_popup.R`

| Gruppo | Funzioni |
|---|---|
| Formati | `formatta_data_ora()`, `formatta_numero()` |
| Popup del marker | `create_popup_html()` |
| Dettaglio del bidone | `crea_info_panel()`. Ausiliarie: `blocco_info()`, `elenco_cronologia()`, `barre_stima()` |
| Statistiche | `crea_box_statistiche()`, `righe_servizi()` |

### `R/utils_dbscan_clustering.R`

`cluster_dbscan()` raggruppa le letture di un RFID. `assegna_cluster()` lo applica a tutti gli RFID e aggiunge `cluster_id`. `ordina_cluster()` numera i cluster in ordine di prima lettura. Distanze: `raggio_terra_m()`, `proietta_metri()`, `distanza_haversine_m()`, `calcola_dispersione_90th()`.

### `R/utils_cluster_output.R`

| Gruppo | Funzioni |
|---|---|
| Parametri | `soglie_indicatore()`, `pesi_fiducia()`, `indicatori_config()`, `colonne_analisi_cluster()` |
| Calcolo | `calcola_analisi_cluster()` produce la tabella con una riga per cluster. Usa `classifica_cluster()` e `indice_fiducia()` |
| Esportazione | `nome_file_analisi_cluster()`, `esporta_analisi_cluster()` |
| Ausiliarie | `analisi_cluster_vuota()`, `ultimo_valido()`, `sequenza_valori()`, `tra_zero_e_uno()` |

### `R/utils_cluster_viste.R`

`tabella_cluster_dt()`, `grafico_indicatori()`, `grafico_dispersione()`, `mappa_cluster()`, `popup_cluster()`. Ausiliarie: `lingua_dt()`, `inchiostro_grafici()`.

### Altri file

| File | Funzioni |
|---|---|
| `R/app_ui.R` | `app_ui()`, `sezione_sidebar()` per i riquadri della barra laterale, `golem_add_external_resources()` |
| `R/app_server.R` | `app_server()` |
| `R/mod_mappa_server.R` | `mod_mappa_server()`, `conta()` per singolari e plurali, ad esempio "1 bidone" |
| `R/mod_ricerca_ui.R` | `mod_ricerca_ui()`, `testi_ricerca()` con le etichette delle due ricerche |
| `R/generate_cluster_analysis.R` | `genera_analisi_cluster()`, `scrivi_analisi_cluster()` |

## Dove intervenire

| Per cambiare | Vai in | Test collegati |
|---|---|---|
| Servizi e giri riconosciuti, icone, colori, emoji | `tipologie_servizio()` in `R/utils_styling.R`. È l'unico punto | `test-utils_styling.R` |
| Verde e rosso dello stato, colori dei cluster di marker | `colori_stato()`. Il JavaScript in `js_icona_cluster()` li legge da lì | `test-utils_styling.R` |
| Forma, bordo o dimensione dei marker | `svg_marker()` e `get_leaflet_icon()` | `test-utils_styling.R` |
| Legenda della mappa | `html_legenda()` | `test-utils_styling.R` |
| Soglia dell'80% per l'icona dei non censiti | argomento `soglia` di `aggiungi_servizio_icona()`, chiamata in `R/mod_periodo_server.R` | `test-utils_data_processing.R` |
| Testo del popup dei marker | `create_popup_html()` | `test-utils_popup.R` |
| Casi e testi del dettaglio del bidone | `analizza_rfid()` per la logica, `crea_info_panel()` per i testi | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Riquadro delle statistiche | `calcola_statistiche()` e `crea_box_statistiche()` | `test-utils_data_processing.R`, `test-utils_popup.R` |
| Colonne del CSV, formati di data, regole di validazione | `colonne_obbligatorie()`, `colonne_facoltative()`, `converti_data_ora()`, `validate_dataset()` | `test-utils_data_processing.R` |
| Dimensione massima del file caricato | `options(shiny.maxRequestSize = ...)` in `R/app_server.R` | nessuno |
| Un filtro laterale nuovo o modificato | interfaccia in `R/mod_filtri_ui.R`, stato in `R/mod_filtri_server.R`, regola in `filtra_letture()` | `test-moduli.R`, `test-utils_data_processing.R` |
| Selezione del periodo, preimpostazioni | `R/mod_periodo_ui.R`, `R/mod_periodo_server.R`, `periodo_anno()`, `etichetta_periodo()` | `test-moduli.R` |
| Titolo in alto | `output$titolo` in `R/app_server.R` | nessuno |
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
| Dataset di esempio | `data-raw/genera_sample_dataset.R`, poi rigenerare i due file in `inst/extdata/` | `test-sample_dataset.R` |
| Dipendenze | `DESCRIPTION`, poi `devtools::document()` | `devtools::check()` |

Dopo una modifica alle regole dell'analisi dei cluster vanno aggiornati anche `docs/analisi_cluster.md` e `README.md`. Il report `docs/indice_fiducia.Rmd` contiene controlli che fermano la compilazione se il testo non corrisponde più ai dati.

## Comandi

```r
devtools::load_all()                        # carica il pacchetto dai sorgenti
run_app(demo = TRUE)                        # avvia l'app con il dataset di esempio
source("dev/run_dev.R")                     # avvio usato in sviluppo: rigenera la documentazione e avvia
devtools::test()                            # tutti i test
devtools::test(filter = "utils_cluster")    # solo i file di test che contengono quel testo
devtools::document()                        # rigenera NAMESPACE e man/
devtools::check()                           # controllo completo del pacchetto
rmarkdown::render("docs/indice_fiducia.Rmd")   # compila il report in HTML
```

```sh
# rigenera il dataset di esempio
Rscript data-raw/genera_sample_dataset.R
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
- **Licenza.** Il campo `License` di `DESCRIPTION` contiene un segnaposto. La scelta spetta al proprietario del progetto: non compilarlo.
- **Git.** I commit li fa il proprietario del progetto. Non committare se non viene chiesto.

## Trappole note

- **Mappe in schede nascoste.** Leaflet non disegna bene in un contenitore invisibile. Per questo `mod_mappa` aspetta che la scheda sia attiva. Una mappa nuova deve seguire lo stesso schema, oppure essere ridisegnata per intero con `renderLeaflet()` come fa la mappa dei cluster spaziali.
- **`summarise()` e nomi riusati.** Dentro un `dplyr::summarise()` una colonna appena creata nasconde quella originale nelle espressioni successive. I riepiloghi che servono la colonna originale vanno calcolati prima di sovrascriverla.
- **`dbscan::dbscan()` e matrici.** Una matrice viene letta come coordinate dei punti, non come distanze. Il progetto passa coordinate proiettate in metri.
- **Date nella tabella interattiva.** Le colonne di tipo `Date` slittano di un giorno con i formati data del browser. In `tabella_cluster_dt()` sono convertite in testo.
- **Dati di esempio citati per nome.** Test e report fanno riferimento a RFID e conteggi precisi. Cambiare il seme o le regole dello script del dataset li fa fallire: vanno aggiornati insieme.
- **Compilazione del report.** Va compilato dalla cartella `docs/`, senza l'opzione `intermediates_dir`, che produce avvisi spuri.

## Documenti collegati

| Documento | Contenuto |
|---|---|
| `README.md` | Uso dell'app, formato del CSV, differenze rispetto alle specifiche iniziali |
| `docs/analisi_cluster.md` | Analisi dei cluster spaziali: colonne, indicatori, indice di fiducia, scelte e limiti |
| `docs/indice_fiducia.Rmd` | Report sull'indice di fiducia, con esempi svolti e prove di efficacia |
