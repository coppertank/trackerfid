# Misura i tempi e la memoria di ogni passaggio dell'app su un file di letture.
#
# Uso, dalla radice del pacchetto:
#   Rscript dev/misura_prestazioni.R <file delle letture>
#
# Accetta gli stessi formati dell'app: CSV, CSV compresso con gzip, Parquet,
# DuckDB. Per un file di prova: Rscript data-raw/genera_dataset_grande.R <cartella>
#
# I tempi sono quelli del solo server: a questi si aggiunge il disegno nel
# browser, che dipende dal peso del messaggio riportato per la mappa.

argomenti <- commandArgs(TRUE)
if (length(argomenti) < 1) {
  stop("Indica il file delle letture.")
}
suppressMessages(pkgload::load_all(".", quiet = TRUE))

misure <- data.frame(
  passaggio = character(0),
  secondi = numeric(0),
  picco_mb = numeric(0)
)
# Esegue un passaggio, ne registra durata e picco di memoria e ne restituisce
# il risultato.
misura <- function(passaggio, espressione) {
  invisible(gc(reset = TRUE))
  secondi <- system.time(valore <- force(espressione))[["elapsed"]]
  uso <- gc()
  misure[nrow(misure) + 1, ] <<- list(passaggio, secondi, sum(uso[, ncol(uso)]))
  invisible(valore)
}
megabyte <- function(x) round(as.numeric(utils::object.size(x)) / 1024^2)

file <- argomenti[1]
message(sprintf("%s, %.0f MB", basename(file), file.size(file) / 1024^2))

# ---- Caricamento -----------------------------------------------------------------
esito <- misura("Lettura e validazione in DuckDB", carica_archivio(file))
archivio <- esito$archivio
message(sprintf(
  "%s letture, %s RFID, %d avvisi",
  formatta_numero(archivio$n),
  formatta_numero(archivio$n_rfid),
  length(esito$avvisi)
))
if (tolower(tools::file_ext(file)) != "duckdb") {
  # Lo stesso archivio scritto in un file .duckdb, come fa il passo 3, e
  # riaperto: è il caricamento dell'app con il file del passo 3.
  file_duckdb <- tempfile(fileext = ".duckdb")
  misura("Scrittura del file .duckdb", scrivi_archivio(archivio, file_duckdb))
  message(sprintf("  file .duckdb di %.0f MB", file.size(file_duckdb) / 1024^2))
  riaperto <- misura("Apertura del file .duckdb", carica_archivio(file_duckdb))
  chiudi_archivio(riaperto$archivio)
  unlink(file_duckdb)
}

# ---- Periodo, filtri e mappa principale --------------------------------------------
anno <- max(anni_disponibili(archivio))
periodo <- periodo_anno(anno)
selezione <- misura(
  "Periodo: conteggi e stima dei non censiti",
  seleziona_periodo(archivio, periodo)
)
scelte <- misura("Voci dei filtri", scelte_periodo(selezione))
ultime <- misura("Ultima lettura per RFID, senza filtri", ultime_letture(selezione))
giro <- scelte$giri[1]
misura("Cambio di un filtro, in tutto", {
  ultime_letture(selezione, giri_esclusi = giro, coerenza_escluse = "coerente")
  conteggi_cantieri(selezione, giri_esclusi = giro, coerenza_escluse = "coerente")
})
# Il servizio: per i censiti quello a database, per i non censiti quello
# stimato dai giri. Un servizio in meno e senza i non censiti con la stima
# incerta.
misura(
  "Cambio del filtro sul servizio, in tutto",
  {
    ultime_letture(
      selezione,
      servizi_esclusi = scelte$servizi[1],
      includi_stima_incerta = FALSE
    )
    conteggi_cantieri(
      selezione,
      servizi_esclusi = scelte$servizi[1],
      includi_stima_incerta = FALSE
    )
  }
)
misura(
  "Cambio del comune, con l'area da inquadrare",
  {
    comune <- scelte$comuni$comune_assegnato[1]
    ultime_letture(selezione, comuni = comune)
    area_filtri(selezione, comuni = comune)
  }
)
misura(
  "Statistiche",
  calcola_statistiche(ultime, conteggi_cantieri(selezione))
)

# La mappa: bidoni e bolle vanno al browser in un messaggio solo, poi zoom e
# spostamenti non chiedono niente al server.
dati <- misura("Mappa: dati per il browser", dati_mappa(ultime, "cluster"))
messaggio <- jsonlite::toJSON(
  dati,
  auto_unbox = TRUE,
  null = "null",
  na = "null",
  digits = I(16),
  use_signif = TRUE,
  force = TRUE
)
message(sprintf(
  "  %s bidoni, messaggio al browser %.2f MB",
  formatta_numero(length(dati$punti$x)),
  nchar(messaggio) / 1024^2
))

# ---- Ricerca, dettaglio, analisi ---------------------------------------------------
rfid <- interroga(
  archivio,
  sprintf(
    "SELECT RFID FROM %s WHERE %s GROUP BY RFID ORDER BY count(*) DESC, RFID LIMIT 1",
    archivio$tabella,
    sql_periodo(selezione)
  )
)$RFID
trovate <- misura("Ricerca di un RFID", cerca_per_rfid(selezione, rfid))
message(sprintf(
  "  l'RFID con piu' letture ne ha %s: sulla mappa della ricerca ne vanno %s",
  formatta_numero(nrow(trovate)),
  formatta_numero(nrow(limita_letture_ricerca(trovate)))
))
misura("Dettaglio di un bidone", {
  crea_info_panel(analizza_rfid(letture_del_rfid(selezione, rfid)))
})
mezzi <- mezzi_presenti(selezione)
giorni <- giorni_letture(selezione)
misura("Ricerca dei giri: un mezzo in un giorno, con il riepilogo", {
  riepilogo_giri(cerca_giri(selezione, mezzi[1], max(giorni), max(giorni)))
})
misura("Ricerca dei giri: tutti i mezzi in un mese, con il riepilogo", {
  riepilogo_giri(cerca_giri(selezione, NULL, max(giorni) - 30, max(giorni)))
})
letture <- misura("Analisi dei cluster: letture del periodo", letture_periodo(selezione))
analisi <- misura(
  "Analisi dei cluster",
  calcola_analisi_cluster(
    letture,
    periodo[1],
    periodo[2],
    copertura = c(archivio$primo, archivio$ultimo)
  )
)
message(sprintf("  %s cluster", formatta_numero(nrow(analisi))))

misure$secondi <- round(misure$secondi, 2)
misure$picco_mb <- round(misure$picco_mb)
print(misure, row.names = FALSE, right = FALSE)
