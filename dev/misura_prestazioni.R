# Misura i tempi e la memoria di ogni passaggio dell'app su un file di letture.
#
# Uso, dalla radice del pacchetto:
#   Rscript dev/misura_prestazioni.R <file delle letture> [file delle letture storiche]
#
# Accetta gli stessi formati dell'app: CSV, CSV compresso con gzip, Parquet.
# Per un file di prova: Rscript data-raw/genera_dataset_grande.R <cartella>
#
# I tempi sono quelli del solo server: a questi si aggiunge il disegno nel
# browser, che dipende dal peso del messaggio riportato per ogni vista.

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
grezzo <- misura("Lettura del file", leggi_tabella(file))
esito <- misura("Validazione", validate_dataset(grezzo))
rm(grezzo)
letture <- esito$dati
n_rfid <- dplyr::n_distinct(letture$RFID)
message(sprintf(
  "%s letture, %s RFID, %d MB in memoria, %d avvisi",
  formatta_numero(nrow(letture)),
  formatta_numero(n_rfid),
  megabyte(letture),
  length(esito$avvisi)
))

# ---- Periodo, filtri e mappa principale --------------------------------------------
anno <- max(anni_disponibili(letture))
periodo <- periodo_anno(anno)
nel_periodo <- misura(
  "Filtro del periodo e icone",
  aggiungi_servizio_icona(filtra_periodo(letture, periodo))
)
maschera <- misura("Filtri: letture che li superano", maschera_filtri(nel_periodo))
ultime <- misura(
  "Filtri: ultima lettura per RFID",
  deduplica_ultimo_rfid(nel_periodo, maschera)
)
servizio <- ordina_servizi(nel_periodo$servizio_transponder)[1]
misura(
  "Cambio di un filtro, in tutto",
  deduplica_ultimo_rfid(
    nel_periodo,
    maschera_filtri(nel_periodo, transponder_esclusi = servizio)
  )
)
# I non censiti si filtrano con il servizio stimato dai giri: le voci
# dell'elenco, poi un servizio in meno e senza quelli con la stima incerta.
stimati <- misura(
  "Elenco dei servizi stimati dei non censiti",
  ordina_servizi(nel_periodo$servizio_icona[
    nel_periodo$presente_a_database == "Non Presente"
  ])
)
misura(
  "Cambio del filtro sui non censiti, in tutto",
  deduplica_ultimo_rfid(
    nel_periodo,
    maschera_filtri(
      nel_periodo,
      atteso_esclusi = stimati[1],
      includi_stima_incerta = FALSE
    )
  )
)
misura(
  "Statistiche",
  calcola_statistiche(ultime, nel_periodo$cantiere[maschera])
)

# La mappa: che cosa disegna a tre zoom, e quanto pesa il messaggio al browser.
zona <- riquadro_zona()
tutta <- list(
  north = zona$lat_max,
  south = zona$lat_min,
  east = zona$lng_max,
  west = zona$lng_min
)
centro <- c(stats::median(ultime$latitudine), stats::median(ultime$longitudine))
viste <- list(
  "zoom 9, tutta la zona" = list(riquadro = tutta, zoom = 9),
  "zoom 12, un'area di 30 km" = list(
    riquadro = list(
      north = centro[1] + 0.10,
      south = centro[1] - 0.10,
      east = centro[2] + 0.20,
      west = centro[2] - 0.20
    ),
    zoom = 12
  ),
  "zoom 15, un quartiere" = list(
    riquadro = list(
      north = centro[1] + 0.012,
      south = centro[1] - 0.012,
      east = centro[2] + 0.025,
      west = centro[2] - 0.025
    ),
    zoom = 15
  )
)
for (nome in names(viste)) {
  vista <- viste[[nome]]
  mappa <- misura(paste("Mappa:", nome), {
    scelta <- scegli_vista(ultime, vista$riquadro, vista$zoom)
    if (scelta$tipo %in% c("tutti", "singoli")) {
      righe <- if (scelta$tipo == "tutti") ultime else ultime[scelta$righe, ]
      disegna_marker(mappa_base(), righe, "cluster")
    } else {
      righe <- if (scelta$tipo == "griglia") ultime[scelta$righe, ] else ultime
      aggiungi_bolle(mappa_base(), aggrega_marker(righe, scelta$tipo, vista$zoom))
    }
  })
  messaggio <- jsonlite::toJSON(mappa$x$calls, auto_unbox = TRUE, force = TRUE)
  message(sprintf(
    "  %-28s vista '%s', messaggio al browser %.2f MB",
    nome,
    scegli_vista(ultime, vista$riquadro, vista$zoom)$tipo,
    nchar(messaggio) / 1024^2
  ))
}

# ---- Ricerca, dettaglio, analisi ---------------------------------------------------
rfid <- names(sort(table(nel_periodo$RFID), decreasing = TRUE))[1]
trovate <- misura("Ricerca di un RFID", cerca_per_rfid(nel_periodo, rfid))
message(sprintf(
  "  l'RFID con piu' letture ne ha %s: sulla mappa della ricerca ne vanno %s",
  formatta_numero(nrow(trovate)),
  formatta_numero(nrow(limita_letture_ricerca(trovate)))
))
misura("Dettaglio di un bidone", {
  analizza_rfid(nel_periodo[nel_periodo$RFID == rfid, ])
  andamento_rfid(rfid, letture, NULL)
})
analisi <- misura(
  "Analisi dei cluster",
  calcola_analisi_cluster(letture, periodo[1], periodo[2])
)
message(sprintf("  %s cluster", formatta_numero(nrow(analisi))))

# ---- Letture storiche e confronto del report sul beneficio delle antenne ------------
if (length(argomenti) >= 2) {
  storico <- misura("Letture storiche: lettura e validazione", {
    carica_storico(argomenti[2])$dati
  })
  message(sprintf("  %s letture storiche", formatta_numero(nrow(storico))))
  confronto <- misura("Confronto tra i due sistemi", {
    confronto_sistemi(unisci_letture(letture, storico), anagrafica_rfid(letture))
  })
  misura("Riepilogo per territorio, cantiere e comune", {
    riepilogo_confronto(confronto)
    riepilogo_confronto(confronto, "cantiere")
    riepilogo_confronto(confronto, "comune")
  })
}

misure$secondi <- round(misure$secondi, 2)
misure$picco_mb <- round(misure$picco_mb)
print(misure, row.names = FALSE, right = FALSE)
