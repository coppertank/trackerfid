#!/usr/bin/env Rscript

# Compila il report sul beneficio delle antenne a tutti i livelli: l'intero
# territorio, un report per cantiere e uno per comune.
#
# Uso, dalla radice del progetto:
#
#   Rscript docs/compila_report.R
#       sui dati di esempio
#   Rscript docs/compila_report.R <letture> <storico> [cartella] [opzioni]
#       sui file indicati, con il percorso dalla radice del progetto
#
# Opzioni:
#   --servizio=Secco      tipologia di rifiuto del confronto. Vuota: tutte
#   --livelli=territorio,cantiere,comune
#                         quali report compilare
#   --minimo=5            contenitori del servizio che un comune deve avere
#                         perché il suo report venga compilato
#
# La cartella predefinita è data-raw/output/report, esclusa da git: sui dati
# reali gli HTML contengono dati aziendali e devono restare lì.
#
# Servono i pacchetti rmarkdown, ggplot2 e pkgload.

if (!file.exists("DESCRIPTION") || !file.exists("docs/beneficio_antenne.Rmd")) {
  stop("Esegui lo script dalla radice del progetto trackerfid.", call. = FALSE)
}

argomenti <- commandArgs(trailingOnly = TRUE)
con_nome <- grepl("^--", argomenti)
posizionali <- argomenti[!con_nome]
opzione <- function(nome, predefinito) {
  prefisso <- paste0("--", nome, "=")
  trovata <- argomenti[startsWith(argomenti, prefisso)]
  if (length(trovata) == 0) {
    return(predefinito)
  }
  sub(prefisso, "", trovata[length(trovata)], fixed = TRUE)
}
if (length(posizionali) == 1) {
  stop(
    "Indica entrambi i file, letture e storico, oppure nessuno dei due.",
    call. = FALSE
  )
}
file_letture <- if (length(posizionali) >= 2) posizionali[1] else ""
file_storico <- if (length(posizionali) >= 2) posizionali[2] else ""
cartella <- if (length(posizionali) >= 3) {
  posizionali[3]
} else {
  "data-raw/output/report"
}
servizio <- opzione("servizio", "Secco")
livelli <- strsplit(opzione("livelli", "territorio,cantiere,comune"), ",")[[1]]
minimo <- as.integer(opzione("minimo", "5"))
sconosciuti <- setdiff(livelli, c("territorio", "cantiere", "comune"))
if (length(sconosciuti) > 0) {
  stop(
    "Livelli non riconosciuti: ",
    paste(sconosciuti, collapse = ", "),
    call. = FALSE
  )
}

suppressMessages(pkgload::load_all(".", quiet = TRUE))

# I file si leggono una volta sola: il report li riceve già validati.
percorso_letture <- if (nzchar(file_letture)) {
  file_letture
} else {
  percorso_dataset_esempio()
}
percorso_storico <- if (nzchar(file_storico)) {
  file_storico
} else {
  percorso_storico_esempio()
}
message("Lettura dei file...")
letture <- carica_dataset(percorso_letture)$dati
storico <- carica_storico(percorso_storico)$dati
options(
  trackerfid.report_dati = list(
    file_letture = file_letture,
    file_storico = file_storico,
    letture = letture,
    storico = storico
  )
)

# Cantieri e comuni con contenitori del servizio nella coorte: quelli letti da
# tutti e due i sistemi.
anagrafica <- anagrafica_rfid(senza_sacchetti(letture))
anagrafica <- anagrafica[anagrafica$RFID %in% storico$RFID, , drop = FALSE]
if (nzchar(servizio)) {
  anagrafica <- anagrafica[
    tolower(anagrafica$tipologia) %in% tolower(servizio),
    ,
    drop = FALSE
  ]
}
if (nrow(anagrafica) == 0) {
  stop(
    "Nessun contenitore del servizio indicato è stato letto da tutti e due ",
    "i sistemi.",
    call. = FALSE
  )
}
per_comune <- table(anagrafica$comune)
comuni <- names(per_comune)[per_comune >= minimo]
pochi <- names(per_comune)[per_comune < minimo]

da_compilare <- rbind(
  if ("territorio" %in% livelli) {
    data.frame(livello = "territorio", cantiere = "", comune = "")
  },
  if ("cantiere" %in% livelli) {
    data.frame(
      livello = "cantiere",
      cantiere = sort(unique(stats::na.omit(anagrafica$cantiere))),
      comune = ""
    )
  },
  if ("comune" %in% livelli && length(comuni) > 0) {
    data.frame(livello = "comune", cantiere = "", comune = sort(comuni))
  }
)
# Nome del file: senza spazi, accenti e apostrofi.
nome_file <- function(livello, nome) {
  pulito <- gsub("[^A-Za-z0-9]+", "_", iconv(nome, to = "ASCII//TRANSLIT"))
  pulito <- gsub("^_+|_+$", "", pulito)
  paste0(
    "beneficio_antenne_",
    livello,
    if (nzchar(pulito)) paste0("_", pulito),
    ".html"
  )
}
da_compilare$file <- mapply(
  nome_file,
  da_compilare$livello,
  paste0(da_compilare$cantiere, da_compilare$comune)
)

dir.create(cartella, recursive = TRUE, showWarnings = FALSE)
cartella <- normalizePath(cartella)
message(sprintf(
  "Compilo %d report in %s",
  nrow(da_compilare),
  cartella
))

da_compilare$esito <- vapply(
  seq_len(nrow(da_compilare)),
  function(i) {
    riga <- da_compilare[i, ]
    esito <- tryCatch(
      {
        rmarkdown::render(
          "docs/beneficio_antenne.Rmd",
          params = list(
            letture = file_letture,
            storico = file_storico,
            cantiere = riga$cantiere,
            comune = riga$comune,
            servizio = servizio
          ),
          output_dir = cartella,
          output_file = riga$file,
          envir = new.env(),
          quiet = TRUE
        )
        "compilato"
      },
      error = function(e) paste("non compilato:", conditionMessage(e))
    )
    message(sprintf("  %-60s %s", riga$file, esito))
    esito
  },
  character(1)
)

if (length(pochi) > 0 && "comune" %in% livelli) {
  message(sprintf(
    "Comuni senza report, con meno di %d contenitori del servizio: %s",
    minimo,
    paste(sort(pochi), collapse = ", ")
  ))
}
falliti <- sum(da_compilare$esito != "compilato")
message(sprintf(
  "Report compilati: %d su %d",
  nrow(da_compilare) - falliti,
  nrow(da_compilare)
))
if (falliti > 0) {
  quit(status = 1)
}
