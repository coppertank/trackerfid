# Passo 5, facoltativo: report sui conferimenti del secco oltre il limite.
#
# Compila docs/conferimenti_secco.Rmd sul CSV del passo 3 e scrive in
# data-raw/output/ il report in HTML e tre CSV: tutte le forniture, quelle da
# contattare e i bidoni non censiti del secco. Tutto resta fuori dal
# repository: contiene codici di forniture e utenze.
#
# Il CSV deve avere le colonne della fornitura aggiunte dal passo 3 con
# associa_nucleo_e_manovra(): nucleo, categoria e manovra sociale. Un CSV
# preparato prima va rigenerato con il passo 3. L'archivio .duckdb non basta:
# non contiene quelle colonne.
#
# Eseguire dalla radice del progetto. Servono i pacchetti rmarkdown, ggplot2
# e pkgload.

# Periodo dell'analisi, "AAAA-MM-GG". Vuoti: dal primo gennaio dell'anno
# dell'ultima lettura all'ultima lettura; l'analisi parte comunque dal primo
# giorno con letture.
dal <- ""
al <- ""

letture <- "data-raw/output/letture_app.csv"
cartella <- "data-raw/output"

if (!file.exists(letture)) {
  stop(
    "Manca ", letture, ": eseguire prima il passo 3, ",
    "source(\"data-raw/03_crea_letture_app.R\").",
    call. = FALSE
  )
}

# Percorsi assoluti: il report si compila dalla cartella docs/.
parametri <- list(
  letture = normalizePath(letture),
  dal = dal,
  al = al,
  cartella_csv = normalizePath(cartella)
)
nome <- sprintf("conferimenti_secco_%s.html", format(Sys.Date(), "%Y-%m-%d"))
rmarkdown::render(
  "docs/conferimenti_secco.Rmd",
  params = parametri,
  output_file = nome,
  output_dir = parametri$cartella_csv,
  quiet = TRUE
)
message("Report e CSV scritti in ", cartella, ": ", nome)
