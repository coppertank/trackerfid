-- Filtro di Pandoc per il PDF (Typst) del report: tabelle leggibili con molti campi.
--
-- Si usa con `filters: [pdf_tabelle.lua]` nel formato `typst` di
-- `indicatori_letture.qmd`. Fa due cose.
--
-- 1. Larghezze delle colonne. Le tabelle di `knitr::kable()` sono tabelle "pipe" di
--    Markdown: Pandoc ne ricava larghezze relative dal numero di trattini della
--    riga di separazione e le scrive in Typst come percentuali fisse, e con molti
--    campi le colonne diventano troppo strette. Togliendo le larghezze, Typst
--    dimensiona ogni colonna sul suo contenuto e va a capo nelle intestazioni
--    solo se la tabella non sta nella pagina.
--
-- 2. Tabelle troppo larghe. Typst non spezza le parole: se la parola piu' lunga di
--    ogni colonna, messe insieme, non sta nella pagina, le intestazioni si
--    sovrappongono. In quel caso la tabella si divide in blocchi di colonne, uno
--    sotto l'altro, e la prima colonna (l'etichetta delle righe) si ripete in ogni
--    blocco. La stima usa la larghezza media di un carattere a 8 pt (lo stile in
--    `stile_pdf.typ`) e la larghezza utile di una pagina A4 orizzontale con
--    margini di 1,5 cm (757 pt, con un po' di respiro).

local PUNTI_PER_CARATTERE = 3.8   -- carattere a 8 pt, in media
local INSET_CELLA = 8             -- 4 pt a sinistra e a destra
local LARGHEZZA_UTILE = 740       -- punti

-- Lunghezza della parola piu' lunga di un testo (si va a capo anche dopo "/" e "-").
local function parola_piu_lunga(testo)
  local massima = 0
  for parola in testo:gmatch("[^%s/%-]+") do
    local n = utf8.len(parola) or #parola
    if n > massima then massima = n end
  end
  return massima
end

-- Tutte le righe di una tabella, intestazione, corpo e piede.
local function tutte_le_righe(tabella)
  local righe = {}
  for _, r in ipairs(tabella.head.rows) do righe[#righe + 1] = r end
  for _, corpo in ipairs(tabella.bodies) do
    for _, r in ipairs(corpo.head) do righe[#righe + 1] = r end
    for _, r in ipairs(corpo.body) do righe[#righe + 1] = r end
  end
  for _, r in ipairs(tabella.foot.rows) do righe[#righe + 1] = r end
  return righe
end

-- Larghezza minima di ogni colonna; nil se ci sono celle che occupano piu' colonne
-- o righe (non e' il caso delle tabelle del report: allora non si divide).
local function larghezze_minime(tabella)
  local n = #tabella.colspecs
  local minime = {}
  for i = 1, n do minime[i] = 0 end
  for _, riga in ipairs(tutte_le_righe(tabella)) do
    if #riga.cells ~= n then return nil end
    for i, cella in ipairs(riga.cells) do
      if cella.col_span ~= 1 or cella.row_span ~= 1 then return nil end
      local larghezza = parola_piu_lunga(pandoc.utils.stringify(cella.contents))
        * PUNTI_PER_CARATTERE + INSET_CELLA
      if larghezza > minime[i] then minime[i] = larghezza end
    end
  end
  return minime
end

-- Gruppi di colonne: la prima sempre, poi quante ne stanno nella pagina.
local function gruppi_di_colonne(minime)
  local gruppi = {}
  local corrente, somma = { 1 }, minime[1]
  for i = 2, #minime do
    if somma + minime[i] > LARGHEZZA_UTILE and #corrente > 1 then
      gruppi[#gruppi + 1] = corrente
      corrente, somma = { 1 }, minime[1]
    end
    corrente[#corrente + 1] = i
    somma = somma + minime[i]
  end
  gruppi[#gruppi + 1] = corrente
  return gruppi
end

local function scegli_celle(riga, indici)
  local celle = {}
  for _, i in ipairs(indici) do celle[#celle + 1] = riga.cells[i] end
  return pandoc.Row(celle, riga.attr)
end

local function scegli_righe(righe, indici)
  local nuove = {}
  for _, r in ipairs(righe) do nuove[#nuove + 1] = scegli_celle(r, indici) end
  return nuove
end

-- La tabella ristretta alle colonne indicate.
local function sotto_tabella(tabella, indici)
  local colonne = {}
  for _, i in ipairs(indici) do colonne[#colonne + 1] = tabella.colspecs[i] end
  local corpi = {}
  for _, corpo in ipairs(tabella.bodies) do
    corpi[#corpi + 1] = pandoc.TableBody(
      scegli_righe(corpo.body, indici),
      scegli_righe(corpo.head, indici),
      math.min(corpo.row_head_columns, #indici),
      corpo.attr
    )
  end
  return pandoc.Table(
    tabella.caption,
    colonne,
    pandoc.TableHead(scegli_righe(tabella.head.rows, indici), tabella.head.attr),
    corpi,
    pandoc.TableFoot(scegli_righe(tabella.foot.rows, indici), tabella.foot.attr),
    tabella.attr
  )
end

function Table(tabella)
  -- larghezze al default: le decide Typst sul contenuto
  for i, colonna in ipairs(tabella.colspecs) do
    tabella.colspecs[i] = { colonna[1] }
  end
  local minime = larghezze_minime(tabella)
  if minime == nil or #minime < 2 then
    return tabella
  end
  local gruppi = gruppi_di_colonne(minime)
  if #gruppi == 1 then
    return tabella
  end
  local blocchi = {}
  for _, indici in ipairs(gruppi) do
    blocchi[#blocchi + 1] = sotto_tabella(tabella, indici)
  end
  return blocchi
end
