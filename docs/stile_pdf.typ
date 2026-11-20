// Stile del PDF del report (formato `typst` di indicatori_letture.qmd).
//
// Pagina orizzontale: le tabelle con molti campi (la propensione ne ha 15) hanno
// piu' spazio. Il resto del documento segue le impostazioni del formato (margini,
// carattere) in `indicatori_letture.qmd`.
#set page(flipped: true)

// Il testo corre a sinistra: un testo giustificato in una pagina larga lascia
// spazi irregolari tra le parole, specie accanto a un nome di campo in monospazio.
#show par: set par(justify: false)

// Tabelle: carattere piu' piccolo, niente sillabazione nelle celle (un numero o
// un'intestazione spezzata a meta' e' illeggibile), righe a fasce alterne e
// intestazione che si ripete quando la tabella continua nella pagina dopo.
#show table: set text(size: 8pt, hyphenate: false)
#show table: set par(justify: false)
#show table: set table(
  inset: (x: 4pt, y: 3pt),
  stroke: none,
  fill: (x, y) => if y > 0 and calc.even(y) { luma(244) },
)
