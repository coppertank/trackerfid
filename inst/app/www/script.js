// Porta in vista un elemento della barra laterale, ad esempio il dettaglio del
// bidone dopo il click su un marker.
$(document).on("shiny:connected", function () {
  Shiny.addCustomMessageHandler("trackerfid-mostra", function (id) {
    window.setTimeout(function () {
      var elemento = document.getElementById(id);
      if (elemento) {
        elemento.scrollIntoView({ behavior: "smooth", block: "nearest" });
      }
    }, 100);
  });
});

// Una mappa nata in una scheda nascosta non conosce la sua misura, e una che
// riceve un inquadramento mentre è nascosta lo rimanda: tutte e due
// aspettano un ridimensionamento, che chiediamo qui a ogni scheda che torna
// visibile. Lo stesso gestore riceve anche l'evento "shown" con cui Shiny
// mostra un riquadro condizionale.
$(document).on("shown.bs.tab", function () {
  if (window.TrackerfidMappa) {
    window.TrackerfidMappa.ridimensiona();
  }
});

// I menu dei filtri sopra la mappa principale restano aperti mentre si
// spuntano le voci: un click dentro il riquadro non arriva al documento, che
// chiuderebbe il menu.
$(document).on("click", ".filtro-menu", function (evento) {
  evento.stopPropagation();
});

// Un menu con qualche voce tolta lo dice sul pulsante, con il numero delle
// voci rimaste. Shiny lancia "change" anche quando il server aggiorna le voci.
$(document).on("change", ".filtro-mappa", function () {
  var caselle = $(this).find(".filtro-menu input[type=checkbox]");
  var spuntate = caselle.filter(":checked").length;
  var attivo = caselle.length > 0 && spuntate < caselle.length;
  $(this).toggleClass("filtro-attivo", attivo);
  $(this)
    .find(".filtro-conteggio")
    .text(attivo ? spuntate + "/" + caselle.length : "");
});

// La legenda delle mappe nasce chiusa a ogni avvio e si apre con un click
// sull'intestazione. La scelta vale per tutte le mappe della pagina, ma non
// viene ricordata: al prossimo avvio la legenda è di nuovo chiusa.
$(document).on("click", ".legenda-intestazione", function (evento) {
  evento.preventDefault();
  var aperta = !this.parentNode.open;
  document.querySelectorAll(".legenda-dettagli").forEach(function (d) {
    d.open = aperta;
  });
});
