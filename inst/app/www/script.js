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

// Una mappa che riceve i dati mentre è nascosta li tiene in attesa di un
// ridimensionamento. Quando torna in vista con la misura di prima il
// ridimensionamento non arriva, e la mappa resterebbe grigia: lo chiediamo
// qui, a ogni scheda che torna visibile. Lo stesso gestore riceve anche
// l'evento "shown" con cui Shiny mostra un riquadro condizionale.
$(document).on("shown.bs.tab", function () {
  $(".leaflet.html-widget").each(function () {
    if (this.offsetWidth === 0 || this.offsetHeight === 0) {
      return;
    }
    var mappa = window.HTMLWidgets && HTMLWidgets.find("#" + this.id);
    if (mappa && mappa.resize) {
      mappa.resize(this.offsetWidth, this.offsetHeight);
    }
  });
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
