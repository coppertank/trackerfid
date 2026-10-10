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
