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
