// Mappe WebGL dell'app (MapLibre GL JS, attraverso mapgl).
//
// R disegna la mappa di base e la configurazione: icone, sorgenti, livelli,
// livelli che rispondono al mouse. I dati arrivano dopo, con i messaggi
// "trackerfid-mappa": questo file li trasforma in GeoJSON e li mette nelle
// sorgenti. Zoom, spostamenti, cluster e passaggio tra le fasce di zoom
// restano nel browser, senza chiedere niente al server.
(function () {
  "use strict";

  // Stato di ogni mappa, per identificativo dell'elemento.
  var mappe = {};
  // Messaggi arrivati prima che la mappa esistesse: di ogni tipo l'ultimo.
  var inAttesa = {};

  // jsonlite scrive un vettore di un elemento come valore singolo.
  function elenco(valore) {
    if (valore === undefined || valore === null) {
      return [];
    }
    return Array.isArray(valore) ? valore : [valore];
  }

  function collezione(elementi) {
    return { type: "FeatureCollection", features: elementi };
  }

  // Disegna le icone dei marker dai loro SVG, al doppio della risoluzione.
  function caricaIcone(map, icone) {
    var id = elenco(icone.id);
    var svg = elenco(icone.svg);
    return Promise.all(
      id.map(function (nome, j) {
        if (map.hasImage(nome)) {
          return Promise.resolve();
        }
        return new Promise(function (fatto) {
          var doppio = svg[j].replace(
            /width="([\d.]+)" height="([\d.]+)"/,
            function (_, l, a) {
              return 'width="' + 2 * l + '" height="' + 2 * a + '"';
            }
          );
          var immagine = new Image();
          immagine.onload = function () {
            if (!map.hasImage(nome)) {
              map.addImage(nome, immagine, { pixelRatio: 2 });
            }
            fatto();
          };
          immagine.onerror = function () {
            fatto();
          };
          immagine.src =
            "data:image/svg+xml;charset=utf-8," + encodeURIComponent(doppio);
        });
      })
    );
  }

  function primoLivelloDiTesto(map) {
    var livelli = map.getStyle().layers;
    for (var i = 0; i < livelli.length; i++) {
      if (livelli[i].type === "symbol") {
        return livelli[i].id;
      }
    }
    return undefined;
  }

  // Sorgenti e livelli della configurazione. I confini stanno sotto le
  // etichette dello sfondo, il resto sopra tutto.
  function aggiungiLivelli(stato) {
    var map = stato.map;
    var config = stato.config;
    Object.keys(config.sorgenti).forEach(function (nome) {
      var sorgente = Object.assign({}, config.sorgenti[nome]);
      if (sorgente.data === "confini") {
        sorgente.data = config.confini;
      }
      map.addSource(nome, sorgente);
    });
    var etichette = primoLivelloDiTesto(map);
    config.livelli.forEach(function (definizione) {
      var livello = Object.assign({}, definizione);
      var sotto = livello.sotto_etichette;
      delete livello.sotto_etichette;
      map.addLayer(livello, sotto ? etichette : undefined);
    });
  }

  function testoAlPassaggio(elemento, regola) {
    var p = elemento.properties || {};
    if (regola.click === "cluster") {
      return (
        p.point_count.toLocaleString("it-IT") +
        " bidoni, " +
        p.censiti.toLocaleString("it-IT") +
        " censiti"
      );
    }
    return p[regola.testo];
  }

  function trovaElemento(stato, punto) {
    var livelli = Object.keys(stato.config.interattivi || {}).filter(
      function (id) {
        return stato.map.getLayer(id);
      }
    );
    if (livelli.length === 0) {
      return null;
    }
    var trovati = stato.map.queryRenderedFeatures(punto, { layers: livelli });
    return trovati.length > 0 ? trovati[0] : null;
  }

  function aggiungiEventi(stato) {
    var map = stato.map;
    var passaggio = new maplibregl.Popup({
      closeButton: false,
      closeOnClick: false,
      className: "tooltip-mappa",
      offset: 14
    });
    map.on("mousemove", function (e) {
      var elemento = trovaElemento(stato, e.point);
      if (!elemento) {
        map.getCanvas().style.cursor = "";
        passaggio.remove();
        return;
      }
      map.getCanvas().style.cursor = "pointer";
      var testo = testoAlPassaggio(
        elemento,
        stato.config.interattivi[elemento.layer.id]
      );
      if (testo) {
        passaggio.setLngLat(e.lngLat).setText(testo).addTo(map);
      } else {
        passaggio.remove();
      }
    });
    map.getCanvas().addEventListener("mouseleave", function () {
      passaggio.remove();
    });
    map.on("click", function (e) {
      var elemento = trovaElemento(stato, e.point);
      if (!elemento) {
        return;
      }
      passaggio.remove();
      var regola = stato.config.interattivi[elemento.layer.id];
      if (regola.click === "punto") {
        scegliPunto(stato, elemento.properties.i, elemento.geometry.coordinates);
      } else if (regola.click === "cluster") {
        apriCluster(stato, elemento);
      } else if (regola.click === "bolla") {
        apriBolla(stato, elemento);
      }
    });
  }

  // Un bidone scelto: il server apre il dettaglio e manda il popup.
  function scegliPunto(stato, indice, coordinate) {
    if (!window.Shiny || indice === undefined || indice === null) {
      return;
    }
    Shiny.setInputValue(
      stato.id + "_clic",
      {
        i: indice,
        lng: coordinate[0],
        lat: coordinate[1],
        nonce: Math.random()
      },
      { priority: "event" }
    );
  }

  // Un cluster si apre ingrandendo; uno che non si divide più, perché i
  // suoi bidoni stanno nello stesso punto, mostra l'elenco dei bidoni.
  function apriCluster(stato, elemento) {
    var map = stato.map;
    var sorgente = map.getSource(elemento.source);
    var id = elemento.properties.cluster_id;
    var centro = elemento.geometry.coordinates;
    var massimo = stato.config.sorgenti[elemento.source].clusterMaxZoom;
    sorgente.getClusterExpansionZoom(id).then(function (zoom) {
      if (zoom <= massimo) {
        map.easeTo({ center: centro, zoom: zoom });
        return;
      }
      sorgente.getClusterLeaves(id, 50, 0).then(function (foglie) {
        elencoCluster(stato, centro, foglie, elemento.properties.point_count);
      });
    });
  }

  function elencoCluster(stato, centro, foglie, totale) {
    var contenitore = document.createElement("div");
    contenitore.className = "elenco-cluster";
    var titolo = document.createElement("b");
    titolo.textContent = totale.toLocaleString("it-IT") + " bidoni nello stesso punto";
    contenitore.appendChild(titolo);
    var lista = document.createElement("ul");
    foglie.forEach(function (foglia) {
      var voce = document.createElement("li");
      var collegamento = document.createElement("a");
      collegamento.href = "#";
      collegamento.textContent = foglia.properties.r;
      collegamento.addEventListener("click", function (evento) {
        evento.preventDefault();
        scegliPunto(stato, foglia.properties.i, foglia.geometry.coordinates);
      });
      voce.appendChild(collegamento);
      lista.appendChild(voce);
    });
    contenitore.appendChild(lista);
    if (totale > foglie.length) {
      var altri = document.createElement("div");
      altri.textContent = "e altri " + (totale - foglie.length);
      contenitore.appendChild(altri);
    }
    apriPopup(stato, centro, contenitore);
  }

  // Una bolla si apre: ingrandisce sui suoi bidoni, almeno fino al livello
  // successivo, i comuni per un cantiere e i bidoni per un comune.
  function apriBolla(stato, elemento) {
    var map = stato.map;
    var p = elemento.properties;
    var minimo = stato.config.zoom_bolle[p.livello];
    var camera = map.cameraForBounds(
      [
        [p.ovest - 0.001, p.sud - 0.001],
        [p.est + 0.001, p.nord + 0.001]
      ],
      { padding: 40, maxZoom: stato.config.zoom_massimo }
    );
    if (!camera || camera.zoom < minimo) {
      map.easeTo({ center: elemento.geometry.coordinates, zoom: minimo });
    } else {
      map.easeTo({ center: camera.center, zoom: camera.zoom });
    }
  }

  function apriPopup(stato, coordinate, contenuto) {
    if (stato.popup) {
      stato.popup.remove();
    }
    stato.popup = new maplibregl.Popup({
      maxWidth: "340px",
      className: "popup-mappa"
    }).setLngLat(coordinate);
    if (typeof contenuto === "string") {
      stato.popup.setHTML(contenuto);
    } else {
      stato.popup.setDOMContent(contenuto);
    }
    stato.popup.addTo(stato.map);
  }

  // --- Dati ------------------------------------------------------------------

  function collezionePunti(punti, icone) {
    var x = elenco(punti.x);
    var y = elenco(punti.y);
    var k = elenco(punti.k);
    var c = elenco(punti.c);
    var r = elenco(punti.r);
    var u = punti.u === undefined ? null : elenco(punti.u);
    var elementi = new Array(x.length);
    for (var j = 0; j < x.length; j++) {
      var proprieta = { i: j, k: icone[k[j]], c: c[j], r: r[j] };
      if (u) {
        proprieta.u = u[j];
      }
      elementi[j] = {
        type: "Feature",
        geometry: { type: "Point", coordinates: [x[j] / 1e5, y[j] / 1e5] },
        properties: proprieta
      };
    }
    return collezione(elementi);
  }

  function collezioneBolle(bolle) {
    var x = elenco(bolle.x);
    var elementi = new Array(x.length);
    var campi = ["n", "censiti", "nome", "d", "ovest", "sud", "est", "nord", "livello"];
    var colonne = {};
    campi.forEach(function (campo) {
      colonne[campo] = elenco(bolle[campo]);
    });
    var y = elenco(bolle.y);
    for (var j = 0; j < x.length; j++) {
      var proprieta = {};
      campi.forEach(function (campo) {
        proprieta[campo] = colonne[campo][j];
      });
      elementi[j] = {
        type: "Feature",
        geometry: { type: "Point", coordinates: [x[j] / 1e5, y[j] / 1e5] },
        properties: proprieta
      };
    }
    return collezione(elementi);
  }

  function collezioneRiquadri(riquadri) {
    var r = elenco(riquadri.r);
    var o = elenco(riquadri.ovest);
    var s = elenco(riquadri.sud);
    var e = elenco(riquadri.est);
    var n = elenco(riquadri.nord);
    return collezione(
      r.map(function (rfid, j) {
        return {
          type: "Feature",
          properties: { r: rfid },
          geometry: {
            type: "LineString",
            coordinates: [
              [o[j], s[j]],
              [e[j], s[j]],
              [e[j], n[j]],
              [o[j], n[j]],
              [o[j], s[j]]
            ]
          }
        };
      })
    );
  }

  function collezioneAnelli(anelli) {
    return collezione(
      elenco(anelli).map(function (anello) {
        var x = elenco(anello.x);
        var y = elenco(anello.y);
        return {
          type: "Feature",
          properties: {},
          geometry: {
            type: "LineString",
            coordinates: x.map(function (valore, j) {
              return [valore, y[j]];
            })
          }
        };
      })
    );
  }

  function collezioneCluster(punti) {
    var x = elenco(punti.x);
    var y = elenco(punti.y);
    var colonne = {
      colore: elenco(punti.colore),
      ind: elenco(punti.ind),
      r22: elenco(punti.r22),
      r: elenco(punti.r)
    };
    var elementi = new Array(x.length);
    for (var j = 0; j < x.length; j++) {
      elementi[j] = {
        type: "Feature",
        geometry: { type: "Point", coordinates: [x[j] / 1e5, y[j] / 1e5] },
        properties: {
          i: j,
          colore: colonne.colore[j],
          ind: colonne.ind[j],
          r22: colonne.r22[j],
          r: colonne.r[j]
        }
      };
    }
    return collezione(elementi);
  }

  function imposta(map, sorgente, dati) {
    var s = map.getSource(sorgente);
    if (s) {
      s.setData(dati);
    }
  }

  function impostaDati(stato, dati) {
    var map = stato.map;
    var icone = elenco(stato.config.icone.id);
    if (stato.popup) {
      stato.popup.remove();
    }
    switch (stato.config.modalita) {
      case "cluster":
        // Le bolle arrivano prima, con il messaggio "bolle".
        imposta(map, "bidoni", collezionePunti(dati.punti, icone));
        break;
      case "rfid":
        imposta(map, "letture", collezionePunti(dati.punti, icone));
        imposta(map, "riquadri", collezioneRiquadri(dati.riquadri));
        break;
      case "giro":
        imposta(map, "letture", collezionePunti(dati.punti, icone));
        imposta(map, "comuni_giro", collezioneAnelli(dati.comuni_giro));
        break;
      case "analisi":
        imposta(map, "cluster", collezioneCluster(dati.punti));
        aggiornaIndicatori(stato, dati);
        break;
      default:
        imposta(map, "letture", collezionePunti(dati.punti, icone));
    }
  }

  function impostaBolle(stato, dati) {
    imposta(stato.map, "bolle_cantiere", collezioneBolle(dati.bolle_cantiere));
    imposta(stato.map, "bolle_comune", collezioneBolle(dati.bolle_comune));
    impostaFasce(stato, dati.territorio);
  }

  // Bolle del territorio da lontano, bidoni da vicino. Un file senza cantieri
  // né comuni non ha bolle: i bidoni si vedono a ogni zoom.
  function impostaFasce(stato, territorio) {
    var map = stato.map;
    var minimo = territorio ? stato.config.zoom_bolle.comune : 0;
    ["cluster", "cluster-testo", "bidoni"].forEach(function (id) {
      if (map.getLayer(id)) {
        map.setLayerZoomRange(id, minimo, 24);
      }
    });
    [
      "bolle-cantiere",
      "bolle-cantiere-testo",
      "bolle-cantiere-nome",
      "bolle-comune",
      "bolle-comune-testo"
    ].forEach(function (id) {
      if (map.getLayer(id)) {
        map.setLayoutProperty(id, "visibility", territorio ? "visible" : "none");
      }
    });
  }

  // --- Indicatori della mappa dei cluster spaziali ----------------------------

  function ControlloIndicatori() {}
  ControlloIndicatori.prototype.onAdd = function () {
    this.contenitore = document.createElement("div");
    this.contenitore.className = "maplibregl-ctrl controllo-indicatori";
    return this.contenitore;
  };
  ControlloIndicatori.prototype.onRemove = function () {
    this.contenitore.parentNode.removeChild(this.contenitore);
  };

  function aggiornaIndicatori(stato, dati) {
    var map = stato.map;
    if (!stato.indicatori) {
      stato.indicatori = new ControlloIndicatori();
      map.addControl(stato.indicatori, "top-right");
      stato.nota = document.createElement("div");
      stato.nota.className = "nota-campione";
      map.getContainer().appendChild(stato.nota);
    }
    var nomi = elenco(dati.indicatori && dati.indicatori.nome);
    var colori = elenco(dati.indicatori && dati.indicatori.colore);
    var contenitore = stato.indicatori.contenitore;
    contenitore.innerHTML = "";
    var applicaFiltro = function () {
      var attivi = [];
      contenitore.querySelectorAll("input").forEach(function (casella) {
        if (casella.checked) {
          attivi.push(casella.value);
        }
      });
      var filtro = ["in", ["get", "ind"], ["literal", attivi]];
      ["dispersione", "baricentri"].forEach(function (id) {
        map.setFilter(id, filtro);
      });
    };
    nomi.forEach(function (nome, j) {
      var riga = document.createElement("label");
      var casella = document.createElement("input");
      casella.type = "checkbox";
      casella.checked = true;
      casella.value = nome;
      casella.addEventListener("change", applicaFiltro);
      var punto = document.createElement("span");
      punto.className = "punto-indicatore";
      punto.style.background = colori[j];
      riga.appendChild(casella);
      riga.appendChild(punto);
      riga.appendChild(document.createTextNode(nome));
      contenitore.appendChild(riga);
    });
    contenitore.style.display = nomi.length > 0 ? "" : "none";
    applicaFiltro();
    stato.nota.textContent = dati.nota || "";
    stato.nota.style.display = dati.nota ? "" : "none";
  }

  // --- Messaggi dal server ------------------------------------------------------

  function visibile(stato) {
    return stato.el.offsetWidth > 0 && stato.el.offsetHeight > 0;
  }

  function inquadra(stato, messaggio) {
    if (!visibile(stato)) {
      // Una mappa nascosta non sa che zoom usare: inquadra quando torna in
      // vista.
      stato.inquadraInAttesa = messaggio;
      return;
    }
    stato.inquadraInAttesa = null;
    var r = elenco(messaggio.riquadro);
    if (r.length !== 4) {
      return;
    }
    stato.map.fitBounds(
      [
        [r[0], r[1]],
        [r[2], r[3]]
      ],
      {
        padding: 30,
        maxZoom: messaggio.zoom_massimo || stato.config.zoom_massimo,
        duration: messaggio.subito ? 0 : 500
      }
    );
  }

  function applica(stato, messaggio) {
    if (messaggio.tipo === "bolle") {
      impostaBolle(stato, messaggio.dati);
    } else if (messaggio.tipo === "dati") {
      impostaDati(stato, messaggio.dati);
    } else if (messaggio.tipo === "inquadra") {
      inquadra(stato, messaggio);
    } else if (messaggio.tipo === "popup") {
      apriPopup(stato, [messaggio.lng, messaggio.lat], messaggio.html);
    }
  }

  function svuotaCoda(stato) {
    ["bolle", "dati", "inquadra", "popup"].forEach(function (tipo) {
      var messaggio = stato.coda[tipo];
      if (messaggio) {
        delete stato.coda[tipo];
        applica(stato, messaggio);
      }
    });
  }

  function ricevi(messaggio) {
    var stato = mappe[messaggio.id];
    if (!stato || !stato.pronta) {
      var coda = stato ? stato.coda : (inAttesa[messaggio.id] = inAttesa[messaggio.id] || {});
      coda[messaggio.tipo] = messaggio;
      return;
    }
    applica(stato, messaggio);
  }

  function inizia(widget, el, config) {
    var map = widget.getMap();
    var precedente = mappe[el.id];
    var stato = {
      id: el.id,
      el: el,
      map: map,
      config: config,
      pronta: false,
      coda: Object.assign(
        {},
        precedente && !precedente.pronta ? precedente.coda : {},
        inAttesa[el.id] || {}
      ),
      popup: null,
      inquadraInAttesa: null,
      ripiego: false
    };
    delete inAttesa[el.id];
    mappe[el.id] = stato;
    map.dragRotate.disable();
    map.touchZoomRotate.disableRotation();
    map.addControl(new maplibregl.NavigationControl({ showCompass: false }), "top-left");
    map.addControl(new maplibregl.ScaleControl({ unit: "metric" }), "bottom-left");
    // Se lo stile dello sfondo non si scarica, ad esempio senza la rete,
    // MapLibre non disegnerebbe niente: la mappa passa a uno sfondo semplice,
    // e confini, bolle, cluster e marker si disegnano lo stesso.
    map.on("error", function (e) {
      var errore = e && e.error;
      if (
        stato.pronta ||
        stato.ripiego ||
        !config.stile_url ||
        !config.stile_ripiego ||
        !errore ||
        typeof errore.url !== "string" ||
        errore.url.indexOf(config.stile_url) !== 0
      ) {
        return;
      }
      stato.ripiego = true;
      map.setStyle(config.stile_ripiego);
    });
    map.on("resize", function () {
      if (stato.inquadraInAttesa && visibile(stato)) {
        inquadra(stato, stato.inquadraInAttesa);
      }
    });
    var avvia = function () {
      caricaIcone(map, config.icone).then(function () {
        aggiungiLivelli(stato);
        aggiungiEventi(stato);
        stato.pronta = true;
        svuotaCoda(stato);
      });
    };
    if (map.loaded()) {
      avvia();
    } else {
      map.once("load", avvia);
    }
  }

  // Le mappe tornate visibili si ridimensionano: una mappa nata in una scheda
  // nascosta non conosce la sua misura.
  function ridimensiona() {
    Object.keys(mappe).forEach(function (id) {
      var stato = mappe[id];
      if (visibile(stato)) {
        stato.map.resize();
      }
    });
  }

  $(document).on("shiny:connected", function () {
    Shiny.addCustomMessageHandler("trackerfid-mappa", ricevi);
  });

  window.TrackerfidMappa = { inizia: inizia, ridimensiona: ridimensiona };
})();
