#' @export
jsCode <- '
shinyjs.loadStringData = function(genes) {
    if (Array.isArray(genes)) {
        genes = genes[0];
    }
    var url = "https://string-db.org/api/json/network?identifiers=" + encodeURIComponent(genes) + "&species=9606";

    fetch(url)
        .then(response => response.json())
        .then(data => {
            var elements = [];
            var nodes = {};

            data.forEach(function(d) {
                if (!nodes[d.preferredName_A]) {
                    nodes[d.preferredName_A] = { data: { id: d.preferredName_A, name: d.preferredName_A } };
                    elements.push(nodes[d.preferredName_A]);
                }
                if (!nodes[d.preferredName_B]) {
                    nodes[d.preferredName_B] = { data: { id: d.preferredName_B, name: d.preferredName_B } };
                    elements.push(nodes[d.preferredName_B]);
                }
                elements.push({
                    data: {
                        id: d.preferredName_A + "-" + d.preferredName_B,
                        source: d.preferredName_A,
                        target: d.preferredName_B
                    }
                });
            });

            // Přidáme samostatné uzly, které nemají žádné spojení
            var inputGenes = genes.split("%0D");
            inputGenes.forEach(function(gene) {
                if (!nodes[gene]) {
                    elements.push({ data: { id: gene, name: gene } });
                }
            });

            Shiny.setInputValue("stringData", JSON.stringify(elements));
        })
        .catch(error => {
            console.error("Error fetching data:", error);
        });
}
'

#' @export
custom_setting <- '
  var cy; // Definice globální proměnné cy

  Shiny.addCustomMessageHandler("initCytoscape", function(message) {
    var elements = JSON.parse(message.elements);
    var show_singletons = message.show_singletons;
    cy = cytoscape({
      container: document.getElementById("cy"),
      elements: elements,
      style: [
        {
          selector: "node[name]",
          style: {
            "background-color": "#666",
            "label": "data(name)"
          }
        },
        {
          selector: "edge",
          style: {
            "width": 3,
            "line-color": "#ccc",
            "target-arrow-color": "#ccc",
            "target-arrow-shape": "triangle"
          }
        }
      ],
      layout: {
        name: "cose",
        fit: true,
        padding: 10,
        randomize: true,
        nodeRepulsion: 2048,
        idealEdgeLength: 100,
        edgeElasticity: 100,
        gravity: 1,
        numIter: 1000,
        initialTemp: 200,
        coolingFactor: 0.95,
        minTemp: 1.0
      },
      // initial viewport state:
      // pokud je fit = true, parametry mohou být ingnorovány
      zoom: 1,
      pan: { x: 0, y: 0 },

      // interaction options:
      minZoom: 1e-50,
      maxZoom: 1e50,
      zoomingEnabled: true,
      userZoomingEnabled: true,
      panningEnabled: true,
      userPanningEnabled: true,
      boxSelectionEnabled: true,
      selectionType: "single",
      touchTapThreshold: 8,
      desktopTapThreshold: 4,
      autolock: false,
      autoungrabify: false,
      autounselectify: false,
      multiClickDebounceTime: 250,

      // rendering options:
      headless: false,
      styleEnabled: true,
      hideEdgesOnViewport: false,
      textureOnViewport: false,
      motionBlur: false,
      motionBlurOpacity: 0.2,
      wheelSensitivity: 1,
      pixelRatio: "auto"
    });

    if (show_singletons) {
      cy.nodes().forEach(function(node) {
        if (node.degree(false) === 0) {
          node.style("background-color", "gray");
          node.style("width", "20");
          node.style("height", "20");
        }
      });
    }
  });

  Shiny.addCustomMessageHandler("toggleSingletons", function(message) {
    var show = message.show;
    if (cy) {
      cy.nodes().forEach(function(node) {
        if (node.degree(false) === 0) {
          node.style("display", show ? "element" : "none");
        }
      });
    }
  });
'
