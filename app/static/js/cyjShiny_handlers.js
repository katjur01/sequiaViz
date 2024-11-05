
Shiny.addCustomMessageHandler("initCytoscape", function(message) {
  console.log("Cytoscape loaded")
  var cy = cytoscape({
    container: document.getElementById("cy"),
    elements: [
      { data: { id: "a" } },
      { data: { id: "b" } },
      { data: { id: "ab", source: "a", target: "b" } }
    ],
    style: [
      {
        selector: "node",
        style: {
          "background-color": "#666",
          "label": "data(id)"
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
      name: "grid",
      rows: 1
    }
  });
});
      

/*
Shiny.addCustomMessageHandler("initializeCy", function(message) {
  var cyContainer = document.getElementById('app-network_graph-cyj_network');
  
  if (cyContainer && !cy) {  // Zkontroluj, zda už nebyla instance cy vytvořena
    var cy = cytoscape({
      container: cyContainer, 
      elements: [], 
      style: []
    });

    console.log("Cytoscape instance (cy) initialized");

    // Definuj handler pro aktualizaci stylů uzlů po změně tkáně
  Shiny.addCustomMessageHandler('updateTissueStyle', function(message) {
    var tissue = message.tissue;
    console.log("JS function 'updateTissueStyle' called with tissue: " + tissue);
  
    // Clear the current node styles before applying new ones
    cy.style()
      .selector('node')
      .style({
        'background-color': ''  // Clear existing colors
      })
      .update();
  
    // Apply new styles based on log2FC
    cy.style()
      .selector('node[log2FC<=0]')
      .style({
        'background-color': 'green'  // Change from blue to green
      })
      .selector('node[log2FC>0]')
      .style({
        'background-color': 'mapData(log2FC, 0, 10, white, red)'
      })
      .update();
      
    console.log("Node styles updated with new colors.");
  });

    
    
  Shiny.addCustomMessageHandler('highlightNode', function(message) {
    console.log('Custom message received: ' + JSON.stringify(message));
    var gene = message.gene;
    console.log('Highlighting node for gene: ' + gene);
    
    cy.style()
      .selector('node[label="' + gene + '"]')
      .style({
        'background-color': 'yellow'
      })
      .update();
    console.log('Node for gene ' + gene + ' should be colored now.');
  });

  } else {
    console.error("Div with ID 'app-network_graph-cyj_network' not found.");
  }
});






