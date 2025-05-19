// Registrace layoutů `cola` a `fcose` probíhá v js/index.js
//cytoscape.use(cytoscapePanzoom); 
console.log("✅ cytoscape_init.js loaded");
console.log("Cytoscape available:", typeof cytoscape !== 'undefined');

// Check if cytoscapeInstances already exists
if (typeof cytoscapeInstances === 'undefined') {
  var cytoscapeInstances = new Map();
}

// Globální proměnná pro sledování posledního zvýrazněného uzlu
var lastHighlightedNode = null;

// Function to generate unique container ID for a patient
function getContainerId(patientId, isSubset = false) {
  if (!patientId) {
    console.error("patientId is undefined");
    return null;
  }
  return `cy-${patientId}-${isSubset ? 'subset' : 'main'}`;
}

// Function to clean up existing cytoscape instance
function cleanupInstance(patientId) {
  if (!patientId) {
    console.error("Cannot cleanup instance: patientId is undefined");
    return;
  }
  
  const mainInstance = cytoscapeInstances.get(getContainerId(patientId));
  const subsetInstance = cytoscapeInstances.get(getContainerId(patientId, true));
  
  if (mainInstance) {
    mainInstance.destroy();
    cytoscapeInstances.delete(getContainerId(patientId));
  }
  
  if (subsetInstance) {
    subsetInstance.destroy();
    cytoscapeInstances.delete(getContainerId(patientId, true));
  }
}

// Function to initialize cytoscape instance
function initializeCytoscape(containerId, elements, layout) {
  console.log("Initializing Cytoscape with containerId:", containerId);
  console.log("Elements:", elements);
  console.log("Layout:", layout);
  
  if (!containerId) {
    console.error("Cannot initialize cytoscape: containerId is undefined");
    return null;
  }
  
  const container = document.getElementById(containerId);
  if (!container) {
    console.error(`Container ${containerId} not found`);
    return null;
  }
  console.log("Container found:", container);

  // Debug node data
  if (elements && elements.nodes) {
    console.log("First few nodes data:", elements.nodes.slice(0, 3));
  }

  try {
    const cy = cytoscape({
      container: container,
      elements: elements,
      style: [
        {
          selector: 'node',
          style: {
            "text-valign": "center",
            "text-halign": "center",
            "font-size": "40",
            "content": "data(name)",
            "width": "mapData(degree, 0, 20, 100, 300)",
            "height": "mapData(degree, 0, 20, 100, 300)"
          }
        },
        {
          selector: 'node[log2FC<=0]',
          style: {
            "label": "data(name)",
            "background-color": "mapData(log2FC, -10, 0, blue, white)"
          }
        },
        {
          selector: 'node[log2FC>0]',
          style: {
            "label": "data(name)",
            "background-color": "mapData(log2FC, 0, 10, white, red)"
          }
        },
        {
          selector: 'node:selected',
          style: {
            "label": "data(name)",
            "background-color": "#B8E788"
          }
        },
        {
          selector: 'node.highlighted',
          style: {
            'background-color': 'yellow',
            'border-color': 'yellow',
            'border-width': '4px'
          }
        },
        {
          selector: 'node.germVariantBorder',
          style: {
            "border-width": 16,
            "border-color": "purple",
            "border-style": "solid"
          }
        },
        {
          selector: 'node.fusionBorder',
          style: {
            "outline-width": 12,
            "outline-color": "orange",
            "outline-offset": 4,
            "outline-style": "solid"
          }
        },
        {
          selector: 'edge',
          style: {
            'curve-style': 'bezier'
          }
        }
      ],
      layout: {
        name: layout || 'cola',
        padding: 50,
        randomize: false,
        fit: true,
        nodeDimensionsIncludeLabels: true
      },
      multiselect: false
    });

    // Přidáme vlastnost pro sledování zvýrazněného uzlu
    cy.lastHighlightedNode = null;

    console.log("Cytoscape instance created successfully");
    return cy;
  } catch (error) {
    console.error("Error creating Cytoscape instance:", error);
    return null;
  }
}

// Event handlers
function setupEventHandlers(cy, patientId, isSubset = false) {
  if (!patientId) {
    console.error("Cannot setup event handlers: patientId is undefined");
    return;
  }
  
  if (!isSubset) {
    let lastSelectedNodes = [];
    cy.on('select unselect', 'node', function(evt) {
      const selectedNodes = cy.$('node:selected').map(node => node.data('id'));
      if (JSON.stringify(selectedNodes) !== JSON.stringify(lastSelectedNodes)) {
        console.log("Selected nodes:", selectedNodes);
        lastSelectedNodes = selectedNodes;
      }
      Shiny.setInputValue(ns + 'cySelectedNodes', selectedNodes, { priority: "event" });
    });

    // Background click
    cy.on('tap', function(evt) {
      if (evt.target === cy) {
        cy.elements().unselect();
      }
    });
  }
}

// Custom message handlers
Shiny.addCustomMessageHandler('cy-init', function(data) {
  console.log("Received data from R:", data);
  if (!data || !data.patientId) {
    console.error("Invalid data received for cy-init:", data);
    return;
  }
  
  const { patientId, elements, layout } = data;
  console.log("Raw elements:", elements);
  
  // Parse the JSON string if it's a string
  const parsedElements = typeof elements === 'string' ? JSON.parse(elements) : elements;
  console.log("Parsed elements:", parsedElements);
  
  // Extract the actual elements from the nested structure for main graph
  const actualElements = parsedElements.elements || parsedElements;
  console.log("Actual elements:", actualElements);
  
  // Clean up existing instance
  cleanupInstance(patientId);
  
  // Initialize main graph
  const mainCy = initializeCytoscape(getContainerId(patientId), actualElements, layout);
  if (mainCy) {
    cytoscapeInstances.set(getContainerId(patientId), mainCy);
    setupEventHandlers(mainCy, patientId);
  }
});

Shiny.addCustomMessageHandler('cy-subset', function(data) {
  if (!data || !data.patientId) {
    console.error("Invalid data received for cy-subset:", data);
    return;
  }
  
  const { patientId, elements, layout } = data;
  console.log("Subset data received:", data);
  
  // Parse the JSON string if it's a string
  const parsedElements = typeof elements === 'string' ? JSON.parse(elements) : elements;
  console.log("Parsed subset elements:", parsedElements);
  
  // Clean up existing subset instance
  const subsetInstance = cytoscapeInstances.get(getContainerId(patientId, true));
  if (subsetInstance) {
    subsetInstance.destroy();
    cytoscapeInstances.delete(getContainerId(patientId, true));
  }
  
  // Initialize subset graph
  const subsetCy = initializeCytoscape(getContainerId(patientId, true), parsedElements, layout);
  if (subsetCy) {
    cytoscapeInstances.set(getContainerId(patientId, true), subsetCy);
    setupEventHandlers(subsetCy, patientId, true);
  }
});

Shiny.addCustomMessageHandler('cy-layout', function(data) {
  if (!data || !data.patientId) {
    console.error("Invalid data received for cy-layout:", data);
    return;
  }
  
  const { patientId, layout } = data;
  const cy = cytoscapeInstances.get(getContainerId(patientId));
  if (cy) {
    cy.layout({ name: layout }).run();
  }
});

Shiny.addCustomMessageHandler('variant-border', function(data) {
  if (!data || !data.patientId) {
    console.error("Invalid data received for variant-border:", data);
    return;
  }
  
  const { patientId, type, nodes } = data;
  const cy = cytoscapeInstances.get(getContainerId(patientId));
  if (cy) {
    // Remove existing borders
    cy.elements().removeClass('variant-border');
    
    // Add new borders
    if (nodes && nodes.length > 0) {
      const nodeElements = cy.nodes().filter(node => nodes.includes(node.id()));
      nodeElements.addClass('variant-border');
    }
  }
});

Shiny.addCustomMessageHandler('select-first-neighbors', function(data) {
  if (!data || !data.patientId) {
    console.error("Invalid data received for select-first-neighbors:", data);
    return;
  }
  
  const { patientId } = data;
  const cy = cytoscapeInstances.get(getContainerId(patientId));
  if (cy) {
    // Get currently selected nodes
    const selectedNodes = cy.$(':selected');
    if (selectedNodes.length === 0) {
      console.warn("No nodes are currently selected");
      return;
    }

    // Find neighbors (closed neighbourhood)
    const neighbors = selectedNodes.neighborhood().add(selectedNodes); // Add original nodes

    // Select the neighbors
    neighbors.nodes().select();
  }
});

Shiny.addCustomMessageHandler('fit-selected-nodes', function(data) {
  if (!data || !data.patientId) {
    console.error("Invalid data received for fit-selected-nodes:", data);
    return;
  }
  
  const { patientId, nodes } = data;
  const cy = cytoscapeInstances.get(getContainerId(patientId));
  if (cy) {
    if (nodes && nodes.length > 0) {
      const selectedElements = cy.nodes().filter(node => nodes.includes(node.id()));
      cy.fit(selectedElements, 50);
    } else {
      cy.fit(50);
    }
  }
});

Shiny.addCustomMessageHandler('clear-selection', function(data) {
  if (!data || !data.patientId) {
    console.error("Invalid data received for clear-selection:", data);
    return;
  }
  
  const { patientId } = data;
  const cy = cytoscapeInstances.get(getContainerId(patientId));
  if (cy) {
    // Clear all selections
    cy.elements().unselect();
    Shiny.setInputValue(ns + 'cySelectedNodes', [], { priority: "event" });
    console.log("Selection cleared for patient:", patientId);
  }
});

// Přidání handleru pro zvýraznění uzlu
Shiny.addCustomMessageHandler('cy-add-node-selection', function(data) {
    console.log("Received data for node selection:", data);
    
    // Zpětná kompatibilita se starým formátem
    if (!data.patientId) {
        // Pokud nemáme patientId, použijeme první dostupnou instanci
        const firstInstance = cytoscapeInstances.values().next().value;
        if (!firstInstance) {
            console.error("No Cytoscape instance found");
            return;
        }
        data.patientId = firstInstance.container().id.split('-')[1]; // Extrahujeme patientId z ID kontejneru
    }

    const containerId = getContainerId(data.patientId);
    console.log("Looking for instance with containerId:", containerId);
    
    const cytoscapeInstance = cytoscapeInstances.get(containerId);
    if (!cytoscapeInstance) {
        console.error("Cytoscape instance not found for patient:", data.patientId);
        return;
    }

    // Najděte uzel odpovídající vybranému genu
    const nodeToHighlight = cytoscapeInstance.nodes().filter(function(ele) {
        return ele.data('name') === data.gene.gene;
    });

    if (nodeToHighlight.length > 0) {
        // Pokud už je stejný uzel zvýrazněn, zrušíme zvýraznění
        if (cytoscapeInstance.lastHighlightedNode && cytoscapeInstance.lastHighlightedNode.same(nodeToHighlight)) {
            cytoscapeInstance.lastHighlightedNode.removeClass('highlighted');
            cytoscapeInstance.lastHighlightedNode = null;
            console.log("Node unhighlighted:", nodeToHighlight.data('id'));
        } else {
            // Pokud je zvýrazněn jiný uzel, zrušíme jeho zvýraznění
            if (cytoscapeInstance.lastHighlightedNode) {
                cytoscapeInstance.lastHighlightedNode.removeClass('highlighted');
            }
            
            // Přidáme zvýraznění novému uzlu
            nodeToHighlight.addClass('highlighted');
            console.log("Node highlighted:", nodeToHighlight.data('id'));

            // Uložíme aktuální zvýrazněný uzel jako poslední
            cytoscapeInstance.lastHighlightedNode = nodeToHighlight;
        }
    } else {
        console.error("Node not found for gene:", data.gene);
    }
});

// Cleanup on page unload
window.addEventListener('unload', function() {
  cytoscapeInstances.forEach((cy, id) => {
    cy.destroy();
  });
  cytoscapeInstances.clear();
});


