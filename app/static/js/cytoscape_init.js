// Registrace layoutů `cola` a `fcose`
cytoscape.use(cytoscapeCola);
cytoscape.use(cytoscapeFcose);  // Použití `cytoscapeFcose` pro fcose layout
//cytoscape.use(cytoscapePanzoom); 

let cy;
let cySubset;
let lastHighlightedNode = null;

// Společná funkce pro inicializaci Cytoscape grafu
function initializeCytoscape(containerId, elementsData, isSubset = false) {
    const container = document.getElementById(containerId);
    if (!container) {
        console.error(`Kontejner s ID ${containerId} nebyl nalezen.`);
        return null;
    }

    const cytoscapeInstance = cytoscape({
        container: container,
        elements: elementsData,
        style: [
            {
                selector: 'node',
                style: {
                    "text-valign": "center",
                    "text-halign": "center",
                    "font-size": "40",
                    "border-color": "black",
                    "content": "data(label)",
                    "border-width": "1px",
                    "width": "mapData(degree, 0, 20, 100, 300)",
                    "height": "mapData(degree, 0, 20, 100, 300)"
                }
            },
            {
                selector: 'node[log2FC<=0]',
                style: {
                    'label': 'data(name)',
                    "background-color": "mapData(log2FC, -10, 0, blue, white)"
                }
            },
            {
                selector: 'node[log2FC>0]',
                style: {
                    'label': 'data(name)',
                    "background-color": "mapData(log2FC, 0, 10, white, red)"
                }
            },
            {
                selector: 'node:selected',
                style: {
                    'label': 'data(name)',
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
                selector: 'edge',
                style: {
                    'curve-style': 'bezier'
                }
            }
        ],
        layout: { name: 'cola' },  // Výchozí layout
        multiselect: false
    });
    

    cytoscapeInstance.on('click', (event) => {
      if (event.target === cytoscapeInstance) {         // click on the background
        console.log("Background deselection is forbidden.");
        cytoscapeInstance.nodes().unselectify();
      } else {
        cytoscapeInstance.nodes().selectify();
      }
    });

    // Sledování výběru a odznačení uzlů
    if (!isSubset) {

        cytoscapeInstance.on('select unselect', 'node', function(evt) {
            const selectedNodes = cytoscapeInstance.$('node:selected').map(node => node.data('id'));
            console.log("Vybrané uzly:", selectedNodes);
            
            Shiny.setInputValue(ns + 'cySelectedNodes', selectedNodes, { priority: "event" });  // Nastavení hodnoty pro Shiny pomocí správného ID
            
             if (evt.type === 'unselect') {
              const cyUnselectedNodes = evt.target.data('id');  // Nebo použijte jiný atribut podle potřeby
              console.log("Odznačený uzel:", cyUnselectedNodes);

              Shiny.setInputValue(ns + 'cyUnselectedNodes', cyUnselectedNodes, { priority: "event" });  // Nastavení hodnoty pro Shiny s odznačeným uzlem
            } 
        });

        // Synchronizace vybraných uzlů s hlavním grafem
        Shiny.addCustomMessageHandler('update-selected-from-gene-list', function(data) {
            const selectedNodes = data.selected_nodes;
        
            if (!selectedNodes || !Array.isArray(selectedNodes)) {
                console.warn("Selected_nodes is not valid or undefined.");
                return;
            }
        
            console.log("Selected nodes to update:", selectedNodes);
        
            if (selectedNodes.length === 0) {            // Odznačení všech aktuálních uzlů, pokud je seznam `selectedNodes` prázdný
                cy.nodes(':selected').unselect();
                console.log("All nodes deselected.");
                return;
            }
        
            const currentlySelectedNodes = cy.$('node:selected').map(node => node.data('id'));            // Získání aktuálně vybraných uzlů v hlavním grafu
            cy.nodes(':selected').unselect();            // Odznačení všech aktuálních uzlů
        
            selectedNodes.forEach(nodeId => {     // Označení uzlů
                const node = cy.getElementById(nodeId);
                if (node && node.length > 0) {
                    node.select();
                    console.log("Uzly byly označeny:", nodeId);
                } else {
                    console.warn("Uzel nebyl nalezen v hlavním grafu:", nodeId);
                }
            });
        
            console.log("Aktualizované vybrané uzly v hlavním grafu: ", selectedNodes);
        });

        Shiny.addCustomMessageHandler('cy-add-node-selection', function(data) {
            // Najděte uzel odpovídající vybranému genu
            const nodeToHighlight = cytoscapeInstance.nodes().filter(function(ele) {
                return ele.data('name') === data.gene.gene;
            });
    
            if (nodeToHighlight.length > 0) {
                // Pokud už je stejný uzel zvýrazněn, zrušíme zvýraznění
                if (lastHighlightedNode && lastHighlightedNode.same(nodeToHighlight)) {
                    lastHighlightedNode.removeClass('highlighted');
                    lastHighlightedNode = null;
                    console.log("Node unhighlighted:", nodeToHighlight.data('id'));
                } else {
                    // Pokud je zvýrazněn jiný uzel, zrušíme jeho zvýraznění
                    if (lastHighlightedNode) {
                        lastHighlightedNode.removeClass('highlighted');
                    }
                    
                    // Přidáme zvýraznění novému uzlu
                    nodeToHighlight.addClass('highlighted');
                    console.log("Node highlighted:", nodeToHighlight.data('id'));
    
                    // Uložíme aktuální zvýrazněný uzel jako poslední
                    lastHighlightedNode = nodeToHighlight;
                }
            } else {
                console.error("Node not found for gene:", data.gene);
            }
        });
        
        // Handler pro "Select first neighbors"
        Shiny.addCustomMessageHandler('select-first-neighbors', function(data) {
            // Získání aktuálně vybraných uzlů
            const selectedNodes = cytoscapeInstance.$(':selected');
            if (selectedNodes.length === 0) {
                console.warn("Nebyl vybrán žádný uzel.");
                return;
            }
    
            // Najdeme sousedy (closed neighbourhood)
            const neighbors = selectedNodes.neighborhood().add(selectedNodes); // Přidání původních uzlů
    
            // Označení sousedních uzlů
            neighbors.nodes().select();
    
            // Výpis do konzole pro ladění
            console.log("Vybrané uzly a jejich sousedi (closed neighbourhood):", neighbors.nodes().map(n => n.data('id')));
        });
    }

    return cytoscapeInstance;
}

Shiny.addCustomMessageHandler('select-first-neighbors', function(data) {
    // Získání aktuálně vybraných uzlů
    const selectedNodes = cy.$(':selected');
  console.log("Vybrané uzly a jejich sousedi (closed neighbourhood) část 1: ", selectedNodes);
    if (selectedNodes.length === 0) {
        console.warn("Nebyl vybrán žádný uzel.");
        return;
    }

    // Najdeme sousedy (closed neighbourhood)
    const neighbors = selectedNodes.neighborhood().add(selectedNodes); // Přidání původních uzlů

    // Označení sousedních uzlů
    neighbors.nodes().select();

    // Výpis do konzole pro ladění
    console.log("Vybrané uzly a jejich sousedi (closed neighbourhood):", neighbors.nodes().map(n => n.data('id')));
});

Shiny.addCustomMessageHandler('fit-selected-nodes', function(data) {
    if (data.nodes && data.nodes.length > 0) {
        // Vycentruje na konkrétní vybrané uzly
        const nodesToFit = cy.nodes().filter(function(node) {
            return data.nodes.includes(node.data('id'));
        });

        if (nodesToFit.length > 0) {
            cy.fit(nodesToFit, 50); // 50 je padding kolem vybraných uzlů
            console.log("Fitting view to selected nodes:", data.nodes);
        } else {
            console.warn("No matching nodes found to fit.");
        }
    } else {
        // Vycentruje na celý graf
        cy.fit(50); // 50 je padding kolem všech uzlů
        console.log("Fitting view to the entire graph.");
    }
});

// Handler for plotting main Network (graph)
Shiny.addCustomMessageHandler('cy-init', function(data) {
    console.log('Received data for main graph:', data);

    // Save selected nodes, if they exists
    const previouslySelectedNodes = cy ? cy.$('node:selected').map(node => node.data('id')) : [];

    if (!cy) {
        cy = initializeCytoscape(cyContainerId, data.elements);
    } else {
        cy.elements().remove();
        cy.add(data.elements);
        cy.layout({ name: 'cola' }).run(); // Default layout for run
    }

    // Znovu označ uzly, které byly vybrány před překreslením
    if (previouslySelectedNodes.length > 0) {
        const nodesToSelect = cy.nodes().filter(function(ele) {
            return previouslySelectedNodes.includes(ele.data('id'));
        });
        nodesToSelect.select();
        console.log("Nodes re-selected:", previouslySelectedNodes);
    }
});


// Handler for plotting subNetwork (subgraph)
Shiny.addCustomMessageHandler('cy-subset', function(data) {
    console.log('Received data for subset graph:', data);
    
    if (!cySubset) {
        cySubset = initializeCytoscape(cySubsetContainerId, data.elements, true);
    } else {
        cySubset.elements().remove();
        cySubset.add(data.elements);
        cySubset.layout({ name: 'cola' }).run();
    }
});


// Highlight row by clicking 
Shiny.addCustomMessageHandler('highlight-row', function(data) {
    const rows = document.querySelectorAll('.Reactable .rt-tbody .rt-tr-group .rt-tr'); // Select all rows in table

    // Nejprve odstraníme zvýraznění ze všech řádků
    rows.forEach(row => {
        row.style.backgroundColor = '';  // Odstranění zvýraznění
    });

    // Poté zvýrazníme pouze vybraný řádek
    rows.forEach(row => {
        const geneNameCell = row.querySelector('.rt-td:nth-child(2)'); // Second column with gene name is selected (feature_name)

        if (geneNameCell.textContent.trim() === data.gene.gene) {
            row.style.backgroundColor = '#FFFF99';  // selected row is yellow
            console.log("Highlighting row for gene: ", data.gene.gene);
        }
    });
});




// Handler for layout change
Shiny.addCustomMessageHandler('cy-layout', function(layout) {
    if (cy) {
        cy.layout({ name: layout }).run();
    }
});


