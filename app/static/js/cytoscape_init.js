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
                    "background-color": "green"
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
        layout: { name: 'cola' }  // Výchozí layout
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

            // Nastavení hodnoty pro Shiny pomocí správného ID
            Shiny.setInputValue(ns + 'cySelectedNodes', selectedNodes, { priority: "event" });
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
    }

    return cytoscapeInstance;
}


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






// Listening clearSelectionButton for unselecting all nodes
document.addEventListener('DOMContentLoaded', function() {
    // Počkej, až bude `ns` definováno
    if (typeof ns === 'undefined') {
        console.error("Namespace 'ns' is not defined, waiting 500ms before another try.");
        setTimeout(arguments.callee, 500);
        return;
    }

    const clearButton = document.getElementById(ns + 'clearSelectionButton');
    if (clearButton) {
        clearButton.addEventListener('click', function() {
            if (cy) {
                console.log("Clearing selection...");
                cy.$(':selected').unselect();  // Unselect all selected nodes

                // Check if the nodes are truly unselected
                const selectedNodesAfterClear = cy.$(':selected').map(node => node.data('id'));
                console.log("Unselecting some nodes, selected nodes left:", selectedNodesAfterClear);

                // Set shiny value as empty list
                Shiny.setInputValue(ns + 'cySelectedNodes', [], { priority: "event" });
                console.log("Selection cleared, seting Shiny input as empty list.");
            }
        });
    } else {
        console.error("Tlačítko 'clearSelectionButton' nebylo nalezeno.");
    }
});

// Highlight row by clicking 
Shiny.addCustomMessageHandler('highlight-row', function(data) {
    const rows = document.querySelectorAll('.Reactable .rt-tbody .rt-tr-group .rt-tr'); // Select all rows in table

    rows.forEach(row => {
        const geneNameCell = row.querySelector('.rt-td:nth-child(2)'); // Second column with gene name is selected (feature_name)

        if (geneNameCell.textContent.trim() === data.gene.gene) {
            if (row.style.backgroundColor) {
                row.style.backgroundColor = '';  // delete previous color row - row has no color
                console.log("Unhighlighting row for gene: ", data.gene.gene);
            } else {
                row.style.backgroundColor = '#FFFF99';  // selected row is yellow
                console.log("Highlighting row for gene: ", data.gene.gene);
            }
        }
    });
});



// Handler for layout change
Shiny.addCustomMessageHandler('cy-layout', function(layout) {
    if (cy) {
        cy.layout({ name: layout }).run();
    }
});
