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


// Handler pro vykreslení hlavního grafu
Shiny.addCustomMessageHandler('cy-init', function(data) {
    console.log('Received data for main graph:', data);
    
    data.elements.nodes.forEach(node => {
        if (node.data && node.data.log2FC !== undefined) {
            console.log(`Hlavní graf - Node ID: ${node.data.id}, log2FC: ${node.data.log2FC}`);
        }
    });

    if (!cy) {
        cy = initializeCytoscape(cyContainerId, data.elements);
    } else {
        cy.elements().remove();
        cy.add(data.elements);
        cy.layout({ name: 'cola' }).run(); // Použije výchozí layout při spuštění
    }
});

// Handler pro vykreslení podgrafu
Shiny.addCustomMessageHandler('cy-subset', function(data) {
    console.log('Received data for subset graph:', data);

    data.elements.nodes.forEach((node, index) => {
        console.log(`Podgraf - Index: ${index}, Node ID: ${node.data.id}, log2FC: ${node.data.log2FC}`);
    });

    if (!cySubset) {
        cySubset = initializeCytoscape(cySubsetContainerId, data.elements, true);
    } else {
        cySubset.elements().remove();
        cySubset.add(data.elements);
        cySubset.layout({ name: 'cola' }).run();
    }
});






// Přidání naslouchání pro kliknutí na tlačítko pro odznačení všech uzlů
document.addEventListener('DOMContentLoaded', function() {
    // Počkej, až bude `ns` definováno
    if (typeof ns === 'undefined') {
        console.error("Namespace 'ns' není definován, zkusíme znovu za 100ms.");
        setTimeout(arguments.callee, 100);
        return;
    }

    const clearButton = document.getElementById(ns + 'clearSelectionButton');
    if (clearButton) {
        console.log("Tlačítko 'clearSelectionButton' nalezeno, přidávám událost pro vymazání výběru.");
        clearButton.addEventListener('click', function() {
            if (cy) {
                console.log("Clearing selection...");
                cy.$(':selected').unselect();  // Deselect all selected nodes

                // Ověření, zda jsou uzly skutečně odznačeny
                const selectedNodesAfterClear = cy.$(':selected').map(node => node.data('id'));
                console.log("Po výběru vymazání, vybrané uzly:", selectedNodesAfterClear);

                // Nastavení hodnoty pro Shiny na prázdný seznam
                Shiny.setInputValue(ns + 'cySelectedNodes', [], { priority: "event" });
                console.log("Selection cleared, nastavuji Shiny input na prázdný seznam.");
            }
        });
    } else {
        console.error("Tlačítko 'clearSelectionButton' nebylo nalezeno.");
    }
});

// Handler pro změnu layoutu
Shiny.addCustomMessageHandler('cy-layout', function(layout) {
    if (cy) {
        cy.layout({ name: layout }).run();
    }
});
