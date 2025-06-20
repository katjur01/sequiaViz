// Registrace layoutů `cola` a `fcose` probíhá v js/index.js
//cytoscape.use(cytoscapePanzoom); 
console.log("✅ cytoscape_init.js loaded");


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
                  //  "border-color": "purple",
                    "content": "data(label)",
                  //  "border-width": "1px",
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
                    'background-color': '#B8E788'
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
                selector: 'node.somVariantBorder',
                style: {
                    // Vnitřní hrana
                    'border-width': 16,                // Šířka vnitřní hrany
                    'border-color': 'green',         // Barva vnitřní hrany
                    'border-style': 'solid',          // Styl vnitřní hrany
                }
            },
            {
                selector: 'node.germVariantBorder',
                style: {
                    // Vnitřní hrana
                    'border-width': 16,                // Šířka vnitřní hrany
                    'border-color': 'purple',         // Barva vnitřní hrany
                    'border-style': 'solid',          // Styl vnitřní hrany
                }
            },
            {
                selector: 'node.fusionBorder',
                style: {
                    // Vnější hrana
                    "outline-width": 12,              // Šířka vnější hrany
                    "outline-color": "orange",        // Barva vnější hrany
                    "outline-offset": 4,              // Vzdálenost od vnitřní hrany
                    "outline-style": "solid",         // Styl vnější hrany
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
             /*if (evt.type === 'unselect') {
              const cyUnselectedNodes = evt.target.data('id');  // Nebo použijte jiný atribut podle potřeby
              console.log("Odznačený uzel:", cyUnselectedNodes);

              Shiny.setInputValue(ns + 'cyUnselectedNodes', cyUnselectedNodes, { priority: "event" });  // Nastavení hodnoty pro Shiny s odznačeným uzlem
            } */
        });

            // Synchronizace vybraných uzlů s hlavním grafem
        Shiny.addCustomMessageHandler('update-selected-from-gene-list', function(data) {
          let selectedNodes = data.selected_nodes;
      
          // Ověření, že selected_nodes je pole
          if (typeof selectedNodes === 'string') {
              selectedNodes = [selectedNodes]; // Převod na pole, pokud je string
          } else if (!Array.isArray(selectedNodes)) {
              console.warn("Expected an array for selected_nodes, received:", selectedNodes);
              return;
          }
      
          console.log("Selected nodes to update:", selectedNodes);
      
          // Získání aktuálně vybraných uzlů z Cytoscape
          const currentlySelectedNodes = cy.$('node:selected').map(node => node.data('id'));
      
          console.log("Currently selected nodes in Cytoscape:", currentlySelectedNodes);
      
          // Kontrola, zda se výběr skutečně změnil
          if (arraysEqual(selectedNodes, currentlySelectedNodes)) {
              console.log("Selection is already up to date. No changes needed.");
              return;
          }
      
          // Označení uzlů, které mají být vybrány
          selectedNodes.forEach(nodeId => {
              const node = cy.getElementById(nodeId);
              if (node && node.length > 0 && !node.selected()) {
                  node.select();
                  console.log("Node selected:", nodeId);
              } else if (!node || node.length === 0) {
                  console.warn("Node not found in the main graph:", nodeId);
              }
          });
      
          // Odznačení uzlů, které nemají být vybrány
          cy.nodes(':selected').forEach(node => {
              if (!selectedNodes.includes(node.data('id'))) {
                  node.unselect();
                  console.log("Node unselected:", node.data('id'));
              }
          });
      
          console.log("Updated selected nodes in main graph:", selectedNodes);
        });


        function arraysEqual(array1, array2) {
            if (array1.length !== array2.length) return false;
            return array1.every((value, index) => array2.includes(value));
        }

//////////////////////
        // Přidání handleru pro přidání vnitřních hran uzlům s germline variantami

Shiny.addCustomMessageHandler('variant-border', function(data) {
    console.log('Received data for variant-border:', data);

    if (!data || !data.type || !Array.isArray(data.nodes)) {
        console.warn("Invalid data provided to variant-border handler.");
        return;
    }

    const type = data.type; // Typ dat ('germline' nebo 'fusion')
    let className;

    // Nastavení třídy na základě typu
    if (type === 'germline') {
        className = 'germVariantBorder';
    } else if (type === 'somatic') {
        className = 'somVariantBorder';
    } else if (type === 'fusion') {
        className = 'fusionBorder';
    } else {
        console.warn("Unknown type provided:", type);
        return;
    }

    // Odebrání stávající třídy z uzlů
    const allNodesWithBorder = cytoscapeInstance.nodes(`.${className}`);
    if (allNodesWithBorder.length > 0) {
        allNodesWithBorder.removeClass(className);
        console.log(`Removed ${className} from all nodes.`);
    }

    // Přidání třídy k uzlům, pokud jsou data validní
    if (data.nodes.length > 0) {
        data.nodes.forEach(function(gene) {
            const node = cytoscapeInstance.nodes().filter(function(ele) {
                return ele.data('name') === gene; // Najít uzel podle atributu 'name'
            });

            if (node.length > 0) {
                node.addClass(className); // Přidání třídy
                console.log(`Added ${className} to node:`, node.data('id'));
            } else {
                console.warn("Node not found for gene:", gene);
            }
        });
    } else {
        console.log("No valid nodes provided or empty array.");
        Shiny.setInputValue(`${type}VariantWarning`, true); // Informace do Shiny, pokud nejsou uzly validní
    }
});

//////////////////////

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
/*
Shiny.addCustomMessageHandler('cy-reset', function(data) {
    console.log("Resetting graph selection...");
    cy.nodes(':selected').unselect(); // Odznačí všechny vybrané uzly
    console.log("Graph selection cleared.");
});
*/

// Handler for plotting main Network (graph)
Shiny.addCustomMessageHandler('cy-init', function(data) {
    console.log('Received data for main graph:', data);

    // Save selected nodes, if they exists
    const previouslySelectedNodes = cy ? cy.$('node:selected').map(node => node.data('id')) : [];

    if (!cy) {
        cy = initializeCytoscape(cyContainerId, data.elements);
    } else {
            /*
        cy.elements().remove();
        cy.add(data.elements);
        cy.layout({ name: 'cola' }).run();
        */
        
        cy.batch(() => {
            cy.elements().remove();     // smaže starý pathway
            cy.add(data.elements);      // přidá nový
        });

        cy.style().update();            // vynutí překreslení (řeší „neviditelné“ hrany)
        cy.resize();                    // totéž, navíc přepočte viewport
        cy.layout({ name: 'cola', animate: true }).run();   // layout až PO přidání
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
      /*
        cySubset.elements().remove();
        cySubset.add(data.elements);
        cySubset.layout({ name: 'cola' }).run();
        */
        cySubset.batch(() => {
            cySubset.elements().remove();   // smaže starý subset
            cySubset.add(data.elements);    // přidá nový
        });

        cySubset.style().update();          // vynutí redraw (canvas-bug)
        cySubset.resize();                  // přepočítá viewport

        cySubset.layout({ name: 'cola', animate: false }).run(); 
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


