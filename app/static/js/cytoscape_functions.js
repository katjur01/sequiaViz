// Uložíme Cytoscape instanci do globální proměnné
var cy;

document.addEventListener('DOMContentLoaded', function() {
  cy = window.cyInstance;  // Uložení Cytoscape instance
});

// Funkce pro zvýraznění uzlu
function highlightNode(nodeId) {
  if (cy) {
    console.log("We are in handler and trying to dye node as yellow.")
    cy.nodes("[id='" + nodeId + "']").addClass('highlighted');
  } else {
    console.error("Cytoscape instance not found.");
  }
}
