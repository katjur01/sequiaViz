import cytoscape from 'cytoscape';
import cola from 'cytoscape-cola';
import fcose from 'cytoscape-fcose';

cytoscape.use(cola);
cytoscape.use(fcose);

window.cytoscape = cytoscape;