[
   {"selector":"node", "css": {
       "text-valign":"center",
       "text-halign":"center",
       "font-size":"40",
       "border-color": "black",
       "content": "data(label)",
       "border-width": "1px",
       "width": "mapData(degree, 0, 20, 100, 300)",
       "height": "mapData(degree, 0, 20, 100, 300)"
       }},

    {"selector": "node[lfc<=0]", "css": {
        "background-color": "mapData(lfc, -1, 0, blue, white)"
        }},

    {"selector": "node[lfc>0]", "css": {
        "background-color": "mapData(lfc, 0, 2, white, red)"
    }},

    {"selector": "node:selected", "css": {
       "overlay-opacity": 0.3,
       "overlay-color": "gray"
    }},

    {"selector": "edge", "css": {
        "curve-style": "bezier"
    }},
    
    {
    // initial viewport state:
    "zoom": 1,
    "pan": "{ x: 0, y: 0 }",
  
    // interaction options:
    "minZoom": 1e-50,
    "maxZoom": 1e50,
    "zoomingEnabled": true,
    "userZoomingEnabled": true,
    "panningEnabled": true,
    "userPanningEnabled": true,
    "boxSelectionEnabled": true,
    "selectionType": "single",
    "touchTapThreshold": 8,
    "desktopTapThreshold": 4,
    "autolock": false,
    "autoungrabify": false,
    "autounselectify": false,
    "multiClickDebounceTime": 250,
  
    // rendering options:
    "headless": false,
    "styleEnabled": true,
    "hideEdgesOnViewport": false,
    "textureOnViewport": false,
    "motionBlur": false,
    "motionBlurOpacity": 0.2,
    "wheelSensitivity": 1,
    "pixelRatio": "auto"
    }
]