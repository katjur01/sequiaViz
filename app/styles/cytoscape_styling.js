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

    {"selector": "node[log2FC<=0]", "css": {
        "background-color": "mapData(log2FC, -10, 0, blue, white)"
    }},
    {"selector": "node[log2FC>0]", "css": {
        "background-color": "mapData(log2FC, 0, 10, white, red)"
    }},
    {"selector": "node:selected", "css": {
       "background-color": "green"
    }},
    {"selector": "edge", "css": {
        "curve-style": "bezier"
    }}
]