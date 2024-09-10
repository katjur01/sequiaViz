library(shiny)
library(shinyjs)
library(htmlwidgets)


ui <- fluidPage(
  titlePanel("Cytoscape.js v Shiny aplikaci"),
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
    tags$script(
      '
      Shiny.addCustomMessageHandler("initCytoscape", function(message) {
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
      '
    )
  ),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      div(id = "cy", style = "width: 600px; height: 600px;")
    )
  )
)

server <- function(input, output, session) {
  observe({
    session$sendCustomMessage(type = 'initCytoscape', message = list())
  })
}

shinyApp(ui, server)