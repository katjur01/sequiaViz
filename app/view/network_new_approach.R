library(shiny)
library(cyjShiny)
library(htmlwidgets)
library(graph)
library(jsonlite)

library(data.table)

# HELPER FUNCTIONS ----
# printf prints formatted output like in C
printf <- function(...) cat(sprintf(...))

# LOAD DATA ----
# The yeast galactose network was the earliest demo used in the Cytoscape project,
# consisting of the graph (a Bioconductor graphNEL) and expression (a data.frame)

# g <- get(load(system.file(package="cyjShiny", "extdata", "yeastGalactoseGraphNEL.RData")))
# printf("--- loaded g")
# print(g)
# 
# tbl.mrna <- get(load(system.file(package="cyjShiny", "extdata", "yeastGalactoseExpressionTable.RData")))
# printf("--- loaded tbl.mrna: %d x %d", nrow(tbl.mrna), ncol(tbl.mrna))

# GENERATE GRAPH ----
# tbl.mrna <- as.data.frame(tbl.mrna)
nodeAttrs <- nodeData(g, attr="label")

tbl.mrna <- copy(fc_df)
graph <- copy(network_json)


# Not used in all three experimental conditions
g <- removeNode("YER056CA", g) 

yeastGalactoseNodeNames <- as.character(nodeAttrs)
yeastGalactodeNodeIDs <- nodes(g)

g <- addNode("gal1RGexp", g)

## Convert graphNEL data to cytoscape.js JSON structure (see https://js.cytoscape.org/#notation/elements-json)
# NOTE: graphNEL is not a requirement of cyjShiny, but the cytoscape.js JSON structured string in variable "graph" is

# graph <- graphNELtoJSON(g)
###############################################xx



data <- fread("../input_files/testing_pathway_data.tsv")
data <- data[kegg_paths_name != ""]
setnames(data,"P_001","FC")
proteins <- data[kegg_paths_name == "Metabolic pathways",gene_name][1:10]
fc_values <- data[kegg_paths_name == "Metabolic pathways",.(gene_name,FC)][1:10]
fc_df <- as.data.frame(fc_values,row.names = fc_values$gene_name)
fc_df$gene_name <- NULL
setnames(fc_df,"FC","gal1RGexp")




get_string_interactions <- function(proteins, species = 9606) {
  base_url <- "https://string-db.org/api/json/network?"
  query <- paste0("identifiers=", paste(proteins, collapse = "%0D"), "&species=", species)
  url <- paste0(base_url, query)
  
  response <- GET(url)
  
  if (status_code(response) == 200) {
    content <- fromJSON(content(response, as = "text"))
    return(content)
  } else {
    stop("Request failed with status: ", status_code(response))
  }
}

prepare_cytoscape_network <- function(interactions) {
  # Připrava uzlů
  nodes <- unique(c(interactions$preferredName_A, interactions$preferredName_B))
  node_data <- data.frame(id = nodes, 
                          lfc = 10,
                          stringsAsFactors = FALSE)
  
  # Připrava hran s vlastním sloupcem interaction
  edges <- data.frame(source = interactions$preferredName_A,
                      target = interactions$preferredName_B,
                      interaction = "unknown",  # Vytvoření obecného sloupce interaction
                      stringsAsFactors = FALSE)
  
  # Generování JSON pro cyjShiny
  network_json <- toJSON(dataFramesToJSON(edges, node_data), auto_unbox = TRUE)
  return(network_json)
}
interactions <- get_string_interactions(proteins)
network_json <- prepare_cytoscape_network(interactions)


###############################################



# Style files (see https://js.cytoscape.org/#style)
# yeastGalactoseStyleFile <- system.file(file.path("demos", "basicDemo", "yeastGalactoseStyle.js"), package="cyjShiny")
yeastGalactoseStyleFile <- file.path("../styles/style.js")

# 

# SET INPUT OPTIONS ----
styleList <- c("", "Yeast-Galactose"="yeastGalactoseStyleFile")
# condition <- c("gal1RGexp", "gal4RGexp", "gal80Rexp")
condition <- "gal1RGexp"

# UI ----
ui <-  shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css",
              href = "http://maxcdn.bootstrapcdn.com/font-awesome/4.2.0/css/font-awesome.min.css"),
    tags$style("#cyjShiny{height:95vh !important;}")),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("loadStyleFile", "Select Style: ", choices=styleList),
      selectInput("doLayout", "Select Layout:",
                  choices=c("",
                            "cose",
                            "cola",
                            "circle",
                            "concentric",
                            "breadthfirst",
                            "grid",
                            "random",
                            "preset",
                            "fcose")),
      
      selectInput("setNodeAttributes", "Select Condition:", choices=condition),
      selectInput("selectName", "Select Node by ID:", choices = c("", nodes(g))),
      actionButton("sfn", "Select First Neighbor"),
      actionButton("fit", "Fit Graph"),
      actionButton("fitSelected", "Fit Selected"),
      actionButton("clearSelection", "Unselect Nodes"),
      HTML("<br>"),
      actionButton("loopConditions", "Loop Conditions"),
      HTML("<br>"),
      actionButton("getSelectedNodes", "Get Selected Nodes"),
      HTML("<br><br>"),
      htmlOutput("selectedNodesDisplay"),
      width=2
    ),
    mainPanel(
      cyjShinyOutput('cyjShiny'),
      width=10
    )
  ) # sidebarLayout
))

# SERVER ----
server <- function(input, output, session) {
  # Event observers 
  observeEvent(input$fit, ignoreInit=TRUE, {
    fit(session, 80)
  })
  
  observeEvent(input$setNodeAttributes, ignoreInit=TRUE, {
    attribute <- "lfc"
    expression.vector <- switch(input$setNodeAttributes,
                                "gal1RGexp" = tbl.mrna$gal1RGexp)
    # expression.vector <- switch(input$setNodeAttributes,
    #                             "gal1RGexp" = tbl.mrna$gal1RGexp,
    #                             "gal4RGexp" = tbl.mrna$gal4RGexp,
    #                             "gal80Rexp" = tbl.mrna$gal80Rexp)
    setNodeAttributes(session, attributeName=attribute, nodes=yeastGalactodeNodeIDs, values=expression.vector)
  })
  
  observeEvent(input$loadStyleFile, ignoreInit=TRUE, {
    if(input$loadStyleFile != "") {
      styleFile = get(input$loadStyleFile)
      loadStyleFile(styleFile)  
    }
  })
  
  observeEvent(input$doLayout, ignoreInit=TRUE,{
    strategy <- input$doLayout
    doLayout(session, strategy)
    #session$sendCustomMessage(type="doLayout", message=list(input$doLayout))
  })
  
  observeEvent(input$selectName, ignoreInit=TRUE,{
    session$sendCustomMessage(type="selectNodes", message=list(input$selectName))
  })
  
  observeEvent(input$sfn, ignoreInit=TRUE,{
    session$sendCustomMessage(type="sfn", message=list())
  })
  
  observeEvent(input$fitSelected, ignoreInit=TRUE,{
    fitSelected(session, 100)
  })
  
  observeEvent(input$getSelectedNodes, ignoreInit=TRUE, {
    output$selectedNodesDisplay <- renderText({" "})
    getSelectedNodes(session)
  })
  
  observeEvent(input$clearSelection, ignoreInit=TRUE, {
    session$sendCustomMessage(type="clearSelection", message=list())
  })
  
  observeEvent(input$loopConditions, ignoreInit=TRUE, {
    # condition.names <- c("gal1RGexp", "gal4RGexp", "gal80Rexp")
    condition.names <- "gal1RGexp"
    for(condition.name in condition.names){
      expression.vector <- tbl.mrna[, condition.name]
      setNodeAttributes(session, attributeName="lfc", nodes=yeastGalactodeNodeIDs, values=expression.vector)
      Sys.sleep(1)
    } # for condition.name
    updateSelectInput(session, "setNodeAttributes", selected="gal1RGexp")
  })
  
  observeEvent(input$selectedNodes, {
    newNodes <- input$selectedNodes;
    output$selectedNodesDisplay <- renderText({
      paste(newNodes)
    })
  })
  
  # Output variables 
  output$value <- renderPrint({ input$action })
  
  output$cyjShiny <- renderCyjShiny({
    cyjShiny(graph, layoutName="cose", styleFile=yeastGalactoseStyleFile)
  })
} 

# RUN SHINY APP ----
shinyApp(ui = ui, server = server)