# shiny::shinyAppDir(".", options = list(launch.browser = TRUE))


box::use(
  shiny[NS, moduleServer, observeEvent, observe, tagList, fluidPage, fluidRow, column, textInput, actionButton, selectInput, reactive, req,reactiveVal,conditionalPanel,verbatimTextOutput,
        renderPrint,renderText,textOutput,htmlOutput,uiOutput,renderUI],
  httr[GET, status_code, content],
  htmltools[h3, h4, tags, div,HTML,p],
  jsonlite[fromJSON, toJSON,read_json],
  cyjShiny[cyjShinyOutput, renderCyjShiny, cyjShiny, dataFramesToJSON, selectNodes,setNodeAttributes,selectFirstNeighbors,fit,fitSelected,clearSelection,getSelectedNodes],
  data.table[fread,setnames],
  readxl[read_excel],
  graph[nodes],
  reactable[reactable,colDef,renderReactable,reactableOutput,JS],
  shinyWidgets[radioGroupButtons,pickerInput],
  
)

box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/waiters[use_spinner],
  app/logic/networkGraph_helper[get_string_interactions,prepare_cytoscape_network,get_pathway_list,get_tissue_list]
)


input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file")
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag)
  return(data)
}

ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/webcola@3.4.0/WebCola/cola.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/cytoscape-cola@2.5.1/cytoscape-cola.min.js"),
      tags$script(src="https://unpkg.com/layout-base/layout-base.js"),
      tags$script(src="https://unpkg.com/cose-base/cose-base.js"),
      tags$script(src="https://unpkg.com/cytoscape-fcose/cytoscape-fcose.js"),
      # tags$script(src="https://www.unpkg.com/jquery@3.0.0/dist/jquery.min.js"),
      # tags$script(src="https://unpkg.com/cytoscape-panzoom@2.5.3/cytoscape-panzoom.js"),
      tags$script(src = "static/js/cytoscape_init.js"),
      tags$script(HTML(sprintf("var cyContainerId = '%s'; var cySubsetContainerId = '%s'; var cySelectedNodesInputId = '%s';", 
                               ns("cyContainer"), ns("cySubsetContainer"), ns("cySelectedNodes"))))
    ),
    pickerInput(ns("selected_pathway"), "Pathway", choices = get_pathway_list(), options = list(`live-search` = TRUE)), #choices = c("", get_pathway_list())
    selectInput(ns("selected_layout"),"Choose layout: ", choices = c("cola","cose","fcose"),selected = "cola"),
    
    uiOutput(ns("js_namespace")),
    textOutput(ns("selected_nodes_text")),
    actionButton(ns("clearSelectionButton"), "Clear Selection"),
    fluidRow(
      column(7,div(id = ns("cyContainer"), style = "width: 100%; height: 600px;")),
      column(5,div(id = ns("cySubsetContainer"), style = "width: 100%; height: 600px;"))
    ),
    # tags$script(HTML(sprintf("var cyContainerId = '%s';", ns("cyContainer")))),
    # tags$script(HTML(sprintf("var cySelectedNodesInputId = '%s';", ns("cySelectedNodes")))),
    
    
    radioGroupButtons(ns("selected_tissue"),"Choose a tissue :",choices = get_tissue_list(),justified = TRUE),
    reactableOutput(ns("network_tab")),
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    interactions <- reactiveVal(NULL)
    previous_selected_nodes <- reactiveVal(NULL)
    ns <- session$ns
    dt <- input_data("MR1507","all_genes")
    
    # pathway_dt <- unique(dt[grepl("Metabolic pathways", all_kegg_paths_name, fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    # tissue_dt <- unique(pathway_dt[tissue == "Breast"])
    # interactions <- get_string_interactions(tissue_dt[, feature_name])
    # network_json <- prepare_cytoscape_network(interactions, tissue_dt[, feature_name], tissue_dt[, log2FC])

    pathway_dt <- reactive({
      req(input$selected_pathway)
      unique(dt[grepl(input$selected_pathway, all_kegg_paths_name,fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    })

    tissue_dt <- reactive({
      req(input$selected_tissue)
      req(pathway_dt())
      unique(pathway_dt()[tissue == input$selected_tissue])
    })
    
    observe({
      req(tissue_dt())
      # Fetch STRING interactions for the current tissue
      message("Fetching STRING interactions")
      interactions(get_string_interactions(tissue_dt()[, feature_name]))
    })
    
    network_json <- reactive({
      req(tissue_dt(),interactions())
      # Prepare the Cytoscape network using the fetched interactions and log2FC values
      prepare_cytoscape_network(interactions(), tissue_dt()[, feature_name], tissue_dt()[, log2FC])
    })
  
    ###### network observeEvents #####

    observeEvent(list(input$selected_pathway, input$selected_tissue), {
      req(input$selected_pathway, input$selected_tissue)
      message("Selected pathway: ", input$selected_pathway, ", Selected tissue: ", input$selected_tissue)
      session$sendCustomMessage("cy-init", network_json())
      previous_selected_nodes(NULL)  # Resetujeme uložený výběr uzlů
    })
    
    # Plot subset network when nodes are selected
    observeEvent(list(input$selected_pathway,input$selected_tissue, input$cySelectedNodes), {
      req(interactions())
      selected_nodes <- input$cySelectedNodes
      previous_nodes <- previous_selected_nodes()
      
      if (!identical(selected_nodes, previous_nodes)) {
        # Aktualizujte pouze, pokud se výběr změnil a nastavíme novou hodnotu výběru
        previous_selected_nodes(selected_nodes)
        
        if (is.null(selected_nodes) || length(selected_nodes) == 0) {
          empty_json <- toJSON(list(elements = list(nodes = list(), edges = list())), auto_unbox = TRUE)
          session$sendCustomMessage("cy-subset", empty_json)
        } else {
          subnetwork_json <- prepare_cytoscape_network(interactions(), selected_nodes, tissue_dt()[feature_name %in% selected_nodes, log2FC])
          session$sendCustomMessage("cy-subset", subnetwork_json)
        }
      }
    })
    ####
    output$selected_nodes_text <- renderText({
      if (is.null(input$cySelectedNodes) || length(input$cySelectedNodes) == 0) {
        "Žádné uzly nejsou vybrány."
      } else {
        paste("Vybrané uzly: ", paste(input$cySelectedNodes, collapse = ", "))
      }
    })
    
    # Předat jmenný prostor do JavaScriptu - umožní  používat ns v JS
    output$js_namespace <- renderUI({
      tags$script(HTML(sprintf("var ns = '%s';", ns(""))))
    })
    
    
    observeEvent(input$selected_layout, {
      session$sendCustomMessage("cy-layout",input$selected_layout)
    })
    
    observeEvent(input$clearSelectionButton, {
      message("Clearing selection...")
      session$sendCustomMessage("cy-clear-selection", NULL)
    })
    
  ##### reactable calling #####
    
    output$network_tab <- renderReactable({
      message("Rendering Reactable for network")
      reactable(tissue_dt(),
                columns = list(
                  feature_name = colDef(name = "Gene name", maxWidth = 100),
                  geneid = colDef(name = "Ensembl id", width = 140),
                  refseq_id = colDef(name = "Refseq id", maxWidth = 80),
                  fc = colDef(name = "FC", maxWidth = 100),
                  all_kegg_paths_name = colDef(name = "Pathway name", minWidth = 140, maxWidth = 180, resizable = TRUE)
                ),
                defaultPageSize = 10,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 20, 50, 100),
                # onClick = JS(sprintf("function(rowInfo, column) {
                #   console.log('Clicked row index: ' + rowInfo.index);
                #   Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
                # }", session$ns("selected_row"))),
                onClick = JS(sprintf("function(rowInfo, column) {
                  console.log('Clicked row gene name: ' + rowInfo.row.feature_name);
                  Shiny.setInputValue('%s', { gene: rowInfo.row.feature_name }, { priority: 'event' });
                  backgroundColor: '#ADD8E6';
                }", session$ns("selected_row"))),
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE)
    })

    observeEvent(input$selected_row, {
      selected_gene <- input$selected_row
      message("Vybraný gen z tabulky: ", selected_gene)
      
      # Pošlete zprávu do JavaScriptu pro vybrání uzlu
      session$sendCustomMessage("cy-add-node-selection", list(gene = selected_gene))
      session$sendCustomMessage("highlight-row", list(gene = selected_gene))
    })
    
    
  })
}

# ui <- fluidPage(
#   titlePanel("Cytoscape.js v Shiny aplikaci"),
#   fui("network")
# )
# server <- function(input, output, session){
#   fserver("network")
# }
# shinyApp(ui, server, options = list(launch.browser = TRUE))


# json_to_dataframe <- function(json_str,gene_name) {
  # json_data <- fromJSON(fromJSON(network_json, simplifyVector = FALSE), simplifyVector = FALSE)
#   json_data$elements$nodes <- lapply(json_data$elements$nodes, function(node) {
#     if (node$data$name == gene_name) {
#       node$data$highlighted <- "yes"
#     }
#     return(node)
#   })
#   node_data <- lapply(json_data$elements$nodes, function(node) {
#     node$data
#   })
#   nodes_df <- bind_rows(node_data)
#   return(nodes_df)
# }
# json_to_dataframe(network_json,input$selected_row)

# library(shiny)
# library(rhino)
# library(htmltools)
# 
# fui <- function(id){
#     ns <- NS(id)
#     tagList(
# 
#       tags$head(
#         tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
#         tags$script(src = "static/js/cyjShiny_handlers.js")
#       ),
#       sidebarLayout(
#         sidebarPanel(),
#         mainPanel(
#           div(id = ns("cy"), style = "width: 600px; height: 600px;")
#         )
#       )
#     )
# }
# 
# fserver <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     observe({
#       session$sendCustomMessage(type = 'initCytoscape', message = list())
#     })
#   })
# }
# 
# ui <- fluidPage(
#   titlePanel("Cytoscape.js v Shiny aplikaci"),
#   fui("network")
# )
# server <- function(input, output, session){
#   fserver("network")
# }
# shinyApp(ui, server, options = list(launch.browser = TRUE))

