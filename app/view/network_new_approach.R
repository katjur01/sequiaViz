# # app/view/network_graph.R
# # shiny::shinyAppDir(".", options = list(launch.browser = TRUE))
# 
# 
# 
# box::use(
#   shiny[NS, moduleServer, observeEvent, observe, tagList, fluidPage, fluidRow, column, textInput, actionButton, selectInput, reactive, req,reactiveVal,conditionalPanel,verbatimTextOutput,
#         renderPrint,renderText],
#   httr[GET, status_code, content],
#   htmltools[h3, tags, div,HTML],
#   jsonlite[fromJSON, toJSON],
#   cyjShiny[cyjShinyOutput, renderCyjShiny, cyjShiny, dataFramesToJSON,setNodeAttributes],
#   data.table[fread,setnames],
#   readxl[read_excel],
#   graph[nodes],
#   reactable[reactable,colDef,renderReactable,reactableOutput,JS],
#   shinyWidgets[radioGroupButtons,pickerInput],
#   
# )
# 
# box::use(
#   app/logic/load_data[get_inputs,load_data],
#   app/logic/waiters[use_spinner],
#   
# )
# 
# # Funkce pro získání interakcí mezi proteiny z STRING API
# get_string_interactions <- function(proteins, species = 9606, chunk_size = 100) {
#   # Funkce pro odesílání jednotlivých požadavků
#   fetch_interactions <- function(protein_chunk) {
#     base_url <- "https://string-db.org/api/json/network?"
#     query <- paste0("identifiers=", paste(protein_chunk, collapse = "%0D"), "&species=", species)
#     url <- paste0(base_url, query)
#     
#     response <- GET(url)
#     
#     if (status_code(response) == 200) {
#       content <- fromJSON(content(response, as = "text"))
#       return(content)
#     } else {
#       stop("Request failed with status: ", status_code(response))
#     }
#   }
#   
#   # Rozdělení proteinů na bloky podle chunk_size (občas je proteinů moc)
#   protein_chunks <- split(proteins, ceiling(seq_along(proteins) / chunk_size))
#   all_interactions <- do.call(rbind, lapply(protein_chunks, fetch_interactions))
#   
#   return(all_interactions)
# }
# 
# 
# prepare_cytoscape_network <- function(interactions, proteins, fc_values) {
#   # Získání uzlů z interakcí
#   interaction_nodes <- unique(c(interactions$preferredName_A, interactions$preferredName_B))
#   all_nodes <- unique(c(interaction_nodes, proteins))
#   
#   # Spočítání stupně (degree) pro každý uzel - singletony mají stupen 0
#   degrees <- table(c(interactions$preferredName_A, interactions$preferredName_B))
#   degree_values <- sapply(all_nodes, function(x) ifelse(x %in% names(degrees), degrees[x], 0))
#   
#   # Příprava uzlů včetně fold-change hodnot, fc a label
#   node_data <- data.frame(
#     id = all_nodes,
#     name = all_nodes,
#     label = all_nodes,
#     log2FC = ifelse(all_nodes %in% proteins, fc_values[match(all_nodes, proteins)], NA),  # Přidání sloupce fc
#     degree = degree_values,  # Přidání stupně (degree) uzlu
#     stringsAsFactors = FALSE
#   )
#   
#   # Příprava hran (interakcí)
#   edges <- data.frame(
#     source = interactions$preferredName_A,
#     target = interactions$preferredName_B,
#     interaction = "interaction",  # Obecný popis interakce
#     stringsAsFactors = FALSE
#   )
#   
#   # Generování JSON pro cyjShiny
#   network_json <- toJSON(dataFramesToJSON(edges, node_data), auto_unbox = TRUE)
#   return(network_json)
# }
# 
# get_pathway_list <- function(){
#   dt <- fread("input_files/kegg_tab.tsv")
#   pathway_list <- sort(unique(dt$kegg_paths_name))
#   return(pathway_list)
# }
# 
# get_tissue_list <- function(){
#   input_files <- get_inputs("per_sample_file")
#   tissue_list <- sort(unique(gsub(".*/RNAseq21_NEW/[^/]+/([^_]+)_.*", "\\1", input_files$expression.file)))
#   return(tissue_list)
# }
# 
# input_data <- function(sample,expr_flag){
#   input_files <- get_inputs("per_sample_file")
#   # message("Loading data for expression profile: ", filenames$expression.files)
#   data <- load_data(input_files$expression.files,"expression",sample,expr_flag)
#   # data <- data[kegg_paths_name != ""]
#   return(data)
# }
# 
# 
# # Shiny aplikace UI modul
# ui <- function(id) {
#   ns <- NS(id)
#   tagList(
#     # tags$script(src = "static/js/cyjShiny_handlers.js"),
#     pickerInput(ns("selected_pathway"), "Pathway", choices = get_pathway_list(), options = list(`live-search` = TRUE)), #choices = c("", get_pathway_list())
#     textInput(ns("proteins"), "List of gene names (comma-separated)", value = ""),
#     use_spinner(cyjShinyOutput(ns("cyj_network"), height = "900px")), # conditionalPanel is not working for some reason!!!
#     
#     radioGroupButtons(ns("selected_tissue"),"Choose a tissue :",choices = get_tissue_list(),justified = TRUE),
#     reactableOutput(ns("network_tab"))
#   )
# }
# 
# # Shiny aplikace server modul
# server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     
#     dt <- input_data("MR1507","all_genes")
#     
#     pathway_dt <- reactive({
#       req(input$selected_pathway)
#       unique(dt[grep(input$selected_pathway, all_kegg_paths_name),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
#     })
#     
#     tissue_dt <- reactive({
#       req(input$selected_tissue)
#       req(pathway_dt())
#       unique(pathway_dt()[tissue == input$selected_tissue])
#     })
#     
#     
#     network_json <- reactive({
#       req(tissue_dt())
#       # Fetch STRING interactions for the current tissue
#       interactions <- get_string_interactions(tissue_dt()[, feature_name])
#       # Prepare the Cytoscape network using the fetched interactions and log2FC values
#       prepare_cytoscape_network(interactions, tissue_dt()[, feature_name], tissue_dt()[, log2FC])
#     })
#     
#     
#     
#     output$network_tab <- renderReactable({
#       message("Rendering Reactable for network")
#       reactable(tissue_dt(),
#                 columns = list(
#                   feature_name = colDef(name = "Gene name", maxWidth = 100),
#                   geneid = colDef(name = "Ensembl id", width = 140),
#                   refseq_id = colDef(name = "Refseq id", maxWidth = 80),
#                   fc = colDef(name = "FC", maxWidth = 100),
#                   all_kegg_paths_name = colDef(name = "Pathway name", minWidth = 140, maxWidth = 180, resizable = TRUE)
#                 ),
#                 defaultPageSize = 10,
#                 showPageSizeOptions = TRUE,
#                 pageSizeOptions = c(10, 20, 50, 100),
#                 # selection = "single",
#                 # onClick = JS("function(rowInfo) { 
#                 #   console.log('Clicked row index: ' + rowInfo.index);  // Tiskne do konzole při kliknutí
#                 #   Shiny.setInputValue('selected_row', rowInfo.index + 1, {priority: 'event'});
#                 # }"),
#                 onClick = JS(sprintf("function(rowInfo, column) {
#                   console.log('Clicked row index: ' + rowInfo.index);
#                   Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
#                 }", session$ns("selected_row"))),
#                 
#                 striped = TRUE,
#                 wrap = FALSE,
#                 highlight = TRUE,
#                 outlined = TRUE)
#     })
#     
#     observeEvent(input$selected_pathway, {
#       req(input$selected_pathway != "")
#       message("Selected pathway: ", input$selected_pathway)
#       output$cyj_network <- renderCyjShiny({
#         cyjShiny(network_json(), layoutName = "cola", styleFile = "app/styles/cytoscape_styling.js")
#       })
#     })
#     
#     
#     # observeEvent(input$selected_row, {
#     #   selected_gene <- tissue_dt()[input$selected_row$index, "feature_name"]
#     #   setNodeAttributes(session,
#     #                     attribute = "color",
#     #                     nodes = selected_gene,
#     #                     values = list(selected_gene = "yellow"))
#     # })
#     
#     
#   })
# }
# 







# # preset_graph_file <- system.file("/Users/katerinajuraskova/Desktop/sequiaViz/STRING network (physical)--clustered.cyjs")
# # graph_json <- readAndStandardizeJSONNetworkFile(preset_graph_file)
# # writeLines(graph_json, "cyjshiny_cytoscape_desktop.cyjs")
# # 
# # cyjShiny(graph_json, layoutName="preset")
# 
# ##############################################
# ##############################################
# 
# # dt <- fread("input_files/testing_pathway_data.tsv")
# # dt <- dt[kegg_paths_name != ""]
# #
# # library(shiny)
# # library(shinyjs)
# # library(jsonlite)
# # library(readxl)

box::use(
  shiny[NS,moduleServer,observeEvent,tagList,actionButton,checkboxInput,fluidPage],
  shinyjs[js,useShinyjs,extendShinyjs],
  htmltools[h3,tags,div],
  data.table[fread,setnames],
  readxl[read_excel]
)

box::use(
  app/js/cytoscape_js[jsCode, custom_setting],
  # app/logic/waiters[use_spinner],
)

fui <- function(id){
  ns <- NS(id)
  tagList(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("loadStringData")),
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
      tags$script(custom_setting)
    ),
    actionButton(ns("button"), "Show"),
    checkboxInput(ns("show_singletons"), "Show singletons", value = TRUE),
    h3("Network:"),
    div(id = "cy", style = "width: 800px; height: 600px;"),
  )
}

fserver <- function(id) {
  moduleServer(id, function(input, output, session) {
    # data <- fread("input_files/testing_pathway_data.tsv")
    # data <- data[kegg_paths_name != ""]
    # setnames(data,"P_001","FC")
    # genes <- data[kegg_paths_name == "Metabolic pathways",gene_name][1:10]
    # fc_values <- data[kegg_paths_name == "Metabolic pathways",FC]

    # Načtení dat z Excelu
    data <- read_excel("../../input_files/MOII_e117/RNAseq21/DZ1601_032023_Blood Vessel_report.xlsx", sheet = 1)
    genes <- data$Gene
    fc_values <- data$FC

    observeEvent(input$button, {
      genes <- paste(genes, collapse = "%0D")
      js$loadStringData(genes)
    })
#
#     observeEvent(input$button, {
#       shinyjs::runjs("$('#cy').addClass('loading-spinner')")  # Přidej spinner
#
#       genes <- paste(genes, collapse = "%0D")
#       print(genes)
#       print(js$loadStringData(genes))
#       js$loadStringData(genes)
#
#       # Po načtení dat ze sítě
#       shinyjs::delay(3000, {
#         shinyjs::runjs("$('#cy').removeClass('loading-spinner')")  # Odeber spinner po 3s (pro test)
#       })
#     })
#



    observeEvent(input$stringData, {
      session$sendCustomMessage(type = "initCytoscape", message = list(elements = input$stringData))
    })

    observeEvent(input$show_singletons, {
      session$sendCustomMessage(type = "toggleSingletons", message = list(show = input$show_singletons))
    })
  })
}

ui <- fluidPage(
  fui("network")
)
server <- function(input, output, session){
  fserver("network")
}
shinyApp(ui, server, options = list(launch.browser = TRUE))
