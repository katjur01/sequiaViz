# app/view/network_graph.R



box::use(
  shiny[NS, moduleServer, observeEvent, tagList, fluidPage, fluidRow, column, textInput, actionButton, selectInput, reactive, req,reactiveVal,conditionalPanel],
  httr[GET, status_code, content],
  htmltools[h3, tags, div,HTML],
  jsonlite[fromJSON, toJSON],
  cyjShiny[cyjShinyOutput, renderCyjShiny, cyjShiny, dataFramesToJSON,setNodeAttributes],
  data.table[fread,setnames],
  readxl[read_excel],
  graph[nodes],
  reactable[reactable,colDef,renderReactable,reactableOutput,JS],
  shinyWidgets[radioGroupButtons,pickerInput],

)

box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/waiters[use_spinner],
  
)

# Funkce pro získání interakcí mezi proteiny z STRING API
get_string_interactions <- function(proteins, species = 9606, chunk_size = 100) {
  # Funkce pro odesílání jednotlivých požadavků
  fetch_interactions <- function(protein_chunk) {
    base_url <- "https://string-db.org/api/json/network?"
    query <- paste0("identifiers=", paste(protein_chunk, collapse = "%0D"), "&species=", species)
    url <- paste0(base_url, query)
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      content <- fromJSON(content(response, as = "text"))
      return(content)
    } else {
      stop("Request failed with status: ", status_code(response))
    }
  }
  
  # Rozdělení proteinů na bloky podle chunk_size (občas je proteinů moc)
  protein_chunks <- split(proteins, ceiling(seq_along(proteins) / chunk_size))
  all_interactions <- do.call(rbind, lapply(protein_chunks, fetch_interactions))
  
  return(all_interactions)
}


prepare_cytoscape_network <- function(interactions, proteins, fc_values) {
  # Získání uzlů z interakcí
  interaction_nodes <- unique(c(interactions$preferredName_A, interactions$preferredName_B))
  all_nodes <- unique(c(interaction_nodes, proteins))
  
  # Spočítání stupně (degree) pro každý uzel - singletony mají stupen 0
  degrees <- table(c(interactions$preferredName_A, interactions$preferredName_B))
  degree_values <- sapply(all_nodes, function(x) ifelse(x %in% names(degrees), degrees[x], 0))
  
  # Příprava uzlů včetně fold-change hodnot, fc a label
  node_data <- data.frame(
    id = all_nodes,
    name = all_nodes,
    label = all_nodes,
    log2FC = ifelse(all_nodes %in% proteins, fc_values[match(all_nodes, proteins)], NA),  # Přidání sloupce fc
    degree = degree_values,  # Přidání stupně (degree) uzlu
    stringsAsFactors = FALSE
  )
  
  # Příprava hran (interakcí)
  edges <- data.frame(
    source = interactions$preferredName_A,
    target = interactions$preferredName_B,
    interaction = "interaction",  # Obecný popis interakce
    stringsAsFactors = FALSE
  )
  
  # Generování JSON pro cyjShiny
  network_json <- toJSON(dataFramesToJSON(edges, node_data), auto_unbox = TRUE)
  return(network_json)
}

get_pathway_list <- function(){
  dt <- fread("input_files/kegg_tab.tsv")
  pathway_list <- sort(unique(dt$kegg_paths_name))
  return(pathway_list)
}

get_tissue_list <- function(){
  input_files <- get_inputs("per_sample_file")
  tissue_list <- sort(unique(gsub(".*/RNAseq21_NEW/[^/]+/([^_]+)_.*", "\\1", input_files$expression.file)))
  return(tissue_list)
}

input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file")
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag)
  # data <- data[kegg_paths_name != ""]
  return(data)
}


# Shiny aplikace UI modul
ui <- function(id) {
  ns <- NS(id)
  tagList(
     tags$script(src = "static/js/cyjShiny_handlers.js"),
     tags$script(src = "static/js/reactable_handlers.js"),
     tags$style(HTML("
        .selected {
          background-color: lightblue !important;
        }")),
     
     pickerInput(ns("selected_pathway"), "Pathway", choices = get_pathway_list(), options = list(`live-search` = TRUE)), #choices = c("", get_pathway_list())
     
     textInput(ns("proteins"), "List of gene names (comma-separated)", value = ""),
     # actionButton(ns("plot"), "Plot Network"),

     # cyjShinyOutput(ns("cyj_network"), height = "900px"),
     
     use_spinner(cyjShinyOutput(ns("cyj_network"), height = "900px")), # conditionalPanel is not working for some reason!!!
     
     radioGroupButtons(ns("selected_tissue"),"Choose a tissue :",choices = get_tissue_list(),justified = TRUE),
     reactableOutput(ns("network_tab"))
     # use_spinner(cyjShinyOutput(ns("cyj_network"), height = "900px"))
  )
}

# Shiny aplikace server modul
server <- function(id) {
  moduleServer(id, function(input, output, session) {

    dt <- input_data("MR1507","all_genes")
    
    pathway_dt <- reactive({
      req(input$selected_pathway)
      unique(dt[grep(input$selected_pathway, all_kegg_paths_name),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    })
    
    tissue_dt <- reactive({
      req(input$selected_tissue)
      req(pathway_dt())
      unique(pathway_dt()[tissue == input$selected_tissue])
    })
    

    network_json <- reactive({
      req(tissue_dt())
      # Fetch STRING interactions for the current tissue
      interactions <- get_string_interactions(tissue_dt()[, feature_name])
      # Prepare the Cytoscape network using the fetched interactions and log2FC values
      prepare_cytoscape_network(interactions, tissue_dt()[, feature_name], tissue_dt()[, log2FC])
    })
    
    
    
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
                # selection = "single",
                # onClick = "select",
                onClick = JS("function(rowInfo) { 
                  Shiny.setInputValue('selected_row', rowInfo.index);}"),
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE)
    })
    
    
    
    observeEvent(input$selected_pathway, {
      req(input$selected_pathway != "")
      message("Selected pathway: ", input$selected_pathway)
      output$cyj_network <- renderCyjShiny({
        cyjShiny(network_json(), layoutName = "cola", styleFile = "app/styles/cytoscape_styling.js")
      })
      # 
      # session$sendCustomMessage(type = "initializeCy", message = list())
    })
    
    # Aktualizace uzlů na základě výběru tkáně bez opětovného renderování grafu
    observeEvent(input$selected_tissue, {
      req(tissue_dt(), input$selected_tissue != "")
      message("Selected tissue: ", input$selected_tissue)
      session$sendCustomMessage(type = "updateTissueStyle", message = list(tissue = input$selected_tissue))
    })
    
    # Color selected row (without checkbox)
    observeEvent(input$selected_row, {
      message("Row selected: ", input$selected_row)  # Log pro kontrolu, zda se kliknutí na řádek detekuje
      session$sendCustomMessage(type = "highlightRow", message = list(rowIndex = input$selected_row))
    })
    
    
    
    
    
  })
}

# #fusion
# input_data <- function(sample,expr_flag = NULL){
#   filenames <- get_inputs("per_sample_file")
# 
#   fusion <- as.data.table(prepare_fusion_genes_table(load_data(filenames$fusions,"fusion",sample),sample))
#   germline <- as.data.table(prepare_germline_table(load_data(filenames$var_call.germline,"varcall",sample)))
#   # expression <- as.data.table(prepare_expression_table(load_data(input_files$expression.files,"expression",sample,expr_flag = "all_genes"),expr_flag = "all_genes"))
#   
#   
#   germline[!is.na(Gene_symbol), n_mut := .N,.(Gene_symbol)]
#   
#   fusion[,visual_check := FALSE]
#   fusion[gene1 == "RUNX2",visual_check := TRUE]
#   fusion[,fus_id := 1:length(gene1)]
#   expression[feature_name %in% fusion$gene1]$feature_name
#   
#   
#   
#   return(list(fusion,germline,expression))
# }
# 
# 
# dt <- reactive({
#   message("Loading input data for fusion")
#   input_data(selected_samples) 
# })
# 
# 
# create_omic_table <- function(){
#   
# }








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
# 
# box::use(
#   shiny[NS,moduleServer,observeEvent,tagList,actionButton,checkboxInput,fluidPage],
#   shinyjs[js,useShinyjs,extendShinyjs],
#   htmltools[h3,tags,div],
#   data.table[fread,setnames],
#   readxl[read_excel]
# )
# 
# box::use(
#   app/js/cytoscape_js[jsCode, custom_setting],
#   # app/logic/waiters[use_spinner],
# )
# 
# fui <- function(id){
#   ns <- NS(id)
#   tagList(
#     useShinyjs(),
#     extendShinyjs(text = jsCode, functions = c("loadStringData")),
#     tags$head(
#       tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
#       tags$script(custom_setting)
#     ),
#     actionButton(ns("button"), "Show"),
#     checkboxInput(ns("show_singletons"), "Show singletons", value = TRUE),
#     h3("Network:"),
#     div(id = "cy", style = "width: 800px; height: 600px;"),
#   )
# }
# 
# fserver <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     # data <- fread("input_files/testing_pathway_data.tsv")
#     # data <- data[kegg_paths_name != ""]
#     # setnames(data,"P_001","FC")
#     # genes <- data[kegg_paths_name == "Metabolic pathways",gene_name][1:10]
#     # fc_values <- data[kegg_paths_name == "Metabolic pathways",FC]
# 
#     # Načtení dat z Excelu
#     data <- read_excel("input_files/MOII_e117/RNAseq21/DZ1601_032023_Blood Vessel_report.xlsx", sheet = 1)
#     genes <- data$Gene
#     fc_values <- data$FC
# 
#     observeEvent(input$button, {
#       genes <- paste(genes, collapse = "%0D")
#       js$loadStringData(genes)
#     })
# # 
# #     observeEvent(input$button, {
# #       shinyjs::runjs("$('#cy').addClass('loading-spinner')")  # Přidej spinner
# # 
# #       genes <- paste(genes, collapse = "%0D")
# #       print(genes)
# #       print(js$loadStringData(genes))
# #       js$loadStringData(genes)
# # 
# #       # Po načtení dat ze sítě
# #       shinyjs::delay(3000, {
# #         shinyjs::runjs("$('#cy').removeClass('loading-spinner')")  # Odeber spinner po 3s (pro test)
# #       })
# #     })
# # 
# 
# 
# 
#     observeEvent(input$stringData, {
#       session$sendCustomMessage(type = "initCytoscape", message = list(elements = input$stringData))
#     })
# 
#     observeEvent(input$show_singletons, {
#       session$sendCustomMessage(type = "toggleSingletons", message = list(show = input$show_singletons))
#     })
#   })
# }
# # 
# # ui <- fluidPage(
# #   fui("network")
# # )
# # server <- function(input, output, session){
# #   fserver("network")
# # }
# # shinyApp(ui, server, options = list(launch.browser = TRUE))
# 
# 
# # 
# # # Převod dat do formátu JSON pro JavaScript
# # gene_data <- lapply(1:nrow(data), function(i) {
# #   list(
# #     Gene = data$Gene[i],
# #     FC = data$FC[i]
# #   )
# # })
# # genes_json <- toJSON(gene_data, auto_unbox = TRUE)
# #
# 

# ###############################################
# ##  USING STRING APP FOR NETWORK EXTRACTING  ##
# ###############################################
# box::use(
#   shiny[NS,moduleServer,observeEvent,tagList,actionButton,checkboxInput,fluidPage,textInput,req,includeScript,selectInput,icon,fluidRow,column,reactive],
#   shinyjs[js,useShinyjs,extendShinyjs,onclick],
#   htmltools[h2,h3,tags,div,br],
#   data.table[fread,setnames],
#   readxl[read_excel],
#   reactable[reactable,colDef,renderReactable,reactableOutput]
# )
# 
# 
# jsCode <- "
# shinyjs.loadStringData = function(params) {
# 
#     // Note:
#     // 1. The 'identifiers' argument needs to be an array otherwise we get an
#     //    error: TypeError: value.join is not a function.
#     // 2. Best to let shinyjs match function arguments via shinyjs.getParams;
#     //    see:
#     //    https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-extend.html
#     //    This also means we can avoid a req(input$gene) in onclick(`button`)
# 
# 
#     
#     var defaultParams = {
#         organism : '9606',
#         gene : 'TP53',
#         structure_pics : '1',
#         hide_node_labels : '0'
#     };
# 
#     params = shinyjs.getParams(params, defaultParams);
#     if (params.structure_pics){params.structure_pics = '0'} else {params.structure_pics = '1'}
#     if (params.hide_node_labels){params.hide_node_labels = '1'} else {params.hide_node_labels = '0'}
# 
#     getSTRING('https://string-db.org/', {
#         'species': params.organism,
#         'identifiers': [params.gene],
#         'network_flavor':'confidence',
#         'block_structure_pics_in_bubbles': params.structure_pics,
#         'hide_node_labels' : params.hide_node_labels
#     });
# };
# 
# 
# shinyjs.saveSVGasPNG = function() {
# 
#     // Initiate download of blob
#     function download(filename, blob) {
#         if (window.navigator.msSaveOrOpenBlob) {
#             window.navigator.msSaveBlob(blob, filename);
#         } else {
#             const elem = window.document.createElement('a');
#             elem.href = window.URL.createObjectURL(blob);
#             elem.download = filename;
#             document.body.appendChild(elem);
#             elem.click();
#             document.body.removeChild(elem);
#         }
#     };
# 
#     function scaleSVG(svg, width, height) {
#         // Need to clone svg otherwise changes will affect the original SVG
#         // Note to self: All function arguments in JS are mutable bindings
#         var svg_scaled = svg.cloneNode(true);
#         var orgWidth = parseFloat(svg.getAttribute(`width`));
#         var orgHeight = parseFloat(svg.getAttribute(`height`));
#         svg_scaled.setAttribute(`width`, width);
#         svg_scaled.setAttribute(`height`, height);
#         svg_scaled.setAttribute(`viewBox`, `0 0 ` + orgWidth + ` ` + orgHeight);
#         return svg_scaled;
#     };
# 
#     // Get SVG and rescale
#     var svg = document.querySelector('#svg_network_image');
#     var scaled_svg = scaleSVG(svg, width = 1280, height = 800);
# 
#     var data = (new XMLSerializer()).serializeToString(scaled_svg);
# 
#     var canvas = document.createElement('canvas');
#     canvg(canvas, data, {
#         renderCallback: function () {
#             canvas.toBlob(function (blob) {
#                    download('my_network.png', blob);
#             });
#         }
#     });
# };
# "
# ncbi_taxon_id <- list(
#   "Homo sapiens" = 9606,
#   "Mus musculus" = 10090,
#   "Drosophila melanogaster" = 7227,
#   "Arabidopsis thaliana" = 3702)
# 
# get_pathway_list <- function(){
#   dt <- fread("input_files/testing_pathway_data.tsv")
#   pathway_list <- unique(dt$kegg_paths_name)
#   return(pathway_list)
# }
# 
# ui <- function(id){
#   ns <- NS(id)
#   fluidPage(
#     useShinyjs(),
#     extendShinyjs(text = jsCode, functions = c("loadStringData","saveSVGasPNG")),
#     includeScript("http://string-db.org/javascript/combined_embedded_network_v2.0.4.js"),
#     includeScript("https://blueimp.github.io/JavaScript-Canvas-to-Blob/js/canvas-to-blob.js"),
#     includeScript("https://cdnjs.cloudflare.com/ajax/libs/canvg/1.4/rgbcolor.min.js"),
#     includeScript("https://cdnjs.cloudflare.com/ajax/libs/stackblur-canvas/1.4.1/stackblur.min.js"),
#     includeScript("https://cdn.jsdelivr.net/npm/canvg/dist/browser/canvg.min.js"),
#     # includeCSS("www/style.css"),
# 
#     ## I should map the proteins first ##
#     ## https://string-db.org/api/tsv/get_string_ids?identifiers=p53%0dcdk2&species=9606 ##
#     
#     fluidRow(
#       column(6,
#              selectInput(ns("organism"), "Organism", ncbi_taxon_id),
#              
#              selectInput(ns("selected_pathway"), "Pathway", choices = get_pathway_list(), selected = NULL),
#              br(),
#              h2("Network display options:"),
#              checkboxInput(ns("structure_pics"),label = "enable structure pictures inside the bubble", FALSE),
#              checkboxInput(ns("hide_node_labels"),label = "hides all protein names from the picture", FALSE),
#              actionButton(ns("gene_button"), "Show network!"),
#              actionButton(ns("button_save"), "Save network as PNG"),
#              
#              tags$div(id = "stringEmbedded")
#              ),
#       column(6,
#              reactableOutput(ns("network_tab"))
#              )
#     )
#   )
# }
# 
# server <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     
#     dt <- fread("input_files/testing_pathway_data.tsv")
#     
#     gene_list <- reactive({
#       req(input$selected_pathway)
#       unique(dt[kegg_paths_name == input$selected_pathway])
#     })
# 
#     output$network_tab <- renderReactable({
#       message("Rendering Reactable for network")
#       reactable(gene_list())
#                         
#     })
# 
#     
#     observeEvent(input$gene_button, {
#       # gene_expr <- gene_list()$P_001  # Assuming your data frame has expression values
#       js$loadStringData(input$organism, paste(gene_list()$gene_name, collapse = "%0D"),input$structure_pics,input$hide_node_labels)
#       # js$colorNodesByExpression(gene_expr)
#     })
# 
#     observeEvent(input$button_save, {
#       js$saveSVGasPNG()
#     })
#     
#   })
# }
# 
# # shinyApp(ui, server)