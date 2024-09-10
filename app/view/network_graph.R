library(cyjShiny)


preset_graph_file <- system.file("/Users/katerinajuraskova/Desktop/sequiaViz/STRING network (physical)--clustered.cyjs")
graph_json <- readAndStandardizeJSONNetworkFile(preset_graph_file)
writeLines(graph_json, "cyjshiny_cytoscape_desktop.cyjs")

cyjShiny(graph_json, layoutName="preset")




# library(shiny)
# library(shinyjs)
# library(jsonlite)
# library(readxl)
# 
# box::use(
#   app/js/cytoscape_js[jsCode, custom_setting],
# )
# 
# # Načtení dat z Excelu
# data <- read_excel("../../input_files/MOII_e117/RNAseq21/DZ1601_032023_Blood Vessel_report.xlsx", sheet = 1)
# genes <- data$Gene
# fc_values <- data$FC
# 
# # Převod dat do formátu JSON pro JavaScript
# gene_data <- lapply(1:nrow(data), function(i) {
#   list(
#     Gene = data$Gene[i],
#     FC = data$FC[i]
#   )
# })
# genes_json <- toJSON(gene_data, auto_unbox = TRUE)
# 
# ui <- fluidPage(
#   useShinyjs(),
#   extendShinyjs(text = jsCode, functions = c("loadStringData")),
#   tags$head(
#     tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
#     tags$script(custom_setting)
#   ),
#   actionButton("button", "Show"),
#   checkboxInput("show_singletons", "Show singletons", value = TRUE),
#   h3("Network:"),
#   div(id = "cy", style = "width: 800px; height: 600px;")
# )
# 
# server <- function(input, output, session) {
#   observeEvent(input$button, {
#     genes <- paste(genes, collapse = "%0D")
#     js$loadStringData(genes)
#   })
#   
#   observeEvent(input$stringData, {
#     session$sendCustomMessage(type = "initCytoscape", message = list(elements = input$stringData))
#   })
#   
#   observeEvent(input$show_singletons, {
#     session$sendCustomMessage(type = "toggleSingletons", message = list(show = input$show_singletons))
#   })
#   
# }
# 
# shinyApp(ui, server, options = list(launch.browser = TRUE))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###############################################
# ##  USING STRING APP FOR NETWORK EXTRACTING  ##
# ###############################################
# 
# # jsCode <- "
# # shinyjs.loadStringData = function(gene) {
# #     getSTRING('https://string-db.org', {
# #         'ncbiTaxonId':'9606',
# #         'identifiers': gene,
# #         'network_flavor':'confidence'})
# # }"
# # 
# # commandsRun(string.cmd)
# # ui <- fluidPage(
# #   useShinyjs(),
# #   extendShinyjs(text = jsCode, functions = c("loadStringData")),
# #   tags$head(tags$script(src = "http://string-db.org/javascript/combined_embedded_network_v2.0.4.js")),
# #   textInput("gene", "Gene symbol", value = "TP53"),
# #   actionButton("button", "Show"),
# #   h3("Network:"),
# #   tags$div(id = "stringEmbedded")
# # )
# # 
# # server <- function(input, output, session) {
# #   
# #   observeEvent(input$button, {
# #     req(input$gene)
# #     js$loadStringData(input$gene)
# #   })
# #   
# # }
# # 
# # shinyApp(ui, server)
