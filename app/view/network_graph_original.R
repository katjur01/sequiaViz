# # app/view/network_graph.R
# #
# # library(cyjShiny)
# # 
# # 
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
box::use(
  shiny[NS,moduleServer,observeEvent,tagList,actionButton,checkboxInput,fluidPage,textInput,req,includeScript,selectInput,icon,fluidRow,column,reactive],
  shinyjs[js,useShinyjs,extendShinyjs,onclick],
  htmltools[h2,h3,tags,div,br],
  data.table[fread,setnames],
  readxl[read_excel],
  reactable[reactable,colDef,renderReactable,reactableOutput]
)


jsCode <- "
shinyjs.loadStringData = function(params) {

    // Note:
    // 1. The 'identifiers' argument needs to be an array otherwise we get an
    //    error: TypeError: value.join is not a function.
    // 2. Best to let shinyjs match function arguments via shinyjs.getParams;
    //    see:
    //    https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-extend.html
    //    This also means we can avoid a req(input$gene) in onclick(`button`)


    
    var defaultParams = {
        organism : '9606',
        gene : 'TP53',
        structure_pics : '1',
        hide_node_labels : '0'
    };

    params = shinyjs.getParams(params, defaultParams);
    if (params.structure_pics){params.structure_pics = '0'} else {params.structure_pics = '1'}
    if (params.hide_node_labels){params.hide_node_labels = '1'} else {params.hide_node_labels = '0'}
    
    getSTRING('https://string-db.org', {
        'species': params.organism,
        'identifiers': [params.gene],
        'network_flavor':'confidence',
        'block_structure_pics_in_bubbles': params.structure_pics,
        'hide_node_labels' : params.hide_node_labels
    });
};


shinyjs.saveSVGasPNG = function() {

    // Initiate download of blob
    function download(filename, blob) {
        if (window.navigator.msSaveOrOpenBlob) {
            window.navigator.msSaveBlob(blob, filename);
        } else {
            const elem = window.document.createElement('a');
            elem.href = window.URL.createObjectURL(blob);
            elem.download = filename;
            document.body.appendChild(elem);
            elem.click();
            document.body.removeChild(elem);
        }
    };

    function scaleSVG(svg, width, height) {
        // Need to clone svg otherwise changes will affect the original SVG
        // Note to self: All function arguments in JS are mutable bindings
        var svg_scaled = svg.cloneNode(true);
        var orgWidth = parseFloat(svg.getAttribute(`width`));
        var orgHeight = parseFloat(svg.getAttribute(`height`));
        svg_scaled.setAttribute(`width`, width);
        svg_scaled.setAttribute(`height`, height);
        svg_scaled.setAttribute(`viewBox`, `0 0 ` + orgWidth + ` ` + orgHeight);
        return svg_scaled;
    };

    // Get SVG and rescale
    var svg = document.querySelector('#svg_network_image');
    var scaled_svg = scaleSVG(svg, width = 1280, height = 800);

    var data = (new XMLSerializer()).serializeToString(scaled_svg);

    var canvas = document.createElement('canvas');
    canvg(canvas, data, {
        renderCallback: function () {
            canvas.toBlob(function (blob) {
                   download('my_network.png', blob);
            });
        }
    });
};
"
ncbi_taxon_id <- list(
  "Homo sapiens" = 9606,
  "Mus musculus" = 10090,
  "Drosophila melanogaster" = 7227,
  "Arabidopsis thaliana" = 3702)

get_pathway_list <- function(){
  dt <- fread("input_files/testing_pathway_data.tsv")
  pathway_list <- unique(dt$kegg_paths_name)
  return(pathway_list)
}

ui <- function(id){
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    extendShinyjs(text = jsCode, functions = c("loadStringData","saveSVGasPNG")),
    includeScript("http://string-db.org/javascript/combined_embedded_network_v2.0.4.js"),
    includeScript("https://blueimp.github.io/JavaScript-Canvas-to-Blob/js/canvas-to-blob.js"),
    includeScript("https://cdnjs.cloudflare.com/ajax/libs/canvg/1.4/rgbcolor.min.js"),
    includeScript("https://cdnjs.cloudflare.com/ajax/libs/stackblur-canvas/1.4.1/stackblur.min.js"),
    includeScript("https://cdn.jsdelivr.net/npm/canvg/dist/browser/canvg.min.js"),
    # includeCSS("www/style.css"),

    ## I should map the proteins first ##
    ## https://string-db.org/api/tsv/get_string_ids?identifiers=p53%0dcdk2&species=9606 ##
    
    fluidRow(
      column(6,
             selectInput(ns("organism"), "Organism", ncbi_taxon_id),
             
             selectInput(ns("selected_pathway"), "Pathway", choices = get_pathway_list(), selected = NULL),
             br(),
             h2("Network display options:"),
             checkboxInput(ns("structure_pics"),label = "enable structure pictures inside the bubble", FALSE),
             checkboxInput(ns("hide_node_labels"),label = "hides all protein names from the picture", FALSE),
             actionButton(ns("gene_button"), "Show network!"),
             actionButton(ns("button_save"), "Save network as PNG"),
             
             tags$div(id = "stringEmbedded")
             ),
      column(6,
             reactableOutput(ns("network_tab"))
             )
    )
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    dt <- fread("input_files/testing_pathway_data.tsv")
    
    gene_list <- reactive({
      req(input$selected_pathway)
      unique(dt[kegg_paths_name == input$selected_pathway])
    })

    output$network_tab <- renderReactable({
      message("Rendering Reactable for network")
      reactable(gene_list())
                        
    })

    
    observeEvent(input$gene_button, {
      # gene_expr <- gene_list()$P_001  # Assuming your data frame has expression values
      js$loadStringData(input$organism, paste(gene_list()$gene_name, collapse = "%0D"),input$structure_pics,input$hide_node_labels)
      # js$colorNodesByExpression(gene_expr)
    })

    observeEvent(input$button_save, {
      js$saveSVGasPNG()
    })
    
  })
}

# shinyApp(ui, server)