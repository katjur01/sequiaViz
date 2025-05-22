box::use(
  shiny[br, NS,h3, tagList, div, observe, observeEvent, mainPanel, titlePanel, uiOutput, renderUI, HTML, fluidPage,fluidRow, moduleServer,
        reactiveValues, column,req],
  htmltools[tags],
  bs4Dash[actionButton,box],
  shinyjs[useShinyjs, runjs],
  reactable,
  reactable[reactableOutput,renderReactable,reactable,JS,colDef],
)

# start_server <- function(){
#   setwd("/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze/mapped/")
#   data_server = serve_data('./')
#   data_server$stop_server()
# }

#' @export
igv_ui <- function(id) {
  ns <- NS(id)
  
  # titlePanel("IGV Viewer")
  tagList(
    tags$style(HTML("
       #igv_page {
          width: 100%;
          height: 600px;
          margin: 0 auto;
          padding: 20px;
          box-sizing: border-box;
      }
      #igv-igvDiv {
          width: 100%;
          height: auto;
          border: none; 
          margin: 0 auto;
          padding: 20px;
          box-sizing: border-box;
      }
  ")),
    useShinyjs(),
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/igv@2.15.12/dist/igv.min.js")
    ),
    fluidRow(
      column(6, reactableOutput(ns("bookmarks")))),
    br(),
    fluidRow(
      column(2, actionButton(ns("loadIGVButton"), "Load IGV Viewer"))
    ),
    
    br(),
    uiOutput(ns("igvDivOutput"))
    
  )
}

#' @export
igv_server <- function(id,shared_data) {
  moduleServer(id, function(input, output, session) {
    
    #values <- reactiveValues()
    
    # values$bookmark_df <- data.frame(
    #   gene1 = c("KANSL1", "KMT2A", "METTL13"),
    #   #gene2 = c("LRRC37A4P", "MLLT3", "DNM3"),
    #   position1 = c("17:45597764", "11:118482495", "1:171814013")
    #   #position2 = c("17:45545676", "11:20365744", "1:171987759")
    # )
    
    #values$bookmark_df <- shared_data$selected_somatic_variants
    
    output$bookmarks <- renderReactable({
      req(shared_data$selected_somatic_variants)
      reactable(shared_data$selected_somatic_variants,
                pagination = FALSE,
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE,
                defaultColDef = colDef(resizable = TRUE, show = TRUE, align = "center"),
                columns = list("var_name"= colDef(name="Variant name"),
                               "Gene_symbol"=colDef(name="Gene symbol"),
                               "tumor_variant_freq"=colDef(show=FALSE),
                               "clinvar_sig"=colDef(name="ClinVar significance"),
                               "position1"=colDef(show=FALSE)),
                onClick = JS(sprintf("function(rowInfo, column) {
                  Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
                }", session$ns("bookmarks_click")))
      )
    })
    
    observeEvent(input$loadIGVButton, {
      # Krok 1: Vytvoření divu pro IGV
      output$igvDivOutput <- renderUI({
        div(id = session$ns("igv-igvDiv"))  # Zajistíme, že div má správný namespace
      })
      
      # Krok 2: Po vykreslení spustíme JavaScript pro IGV s mírným zpožděním
      runjs(sprintf("
        setTimeout(function() {
          var igvDiv = document.getElementById('%s');
          if (igvDiv) {
            var options = {
              genome: 'hg38',
              locus: 'all',
              tracks: [
                {
                  name: 'Local BAM Track',
                  type: 'alignment',
                  format: 'bam',
                  url: 'DZ1601krev.bam',
                  indexURL: 'DZ1601krev .bam.bai'
                }
            ]
            };
            igv.createBrowser(igvDiv, options).then(function(browser) {
              console.log('IGV browser created');
              window.igvBrowser = browser;
            });
          } else {
            console.error('IGV div not found.');
          }
        }, 20);  // Zpoždění 20 ms k zajištění, že div je vykreslen
      ", session$ns("igv-igvDiv")))  # Přidáme správné ID divu s namespace
    })
    
    observeEvent(input$bookmarks_click, {
      selected <- input$bookmarks_click
      message("Clicked row info: ", selected$index)
      if (!is.null(selected)) {
        position <- shared_data$selected_somatic_variants$position1[selected$index]
        #position2 <- values$bookmark_df$position2[selected$index]
        #positions <- paste0(position1, " ", position2)
        
        message("Navigating to positions: ", position)
        
        runjs(sprintf("
          console.log('Navigating to positions: %s');
          if (window.igvBrowser) {
            console.log('IGV browser exists');
            window.igvBrowser.search('%s').then(function() {
              console.log('Navigation complete');
            }).catch(function(error) {
              console.error('Navigation error:', error);
            });
          } else {
            console.log('IGV browser does not exist');
          }", position, position))
      }
    })
  })
}



# ui <- fluidPage(
#   box(id = "igv_page", title = "IGV Viewer",width = 10, collapsible = FALSE,
#     igv_ui("igv")
#   )
# )
# server <- function(input, output, session) {
#   igv_server("igv")
# }
# shinyApp(ui, server, options = list(launch.browser = TRUE))





# {
#   'name': 'DZ1601',
#   'url': 'http://127.0.0.1:5000/DZ1601fuze.bam',
#   'indexURL': 'http://127.0.0.1:5000/DZ1601fuze.bam.bai',
#   'format': 'bam'
# },
# {
#   'name': 'DZ1601 chimeric',
#   'url': 'http://127.0.0.1:5000/DZ1601fuzeChimeric.out.bam',
#   'indexURL': 'http://127.0.0.1:5000/DZ1601fuzeChimeric.out.bam.bai',
#   'format': 'bam'
# }

# {
#   'name': 'HG00103',
#   'url': 'https://s3.amazonaws.com/1000genomes/data/HG00103/alignment/HG00103.alt_bwamem_GRCh38DH.20150718.GBR.low_coverage.cram',
#   'indexURL': 'https://s3.amazonaws.com/1000genomes/data/HG00103/alignment/HG00103.alt_bwamem_GRCh38DH.20150718.GBR.low_coverage.cram.crai',
#   'format': 'cram'
# }
# 
# /*  tracks: [
#   {
#     'name': 'DZ1601',
#     'url': 'http://127.0.0.1:5000/DZ1601fuze.bam',
#     'indexURL': 'http://127.0.0.1:5000/DZ1601fuze.bam.bai',
#     'format': 'bam'
#   },
#   {
#     'name': 'DZ1601 chimeric',
#     'url': 'http://127.0.0.1:5000/DZ1601fuzeChimeric.out.bam',
#     'indexURL': 'http://127.0.0.1:5000/DZ1601fuzeChimeric.out.bam.bai',
#     'format': 'bam'
#   }
# ] */