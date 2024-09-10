box::use(
  shiny[br, NS,h3, tagList, div, observe, observeEvent, mainPanel, titlePanel, uiOutput, renderUI, HTML, fluidPage,fluidRow, moduleServer,
        reactiveValues, column],
  htmltools[tags],
  bs4Dash[actionButton,box],
  shinyjs[useShinyjs, runjs],
  reactable,
  reactable[reactableOutput,renderReactable,reactable,JS],
)

start_server <- function(){
  setwd("/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze/mapped/")
  data_server = serve_data('./')
  data_server$stop_server()
}


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

igv_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues()
    
    values$bookmark_df <- data.frame(
      gene1 = c("KANSL1", "KMT2A", "METTL13"),
      gene2 = c("LRRC37A4P", "MLLT3", "DNM3"),
      position1 = c("17:45597764", "11:118482495", "1:171814013"),
      position2 = c("17:45545676", "11:20365744", "1:171987759")
    )
    
    output$bookmarks <- renderReactable({
      reactable(values$bookmark_df,
                pagination = FALSE,
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE,
                onClick = JS(sprintf("function(rowInfo, column) {
                  Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
                }", session$ns("bookmarks_click")))
      )
    })
    
    observeEvent(input$loadIGVButton, {
      runjs(sprintf("
       var igvDiv = document.createElement('div');
        igvDiv.id = 'igv-igvDiv';  /* Apply the correct ID */
        var container = document.getElementById('%s');  /* Find the box element by its ID */
        container.appendChild(igvDiv);

        var options = {
          genome: 'hg38',
          locus: 'all'
        };

      igv.createBrowser(igvDiv, options).then(function(browser) {
                console.log('IGV browser created');
                window.igvBrowser = browser;
              });
            ", "igv_page"))
      
        #window.igvBrowser = igv.createBrowser(igvDiv, options);", "igv_page"))
      
      
      output$igvDivOutput <- renderUI({
        HTML('<div id="igv-igvDiv"></div>')
      })
    })
    
    observeEvent(input$bookmarks_click, {
      selected <- input$bookmarks_click
      message("Clicked row info: ", selected$index)
      if (!is.null(selected)) {
        position1 <- values$bookmark_df$position1[selected$index]
        position2 <- values$bookmark_df$position2[selected$index]
        positions <- paste0(position1, " ", position2)
        
        message("Navigating to positions: ", positions)
        
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
          }", positions, positions))
      }
    })
  
  })
}


# ui <- fluidPage(
#   box(id = "igv_page", title = "IGV Viewer",width = 10, collapsible = FALSE,
#     igv_ui("igv")
#   )
# )
# 
# 
# 
# 
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