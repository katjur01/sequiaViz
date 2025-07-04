box::use(
  shiny[br, NS,h3, tagList, div, observe, observeEvent, mainPanel, titlePanel, uiOutput, renderUI, HTML, fluidPage,fluidRow, moduleServer,
        reactiveValues, column],
  htmltools[tags],
  bs4Dash[actionButton,box],
  shinyjs[useShinyjs, runjs],
  reactable,
  reactable[reactableOutput,renderReactable,reactable,JS],
  processx[process,]
)
box::use(
  app/logic/igv_helper[build_igv_tracks,start_static_server,stop_static_server]
)

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
      # tags$script(src = "https://cdn.jsdelivr.net/npm/igv@2.15.12/dist/igv.min.js")
      tags$script(src = "https://cdn.jsdelivr.net/npm/igv@3.3.0/dist/igv.min.js")
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
igv_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    values <- reactiveValues()
    
    # Seznam vzorků
    samples <- list(
      list(name = "DZ1601", file = "DZ1601fuze.bam"),
      list(name = "MR1507", file = "MR1507fuze.bam")
    )
    
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
      # Krok 1: Vytvoření divu pro IGV
      output$igvDivOutput <- renderUI({
        div(id = session$ns("igv-igvDiv"))  # Zajistíme, že div má správný namespace
      })
      
      track_block <- build_igv_tracks(samples)
      
      # Krok 2: Po vykreslení spustíme JavaScript pro IGV s mírným zpožděním
      runjs(sprintf("
        setTimeout(function() {
          var igvDiv = document.getElementById('%s');
          if (igvDiv) {
            var options = {
              genome: 'hg38',
              locus: 'all',
              tracks: [%s],
              showNavigation: true,       // horní navigační lišta
              showRuler: true,            // číslování pozice nahoře
              showSampleName: true,       // pokud máš sampleName v tracku
              showCenterGuide: true,      // vertikální linka ve středu
              trackHeight: 300,           // výchozí výška jednoho BAM tracku
              minTrackHeight: 50,
              maxTrackHeight: 1000,
              search: {
                url: 'https://www.ncbi.nlm.nih.gov/gene/?term=$$'
              }
            };
            igv.createBrowser(igvDiv, options).then(function(browser) {
              console.log('IGV browser created');
              window.igvBrowser = browser;
            });
          } else {
            console.error('IGV div not found.');
          }
        }, 20);  // Zpoždění 20 ms k zajištění, že div je vykreslen
      ", session$ns("igv-igvDiv"), track_block))  # Přidáme správné ID divu s namespace
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


# 
# ui <- fluidPage(
#   box(id = "igv_page", title = "IGV Viewer", width = 10, collapsible = FALSE,
#       igv_ui("igv")
#   )
# )
# 
# server <- function(input, output, session) {
#   
#   # Spustíme statický server při startu celé aplikace
#   start_static_server(dir = "/Users/katerinajuraskova/Desktop/sequiaViz/input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze/mapped")
#   
#   igv_server("igv")
#   
#   # Ukončení serveru při zavření celé session
#   session$onSessionEnded(function() {
#     stop_static_server()
#   })
#   
# }
# 
# shinyApp(ui, server, options = list(launch.browser = TRUE))
