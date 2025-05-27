# app/view/IGV.R

box::use(
  shiny[br, NS,h3, tagList, div, observe, observeEvent, mainPanel, titlePanel, uiOutput, renderUI, HTML, fluidPage,fluidRow, moduleServer,
        reactiveValues, column,req, isolate,modalDialog,modalButton,showModal],
  htmltools[tags],
  bs4Dash[actionButton,box],
  shinyjs[useShinyjs, runjs],
  reactable[reactableOutput,renderReactable,reactable,JS,colDef],
  processx[process]
)
box::use(
  app/logic/igv_helper[build_igv_tracks,start_static_server,stop_static_server]
)

# UI function for IGV tab
#' @export
igv_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("#igv_page {width: 100%; height: 600px; margin: 0 auto; padding: 20px; box-sizing: border-box;}
                    #igv-igvDiv {width: 100%; height: auto; border: none;
                      margin: 0 auto; padding: 20px; box-sizing: border-box;}
                    ")),
    useShinyjs(),
    tags$head(
      tags$script(src = "https://cdn.jsdelivr.net/npm/igv@2.15.12/dist/igv.min.js")
      #tags$script(src = "https://cdn.jsdelivr.net/npm/igv@3.3.0/dist/igv.min.js")
    ),
    fluidRow(
      column(12, reactableOutput(ns("bookmarks")))),
    br(),
    fluidRow(
      column(2, actionButton(ns("loadIGVButton"), "Load IGV Viewer"))
    ),
    br(),
    uiOutput(ns("igvDivOutput"))
  )
}

# Server function for IGV tab
#' @export
igv_server <- function(id,shared_data) {
  moduleServer(id, function(input, output, session) {
    output$bookmarks <- renderReactable({
      req(shared_data$selected_variants)
      reactable(shared_data$selected_variants,
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
                               "position1"=colDef(show=FALSE),
                               "patients"=colDef(name="Detected for patient")),
                onClick = JS(sprintf("function(rowInfo, column) {
                  Shiny.setInputValue('%s', { index: rowInfo.index + 1 }, { priority: 'event' });
                }", session$ns("bookmarks_click")))
      )
    })
    
    observeEvent(input$loadIGVButton, {
      selected_empty <- is.null(shared_data$selected_variants) || 
        (is.data.frame(shared_data$selected_variants) && nrow(shared_data$selected_variants) == 0)
      bam_empty <- is.null(shared_data$bam_files) || length(shared_data$bam_files) == 0

      if (selected_empty || bam_empty) {
        showModal(modalDialog(
          title = "Missing input",
          "You have not selected variants or patients for visualization. Please return to the Somatic variant calling tab and define them.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else {
        output$igvDivOutput <- renderUI({
          div(id = session$ns("igv-igvDiv"))
        })
        track_block <- build_igv_tracks(isolate(shared_data$bam_files))
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
        ", session$ns("igv-igvDiv"), track_block))
      }
    })
    
    observeEvent(input$bookmarks_click, {
      selected <- input$bookmarks_click
      message("Clicked row info: ", selected$index)
      if (!is.null(selected)) {
        position <- shared_data$selected_variants$position1[selected$index]
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
          }", position,position))
      }
    })
  })
}
