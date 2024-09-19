# app/view/fusion_genes_table.R

box::use(
  shiny[moduleServer,NS,h3,tagList,div,textInput,renderPrint,reactive,observe,observeEvent,icon,mainPanel,titlePanel,isolate,
        uiOutput,renderUI,HTML,req],
  reactable,
  reactable[colDef],
  htmltools[tags],
  bs4Dash[actionButton],
  shinyjs[useShinyjs,runjs],
  reactablefmtr[pill_buttons,icon_assign],
  data.table[fifelse,setcolorder]
)
box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_fusion_genes_table,prepare_arriba_images,columnName_map], 
  app/logic/waiters[use_spinner],
  app/logic/reactable_helpers[generate_columnsDef],
  app/logic/prepare_table[colFilter]
)

# Load and process data table
input_data <- function(sample){
  filenames <- get_inputs("per_sample_file")
  # message("Loading data for fusion: ", filenames$fusions)
  data <- prepare_fusion_genes_table(load_data(filenames$fusions,"fusion",sample),sample)
  return(as.data.frame(data))
}

##############  pozn  #####################
#ploty budu dělat pomocí balíku 
#nicméně je problém s instalací XML knihovny. 
#Důvod a co mám dělat je zde:
#  https://support.bioconductor.org/p/52539/
##########################################
#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    # div(
    #   class = "fusion-table-contant",
      use_spinner(reactable$reactableOutput(ns("fusion_genes_tab")))
    # )
  )

}

#' @export
server <- function(id, selected_samples, selected_columns,column_mapping) {
  moduleServer(id, function(input, output, session) {
    # prepare_arriba_images(selected_samples)
    
  # Call loading function to load data
    dt <- reactive({
      message("Loading input data for fusion")
      input_data(selected_samples) 
    })

  # Call generate_columnsDef to generate colDef setting for reactable
    column_defs <- reactive({
      message("Generating colDef for fusion")
      req(selected_columns())
      generate_columnsDef(names(dt()), selected_columns(), "fusion", column_mapping, session)
    })
    
    output$fusion_genes_tab <- reactable$renderReactable({
      message("Rendering Reactable for fusion")
      reactable$reactable(dt(),
                          columns = column_defs(),
                          class = "fusion-table",
                          resizable = TRUE,
                          showPageSizeOptions = TRUE,
                          pageSizeOptions = c(10, 20, 50, 100),
                          defaultPageSize = 20,
                          striped = TRUE,
                          wrap = FALSE,
                          highlight = TRUE,
                          outlined = TRUE,
                          defaultColDef = colDef(
                            align = "center",
                            sortNALast = TRUE
                          ),
                          defaultSorted = list("arriba.confidence" = "asc","arriba.called" = "desc","starfus.called" = "desc"),
                          details = function(index) {
                            # row <- data[index, ]
                            svg_file <- dt()$svg_path[index]
                            png_file <- dt()$png_path[index]
                            tags$div(
                              style = "display: flex; align-items: center;",
                              if (file.exists(paste0("www/",svg_file))) {
                                tags$img(src = svg_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top;")
                              } else {
                                tags$strong("Starfusion doesn't provide this picture.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                              },
                              if (file.exists(paste0("www/",png_file))) {
                                tags$img(src = png_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top; margin-bottom:40px;")
                              } else {
                                tags$strong("IGV didn't snapshot this position.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                              }
                            )
                          },
                          onClick = "expand",
                          elementId = "tbl-fusion"
        )
  })
    
    # observeEvent(input$igvButton_click, {
    #   runjs("
    #     var newWindow = window.open(window.location.href.split('#')[0] + '#shiny-tab-hidden_igv', '_blank');
    #     newWindow.onload = function() {
    #       newWindow.Shiny.setInputValue('go_to_hidden_igv', true);
    #     }
    #   ")
    # })

    
    # observeEvent(input$openIGVButton, {
    #     IGV$server("igv")
    # 
    # })
#     
#     # observeEvent(input$igvButton_click, {
#     #   IGV$server("igv")
#     #   # system("open -a /Users/katerinajuraskova/Desktop/IGV.app -b /Users/katerinajuraskova/Desktop/sequiaViz/batch_file_2.txt")
#     #   # system("npx http-server -a localhost ./ -p 8080", wait = FALSE) # -p 8080
#     #   # runjs("window.open('http://localhost:8080/www/igv_web_app/', '_blank');")
#     # })

  })
}


#
# fusionGenes_Ui <- fluidPage(
#   piechart_input("plots")
#   # table_ui("geneFusion_tab")
# )
#
# fusionGenes_Server <- function(input, output, session){
#   piechart_server("plots")
#   # table_server("geneFusion_tab")
# }
#
# shinyApp(ui = fusionGenes_Ui, server = fusionGenes_Server)
