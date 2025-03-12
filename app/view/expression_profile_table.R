# app/view/expression_profile_table.R

box::use(
  shiny[moduleServer,NS,tagList,fluidRow,fluidPage,column,tabPanel,reactive,req,observe,div,observeEvent,
        updateSelectInput,selectInput,numericInput,actionButton,renderPlot,plotOutput,uiOutput,renderUI,verbatimTextOutput,renderPrint,reactiveValues,isolate],
  reactable,
  reactable[colDef,reactableOutput,renderReactable,reactable,colGroup],
  htmltools[tags],
  plotly[plotlyOutput,renderPlotly,toWebGL],
  # ggiraph[renderGirafe,girafeOutput],
  reactablefmtr[pill_buttons,data_bars],
  utils[head],
  shinyWidgets[radioGroupButtons],
  data.table[rbindlist],
    promises[future_promise,`%...!%`,`%...>%`,catch],
  future[plan,multisession],
  # shinyjs[useShinyjs,runjs],
  # future.apply[future_lapply],
  # microbenchmark[microbenchmark],
  # future[future]
)

# plan(multisession) 

box::use(
  app/logic/plots[prepare_barPlot_data,create_barPlot], 
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs,load_data],
  app/logic/reactable_helpers[generate_columnsDef,custom_colGroup_setting],
  app/logic/prepare_table[prepare_expression_table,set_pathway_colors,get_tissue_list],
  app/logic/plots[prepare_volcano,volcanoPlot,ggvolcanoPlot,classify_volcano_genes], #plot_volcano
)



# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file") 
  # message("Loading data for expression profile: ", input_files$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes"|"genes_of_interest" #sample = "MR1507"
  dt <- prepare_expression_table(data,expr_flag)
  return(dt)
}


ui_volcano <- function(id, patient){
  ns <- NS(id)
  # tagList(
  #   fluidRow(
  #     # selectInput(ns("selected_tissue"), "Vyber tkáň:", choices = "Blood"),  # Naplníme dynamicky
  #     numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01),
  #     numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1),
  #     numericInput(ns("top_n"), "Počet popisků genů:", value = 20, min = 0, step = 1),
  #     # actionButton(ns("update_plot"), "Aktualizovat graf")
  #   ),
  #   fluidRow(
  #     # uiOutput(ns("volcano_plots_container")),
  #     plotlyOutput(outputId = ns("volcanoPlot_blood"))
  #     # plotOutput(outputId = ns(paste0("volcanoPlot_", get_tissue_list())))
  #   )
  # )
  tagList(
    fluidRow(
      plotOutput(outputId = ns("ggvolcanoPlot"))
    ),
    fluidRow(
      column(6, radioGroupButtons(ns("selected_tissue"), "Choose a tissue :", choices = get_tissue_list(), justified = TRUE))
    ),
    fluidRow(
      column(6, plotlyOutput(outputId = ns("volcanoPlot_blood"))),
      column(1,),
      column(1, numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01)),
      column(1, numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1)),
      column(1, numericInput(ns("top_n"), "Počet popisků genů:", value = 20, min = 0, step = 1))
    )
  )
}


server_volcano <- function(id, patient) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      input_data(patient, "all_genes") 
    })
    
    tissue_names <- get_tissue_list()

    
    output$ggvolcanoPlot <- renderPlot({
      req(data())

      dt_all <- rbindlist(lapply(tissue_names, function(tissue) {
        dt <- prepare_volcano(data(), tissue)
        classify_volcano_genes(dt)
      }))

      print("✅ Data připravena, vykresluji ggplot...")  # Debug

      # Vykreslíme ggplot s facetováním
      # ggvolcanoPlot(dt_all)
      ggvolcanoPlot(dt_all)
    })

    output$volcanoPlot_blood <- renderPlotly({
      req(data())
      # toWebGL()
      volcanoPlot(prepare_volcano(data(), "Blood"), "Blood")
    })})
  
    # output$volcano_plots_container <- renderUI({
    #   ns <- session$ns
    #   if (length(tissue_names) == 0) {
    #     return("Žádná dostupná data pro vykreslení.")
    #   }
    # 
    #   plot_list <- lapply(tissue_names, function(tissue) {
    #     div(style = "flex: 1 1 30%; padding: 10px;",
    #         use_spinner(plotlyOutput(ns(paste0("volcanoPlot_", tissue)), height = "600px", width = "600px")))
    #   })
    #   div(style = "display: flex; flex-wrap: wrap; justify-content: center;", plot_list)
    # })

    # output$volcano_plots_container <- renderUI({
    #   plot_list <- lapply(tissue_names, function(tissue) {
    #     # plot_volcano(prepare_volcano(data(), tissue), tissue)
    #     volcanoPlot(prepare_volcano(data(), tissue), tissue)
    #   })
    #   girafeOutput(plot_list)
    # })
    


    
    # # Paralelní vykreslování každého grafu
    # lapply(tissue_names, function(tissue) {
    #   output[[paste0("volcanoPlot_", tissue)]] <- renderPlotly({
    #     req(data())
    #     # toWebGL()
    #     plot <- volcanoPlot(prepare_volcano(data(), tissue), tissue)
    #     # plot <- plot_volcano(prepare_volcano(data(), tissue), tissue)
    #     
    #     plot
    #   })
    # })

    
  })
}



ui_allGenes <- function(id, patient) {
  ns <- NS(id)
    tagList(
      fluidRow( # style = "padding-top:30px",
        column(12, use_spinner(reactableOutput(outputId = ns(paste0("table_", patient))))))
  )
}

server_allGenes <- function(id, patient,selected_columns, column_mapping) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      input_data(patient, "all_genes") 
    })
    
    # Call generate_columnsDef to generate colDef setting for reactable
    column_defs <- reactive({
      message("Generating colDef for expression")
      req(selected_columns())
      generate_columnsDef(names(data()), selected_columns(), "expression", column_mapping, session)
    })


    output[[paste0("table_", patient)]] <- renderReactable({
      reactable(as.data.frame(data()),
                class = "expression-table",
                columns = column_defs(),
                resizable = TRUE,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 20, 50, 100),
                defaultPageSize = 20,
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE,
                filterable = TRUE,
                compact = TRUE,
                defaultColDef = colDef(sortNALast = TRUE,align = "center"),
                columnGroups = custom_colGroup_setting("expression"),
                defaultSorted = list("geneid" = "asc")
      )
    })
  })
}


ui_genesOfInterest <- function(id,patient){
  ns <- NS(id)
  tagList(
    fluidRow( # style = "padding-top:30px",
      column(12, use_spinner(reactableOutput(outputId = ns(paste0("table_", patient))))))
    # fluidRow(style = "padding-top:30px",
    #          column(12, plotlyOutput(outputId = ns(paste0("barplot_", patient, "_", get_tissues(patient)[1]))))),
    # fluidRow(style = "padding-top:30px",
    #          column(12, plotlyOutput(outputId = ns(paste0("barplot_", patient, "_", get_tissues(patient)[2])))))
    # column(6, tags$img(src = "./DZ1601_specBckg_BloodVessel.png"))
  )
}


server_genesOfInterest <- function(id, patient, selected_columns, column_mapping) {
  moduleServer(id, function(input, output, session) {

    data <- reactive({
      dt <- input_data(patient,"genes_of_interest")
    })

    column_defs <- reactive({
      message("Generating colDef for expression")
      req(selected_columns())
      generate_columnsDef(names(data()), selected_columns(), "expression", column_mapping, session)
    })

    output[[paste0("table_", patient)]] <- renderReactable({
      reactable(as.data.frame(data()),
                columns = column_defs(),
                resizable = TRUE,
                showPageSizeOptions = TRUE,
                height = 600,
                defaultPageSize = 10,
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE,
                filterable = TRUE,
                pagination = FALSE,
                compact = TRUE,
                defaultColDef = colDef(sortNALast = TRUE,align = "center"),
                columnGroups = custom_colGroup_setting("expression")
      )
    })

    # lapply(get_tissues(patient), function(tissue) { # tissue = "BloodVessel"
    #   output[[paste0("barplot_", patient, "_", tissue)]] <- renderPlotly({
    #     create_barPlot(prepare_barPlot_data(tissue,data()), patient, tissue)
    #   })
    # })

  })
}

# expUI <- fluidPage(
#   ui("exp1", "DZ1601")
# )
#
# expSerever <- function(input, output, session) {
#   server("exp1", "DZ1601")
# }
#
# shinyApp(expUI, expSerever)



## testování rychlosti
# library(microbenchmark)
# 
# times <- microbenchmark(
#   sync = {
#     lapply(tissue_names, function(tissue) {
#       volcanoPlot(prepare_volcano(dt, tissue), tissue)
#     })
#   },
#   async = {
#     lapply(tissue_names, function(tissue) {
#       plot_volcano(prepare_volcano(dt, tissue), tissue)
#     })
#   },
#   times = 3  # Počet opakování testu
# )
# print(times)



