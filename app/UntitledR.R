box::use(
  shiny[moduleServer,NS,tagList,fluidRow,fluidPage,column,tabPanel,reactive,req,
        updateSelectInput,selectInput,numericInput,actionButton,renderPlot,plotOutput],
  reactable,
  reactable[colDef,reactableOutput,renderReactable,reactable,colGroup],
  htmltools[tags],
  plotly[plotlyOutput,renderPlotly],
  reactablefmtr[pill_buttons,data_bars],
  utils[head]
  
)

box::use(
  app/logic/plots[prepare_barPlot_data,create_barPlot], 
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs,load_data],
  app/logic/reactable_helpers[generate_columnsDef,custom_colGroup_setting],
  app/logic/prepare_table[prepare_expression_table,set_pathway_colors,get_tissue_list],
  app/logic/plots[prepare_volcano,plot_volcano],
)

# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file") 
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes"|"genes_of_interest" #sample = "MR1507"
  print(data)
  dt <- prepare_expression_table(data,expr_flag)
  print(head(dt))
  return(dt)
}


ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      selectInput(ns("selected_tissue"), "Vyber tkáň:", choices = "Blood"),  # Naplníme dynamicky
      numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01),
      numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1),
      numericInput(ns("top_n"), "Počet popisků genů:", value = 20, min = 0, step = 1),
      actionButton(ns("update_plot"), "Aktualizovat graf")
    ),
    fluidRow(
      plotOutput(ns("volcano_plot"))
    )
  )
}



server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      input_data("MR1507", "all_genes") 
    })
    
    # tissue_names <- reactive({
    #   unique(gsub("log2FC_", "", grep("log2FC_", colnames(data()), value = TRUE)))
    # })

    
    # Reaktivní dataset na základě vybrané tkáně
    selected_data <- reactive({
      # message("YYYYYYYYYY: ",input$selected_tissue)
      # req(input$selected_tissue)  # Počkej, než uživatel něco vybere

      prepare_volcano(data(), "Blood")
    })
    
    # Reaktivní volcano plot
    output$volcano_plot <- renderPlot({
      message("Selected data: ", prepare_volcano(data(), "Blood"))
      # req(input$selected_tissue)
      
      # isolate({
      #   input$update_plot  # Aby se graf aktualizoval jen po kliknutí na tlačítko
      # })
      
      plot_volcano(selected_data(), "Blood"
                   
                   # input$selected_tissue,
                   # top_n = input$top_n,
                   # padj_cutoff = input$padj_cutoff,
                   # logfc_cutoff = input$logfc_cutoff
      )
      
    })
    
    
  })
}

expUI <- fluidPage(
  ui("exp1")
)

expSerever <- function(input, output, session) {
  server("exp1")
}

shinyApp(expUI, expSerever)
