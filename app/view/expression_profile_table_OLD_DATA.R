# app/view/expression_profile_table.R

box::use(
  shiny[moduleServer,NS,tagList,fluidRow,column,tabPanel,reactive],
  reactable,
  reactable[colDef,reactableOutput,renderReactable,reactable,colGroup],
  htmltools[tags],
  plotly[plotlyOutput,renderPlotly]
)

box::use(
  app/logic/expression_profile_helpers[get_tissues,sorting_setting,groups_setting,set_pathway_colors], 
  app/logic/plots[prepare_barPlot_data,create_barPlot], 
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs,load_data],
  app/logic/reactable_helpers[custom_colDef_setting]
)

# Load and process data table
input_data <- function(sample){
  input_files <- get_inputs("per_sample_file")
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample)
  pathway_colors <- set_pathway_colors()
  data$Color <- pathway_colors[data$Pathway]
  return(data)
}


ui <- function(id,patient){
  ns <- NS(id)
  tagList(
       fluidRow( # style = "padding-top:30px",
         column(12, use_spinner(reactableOutput(outputId = ns(paste0("table_", patient)))))),
       fluidRow(style = "padding-top:30px",
                column(12, plotlyOutput(outputId = ns(paste0("barplot_", patient, "_", get_tissues(patient)[1]))))),
       fluidRow(style = "padding-top:30px",
                column(12, plotlyOutput(outputId = ns(paste0("barplot_", patient, "_", get_tissues(patient)[2])))))
       # column(6, tags$img(src = "./DZ1601_specBckg_BloodVessel.png"))
  )
}


server <- function(id, patient) {
  moduleServer(id, function(input, output, session) {
      
    data <- reactive({
      input_data(patient)
      })
      
    column_defs <- reactive({custom_colDef_setting("expression",data = data())})
    default_sorted <- reactive({sorting_setting(data())})
    column_groups <- reactive({groups_setting(data())})
      
      
      output[[paste0("table_", patient)]] <- renderReactable({
        reactable(as.data.frame(data()),
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
                  defaultSorted = default_sorted(),
                  columnGroups = column_groups(),
                  columns = column_defs()
        )
      })
      # 
      lapply(get_tissues(patient), function(tissue) { # tissue = "BloodVessel"
        output[[paste0("barplot_", patient, "_", tissue)]] <- renderPlotly({
          create_barPlot(prepare_barPlot_data(tissue,data()), patient, tissue)
        })
      })
    
  })
}
