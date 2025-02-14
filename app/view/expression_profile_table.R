# app/view/expression_profile_table.R

box::use(
  shiny[moduleServer,NS,tagList,fluidRow,fluidPage,column,tabPanel,reactive,req],
  reactable,
  reactable[colDef,reactableOutput,renderReactable,reactable,colGroup],
  htmltools[tags],
  plotly[plotlyOutput,renderPlotly],
  reactablefmtr[pill_buttons,data_bars]
)

box::use(
  app/logic/plots[prepare_barPlot_data,create_barPlot], 
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs,load_data],
  app/logic/reactable_helpers[generate_columnsDef,custom_colGroup_setting],
  app/logic/prepare_table[prepare_expression_table,set_pathway_colors,get_tissue_list]
)

# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file")
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes" #sample = "MR1507"
  dt <- prepare_expression_table(data,expr_flag)
  return(dt)
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
      # pathway_colors <- set_pathway_colors()
      # dt$color <- pathway_colors[dt$pathway]
      # dt
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
    #
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





