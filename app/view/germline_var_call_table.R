# app/view/germline_var_call_table.R

#########################################################################################################
## pozn. reactable.extras je opravdu rychlejší, ale nefungují nějaké reactable parametry jako např. 
## showPageSizeOptions, pageSizeOptions a defaultPageSize
## Také nefunguje balík jako reactablefmtr, což je velká škoda. 
#########################################################################################################

# dt <- openxlsx::read.xlsx("../AK1860krev.germ_variants.xlsx")

box::use(
  shiny[moduleServer,NS,h2,h3,tagList,div,tabsetPanel,tabPanel,observeEvent,fluidPage,fluidRow, reactive,icon,textInput,isTruthy,
        sliderInput,showModal,modalDialog,column,uiOutput,renderUI,textOutput,renderText,reactiveVal,req,observe,outputOptions,checkboxInput],
  bs4Dash[actionButton, box,popover,addPopover],
  reactable,
  reactable[reactable,renderReactable,colDef,colGroup,JS,getReactableState],
  # reactable.extras[reactable_extras_ui,reactable_extras_server],
  htmltools[tags,HTML],
  app/logic/patients_list[set_patient_to_sample],
  shinyWidgets[prettyCheckbox],
  # reactablefmtr
)

box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_germline_table,columnName_map],
  app/logic/waiters[use_spinner],
  app/logic/reactable_helpers[selectFilter,minRangeFilter,filterMinValue,generate_columnsDef]
)

# Load and process data table
input_data <- function(sample){
  filenames <- get_inputs("per_sample_file")
  message("Loading data for germline: ", filenames$var_call.germline)
  data <- prepare_germline_table(load_data(filenames$var_call.germline,"varcall",sample))
  return(data)
}

ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .rt-td-inner select {
        border: 1px solid rgba(0,0,0,.1); /* Barva hrany bunky*/
        border-radius: 3px;  /* Zaoblení hrany bunky */
        }
      ")),
    use_spinner(reactable$reactableOutput(ns("germline_var_call_tab"))),
    tags$br(),
    tags$div(id = ns("checkbox_popover"), style = "width:245px; position: absolute; left: 10;", #margin-top: 13.5px;
             checkboxInput(ns("fullTable_checkbox"),label = "Keep pre-filtered variant table",value = TRUE)),
    tags$br()
  )
}

server <- function(id, selected_samples, selected_columns, column_mapping, selection_enabled) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Call loading function to load data
    data <- reactive({
      message("Loading input data for germline")
      input_data(selected_samples)
    })
    
    # Call generate_columnsDef to generate colDef setting for reactable
    column_defs <- reactive({
      req(selected_columns())
      generate_columnsDef(names(filtered_data()), selected_columns(), "germline", column_mapping, session)
    })
    
    filtered_data <- reactive({
      message("Before filtering")
      print(input$fullTable_checkbox)
      
      dt <- data()
      
      if (isTruthy(input$fullTable_checkbox)) {
        dt <- dt[gnomAD_NFE <= 0.01 & coverage_depth > 10 & Consequence != "synonymous_variant"]
        dt <- dt[gene_region == "exon" | gene_region == "splice"]
        message("Filtered data for germline")
      } else {
        message("Full data for germline")
      }
      as.data.frame(dt)
    })
    
    # Reactive value to store selected rows
    selected_rows <- reactiveVal(data.frame())
    
    # Render reactable with conditional selection
    output$germline_var_call_tab <- renderReactable({
      message("Rendering Reactable for germline")
      reactable(
        filtered_data(),
        columns = column_defs(),
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 20, 50, 100),
        defaultPageSize = 20,
        striped = TRUE,
        wrap = FALSE,
        highlight = TRUE,
        outlined = TRUE,
        defaultColDef = colDef(align = "center", sortNALast = TRUE),
        defaultSorted = list("CGC_Germline" = "desc", "trusight_genes" = "desc", "fOne" = "desc"),
        columnGroups = list(
          colGroup(name = "Databases", columns = c("gnomAD_NFE", "clinvar_sig", "snpDB", "CGC_Germline", "trusight_genes", "fOne")),
          colGroup(name = "Annotation", columns = c("Consequence", "HGVSc", "HGVSp", "all_full_annot_name"))
        ),
        selection = if (selection_enabled()) "multiple" else NULL,  # Enable selection conditionally
        onClick = if (selection_enabled()) "select" else NULL,  # Enable selection by click
        class = "germline-table",
        elementId = "tbl-germline"
      )
    })
    
    # Observe changes in selected rows if selection is enabled
    observe({
      if (selection_enabled()) {
        selected <- getReactableState("germline_var_call_tab", "selected")
        selected_rows(filtered_data()[selected, , drop = FALSE])
      }
    })
    
    # You can access the selected rows using selected_rows() in other parts of the server logic
    observeEvent(selected_rows(), {
      if (selection_enabled()) {
        message("Selected rows:")
        print(selected_rows())
      }
    })
    
    addPopover(id = "checkbox_popover",
               options = list(
                 title = "Current filters:",
                 content = "gnomAD NFE <= 0.01, Coverage > 10, Consequence w/o synonymous_variant, Gene region is exon or splice",
                 placement = "bottom",
                 trigger = "hover"))
  })
}





