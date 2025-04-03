# app/view/expression_profile_table.R

box::use(
  shiny[moduleServer,NS,tagList,fluidRow,fluidPage,column,tabPanel,reactive,req,observe,div,observeEvent,reactiveVal,icon,splitLayout,h4,
        updateSelectInput,selectInput,numericInput,actionButton,renderPlot,plotOutput,uiOutput,renderUI,verbatimTextOutput,renderPrint,reactiveValues,isolate],
  reactable,
  reactable[colDef,reactableOutput,renderReactable,reactable,colGroup],
  htmltools[tags],
  plotly[plotlyOutput,renderPlotly,toWebGL],
  # ggiraph[renderGirafe,girafeOutput],
  reactablefmtr[pill_buttons,data_bars],
  utils[head],
  shinyWidgets[radioGroupButtons,checkboxGroupButtons,updateCheckboxGroupButtons,dropdown,actionBttn,awesomeCheckboxGroup,pickerInput],
  data.table[rbindlist,dcast.data.table,as.data.table,melt.data.table,copy],
  grDevices[colorRampPalette],
  pheatmap[pheatmap],
  stats[setNames],
  #   promises[future_promise,`%...!%`,`%...>%`,catch],
  # future[plan,multisession],
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
  app/logic/networkGraph_helper[get_pathway_list],
)

# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file") 
  # message("Loading data for expression profile: ", input_files$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes"|"genes_of_interest" #sample = "MR1507"
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
    div(class = "filter-button-wrapper",
      filterTab_ui(ns("filterTab_dropdown"), column_mapping$dropdown_btn, all_colnames$all_columns, all_colnames$default_setting),
      use_spinner(reactableOutput(outputId = ns(paste0("table_", patient)))))
  )
}


server_genesOfInterest <- function(id, patient, selected_columns, column_mapping, all_colnames) {
  moduleServer(id, function(input, output, session) {
    ns <- NS(id)

    output$filterTab <- renderUI({
      req(all_colnames)
      filterTab_ui(ns("filterTab_dropdown"), column_mapping$dropdown_btn, all_colnames$all_columns, all_colnames$default_setting)
    })
    
    
    data <- reactive({
      dt <- input_data(patient,"genes_of_interest")
    })
    
    
    selected_tissues_final <- reactiveVal(get_tissue_list())
    selected_pathway_final <- reactiveVal(get_pathway_list("genes_of_interest"))
    log2fc_bigger1_final <- reactiveVal(NULL)
    log2fc_smaller1_final <- reactiveVal(NULL)
    pval_final <- reactiveVal(NULL)
    padj_final <- reactiveVal(NULL)
    log2fc_bigger1_btn_final <- reactiveVal(FALSE)
    log2fc_smaller1_btn_final <- reactiveVal(FALSE)
    pval_btn_final <- reactiveVal(FALSE)
    padj_btn_final <- reactiveVal(FALSE)

    
    filtered_data <- reactive({
      req(data())
      df <- copy(data())  # defensivní kopie
      base_cols <- c("sample", "feature_name", "geneid", "pathway")
      
      # --- Filtr pathways ---
      pathways_selected <- selected_pathway_final()
      if (!is.null(pathways_selected) && length(pathways_selected) > 0 &&
          length(pathways_selected) < length(get_pathway_list("genes_of_interest"))) {
        df <- df[pathway %in% pathways_selected]
      }
      
      # --- Filtrace podle aktivních filtrů pro tkáně ---
      # log2FC > 1
      if (log2fc_bigger1_btn_final()) {
        tissues <- log2fc_bigger1_final()
        if (length(tissues) > 0) {
          for (tissue in tissues) {
            col <- paste0("log2FC_", tissue)
            if (col %in% names(df)) {
              df <- df[get(col) > 1]
            }
          }
        }
      }
      
      # log2FC < -1
      if (log2fc_smaller1_btn_final()) {
        tissues <- log2fc_smaller1_final()
        if (length(tissues) > 0) {
          for (tissue in tissues) {
            col <- paste0("log2FC_", tissue)
            if (col %in% names(df)) {
              df <- df[get(col) < -1]
            }
          }
        }
      }
      
      # p-value < 0.05
      if (pval_btn_final()) {
        tissues <- pval_final()
        if (length(tissues) > 0) {
          for (tissue in tissues) {
            col <- paste0("p_value_", tissue)
            if (col %in% names(df)) {
              df <- df[get(col) < 0.05]
            }
          }
        }
      }
      
      # p-adj < 0.05
      if (padj_btn_final()) {
        tissues <- padj_final()
        if (length(tissues) > 0) {
          for (tissue in tissues) {
            col <- paste0("p_adj_", tissue)
            if (col %in% names(df)) {
              df <- df[get(col) < 0.05]
            }
          }
        }
      }
      
      # --- Výběr sloupců podle vybraných tkání ---
      tissues <- selected_tissues_final()
      if (is.null(tissues) || length(tissues) == 0) return(df[, ..base_cols])
      selected_cols <- unlist(lapply(tissues, function(tissue) {
        c(paste0("log2FC_", tissue), paste0("p_value_", tissue), paste0("p_adj_", tissue))
      }))
      valid_cols <- intersect(selected_cols, names(df))
      df_filtered <- df[, c(base_cols, valid_cols), with = FALSE]
      
      return(df_filtered)
    })
    
    

    filter_state <- filterTab_server("filterTab_dropdown")
    
    observeEvent(filter_state$confirm(), {
      selected_tissues_final(filter_state$selected_tissue())
      selected_pathway_final(filter_state$selected_pathway())
      
      log2fc_bigger1_final(filter_state$log2fc_bigger1_tissue())
      log2fc_smaller1_final(filter_state$log2fc_smaller1_tissue())
      pval_final(filter_state$pval_tissue())
      padj_final(filter_state$padj_tissue())
      
      log2fc_bigger1_btn_final("log2FC > 1" %in% filter_state$log2fc_bigger1_btn())
      log2fc_smaller1_btn_final("log2FC < -1" %in% filter_state$log2fc_smaller1_btn())
      pval_btn_final("p-value < 0.05" %in% filter_state$pval_btn())
      padj_btn_final("p-adj < 0.05" %in% filter_state$padj_btn())
    })
    
    
    
    column_defs <- reactive({
      message("Generating colDef for expression")
      req(selected_columns())
      generate_columnsDef(names(data()), selected_columns(), "expression", column_mapping$table, session)
    })

    output[[paste0("table_", patient)]] <- renderReactable({
      
      reactable(as.data.frame(filtered_data()),
                class = "expression-table",
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

    

  })
}

filterTab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    observe({
      message(" get_pathway_list(genes_of_interest): " ,get_pathway_list("genes_of_interest"))
      updateCheckboxGroupButtons(session, "log2fc_bigger1_btn",
                                 selected = if (length(input$log2fc_bigger1_tissue) > 0) "log2FC > 1" else character(0))
      updateCheckboxGroupButtons(session, "log2fc_smaller1_btn",
                                 selected = if (length(input$log2fc_smaller1_tissue) > 0) "log2FC < -1" else character(0))
      updateCheckboxGroupButtons(session, "pval_btn",
                                 selected = if (length(input$pval_tissue) > 0) "p-value < 0.05" else character(0))
      updateCheckboxGroupButtons(session, "padj_btn",
                                 selected = if (length(input$padj_tissue) > 0) "p-adj < 0.05" else character(0))
    })
    
    return(list(
      confirm = reactive(input$confirm_btn),
      selected_tissue = reactive(input$select_tissue),
      selected_pathway = reactive(input$filter_pathway),
      
      log2fc_bigger1_tissue = reactive(input$log2fc_bigger1_tissue),
      log2fc_smaller1_tissue = reactive(input$log2fc_smaller1_tissue),
      pval_tissue = reactive(input$pval_tissue),
      padj_tissue = reactive(input$padj_tissue),
      
      log2fc_bigger1_btn = reactive(input$log2fc_bigger1_btn),
      log2fc_smaller1_btn = reactive(input$log2fc_smaller1_btn),
      pval_btn = reactive(input$pval_btn),
      padj_btn = reactive(input$padj_btn)
    ))
    
  })
}


  
filterTab_ui <- function(id,columnName_map,column_list,default_setting){
  ns <- NS(id)
  tagList(
    dropdown(right = TRUE,size = "xs",icon = icon("filter"),style = "material-flat",width = "auto",
       fluidRow(style = "width: 45rem;",
         column(6,
                div(style = "display: flex; flex-direction: column; flex-wrap: wrap; align-items: baseline; width: 100% !important; border-right: 1px solid #e0e0e0;",
                    div(class = "filterTab-select-tissue",
                      checkboxGroupButtons(ns("select_tissue"),"Select tissues:",choices = get_tissue_list(),selected = get_tissue_list(),individual = TRUE)),
                    tags$span("Table filter:", style = "font-size: 1rem; font-weight: bold; isplay: inline-block; margin-bottom: .5rem;"),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                          div(style = "width = 100%",
                            checkboxGroupButtons(ns("log2fc_bigger1_btn"),choices = "log2FC > 1",selected = "",individual = TRUE)),
                          div(class = "filter_pathway",
                            pickerInput(ns("log2fc_bigger1_tissue"),choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)))),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                          checkboxGroupButtons(ns("log2fc_smaller1_btn"),choices = "log2FC < -1",selected = "",individual = TRUE),
                          div(class = "filter_pathway",
                            pickerInput(ns("log2fc_smaller1_tissue"), choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)))),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                          checkboxGroupButtons(ns("pval_btn"),choices = "p-value < 0.05",selected = "",individual = TRUE),
                          div(class = "filter_pathway",
                            pickerInput(ns("pval_tissue"), choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE)))),
                    div(style = "display: flex; gap: 10px; margin-bottom: -10px;",
                          checkboxGroupButtons(ns("padj_btn"),choices = "p-adj < 0.05",selected = "",individual = TRUE),
                          div(class = "filter_pathway",
                           pickerInput(ns("padj_tissue"), choices = get_tissue_list(), multiple = TRUE, options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select tissue",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))))
                    )
         ),
         column(6,
            div(style = "flex: 1; min-width: 300px;",
                div(class = "filter_pathway",
                    pickerInput(ns("filter_pathway"), "Filter pathways", 
                                choices = get_pathway_list("genes_of_interest"), multiple = TRUE, 
                                options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))),
                awesomeCheckboxGroup(ns("filter_column"),"Columns selection:", 
                                     choices  = c("Gene name","Gene ID","Pathway","log2FC","p-value","p-adj"),
                                     selected = c("Gene name","Gene ID","Pathway","log2FC","p-value","p-adj")
                                     )
            )
         )
       ),
       div(style = "display: flex; justify-content: center; margin-top: 10px;",
           actionBttn(ns("confirm_btn"),"Confirm changes",style = "stretch",color = "success",size = "sm",individual = TRUE,value = 0))
    )
  )
}