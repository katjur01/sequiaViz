# app/view/expression_profile_table.R

box::use(
  shiny[
    moduleServer, NS, tagList, fluidRow, fluidPage, column, tabPanel, reactive, req, observe, div, observeEvent, reactiveVal, icon, splitLayout, h4, bindEvent,
    updateSelectInput, selectInput, numericInput, actionButton, renderPlot, plotOutput, uiOutput, renderUI, verbatimTextOutput, renderPrint, reactiveValues, isolate
  ],
  reactable,
  bs4Dash[box],
  reactable[colDef, reactableOutput, renderReactable, reactable, colGroup],
  htmltools[tags],
  plotly[plotlyOutput, renderPlotly, toWebGL],
  reactablefmtr[pill_buttons, data_bars],
  utils[head],
  shinyWidgets[
    radioGroupButtons, checkboxGroupButtons, updateCheckboxGroupButtons, dropdown, actionBttn, awesomeCheckboxGroup, pickerInput
  ],
  data.table[rbindlist, dcast.data.table, as.data.table, melt.data.table, copy],
  grDevices[colorRampPalette],
  pheatmap[pheatmap],
  stats[setNames],
  magrittr[`%>%`]
)

box::use(
  app/logic/plots[prepare_barPlot_data, create_barPlot, prepare_volcano, volcanoPlot, ggvolcanoPlot, classify_volcano_genes],
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs, load_data],
  app/logic/reactable_helpers[generate_columnsDef, custom_colGroup_setting],
  app/logic/prepare_table[prepare_expression_table, set_pathway_colors, get_tissue_list],
  app/logic/networkGraph_helper[get_pathway_list]
)


# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file") 
  # message("Loading data for expression profile: ", input_files$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes"|"genes_of_interest" #sample = "FZ0711"
  dt <- prepare_expression_table(data,expr_flag)
  return(dt)
}



ui <- function(id, patient, use_column_wrapper = FALSE) {
  ns <- NS(id)
  tagList(
    div(class = "filter-button-wrapper",
        uiOutput(ns("filterTab")),
        use_spinner(reactableOutput(ns("expression_table"))),
        plot_ui(ns("plot")) 
    )
  )
}


server <- function(id,  patient,
        dataset_type, # "genes_of_interest" nebo "all_genes"
        pathways_colname, # "pathway" nebo "all_kegg_paths_name"
        base_columns, # vektor základních sloupců
        selected_columns, column_mapping, all_colnames, shared_data) {
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # UI filter tab
    output$filterTab <- renderUI({
      req(all_colnames)
      filterTab_ui(ns("filterTab_dropdown"), dataset_type, column_mapping$dropdown_btn, all_colnames$all_columns, all_colnames$default_setting)
    })
    
    filter_state <- filterTab_server("filterTab_dropdown")
    
    # Reaktivní hodnoty filtrů
    selected_tissues_final <- reactiveVal(get_tissue_list())
    selected_pathway_final <- reactiveVal(get_pathway_list(dataset_type))
    log2fc_bigger1_final <- reactiveVal(NULL)
    log2fc_smaller1_final <- reactiveVal(NULL)
    pval_final <- reactiveVal(NULL)
    padj_final <- reactiveVal(NULL)
    log2fc_bigger1_btn_final <- reactiveVal(FALSE)
    log2fc_smaller1_btn_final <- reactiveVal(FALSE)
    pval_btn_final <- reactiveVal(FALSE)
    padj_btn_final <- reactiveVal(FALSE)
    
    # Načtení dat
    data <- reactive({
      input_data(patient, dataset_type)
    })
    
    # Filtrace dat
    filtered_data <- reactive({
      req(data())
      df <- copy(data())
      
      # --- Pathways filtr ---
      pathways_selected <- selected_pathway_final()
      if (!is.null(pathways_selected) && length(pathways_selected) > 0 && length(pathways_selected) < length(get_pathway_list(dataset_type))) {
        pattern <- paste(pathways_selected, collapse = "|")
        df <- df[grepl(pattern, get(pathways_colname))]
      }
      
      # --- Tkáně a prahové hodnoty ---
      for (filter_name in c("log2fc_bigger1", "log2fc_smaller1", "pval", "padj")) {
        tissues <- get(paste0(filter_name, "_final"))()
        btn_state <- get(paste0(filter_name, "_btn_final"))()
        if (btn_state && length(tissues) > 0) {
          for (tissue in tissues) {
            col <- switch(filter_name,
                          "log2fc_bigger1" = paste0("log2FC_", tissue),
                          "log2fc_smaller1" = paste0("log2FC_", tissue),
                          "pval" = paste0("p_value_", tissue),
                          "padj" = paste0("p_adj_", tissue))
            if (col %in% names(df)) {
              df <- df[
                switch(filter_name,
                       "log2fc_bigger1" = get(col) > 1,
                       "log2fc_smaller1" = get(col) < -1,
                       "pval" = get(col) < 0.05,
                       "padj" = get(col) < 0.05)
              ]
            }
          }
        }
      }
      
      # --- Výběr sloupců ---
      tissues <- selected_tissues_final()
      if (is.null(tissues) || length(tissues) == 0) return(df[, ..base_columns])
      selected_cols <- unlist(lapply(tissues, function(tissue) {
        c(paste0("log2FC_", tissue), paste0("p_value_", tissue), paste0("p_adj_", tissue))
      }))
      valid_cols <- intersect(selected_cols, names(df))
      df_filtered <- df[, c(base_columns, valid_cols), with = FALSE]
      return(df_filtered)
    })
    
    # Generování columns pro reactable
    column_defs <- reactive({
      message("Generating colDef for expression")
      req(selected_columns())
      generate_columnsDef(names(data()), selected_columns(), "expression", column_mapping$table, session)
    })
    
    output$expression_table <- renderReactable({
      reactable(
        as.data.frame(filtered_data()),
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
        defaultColDef = colDef(sortNALast = TRUE, align = "center"),
        columnGroups = custom_colGroup_setting("expression", selected_tissues_final()),
        defaultSorted = list("geneid" = "asc")
      )
    })
    
    # Obsluha tlačítka Confirm
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
    
    
    plot_server("plot", patient, data, dataset_type) 
  })
}


filterTab_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      updateCheckboxGroupButtons(session, "log2fc_bigger1_btn", selected = if (length(input$log2fc_bigger1_tissue) > 0) "log2FC > 1" else character(0))
    }) %>% bindEvent(input$log2fc_bigger1_tissue)
    
    observe({
      updateCheckboxGroupButtons(session, "log2fc_smaller1_btn", selected = if (length(input$log2fc_smaller1_tissue) > 0) "log2FC < -1" else character(0))
    }) %>% bindEvent(input$log2fc_smaller1_tissue)
    
    observe({
      updateCheckboxGroupButtons(session, "pval_btn",  selected = if (length(input$pval_tissue) > 0) "p-value < 0.05" else character(0))
    }) %>% bindEvent(input$pval_tissue)
    
    observe({
      updateCheckboxGroupButtons(session, "padj_btn", selected = if (length(input$padj_tissue) > 0) "p-adj < 0.05" else character(0))
    }) %>% bindEvent(input$padj_tissue)
    
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



filterTab_ui <- function(id,expr_tag,columnName_map,column_list,default_setting){
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
                                     div(style = "width: 100%",
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
                                                 choices = get_pathway_list(expr_tag), multiple = TRUE, 
                                                 options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select pathways",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-check",`dropupAuto` = FALSE))),
                                 awesomeCheckboxGroup(ns("filter_column"),"Columns selection:", 
                                                      choices  = c("Gene name","Gene ID","Pathway","log2FC","p-value","p-adj"),
                                                      selected = c("Gene name","Gene ID","Pathway","log2FC","p-value","p-adj"))
                             )
                      )
             ),
             div(style = "display: flex; justify-content: center; margin-top: 10px;",
                 # actionButton(ns("confirm_btn"),"Confirm changes"))
                 actionBttn(ns("confirm_btn"),"Confirm changes",style = "stretch",color = "success",size = "sm",individual = TRUE,value = 0))
    )
  )
}



plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("selected_plot_ui"))
  )
}


plot_server <- function(id, patient, data, expr_flag) {
  moduleServer(id, function(input, output, session) {
    
    tissue_names <- get_tissue_list()
    
    ### render ui ###
    
    output$selected_plot_ui <- renderUI({
      ns <- session$ns
      
      width_px <- if (expr_flag == "all_genes") "600px" else "600px"
      height_px <- if (expr_flag == "all_genes") "800px" else "1500px"
      
      tagList(
        div(class = "collapsible-box",
            box(width = 12, closable = FALSE, collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Volcano plot"),
                column(6, div(class = "filterTab-select-tissue",
                              radioGroupButtons(ns("selected_tissue"), "Choose a tissue :", choices = get_tissue_list(), justified = TRUE))),
                fluidRow(
                  column(6, use_spinner(plotlyOutput(outputId = ns("volcanoPlot_blood")))),
                  column(1,),
                  column(1, numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01)),
                  column(1, numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1)),
                  column(1, numericInput(ns("top_n"), "Gene labels:", value = 0, min = 0, step = 1))
                )
            )
        ),
        div(class = "collapsible-box",
            box(width = 12, closable = FALSE,collapsible = TRUE, collapsed = TRUE, title = tags$div(style = "padding-top: 8px;","Heatmap"),
                use_spinner(plotOutput(outputId = ns("heatmapPlot"), width = width_px, height = height_px)))
        )
      )
    })
    
    # 🔥 Heatmap rendering
    
    #####
    # This heatmap is for top 20 expressed genes, selected for each tissue independently. Infinit values and NA's where set to 0. In this example, there is no negative log2FC present.
    # Other possibilities:
    #      1. top 20 genes general
    #      2. top 20 up-regulated or top 20 down-regulated
    # I should also set some treshold for number of tissues displayed in heatmap.
    #####
    
    output$heatmapPlot <- renderPlot({
      req(heatmap_matrix())
      
      min_val <- min(heatmap_matrix(), na.rm = TRUE)
      max_val <- max(heatmap_matrix(), na.rm = TRUE)
      
      if (min_val >= 0) {
        custom_palette <- colorRampPalette(c("white", "red"))(255)
      } else if (max_val <= 0) {
        custom_palette <- colorRampPalette(c("blue", "white"))(255)
      } else {
        custom_palette <- colorRampPalette(c("blue", "white", "red"))(255)
      }
      
      plot_titul <- if (expr_flag == "all_genes") "Top 20 selected genes" else "All genes of interest"
      
      print(paste("Rendering heatmap for patient:", patient))
      pheatmap(heatmap_matrix(),
               scale = "none",
               cluster_rows = TRUE,
               cluster_cols = TRUE,
               show_rownames = TRUE,
               color = custom_palette,
               main = plot_titul)
    })
    # # 🌋 Volcano plot rendering
    
    output$ggvolcanoPlot <- renderPlot({
      req(data())
      
      dt_all <- rbindlist(lapply(tissue_names, function(tissue) {
        dt <- prepare_volcano(data(), tissue)
        classify_volcano_genes(dt)}))
      
      ggvolcanoPlot(dt_all)
    })
    observeEvent(input$selected_tissue, {
      message("Selected tissue: ",input$selected_tissue)
      
      output$volcanoPlot_blood <- renderPlotly({
        req(data())
        # toWebGL()
        volcanoPlot(prepare_volcano(data(), input$selected_tissue), input$selected_tissue)
      })
    })
    
    
    
    heatmap_matrix <- reactive({
      req(data())  # Ujisti se, že data jsou dostupná
      print(paste("Generating heatmap for patient:", patient))
      
      data_dt <- as.data.table(data())
      
      if(expr_flag == "all_genes"){
        p_adj_cols <- grep("^p_adj_", names(data_dt), value = TRUE)
        data_dt[, (p_adj_cols) := lapply(.SD, as.numeric), .SDcols = p_adj_cols]
        
        # Vybrat top 20 genů pro každou tkáň
        top_20_by_tissue <- lapply(p_adj_cols, function(col) {
          data_dt[get(col) > 0][order(get(col)), .(
            geneid, feature_name,
            tissue = gsub("^p_adj_", "", col),
            log2FC = get(gsub("p_adj", "log2FC", col)),
            p_adj = get(col)
          )][1:20]
        })
        
        top_20_dt <- unique(rbindlist(top_20_by_tissue))
        heatmap_data <- dcast.data.table(top_20_dt, geneid + feature_name ~ tissue, value.var = "log2FC", fill = NA)
        
        
      } else {
        log2FC_cols <- grep("^log2FC_", names(data_dt), value = TRUE)
        long_data <- melt.data.table(data_dt, id.vars = c("geneid", "feature_name"), measure.vars = log2FC_cols, variable.name = "tissue", value.name = "log2FC")
        long_data[, tissue := gsub("^log2FC_", "", tissue)]
        heatmap_data <- dcast.data.table(long_data, geneid + feature_name ~ tissue, value.var = "log2FC", fill = NA)
        
      }
      
      
      heatmap_matrix <- as.matrix(heatmap_data[, -c("geneid", "feature_name"), with = FALSE])
      heatmap_matrix <- apply(heatmap_matrix, 2, as.numeric)
      rownames(heatmap_matrix) <- heatmap_data$feature_name
      
      # Ošetření NA hodnot
      heatmap_matrix[is.na(heatmap_matrix)] <- 0
      
      print(paste("Heatmap matrix generated for:", patient))
      return(heatmap_matrix)
    })
    
    
  })
}