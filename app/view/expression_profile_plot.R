# app/view/expression_profile_plot.R

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
  shinyWidgets[radioGroupButtons,checkboxGroupButtons,dropdown,actionBttn,awesomeCheckboxGroup,pickerInput],
  data.table[rbindlist,dcast.data.table,as.data.table,melt.data.table],
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
)



# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file") 
  # message("Loading data for expression profile: ", input_files$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes"|"genes_of_interest" #sample = "MR1507"
  dt <- prepare_expression_table(data,expr_flag)
  return(dt)
}

ui <- function(id, patient){
  ns <- NS(id)
  tagList(
    radioGroupButtons(ns("selectPlot_btn"),label = "",choices = c("Heatmap","Volcano plot"),status = "mygrey",individual = TRUE),
    uiOutput(ns("selected_plot_ui"))
  )
}


server <- function(id, patient, expr_flag) {
  moduleServer(id, function(input, output, session) {
    
    data <- reactive({
      input_data(patient, expr_flag) # expr_flag = "genes_of_interest" # sample = "DZ1601"
    })
    
    tissue_names <- get_tissue_list()
    
    #### Volcano plot for genes of interest
    # all_data <- input_data(patient, "all_genes")
    # all_data <- prepare_volcano(all_data, "Blood")
    # all_data <- all_data[!is.na(log2FC) & !is.na(padj)]
    # 
    # sub_data <- input_data(patient, "genes_of_interest")
    # sub_data <- prepare_volcano(sub_data, "Blood")
    # sub_data <- sub_data[!is.na(log2FC) & !is.na(padj)]
    # 
    # merged_data <- unique(merge(all_data, sub_data[, .(geneid, pathway)], by = "geneid", all.x = TRUE))
    # merged_data[is.na(pathway), pathway := "no_pathway"]
    # 
    # 
    # p_value_threshold <- 0.01
    # TOP <- 20
    # RANGE <- seq_len(TOP)
    # path_colours <- c(brewer.pal(8, "Dark2"),brewer.pal(8, "Set2"),brewer.pal(8, "Pastel2")) # 8 is maximum for every set, i will get 24 colors total
    # color_map <- c("sig" = "gray", "down" = "blue", "up" = "red", "nsig" = "black", "na" = "gray")
    # 
    # merged_data <- classify_volcano_genes(merged_data) 
    # merged_data[,neg_log10_padj := -log10(padj)]
    # 
    # 
    # p1 <- ggplot(data = merged_data, aes(x = log2FC, y = neg_log10_padj)) +
    #           geom_vline(xintercept = c(-2, 2), col = "gray", linetype = 'dashed') +
    #           geom_hline(yintercept = -log10(p_value_threshold), col = "gray", linetype = 'dashed') +
    #           geom_point(data = merged_data[pathway == "no_pathway", ], color = "gray90", size = 2) +
    #           geom_point(data = merged_data[pathway != "no_pathway" & sig == "nsig", ], color = "gray15", size = 2) +
    #           geom_point(data = merged_data[pathway != "no_pathway" & sig %in% c("down", "up", "sig"), ], aes(color = sig), size = 2) +
    #           scale_color_manual(values = color_map, name = "sig") +
    #           # geom_text_repel(data = merged_data[pathway != "no_pathway" & sig != "nsig",][RANGE,], aes(label = feature_name), size = 3, max.overlaps = 2*TOP) +
    #           ggtitle(paste("Volcanoplot of ", tissue, " tissue\n  top ", TOP, " genes", sep = "")) +
    #           theme(plot.title = element_text(hjust = 0.5)) + theme_bw() + theme(plot.title = element_text())+
    #           theme(legend.title = element_text(size = 10),
    #                 legend.text = element_text(size = 7),
    #                 legend.key.size = unit(0.1, "lines"))
    # p1
    
    ### render ui ###
    
    output$selected_plot_ui <- renderUI({
      ns <- session$ns
      
      width_px <- if (expr_flag == "all_genes") "600px" else "600px"
      height_px <- if (expr_flag == "all_genes") "800px" else "1500px"
      
      if (input$selectPlot_btn == "Heatmap") {
        tagList(fluidRow(
          use_spinner(plotOutput(outputId = ns("heatmapPlot"), width = width_px, height = height_px))))
      } else if (input$selectPlot_btn == "Volcano plot") {
        tagList(
          fluidRow(
            plotOutput(outputId = ns("ggvolcanoPlot"))
          ),
          fluidRow(
            column(6, div(class = "filterTab-select-tissue",
                          radioGroupButtons(ns("selected_tissue"), "Choose a tissue :", choices = get_tissue_list(), justified = TRUE)))
          ),
          fluidRow(
            column(6, use_spinner(plotlyOutput(outputId = ns("volcanoPlot_blood")))),
            column(1,),
            column(1, numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01)),
            column(1, numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1)),
            column(1, numericInput(ns("top_n"), "Gene labels:", value = 20, min = 0, step = 1))
          )
        )
      }
    })
    
    # 🔥 Heatmap rendering
    
    #####
    # This heatmap is for top 20 expressed genes, selected for each tissue independently. Infinit values and NA's where set to 0. In this example, there is no negative log2FC present.
    # Other possibilities:
    #      1. top 20 genes general
    #      2. top 20 up-regulated or top 20 down-regulated
    # I should also set some treshold for number of tissues displayed in heatmap.
    #####
    
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
    
    output$heatmapPlot <- renderPlot({
      req(heatmap_matrix())
      req(input$selectPlot_btn == "Heatmap")
      
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
        req(input$selectPlot_btn == "Volcano plot")
        # toWebGL()
        volcanoPlot(prepare_volcano(data(), input$selected_tissue), input$selected_tissue)
      })
    })
    
  })
}







# ui_heatmap <- function(id, patient){
#   ns <- NS(id)
#   tagList(
#     fluidRow(
#       use_spinner(plotOutput(outputId = ns("heatmapPlot"), width = "600px", height = "800px"))
#     )
#   )
# }
# 
# 
# server_heatmap <- function(id, patient){
#   moduleServer(id, function(input, output, session) {
#     data <- reactive({
#       input_data(patient, "all_genes")
#     })
#     
#     #####
#     # This heatmap is for top 20 expressed genes, selected for each tissue independently. Infinit values and NA's where set to 0. In this example, there is no negative log2FC present.
#     # Other possibilities:
#     #      1. top 20 genes general
#     #      2. top 20 up-regulated or top 20 down-regulated
#     # I should also set some treshold for number of tissues displayed in heatmap.
#     #####
#     
#     heatmap_matrix <- reactive({
#       req(data())  # Ujisti se, že data jsou dostupná
#       print(paste("Generating heatmap for patient:", patient))
#       
#       data_dt <- as.data.table(data())
#       p_adj_cols <- grep("^p_adj_", names(data_dt), value = TRUE)
#       data_dt[, (p_adj_cols) := lapply(.SD, as.numeric), .SDcols = p_adj_cols]
#       
#       # Vybrat top 20 genů pro každou tkáň
#       top_20_by_tissue <- lapply(p_adj_cols, function(col) {
#         data_dt[get(col) > 0][order(get(col)), .(
#           geneid, feature_name,
#           tissue = gsub("^p_adj_", "", col),
#           log2FC = get(gsub("p_adj", "log2FC", col)),
#           p_adj = get(col)
#         )][1:20]
#       })
#       
#       top_20_dt <- unique(rbindlist(top_20_by_tissue))
#       
#       heatmap_data <- dcast.data.table(top_20_dt, geneid + feature_name ~ tissue, value.var = "log2FC", fill = NA)
#       heatmap_matrix <- as.matrix(heatmap_data[, -c("geneid", "feature_name"), with = FALSE])
#       heatmap_matrix <- apply(heatmap_matrix, 2, as.numeric)
#       rownames(heatmap_matrix) <- heatmap_data$feature_name
#       
#       # Ošetření NA hodnot
#       heatmap_matrix[is.na(heatmap_matrix)] <- 0
#       
#       print(paste("Heatmap matrix generated for:", patient))
#       return(heatmap_matrix)
#     })
#     
#     output$heatmapPlot <- renderPlot({
#       req(heatmap_matrix())  # Zajisti, že máme heatmap_matrix
#       
#       min_val <- min(heatmap_matrix(), na.rm = TRUE)
#       max_val <- max(heatmap_matrix(), na.rm = TRUE)
#       
#       if (min_val >= 0) {
#         custom_palette <- colorRampPalette(c("white", "red"))(255)
#       } else if (max_val <= 0) {
#         custom_palette <- colorRampPalette(c("blue", "white"))(255)
#       } else {
#         custom_palette <- colorRampPalette(c("blue", "white", "red"))(255)
#       }
#       
#       print(paste("Rendering heatmap for patient:", patient))
#       pheatmap(heatmap_matrix(),
#                scale = "none",
#                cluster_rows = TRUE,
#                cluster_cols = TRUE,
#                show_rownames = TRUE,
#                color = custom_palette,
#                fontsize_row = 10,
#                fontsize_col = 12,
#                # width = 6,
#                # height = 8,
#                main = paste("Top 20 genes for", patient))
#     }#, width = 800, height = 600)
#     )
#   })
# }
