#app/variant_ui_server.R

box::use(
  shiny[NS, sliderInput, fluidRow, column, tagList, br, uiOutput, plotOutput, downloadButton, actionButton, numericInput, renderPlot, checkboxGroupInput, fluidPage, selectInput,
        icon,div,tabPanel,moduleServer,downloadHandler,observe, observeEvent,reactive,renderUI,updateCheckboxGroupInput,updateSliderInput,updateNumericInput,req,
        reactiveVal,showModal,modalDialog,modalButton],
  reactable[colDef,reactableOutput,renderReactable,reactable],
  bs4Dash[box,tabsetPanel,updateTabItems],
  htmltools[tags, span,HTML],
  shinyWidgets[pickerInput, dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup,actionBttn,pickerOptions,dropdown],
  networkD3[sankeyNetwork,renderSankeyNetwork,sankeyNetworkOutput],
  data.table[fread],
  billboarder[billboarderOutput]
)

box::use(
  app/logic/prepare_main_table_and_filters[load_and_prepare,add_library_column,
                            map_column_names,map_gene_region_names,map_clin_sig_names],
  app/logic/vaf_plot[generate_vaf],
  app/logic/sankey_plot[sankey_plot],
  app/logic/pie_plots[prepare_pie_chart,make_pie_chart],
  app/logic/waiters[use_spinner],
  app/view/col_settings[default_col],
  app/view/selected_variants[render_selected_variants_ui,render_selected_variants_table,
                        handle_delete_variant, handle_confirm_selected],
  app/view/export_functions[get_table_download_handler,handle_pie_download,get_sankey_download_handler,get_hist_download_handler]
)

# UI funkce pro modul nastavujici vzhled veskerych grafickych prvku v zalozce somatic variants

#' @export
ui <- function(id) {
  ns <- NS(id)
  file_paths <- list.files("/home/annamo/sequiaViz/input_files/tsv_formated",full.names = TRUE)
  patient_names <- substr(basename(file_paths), 1, 6)
  tagList(tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$style(HTML(".dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right;margin-top -1px;}
                    .checkbox label {font-weight: normal !important;}
                    .checkbox-group .checkbox {margin-bottom: 0px !important;}
                    .pretty-checkbox-group .shiny-options-group {column-count: 2 !important; column-gap: 20px !important;}
                    .pretty-checkbox-group label {font-weight: normal !important;}
                    .my-blue-btn {background-color: #007bff;color: white;border: none;}
                    .dropdown-menu .bootstrap-select .dropdown-toggle {border: 1px solid #ced4da !important; background-color: #fff !important;
                      color: #495057 !important; height: 38px !important; font-size: 16px !important; border-radius: 4px !important; 
                      box-shadow: none !important;}
                    .sw-dropdown-content {border: 1px solid #ced4da !important; border-radius: 4px !important; box-shadow: none !important;
                      background-color: white !important;}
                    .glyphicon-triangle-bottom {font-size: 12px !important; line-height: 12px !important; vertical-align: middle;}
                    .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}
                    #app-somatic_var_call_tab-igv_dropdownButton {width: 230px !important; height: 38px !important; font-size: 16px !important;}
                    "))
  ),
  br(),
  fluidRow(
    column(width = 2,
       box(width = 12,
           title = tags$div(style = "padding-top: 8px;","Filter data by:"
           ),
           closable = FALSE,
           collapsible = FALSE,
           tagList(
             sliderInput(ns("coverage"), tags$strong("Minimal coverage"), min = 0, max = 1000, value = 10),
             sliderInput(ns("gnomad_slider"), tags$strong("GnomAD NFE range"), min = 0, max = 1, value = c(0, 0.999), step = 0.00001),
             fluidRow(
               column(6, numericInput(ns("gnom_od"), label = NULL, value = 0, min = 0, max = 1, step = 0.00001)),
               column(6, numericInput(ns("gnom_do"), label = NULL, value = 0.999, min = 0, max = 1, step = 0.00001))
             ),
             checkboxGroupInput(
               inputId = ns("gene_regions"),   
               label = tags$strong("Gene region"),
               choices = NULL,
               selected = c("exon","intron","upstream","3_prime_UTR","splice","downstream",       
                            "non_coding","5_prime_UTR","regulatory_region")
             ),
             checkboxGroupInput(
               inputId = ns("clinvar_sig"),
               label = tags$strong("ClinVar significance"),
               choices = NULL, 
               selected = c("Conflicting classifications of pathogenicity","Likely benign",
                            "Pathogenic","Uncertain significance","Benign"))
         )
       ),
    ),
    column(width = 10,
      box(width = 12,
       title = tags$div(style = "padding-top: 8px;","Patient data"),
       closable = FALSE,
       collapsible = TRUE,
       tagList(
         dropdownButton(
           label = NULL,
           right = TRUE,
           width = "240px",
           icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC; margin-bottom: 0; padding-bottom: 0;"></i>'),
           selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
           selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
           downloadButton(ns("Table_download"),"Download")
         ),
         dropdownButton(
           label = NULL,
           right = TRUE,
           width = "480px",
           icon = HTML('<i class="fa-solid fa-filter fa-2sm" style="color: #74C0FC; margin-bottom: 0; padding-bottom: 0;"></i>'),
           div(class = "pretty-checkbox-group",
               prettyCheckboxGroup(
                 inputId = ns("colFilter_checkBox"),
                 label = HTML("<b>Show columns:</b>"),
                 choices = NULL,
                 selected = c("var_name", "library", "Gene_symbol", "HGVSp", "HGVSc", "tumor_variant_freq", 
                              "tumor_depth", "gnomAD_NFE", "clinvar_sig", "clinvar_DBN", "CGC_Somatic", 
                              "gene_region", "Consequence", "all_full_annot_name"),
                 icon = icon("check"),
                 status = "primary",
                 outline = FALSE
               )
           ),
           div(style = "display: flex; gap: 10px; width: 100%;",
             actionButton(inputId = ns("show_all"), label = "Show All", style = "flex-grow: 1; width: 0;"
             ),
             actionButton(inputId = ns("show_default"), label = "Show Default", style = "flex-grow: 1; width: 0;"
             )
           )
         ),
         tags$div(style = "margin-top: 49px;"),
         div(style = "width: 100%;",do.call(tabsetPanel, c(
           lapply(seq_along(patient_names), function(i) {
             tabPanel(patient_names[i], use_spinner(reactableOutput(ns(paste0("my_table", i)))))}),
           id = ns("tabset")))
         ),
         br(),
         div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
           actionButton(ns("confirm_selected"), label = "Confirm selected rows"),
           div(
             style = "height: 38px;font-size: 16px;",
             dropdown(
               inputId = ns("igv_dropdownButton"),
               label = "Interactive Genome Viewer",
               status = "primary", 
               icon = NULL,
               right = TRUE,
               width = 230,
               pickerInput(
                 inputId = ns("idpick"), 
                 label = "Select patients for IGV:", 
                 choices = patient_names, 
                 options = pickerOptions(actionsBox = FALSE, size = 4, maxOptions = 4, dropupAuto = FALSE, maxOptionsText = "Select max. 4 patients"), 
                 multiple = TRUE
               ),
               div(style = "display: flex; justify-content: center; margin-top: 10px;",
                   actionBttn(inputId = ns("go2igv_button"), label = "Go to IGV", style = "stretch", color = "primary", size = "sm", individual = TRUE)
               )
             )
           )
         ),
         uiOutput(ns("confirm_button_ui"))
       )),
     box(
       width = 12,
       title = tags$div(style = "padding-top: 8px;","Tumor variant frequency histogram"),
       closable = FALSE,
       collapsible = TRUE,
       dropdownButton(
         label = "Export Circos Plot",
         right = TRUE,
         width = "240px",
         icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC;"></i>'),
         downloadButton(ns("Hist_download"),"Download as PNG")
       ),
       br(),
       br(),
       div(style = "width: 100%; margin: auto;",
         use_spinner(plotOutput(ns("Histogram"),height = "480px"))
       )
     ),
     box(width = 12,
         title = tags$div(style = "padding-top: 8px;","Predicted variant impact overview"),
         closable = FALSE,
         collapsible = TRUE,
         dropdownButton(
           label = "Export Pie Plots",
           right = TRUE,
           width = "240px",
           icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC;"></i>'),
           selectInput(ns("export_chart_pie"), "Select chart:", choices = c("Consequence","SIFT","PolyPhen")),
           actionButton(ns("Pie_download"),"Download as PNG",icon = icon("download"))
         ),
         br(),
         br(),
         use_spinner(fluidRow(
           column(4, billboarderOutput(ns("pie1"))),
           column(4, billboarderOutput(ns("pie2"))),
           column(4, billboarderOutput(ns("pie3")))
         ))
      ),
     box(
       width = 12,
       title = tags$div(style = "padding-top: 8px;","Sankey diagram"),
       closable = FALSE,
       collapsible = TRUE,
       dropdownButton(
         label = "Export Sankey Plot",
         right = FALSE,
         width = "240px",
         icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC;"></i>'),
         selectInput(ns("export_format"), "Select format:", choices = c("HTML" = "html", "PNG" = "png")),
         downloadButton(ns("Sankey_download"),"Download")
       ),
       br(),
       tags$div(
         style = "display: flex; justify-content: space-between; width: 100%;",
         tags$span(style = "margin-left: 3cm;", "Variant"),
         tags$span(style = "margin-right: 2cm;", "Gene"),
         tags$span(style = "margin-right: 10cm;", "Pathway")
       ),
       use_spinner(uiOutput(ns("diagram"),heigth="90%",width="90%")),
       br()
     )
     )
    )
  )
}

# Serverova funkce pro modul definující funkce veskerych prvku v zalozce somatic variants
#' @export
server <- function(id, parent_session = NULL, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # nacteni a priprava dat ___________________________________________________
    file_paths <- list.files("/home/annamo/sequiaViz/input_files/tsv_formated", full.names = TRUE)
    patient_names <- substr(basename(file_paths),1,6)
    data_list <- load_and_prepare("/home/annamo/sequiaViz/input_files/tsv_formated")

    observeEvent(input$go2igv_button, {
      selected_empty <- is.null(shared_data$selected_variants) || (is.data.frame(shared_data$selected_variants) && nrow(shared_data$selected_variants) == 0)
      bam_empty <- is.null(shared_data$bam_files) || length(shared_data$bam_files) == 0
      
      if (selected_empty || bam_empty) {
        showModal(modalDialog(
          title = "Missing input",
          "You have not selected variants or patients for visualization. Please return to the Somatic variant calling tab and define them.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
      } else {
        shinyjs::runjs("document.querySelector('[data-value=\"app-hidden_igv\"]').click();")
      }
    })
    
    
    # automaticka priprava zvolenych filteru ___________________________________
    observe({
    updateCheckboxGroupInput(
      inputId = "gene_regions",
      choiceValues = unique(unlist(lapply(data_list, function(df) unique(df$gene_region)))),
      choiceNames = map_gene_region_names(unique(unlist(lapply(data_list, function(df) unique(df$gene_region))))),
      selected = unique(unlist(lapply(data_list, function(df) unique(df$gene_region))))
    )
    })
    observe({
      updateCheckboxGroupInput(
        inputId = "clinvar_sig",
        choiceValues = unique(unlist(lapply(data_list, function(df) unique(df$clinvar_sig)))),
        choiceNames = map_clin_sig_names(unique(unlist(lapply(data_list, function(df) unique(df$clinvar_sig))))),
        selected = unique(unlist(lapply(data_list, function(df) unique(df$clinvar_sig))))
      )
    })

    #uprava filtru GnomAD NFE
    observeEvent(input$gnomad_slider, {
      updateNumericInput(session, "gnom_od", value = input$gnomad_slider[1])
      updateNumericInput(session, "gnom_do", value = input$gnomad_slider[2])
    })
    observeEvent(input$gnom_od, {
      updateSliderInput(session, "gnomad_slider", value = c(input$gnom_od, input$gnom_do))
    })
    observeEvent(input$gnom_do, {
      updateSliderInput(session, "gnomad_slider", value = c(input$gnom_od, input$gnom_do))
    })
    
    # zakladni nastaveni zobrazovanych sloupcu a jejich aktualizace ____________
    default_columns <- default_col()
    observe({
      updatePrettyCheckboxGroup(
        inputId = "colFilter_checkBox",
        choiceNames = map_column_names(colnames(data_list[[1]])),
        choiceValues = colnames(data_list[[1]]),
        selected = c(
          "var_name", "library", "Gene_symbol", "HGVSp", "HGVSc",
          "tumor_variant_freq", "tumor_depth", "gnomAD_NFE","snpDB","COSMIC","HGMD", "clinvar_sig",
          "clinvar_DBN", "fOne","CGC_Somatic", "gene_region", "Consequence",
          "all_full_annot_name"
        ),
      )
    })
    observeEvent(input$show_all,{
      updatePrettyCheckboxGroup(
        inputId = "colFilter_checkBox",
        selected = colnames(data_list[[1]])
      )
    })
    observeEvent(input$show_default,{
      updatePrettyCheckboxGroup(
        inputId = "colFilter_checkBox",
        selected = c("var_name", "library", "Gene_symbol", "HGVSp", "HGVSc",
                     "tumor_variant_freq", "tumor_depth", "gnomAD_NFE","snpDB","COSMIC",
                     "HGMD", "clinvar_sig", "clinvar_DBN", "fOne","CGC_Somatic", "gene_region",
                     "Consequence", "all_full_annot_name"
        )
      )
    })
    
    # aktualizace zobrazovanych sloupcu na zaklade zvolenych ___________________
    reactive_columns <- reactive({
      req(input$colFilter_checkBox)
      selected_columns <- input$colFilter_checkBox
      updated_columns <- default_col()
      for (col_name in names(updated_columns)) {
        updated_columns[[col_name]]$show <- col_name %in% selected_columns
      }
      return(updated_columns)
    })
    
    # filtrace zobrazenych dat _________________________________________________
    filtered_data <- lapply(1:length(data_list), function(i) {
      reactive({
        df <- data_list[[i]]
        filtered_df <- df[tumor_depth >= input$coverage & gnomAD_NFE >= input$gnom_od & gnomAD_NFE <= input$gnom_do & gene_region %in% input$gene_regions & clinvar_sig %in% input$clinvar_sig, ]
        return(filtered_df)
      })
    })

    # vykresleni dat do tabulky ________________________________________________
    lapply(1:length(data_list), function(i) {
      output[[paste0("my_table", i)]] <- renderReactable({
        #req(input$tabset == patient_names[i])
        reactable(
          filtered_data[[i]](),
          selection = "multiple",
          onClick = "select",
          wrap = FALSE,
          highlight = TRUE,
          #outlined = TRUE,
          defaultPageSize = 10,
          showPageSizeOptions = TRUE,
          pageSizeOptions = c(5,10, 20, 30, 50),
          defaultColDef = colDef(resizable = TRUE, show = TRUE, align = "center"),
          striped = TRUE,
          defaultSorted = list("fOne" = "desc", "CGC_Somatic" = "desc"),
          columns = reactive_columns()
        )
      })
    })
    
    #export dat ________________________________________________________________
    output$Table_download <- get_table_download_handler(
      input = input,
      patient_names = patient_names,
      filtered_data = filtered_data,
      data_list = data_list
    )
    handle_pie_download(input)
    output$Sankey_download <- get_sankey_download_handler(
      input = input,
      p = p  # p = reaktivní funkce vracející sankey objekt
    )
    output$Hist_download <- get_hist_download_handler(h = h)
    
    # prace s vybranymi variantami, nacteni pomocne tabulky ____________________
    selected_data <- reactiveVal()
    selected_data_actual_patient <- reactiveVal()
    output$confirm_button_ui <- renderUI({
      render_selected_variants_ui(ns, selected_data_actual_patient)
    })
    output$selected_variants_table <- renderReactable({
      render_selected_variants_table(selected_data_actual_patient())
    })
    observeEvent(input$delete_variant, {
      handle_delete_variant(selected_data, selected_data_actual_patient, shared_data)
    })
    observeEvent(input$confirm_selected, {
      req(input$tabset)
      selected_tab_id <- input$tabset
      handle_confirm_selected(selected_tab_id, patient_names, filtered_data, selected_data, selected_data_actual_patient, shared_data)
    })
    observeEvent(input$tabset, {
      req(selected_data())
      df <- selected_data()
      selected_data_actual_patient(subset(df, patients == input$tabset))
    })
    
    observe({
      bam_list <- NULL
      if (!is.null(input$idpick) && length(input$idpick) > 0) {
        bam_list <- lapply(input$idpick, function(id_val) {
          list(name = id_val, file = paste0(id_val, ".bam"))
        })
      }
      shared_data$bam_files <- bam_list
    })
    
    
    # vykresleni VAF diagramu __________________________________________________
    output$Histogram <- renderPlot({
      req(input$tabset)
      generate_vaf(data_list[[which(patient_names == input$tabset)]],selected_data=selected_data_actual_patient(),input$tabset)
    }, height = 480)
    
    # vykresleni kolacovych diagramu ___________________________________________
    pie_plot_data <- reactive({
      req(input$tabset)
      selected_tab_id <- which(patient_names == input$tabset)
      pie_data <- data_list[[selected_tab_id]]
      pie_data$SIFT[pie_data$SIFT == "."] <- "unknown"
      pie_data$Consequence[pie_data$Consequence == "."] <- "unknown"
      pie_data$PolyPhen[pie_data$PolyPhen == "."] <- "unknown"
      return(pie_data)
    })
    observeEvent(input$tabset, {
      pie_plot_data <- pie_plot_data()
      output$pie1 <- make_pie_chart("Consequence",pie_plot_data)
      output$pie2 <- make_pie_chart("SIFT",pie_plot_data)
      output$pie3 <- make_pie_chart("PolyPhen",pie_plot_data)
    })
    
    # sankey network ___________________________________________________________
    plot_data <- reactive({
       req(input$tabset)
       selected_tab_id <- which(patient_names == input$tabset)
       sankey_plot(filtered_data[[selected_tab_id]]())
     })
    output$sankey_plot <- renderSankeyNetwork({
      data <- plot_data()
      sankeyNetwork(
        Links = data$links, Nodes = data$nodes,
        Source = "IDsource", Target = "IDtarget",
        Value = "value", NodeID = "name",
        sinksRight = FALSE, fontSize = 15,
        height = data$plot_height, width = "100%"
      )
    })
    output$diagram <- renderUI({
      data <- plot_data()
      sankeyNetworkOutput(ns("sankey_plot"), height = data$plot_height)
    })
  })
}

#runApp('sequiaViz')
