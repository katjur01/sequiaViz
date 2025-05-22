# HLAVNI SKRIPT

box::use(
  shiny[NS, sliderInput, wellPanel, fluidRow, column, tagList, br, uiOutput, plotOutput, downloadButton, actionButton, numericInput, renderPlot, checkboxGroupInput, fluidPage, selectInput,
        icon,div,tabPanel,moduleServer,downloadHandler,observe, observeEvent,reactive,renderUI,updateCheckboxGroupInput,updateSliderInput,updateNumericInput,req,hr,verbatimTextOutput,
        renderPrint,reactiveVal],
  reactable[colDef,reactableOutput,renderReactable,reactable,getReactableState],
  # Shiny Modules (helper functions)
  shinyjs[useShinyjs, runjs, toggle],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, menuSubItem, dashboardControlbar, 
          tabItems, tabItem, bs4Card, infoBox, tabBox, tabsetPanel, bs4ValueBox, controlbarMenu, controlbarItem, 
          column, box, boxLabel, descriptionBlock, boxProfile, boxProfileItem, attachmentBlock, boxComment, userBlock, 
          updateTabItems, boxDropdown, boxDropdownItem, dropdownDivider, navbarMenu, navbarTab],
  htmltools[tags, p, span,HTML],
  shinyWidgets[pickerInput, prettySwitch, dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup,actionBttn,pickerOptions,dropdown],
  networkD3[sankeyNetwork, saveNetwork,renderSankeyNetwork,sankeyNetworkOutput],
  circlize,
  ggplot2[ggsave, ggplot, geom_density, aes, labs, theme, element_text, scale_x_continuous, scale_y_continuous, 
          geom_histogram,expansion,margin,element_rect,element_line,scale_color_manual,unit,geom_vline,annotate],
  dplyr[anti_join],
  data.table[fread],
  openxlsx[write.xlsx],
  billboarder[billboarderOutput,renderBillboarder,billboarder,bb_piechart,bb_title,bb_pie,bb_export,billboarderProxy],
  webshot[webshot],
  utils[str]
)

box::use(
  app/prepare_main_table_and_filters[default_col,load_and_prepare,add_library_column,
                            map_column_names,map_gene_region_names,map_clin_sig_names,use_spinner],
  app/vaf_plot[generate_vaf],
  app/sankey_plot[sankey_plot],
  app/pie_plots[prepare_pie_chart,make_pie_chart]
)

# UI funkce pro modul nastavujici vzhled veskerych grafickych prvku

#' @export
ui <- function(id) {
  ns <- NS(id)
  file_paths <- list.files("D:/Diplomka/secondary_analysis/per_sample_final_var_tabs/tsv_formated",full.names = TRUE)
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
    
  .sw-dropdown-content {
  border: 1px solid #ced4da !important;  /* standard Bootstrap gray */
  border-radius: 4px !important;
  box-shadow: none !important;

  background-color: white !important; /* ensure white background */
  }
  
  .glyphicon-triangle-bottom {
  font-size: 12px !important;      /* smaller size */
  line-height: 12px !important;    /* keep line height consistent */
  vertical-align: middle;           /* optional, to align nicely */
}

.glyphicon-triangle-bottom {
  display: none !important;
  width: 0 !important;
  margin: 0 !important;
  padding: 0 !important;
}

#app-somatic_var_call_tab-igv_dropdownButton {
  width: 230px !important;
  height: 38px !important;
  font-size: 16px !important;
}

                    "))
    
    
  ),
  br(),
  #fluidPage(
    fluidRow(
      column(width = 2,  # levý "sidebar"
             box(width = 12,
                 title = tags$div(
                   style = "padding-top: 8px;",
                   "Filter data by:"
                 ),
                 closable = FALSE,
                 collapsible = FALSE,
                 tagList(
               sliderInput(ns("coverage"), tags$strong("Minimal coverage"), min = 0, max = 1000, value = 10),
               #br(),
               sliderInput(ns("gnomad_slider"), tags$strong("GnomAD NFE range"), min = 0, max = 1, value = c(0, 0.999), step = 0.00001),
               fluidRow(
                 column(6, numericInput(ns("gnom_od"), label = NULL, value = 0, min = 0, max = 1, step = 0.00001)),
                 column(6, numericInput(ns("gnom_do"), label = NULL, value = 0.999, min = 0, max = 1, step = 0.00001))
               ),
               #br(),
               checkboxGroupInput(
                 inputId = ns("gene_regions"),   
                 label = tags$strong("Gene region"),
                 choices = NULL,
                 selected = c("exon","intron","upstream","3_prime_UTR","splice","downstream",       
                              "non_coding","5_prime_UTR","regulatory_region")
               ),
               #br(),
               checkboxGroupInput(
                 inputId = ns("clinvar_sig"),
                 label = tags$strong("ClinVar significance"),
                 choices = NULL, 
                 selected = c("Conflicting classifications of pathogenicity","Likely benign",
                              "Pathogenic","Uncertain significance","Benign")
               ))
               
             ),
      ),
      
      column(width = 10,
                                box(
                                 width = 12,
                                 title = tags$div(
                                   style = "padding-top: 8px;","Patient data"),
                                 closable = FALSE,
                                 collapsible = TRUE,
                                 tagList(
                                 dropdownButton(
                                   label = NULL,
                                   right = TRUE,
                                   width = "240px",
                                   icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC; margin-bottom: 0; padding-bottom: 0;"></i>'),
                                   selectInput(ns("export_data_table"), "Select data:",
                                               choices = c("All data" = "all", "Filtered data" = "filtered")),
                                   selectInput(ns("export_format_table"), "Select format:",
                                               choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                                   downloadButton(ns("Table_download"),"Download")
                                 ),
                                 dropdownButton(
                                   label = NULL,
                                   right = TRUE,
                                   width = "480px",
                                   icon = HTML('<i class="fa-solid fa-filter fa-2sm" style="color: #74C0FC; margin-bottom: 0; padding-bottom: 0;"></i>'),
                                   #tagList(
                                     div(class = "pretty-checkbox-group",
                                         prettyCheckboxGroup(
                                           inputId = ns("colFilter_checkBox"),
                                           label = HTML("<b>Show columns:</b>"),
                                           choices = NULL,
                                           selected = c(
                                             "var_name", "library", "Gene_symbol", "HGVSp", "HGVSc",
                                             "tumor_variant_freq", "tumor_depth", "gnomAD_NFE", "clinvar_sig",
                                             "clinvar_DBN", "CGC_Somatic", "gene_region", "Consequence",
                                             "all_full_annot_name"
                                           ),
                                           icon = icon("check"),
                                           status = "primary",
                                           outline = FALSE
                                         )
                                     ),
                                   div(style = "display: flex; gap: 10px; width: 100%;",
                                       actionButton(
                                         inputId = ns("show_all"),
                                         label = "Show All",
                                         style = "flex-grow: 1; width: 0;"
                                       ),
                                       actionButton(
                                         inputId = ns("show_default"),
                                         label = "Show Default",
                                         style = "flex-grow: 1; width: 0;"
                                       )
                                   )
                                 ),
                                 tags$div(style = "margin-top: 49px;"),
                                 div(style = "width: 100%;",do.call(tabsetPanel, c(
                                   lapply(seq_along(patient_names), function(i) {
                                     tabPanel(patient_names[i], use_spinner(reactableOutput(ns(paste0("my_table", i)))))
                                   }),
                                   id = ns("tabset")
                                 ))),
                                 br(),
                                 div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                                     actionButton(ns("confirm_selected"), label = "Confirm selected rows"),
                                     #actionButton(ns("igv_button"), label = "Interactive Genome Viewer", class = "my-blue-btn")
                                     div(
                                       style = "height: 38px;font-size: 16px;",
                                       dropdown(
                                         inputId = ns("igv_dropdownButton"),
                                         label = "Interactive Genome Viewer",
                                         #style = "simple",   # Avoids extra styling interference
                                         status = "primary",      # No color style
                                         #size = NULL,
                                         icon = NULL,
                                         right = TRUE,
                                         width = 230,
                                         pickerInput(
                                           inputId = ns("idpick"), 
                                           label = "Select patients for IGV:", 
                                           choices = patient_names, 
                                           options = pickerOptions(
                                             actionsBox = FALSE, 
                                             size = 4,
                                             maxOptions = 4,
                                             dropupAuto = FALSE,
                                             maxOptionsText = "Select max. 4 patients"
                                             #selectedTextFormat = "count > 3"
                                           ), 
                                           multiple = TRUE
                                         ),
                                         div(style = "display: flex; justify-content: center; margin-top: 10px;",
                                             actionBttn(
                                               inputId = ns("go2igv_button"),
                                               label = "Go to IGV",
                                               style = "stretch",
                                               color = "primary",
                                               size = "sm",
                                               individual = TRUE)
                                         )
                                       )
                                     )
                                 ),
                                 
                                 uiOutput(ns("confirm_button_ui"))
                                 )),
                               box(
                                 width = 12,
                                 title = tags$div(
                                   style = "padding-top: 8px;","Tumor variant frequency histogram"),
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
                                 div(
                                   style = "width: 80%; margin: auto;",
                                   use_spinner(plotOutput(ns("Histogram"),height = "480px")))
                               ),
                               box(width = 12,
                                   title = tags$div(
                                     style = "padding-top: 8px;","Predicted variant impact overview"),
                                   closable = FALSE,
                                   collapsible = TRUE,
                                               dropdownButton(
                                                 label = "Export Pie Plots",
                                                 right = TRUE,
                                                 width = "240px",
                                                 icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC;"></i>'),
                                                 selectInput(ns("export_chart_pie"), "Select chart:",
                                                    choices = c("Consequence","SIFT","PolyPhen")),
                                                 actionButton(ns("Pie_download"),"Download as PNG",icon = icon("download"))
                                               ),
                                               br(),
                                               br(),
                                   use_spinner(fluidRow(
                                     column(4, billboarderOutput(ns("pie1"))
                                     ),
                                     column(4, billboarderOutput(ns("pie2"))
                                     ),
                                     column(4, billboarderOutput(ns("pie3"))
                                     )
                                   ))
                                   ),
                               box(
                                 width = 12,
                                 title = tags$div(
                                   style = "padding-top: 8px;","Sankey diagram"),
                                 closable = FALSE,
                                 collapsible = TRUE,
                                 dropdownButton(
                                   label = "Export Sankey Plot",
                                   right = FALSE,
                                   width = "240px",
                                   icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC;"></i>'),
                                   selectInput(ns("export_format"), "Select format:",
                                               choices = c("HTML" = "html", "PNG" = "png")),
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

# Serverova funkce pro modul
#' @export
server <- function(id,session,shared_data,parent_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # nacteni a priprava dat
    file_paths <- list.files("D:/Diplomka/secondary_analysis/per_sample_final_var_tabs/tsv_formated", full.names = TRUE)
    patient_names <- substr(basename(file_paths),1,6)
    data_list <- load_and_prepare("D:/Diplomka/secondary_analysis/per_sample_final_var_tabs/tsv_formated")
    
    observeEvent(input$go2igv_button, {
      if (!is.null(parent_session)) {
        # Use parent session to update tabs
        updateTabItems(parent_session, "main_navbar", selected = "hidden_igv")
      }
    })
    
    output$Table_download <- downloadHandler(
      filename = function() {
        switch(input$export_format_table,
               "csv" = "table_export.csv",
               "tsv" = "table_export.tsv",
               "xlsx" = "table_export.xlsx")
        },
      content = function(file) {
        # Zvolíme data podle výběru
        index <- which(patient_names == input$tabset)
        export_data <- if (input$export_data_table == "filtered") {
          filtered_data[[index]]()  # musíš mít připravenou reaktivní funkci s filtrovanými daty
        } else {
          data_list[[index]]
        }

        # Export dle formátu
        switch(input$export_format_table,
               "csv" = {
                 write.csv(export_data, file, row.names = FALSE)
               },
               "tsv" = {
                 write.table(export_data, file, sep = "\t", row.names = FALSE, quote = FALSE)
               },
               "xlsx" = {
                 write.xlsx(export_data, file)
               }
        )
      }
    )
    
    observeEvent(input$Pie_download,{
      shiny_id <- switch (input$export_chart_pie,
              "Consequence" = "pie1",
              "SIFT" = "pie2",
              "PolyPhen" = "pie3"
              )
      proxy <- billboarderProxy(shinyId = shiny_id)
      bb_export(proxy, filename = "pie-chart")
    })
  
    output$Sankey_download <- downloadHandler(
      filename = function() {
        if (input$export_format == "html") {
          "sankey.html"
        } else if (input$export_format == "png") {
          "sankey.png"
        }
      },

      content = function(file) {
        if (input$export_format == "html") {
          # HTML export
          saveNetwork(p(), file, selfcontained = TRUE)

        } else if (input$export_format == "png") {
          #saveNetwork(p, file, selfcontained = TRUE)

          temp_html <- tempfile(fileext = ".html")
          saveNetwork(p(),temp_html, selfcontained = TRUE)
          webshot(temp_html, file, vwidth = 733, vheight = 317)
          unlink(temp_html)  # smaže dočasný HTML soubor
        }
      }
    )

    output$Hist_download <- downloadHandler(
      filename = "TVF_histogram.png",

      content = function(file) {
        ggsave(file,h(),width = 12, height = 4)
        }

    )


    # aktualizace zvolenych filteru
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

    # aktualizace zvolenych sloupcu k zobrazeni na zaklade rucniho vyberu
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

    # aktualizace zvolenych sloupcu k zobrazeni na zaklade tlacitka
    observeEvent(input$show_all,{
      updatePrettyCheckboxGroup(
        inputId = "colFilter_checkBox",
        selected = colnames(data_list[[1]])
      )
    })

    # aktualizace zvolenych sloupcu k zobrazeni na zaklade tlacitka
    observeEvent(input$show_default,{
      updatePrettyCheckboxGroup(
        inputId = "colFilter_checkBox",
        selected = c(
          "var_name", "library", "Gene_symbol", "HGVSp", "HGVSc",
          "tumor_variant_freq", "tumor_depth", "gnomAD_NFE","snpDB","COSMIC","HGMD", "clinvar_sig",
          "clinvar_DBN", "fOne","CGC_Somatic", "gene_region", "Consequence",
          "all_full_annot_name"
        )
      )
    })

    # zakladni nastaveni zobrazovanych sloupcu
    default_columns <- default_col()

    # aktualizace zobrazovanych sloupcu na zaklade zvolenych
    reactive_columns <- reactive({
      req(input$colFilter_checkBox)
      selected_columns <- input$colFilter_checkBox
      updated_columns <- default_col()
      for (col_name in names(updated_columns)) {
        updated_columns[[col_name]]$show <- col_name %in% selected_columns
      }
      return(updated_columns)
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

    # filtrace zobrazenych dat
    filtered_data <- lapply(1:length(data_list), function(i) {
      reactive({
        df <- data_list[[i]]
        filtered_df <- df[tumor_depth >= input$coverage & gnomAD_NFE >= input$gnom_od & gnomAD_NFE <= input$gnom_do & gene_region %in% input$gene_regions & clinvar_sig %in% input$clinvar_sig, ]
        return(filtered_df)
      })
    })

    # samotne vykresleni dat do tabulky
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
    
    output$confirm_button_ui <- renderUI({
      df <- selected_data_actual_patient()
      req(!is.null(df), nrow(df) > 0)
      tagList(
        br(),
        fluidRow(
          column( width = 10, reactableOutput(ns("selected_variants_table"))),
          column( width = 3,)),
        br(),
        actionButton(ns("delete_variant"), "Delete variant")
      )
    })
    
    output$selected_variants_table <- renderReactable({
      df <- selected_data_actual_patient()
      reactable(df,
                selection = "multiple",
                onClick = "select",
                highlight = TRUE,
                wrap = FALSE,
                #outlined = TRUE,
                striped = TRUE,
                defaultColDef = colDef(resizable = TRUE, show = TRUE, align = "center"),
                columns = list("var_name"= colDef(name="Variant name",minWidth = 140),
                               "Gene_symbol"=colDef(name="Gene symbol",minWidth = 110),
                               "tumor_variant_freq"=colDef(show=FALSE),
                               "Consequence"=colDef(minWidth = 140),
                               "clinvar_sig"=colDef(name="ClinVar significance",minWidth = 140),
                               "position1"=colDef(show=FALSE),
                               "patients"=colDef(name="Detected for patient",minWidth = 120,show=FALSE))
      )
    })
    
    observeEvent(input$delete_variant, {
      req(getReactableState("selected_variants_table", "selected"))
      
      selected_idx <- getReactableState("selected_variants_table", "selected")
      
      df_patient <- selected_data_actual_patient()
      selected_rows <- df_patient[selected_idx, ]
      
      # Odstranit přesně shodné řádky z selected_data()
      df_all <- selected_data()
      df_all_new <- anti_join(df_all, selected_rows, by = colnames(df_all))
      
      # Update obou datasetů
      df_patient_new <- anti_join(df_patient, selected_rows, by = colnames(df_patient))
      
      selected_data(df_all_new)
      selected_data_actual_patient(df_patient_new)
      shared_data$selected_variants <- df_all_new
    })
    
    selected_data <- reactiveVal()
    selected_data_actual_patient <- reactiveVal()
    
    observeEvent(input$confirm_selected, {
      req(input$tabset)
      
      selected_tab_id <- which(patient_names == input$tabset)
      req(getReactableState(paste0("my_table", selected_tab_id), "selected"))
      selected_idx <- getReactableState(paste0("my_table", selected_tab_id), "selected")
      
      df <- filtered_data[[selected_tab_id]]()
      df <- df[selected_idx, c("var_name", "tumor_variant_freq","Gene_symbol","Consequence","clinvar_sig"), drop = FALSE]
      df$position1 <- sub("^([^_]+)_([^_]+)_.*", "\\1:\\2", df$var_name)
      df$patients <- input$tabset
      
      combined <- as.data.frame(rbind(selected_data(),df))
      
      combined <- combined[!duplicated(combined[c("var_name", "Gene_symbol", "patients")]), ]
      
      selected_data(combined)
      selected_data_actual_patient(subset(combined,patients == input$tabset))
      shared_data$selected_variants <- combined
    })
    
    
    
    observeEvent(input$tabset,{
      req(selected_data())
      df <- selected_data()
      selected_data_actual_patient(subset(df,patients == input$tabset))
    })
    
    #vaf diagram
    output$Histogram <- renderPlot({
      req(input$tabset)
      generate_vaf(data_list[[which(patient_names == input$tabset)]],selected_data=selected_data_actual_patient(),input$tabset)
    }, height = 480)
    
    #pie plots
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
    
    #sankey network
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


