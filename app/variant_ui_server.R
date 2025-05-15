# HLAVNI SKRIPT

# nacteni knihoven a pomocnych funkci
# library(shiny)
# library(reactable)
# library(data.table)
# library(htmltools)
# library(shinyWidgets)
# library(bslib)
# library(networkD3)
# library(dplyr)
# library(circlize)
# library(webshot)
# library(ggplot2)
# library(openxlsx)


box::use(
  # Shiny core
  shiny[NS, sliderInput, wellPanel, fluidRow, column, tagList, br, uiOutput, plotOutput, downloadButton, actionButton, numericInput, renderPlot, checkboxGroupInput, fluidPage, selectInput,
        icon,div,tabPanel,moduleServer,downloadHandler,observe, observeEvent,reactive,renderUI,updateCheckboxGroupInput,updateSliderInput,updateNumericInput,req,hr,verbatimTextOutput,
        renderPrint],
  reactable[colDef,reactableOutput,renderReactable,reactable,getReactableState],
  # Shiny Modules (helper functions)
  shinyjs[useShinyjs, runjs, toggle],
  shinyBS[bsCollapse,bsCollapsePanel],
  # bs4Dash UI elements
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, menuSubItem, dashboardControlbar, 
          tabItems, tabItem, bs4Card, infoBox, tabBox, tabsetPanel, bs4ValueBox, controlbarMenu, controlbarItem, 
          column, box, boxLabel, descriptionBlock, boxProfile, boxProfileItem, attachmentBlock, boxComment, userBlock, 
          updateTabItems, boxDropdown, boxDropdownItem, dropdownDivider, navbarMenu, navbarTab],
  
  # Useful HTML tools
  htmltools[tags, p, span,HTML],
  
  # shinyWidgets (Custom widgets)
  shinyWidgets[pickerInput, prettySwitch, dropdownButton,prettyCheckboxGroup,updatePrettyCheckboxGroup],
  
  # Network and data visualization
  networkD3[sankeyNetwork, saveNetwork,renderSankeyNetwork,sankeyNetworkOutput],
  circlize,
  ggplot2[ggsave, ggplot, geom_density, aes, labs, theme, element_text, scale_x_continuous, scale_y_continuous, 
          geom_histogram,expansion,margin,element_rect,element_line,scale_color_manual,unit,geom_vline,annotate],
  
  # Data manipulation and export
  #dplyr[select, mutate, filter, bind_rows],
  dplyr,
  data.table[fread],
  openxlsx[write.xlsx],
  billboarder[billboarderOutput,renderBillboarder,billboarder,bb_piechart,bb_title,bb_pie,bb_export,billboarderProxy],
  # Webshot for rendering images
  webshot[webshot],
  # Other utility functions
  utils[str]
)

box::use(
  app/pomocnefunkce[default_col,load_and_prepare,add_library_column,
                            map_column_names,map_gene_region_names,map_clin_sig_names,use_spinner,
                            sankey_plot],
)

# UI funkce pro modul nastavujici vzhled veskerych grafickych prvku

#' @export
ui <- function(id) {
  ns <- NS(id)
  file_paths <- list.files("D:/Diplomka/secondary_analysis/per_sample_final_var_tabs/tsv_formated",full.names = TRUE)
  patient_names <- substr(basename(file_paths), 1, 6)
  tagList(tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
    tags$style(HTML(".dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none;}
                    .checkbox label {font-weight: normal !important;}
                    .checkbox-group .checkbox {margin-bottom: 0px !important;}
                    .pretty-checkbox-group .options-container {
        column-count: 2;
        column-gap: 20px;
      }
                    "))
  ),
  br(),
  #fluidPage(
    fluidRow(
      column(width = 2,  # levý "sidebar"
             wellPanel(
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
               ),
               
             ),
      ),
      
      column(width = 10,  # hlavní panel
             fluidPage(      bsCollapse(
                               id = "bsco",
                               #multiple = TRUE,
                               open = "patient_panel1",
                               bsCollapsePanel(
                                 title = "Patient data",
                                 value = "patient_panel1",
                                 style = "success",
                                 tagList(
                                 tags$style(HTML("
                                     .dropdown-toggle {float: right;margin-top -1px;}
                                     ")),
                                 dropdownButton(
                                   label = NULL,
                                   right = TRUE,
                                   width = "240px",
                                   icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC;"></i>'),
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
                                   icon = HTML('<i class="fa-solid fa-filter fa-2sm" style="color: #74C0FC";></i>'),
                                   tagList(
                                     prettyCheckboxGroup(
                                       inputId = ns("colFilter_checkBox"),
                                       label = "Show columns:",
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
                                 br(),
                                 br(),
                                 br(),
                                 do.call(tabsetPanel, c(
                                   lapply(seq_along(patient_names), function(i) {
                                     tabPanel(patient_names[i], reactableOutput(ns(paste0("my_table", i))))
                                   }),
                                   id = ns("tabset")
                                 )),
                                 # br(),
                                 # reactableOutput(ns("selected_checkbox_table")), 
                                 hr())),
                               bsCollapsePanel(
                                 "Tumor variant frequency histogram",
                                 
                                 dropdownButton(
                                   label = "Export Circos Plot",
                                   right = TRUE,
                                   width = "240px",
                                   icon = HTML('<i class="fa-solid fa-download" style="color: #74C0FC;"></i>'),
                                   #selectInput(ns("export_format"), "Select format:",
                                               #choices = c("PNG" = "png")),
                                   downloadButton(ns("Hist_download"),"Download as PNG")
                                 ),
                                 br(),
                                 br(),
                                 #verbatimTextOutput(ns("debug_checkbox_data")),
                                 div(
                                   style = "width: 80%; margin: auto;",
                                   use_spinner(plotOutput(ns("Histogram"),height = "480px"))),
                               hr()),
                               bsCollapsePanel("Predicted variant impact overview",
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
                                               fluidRow(
                                                 column(4,
                                                        use_spinner(billboarderOutput(ns("pie1")))
                                                 ),
                                                 column(4,
                                                        use_spinner(billboarderOutput(ns("pie2")))
                                                 ),
                                                 column(4,
                                                        use_spinner(billboarderOutput(ns("pie3")))
                                                 )
                                               ),
                                               hr()),
                               bsCollapsePanel(
                                 "Sankey diagram",
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
                               )),
                             )
      )
    ))
}

# Serverova funkce pro modul
#' @export
server <- function(id,session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # nacteni a priprava dat
    file_paths <- list.files("D:/Diplomka/secondary_analysis/per_sample_final_var_tabs/tsv_formated", full.names = TRUE)
    patient_names <- substr(basename(file_paths),1,6)
    data_list <- load_and_prepare("D:/Diplomka/secondary_analysis/per_sample_final_var_tabs/tsv_formated")
  
    b <- billboarder()
    b <- bb_piechart(b,data = data.frame(
        category = c("A", "B"),
        value = c(30, 70)
      ))
    b <- bb_title(b,text = "Chart 1")
    
    pie_plot_data <- reactive({
      req(input$tabset)
      selected_tab_id <- which(patient_names == input$tabset)
      pie_data <- filtered_data[[selected_tab_id]]()
      pie_data$SIFT[pie_data$SIFT == "."] <- "unknown"
      pie_data$Consequence[pie_data$Consequence == "."] <- "unknown"
      pie_data$PolyPhen[pie_data$PolyPhen == "."] <- "unknown"
      return(pie_data)
    })
    
    prepare_pie_chart <- function(column){
      data_clean <- gsub("_", " ", gsub("\\(.*\\)", "", pie_plot_data()[[column]]))
      data_pie_prepared <- as.data.frame(table(data_clean))
      return(data_pie_prepared)  
    }
    
    output$pie1 <- renderBillboarder({
      data_pie_prepared <- prepare_pie_chart("Consequence")
      b <- billboarder()
      b <- bb_piechart(b,data = data_pie_prepared)
      b <- bb_pie(b,label = list(
        format = htmlwidgets::JS("function(value, ratio, id) { return Math.round(ratio * 100) + '%'; }")
      ))
      b <- bb_title(b,text = "Consequence")
      b
    })
    
    output$pie2 <- renderBillboarder({
      data_pie_prepared <- prepare_pie_chart("SIFT")
      b <- billboarder()
      b <- bb_piechart(b,data = data_pie_prepared)
      b <- bb_pie(b,label = list(
        format = htmlwidgets::JS("function(value, ratio, id) { return Math.round(ratio * 100) + '%'; }")
      ))
      b <- bb_title(b,text = "SIFT")
      b
    })
    
    output$pie3 <- renderBillboarder({
      data_pie_prepared <- prepare_pie_chart("PolyPhen")
      b <- billboarder()
      b <- bb_piechart(b,data = data_pie_prepared)
      b <- bb_pie(b,label = list(
        format = htmlwidgets::JS("function(value, ratio, id) { return Math.round(ratio * 100) + '%'; }")
      ))
      b <- bb_title(b,text = "PolyPhen")
      b
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

    plot_data <- reactive({
       req(input$tabset)
       selected_tab_id <- which(patient_names == input$tabset)
       sankey_plot(filtered_data[[selected_tab_id]]())
     })

    p <- reactive({
      data <- plot_data()
      sankeyNetwork(
        Links = data$links, Nodes = data$nodes,
        Source = "IDsource", Target = "IDtarget",
        Value = "value", NodeID = "name",
        sinksRight = FALSE, fontSize = 15,
        height = data$plot_height, width = "100%"
      )
    })

    # Render Sankey Network when data is available
    output$sankey_plot <- renderSankeyNetwork({
      p()
    })
    
    selected_data <- reactive({
      req(input$tabset)
      selected_tab_id <- which(patient_names == input$tabset)
      
      selected_idx <- getReactableState(paste0("my_table", selected_tab_id), "selected")
      
      df <- filtered_data[[selected_tab_id]]()
      df <- df[selected_idx, c("var_name", "tumor_variant_freq"), drop = FALSE]
      return(df)
    })
    
    
    output$selected_checkbox_table <- renderReactable({
      df <- selected_data()
      reactable(df)
    })
    
    h <- reactive({
      req(input$tabset)
      data <- fread(paste0("D:/Diplomka/secondary_analysis/per_sample_final_var_tabs/tsv_formated/",
                                   input$tabset,
                                   ".variants.tsv"))
      data <- data[,"tumor_variant_freq", drop = FALSE]
      
      # basic histogram
      ggplot(data, aes(x=tumor_variant_freq)) +
        geom_histogram(binwidth = 0.01,fill="#A7C6ED", color="#e9ecef", alpha=0.9)+
        #geom_density(color = "#333333", size = 0.5)+
        geom_density(aes(color = "Distribution curve"), size = 0.5) +  # <- klíčová změna
        scale_color_manual(values = c("Distribution curve" = "#333333"), name = "") +  # <- legenda
        labs(x="Tumor variant frequency",y="Number of found variants")+
        #geom_vline(xintercept = 0.5, color = "blue", linetype = "dashed", size = 1) +
        # geom_vline(xintercept = c(0.2,0.3), color = "blue", linetype = "dashed", size = 1) +
        # annotate("text",
        #          x = c(0.2,0.3),
        #          y = rep(Inf, length(c(0.2,0.3))),  # top of the plot
        #          label = c("Varianta 1                     ","Varianta B                     "),
        #          vjust = -0.5, size = 5, angle = 90, color = "blue")+
        geom_vline(xintercept = selected_data()[["tumor_variant_freq"]], color = "blue", linetype = "dashed", size = 1) +
        annotate("text", x = selected_data()[["tumor_variant_freq"]],
           y = rep(Inf, length(selected_data()[["tumor_variant_freq"]])),  # top of the plot
           label = paste0(selected_data()[["var_name"]], "                                        "),
           vjust = -0.5, size = 5, angle = 90, color = "blue")+
        scale_x_continuous(breaks = seq(0,1,by=0.05),minor_breaks = seq(0, 1, by = 0.01))+
        scale_y_continuous(expand=expansion(mult = c(0, 0.01)),breaks = seq(0,100,by=1),minor_breaks = seq(0,100,by=1))+
        theme(
          axis.title.x = element_text(size=15,face="bold"),
          axis.title.y = element_text(size=15,face="bold"),
          axis.text.x = element_text(size=15,margin = margin(t=10)),
          axis.text.y = element_text(size=15,margin = margin(r=10)),
          panel.grid.major = element_line(color = "grey80"),
          panel.grid.minor = element_line(color = "grey80"),
          panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          legend.text = element_text(size = 13),
          legend.key.size = unit(0.5,"cm")
        )

    })

    output$Histogram <- renderPlot({
      h()
      }, height = 480)


    # Dynamically set plot height based on data
    output$diagram <- renderUI({
      data <- plot_data()
      sankeyNetworkOutput(ns("sankey_plot"), height = data$plot_height)
    })
  })
}


