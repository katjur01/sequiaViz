library(shiny)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(officer)
library(flextable)
library(reactable)
library(data.table)


ui <- fluidPage(
  titlePanel("Tvorba reportu z templátu"),
  sidebarLayout(
    sidebarPanel(
      fileInput("template", "Nahraj .Rmd šablonu"),
      downloadButton("download_report", "Stáhnout report")
    ),
    mainPanel(
      h3("Germline mutations"), uiOutput("germ_content"),
      br(),
      h3("Somatic mutations"), uiOutput("som_content"),  # Změna na uiOutput pro dynamický obsah
      br(),
      h3("Gene fusions"), uiOutput("fusion_content")  # Změna na uiOutput pro dynamický obsah
    )
  )
)

server <- function(input, output) {

  shared_data <- reactiveValues(germline_data = reactiveVal(NULL))
  
  germline_dt <- reactive({
    dt <- data.table(
      var_name = "19_45352801_C/G",
      Gene_symbol = "ERCC2",
      variant_freq = 0.286,
      coverage_depth = 28,
      Consequence = "missense_variant",
      HGVSc = "c.1847G>C",
      HGVSp = "p.R616P",
      variant_type = "SNV",
      Feature = "NM_000400.4",
      clinvar_sig = "Pathogenic",
      variant_classification = "IIIc",
      therapeutic_option = "Sonidegib, vismodegib"
      # 
    )
    dt[,reads := paste0(round(variant_freq * coverage_depth),"/",coverage_depth)]
  })

  
  somatic_dt <- reactive({
    dt <- data.table(
      sample = character(),
      var_name = character(),
      Gene_symbol = character(),
      variant_freq= character(),
      coverage_depth = character(),
      Consequence = character(),
      HGVSc = character(),
      HGVSp = character(),
      variant_type = character(),
      Feature = character(),
      clinvar_sig = character(),
      reads = character(),
      variant_classification = character(),
      therapeutic_option = character()
    )
    dt
  })

  fusion_dt <- reactive({
    dt <- data.table(
      gene1 = "KMT2A",
      transcript5 = "NM_",
      gene2 = "MLLT3",
      transcript3 = "NM_",
      overall_support = 35,
      phasing = "in-frame",
      reads = "x/y"
    )
    dt
  })
  
  
  output$fusion_content <- renderUI({
    if (is.null(fusion_dt()) || nrow(fusion_dt()) == 0) {
      p("No clinically relevant fusion genes were found.")
    } else {
      reactableOutput("fusion_resultTab")
    }
  })
  
  observe({
    if (!is.null(fusion_dt()) && nrow(fusion_dt()) > 0) {
      output$fusion_resultTab <- renderReactable({
        reactable(as.data.frame(fusion_dt()))
      })
    }
  })
  
  
  output$som_content <- renderUI({
    if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
      p("No variants with known or potential clinical significance were found.")
    } else {
      reactableOutput("som_resultTab")
    }
  })
  
  observe({
    if (!is.null(somatic_dt()) && nrow(somatic_dt()) > 0) {
      output$som_resultTab <- renderReactable({
        reactable(as.data.frame(somatic_dt()))
      })
    }
  })
  
  output$germ_content <- renderUI({
    if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
      p("No variants with known or potential clinical significance in genes associated with hereditary cancer-predisposing syndromes were found.")
    } else {
      reactableOutput("germ_resultTab")
    }
  })

  observe({
    if (!is.null(germline_dt()) && nrow(germline_dt()) > 0) {
      output$germ_resultTab <- renderReactable({
        reactable(as.data.frame(germline_dt()))
      })
    }
  })


  
  data <- reactive({
    data.frame(
      Kategorie = sample(c("A", "B", "C"), 100, replace = TRUE)
    )
  })
  
  summary_text <- reactive({
    sprintf("Dataset obsahuje %d záznamů rozdělených do %d kategorií.",
            nrow(data()), length(unique(data()$Kategorie)))
  })
  
  plot_file <- reactive({
    file <- tempfile(fileext = ".png")
    p <- ggplot(data(), aes(Kategorie)) +
      geom_bar(fill = "steelblue") +
      theme_minimal()
    ggsave(filename = file, plot = p, width = 6, height = 4)
    return(file)
  })

  
  output$plot <- renderPlot({
    ggplot(data(), aes(Kategorie)) +
      geom_bar(fill = "tomato") +
      theme_minimal()
  })
  

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(input$template)
      
        # 1. Načti šablonu
        doc <- read_docx(path = input$template$datapath)
        
        doc <- body_replace_all_text(doc, "<<summary_text>>", summary_text())
        doc <- body_replace_all_text(doc, "<<germline_table>>", "")
        doc <- body_add_flextable(doc, flextable(germline_dt()))
        
        doc <- body_replace_all_text(doc, "<<somatic_table>>", "")
        doc <- body_add_flextable(doc, flextable(somatic_dt()))
        
        doc <- body_replace_all_text(doc, "<<plot1>>", "")
        doc <- body_add_img(doc, src = plot_file(), width = 6, height = 4)
        
        print(doc, target = file)

        # 
        # germ_data <- if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
        #   NULL  # Pro textovou zprávu použijeme NULL
        # } else {
        #   as.data.frame(germline_dt())
        # }
        # 
        # # Připravit somatic data
        # som_data <- if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
        #   NULL  # Pro textovou zprávu použijeme NULL
        # } else {
        #   as.data.frame(somatic_dt())
        # }
        # 
        # if (is.null(germ_data)) {
        #   doc <- body_replace_all_text(doc, "<<germline_table>>", "No variants with known or potential clinical significance were found.")
        # } else {
        #   # Označíme místo speciálním řetězcem, který pak nahradíme tabulkou
        #   doc <- body_add_flextable(doc, "<<germline_table>>", flextable(germ_data))
        # }
        # 
        # # Somatic tabulka - stejný princip
        # if (is.null(som_data)) {
        #   doc <- body_replace_all_text(doc, "<<somatic_table>>", "No variants with known or potential clinical significance were found.")
        # } else {
        #   doc <- body_add_flextable(doc, "<<somatic_table>>", flextable(som_data))
        # }
    }
  )
 
  
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
