library(shiny)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(officer)
library(flextable)




ui <- fluidPage(
  titlePanel("Tvorba reportu z templátu"),
  sidebarLayout(
    sidebarPanel(
      fileInput("template", "Nahraj .Rmd šablonu"),
      downloadButton("download_report", "Stáhnout report")
    ),
    mainPanel(
        titul = "Náhled HTML", uiOutput("preview_html")
      # plotOutput("plot"),
      # tableOutput("table")
    )
  )
)

server <- function(input, output) {
  # Ukázková data
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
  
  
  summary_table <- reactive({
    as.data.frame(table(data()$Kategorie))
  })
  
  output$plot <- renderPlot({
    ggplot(data(), aes(Kategorie)) +
      geom_bar(fill = "tomato") +
      theme_minimal()
  })
  
  output$table <- renderTable({
    summary_table()
  })
  
  generated_html_file <- reactive({
    req(input$template)
    
    # Pokud to není Rmd soubor, nic nevracej (náhled se nezobrazí)
    if (!grepl("\\.Rmd$", input$template$name)) return(NULL)
    
    template <- readLines(input$template$datapath)
    
    replacements <- list(
      summary_text = summary_text(),
      category_plot = paste0("![](", plot_file(), ")"),
      summary_table = paste(knitr::kable(summary_table(), format = "markdown"), collapse = "\n")
    )
    
    for (key in names(replacements)) {
      template <- gsub(paste0("{{", key, "}}"), replacements[[key]], template, fixed = TRUE)
    }
    
    temp_rmd <- tempfile(fileext = ".Rmd")
    writeLines(template, con = temp_rmd)
    
    html_file <- tempfile(fileext = ".html")
    rmarkdown::render(temp_rmd, output_file = html_file, quiet = TRUE)
    
    return(html_file)
  })
  
  output$preview_html <- renderUI({
    req(generated_html_file())
    includeHTML(generated_html_file())
  })
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      req(input$template)
      
      if (grepl("\\.Rmd$", input$template$name)) {
        # Načtení šablony
        template <- readLines(input$template$datapath)
        
        # Nahrazení placeholderů
        replacements <- list(
          summary_text = summary_text(),
          category_plot = paste0("![](", plot_file(), ")"),
          summary_table = paste(knitr::kable(summary_table(), format = "html"), collapse = "\n")
        )
        
        for (key in names(replacements)) {
          template <- gsub(paste0("{{", key, "}}"), replacements[[key]], template, fixed = TRUE)
        }
        
        temp_rmd <- tempfile(fileext = ".Rmd")
        writeLines(template, con = temp_rmd)
        
        rmarkdown::render(temp_rmd, output_file = file, quiet = TRUE)

      } else if (grepl("\\.docx$", input$template$name)) {
        # 1. Načti šablonu
        doc <- read_docx(path = input$template$datapath)
        
        doc <- body_replace_all_text(doc, "<<summary_text>>", summary_text())
        doc <- body_replace_all_text(doc, "<<plot1>>", "")
        doc <- body_add_img(doc, src = plot_file(), width = 6, height = 4)
        doc <- body_replace_all_text(doc, "<<summary_table>>", "")
        doc <- body_add_flextable(doc, flextable(summary_table()))
        
        print(doc, target = file)
        
      }
    }
  )
  
}

shinyApp(ui, server)
