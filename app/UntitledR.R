library(data.table)
library(openxlsx)
library(reactable)
library(shiny)
library(htmltools)
library(bs4Dash)

ui <- fluidPage(
  reactableOutput("table")
)

server <- function(input, output, session) {
  
  library <- "MOII_e117"
  germline_variant_calling_project <- "117_WES_germline"
  data <- fread("./input_files/MOII_e117/117_WES_germline/per_sample_final_var_tabs/tsv_formated/VH0452krev.variants.tsv")

  data[, Visual_Check := ""]
  
  filtered_dt <- data[, names(data) %in% c("sample", "var_name", "Gene_symbol", "variant_freq", "coverage_depth", "in_library", "HGVSc", "HGVSp", "Visual_Check",
                                           "gnomAD_NFE","snpDB","gene_region", "Consequence", "clinvar_sig", "all_full_annot_name"), with = FALSE]
  
  setcolorder(filtered_dt, c("sample", "var_name", "variant_freq", "in_library", "Gene_symbol", "coverage_depth", "gene_region",
                             "gnomAD_NFE", "clinvar_sig","snpDB","Consequence", "HGVSc", "HGVSp", "all_full_annot_name", "Visual_Check"))

  
  
  dt <- filtered_dt[1:100]
  output$table <- renderReactable({
    message("RenderReactable called")
    reactable(
      dt,
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
      columns = list(
        IGV = colDef(
          maxWidth = 100, filterable = FALSE,
          cell = function(value, index) {
            if (index <= input$table_state$lastRow - input$table_state$firstRow + 1) {
              message(paste("Rendering action button for row", index))
              actionButton(
                inputId = paste0("igvButton_", index),
                label = NULL,
                icon = icon("play")
              )
            }
          }
        )
      )
    )
  })

  observe({
    input$table_state
    invalidateLater(20)
  })
}

shinyApp(ui, server)
