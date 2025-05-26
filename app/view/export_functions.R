# app/export_functions.R

box::use(
  shiny[downloadHandler, observeEvent, reactive, req],
  openxlsx[write.xlsx],
  utils[write.csv, write.table],
  billboarder[bb_export, billboarderProxy],
  networkD3[saveNetwork],
  webshot[webshot],
  ggplot2[ggsave]
)

# Export tabulky (CSV, TSV, XLSX)
#' @export
get_table_download_handler <- function(input, patient_names, filtered_data, data_list) {
  downloadHandler(
    filename = function() {
      switch(input$export_format_table,
             "csv" = "table_export.csv",
             "tsv" = "table_export.tsv",
             "xlsx" = "table_export.xlsx")
    },
    content = function(file) {
      index <- which(patient_names == input$tabset)
      export_data <- if (input$export_data_table == "filtered") {
        filtered_data[[index]]()
      } else {
        data_list[[index]]
      }
      switch(input$export_format_table,
             "csv" = { write.csv(export_data, file, row.names = FALSE) },
             "tsv" = { write.table(export_data, file, sep = "\t", row.names = FALSE, quote = FALSE) },
             "xlsx" = { write.xlsx(export_data, file) }
      )
    }
  )
}

# Export koláčového grafu
#' @export
handle_pie_download <- function(input) {
  observeEvent(input$Pie_download, {
    shiny_id <- switch(input$export_chart_pie,
                       "Consequence" = "pie1",
                       "SIFT" = "pie2",
                       "PolyPhen" = "pie3")
    proxy <- billboarderProxy(shinyId = shiny_id)
    bb_export(proxy, filename = "pie-chart")
  })
}

# Export Sankey grafu (HTML, PNG)
#' @export
get_sankey_download_handler <- function(input, p) {
  downloadHandler(
    filename = function() {
      if (input$export_format == "html") {
        "sankey.html"
      } else {
        "sankey.png"
      }
    },
    content = function(file) {
      if (input$export_format == "html") {
        saveNetwork(p(), file, selfcontained = TRUE)
      } else {
        temp_html <- tempfile(fileext = ".html")
        saveNetwork(p(), temp_html, selfcontained = TRUE)
        webshot(temp_html, file, vwidth = 733, vheight = 317)
        unlink(temp_html)
      }
    }
  )
}

# Export histogramu TVF
#' @export
get_hist_download_handler <- function(h) {
  downloadHandler(
    filename = "TVF_histogram.png",
    content = function(file) {
      ggsave(file, h(), width = 12, height = 4)
    }
  )
}
