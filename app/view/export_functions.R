# app/view/export_functions.R

box::use(
  shiny[downloadHandler, observeEvent, reactive, req],
  openxlsx[write.xlsx],
  utils[write.csv, write.table],
  billboarder[bb_export, billboarderProxy],
  networkD3[saveNetwork],
  webshot2[webshot],
  ggplot2[ggsave]
)

# Export table (CSV, TSV, XLSX)
#' @export
get_table_download_handler <- function(input, patient, data, filtered_data) {
  downloadHandler(
    filename = function() {
      file_name <- switch(input$export_data_table,
                          "filtered" = paste0(patient,"_filtered_data"),
                          "all" = paste0(patient,"_all_data"))
      switch(input$export_format_table,
             "csv" = paste0(file_name,".csv"),
             "tsv" = paste0(file_name,".tsv"),
             "xlsx" = paste0(file_name,".xlsx"))
    },
    content = function(file) {
      export_data <- if (input$export_data_table == "filtered") {
        filtered_data
      } else {
        data
      }
      switch(input$export_format_table,
             "csv" = { write.csv(export_data, file, row.names = FALSE) },
             "tsv" = { write.table(export_data, file, sep = "\t", row.names = FALSE, quote = FALSE) },
             "xlsx" = { write.xlsx(export_data, file) }
      )
    }
  )
}


# Export histogramu TVF
#' @export
get_hist_download_handler <- function(patient,h) {
  downloadHandler(
    filename = paste0(patient,"_TVF_histogram.png"),
    content = function(file) {
      ggsave(file, h, width = 12, height = 4)
    }
  )
}


# Export Sankey plot (HTML, PNG)
#' @export
get_sankey_download_handler <- function(input, patient, p) {
  downloadHandler(
    filename = function() {
      if (input$export_format == "html") {
        paste0(patient,"_sankey.html")
      } else {
        paste0(patient,"_sankey.png")
      }
    },
    content = function(file) {
      if (input$export_format == "html") {
        saveNetwork(p, file, selfcontained = TRUE)
      } else {
        temp_html <- tempfile(fileext = ".html")
        saveNetwork(p, temp_html, selfcontained = TRUE)
        webshot(temp_html, file, vwidth = 733, vheight = 317)
        unlink(temp_html)
      }
    }
  )
}
