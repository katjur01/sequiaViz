# app/pie_plots.R

box::use(
  billboarder[billboarderOutput,renderBillboarder,billboarder,bb_piechart,bb_title,bb_pie,bb_export,billboarderProxy,bb_colors_manual])

#' @export
prepare_pie_chart <- function(column,pie_plot_data){
  data_clean <- gsub("_", " ", gsub("\\(.*\\)", "", pie_plot_data[[column]]))
  data_pie_prepared <- as.data.frame(table(data_clean))
  return(data_pie_prepared)  
}

#' @export
make_pie_chart <- function(type,pie_plot_data){
  renderBillboarder({
    data_pie_prepared <- prepare_pie_chart(type,pie_plot_data)
    b <- billboarder()
    b <- bb_piechart(b,data = data_pie_prepared)
    b <-bb_colors_manual(b,"Set2")
    b <- bb_pie(b,label = list(
      format = htmlwidgets::JS("function(value, ratio, id) { return Math.round(ratio * 100) + '%'; }")
    ))
    b <- bb_title(b,text = type)
    b
  })  
}