box::use(
  data.table[setnames],
  plotly[plot_ly,plotlyOutput,renderPlotly,layout],
  magrittr[`%>%`]
)

#########################
##   create bar plot   ##
#########################

#' @export
prepare_barPlot_data <- function(tissue,data){
  
  selected_columns <- c("Sample", "Gene", "Pathway", grep(tissue, names(data), value = TRUE))
  filtered_data <- data[, ..selected_columns]
  setnames(filtered_data, c("Sample", "Gene", "Pathway", "Tissue", "Scale", "FC"))
  
  merged_data <- filtered_data[, .(
    Pathway = paste(Pathway, collapse = ", "),
    Scale = unique(Scale),
    FC = unique(FC)
  ), by = .(Sample, Gene, Tissue)]
  
}  

#' @export
create_barPlot <- function(df,patient,tissue){ # df = copy(merged_data)
  
  y_max <- ceiling(max(abs(df$FC)))
  y_min <- -y_max
  
  p <- plot_ly(df, x = ~Gene, y = ~FC, type = 'bar', 
               marker = list(color = ~FC, 
                             colorscale = list(c(0, 0.2, 0.5, 0.8, 1),
                                               c("darkblue", "blue", "white", "red", "darkred")),
                             cmin = y_min, cmax = y_max,
                             colorbar = list(title = "FC", 
                                             len = 1, 
                                             tickvals = c( -7.5, -5, -2.5, 0, 2.5, 5, 7.5), 
                                             ticktext = c("-7.5", "-5", "-2.5", "0", "2.5", "5", "7.5"))),
               text = "",
               hovertext =  ~paste("Gene:", Gene, "<br>Pathway:", sapply(Pathway, paste, collapse=", "), "<br>FC:", FC),
               hoverinfo = 'text'
  ) %>%
    layout(title = paste0("Tissue: ",tissue," - Gene Fold Change Distribution in Pathways"), 
           xaxis = list(title = "Genes"), # ,showticklabels = FALSE
           yaxis = list(title = "FC", range = c(y_min, y_max)),
           margin = list(t = 100))
  
  return(p)
}