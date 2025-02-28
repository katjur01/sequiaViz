box::use(
  data.table[setnames],
  plotly[plot_ly,plotlyOutput,renderPlotly,layout],
  magrittr[`%>%`],
  ggplot2[ggplot,geom_point,scale_color_manual,geom_hline,geom_vline,ggtitle,theme_bw,ggsave,aes],
  ggrepel[geom_text_repel],
  promises[`%...>%`,catch],
  future[future],
  
)


#############################
##   create volcano plot   ##
#############################
#' @export
prepare_volcano <- function(dt, tissue) {
  # Dynamicky vybereme relevantní sloupce pro konkrétní tkáň
  
  fc_col <- paste0("log2FC_", tissue)
  pval_col <- paste0("p_value_", tissue)
  padj_col <- paste0("p_adj_", tissue)
  
  # Kontrola, jestli sloupce existují (pro případ, že by nějaká tkáň chyběla)
  if (!(fc_col %in% colnames(dt)) || !(pval_col %in% colnames(dt)) || !(padj_col %in% colnames(dt))) {
    stop(paste("Chybí sloupce pro tkáň:", tissue))
  }
  # Vytvoření nové datové tabulky s univerzálními názvy sloupců
  dt_tissue <- dt[, .(feature_name, geneid,
                      log2FC = as.numeric(as.character(get(fc_col))),
                      p_value = as.numeric(as.character(get(pval_col))),
                      p_adj = as.numeric(as.character(get(padj_col))))]
  
  # Odstranění neplatných hodnot
  dt_tissue <- dt_tissue[!is.na(log2FC) & !is.infinite(log2FC) & !is.nan(log2FC)]
  
  # Přidání názvu tkáně jako nový sloupec
  dt_tissue[, tissue := tissue]

  return(dt_tissue)
}

classify_volcano_genes <- function(dt, padj_cutoff = 0.05, logfc_cutoff = 1) {
  

  dt[, abs.logfc := abs(log2FC)]
  dt[, sig := "na"]  # Defaultní barva
  
  dt[is.na(p_adj), sig := "na"]
  dt[p_adj >= padj_cutoff, sig := "nsig"]  # Nesignifikantní
  dt[p_adj < padj_cutoff & log2FC > -logfc_cutoff & log2FC < logfc_cutoff, sig := "sig"]  # Signifikantní
  dt[p_adj < padj_cutoff & log2FC <= -logfc_cutoff, sig := "down"]  # Downregulated
  dt[p_adj < padj_cutoff & log2FC >= logfc_cutoff, sig := "up"]  # Upregulated

  return(dt)
}

#' @export
volcanoPlot <- function(dt, tissue, top_n = 10) {
  dt <- classify_volcano_genes(dt)  # Klasifikace genů
  
  # Výběr top genů pro popisky
  top_genes <- dt[order(p_adj)][1:top_n]
  
  plot <- ggplot(dt, aes(x = log2FC, y = -log10(p_adj), color = sig)) +
    geom_point(alpha = 0.7, size = 1) +
    scale_color_manual(values = c("sig" = "gray", "down" = "blue", "up" = "red", "nsig" = "black", "na" = "gray")) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "black") +  # Threshold p-adj
    geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "black") +  # Threshold logFC
    geom_text_repel(data = top_genes, aes(label = feature_name), size = 3, max.overlaps = 10) +
    ggtitle(paste("Volcano Plot -", tissue)) +
    theme_bw()
  
  return(plot)
}

#' @export
plot_volcano <- function(dt, tissue) {
  future({
    # message("🖌️ Paralelní generování plotu pro:", tissue)
    
    # Předpokládáme, že dt už je připravené (prepare_volcano proběhlo v serveru)
    volcanoPlot(dt, tissue)
  }) %...>% 
    (function(plot) {
      # message("✅ Plot hotov:", tissue)
      plot  # Vrátí ggplot objekt
    }) %>%
    catch(function(err) {
      message("❌ Chyba při generování grafu:", tissue, " - ", err$message)
      NULL
    })
}





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