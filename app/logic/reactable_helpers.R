box::use(  
  shiny[tags,tagList, icon, actionButton, textInput,HTML],
  reactable,
  reactable[colDef,JS,colGroup],
  htmltools[div,tags,tagAppendAttributes],
  stats[na.omit],
  bs4Dash[actionButton],
  reactablefmtr[pill_buttons,data_bars],
  htmltools[HTML,span, div, tagList]
)

box::use(
  app/logic/prepare_table[get_tissue_list]
)


# Select input filter with an "All" default option
#' @export
selectFilter <- function(tableId, style = "width: 100%; height: 100%;") {
  function(values, name) {
    tags$select(
      # Set to undefined to clear the filter
      onchange = sprintf("
        const value = event.target.value
        Reactable.setFilter('%s', '%s', value === '__ALL__' ? undefined : value)
      ", tableId, name),
      # "All" has a special value to clear the filter, and is the default option
      tags$option(value = "__ALL__", "All"),
      lapply(unique(values), tags$option),
      "aria-label" = sprintf("Filter %s", name),
      style = style
    )
  }
}

# Min range filter input that handles NaNs
#' @export
minRangeFilter <- function(tableId, style = "width: 100%;") {
  function(values, name) {
    values <- na.omit(values)
    oninput <- sprintf("Reactable.setFilter('%s', '%s', this.value)", tableId, name)
    tags$input(
      type = "range",
      min = floor(min(values)),
      max = ceiling(max(values)),
      value = floor(min(values)),
      oninput = oninput,
      style = style,
      "aria-label" = sprintf("Filter by minimum %s", name)
    )
  }
}

# Min value filter method that handles NaNs
#' @export
filterMinValue <- JS("(rows, columnId, filterValue) => {
  return rows.filter(row => {
    const value = row.values[columnId]
    return !isNaN(value) && value >= filterValue
  })
}")

# Generate column definitions (colDef) for a reactable 
#' @export
generate_columnsDef <- function(column_names, selected_columns, tag, column_mapping,session) {

  # never show this columns in table:
  if (tag == "fusion") {
    hide <- c("sample", "png_path", "svg_path")
  } else if (tag == "germline") {
    hide <- c("sample")
  } else if (tag == "expression") {
    hide <- c("sample")
  } else {
    hide <- c()
    print("No column has been selected for permanent hiding")
  }
  custom_colDef <- custom_colDef_setting(tag,session,column_names)
  column_defs <- lapply(column_names, function(col) {
    if (col %in% hide) {
      return(colDef(show = FALSE))
    } else if (col %in% selected_columns) {
      header_name <- if (!is.null(column_mapping[[col]])) column_mapping[[col]] else col
      if (col %in% names(custom_colDef)) {
        
        custom_def <- custom_colDef[[col]]
        custom_def$header <- header_name
        return(custom_def)
      } else {
        return(colDef(show = TRUE, header = header_name))
      }
    } else {
      return(colDef(show = FALSE))
    }
  })
  names(column_defs) <- column_names

  return(column_defs)
}



#' @export
columnName_map <- function(tag, expr_flag = NULL, all_columns = NULL){
  if (tag == "fusion"){
    map_list <- list(
      gene1 = "Gene 1",
      gene2 = "Gene 2",
      arriba.called = "Arriba called",
      starfus.called = "StarFus called",
      arriba.confidence = "Arriba confidence",
      overall_support = "Overall support",
      IGV = "IGV",
      Visual_Check = "Visual check",
      Notes = "Notes",
      position1 = "Position 1",
      position2 = "Position 2",
      strand1 = "Strand 1",
      strand2 = "Strand 2",
      arriba.site1 = "Arriba site 1",
      arriba.site2 = "Arriba site 2",
      starfus.splice_type = "StarFus splice type",
      DB_count = "DB count",
      DB_list = "DB list",
      arriba.split_reads = "Arriba split reads",
      arriba.discordant_mates	= "Arriba discordant mates",
      arriba.break_coverage	= "Arriba break coverage",
      arriba.break2_coverage = "Arriba break coverage 2",
      starfus.split_reads = "StarFus split reads",
      starfus.discordant_mates = "StarFus discordant mates",
      starfus.counter_fusion1 = "StarFus counter fusion 1",
      starfus.counter_fusion2 = "StarFus counter fusion 2",
      arriba.break_seq = "Arriba break sequence",
      starfus.break_seq = "StarFus break sequence")
  } else if (tag == "germline"){
    map_list <- list(
      var_name = "Variant name",
      variant_freq = "Frequency",
      in_library = "In library",
      Gene_symbol = "Gene name",
      coverage_depth = "Coverage",
      gene_region = "Gene region",
      gnomAD_NFE = "gnomAD NFE",
      clinvar_sig = "Clinvar sig",
      snpDB = "Clinvar ID",
      Consequence = "Consequence",
      HGVSc = "HGVSc",
      HGVSp = "HGVSp",
      all_full_annot_name = "Full annotation name",
      occurance_in_cohort = "Occurence in cohort",
      in_samples = "In samples",
      alarm = "Alarm",
      full_annot_name = "Full annotation name",
      var_gen_coord = "Variant genomic coordinate",
      variant_type = "Variant type",
      genotype = "Genotype",
      Called_by = "Called by",
      `1000g_EUR_AF` = "1000g EUR AF",
      COSMIC = "COSMIC",
      HGMD = "HGMD",
      NHLBI_ESP = "NHLBI ESP",
      clinvar_DBN = "ClinVar DBN",
      fOne = "fOne",
      BRONCO = "BRONCO",
      `md-anderson` = "MD Anderson",
      trusight_genes = "Trusight genes",
      CGC_Germline = "CGC Germline",
      CGC_Tumour_Germline = "CGC tumour germline",
      PolyPhen = "PolyPhen",
      SIFT = "SIFT",
      CADD_RAW = "CADD raw",
      CADD_PHRED = "CADD phred",
      IMPACT = "Impact",
      SOMATIC = "Somatic",
      PHENO = "Phenotype",
      GENE_PHENO = "Gene phenotype",
      PUBMED = "PubMed",
      EXON = "Exon",
      INTRON = "Intron",
      Feature = "Feature",
      Feature_type = "Feature type",
      `Annotation source` = "Annotation source",
      Gene = "Gene ID")
    
  } else if (tag == "expression"){
    
    dropdown_btn <- list()
    table <- list()
    tissue_list <- get_tissue_list()
    
    rename_column <- function(col, tissue_list) {
      for (tissue in tissue_list) {
        if (grepl(tissue, col)) {
          prefix <- gsub(paste0("_", tissue), "", col)  # Odstraníme tkáň z názvu
          tissue <- gsub("_", " ", tissue)  # Nahrazení podtržítka mezerou pro čitelnost

          # Vrátíme přejmenovaný sloupec jen pokud je relevantní
          if (prefix %in% c("log2FC", "p_value", "p_adj")) {
            return(paste(tissue, ifelse(prefix == "log2FC", "log2FC",
                                        ifelse(prefix == "p_value", "p-value", "p-adj"))))
          }
        }
      }
      return(NULL)  # Pokud se sloupec nemá přejmenovat, vrátíme NULL
    }

    if (expr_flag == "all_genes"){
        static_columns <- list(
          feature_name = "Gene name",
          geneid = "Gene ID",
          refseq_id = "RefSeq ID",
          type = "Type",
          gene_definition = "Gene definition",
          all_kegg_gene_names = "KEGG gene names",
          all_kegg_paths_name = "Pathway",
          num_of_paths = "Pathway (n)")
    } else {
        static_columns <- list(
          feature_name = "Gene name",
          geneid = "Gene ID",
          pathway = "Pathway")
    }

    for (col in all_columns) {
      new_name <- rename_column(col, tissue_list)
      if (!is.null(new_name)) {  # Přidáme jen pokud má smysl
        dropdown_btn[[col]] <- new_name
      }
    }

    for (col in all_columns) {
      if (grepl("^log2FC_", col)) {
        table[[col]] <- "log2FC"
      } else if (grepl("^p_value_", col)) {
        table[[col]] <- "p-value"
      } else if (grepl("^p_adj_", col)) {
        table[[col]] <- "p-adj"
      }
    }

    dropdown_btn <- append(static_columns,dropdown_btn)
    table <- c(static_columns,table)
    map_list <- list(dropdown_btn = dropdown_btn, table = table)
    
  } else {
    print("NOT germline, expression or fusion")
  }
  return(map_list)
}

custom_colDef_setting <- function(tag, session = NULL, column_names = NULL){
  if (tag == "fusion"){
    custom_colDef <- list(
      gene1 = colDef(minWidth = 120,filterable = TRUE,sticky = "left"),
      gene2 = colDef(minWidth = 120,filterable = TRUE,sticky = "left"),
      arriba.called = colDef(width = 100,
                             cell = function(value) {
                                   div(class = paste0("tag called-", tolower(value)),value)}),
      starfus.called = colDef(width = 100,
                              cell = function(value) {
                                    div(class = paste0("tag called-", tolower(value)),value)}),
      arriba.confidence = colDef(width = 140,filterable = TRUE,
                                 #filterInput = selectFilter("tbl-fusion")
                                 cell = function(value) {
                                   if (is.na(value)) {
                                     return(NULL)  # Do not render anything for NA values
                                   }
                                   div(class = paste0("tag confidence-", tolower(value)),value)}),
      overall_support = colDef(width = 100),
      IGV = colDef(maxWidth = 80,
        cell = function(value, index) {
          actionButton(
            inputId = paste0(session$ns("igvButton_"), index),
            size = "md",
            label = NULL,
            icon = icon("play"),
            onclick = sprintf("event.stopPropagation();")
            # onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'});",
            #                   session$ns("igvButton_click"), index)
            # onclick = sprintf("window.open('http://localhost:8080', '_blank'); event.stopPropagation();")
          )}),
      Visual_Check = colDef(width = 110,
        cell = function(value, index) {
          tagList(
            actionButton(paste0(session$ns("yesButton_"), index), icon("check"), class = "btn btn-primary btn-md", onclick = sprintf("event.stopPropagation();")),
            actionButton(paste0(session$ns("noButton_"), index), icon("close"), class = "btn btn-primary btn-md", onclick = sprintf("event.stopPropagation();")))}),
      Notes = colDef(minWidth = 120,
        cell = function(value, index) {
          textInput(session$ns(paste0("notesTable_", index)), label = NULL)}),
      position1 = colDef(minWidth = 150),
      position2 = colDef(minWidth = 150),
      strand1 = colDef(width = 100),
      strand2 = colDef(width = 100),
      arriba.site1 = colDef(minWidth = 120,filterable = TRUE),
      arriba.site2 = colDef(minWidth = 120,filterable = TRUE),
      starfus.splice_type = colDef(minWidth = 140),
      DB_count = colDef(maxWidth = 100),
      DB_list = colDef(minWidth = 100),
      arriba.split_reads = colDef(width = 110),
      arriba.discordant_mates = colDef(width = 160),
      arriba.break_coverage = colDef(width = 120),
      arriba.break2_coverage = colDef(width = 120),
      starfus.split_reads = colDef(width = 120),
      starfus.discordant_mates = colDef(width = 160),
      starfus.counter_fusion1 = colDef(width = 150),
      starfus.counter_fusion2 = colDef(width = 150),
      arriba.break_seq = colDef(minWidth = 120),
      starfus.break_seq = colDef(minWidth = 130)
    )

  } else if (tag == "germline"){
    custom_colDef <- list(
      var_name = colDef(minWidth = 150,filterable = TRUE,sticky = "left"),
      variant_freq = colDef(maxWidth = 100,filterable = TRUE),
      in_library = colDef(maxWidth = 100),
      Gene_symbol = colDef(maxWidth = 140,filterable = TRUE),
      coverage_depth = colDef(maxWidth = 100,filterable = TRUE),
      gene_region = colDef(width = 130,filterable = TRUE,filterInput = selectFilter("tbl-germline")),
      gnomAD_NFE = colDef(minWidth = 140,maxWidth = 150,filterable = TRUE),
                          # header = function(value) {
                          #   tagList(value, tags$a(
                          #     href = "https://gnomad.broadinstitute.org/",
                          #     target = "_blank",
                          #     icon("external-link-alt", lib = "font-awesome"),
                          #     style = "margin-left: 6px; color: #007bff; text-decoration: none;"))},
      clinvar_sig = colDef(minWidth = 140,filterable = TRUE,filterInput = selectFilter("tbl-germline")),
      snpDB = colDef(maxWidth = 120,filterable = TRUE
                     # header = function(value) {
                     #   tagList(value, tags$a(
                     #     href = "https://www.ncbi.nlm.nih.gov/clinvar/",
                     #     target = "_blank",
                     #     icon("external-link-alt", lib = "font-awesome"),
                     #     style = "margin-left: 6px; color: #007bff; text-decoration: none;"
                     #     ))}
                     ),
      CGC_Germline = colDef(width = 130),
      trusight_genes = colDef(width = 140),
      fOne = colDef(width = 100),
      Consequence = colDef(minWidth = 170,filterable = TRUE,filterInput = selectFilter("tbl-germline")),
      HGVSc = colDef(minWidth = 100),
      HGVSp = colDef(minWidth = 100),
      all_full_annot_name = colDef(minWidth = 160),
      occurance_in_cohort = colDef(width = 170),
      in_samples = colDef(minWidth = 120),
      alarm = colDef(minWidth = 120),
      var_gen_coord = colDef(minWidth = 170),
      variant_type = colDef(minWidth = 120),
      genotype = colDef(minWidth = 100,filterable = TRUE),
      Called_by = colDef(minWidth = 120),
      `1000g_EUR_AF` = colDef(width = 130),
      COSMIC = colDef(minWidth = 120),
      HGMD = colDef(minWidth = 120),
      NHLBI_ESP = colDef(minWidth = 120),
      clinvar_DBN = colDef(minWidth = 120,filterable = TRUE),
      BRONCO = colDef(width = 100),
      `md-anderson` = colDef(width = 130),
      CGC_Tumour_Germline = colDef(minWidth = 180,filterable = TRUE),
      PolyPhen = colDef(minWidth = 120),
      SIFT = colDef(minWidth = 120),
      CADD_RAW = colDef(width = 100),
      CADD_PHRED = colDef(width = 120),
      IMPACT = colDef(minWidth = 110,filterable = TRUE),
      SOMATIC = colDef(minWidth = 110),
      PHENO = colDef(minWidth = 110),
      GENE_PHENO = colDef(width = 150),
      PUBMED = colDef(minWidth = 150),
      EXON = colDef(width = 110),
      INTRON = colDef(width = 110),
      Feature = colDef(minWidth = 140,filterable = TRUE),
      Feature_type = colDef(minWidth = 130),
      `Annotation source` = colDef(minWidth = 170),
      Gene = colDef(minWidth = 150,filterable = TRUE)
    )
  } else if (tag == "expression") {
    
    custom_colDef <- list(
      # feature_name = colDef(minWidth = 150, filterable = TRUE),
      # geneid = colDef(minWidth = 150, filterable = TRUE),
      # all_kegg_paths_name = colDef(minWidth = 170, filterable = TRUE),
      # pathway = colDef(minWidth = 180, filterable = TRUE),
      # pathway = colDef(
      #   minWidth = 180,
      #   filterable = TRUE,
      #   html = TRUE,  # Povolení HTML v Reactable
      #   cell = function(value) {
      #     if (is.null(value) || value == "") return("N/A")
      #     
      #     pathway_colors <- list(
      #       "RTK Signaling" = "#98FB98",
      #       "Metabolic Signaling" = "#00CED1",
      #       "Epigenetics" = "#DA70D6",
      #       "PI3K/AKT/mTOR Signaling" = "#008000",
      #       "Apoptosis" = "#FF7F00",
      #       "MAPK Signaling" = "#800000",
      #       "WNT signaling" = "#6495ED",
      #       "Hormone Signaling" = "#DC143C",
      #       "DNA damage/repair" = "#87CEEB",
      #       "Cell cycle control" = "#FB9A99",
      #       "Immune Checkpoints" = "#A9A9A9",
      #       "TGF-B Signaling" = "#FFD700",
      #       "JAK/STAT Signaling" = "#BDB76B",
      #       "Hedgehog Signaling" = "#8B008B",
      #       "Non-receptor kinases" = "#6A5ACD",
      #       "Kinase Fusions" = "#D2691E",
      #       "Immune Response" = "#4682B4")
      #     
      #     pathways <- strsplit(value, ",")[[1]] # Rozdělení pathway hodnot do seznamu (některé mohou obsahovat více pathway)
      #     pathways <- trimws(pathways)  # Odstranění mezer kolem názvů
      #     
      #     # Vytvoření pill elementů
      #     pills <- lapply(pathways, function(pathway) {
      #       color <- pathway_colors[[pathway]]
      #       if (is.null(color)) color <- "#D3D3D3"  # Výchozí šedá pro neznámé pathways
      #       div(style = sprintf("display: inline-block; padding: 5px 10px; margin: 2px; border-radius: 15px; background-color: %s; color: white; font-size: 12px; font-weight: bold;", color),
      #           pathway)
      #     })
      #     as.character(tagList(pills)) # Použití `as.character()` k převedení HTML do stringu
      #   }
      # ),
      # num_of_paths = colDef(maxWidth = 100),
      # refseq_id = colDef(maxWidth = 140),
      # type = colDef(maxWidth = 100),
      # gene_definition = colDef(minWidth = 130),
      # all_kegg_gene_names = colDef(minWidth = 150)
    )
    dynamic_columns <- list()
    num_columns <- length(column_names)  # Celkový počet dynamických sloupců
    log2fc_indices <- which(grepl("^log2FC_", column_names))  # Indexy log2FC sloupců
    
    for (i in seq_along(column_names)) {
      col <- column_names[i]
      border_style <- NULL
      
      if (grepl("^log2FC_", col)) {
        # První log2FC dostane čáru na oddělení od statických sloupců
        if (i == log2fc_indices[1]) {
          border_style <- list(borderLeft = "2px solid black")
        }
      }
      
      if (grepl("^p_adj_", col)) {
        # P_adj dostane oddělovací čáru jen pokud to není poslední trojice
        if (i < num_columns) {
          border_style <- list(borderRight = "1px dashed rgba(0, 0, 0, 0.3)")
        }
      }
      
      # Vytvoření sloupců s dynamickými vlastnostmi
      # scientific formát, bohužel to trvá strašně dlouho. nejedná se totiž o přepočet ale pouze o zobrazení čísla 
      # v daném zápisu, který reactable zatím nepodporuje: cell = function(value) formatC(value, format = "e", digits = 2)
      if (grepl("^log2FC_", col)) {
        dynamic_columns[[col]] <- colDef(
          # maxWidth = 130,
          style = JS("function(rowInfo, colInfo) {
                        var value = rowInfo.values[colInfo.id];
                        var borderStyle = colInfo.id.includes('p_adj') ? '1px dashed rgba(0, 0, 0, 0.3)' : '';
                        var color = value > 1 ? '#FFE9E9' : (value < -1 ? '#E6F7FF' : '#FFFFFF');

                        return {
                          backgroundColor: color,
                          borderRight: borderStyle
                        };}"))
      } else if (grepl("^p_value_", col)) {
        dynamic_columns[[col]] <- colDef(
          # maxWidth = 130,
          style = JS("function(rowInfo, colInfo) {
                        var value = rowInfo.values[colInfo.id];
                        var borderStyle = colInfo.id.includes('p_adj') ? '1px dashed rgba(0, 0, 0, 0.3)' : '';
                        var color = (value <= 0.05) ? '#FFEFDE' : '#FFFFFF';

                        return {
                          backgroundColor: color,
                          borderRight: borderStyle
                        };}"))
      } else if (grepl("^p_adj_", col)) {
        dynamic_columns[[col]] <- colDef(
          # maxWidth = 130,
          style = JS("function(rowInfo, colInfo) {
                        var value = rowInfo.values[colInfo.id];
                        var borderStyle = '1px dashed rgba(0, 0, 0, 0.3)';  // p-adj má border vždy
                        var color = (value <= 0.05) ? '#E7FAEF' : '#FFFFFF';
                  
                        return {
                          backgroundColor: color,
                          borderRight: borderStyle
                        };}"))
      }
    }

    custom_colDef <- c(custom_colDef, dynamic_columns)
    
  } else {
    print("NOT fusion, expression or germline")
  }
  return(custom_colDef)
}

#' @export
custom_colGroup_setting <- function(tag){
  if (tag == "expression"){
    custom_colGroup <- lapply(get_tissue_list(), function(tissue) {
        group_name <- gsub("_", " ", tissue)
        colGroup(name = group_name, columns = c(
            paste0("log2FC_", tissue),
            paste0("p_value_", tissue),
            paste0("p_adj_", tissue)
        ))
    })
  }
  # message("custom_colGroup: ",custom_colGroup)
  return(custom_colGroup)
}

set_pathway_colors <- function(){
  pathway_colors <- list(
      "RTK Signaling" = "#98FB98",
      "Metabolic Signaling" = "#00CED1",
      "Epigenetics" = "#DA70D6",
      "PI3K/AKT/mTOR Signaling" = "#008000",
      "Apoptosis" = "#FF7F00",
      "MAPK Signaling" = "#800000",
      "WNT signaling" = "#6495ED",
      "Hormone Signaling" = "#DC143C",
      "DNA damage/repair" = "#87CEEB",
      "Cell cycle control" = "#FB9A99",
      "Immune Checkpoints" = "#A9A9A9",
      "TGF-B Signaling" = "#FFD700",
      "JAK/STAT Signaling" = "#BDB76B",
      "Hedgehog Signaling" = "#8B008B",
      "Non-receptor kinases" = "#6A5ACD",
      "Kinase Fusions" = "#D2691E",
      # "non-WNT/non-SHH medulloblastoma-related markers" = "khaki2",
      "Immune Response" = "#4682B4")

  return(pathway_colors)
}


# #' @export
# rating_stars <- function(rating, max_rating = 4) {
#   star_icon <- function(empty = FALSE) {
#     tagAppendAttributes(shiny::icon("star"),
#                         style = paste("color:", if (empty) "#edf0f2" else "orange"),
#                         "aria-hidden" = "true"
#     )
#   }
# }