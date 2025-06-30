box::use(
  htmltools[div,tags],
  reactable,
  reactable[colDef,JS,colGroup],
  stats[setNames], #na.omit,
  # data.table[uniqueN]
)

box::use(
  app/logic/prepare_table[colFilter]
)

#' @export
map_checkbox_names <- function(map_list){
  map_display_names <- sapply(map_list, function(x) {
    if (!is.null(x$name)) {
      x$name
    } else if (!is.null(x$header)) {
      x$header
    } else {
      NA_character_
    }
  })
  
  map_display_names <- map_display_names[!is.na(map_display_names)]
  choices <- setNames(names(map_display_names), map_display_names)
  return(choices)
}

#' @export
getColFilterValues <- function(flag,expr_flag = NULL) {
  colnames_list <- colFilter(flag,expr_flag)
  list(all_columns = colnames_list$all_columns, default_columns = colnames_list$default_columns)
}

#' @export
generate_columnsDef <- function(column_names, selected_columns, tag, map_list) {
  
  # Definuj permanentně skryté sloupce podle tagu
  hide <- switch(tag,
                 "fusion" = c("sample", "png_path", "svg_path"),
                 "germline" = c("sample"),
                 "expression" = c("sample"),
                 character(0))
  
  if (length(hide) == 0) {
    message("No column has been selected for permanent hiding")
  }
  
  column_defs <- lapply(column_names, function(col) {
    
    # 1️⃣ Permanentně skryté sloupce
    if (col %in% hide) {
      return(colDef(show = FALSE))
    }
    
    # 2️⃣ Pokud je sloupec vybrán uživatelem
    if (col %in% selected_columns) {
      
      # Získat definici z map_list
      map_def <- map_list[[col]]
      
      # Nastavit header z map_def$name nebo map_def$header, fallback na col
      header_name <- if (!is.null(map_def$name)) {
        map_def$name
      } else if (!is.null(map_def$header)) {
        map_def$header
      } else {
        col
      }
      
      # Pokud je definice v map_list, využij ji, doplň header pokud chybí
      if (!is.null(map_def)) {
        map_def$header <- header_name
        return(do.call(colDef, map_def))
      }
      
      # Fallback: není v map_list, ale je vybrán uživatelem
      return(colDef(show = TRUE, header = header_name))
    }
    
    # 3️⃣ Pokud není vybrán uživatelem, skryj
    colDef(show = FALSE)
  })
  
  names(column_defs) <- column_names
  return(column_defs)
}

#' @export
colnames_map_list <- function(tag, expr_flag = NULL, all_columns = NULL){
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
  } else if (tag == "somatic"){
    map_list <- list(
      var_name = colDef(sticky='left', minWidth=140,filterable = TRUE, name = 'Variant name'),
      in_library = colDef(sticky='left',header = "In library"),
      alarm = colDef(name="Alarm"),
      full_annot_name = colDef(name="Annotated name",minWidth = 240,),
      var_gen_coord = colDef(name="Variant coordinates",minWidth = 240,filterable=TRUE),
      variant_type = colDef(filterable = TRUE, name = 'Variant type',minWidth=110),
      Gene_symbol = colDef(sticky = 'left',filterable = TRUE, minWidth=110,name = 'Gene symbol'),
      HGVSp = colDef(minWidth=120,maxWidth=250,name="HGVSp"),
      HGVSc = colDef(minWidth=120,maxWidth=250,name="HGVSc"),
      tumor_variant_freq = colDef(name="Tumor variant frequency",minWidth=200, filterable = TRUE),
      tumor_depth = colDef(name = "Tumor depth",minWidth=110),
      normal_variant_freq = colDef(name="Normal variant frequency",minWidth=200),
      normal_depth = colDef(name = "Normal depth",minWidth=120),
      Called_by	= colDef(name="Called by"),
      `1000g_EUR_AF` = colDef(name="1000G EUR AF",minWidth=130),
      gnomAD_NFE = colDef(name = "GnomAD NFE",filterable=FALSE,minWidth=110),
      snpDB = colDef(name="SnpDB",filterable=TRUE),
      COSMIC = colDef(name="COSMIC",filterable=TRUE),
      NHLBI_ESP = colDef(name="NHLBI ESP",minWidth=110),
      clinvar_sig = colDef(name = "ClinVar significance", filterable = TRUE,minWidth=180),
      clinvar_DBN = colDef(show=TRUE,name="ClinVar DBN",filterable = TRUE,minWidth=110),
      `md-anderson` = colDef(name="MD Anderson",minWidth=110),
      trusight_genes = colDef(name="TruSight genes",minWidth=130),
      CGC_Somatic = colDef(name="CGC Somatic",minWidth=120,
                           cell = function(value) {
                             if (is.na(value)) {
                               return(NULL)  # Do not render anything for NA values
                             }
                             div(class = paste0("db-", tolower(value)),value)}),
      fOne = colDef(width = 100, name = "fOne",
                    cell = function(value) {
                      if (is.na(value)) {
                        return(NULL)  # Do not render anything for NA values
                      }
                      div(class = paste0("db-", tolower(value)),value)}),
      CGC_Tumour_Somatic = colDef(name="CGC Tumour",minWidth=110),
      PolyPhen = colDef(name="PolyPhen",minWidth=190),
      SIFT = colDef(name="SIFT",minWidth=180),
      gene_region = colDef(name="Gene region",filterable = TRUE, minWidth=110),
      HGMD = colDef(name="HGMD"),
      IMPACT = colDef(name="Impact",filterable=TRUE),
      Consequence = colDef(name = "Consequence",minWidth=140,filterable=TRUE),
      EXON = colDef(name = "Exon"),
      INTRON = colDef(name = "Intron"),
      Feature = colDef(name = "Feature",minWidth=180,filterable=TRUE),
      Feature_type = colDef(name = "Feature type",filterable=TRUE,minWidth=110),
      `Annotation source` = colDef(name = "Annotation source",minWidth=160),
      Gene = colDef(name = "Gene"),
      all_full_annot_name = colDef(name = "Full annotated name", minWidth = 240,show=TRUE)
    )
  } else if (tag == "germline"){
    map_list <- list(
      # original germline #
      var_name = colDef(sticky='left', minWidth=140,filterable = TRUE, name = 'Variant name'),
      in_library = colDef(sticky='left', maxWidth = 100, header = "In library"),
      variant_freq = colDef(filterable = TRUE, minWidth = 100, name = "Variant frequency"),
      Gene_symbol = colDef(sticky = 'left',filterable = TRUE, minWidth=110,name = 'Gene symbol'),
      coverage_depth = colDef(maxWidth = 100,filterable = TRUE, name = "Coverage depth"),
      gene_region = colDef(filterable = TRUE, minWidth=110, name="Gene region"),
      gnomAD_NFE = colDef(minWidth = 140,maxWidth = 150,filterable = TRUE,name = "GnomAD NFE"),
      clinvar_sig = colDef(minWidth = 180,filterable = TRUE, name = "ClinVar significance",
                           cell = function(value) {
                             if (is.na(value)) {
                               return(NULL)  # Do not render anything for NA values
                             }
                             # div(class = paste0("clinvar-tag clinvar-", tolower(value)),value)}
                             tags$div(
                               lapply(strsplit(value, "/")[[1]], function(v) {
                                 v_trimmed <- trimws(v)
                                 class_name <- paste0("clinvar-tag clinvar-", tolower(gsub(" ", "_", v_trimmed)))
                                 tags$span(class = class_name, v_trimmed)
                               })
                             )}
      ),
      Consequence = colDef(minWidth = 170,filterable = TRUE,name = "Consequence"),
      HGVSp = colDef(minWidth=120,maxWidth=250,name="HGVSp"),
      HGVSc = colDef(minWidth=120,maxWidth=250,name="HGVSc"),
      all_full_annot_name = colDef(name = "Full annotated name", minWidth = 160),
      snpDB = colDef(maxWidth = 120,filterable = TRUE,name="SnpDB",
                     # header = function(value) {
                     #   tagList(value, tags$a(
                     #     href = "https://www.ncbi.nlm.nih.gov/clinvar/",
                     #     target = "_blank",
                     #     icon("external-link-alt", lib = "font-awesome"),
                     #     style = "margin-left: 6px; color: #007bff; text-decoration: none;"
                     #     ))}
      ),
      CGC_Germline = colDef(width = 130,name="CGC Germline",
                            cell = function(value) {
                              if (is.na(value)) {
                                return(NULL)  # Do not render anything for NA values
                              }
                              div(class = paste0("db-", tolower(value)),value)}),
      trusight_genes = colDef(width = 140,name="TruSight genes",
                              cell = function(value) {
                                if (is.na(value)) {
                                  return(NULL)  # Do not render anything for NA values
                                }
                                div(class = paste0("db-", tolower(value)),value)}),
      fOne = colDef(width = 100, name = "fOne",
                    cell = function(value) {
                      if (is.na(value)) {
                        return(NULL)  # Do not render anything for NA values
                      }
                      div(class = paste0("db-", tolower(value)),value)}),
      occurance_in_cohort = colDef(width = 170,name = "Occurence in cohort"),
      in_samples = colDef(minWidth = 120,name = "In samples"),
      alarm = colDef(minWidth = 120,name="Alarm"),
      var_gen_coord = colDef(minWidth = 170,name="Variant coordinates",filterable=TRUE),
      variant_type = colDef(minWidth = 120,filterable = TRUE, name = "Variant type"),
      genotype = colDef(minWidth = 100,filterable = TRUE,name = "Genotype"),
      Called_by = colDef(minWidth = 120, name="Called by"),
      `1000g_EUR_AF` = colDef(width = 130, name="1000G EUR AF"),
      COSMIC = colDef(minWidth = 120, name="COSMIC",filterable=TRUE),
      HGMD = colDef(minWidth = 120, name="HGMD"),
      NHLBI_ESP = colDef(minWidth = 120, name="NHLBI ESP"),
      clinvar_DBN = colDef(name="ClinVar DBN",filterable = TRUE,minWidth=110),
      BRONCO = colDef(width = 100,name="BRONCO"),
      `md-anderson` = colDef(width = 130,name="MD Anderson"),
      CGC_Tumour_Germline = colDef(minWidth = 180,filterable = TRUE,name="CGC tumour germline"),
      PolyPhen = colDef(minWidth = 120,name="PolyPhen"),
      Feature = colDef(minWidth = 140,filterable = TRUE,name = "Feature"),
      Feature_type = colDef(minWidth = 130,name = "Feature type"),
      `Annotation source` = colDef(minWidth = 170,name = "Annotation source"),
      Gene = colDef(minWidth = 150,filterable = TRUE, name = "Gene"),
      SIFT = colDef(minWidth = 120,name="SIFT"),
      CADD_RAW = colDef(width = 100,name="CADD raw"),
      CADD_PHRED = colDef(width = 120,name="CADD phred"),
      IMPACT = colDef(minWidth = 110,filterable = TRUE,name="Impact"),
      SOMATIC = colDef(minWidth = 110,name="Somatic"),
      PHENO = colDef(minWidth = 110,name="Phenotype"),
      GENE_PHENO = colDef(width = 150,name="Gene phenotype"),
      PUBMED = colDef(minWidth = 150,name="PubMed"),
      EXON = colDef(width = 110,name="Exon"),
      INTRON = colDef(width = 110,name="Intron")
      )
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
        pathway = "Pathway",
        num_of_paths = "Pathway (n)",
        mean_log2FC = "Mean log2FC")
    } else {
      static_columns <- list(
        feature_name = "Gene name",
        geneid = "Gene ID",
        pathway = "Pathway",
        mean_log2FC = "Mean log2FC")
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

