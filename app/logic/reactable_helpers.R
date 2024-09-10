box::use(  
  shiny[tags,tagList, icon, actionButton, textInput,HTML],
  reactable,
  reactable[colDef,JS],
  htmltools[div,tags,tagAppendAttributes],
  stats[na.omit],
  bs4Dash[actionButton],
  reactablefmtr[pill_buttons,data_bars]
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

  if (tag == "fusion") {
    hide <- c("sample", "png_path", "svg_path")
  } else if (tag == "germline") {
    hide <- c("sample")
  } else {
    hide <- c()
    print("No column has been selected for permanent hiding")
  }

  custom_colDef <- custom_colDef_setting(tag,session)

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
custom_colDef_setting <- function(tag, session = NULL, data = NULL){
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

      tissue_columns <- grep("Tissue_", names(data), value = TRUE)
      scale_indices <- grep("^Scale_", names(data))
      fc_indices <- grep("^FC_", names(data))

      custom_colDef <- lapply(names(data), function(col) {
        if (col %in% tissue_columns) {
          colDef(show = FALSE)
        } else if (grepl("^FC_", col)) {
          colDef(
            name = "FC",
            maxWidth = 280,
            cell = data_bars(
              data,
              fill_color = c("#42c2ca","#d9f2f4","#ffbaba","#ff7b7b"),
              number_fmt = scales::number_format(accuracy = 0.01)
            )
            ,style = if (which(names(data) == col) == fc_indices[1]) list(borderRight = "1px dashed rgba(0, 0, 0, 0.3)") else NULL
          )
        } else if (grepl("^Scale_", col)) {
          colDef(
            name = "Scale",
            maxWidth = 120
            # align = "center"
          )
        } else if (col == "Gene") {
          colDef(
            maxWidth = 150
          )
        } else if (col == "Pathway") {
          colDef(
            minWidth = 200,
            align = "left",
            cell = pill_buttons(
              data = data,
              color_ref = "Color",
              box_shadow = TRUE
            ),
            style = list(borderRight = "1px dashed rgba(0, 0, 0, 0.3)")
          )
        } else if (col == "Color") {
          colDef(show = FALSE)
        } else if (col == "Sample") {
          colDef(show = FALSE)
        } else {
          colDef()
        }
      })

      names(custom_colDef) <- names(data)

  } else {
    print("NOT fusion NOR germline")
  }
  return(custom_colDef)
}

                    
    
      

#' @export
set_pathway_colors <- function(){
  pathway_colors <- c(
    "RTK Signaling" = "palegreen2",
    "Metabolic Signaling" = "darkturquoise",
    "Epigenetics" = "orchid1",
    "PI3K/AKT/mTOR Signaling" = "green4",
    "Apoptosis" = "#FF7F00",
    "MAPK Signaling" = "maroon",
    "WNT signaling" = "#6495ED",
    "Hormone Signaling" = "#DC143C",
    "DNA damage/repair" = "skyblue2",
    "Cell cycle control" = "#FB9A99",
    "Immune Checkpoints" = "#A9A9A9",
    "TGF-B Signaling" = "gold1",
    "JAK/STAT Signaling" = "#BDB76B",
    "Hedgehog Signaling" = "#8B008B",
    # "non-WNT/non-SHH medulloblastoma-related markers" = "khaki2",
    "Non-receptor kinases" = "#6A5ACD", 
    "Kinase Fusions" = "#D2691E"
  )
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