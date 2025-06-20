box::use(
  shiny[tagList,fileInput,conditionalPanel,reactive,reactiveValues,reactiveVal,downloadButton,icon,moduleServer,NS,downloadHandler,div,observe,observeEvent],
  flextable[theme_vanilla,bg,fontsize,border_remove,set_header_labels,set_table_properties],
  officer[cursor_reach,body_add_par,body_remove,fp_border,fp_text,read_docx],
  htmltools[tags,HTML],
  shinyWidgets[dropdown,prettyRadioButtons],
  data.table[data.table],
  bs4Dash[box]
)

myReport_theme <- function(ft) {
  ft |>
    theme_vanilla() |>
    color(part = "header", color = "white") |>
    bg(part = "header", bg = "#294779") |> # "#22a9c0"
    bg(i = seq_len(nrow_part(ft, part = "body")), bg = "#9fc5e8", part = "body") |> # "#bce5ec"
    bg(i = seq(1, nrow_part(ft, part = "body"), by = 2), bg ="#cfe2f3" , part = "body") |> # "#e8f6f8"
    fontsize(size = 8, part = "all") |>
    align(align = "left", part = "header") |>
    align(align = "left", part = "body") |>
    border_remove()
}


ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(style = "font-size: 16px !important; font-weight: normal !important;",
      dropdown(right = TRUE, size = "sm", icon = icon("download"), style = "material-flat", width = "300px",
         prettyRadioButtons(
              inputId = ns("template_choice"),
              label = "Choose template:",
              choices = c("Use default template" = "default", "Upload custom template" = "custom"),
              selected = "default"
            ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'custom'", ns("template_choice")),
            fileInput(ns("custom_template"), "Upload your template (.docx)", accept = ".docx")
         ),
        downloadButton(ns("download_report"), "Generate Report")
      )
    )
  )
}


server <- function(id,shared_data) {
  moduleServer(id, function(input, output, session) {

    somatic_dt <- reactive({
      req(shared_data$somatic_var())
      message(" ###########################  shared_data$somatic_var()",shared_data$somatic_var())
      var_dt <- as.data.table(shared_data$somatic_var())
      
      if (is.na(shared_data$somatic_var())){
        message(" ###########################  shared_data$somatic_var() IS NA")
        dt <- data.table(
          # sample = character(),
          Gene = character(),
          Transcript = character(),
          variant = character(),
          VAF = character(),
          Consequence = character(),
          Class = character()
        )
      } else {
        message(" ###########################  shared_data$somatic_var() IS NOT NA")
        dt <- data.table(
          # sample = character(),
          Gene = var_dt$Gene_symbol,
          Transcript = var_dt$Feature,
          variant = paste0(var_dt$HGVSc, "\n(", var_dt$HGVSp, ")"),
          VAF = paste0(round(var_dt$tumor_variant_freq,2) * 100," %"),
          Consequence = var_dt$Consequence,
          Class = ""
        )
        message(" ###########################  dt : ",dt)
      }
      dt
    })
    
    preprare_somatic_dt <- function(somatic_dt) {
      ft <- flextable(somatic_dt, col_keys = c("Gene","Feature","variant","VAF","Consequence","Class"))
      ft <- set_header_labels(ft,
                              Gene = "Gene",
                              Feature = "Transcript",
                              variant = "Variant",
                              VAF = "VAF",
                              Consequence = "Variant effect",
                              Class = "Class")
      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      ft <- width(ft, j = ~ VAF, width = 0.6)
      ft <- width(ft, j =  ~ variant, width = 0.8)
      ft <- width(ft, j =  ~ Class, width = 1.5)
      ft <- bold(ft, j = ~ Gene_symbol, bold = TRUE)
      ft
    }
    
    
    germline_dt <- reactive({
      dt <- data.table(
        var_name = c("19_45352801_C/G","19_45352801_C/G"),
        Gene_symbol = c("ERCC2","ERCC2"),
        gnomAD_NFE = c(0.000000123,0.00026245),
        variant_freq = c(0.286,0.286),
        coverage_depth = c(28, 28),
        Consequence = c("missense_variant","missense_variant"),
        HGVSc = c("c.1847G>C","c.1847G>C"),
        HGVSp = c("p.R616P","p.R616P"),
        variant_type = c("SNV","SNV"),
        Feature = c("NM_000400.4","NM_000400.4"),
        clinvar_sig = c("Pathogenic","Likely pathogenic")
      )
      dt[,reads := paste0(round(variant_freq * coverage_depth),"/",coverage_depth)]
      dt[,var_name := sub("^([0-9XY]+_[0-9]+)_.*", "\\1", var_name)]
      dt[,var_name := gsub("_", ":", var_name)]
      dt[,variant := ifelse(is.na(HGVSp), HGVSc, paste0(HGVSc, "\n(", HGVSp, ")"))]
      dt[,MAF := sprintf("%.5f%%", gnomAD_NFE * 100)]
      # dt[,phenotype := c("Xeroderma pigmentosum, group D (AR), Trichothiodystrophy type 1, photosensitive (AR)",
      #                   "Cerebrooculofacioskeletal syndrome type 2 (AR)")]
      dt[,phenotype := c("","")]
      dt
    })
  
    preprare_germline_dt <- function(germline_dt) {
      ft <- flextable(germline_dt, col_keys = c("Gene_symbol","Feature","var_name","variant","variant_type","Consequence","reads","MAF","clinvar_sig","phenotype"))
      ft <- set_header_labels(ft,
                              Gene_symbol = "Gene",
                              Feature = "Transcript",
                              var_name = "Position",
                              variant = "Variant",
                              variant_type = "Type",
                              MAF = "MAF",
                              Consequence = "Consequence",
                              reads = "Reads",
                              clinvar_sig = "Class",
                              phenotype = "Associated phenotype")
      
      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      for (col in c("variant_type", "reads", "MAF")) {
        ft <- width(ft, j = col, width = 0.6)
      }
      ft <- width(ft, j =  ~ clinvar_sig, width = 0.8)
      ft <- width(ft, j =  ~ phenotype, width = 1.5)
      ft <- bold(ft, j = ~ Gene_symbol, bold = TRUE)
      ft
    }

    
    fusion_dt <- reactive({
      dt <- data.table(
        gene1 = "KMT2A",
        transcript5 = "NM_",
        gene2 = "MLLT3",
        transcript3 = "NM_",
        overall_support = 35,
        phasing = "in-frame",
        reads = "x/y"
      )
      dt
    })
  
    preprare_fusion_dt <- function(fusion_dt) {
      ft <- flextable(fusion_dt, col_keys = c("gene1","transcript5","gene2","transcript3","overall_support","phasing","reads"))
      ft <- set_header_labels(ft,
                              gene1 = "Gene 5'",
                              transcript5 = "Transcript 5'",
                              gene2 ="Gene 3'",
                              transcript3 = "Transcript 3'",
                              overall_support = "Overall support",
                              phasing = "Phasing",
                              reads = "Reads")
      
      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      # ft <- width(ft, j = c("gene1", "gene2"), width = 1)
      # ft <- width(ft, j = c("transcript5", "transcript3"), width = 1)
      ft <- width(ft, j = c("overall_support", "phasing", "reads"), width = 0.9)
      ft <- bold(ft, j = ~ gene1 + gene2, bold = TRUE)
      ft
    }
# 
#     mut_sign_dt <- sprintf("Tumor mutation burden (load): %s mutations/Mb", )
#     
#     preprare_mut_sign_dt <- function(mut_sign_dt) {
#       ft <- width(ft, j = ~ mut_sign_dt, width = 3)
#       ft <- bold(ft, j = ~ mut_sign_dt, bold = TRUE)
#       ft
#     }
    
    expression_dt <- reactive({
      dt <- data.table(
        gene = c("ALK", "DDR2", "EPHA3", "EPHA5", "FGF14", "KIT","CD79B", "LAG3", "PDCD1 (PD1)","DNMT3A", "EZH2", "MSH2", "MSH6", "PBRM1", "TERT", "TET2", "Valproic acid","BID"),
        pathway = c(rep("Receptor Tyrosine Kinase/Growth Factor Signaling", 6),rep("Immune Checkpoints", 3),rep("Chromatin Remodeling/DNA Methylation", 8),"Apoptosis"),
        expression_level = c("++++", "+", "+++", "++++++", "++++", "+++++","+", "++", "++++++","+++", "++++", "++++", "+++", "++", "++++", "++", "","+"),
        FC = c(4.5, 2.0, 3.5, 8.44, 4.5, 5.0, 2.0, 2.5, 7.87, 3.5, 4.5, 4.5, 3.5, 2.5, 4.5, 2.5, 1.0, 2.0),
        therapeutic_option = c("Crizotinib, Ceritinib, Alectinib, Lorlatinib","Regorafenib","","Ponatinib, Vandetanib","","Ponatinib, Sunitinib, Regorafenib, Imatinib, Ripretinib","","","","","","","","","","","Valproic acid","")
      )
      dt
    })
    
    preprare_expression_dt <- function(expression_dt) {
      ft <- flextable(expression_dt)
      ft <- myReport_theme(ft)
      ft <- set_table_properties(ft, width = 1, layout = "autofit")
      ft <- width(ft, j = "therapeutic_option", width = 3)
      ft <- bold(ft, j = ~ gene, bold = TRUE)
      ft
    }
    
    
    details_dt <- reactive({
      dt <- data.table(
        attribute = c("Specimen type", "Date of collection", "Number of biopsy",
                      "Cancer cell content", "Method used", "Library preparation",
                      "Sequencing device", "Date of sequencing"),
        germline = c("Peripheral blood", "7.9.2023", NA, NA, "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "11.9.2023"),
        somatic = c("FFPE tissue", "5.9.2023", "1391/23/1", "NA", "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "9.10.2023"),
        fusion = c("Peripheral blood", "7.9.2023", NA, NA, "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "11.9.2023"),
        expression = c("Frozen tissue", "5.9.2023", "1391/23", "NA", "Whole-transcriptome sequencing", "NEBNext Ultra II Directional RNA Library Prep Kit", "NextSeq 500", "31.10.2023")
      )
      
      active_cols <- c()
  
      if (!is.null(germline_dt()) && nrow(germline_dt()) > 0) active_cols <- c(active_cols, "germline")
      if (!is.null(somatic_dt()) && nrow(somatic_dt()) > 0) active_cols <- c(active_cols, "somatic")
      if (!is.null(fusion_dt()) && nrow(fusion_dt()) > 0) active_cols <- c(active_cols, "fusion")
      if (!is.null(expression_dt()) && nrow(expression_dt()) > 0) active_cols <- c(active_cols, "expression")
  
      details_dt <- dt[, c("attribute", active_cols), with = FALSE]
      details_dt
    })
    
    preprare_details_dt <- function(details_dt) {
      # Definujeme, které sloupce budou aktivní
      active_cols <- c("attribute", "space1",
                       if ("germline" %in% names(details_dt)) c("germline", "space2") else NULL,
                       if ("somatic" %in% names(details_dt)) c("somatic", "space3") else NULL,
                       if ("fusion" %in% names(details_dt)) c("fusion", "space4") else NULL,
                       if ("expression" %in% names(details_dt)) "expression" else NULL)
      space_cols <- grep("space[0-9]+", active_cols, value = TRUE)
      
      # Definujeme okraje
      top_bottom_border <- fp_border(color = "#22a9c0", width = 1)
      
      # Nejprve vytvoříme flextable objekt
      ft <- flextable(details_dt, col_keys = active_cols)
      
      # Nyní můžeme bezpečně získat poslední řádek
      last_row <- nrow_part(ft, part = "body")
      
      # Aplikujeme formátování
      ft <- ft |>
        theme_vanilla() |>
        fontsize(size = 8, part = "all") |>
        align(align = "center", part = "header") |>
        empty_blanks() |>
        bg(part = "header", bg = "white") |>
        bg(i = seq_len(last_row), bg = "white", part = "body") |>
        bg(i = seq(1, last_row, by = 2), bg = "#bce5ec", part = "body") |>
        border(part = "header",
               border.top = top_bottom_border,
               border.bottom = top_bottom_border) |>
        border(part = "body",
               i = last_row,
               j = active_cols,
               border.bottom = top_bottom_border) |>
        bold(part = "header", bold = TRUE) |>
        width(j = "attribute", width = 1.5) |>
        width(j = "germline", width = 1.5) |>
        width(j = "fusion", width = 1.5) |>
        width(j = "expression", width = 1.5) |>
        set_table_properties(width = 1, layout = "autofit") |>
        set_header_labels(attribute = "") |>
        bold(j = "attribute", bold = TRUE) |>
        italic(j = "attribute", italic = TRUE) |>
        bg(j = space_cols, bg = "white", part = "body")
      
      ft
    }
  
    # data <- reactive({
    #   data.frame(
    #     Kategorie = sample(c("A", "B", "C"), 100, replace = TRUE)
    #   )
    # })
    # 
    # summary_text <- reactive({
    #   sprintf("Dataset obsahuje %d záznamů rozdělených do %d kategorií.",
    #           nrow(data()), length(unique(data()$Kategorie)))
    # })
    # 
    # plot_file <- reactive({
    #   file <- tempfile(fileext = ".png")
    #   p <- ggplot(data(), aes(Kategorie)) +
    #     geom_bar(fill = "steelblue") +
    #     theme_minimal()
    #   ggsave(filename = file, plot = p, width = 6, height = 4)
    #   return(file)
    # })
    # 
    # 
    # output$plot <- renderPlot({
    #   ggplot(data(), aes(Kategorie)) +
    #     geom_bar(fill = "tomato") +
    #     theme_minimal()
    # })
    
    # Cesta k výchozí šabloně
    default_template_path <- paste0(getwd(),"/input_files/report_template.docx")
    
    # Reaktivní výraz pro získání cesty k šabloně
    template_path <- reactive({
      if (input$template_choice == "default") {
        return(default_template_path)
      } else {
        # Zkontrolujeme, zda byl nahrán vlastní soubor
        req(input$custom_template)
        return(input$custom_template$datapath)
      }
    })
    
    summary_germline <- reactive({
      sprintf("%s (%s)", germline_dt()$Gene_symbol, paste0(germline_dt()$HGVSc,"/",germline_dt()$HGVSp))
    })
  
    summary_somatic <- reactive({
      req(somatic_dt())
      message(" ######################### somatic_dt is active: ",somatic_dt())
      sprintf("%s (%s)", somatic_dt()$Gene, somatic_dt()$variant)
    })
  
    summary_fusion <- reactive({
      sprintf("%s::%s gene fusion", fusion_dt()$gene1, fusion_dt()$gene2)
    })
    
    germline_interpretation <- reactive({
      variants <- sprintf("%s variant was found in the %s gene.", paste0(germline_dt()$HGVSc,"/",germline_dt()$HGVSp), germline_dt()$Gene_symbol)
      links <- paste0("(https://www.oncokb.org/gene/", germline_dt()$Gene_symbol, ")")
      full_text <- paste(variants, links)
      return(full_text)
    })
    
    somatic_interpretation <- reactive({
      req(somatic_dt())
      variants <- sprintf("%s variant was found in the %s gene.", somatic_dt()$variant, somatic_dt()$Gene)
      links <- paste0("(https://www.oncokb.org/gene/", somatic_dt()$Gene, ")")
      full_text <- paste(variants, links)
      return(full_text)
    })
    
  observe({
    
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("report_", Sys.Date(), ".docx")
      },
      content = function(file) {
        print("Download handler triggered")
        print(template_path())
        
        doc <- read_docx(path = template_path())   # Load template
        note_style <- fp_text(font.size = 7, font.family = "Helvetica") # font for comments
  
        tryCatch({
          doc <- cursor_reach(doc, "<<patient_info>>")
          # Normaly here would be body_remove(doc), but also it would add empty row between titul in template 
          # and text/table from placeholder which I dont want to. Solution is to add text before and after placeholder
          # and remove placeholder at last.
          #
          # Move the cursor to the placeholder <<patient_info>> in the document.
          # After all content (headings and text) has been inserted before this placeholder,
          # the placeholder itself is now removed to finalize the layout.
          doc <- body_add_par(doc, paste0("Patient ID: ", "MR1507"), pos = "before")
          doc <- body_add_par(doc, paste0("Diagnosis: ", "Diffuse midline glioma, H3 K27-altered"), pos = "after")
          doc <- body_add_par(doc, paste0("Report date: ", format(Sys.Date(), "%B %d, %Y")), pos = "after")
  
          doc <- cursor_reach(doc, "<<patient_info>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<patient_info>> was not found.")
        })
        
        
        tryCatch({
          doc <- cursor_reach(doc, "<<summary_germline>>")
          
          if (!is.null(germline_dt()) || nrow(germline_dt()) > 0) {
            for (variant_text in summary_germline()) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<summary_germline>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<summary_germline>> was not found. No pathogenic varints will not be added.")
        })
  
        tryCatch({
          doc <- cursor_reach(doc, "<<summary_somatic>>")
  
          if (!is.null(somatic_dt()) || nrow(somatic_dt()) > 0) {
            for (variant_text in summary_somatic()) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<summary_somatic>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<summary_somatic>> was not found. No pathogenic varints will not be added.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<summary_fusion>>")
          
          if (!is.null(fusion_dt()) || nrow(fusion_dt()) > 0) {
            for (variant_text in summary_fusion()) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<summary_fusion>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<summary_fusion>> was not found. No gene fusion will not be added.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<details_table>>")
          # doc <- body_remove(doc)
          
          if (ncol(details_dt()) == 1) {    # if (is.null(details_dt()) || nrow(details_dt()) == 0) {
            doc <- body_add_par(doc, "No analysis metadata available.")
          } else {
            doc <- body_add_flextable(doc, preprare_details_dt(details_dt()), pos = "before")
          }
          doc <- cursor_reach(doc, "<<details_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<details_table>> was not found. Details table will not be added.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<germline_table>>")
          
          if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
            doc <- body_add_par(doc, "No variants with known or potential clinical significance in genes associated with hereditary cancer-predisposing syndromes were found.")
          } else {
            doc <- body_add_flextable(doc, preprare_germline_dt(germline_dt()), pos = "before")
            # Vysvětlivka pod tabulkou
            doc <- body_add_fpar(doc, fpar(ftext("MAF – minor allele frequency – Non-Finnish European population (gnomAD database)", prop = note_style)), pos = "after")
            doc <- body_add_fpar(doc, fpar(ftext("AD – autosomal dominant inheritance", prop = note_style)), pos = "after")
            doc <- body_add_fpar(doc, fpar(ftext("AR – autosomal recessive inheritance", prop = note_style)), pos = "after")
            doc <- body_add_fpar(doc, fpar(ftext("XLR – X-linked recessive", prop = note_style)), pos = "after")
          }
          doc <- cursor_reach(doc, "<<germline_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {   # No placeholder in template
            message("Placeholder <<germline_table>> was not found. Germline table will not be added.")
        })
  
        tryCatch({
          doc <- cursor_reach(doc, "<<somatic_table>>")
  
          if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
            doc <- body_add_par(doc, "No variants with known or potential clinical significance were found.")
          } else {
            doc <- body_add_flextable(doc, preprare_somatic_dt(somatic_dt()), pos = "before")
          }
          doc <- cursor_reach(doc, "<<somatic_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {   # No placeholder in template
             message("Placeholder <<somatic_table>> was not found. Somatic table will not be added.")
        })
        
        tryCatch({
          doc <- cursor_reach(doc, "<<fusion_table>>")
          
          if (is.null(fusion_dt()) || nrow(fusion_dt()) == 0) {
            doc <- body_add_par(doc, "No clinically relevant fusion genes were found.")
          } else {
            doc <- body_add_flextable(doc, preprare_fusion_dt(fusion_dt()), pos = "before")
          }
          doc <- cursor_reach(doc, "<<fusion_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<fusion_table>> was not found. Fusion table will not be added.")
        })
        # 
        # 
        # tryCatch({
        #   doc <- cursor_reach(doc, "<<mutational_sign_table>>")
        # 
        #   if (is.null(mut_sign_dt()) || nrow(mut_sign_dt()) == 0) {
        #     doc <- body_add_par(doc, "No data about mutational signatures were found.")
        #   } else {
        #     doc <- body_add_flextable(doc, preprare_mut_sign_dt(mut_sign_dt()), pos = "before")
        #   }
        #   doc <- cursor_reach(doc, "<<mutational_sign_table>>")
        #   doc <- body_remove(doc)
        # }, error = function(e) {
        #   message("Placeholder <<mutational_sign_table>> was not found. Mutational signatures table will not be added.")
        # })
  
        tryCatch({
          doc <- cursor_reach(doc, "<<expression_table>>")
          
          dt <- expression_dt()
          if (is.null(dt) || nrow(dt) == 0) {
            doc <- body_add_par(doc, "No data from expression profile analysis were found.")
          } else {
            pathways <- rev(unique(dt$pathway)) # Musím převrátit, jinak bude seznam pathways v opačném pořadí
            for (p in pathways) {
              subset_dt <- dt[pathway == p, .(gene, expression_level, FC, therapeutic_option)]
              doc <- body_add_flextable(doc, preprare_expression_dt(subset_dt), pos = "before")
              doc <- body_add_par(doc, p, style = "heading 5", pos = "before") # Přidej nadpis (podsekce) s názvem pathway
            }
          } 
          doc <- cursor_reach(doc, "<<expression_table>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<expression_table>> was not found. Expression profile table will not be added.")
        })
  
        tryCatch({
          doc <- cursor_reach(doc, "<<germline_interpretation>>")
          
          if (!is.null(germline_dt()) && nrow(germline_dt()) > 0) {
            for (variant_text in germline_interpretation()) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<germline_interpretation>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<germline_interpretation>> was not found. No pathogenic variants will be added.")
        })
       
        
        
        tryCatch({
          doc <- cursor_reach(doc, "<<somatic_interpretation>>")
          
          if (!is.null(somatic_dt()) && nrow(somatic_dt()) > 0) {
            for (variant_text in somatic_interpretation()) {
              doc <- body_add_par(doc, variant_text, pos = "before")
            }
          }
          doc <- cursor_reach(doc, "<<somatic_interpretation>>")
          doc <- body_remove(doc)
        }, error = function(e) {
          message("Placeholder <<somatic_interpretation>> was not found. No pathogenic varints will not be added.")
        })
        
        print(doc, target = file)
      }
    )
  })
  })
}

# shinyApp(ui, server, options = list(launch.browser = TRUE))
