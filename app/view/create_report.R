library(shiny)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(officer)
library(flextable)
library(reactable)
library(data.table)


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


ui <- fluidPage(
  titlePanel("Report Generator"),
  sidebarLayout(
    sidebarPanel(
      # Přepínač mezi výchozí a vlastní šablonou
      radioButtons("template_choice", "Choose template:",
                   choices = c("Use default template" = "default",
                               "Upload custom template" = "custom"),
                   selected = "default"),
      conditionalPanel(
        condition = "input.template_choice == 'custom'",
        fileInput("custom_template", "Upload your template (.docx)",
                  accept = c(".docx"))
      ),

      downloadButton("download_report", "Generate Report")
    ),
    mainPanel(
      #uiOutput("report_preview")
      
      # h3("Germline mutations"), uiOutput("germ_content"),
      # br(),
      # h3("Somatic mutations"), uiOutput("som_content"),
      # br(),
      # h3("Gene fusions"), uiOutput("fusion_content")
    )
  )
)

server <- function(input, output) {

  shared_data <- reactiveValues(germline_data = reactiveVal(NULL))
  
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
  
  somatic_dt <- reactive({
    dt <- data.table(
      sample = character(),
      var_name = character(),
      Gene_symbol = character(),
      variant_freq = character(),
      coverage_depth = character(),
      Consequence = character(),
      HGVSc = character(),
      HGVSp = character(),
      variant_type = character(),
      Feature = character(),
      clinvar_sig = character(),
      reads = character(),
      variant_classification = character(),
      therapeutic_option = character()
    )
    # dt[,reads := paste0(round(variant_freq * coverage_depth),"/",coverage_depth)]
    # dt[,var_name := sub("^([0-9XY]+_[0-9]+)_.*", "\\1", var_name)]
    # dt[,var_name := gsub("_", ":", var_name)]
    # dt[,variant := ifelse(is.na(HGVSp), HGVSc, paste0(HGVSc, "\n(", HGVSp, ")"))]
    # dt[,VAF :=  paste0(round(variant_freq,2) * 100," %")]
    dt
  })

  preprare_somatic_dt <- function(somatic_dt) {
    ft <- flextable(somatic_dt, col_keys = c("Gene_symbol","Feature","var_name","variant","variant_type","Consequence","reads","VAF","clinvar_sig","therapeutic_option"))
    ft <- set_header_labels(ft,
                            Gene_symbol = "Gene",
                            Feature = "Transcript",
                            var_name = "Position",
                            variant = "Variant",
                            variant_type = "Type",
                            VAF = "VAF",
                            Consequence = "Consequence",
                            reads = "Reads",
                            therapeutic_option = "Therapeutic option",
                            clinvar_sig = "Class")
    ft <- myReport_theme(ft)
    ft <- set_table_properties(ft, width = 1, layout = "autofit")
    for (col in c("variant_type", "reads", "VAF")) {
      ft <- width(ft, j = col, width = 0.6)
    }
    ft <- width(ft, j =  ~ clinvar_sig, width = 0.8)
    ft <- width(ft, j =  ~ therapeutic_option, width = 1.5)
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
  
  mut_sign_dt <- reactive({
    dt <- data.table(
      signature = c("Tumour mutation burden (TMB)","Mutation load normal","Microsatelite (in)stability (MSI)","Homologous recombinantion deficiency (HRD)"),
      score = c(2.4, 1.06, 0.1, 0.95),
      treshold = c("10 [0-100+]","","4 [0-100+]","0.5 [0-1]"),
      result = c("Low","Low","MSS (stable)","Deficient")
    )
    dt
  })
  
  preprare_mut_sign_dt <- function(mut_sign_dt) {
    ft <- flextable(mut_sign_dt)
    ft <- myReport_theme(ft)
    ft <- set_table_properties(ft, width = 1, layout = "autofit")
    ft <- width(ft, j = "signature", width = 3)
    ft <- bold(ft, j = ~ signature, bold = TRUE)
    ft
  }
  
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
  
  
  summary_dt <- reactive({
    dt <- data.table(
      attribute = c("Specimen type", "Date of collection", "Number of biopsy",
                    "Cancer cell content", "Method used", "Library preparation",
                    "Sequencing device", "Date of sequencing"),
      germline = c("Peripheral blood", "7.9.2023", NA, NA, "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "11.9.2023"),
      somatic = c("FFPE tissue", "5.9.2023", "1391/23/1", "NA", "Whole-exome sequencing", "KAPA HyperExome", "NextSeq 500", "9.10.2023"),
      fusion = c(NA, NA, NA, NA, NA, NA, NA, NA),
      expression = c("Frozen tissue", "5.9.2023", "1391/23", "NA", "Whole-transcriptome sequencing", "NEBNext Ultra II Directional RNA Library Prep Kit", "NextSeq 500", "31.10.2023")
    )
    
    active_cols <- c()

    if (!is.null(germline_dt()) && nrow(germline_dt()) > 0) active_cols <- c(active_cols, "germline")
    if (!is.null(somatic_dt()) && nrow(somatic_dt()) > 0) active_cols <- c(active_cols, "somatic")
    if (!is.null(fusion_dt()) && nrow(fusion_dt()) > 0) active_cols <- c(active_cols, "fusion")
    if (!is.null(expression_dt()) && nrow(expression_dt()) > 0) active_cols <- c(active_cols, "expression")

    summary_dt <- dt[, c("attribute", active_cols), with = FALSE]
    summary_dt
  })
  
  preprare_summary_dt <- function(summary_dt) {
    # ft <- flextable(summary_dt, col_keys = c("attribute","col1","germline","col2","somatic","col3","fusion","col4","expression"))
    # ft <- myReport_theme(ft)
    # ft <- set_table_properties(ft, width = 1, layout = "autofit")
    # ft <- bold(ft, j = ~ attribute, bold = TRUE)
    # 
    active_cols <- c("attribute", "space1",
                        if ("germline" %in% names(summary_dt)) c("germline", "space2") else NULL,
                        if ("somatic" %in% names(summary_dt)) c("somatic", "space3") else NULL,
                        if ("fusion" %in% names(summary_dt)) c("fusion", "space4") else NULL,
                        if ("expression" %in% names(summary_dt)) "expression" else NULL)
    space_cols <- grep("space[0-9]+", active_cols, value = TRUE)
    
    top_bottom_border <- fp_border(color = "#22a9c0", width = 1)
    last_row <- nrow_part(ft, part = "body")  # ✅ správný index řádku
    
    ft <- flextable(summary_dt, col_keys = active_cols) |>
      theme_vanilla() |>
      fontsize(size = 8, part = "all") |>
      align(align = "center", part = "header") |>
      empty_blanks() |>
      bg(part = "header", bg = "white") |>
      bg(i = seq_len(nrow_part(ft, part = "body")), bg = "white", part = "body") |>
      bg(i = seq(1, nrow_part(ft, part = "body"), by = 2), bg = "#bce5ec", part = "body") |>
      border(part = "header",
             border.top = top_bottom_border,
             border.bottom = top_bottom_border) |>
      border(part = "body",
             i = last_row,
             j = active_cols,
             border.bottom = top_bottom_border) |>
      bold(part = "header", bold = TRUE) |>
      width(j = space_cols, width = 0.4) |>
      width(j = ~ attribute, width = 1.5) |>
      set_table_properties(width = 1, layout = "autofit") |>
      set_header_labels(attribute = "") |>
      bold(j = ~ attribute, bold = TRUE) |>
      italic(j = ~ attribute, italic = TRUE) |>
      bg(j = space_cols, bg = "white", part = "body")
    ft
  }
  



  # })
  # preprare_summary_dt <- function(summary_dt) {
  #   ft <- flextable(summary_dt)
  #   # ft <- set_table_properties(ft, width = 1, layout = "autofit")
  #   # ft <- myReport_theme(ft)
  #   # print("i am preparing summary dt")
  #   # print(ft)
  #   ft
  # }
  
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
  default_template_path <- "../../report_template.docx"
  
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
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("report_", Sys.Date(), ".docx")
    },
    content = function(file) {
      doc <- read_docx(path = template_path())   # Load template
      
      note_style <- fp_text(font.size = 7, font.family = "Helvetica")
      
      # tryCatch({
      #   doc <- cursor_reach(doc, "<<summary_text>>")
      #   doc <- body_remove(doc)
      #   doc <- body_add_par(doc, summary_text())
      # }, error = function(e) {
      #   # V případě, že značka není nalezena, přidej text na konec
      #   doc <- body_add_par(doc, "Summary:", style = "heading 2")
      #   doc <- body_add_par(doc, summary_text())
      # })

      tryCatch({
        doc <- cursor_reach(doc, "<<germline_table>>")
        doc <- body_remove(doc)
        if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
          doc <- body_add_par(doc, "No variants with known or potential clinical significance in genes associated with hereditary cancer-predisposing syndromes were found.")
        } else {
          doc <- body_add_flextable(doc, preprare_germline_dt(germline_dt()))
          # Vysvětlivka pod tabulkou
          doc <- body_add_fpar(doc, fpar(ftext("MAF – minor allele frequency – Non-Finnish European population (gnomAD database)", prop = note_style)))
          doc <- body_add_fpar(doc, fpar(ftext("AD – autosomal dominant inheritance", prop = note_style)))
          doc <- body_add_fpar(doc, fpar(ftext("AR – autosomal recessive inheritance", prop = note_style)))
          doc <- body_add_fpar(doc, fpar(ftext("XLR – X-linked recessive", prop = note_style)))
        }
      }, error = function(e) {   # No placeholder in template
          message("Placeholder <<germline_table>> was not found. Germline table will not be added.")
      })

      tryCatch({
        doc <- cursor_reach(doc, "<<somatic_table>>")
        doc <- body_remove(doc)
        if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
          doc <- body_add_par(doc, "No variants with known or potential clinical significance were found.")
        } else {
          doc <- body_add_flextable(doc, preprare_somatic_dt(somatic_dt()))
        }
      }, error = function(e) {   # No placeholder in template
           message("Placeholder <<somatic_table>> was not found. Somatic table will not be added.")
      })
      
      tryCatch({
        doc <- cursor_reach(doc, "<<fusion_table>>")
        doc <- body_remove(doc)
        if (is.null(fusion_dt()) || nrow(fusion_dt()) == 0) {
          doc <- body_add_par(doc, "No clinically relevant fusion genes were found.")
        } else {
          doc <- body_add_flextable(doc, preprare_fusion_dt(fusion_dt()))
        }
      }, error = function(e) {
          message("Placeholder <<fusion_table>> was not found. Fusion table will not be added.")
      })
      
      tryCatch({
        doc <- cursor_reach(doc, "<<mutational_sign_table>>")
        doc <- body_remove(doc)
        if (is.null(mut_sign_dt()) || nrow(mut_sign_dt()) == 0) {
          doc <- body_add_par(doc, "No data about mutational signatures were found.")
        } else {
          doc <- body_add_flextable(doc, preprare_mut_sign_dt(mut_sign_dt()))
        }
      }, error = function(e) {
        message("Placeholder <<mutational_sign_table>> was not found. Mutational signatures table will not be added.")
      })

      tryCatch({
        doc <- cursor_reach(doc, "<<expression_table>>")
        doc <- body_remove(doc)
        
        dt <- expression_dt()
        
        if (is.null(dt) || nrow(dt) == 0) {
          doc <- body_add_par(doc, "No data from expression profile analysis were found.")
        } else {
          pathways <- unique(dt$pathway) # Rozdělení podle pathways
          for (p in pathways) {
            subset_dt <- dt[pathway == p, .(gene, expression_level, FC, therapeutic_option)]
            doc <- body_add_par(doc, p, style = "heading 5") # Přidej nadpis (podsekce) s názvem pathway
            doc <- body_add_flextable(doc, preprare_expression_dt(subset_dt))
          }
        }
      }, error = function(e) {
        message("Placeholder <<expression_table>> was not found. Expression profile table will not be added.")
      })

      tryCatch({
        doc <- cursor_reach(doc, "<<summary_table>>")
        doc <- body_remove(doc)
        
        if (ncol(summary_dt()) == 1) {    # if (is.null(summary_dt()) || nrow(summary_dt()) == 0) {
          doc <- body_add_par(doc, "No analysis metadata available.")
        } else {
          doc <- body_add_flextable(doc, preprare_summary_dt(summary_dt()))
        }
      }, error = function(e) {
        message("Placeholder <<summary_table>> was not found. Summary table will not be added.")
      })
      
      # # Pro obrázek
      # tryCatch({
      #   doc <- cursor_reach(doc, "<<plot1>>")
      #   doc <- body_remove(doc)
      #   if (!is.null(plot_file()) && file.exists(plot_file())) {
      #     doc <- body_add_img(doc, src = plot_file(), width = 6, height = 4)
      #   } else {
      #     # Volitelně můžete přidat zprávu, že graf není k dispozici
      #     doc <- body_add_par(doc, "No plot data available.")
      #   }
      # }, error = function(e) {
      #   # V případě, že značka není nalezena, přidej obrázek na konec
      #   doc <- body_add_par(doc, "Plot:", style = "heading 2")
      #   if (!is.null(plot_file()) && file.exists(plot_file())) {
      #     doc <- body_add_img(doc, src = plot_file(), width = 6, height = 4)
      #   } else {
      #     doc <- body_add_par(doc, "No plot data available.")
      #   }
      # })
      
      print(doc, target = file)
    }
  )

  # 
  # output$fusion_content <- renderUI({
  #   if (is.null(fusion_dt()) || nrow(fusion_dt()) == 0) {
  #     p("No clinically relevant fusion genes were found.")
  #   } else {
  #     reactableOutput("fusion_resultTab")
  #   }
  # })
  # 
  # observe({
  #   if (!is.null(fusion_dt()) && nrow(fusion_dt()) > 0) {
  #     output$fusion_resultTab <- renderReactable({
  #       reactable(as.data.frame(fusion_dt()))
  #     })
  #   }
  # })
  # 
  # 
  # output$som_content <- renderUI({
  #   if (is.null(somatic_dt()) || nrow(somatic_dt()) == 0) {
  #     p("No variants with known or potential clinical significance were found.")
  #   } else {
  #     reactableOutput("som_resultTab")
  #   }
  # })
  # 
  # observe({
  #   if (!is.null(somatic_dt()) && nrow(somatic_dt()) > 0) {
  #     output$som_resultTab <- renderReactable({
  #       reactable(as.data.frame(somatic_dt()))
  #     })
  #   }
  # })
  # 
  # output$germ_content <- renderUI({
  #   if (is.null(germline_dt()) || nrow(germline_dt()) == 0) {
  #     p("No variants with known or potential clinical significance in genes associated with hereditary cancer-predisposing syndromes were found.")
  #   } else {
  #     reactableOutput("germ_resultTab")
  #   }
  # })
  # 
  # observe({
  #   if (!is.null(germline_dt()) && nrow(germline_dt()) > 0) {
  #     output$germ_resultTab <- renderReactable({
  #       reactable(as.data.frame(germline_dt()))
  #     })
  #   }
  # })
}

shinyApp(ui, server, options = list(launch.browser = TRUE))
