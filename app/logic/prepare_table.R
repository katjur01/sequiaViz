# app/logic/prepare_table.R

box::use(
  data.table[fread,tstrsplit,setcolorder,setnames,uniqueN,dcast],
  shiny[observe],
  openxlsx[read.xlsx]
)
box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_arriba_pictures[pdf2png]
)

replace_dot_with_na <- function(data) {
  for (col in names(data)) {
    data[get(col) == ".", (col) := NA]
  }
  return(data)
}

split_genes <- function(dt) {
  dt_gene1 <- dt[, .(gene1 = unlist(strsplit(gene1, ",", fixed = TRUE)),
                     gene2, chrom1, pos1, chrom2, pos2, path), by = .(.I)]
  dt_gene2 <- dt_gene1[, .(gene1, gene2 = unlist(strsplit(gene2, ",", fixed = TRUE)),
                           chrom1, pos1, chrom2, pos2, path), by = .(.I)]
  dt_gene2[,I := NULL]
  
  return(dt_gene2)
}

prepare_igv_snapshot_paths <- function(data,sample){   # sample = "DZ1601fuze"
  
  input_xlsx <- data[,.(gene1 = paste(unique(gene1),collapse = ","),gene2 = paste(unique(gene2),collapse = ",")),by = .(chrom1,pos1,chrom2,pos2)]
  input_xlsx[, path := sprintf(paste0("./igv_snapshot/",sample,"/",sample,"_%03d.png"), .I)]
  xlsx_dt <- split_genes(input_xlsx)
  setnames(xlsx_dt, "path", "png_path")
  
  return(xlsx_dt)
}

prepare_arriba_image_paths <- function(sample){

  filenames <- get_inputs("per_sample_file")
  arriba_res_folder <- filenames$arriba_res
  input_tsv <- fread(paste0(paste(arriba_res_folder,sample,"arriba/",sep="/"),sample,".arriba_fusion.tsv"))
  input_tsv[, path := sprintf(paste0("./arriba_viz/",sample,"/",sample,"_%03d.svg"), .I)]
  
  input_tsv[, gene1 := gsub("\\(.*?\\)", "", `#gene1`)]
  input_tsv[, gene2 := gsub("\\(.*?\\)", "", gene2)]
  input_tsv[, c("chrom1", "pos1") := tstrsplit(breakpoint1, ":", fixed = TRUE)]
  input_tsv[, c("chrom2", "pos2") := tstrsplit(breakpoint2, ":", fixed = TRUE)]
  input_tsv[, chrom1 := paste0("chr", chrom1)]
  input_tsv[, chrom2 := paste0("chr", chrom2)]
  input_tsv[, pos1 := as.numeric(pos1)]
  input_tsv[, pos2 := as.numeric(pos2)]
  
  tsv_dt <- split_genes(input_tsv)
  setnames(tsv_dt, "path", "svg_path")

  return(tsv_dt)
}

#' @export
prepare_arriba_images <- function(sample){
#   #sample = "LK0302fuze"
#   filenames <- get_inputs("per_sample_file")
#   arriba_images_folder <- filenames$arriba_images
#   input_pdf <- paste0(paste(arriba_images_folder,sample,"arriba/",sep="/"),sample,".arriba_fusion_viz.pdf")
# 
#   print(getwd())
#   folder_path <- paste0("./www/arriba_viz/",sample)
#   output_svg <- paste0(folder_path,"/",sample,"_%03d.svg")
#   print(output_svg)
#   pdf2png(input_pdf,folder_path,output_svg)
}

# selected_samples = "DZ1601fuze"

#' @export
prepare_fusion_genes_table <- function(data,selected_samples){
  
  xlsx_dt <- prepare_igv_snapshot_paths(data,selected_samples)
  tsv_dt <- prepare_arriba_image_paths(selected_samples)
  paths_dt <- merge(xlsx_dt, tsv_dt, by = c("gene1", "gene2","chrom1", "pos1", "chrom2", "pos2"), all.x = TRUE)

  dt <- merge(data, paths_dt, by = c("gene1", "gene2","chrom1", "pos1", "chrom2", "pos2"), all.x = TRUE)
  dt[,position1 := paste0(chrom1,":",pos1)]
  dt[,position2 := paste0(chrom2,":",pos2)]
  dt[,c("chrom1","pos1","chrom2","pos2") := NULL]
  default_selection <- c("gene1","gene2","arriba.called","starfus.called","arriba.confidence","overall_support","IGV","Visual_Check","Notes","position1","strand1","position2","strand2",
                         "arriba.site1","arriba.site2","starfus.splice_type","DB_count","DB_list")
  
  dt[, `:=`(IGV = "", Visual_Check = "", Notes = "")]
  setcolorder(dt, default_selection)
  message(paste0("Fusion genes, pacient ",unique(dt$sample)," (prepare_table script)"))
  return(dt)
}

#' @export
prepare_germline_table <- function(dt){
  
  dt <- replace_dot_with_na(dt)
  dt[,gnomAD_NFE := as.numeric(gnomAD_NFE)]
  dt[,variant_freq := as.numeric(variant_freq)]
  default_selection <- c("var_name","variant_freq","in_library","Gene_symbol","coverage_depth","gene_region",
                         "gnomAD_NFE","clinvar_sig","snpDB","CGC_Germline","trusight_genes","fOne","Consequence","HGVSc", "HGVSp","all_full_annot_name")
  setcolorder(dt, default_selection)
          
  message(paste0("Germline varcall, pacient ",unique(dt$sample)," (prepare_table script)"))
  return(dt)
}

#' @export
get_tissues <- function(patient){  # patient = "DZ1601"
  
  input_files <- get_inputs("per_sample_file")
  patient_files <- input_files$expression.files[grep(patient, input_files$expression.files)]
  patient_files <- patient_files[grep("all_genes", patient_files)]
  tissue_list <- lapply(patient_files, function(file) { # file <- patient_files[1]
    tissue <-   gsub("^.*/|_all_genes\\.tsv$","",patient_files)
  })
  
  return(tissue_list)
}

#' @export
prepare_expression_table <- function(combined_dt,expr_flag){
  if(expr_flag == "genes_of_interest"){
    wide_dt <- dcast(combined_dt,
                     sample + feature_name + geneid + pathway ~ tissue,
                     value.var = c("log2FC", "p_value", "p_adj"))
    # Column ordering
    ordered_columns <- c("sample", "feature_name", "geneid", "pathway")
    tissue_cols <- unlist(lapply(unique(combined_dt$tissue), function(tissue) {
      paste0(c("log2FC", "p_value", "p_adj"), "_", tissue)
    }))
    wide_dt <- wide_dt[, c(ordered_columns, tissue_cols), with = FALSE]
  } else if (expr_flag == "all_genes"){
    wide_dt <- dcast(combined_dt,
                     sample + feature_name + geneid + pathway + refseq_id + type + all_kegg_gene_names
                     + gene_definition + kegg_paths_id + kegg_paths_name ~ tissue,
                     value.var = c("log2FC", "p_value", "p_adj"))
  } else {
    stop("Unknown expr_tag: ", expr_flag)
  }

  
  message(paste0("Expression profile, pacient ",unique(wide_dt$sample)," (prepare_table script)"))
  return(wide_dt)
}

#' @export
set_pathway_colors <- function(){
  pathway_colors <- c(
    "Metabolic Signaling" = "darkturquoise",
    "Chromatin Remodeling/DNA Methylation" = "orchid1",
    "PI3K/AKT1/MTOR Signaling" = "green4",
    "Mitogen Activated Protein (MAP) Kinase Signaling" = "maroon",
    "Receptor Tyrosine Kinase/Growth Factor Signaling" = "palegreen2",
    "Kinase Fusions" = "#D2691E",
    "apoptosis" = "#FF7F00",
    "WNTsignaling" = "#6495ED",
    "Hormone Signaling" = "#DC143C",
    "Cellular architecture and microenvironment" = "#6A5ACD", 
    "DNA damage/repair" = "skyblue2",
    "Cell cycle control" = "#FB9A99",
    "non-WNT/non-SHH medulloblastoma-related markers" = "khaki2",
    "Immune Checkpoints" = "#A9A9A9",
    "TGF-B Signaling" = "gold1",
    "Janus Kinase (JAK)/ (STAT) Signaling" = "#BDB76B",
    "Hedgehog Signaling" = "#8B008B"
  )
  return(pathway_colors)
}

#' @export
colFilter <- function(flag){
  filenames <- get_inputs("per_sample_file")
  # message(paste("Getting columns for flag:", flag))

  if (flag == "germline"){
    all_column_var <- names(fread(filenames$var_call.germline[1], nrows = 0))
    all_column_names  <- setdiff(all_column_var, c("sample"))
    default_selection <- c("var_name","variant_freq","in_library","Gene_symbol","coverage_depth","gene_region",
                           "gnomAD_NFE","clinvar_sig","snpDB","CGC_Germline","trusight_genes","fOne","Consequence","HGVSc", "HGVSp","all_full_annot_name")
    
  } else if (flag == "fusion"){
    if (file.exists(filenames$fusions[1])) {
      all_column_var <- names(read.xlsx(filenames$fusions[1], rows = 1))
      filtered_all_column_var <- setdiff(all_column_var, c("chrom1", "chrom2", "pos1", "pos2"))
      
      all_column_names <- c(filtered_all_column_var,"IGV","Visual_Check","Notes","position1","position2")
      default_selection <- c("gene1","gene2","arriba.called","starfus.called","arriba.confidence","overall_support","IGV","Visual_Check","Notes","position1","strand1","position2","strand2",
                             "arriba.site1","arriba.site2","starfus.splice_type","DB_count","DB_list")
    } else {
      stop("Fusion file not found: ", filenames$fusions[1])
    }

  } else {
    print("NOT germline NOR fusion")
  }

  ordered_columns <- factor(all_column_names, levels = default_selection)
  all_column_names_sorted <- all_column_names[order(ordered_columns)]
  # message(paste("Returning column names for flag:", flag))
  return(list(all_columns = all_column_names_sorted, default_setting = default_selection))
}

#' @export
columnName_map <- function(tag){
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
  } else {
    print("NOT germline NOR fusion")
  }
return(map_list)
}

#' @export
prepare_variant_calling_table <- function(dt,selected_samples){

  if ("CGC_Somatic" %in% names(dt)){
    observe({
      print(selected_samples)
    })

    patients = c("DZ1601","MR1507","VH0452")

    dt <- dt[, names(dt) %in% c("var_name", "Gene_symbol", patients,"HGVSc", "HGVSp", "tumor_variant_freq", "tumor_depth", "gnomAD_NFE", "gene_region",
                                "Consequence","snpDB", "COSMIC", "HGMD", "clinvar_sig","clinvar_DBN","fOne","CGC_Somatic","all_full_annot_name"),with=FALSE]
    dt[,Visual_Check := ""]
    setcolorder(dt,c("var_name", "Gene_symbol", patients,"Visual_Check","tumor_variant_freq", "tumor_depth","gnomAD_NFE","fOne","CGC_Somatic","clinvar_sig","clinvar_DBN","snpDB", "COSMIC", "HGMD","gene_region","Consequence","HGVSc", "HGVSp"))

  } else if ("CGC_Germline" %in% names(dt)) {
    patients = c("DZ1601krev","MR1507krev","VH0452krev")

  } else(print("Neither CGC_Germline nor CGC_Somatic column exists"))

  return(dt)
}

#' @export
default_sorted_table <- function(dt){

  if ("CGC_Somatic" %in% names(dt)){
    default_sorted <- list("fOne" = "desc","clinvar_sig" = "desc","CGC_Somatic" = "desc")

  } else if ("CGC_Germline" %in% names(dt)) {
    default_sorted <- list("fOne" = "desc","clinvar_sig" = "desc","CGC_Germline" = "desc")

  } else(print("Neither CGC_Germline nor CGC_Somatic column exists"))

  return(default_sorted)
}


# dt <- fread("./input_files/MOII_e117/117_WES_germline/final_variant_table.tsv")

#### fusion testing ####
#
# library("data.table")
# library("openxlsx")
# library <- "MOII_e117"
# fusion_genes_project <- "117_fusions"
# input_files <- list.files(paste("input_files", library, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
# input_var <- lapply(input_files,read.xlsx)
# names(input_var) <- gsub(".*results\\/(.*)\\_fusions.xlsx","\\1",input_files)
# data <- rbindlist(input_var, id = "sample", use.names = TRUE)
