# TENTO SKRIPT OBSAHUJE POMOCNE FUNKCE

# nacteni knihoven
box::use(data.table[fread,setcolorder],
         reactable[colDef],
         networkD3[sankeyNetwork],
         dplyr[count,group_by,summarise,n,left_join,select],
         circlize)
box::use(
  shiny[renderPlot],
  shinycssloaders[withSpinner],
)

# funkce slouzi k zobrazeni ukazatele nacitaci behem nacitani dat v tabulce
#' @export
use_spinner <- function(ui_element){
  spinner <- withSpinner(ui_element,type=3,color = "#060606",color.background = "#EEEEEE" )
  return(spinner)
}

# funkce prida ke zpracovavanym datum sloupce in library
#' @export
add_library_column <- function(data_list) {
  all_values <- unique(unlist(lapply(data_list, function(data) data$Gene_symbol)))
  data_list_with_library <- lapply(data_list, function(data) {
    data$library <- sapply(data$Gene_symbol, function(value) {
      count <- sum(sapply(data_list, function(d) value %in% d$Gene_symbol))
      return(paste(count, "/", length(data_list), sep = ""))
    })
    return(data)
  })
  return(data_list_with_library)
}

# funkce nacte a pripravi zobrazovana data
#' @export
load_and_prepare <- function(directory){
  tsv_files <- list.files(directory, pattern = "\\.tsv$", full.names = TRUE)
  data_list <- lapply(tsv_files, function(file_path) {
    data <- fread(file_path)  
    return(data)  
  })
  data_list_l <- add_library_column(data_list)
  for (i in 1:length(data_list_l)){
    data_list_l[[i]] <- data_list_l[[i]][, c("SOMATIC", "PHENO","GENE_PHENO") := NULL]
    order <- c("var_name","library","Gene_symbol","tumor_variant_freq","tumor_depth","gene_region",
               "fOne","CGC_Somatic","gnomAD_NFE","clinvar_sig","clinvar_DBN","snpDB","COSMIC",
               "HGMD","Consequence","HGVSc","HGVSp","all_full_annot_name")
    data_list_l[[i]] <- setcolorder(data_list_l[[i]],order)
  }
  return(data_list_l)
}

# funkce definuje zakladni parametry vsech sloupcu v datech - nazev, viditelnost, moznost filtrace
#' @export
default_col <- function(){
  default_columns <- list(
    "var_name" = colDef(sticky='left', minWidth=140,maxWidth=160,filterable = TRUE, name = 'Variant name'),
    "library"=colDef(sticky='left',header = "In library"),
    "alarm" = colDef(show=FALSE,name="Alarm"),
    "full_annot_name" = colDef(show=FALSE,name="Annotated name",minWidth = 240,),
    "var_gen_coord" = colDef(show=FALSE,name="Variant coordinates",minWidth = 240,filterable=TRUE),
    "variant_type" = colDef(filterable = TRUE, name = 'Variant type',show=FALSE),
    "Gene_symbol" = colDef(sticky = 'left',filterable = TRUE, minWidth=110,maxWidth=140,name = 'Gene symbol'),
    "HGVSp" = colDef(minWidth=120,maxWidth=250,name="HGVSp"),
    "HGVSc" = colDef(minWidth=120,maxWidth=250,name="HGVSc"),
    "tumor_variant_freq" = colDef(name="Tumor variant frequency",minWidth=180,maxWidth=200, filterable = TRUE),
    "tumor_depth" = colDef(name = "Tumor depth",minWidth=110),
    "normal_variant_freq" = colDef(show=FALSE,name="Normal variant frequency",minWidth=190),
    "normal_depth" = colDef(show=FALSE,name = "Normal depth",minWidth=110),
    "Called_by"	= colDef(show=FALSE,name="Called by"),
    "1000g_EUR_AF" = colDef(show=FALSE,name="1000G EUR AF",minWidth=130),
    "gnomAD_NFE" = colDef(name = "GnomAD NFE",filterable=FALSE,minWidth=110),
    "snpDB" = colDef(name="SnpDB"),
    "COSMIC" = colDef(name="COSMIC"),
    "HGMD" = colDef(name="HGMD"),
    "NHLBI_ESP" = colDef(show=FALSE,name="NHLBI ESP",minWidth=110),
    "clinvar_sig" = colDef(name = "ClinVar significance", filterable = TRUE,minWidth=180),
    "clinvar_DBN" = colDef(show=TRUE,name="ClinVar DBN",filterable = TRUE),
    "fOne" = colDef(name="fOne"),
    "md-anderson" = colDef(show=FALSE,name="MD Anderson",minWidth=110),
    "trusight_genes" = colDef(show=FALSE,name="TruSight genes",minWidth=130),
    "CGC_Somatic" = colDef(name="CGC Somatic",minWidth=120),
    "CGC_Tumour_Somatic" = colDef(show=FALSE,name="CGC Tumour",minWidth=110),
    "PolyPhen" = colDef(show=FALSE,name="PolyPhen",minWidth=190),
    "SIFT" = colDef(show=FALSE,name="SIFT",minWidth=180),
    "gene_region" = colDef(name="Gene region",filterable = TRUE, minWidth=110,maxWidth=140),
    "IMPACT" = colDef(show=FALSE,name="Impact"),
    "Consequence" = colDef(name = "Consequence",minWidth=140,maxWidth=700,filterable=TRUE),
    "EXON" = colDef(show=FALSE, name= "Exon"),
    "INTRON" = colDef(show=FALSE, name = "Intron"),
    "Feature" = colDef(show=FALSE, name = "Feature",minWidth=180),
    "Feature_type" = colDef(show=FALSE, name = "Feature type",filterable=TRUE),
    "Annotation source" = colDef(show=FALSE, name = "Annotation source",minWidth=160),
    "all_full_annot_name" = colDef(name = "Full annotated name", minWidth = 240,show=TRUE),
    "Gene" = colDef(show=FALSE, name = "Gene")
  )
  return(default_columns)
}

# funkce pri automatickem nacteni moznych sloupcu prepisuje nazvy predem ocekavanych na vhodnejsi podobu
#' @export
map_column_names <- function(column_names) {
  name_mapping <- list(
    var_name = "Variant name",
    library = "In library",
    Gene_symbol = "Gene symbol",
    HGVSp = "HGVSp",
    HGVSc = "HGVSc",
    tumor_variant_freq = "Tumor variant frequency",
    tumor_depth = "Tumor depth",
    gnomAD_NFE = "GnomAD NFE",
    snpDB = "SnpDB",
    COSMIC = "COSMIC",
    HGMD = "HGMD",
    clinvar_sig = "ClinVar significance",
    clinvar_DBN = "ClinVar DBN",
    fOne = "fOne",
    CGC_Somatic = "CGC Somatic",
    gene_region = "Gene region",
    Consequence = "Consequence",
    all_full_annot_name = "Full annotated name",
    alarm = "Alarm",
    full_annot_name = "Annotated name",
    var_gen_coord = "Variant coordinates",
    variant_type = "Variant type",
    normal_variant_freq = "Normal variant frequency",
    normal_depth = "Normal depth",
    Called_by = "Called by",
    "1000g_EUR_AF" = "1000G EUR AF",
    NHLBI_ESP = "NHLBI ESP",
    "md-anderson" = "MD Anderson",
    trusight_genes = "TruSight genes",
    CGC_Tumour_Somatic = "CGC Tumour",
    PolyPhen = "PolyPhen",
    SIFT = "SIFT",
    IMPACT = "Impact",
    EXON = "Exon",
    INTRON = "Intron",
    Feature = "Feature",
    Feature_type = "Feature type",
    "Annotation source" = "Annotation source",
    Gene = "Gene"
  )

  unname(sapply(column_names, function(col) {
    if (col %in% names(name_mapping)) {
      name_mapping[[col]]
    } else {
      col 
    }
  }))
}

# funkce pri automatickem nacteni moznych oblastni genu prepisuje nazvy predem ocekavanych na vhodnejsi podobu
#' @export
map_gene_region_names <- function(gene_region_names){
  gene_region_mapping <- list(
    "exon" = "Exon",
    "intron" = "Intron",
    "downstream" = "Downstream",
    "splice" = "Splice",
    "5_prime_UTR" = "5 prime UTR",
    "3_prime_UTR" = "3 prime UTR",
    "upstream" = "Upstream",
    "non_coding" = "Non-coding",
    "regulatory_region" = "Regulatory region"
  )
  
  unname(sapply(gene_region_names, function(reg) {
    if (as.character(reg) %in% names(gene_region_mapping)) {
      gene_region_mapping[[reg]]
    } else {
      reg 
    }
  }))
} 

# funkce pri automatickem nacteni moznych clinvar_sig prepisuje nazvy predem ocekavanych na vhodnejsi podobu
#' @export
map_clin_sig_names <- function(clin_sig_names){
  clin_sig_mapping <- list(
    "Likely_benign"="Likely benign",
    "Pathogenic"="Pathogenic",
    "Uncertain_significance"="Uncertain significance",
    "Benign"="Benign",
    "Conflicting_classifications_of_pathogenicity"="Conflicting classifications of pathogenicity",
    "." = "Unknown"
  )
  
  unname(sapply(clin_sig_names, function(sig) {
    if (as.character(sig) %in% names(clin_sig_mapping)) {
      clin_sig_mapping[[sig]]
    } else {
      sig 
    }
  }))
}


