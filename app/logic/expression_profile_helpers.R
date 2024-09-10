# app/logic/expression_profile_helpers.R

box::use(
  reactable[colDef,colGroup],
  reactablefmtr[pill_buttons,data_bars]
)

box::use(
  app/logic/load_data[get_inputs,load_data]
)

#########################
##   get tissue name   ##
#########################

#' @export
get_tissues <- function(patient){  # patient = "DZ1601"
  
  input_files <- get_inputs("per_sample_file")
  patient_files <- input_files$expression.files[grep(patient, input_files$expression.files)]
  
  tissue_list <- lapply(patient_files, function(file) { # file <- patient_files[1]
    tissue <- gsub(paste0(".*",  input_files$expression.project, ".*\\_(.*)\\_report.xlsx"), "\\1", file)
    tissue <- gsub("[ -]", "", tissue)
  })
  
  return(tissue_list)
}



##################################
##   reactable custom setting   ##
##################################

#' @export
set_pathway_colors <- function(){
  pathway_colors <- c(
    "Metabolic Signaling" = "darkturquoise",
    "Chromatin Remodeling/DNA Methylation" = "orchid1",
    "PI3K/AKT1/MTOR Signaling" = "green4",
    "Mitogen Activated Protein (MAP) Kinase Signaling" = "maroon",
    "Receptor Tyrosine Kinase/Growth Factor Signaling" = "palegreen2",
    "Immune Response" = "#D2691E",
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
sorting_setting <- function(data){
  scale_columns <- grep("^Scale_", names(data), value = TRUE)
  fc_columns <- grep("^FC_", names(data), value = TRUE)
  first_scale_col <- scale_columns[1]
  first_fc_col <- fc_columns[1]
  
  default_sorted <- list()
  default_sorted[[first_scale_col]] <- "desc"
  default_sorted[[first_fc_col]] <- "desc"
  
  return(default_sorted)
}  

#' @export
groups_setting <- function(data){
  scale_columns <- grep("^Scale_", names(data), value = TRUE)
  fc_columns <- grep("^FC_", names(data), value = TRUE)
  
  column_groups <- list()
  unique_tissues <- unique(gsub("Scale_|FC_", "", c(scale_columns, fc_columns)))
  
  for (tissue in unique_tissues) {
    scale_col <- grep(paste0("^Scale_", tissue), scale_columns, value = TRUE)
    fc_col <- grep(paste0("^FC_", tissue), fc_columns, value = TRUE)
    column_groups <- append(column_groups, list(colGroup(name = tissue, columns = c(scale_col, fc_col))))
  }
  
  return(column_groups)
}

