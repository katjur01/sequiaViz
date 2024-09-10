# app/logic/prepare_varcall_tab.R

box::use(
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames],
  openxlsx[read.xlsx,getSheetNames],
  app/logic/patients_list[]
)



input_data <- function(){
  filenames <- get_inputs("per_sample_file")
  data <- load_data(filenames$var_call.germline,"varcall","MR1507krev")
  return(data)
}


load_data <- function(input_files,flag, sample = NULL){

    

  
  }
}

get_inputs <- function(flag){
  
  if (flag == "per_sample_file"){
    
    library <- "MOII_e117"
    
    ## somatic var call path
    somatic_variant_calling_project <- "117_WES_somatic"
    somatic_variant_calling_filenames <- list.files(paste("../input_files", library, somatic_variant_calling_project,"per_sample_final_var_tabs/tsv_formated",sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## gemline var call path
    germline_variant_calling_project <- "117_WES_germline"
    germline_variant_calling_filenames <- list.files(paste("./input_files", library, germline_variant_calling_project,"per_sample_final_var_tabs/tsv_formated", sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## structural var call path
    structural_variant_calling_project <- "117_WES_structural"
    structural_variant_calling_filenames <- list.files(paste("../input_files", library, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## fusion var call path
    fusion_genes_project <- "117_fusions"
    fusion_genes_filenames <- list.files(paste("../input_files", library, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
    
    ## arriba folder path
    arriba_res_folder <- paste("../input_files", library, fusion_genes_project, "results", sep = "/")
    
    ## expression profile results path
    expression_profile_project <- "RNAseq21"
    expression_profile_filenames <- list.files(paste("../input_files", library, expression_profile_project, sep = "/"), pattern = "*_report.xlsx", full.names = TRUE)
    
    return(list(var_call.somatic = somatic_variant_calling_filenames,
                var_call.germline = germline_variant_calling_filenames,
                var_call.structural = structural_variant_calling_filenames,
                fusions = fusion_genes_filenames,
                arriba_res = arriba_res_folder,
                expression.files = expression_profile_filenames,
                expression.project = expression_profile_project
    ))

  } else if (flag == "all_sample_file"){
    library <- "MOII_e117"
    ## somatic var call path
    somatic_variant_calling_project <- "117_WES_somatic"
    somatic_variant_calling_filenames <- list.files(paste("../input_files", library, somatic_variant_calling_project,sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## gemline var call path
    germline_variant_calling_project <- "117_WES_germline"
    germline_variant_calling_filenames <- list.files(paste("./input_files", library, germline_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## structural var call path
    structural_variant_calling_project <- "117_WES_structural"
    structural_variant_calling_filenames <- list.files(paste("../input_files", library, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## fusion var call path
    fusion_genes_project <- "117_fusions"
    fusion_genes_filenames <- list.files(paste("../input_files", library, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
    
    ## arriba folder path
    arriba_res_folder <- paste("../input_files", library, fusion_genes_project, "results", sep = "/")
    
    ## expression profile results path
    expression_profile_project <- "RNAseq21"
    expression_profile_filenames <- list.files(paste("../input_files", library, expression_profile_project, sep = "/"), pattern = "*_report.xlsx", full.names = TRUE)
    
    return(list(var_call.somatic = somatic_variant_calling_filenames,
                var_call.germline = germline_variant_calling_filenames,
                var_call.structural = structural_variant_calling_filenames,
                fusions = fusion_genes_filenames,
                arriba_res = arriba_res_folder,
                expression.files = expression_profile_filenames,
                expression.project = expression_profile_project
    ))
    
  } else {
    stop("Invalid tag. Use 'per_sample_file' or 'all_sample_file'.")
  }
  
  library <- "MOII_e117"
  ## somatic var call path
  somatic_variant_calling_project <- "117_WES_somatic"
  somatic_variant_calling_filenames <- list.files(paste("../input_files", library, somatic_variant_calling_project,"per_sample_final_var_tabs/tsv_formated",sep = "/"), pattern = "*.tsv", full.names = TRUE)
  
  ## gemline var call path
  germline_variant_calling_project <- "117_WES_germline"
  germline_variant_calling_filenames_all <- list.files(paste("./input_files", library, germline_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
  
  ## structural var call path
  structural_variant_calling_project <- "117_WES_structural"
  structural_variant_calling_filenames <- list.files(paste("../input_files", library, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
  
  ## fusion var call path
  fusion_genes_project <- "117_fusions"
  fusion_genes_filenames <- list.files(paste("../input_files", library, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
  
  ## arriba folder path
  arriba_res_folder <- paste("../input_files", library, fusion_genes_project, "results", sep = "/")
  
  ## expression profile results path
  expression_profile_project <- "RNAseq21"
  expression_profile_filenames <- list.files(paste("../input_files", library, expression_profile_project, sep = "/"), pattern = "*_report.xlsx", full.names = TRUE)
  
  return(list(var_call.somatic = somatic_variant_calling_filenames,
              var_call.germline = germline_variant_calling_filenames,
              var_call.structural = structural_variant_calling_filenames,
              fusions = fusion_genes_filenames,
              arriba_res = arriba_res_folder,
              expression.files = expression_profile_filenames,
              expression.project = expression_profile_project
  ))
}
