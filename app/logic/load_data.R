# app/logic/load_data.R

box::use(
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames,fwrite],
  openxlsx[read.xlsx,getSheetNames]
)
# sample <- "DZ1601"
# input_files <- fusion_genes_filenames
#' @export
load_data <- function(input_files, flag, sample = NULL,expr_flag = NULL){ 
  if (flag == "varcall"){
    input_var <- input_files[grepl(sample, input_files)]
    dt <- fread(input_var)
    dt[,sample := sample]

    if("in_library" %in% colnames(dt)){
      return(dt)

    } else {
      ## adding information about how unique variants are in samples
      ## this will run only first time when in_library column is not present 
      message("Creating in_library column.")

      filenames_all <- get_inputs("all_sample_file")
      input_files_all <- filenames_all$var_call.germline
      input_var_all <- lapply(input_files_all,fread)
      dt_all <- rbindlist(input_var_all)
      
      sample_list <- gsub(".*\\/(.*)\\.variants.tsv","\\1",input_files)
      
      dt_all[, in_library := rowSums(.SD), .SDcols = sample_list]
      dt_all[, in_library := paste0(in_library, "/", length(sample_list))]
      merged_dt <- merge(dt, unique(dt_all[, .(var_name, in_library)]), by = "var_name", all.x = TRUE)
      fwrite(merged_dt,input_var)
      rm(dt_all,input_var_all,filenames_all,input_files_all,dt)
      
      return(merged_dt)
    }

  } else if (flag == "fusion") {
    input_var <- input_files[grepl(sample, input_files)]
    dt <- as.data.table(read.xlsx(input_var))
    dt[, sample := sample]
    return(dt)
    
  } else if (flag == "expression") { 
    # input_files_var <- get_inputs("per_sample_file")
    patient_files <- input_files[grep(sample, input_files)] # sample = "DZ1601"
    
    if(expr_flag == "genes_of_interest"){
      patient_files <- patient_files[grep(expr_flag, patient_files)] # sample = "DZ1601"
      
      dt_list <- lapply(patient_files, function(file) {
        dt <- fread(file)
        tissue <- gsub("^.*/|_genes_of_interest\\.tsv$","",file)
        dt[, c("tissue", "sample") := .(tissue, sample)]
        return(dt)
      })
      combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)

    } else if (expr_flag == "all_genes"){
      patient_files <- patient_files[grep(expr_flag, patient_files)] # sample = "DZ1601"
      dt_list <- lapply(patient_files, function(file) {
        dt <- fread(file)
        tissue <- gsub("^.*/|_all_genes\\.tsv$","",file)
        dt[, c("tissue", "sample") := .(tissue, sample)]
        return(dt)
      })
      combined_dt <- rbindlist(dt_list, use.names = TRUE, fill = TRUE)
    } else{
      stop("Your data table has wrong name. Must contain string gene_of_interest or all_genes.")
    }

    return(combined_dt)
    
    
    # patient_data <- lapply(patient_files, function(file) { #file <- patient_files[1]
    #   sheet_names <- getSheetNames(file)
    #   input_var <- lapply(sheet_names, function(sheet) read.xlsx(file, sheet = sheet))
    #   names(input_var) <- sheet_names
    #   info <- gsub(paste0(".*", input_files_var$expression.project, "\\/(.*)\\_[0-9]*\\_(.*)\\_report.xlsx"), "\\1_\\2", file)
    #   info <- gsub("[ -]", "", info)
    # 
    #   dt <- as.data.table(rbindlist(input_var, fill = TRUE))
    #   dt[, c("Sample", "Tissue") := tstrsplit(info, "_")]
    #   setcolorder(dt, c("Sample", "Tissue", "Gene", "Pathway", "Scale", "FC"))
    #   tissue <- unique(dt$Tissue)
    #   setnames(dt, c("Sample", paste0("Tissue_",tissue), "Gene", "Pathway", paste0("Scale_",tissue), paste0("FC_",tissue)))
    # 
    #   return(dt)
    # })
# 
#     combined_data <- merge(patient_data[[1]], patient_data[[2]], by = c("Sample", "Gene", "Pathway"), all = TRUE)
#     return(combined_data)

  } else {
    return(print("not varcall nor fusion nor expression"))
  }
}

#' @export
get_inputs <- function(flag){
  
  library <- "MOII_e117"
  path = paste0("./input_files/", library)
  
  if (flag == "per_sample_file"){
    
    ## somatic var call path
    somatic_variant_calling_project <- "117_WES_somatic"
    somatic_variant_calling_filenames <- list.files(paste(path, somatic_variant_calling_project,"per_sample_final_var_tabs/tsv_formated",sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## gemline var call path
    germline_variant_calling_project <- "117_WES_germline"
    germline_variant_calling_filenames <- list.files(paste(path, germline_variant_calling_project,"per_sample_final_var_tabs/tsv_formated", sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## structural var call path
    structural_variant_calling_project <- "117_WES_structural"
    structural_variant_calling_filenames <- list.files(paste(path, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## fusion var call path
    fusion_genes_project <- "117_fusions"
    fusion_genes_filenames <- list.files(paste(path, fusion_genes_project, "results", sep = "/"), pattern = "*.xlsx", full.names = TRUE)
    
    ## arriba folder path
    arriba_res_folder <- paste(path, fusion_genes_project, "results", sep = "/")
    
    ## expression profile results path
    expression_profile_project <- "RNAseq21_NEW"
    expression_profile_dirnames <- list.files(paste(path, expression_profile_project,sep = "/"), pattern = "*", full.names = TRUE)
    expression_profile_filenames <- list.files(expression_profile_dirnames, pattern = "*.tsv", full.names = TRUE)
    # expression_profile_project <- "RNAseq21"
    # expression_profile_filenames <- list.files(paste(path, expression_profile_project, sep = "/"), pattern = "*_report.xlsx", full.names = TRUE)
    # 
    return(list(var_call.somatic = somatic_variant_calling_filenames,
                var_call.germline = germline_variant_calling_filenames,
                var_call.structural = structural_variant_calling_filenames,
                fusions = fusion_genes_filenames,
                arriba_res = arriba_res_folder,
                expression.files = expression_profile_filenames,
                expression.project = expression_profile_project
    ))
    
  } else if (flag == "all_sample_file"){

    ## somatic var call path
    somatic_variant_calling_project <- "117_WES_somatic"
    somatic_variant_calling_filenames <- list.files(paste(path, somatic_variant_calling_project,sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## gemline var call path
    germline_variant_calling_project <- "117_WES_germline"
    germline_variant_calling_filenames <- list.files(paste(path, germline_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
    
    ## structural var call path
    structural_variant_calling_project <- "117_WES_structural"
    structural_variant_calling_filenames <- list.files(paste(path, structural_variant_calling_project, sep = "/"), pattern = "*.tsv", full.names = TRUE)
  

    return(list(var_call.somatic = somatic_variant_calling_filenames,
                var_call.germline = germline_variant_calling_filenames,
                var_call.structural = structural_variant_calling_filenames
    ))
    
  } else {
    stop("Invalid tag. Use 'per_sample_file' or 'all_sample_file'.")
  }
}
  


# test app
# library(data.table)
# library(openxlsx)
