## run in conda enviromant: /mnt/ssd/ssd_1/conda_envs/igv_test
# app/logic/igv_snapshot.R

box::use(
  data.table,
  openxlsx[read.xlsx],
  stringr[str_replace_all]
)

x <- 1
header_list <- lapply(1:length(fusions_tab$chrom_break_pos), function(x){
  paste(
    paste0("goto ",fusions_tab$chrom_break_pos[x])
    ,"viewaspairs"
    ,"maxPanelHeight 10000"
    ,"preference SAM.SHOW_SOFT_CLIPPED true"
    ,"preference IGV.Bounds 94,0,1280,1024"
    ,"preference SAM.SHOW_JUNCTION_TRACK true"
    ,"preference SAM.COLOR_BY NONE"
    ,paste0("snapshot DZ1601fuze_%03d.svg")
  )
})
head_l <- rbind(header_list)
header <- paste("new"
                ,"genome hg38"
                ,"load /mnt/share/share/710000-CEITEC/713000-cmm/713009-slaby/base/sequencing_results/primary_data/230426_MOII_e117_fuze/mapped/DZ1601fuze.bam"
                ,"load /mnt/share/share/710000-CEITEC/713000-cmm/713009-slaby/base/sequencing_results/primary_data/230426_MOII_e117_fuze/mapped/DZ1601fuze/DZ1601fuzeChimeric.out.bam"
                ,"snapshotDirectory /mnt/share/share/710000-CEITEC/713000-cmm/713016-bioit/base/workspace/katka/scripts/DZ1601fuze"

                ,"exit"
                ,sep = "\n")

cat(header,file = "batch_file_2.txt")
return(batch_file_name)
}
                                 chrom_break_pos = fusions_tab$chrom_break_pos[x],
                                 # snapshot_name = paste(fusions_tab$id[x],fusions_tab$fusion_genes[x],sep="_"),
                                 snapshot_name = paste(sample_name,"_%03d.svg"),
                                 output_dir = output_dir,
                                 batch_file_name = paste(fusions_tab$fusion_genes[x],"temp_batch.txt",sep="_"))

# createIGVBatchFile <- function(bam_file,chimeric_file,chrom_break_pos,snapshot_name,batch_file_name,output_dir,genome_build = "hg38"){
#   
#   header <- paste("new"
#                   ,paste0("genome ",genome_build)
#                   ,paste0("load ",bam_file)
#                   ,paste0("load ",chimeric_file)
#                   ,paste0("snapshotDirectory ",output_dir)
#                   ,paste0("goto ",chrom_break_pos)
#                   ,"viewaspairs"
#                   ,"maxPanelHeight 10000"
#                   ,"preference SAM.SHOW_SOFT_CLIPPED true"
#                   ,"preference IGV.Bounds 94,0,1280,1024"
#                   ,"preference SAM.SHOW_JUNCTION_TRACK true"
#                   ,"preference SAM.COLOR_BY NONE"
#                   ,paste0("snapshot ",snapshot_name,".png")
#                   ,"exit"
#                   ,sep = "\n")
#   
#   cat(header,file = batch_file_name)
#   return(batch_file_name)
# }


createIGVBatchFileFromXlsx <- function(xlsx_file,sample_name,zoom = 251){
  
  if(file.exists(xlsx_file)){
    input_table <- as.data.table(openxlsx::read.xlsx(xlsx_file))
  } else {
    cat("Input file don't exist.\n")
    return(NULL)
  }
  setnames(input_table,c("chrom1","chrom2"),c("chr1","chr2"))
  
  fusions_tab <- input_table[,.(gene1, gene2, chr1, pos1, chr2, pos2)]
  
  fusions_tab <- fusions_tab[,.(gene1 = paste(unique(gene1),collapse = ","),gene2 = paste(unique(gene2),collapse = ",")),by = .(chr1,pos1,chr2,pos2)]
  fusions_tab[,id := rep(1:nrow(fusions_tab))]
  
  
  # res <- merge(input_table,fusions_tab,by = c("gene1","gene2","chr1","pos1","chr2","pos2"))
  # res <- res[,-c("DB_count","DB_list","starfus.split_reads","starfus.discordant_mates","starfus.counter_fusion1","starfus.counter_fusion2","starfus.splice_type")]
  # res[,sample := sample_name]
  # res[,visual_check := NA]
  # res[,notes := NA]
  # setcolorder(res,c("sample","id","gene1","gene2","chr1","pos1","strand1","chr2","pos2","strand2","visual_check","notes","arriba.called","starfus.called","overall_support"))
  # setorder(res,gene1)
  # 
  # fwrite(res,file = paste0(sample_name,"_fusions_dt.xlsx"), sep = "\t")
  
  fusions_tab[,chrom_break_pos := paste0(chr1,":",pos1-zoom,"-",pos1+zoom," ", chr2,":",pos2-zoom,"-",pos2+zoom)]
  fusions_tab[,fusion_genes := paste0(gene1,"__",gene2)]
  fusions_tab$fusion_genes <- str_replace_all(fusions_tab$fusion_genes,"[.,()-]","_")
  fusions_tab <- unique(fusions_tab)
  
  fusions_tab[, svg_path := sprintf(paste0("./app/www/igv_snapshot/",sample_name,"/",sample_name,"_%03d.svg"), .I)]
  
  setorder(fusions_tab,gene1)
  
  return(fusions_tab)
}


runIGVSnapshot <- function(IGV_batch_file){
  igv_executive = "igv"
  
  # igv_command <- paste0("xvfb-run --server-args='-screen 0 1280x1024x24 -ac' ",igv_executive," -b ",IGV_batch_file)
  igv_command <- paste0("xvfb-run --auto-servernum --server-args='-screen 0 1280x1024x24 -ac' ",igv_executive," -b ",IGV_batch_file)
  system(igv_command)
  
  file.remove(IGV_batch_file)
}

run_all <- function(args){
  
  if (!dir.exists("./app/www/igv_snapshot")) {
    dir.create("./app/www/igv_snapshot") # Pokud složka neexistuje, vytvoř ji
  }
  
  ## to test
  # bam_file = args[1]
  # chimeric_file = args[2]
  # xlsx_file = args[3]
  # 
  # xlsx_file ="./input_files/MOII_e117/117_fusions/results/DZ1601fuze_fusions.xlsx"
  # bam_file <- "./input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze/mapped/DZ1601fuze.bam"
  # chimeric_file <-  "./input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze/mapped/DZ1601fuze/DZ1601fuzeChimeric.out.bam"

  a <- "./input_files/MOII_e117/117_fusions/results/DZ1601fuze_fusions.xlsx"
  path <- "./"

  
  lapply(args,function(a){
    
    xlsx_file <- a
    sample_name <- paste0(gsub("_fusions.xlsx","",basename(a)))
    bam_file <-  paste0("./input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze","/mapped/", sample_name,".bam")
    chimeric_file <-  paste0("./input_files/MOII_e117/primary_analysis/230426_MOII_e117_fuze/","/mapped/", sample_name,"Chimeric.out.bam")
    output_dir <- paste0("./app/www/igv_snapshot/",sample_name)
    
    if (file.exists(output_dir)){
      setwd(file.path(path))
    } else {
      dir.create(file.path(path, output_dir))
      setwd(file.path(path))
    }
    
    fusions_tab <- createIGVBatchFileFromXlsx(xlsx_file,sample_name)
    
    for(x in 1:nrow(fusions_tab)){
      x=1
      batch_file <- createIGVBatchFile(bam_file = bam_file,
                                       chimeric_file = chimeric_file,
                                       chrom_break_pos = fusions_tab$chrom_break_pos[x],
                                       # snapshot_name = paste(fusions_tab$id[x],fusions_tab$fusion_genes[x],sep="_"),
                                       snapshot_name = paste(sample_name,"_%03d.svg"),
                                       output_dir = output_dir,
                                       batch_file_name = paste(fusions_tab$fusion_genes[x],"temp_batch.txt",sep="_"))
      runIGVSnapshot(batch_file)
    }
  })
  
  # merge all sample tables into one
  files = list.files(pattern = "_fusions_dt.xlsx", full.names = F, all.files = T,recursive = TRUE)
  dt <- rbindlist(lapply(files, fread, header=TRUE))
  fwrite(dt,file = "MOII_all_fusions.xlsx", sep = "\t")
}


args <- commandArgs(trailingOnly = T)
## to test
# args <- c("/mnt/share/share/710000-CEITEC/713000-cmm/713009-slaby/base/sequencing_results/projects/pediatric_oncology_project_fusions/106_fusions/fusion_genes_detection/MV2008fuze_fusions.xlsx",
#           "/mnt/share/share/710000-CEITEC/713000-cmm/713009-slaby/base/sequencing_results/projects/pediatric_oncology_project_fusions/106_fusions/fusion_genes_detection/KL2012fuze_fusions.xlsx",
#           "/mnt/share/share/710000-CEITEC/713000-cmm/713009-slaby/base/sequencing_results/projects/pediatric_oncology_project_fusions/106_fusions/fusion_genes_detection/LN0358fuze_fusions.xlsx")
run_all(args)

