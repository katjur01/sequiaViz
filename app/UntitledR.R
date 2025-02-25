dictionary <- fread("sample_name_list.tsv")
formatted_n <- lapply(dictionary[,1],function(x) sprintf("%03d", as.integer(x)))
dictionary[,protein_name := lapply(formatted_n,function(x) paste0("P_",x))]

ens_prot_table <- fread("ens_prot_table.tsv")
ens_prot_table_unique <- unique(ens_prot_table[,.(PG.ProteinAccessions = uniprotswissprot,ensembl_gene_id)],by = "PG.ProteinAccessions")
ens_prot_table_unique <- unique(ens_prot_table_unique,by = "ensembl_gene_id")

#rna tpm   

RNA_tpm <- fread("final_tab_globalimputing_WO_P037_and_P079.csv")

setnames(RNA_tpm,"patient_prot","sample")
RNA_tpm[,patient_rna := NULL]
RNA_tpm <- merge(ens_prot_table_unique[,.(protein_name = PG.ProteinAccessions,gene_id = ensembl_gene_id)],RNA_tpm,by = "protein_name")
RNA_tpm <- unique(RNA_tpm,by = c("gene_id","sample"))

setnames(RNA_tpm,"count_rna","RNA_raw")
setnames(RNA_tpm,"count_prot","prot_raw")

RNA_tpm[,RNA_log := log2(RNA_raw + 32)]
RNA_tpm[,prot_log := log2(prot_raw)]
RNA_tpm[,RNA_fc := RNA_log - mean(RNA_log), .(gene_id)]
RNA_tpm[,prot_fc := prot_log - mean(prot_log), .(gene_id)]


