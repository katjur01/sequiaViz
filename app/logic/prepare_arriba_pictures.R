# app/logic/prepare_arriba_pictures.R

####################################################
# converting arriba's .pdf pictures to .svg format #
####################################################

#' @export
pdf2png <- function(input_pdf,folder_path,output_svg){
  
  if (!dir.exists(folder_path)) {
    dir.create(folder_path) # Pokud složka neexistuje, vytvoř ji

    system2("pdf2svg", args = c(input_pdf, output_svg, "all"))
    print("Creating arriba pictures...")
  } else { 
    if (length(list.files(folder_path)) == 0) { # Pokud složka existuje, zkontroluj, jestli je prázdná
      print("Folder exists but there are no arriba pictures here.")
      # system2("pdf2svg", args = c(input_pdf, output_svg, "all"))
      res <- system2("pdf2svg", args = c(input_pdf, output_svg, "all"), stdout = TRUE, stderr = TRUE)
      print(res)
      print("Creating arriba pictures...")
    } else {
      print("Arriba svg picture are already prepared. Nothing to do here.")
    }
  }
}
# 
# sample_name <- "NT1162fuze"
# input_pdf <- path.expand(paste0("~/Desktop/sequiaViz/input_files/reanalysed_data/fusions/results/",sample_name,"/arriba/",sample_name,".arriba_fusion_viz.pdf"))
# folder_path <- path.expand(paste0("~/Desktop/sequiaViz/www/arriba_viz/",sample_name))
# output_svg <- file.path(folder_path, paste0(sample_name, "_%03d.svg"))
# 
# 
# pdf2png(input_pdf,folder_path,output_svg)


