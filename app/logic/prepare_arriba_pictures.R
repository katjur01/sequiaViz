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
      system2("pdf2svg", args = c(input_pdf, output_svg, "all"))
      print("Creating arriba pictures...")
    } else {
      print("Arriba svg picture are already prepared. Nothing to do here.")
    }
  }
}

# pdf2png(input_pdf,folder_path,output_svg)


