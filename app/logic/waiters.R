# app/logic/waiters.R

box::use(
  shinycssloaders[withSpinner],
)

#' @export
use_spinner <- function(ui_element){
  spinner <- withSpinner(ui_element,type=3,color = "#060606",color.background = "#EEEEEE" )
  return(spinner)
}
