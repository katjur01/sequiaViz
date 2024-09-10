# app/logic/waiters.R

box::use(
  # shiny[tagList],
  # waiter[useWaiter,Waiter,spin_fading_circles, spin_5]
  shiny,
  shinycssloaders[withSpinner],
)

#' @export
use_spinner <- function(ui_element){
  spinner <- withSpinner(ui_element,type=3,color = "#060606",color.background = "#EEEEEE" )
  return(spinner)
}




# #' @export
# get_spin_waiter <- function(id) {
#   waiter <- Waiter$new(id = id, html = tagList(spin_5()))
#   return(waiter)
# }

