# app/view/variant_calling_table.R

#########################################################################################################
## pozn. funguje pokud je jedna tabulka načtena díky reactable a druhá pomocí reactable.extras.
## Pokud se pokusím nahrát obě tabulky (somatic a germline) pomocí reactable.extras, nezobrazuje se nic.
## Díky reactable.extras by se mělo načítat jen pár údajů nikoli tabulka celá, což asi není úplně pravda,
## protože gemline tabulka se načítá pěkně dlouho.
#########################################################################################################

box::use(
  shiny[moduleServer,NS,h3,tagList,div,tabsetPanel,tabPanel,observeEvent,fluidPage,reactive,icon],
  bs4Dash[dashboardPage,dashboardHeader,dashboardSidebar,actionButton],
  reactable,
  reactable[colDef],
  # reactable.extras[reactable_extras_ui,reactable_extras_server],
  htmltools,
)

box::use(
  app/logic/load_data[load_data],
  app/logic/prepare_table[prepare_variant_calling_table, default_sorted_table],
  app/logic/waiters[use_spinner]
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  if(id == "app-somatic_var_call_tab"){
    use_spinner(reactable$reactableOutput(ns("somatic_var_call_tab")))
  } else if (id == "app-germline_var_call_tab"){
    use_spinner(reactable$reactableOutput(ns("germline_var_call_tab")))
  } else if(id == "app-structural_var_call_tab"){
    print(id)
    use_spinner(reactable$reactableOutput(ns("structural_var_call_tab")))
  } else {
    h3("Wrong input in app/view/variant_calling_table.R, it's not somatic or gemline or stuctural data table.")
  }
}


#' @export
server <- function(id, data, selected_samples) {
  # ns <- NS(id)
  moduleServer(id, function(input, output, session) {

    react_table <- reactable$reactable(
      prepare_variant_calling_table(load_data(data),selected_samples()),
      filterable = TRUE,
      resizable = TRUE,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(10,20,50,100),
      defaultPageSize = 20,
      striped = TRUE,
      wrap = FALSE,
      highlight = TRUE,
      outlined = TRUE,
      defaultColDef = colDef(
        header = function(value) gsub("_", " ", value, fixed = TRUE),
        align = "center",
        sortNALast = TRUE
      ),
      defaultSorted = default_sorted_table(load_data(data)),
      columns = list(
        Visual_Check = colDef(
          filterable = FALSE,
          cell = function(value, index){
            tagList(
              actionButton(paste("yesButton", index, sep = "_"), icon("check"), class = "btn btn-primary btn-sm"),
              actionButton(paste("noButton", index, sep = "_"), icon("close"), class = "btn btn-primary btn-sm")
            )
          }
        )
      )
    )

    if(grepl("somatic",data[1])){
      message("iam rendering somatic table only")
      output$somatic_var_call_tab  <- reactable$renderReactable({ react_table })
    }
    else if(grepl("germline",data[1])){
      message("iam rendering germline table only")
      output$germline_var_call_tab <- reactable$renderReactable({ react_table })

    } else {
      message("This is not somatic nor germline data input")
      output$structural_var_call_tab <- reactable$renderReactable({ react_table })
    }
    # # reactable_extras_server(ns("somatic_var_call_tab"),prepare_variant_calling_table(load_data(data)))
    # # reactable_extras_server(id = ns("germline_var_call_tab"),data = prepare_variant_calling_table(load_data(data)))
  })
}
