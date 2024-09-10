# app/view/dropdown_button.R

box::use(
  shiny[moduleServer,NS,h3,tags,icon,reactive, req,tagList,observeEvent,div,HTML,fluidRow, column],
  shinyWidgets[dropdown,checkboxGroupButtons,actionBttn,prettyCheckboxGroup, updatePrettyCheckboxGroup],
  bs4Dash[actionButton],
  stats[setNames]
)
box::use(
  app/logic/patients_list[sample_list_fuze,sample_list_som,sample_list_germ],
  # app/logic/patients_list[patient_list],
)

#' @export
colFilterDropdown_ui <- function(id,column_list,default_setting,columnName_map) {
  ns <- NS(id)
  dropdown(
    inputId = ns("colFilter_dropdownButton"),
    label = NULL,
    style = "material-flat",
    color = "blue",
    size = "xs",
    icon = icon("gear"),
    right = TRUE,
    width = "240px",
    
    tagList(
    prettyCheckboxGroup(
      inputId = ns("colFilter_checkBox"),
      label = NULL, 
      choices = setNames(column_list, sapply(column_list, function(x) columnName_map[[x]])),
      selected = default_setting,
      icon = icon("check"), 
      status = "primary",
      outline = FALSE),
    
    tags$hr(),
    
    actionButton(
      inputId = ns("colFilter_all"),
      label = "Show all columns",
      icon = icon("eye")),
    
    actionButton(
      inputId = ns("colFilter_default"),
      label = "Show default columns",
      icon = icon("rotate"))
    )
  )
}

#' @export
colFilterDropdown_server <- function(id, all_columns, default_setting) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression to return the selected columns
    
    observeEvent(input$colFilter_all, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = all_columns)
    })
    
    observeEvent(input$colFilter_default, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = default_setting)
    })
    
    selected_columns <- reactive({
      req(input$colFilter_checkBox)
      # message("Selected columns: ", paste(input$colFilter_checkBox, collapse = ", "))
      input$colFilter_checkBox
    })
    
    
    return(selected_columns)
  })
}



#' @export
igvDropdown_ui <- function(id,patients) {
  ns <- NS(id)

  dropdown(
    inputId = ns("igv_dropdownButton"),
    label = "IGV",
    style = "simple", #"material-flat",
    status = "primary",
    size = "md",
    icon = icon("play"),
    right = TRUE,
    
    tagList(
      tags$style(HTML(sprintf("
        #%s {
          border: none; /* Remove border for this button */
        }
      ", ns("selectGermline_button")))),  # Use ns() to get the correct namespaced ID
      
      checkboxGroupButtons(
        inputId = ns("igvCheckbox_buttons"),
        label = "Select patients: ",
        choices = patients,
        status = "mygrey",
        selected = patients
        ),
      tags$b(" Select variants: "),
      actionButton(
        inputId = ns("selectGermline_button"),
        label = "",
        size = "xs",
        icon = icon("table-cells")),
      tags$br(),
      
      div(style = "display: flex; justify-content: center; margin-top: 10px;",
        actionBttn(
          inputId = ns("go2igv_button"),
          label = "Go to IGV",
          style = "stretch",
          color = "success",
          size = "sm",
          individual = TRUE)
      )
    )
  )
}

#' @export
igvDropdown_server <- function(id, parent_session, selection_enabled) {
  moduleServer(id, function(input, output, session) {
    # Reactive expression to return the selected columns
    selected_patient <- reactive({ input$igvCheckbox_buttons })
    
    # Toggle row selection when the button is clicked
    observeEvent(input$selectGermline_button, {
      selection_enabled(!selection_enabled())  # Toggle the reactive value
    })
    
    return(selected_patient)
  })
}



## dropdown button for patient selection
# ui <- function(id,analysis_type) {
#   ns <- NS(id)
#   dropdown(
#     inputId = ns("dropdown_button"),
#     label = tags$h3("Select patients:", style = "margin: 0;"),
#     style = "material-circle",
#     size = "lg",
#     icon = icon("hospital-user"),
#     right = TRUE,
#     virtualSelectInput(
#       ns("select_patients_panel"),
#       label = "",
#       choices = sample_list_fuze(),
#       selected = sample_list_fuze(),
#       showValueAsTags = TRUE,
#       # search = TRUE,
#       multiple = TRUE
#       # keepAlwaysOpen = TRUE
#       # onServerSearch: onSampleSelectServerSearch,
#       # other options: https://sa-si-dev.github.io/virtual-select/#/properties
#     )
#   )
# }
#
# server <- function(id, session) {
#   moduleServer(id, function(input, output, session) {
#     # Reactive expression to return the selected patients
#     selected_samples <- reactive({
#       input$select_patients_panel
#     })
# 
#     return(selected_samples)
#   })
# }
