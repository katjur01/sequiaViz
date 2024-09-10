# ## install/unistall package from dependencies with commands:
# # rhino::pkg_install("shiny")
# # rhino::pkg_remove("dplyr")
#
# ## when .js file added or changed, run this command:
# # rhino::build_js()
#
# ## run this from console when the css style is changed ##
# # rhino::build_sass()
#
# ## run shiny app with command:
# # shiny::runApp("./app/main.R")
#
box::use(
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,
        fluidRow,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,br,updateTabsetPanel, actionButton],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems],
  plotly[renderPlotly, plot_ly,plotlyOutput]
)

box::use(
  app/view/fusion_genes_table,
  app/view/variant_calling_table,
  app/view/dropdown_button,
  app/logic/load_data[get_inputs]
)

# ui <- dashboardPage(
#         dashboardHeader(""),
#         dashboardSidebar(
#           sidebarMenu(id = "tabs",
#                       menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard")),
#                       menuItem("Menu Item 1", tabName = "two", icon = icon("th"))
#           )
#         ),
#         dashboardBody(
#           tabItems(
#             tabItem(tabName = "one",h2("Dashboard tab content"),actionButton('switchtab', 'Switch tab')),
#             tabItem(tabName = "two",h2("Widgets tab content"))
#           )
#         )
#       )
#
# server <- function(input, output, session) {
#
#       observeEvent(input$switchtab, {
#         newtab <- switch(input$tabs, "one" = "two","two" = "one")
#         updateTabItems(session, "tabs", newtab)
#       })
# }
#
# shinyApp(ui = ui, server = server)
#
#
# ui <- dashboardPage(
#   dashboardHeader(""),
#   dashboardSidebar(
#     sidebarMenu(id = "tabs",
#                 menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard")),
#                 menuItem("Menu Item 2", tabName = "two", icon = icon("th"))
#     )
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "one",h2("Dashboard tab content"),actionButton('switchtab', 'Switch tab')),
#       tabItem(tabName = "two",h2("Widgets tab content"),
#               fluidRow(
#                 column(12,
#                        fusion_genes_table$ui("fusion_genes_tab"))
#               ))
#     )
#   )
# )
#
# server <- function(input, output, session) {
#       ### get paths to input files
#       filenames <- get_inputs()
#       fusion_genes_filenames <- filenames$fusions
#       somatic_variant_calling_filenames <- filenames$var_call.somatic
#       germline_variant_calling_filenames <- filenames$var_call.germline
#       structural_variant_calling_filenames <- filenames$var_call.structural
#
#
#       selected_samples <- dropdown_button$server("select_samples")
#       fusion_genes_table$server("fusion_genes_tab", data = fusion_genes_filenames, selected_samples)
#
#
#
#   observeEvent(input$switchtab, {
#     newtab <- switch(input$tabs, "one" = "two","two" = "one")
#     updateTabItems(session, "tabs", newtab)
#   })
# }
#
# shinyApp(ui,server)

# app_ui <- function() {
#   ui("module1")
# }
#
# # Main app server
# app_server <- function(input, output, session) {
#   server("module1")
# }
#
# shinyApp(ui = app_ui, server = app_server)
#

#' @export
ui <- function(id) {
  ns <- NS(id)

  #   dashboardPage(
  #     dashboardHeader(""),
  #     dashboardSidebar(
  #       sidebarMenu(id = "tabs",
  #                   menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard")),
  #                   menuItem("Menu Item 1", tabName = "two", icon = icon("th"))
  #       )
  #     ),
  #     dashboardBody(
  #       tabItems(
  #         tabItem(tabName = "one",h2("Dashboard tab content"),actionButton('switchtab', 'Switch tab')),
  #         tabItem(tabName = "two",h2("Widgets tab content"))
  #       )
  #     )
  #   )
  # #
  dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar(
      id = ns("sidebar"), collapsed = TRUE,
      h3( "MOII_e_117krve", style = "font-size: 20px; padding: 10px; color: #FFFFFF; "),
      bs4Dash::sidebarMenu(id = "sidebar_menu",
                           menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard")),
                           menuItem("Menu Item 1", tabName = "two", icon = icon("th")),
                           bs4Dash::menuItem("Summary",tabName = ns("summary"),icon = icon("id-card-clip"),newTab = TRUE),
                           bs4Dash::menuItem("Variant calling", tabName = ns("variant_calling"), icon = icon("dna"),newTab = TRUE),
                           bs4Dash::menuItem("Fusion genes", tabName = ns("fusion_genes"), icon = icon("atom"),newTab = TRUE),
                           bs4Dash::menuItem("Expression profile", tabName = ns("expression_profile"), icon = icon("chart-line"),newTab = TRUE),
                           bs4Dash::menuItem("Network graph", tabName = ns("network_graph"), icon = icon("diagram-project"),newTab = TRUE) # or icon("circle-nodes")
      )
    ),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "one",h2("Dashboard tab content"),actionButton('switchtab', 'Switch tab')),
        tabItem(tabName = "two",h2("Widgets tab content")),
        tabItem(h3("SUMMARY"),tabName = ns("summary"),
                fluidRow(
                  infoBox("Variant calling", subtitle = "3/4 patients",tabName = ns("variant_calling"), icon = icon("dna")),
                  infoBox("Fusion genes", subtitle = "4/4 patients", tabName = ns("fusion_genes"), icon = icon("atom")),
                  infoBox("Expression profile", subtitle = "1/4 patients", tabName = ns("expression_profile"), icon = icon("chart-line")),
                ),
                fluidRow(
                  bs4Card(title = "Patient: DZ1601", icon=icon("person"),status = "primary",
                          box(title = "Variant calling", width = 12, icon = icon("dna"),
                              fluidRow(
                                column(11,descriptionBlock("Dashboard frekvence variant v Gnomad, Cosmic,atd.",rightBorder = FALSE)),
                                column(1, actionButton(inputId = ns("switch_button"),label = NULL,icon=icon("eye")))
                                # actionButton(ns("summary_varcall_pat2"),label = NULL,icon("eye"),class = "btn btn-primary btn-sm"),
                              )
                              # ,plotlyOutput(ns("pieChart"))
                          ),
                          box(title = "Fusion genes", width = 12, icon = icon("atom"),
                              fluidRow(
                                column(11,descriptionBlock("Dashboard zastoupení v Gnomad, Cosmic,atd.",rightBorder = FALSE)),
                                column(1, actionButton(ns("summary_fusions_pat1"),label = NULL,icon("eye"),class = "btn btn-primary btn-sm", size= "lg"))
                              )
                              # ,plotlyOutput(ns("barPlot"))
                          )
                  ),
                  bs4Card(title = "Patient: LK0302", status = "primary", icon=icon("person"),
                          br(),
                          boxProfile(title = "Variant calling",
                                     bordered = FALSE,
                                     boxProfileItem(
                                       title = "Dashboard",
                                       description = "Dashboard frekvence variant v Gnomad, Cosmic,atd."
                                     )
                          ),
                          br(),
                          boxProfile(title = "Fusion genes",
                                     bordered = FALSE,
                                     boxProfileItem(
                                       title = "Dashboard",
                                       description = "Dashboard list databází Gnomad, Cosmic,atd."
                                     )
                          )
                  )

                )
        ),
        tabItem(tabName = ns("variant_calling"),h3("Variant calling analysis"),
                tabsetPanel(id = ns( "variant_calling_tabs"),
                            tabPanel("Somatic small variant calling",tabName = ns("somatic_var_call_panel"),value = "somatic",
                                     variant_calling_table$ui(ns("somatic_var_call_tab"))
                            ),
                            tabPanel("Germline small variant calling",tabName = ns("germline_var_call_panel"),value = "germline",
                                     h2("Normally, germline results are here :)"),
                                     variant_calling_table$ui(ns("germline_var_call_tab"))
                            ),
                            tabPanel("Structural variant calling",tabName = ns("structural_var_call_panel"),value = "struct",
                                     h2("Nothing here yet"),
                                     # variant_calling_table$ui(ns("structural_var_call_tab"))
                            )
                )

        ),
        tabItem(tabName = ns("fusion_genes"),
                fluidRow(
                  column(12,
                         div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
                             h3("Fusion Genes analysis", style = "margin: 0;"),
                             dropdown_button$ui(ns("select_samples"))))
                ),
                fluidRow(
                  column(12,
                         fusion_genes_table$ui(ns("fusion_genes_tab")))
                )
        ),
        tabItem("One day, here will be EXPRESION PROFILE tab",tabName = ns("expression_profile")
        ),
        tabItem(h1("Network graph"),tabName = ns("network_graph")
        )
      )
    )
  )
}


#' @export
server <- function(id) {

  moduleServer(id, function(input, output, session) {


    ### get paths to input files
    filenames <- get_inputs()
    somatic_variant_calling_filenames <- filenames$var_call.somatic
    germline_variant_calling_filenames <- filenames$var_call.germline
    structural_variant_calling_filenames <- filenames$var_call.structural
    fusion_genes_filenames <- filenames$fusions


    currentTab <- reactive({
      print("current tab panel:")
      print(input$variant_calling_tabs)
    })


    if(file.exists(somatic_variant_calling_filenames)){
      selected_samples <- dropdown_button$server("select_samples")
      variant_calling_table$server("somatic_var_call_tab", data = somatic_variant_calling_filenames, selected_samples)
    }
    if(file.exists(germline_variant_calling_filenames)){
      selected_samples <- dropdown_button$server("select_samples")
      variant_calling_table$server("germline_var_call_tab", data = germline_variant_calling_filenames, selected_samples)
      # variant_calling_table$server("variant_calling_tabs", data = germline_variant_calling_filenames)
    }
    if(isTruthy(file.exists(structural_variant_calling_filenames))){
      selected_samples <- dropdown_button$server("select_samples")
      variant_calling_table$server("structural_var_call_tab", data = structural_variant_calling_filenames, selected_samples)
      # variant_calling_table$server("variant_calling_tabs", data = structural_variant_calling_filenames)
    }
    if(file.exists(fusion_genes_filenames)[1]){
      selected_samples <- dropdown_button$server("select_samples")
      fusion_genes_table$server("fusion_genes_tab", data = fusion_genes_filenames, selected_samples)
    }
    #
    #
    # output$pieChart <- renderPlotly({
    #   plot_ly(
    #     labels = c("Pathogenic", "Likely Pathogenic", "Uncertain Significance", "Benign"),
    #     values = c(25, 35, 12, 4),
    #     type = "pie"
    #   )
    # })
    #
    # observeEvent(
    #   input$summary_varcall_pat1,{
    #     updateTabsetPanel(session,inputId = "variant_calling_tabs",selected = "somatic_var_call_panel")
    #     message(paste0(format(Sys.time(), format="%H:%M:%S"), "Action button"))
    #   }
    # )
    #
    # observeEvent(input$switch_button, {
    #   newtab <- switch(input$sidebar, "summary" = "variant_calling","variant_calling" = "summary")
    #   updateTabItems(session, "sidebar", newtab)
    # })
    #
    #
    # # observeEvent(input$summary_varcall_pat1, {
    # #   updateTabItems(session, "sidebar_menu", selected = "variant_calling")
    # #   updateTabsetPanel(session, inputId = "variant_calling_tabs", selected = "somatic_var_call_panel")
    # #   message(paste0(format(Sys.time(), format = "%H:%M:%S"), " Action button"))
    # # })
    #
    #
    # output$barPlot <- renderPlotly({
    #   plot_ly(
    #     x = c("GnomAD", "ClinVar", "COSMIC", "Chromothripsis", "ChimerSeq"),
    #     y = c(150, 100, 120 ,80, 60),
    #     type = "bar"
    #   )
    # })

  })
  # observeEvent(input$switch_button, {
  #   newtab <- switch(input$sidebar, "summary" = "variant_calling","variant_calling" = "summary")
  #   updateTabItems(session, "sidebar", newtab)
  # })
  # observeEvent(input$switchtab, {
  #   newtab <- switch(input$sidebar_menu, "one" = "two","two" = "one")
  #   updateTabItems(session, "sidebar_menu", newtab)
  # })
}


# shinyApp(
#   ui = tagList(
#     uiApp("ui")
#   ),
#   server = function(input, output, session) {
#     serverApp("ui")
#   }
# )



# ui <- tagList(
#   uiApp("ui")
# )
#
# server <- function(input, output, session) {
#   serverApp("ui")
# }






# # ## install/unistall package from dependencies with commands:
# # # rhino::pkg_install("shiny")
# # # rhino::pkg_remove("dplyr")
# #
# # ## when .js file added or changed, run this command:
# # # rhino::build_js()
# #
# # ## run this from console when the css style is changed ##
# # # rhino::build_sass()
# #
# # ## run shiny app with command:
# # # shiny::runApp("./app/main.R")
# #
# box::use(
#   shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,
#         fluidRow,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,br,updateTabsetPanel, actionButton],
#   bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
#           controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems],
#   plotly[renderPlotly, plot_ly,plotlyOutput]
# )
#
# box::use(
#   app/view/fusion_genes_table,
#   app/view/variant_calling_table,
#   app/view/dropdown_button,
#   app/logic/load_data[get_inputs]
# )
#
#
#
#
#
# # dashboardPage(
# #   dashboardHeader(""),
# #   dashboardSidebar(
# #     sidebarMenu(id = "tabs",
# #                 menuItem("Menu Item 1", tabName = "one", icon = icon("dashboard")),
# #                 menuItem("Menu Item 1", tabName = "two", icon = icon("th"))
# #     )
# #   ),
# #   dashboardBody(
# #     tabItems(
# #       tabItem(tabName = "one",h2("Dashboard tab content"),actionButton('switchtab', 'Switch tab')),
# #       tabItem(tabName = "two",h2("Widgets tab content"))
# #     )
# #   )
# # )
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(
#     id =  "sidebar", collapsed = TRUE,
#     h3( "MOII_e_117krve", style = "font-size: 20px; padding: 10px; color: #FFFFFF; "),
#     bs4Dash::sidebarMenu(id = "sidebar_menu",
#                          bs4Dash::menuItem("Summary",tabName = "summary",icon = icon("id-card-clip"),newTab = TRUE),
#                          bs4Dash::menuItem("Variant calling", tabName = "variant_calling", icon = icon("dna"),newTab = TRUE)
#                          # ,
#                          # bs4Dash::menuItem("Fusion genes", tabName = "fusion_genes", icon = icon("atom"),newTab = TRUE),
#                          # bs4Dash::menuItem("Expression profile", tabName = "expression_profile", icon = icon("chart-line"),newTab = TRUE),
#                          # bs4Dash::menuItem("Network graph", tabName = "network_graph", icon = icon("diagram-project"),newTab = TRUE) # or icon("circle-nodes")
#     )
#   ),
#   body = dashboardBody(
#     tabItems(
#       tabItem(h3("SUMMARY"),tabName = "summary"
#               # ,
#               # fluidRow(
#               #   infoBox("Variant calling", subtitle = "3/4 patients",tabName = "variant_calling", icon = icon("dna")),
#               #   infoBox("Fusion genes", subtitle = "4/4 patients", tabName = "fusion_genes", icon = icon("atom")),
#               #   infoBox("Expression profile", subtitle = "1/4 patients", tabName = "expression_profile", icon = icon("chart-line")),
#               # ),
#               # fluidRow(
#               #   bs4Card(title = "Patient: DZ1601", icon=icon("person"),status = "primary",
#               #           box(title = "Variant calling", width = 12, icon = icon("dna"),
#               #               fluidRow(
#               #                 column(11,descriptionBlock("Dashboard frekvence variant v Gnomad, Cosmic,atd.",rightBorder = FALSE)),
#               #                 column(1, actionButton(inputId = "switch_button",label = NULL,icon=icon("eye")))
#               #                 # actionButton("summary_varcall_pat2",label = NULL,icon("eye"),class = "btn btn-primary btn-sm"),
#               #                 )
#               #               # ,plotlyOutput("pieChart")
#               #           ),
#               #           box(title = "Fusion genes", width = 12, icon = icon("atom"),
#               #               fluidRow(
#               #                 column(11,descriptionBlock("Dashboard zastoupení v Gnomad, Cosmic,atd.",rightBorder = FALSE)),
#               #                 column(1, actionButton("summary_fusions_pat1",label = NULL,icon("eye"),class = "btn btn-primary btn-sm", size= "lg"))
#               #               )
#               #               # ,plotlyOutput("barPlot")
#               #           )
#               #   ),
#               #   bs4Card(title = "Patient: LK0302", status = "primary", icon=icon("person"),
#               #           br(),
#               #           boxProfile(title = "Variant calling",
#               #                      bordered = FALSE,
#               #                      boxProfileItem(
#               #                        title = "Dashboard",
#               #                        description = "Dashboard frekvence variant v Gnomad, Cosmic,atd."
#               #                      )
#               #           ),
#               #           br(),
#               #           boxProfile(title = "Fusion genes",
#               #                      bordered = FALSE,
#               #                      boxProfileItem(
#               #                        title = "Dashboard",
#               #                        description = "Dashboard list databází Gnomad, Cosmic,atd."
#               #                      )
#               #           )
#               #   )
#               #
#       )
#     ),
#     tabItem(tabName = "variant_calling",h3("Variant calling analysis"),
#             tabsetPanel(id = "variant_calling_tabs",
#                         tabPanel("Somatic small variant calling",tabName = "somatic_var_call_panel",value = "somatic"
#                                  # ,
#                                  # variant_calling_table$ui("somatic_var_call_tab")
#                         ),
#                         tabPanel("Germline small variant calling",tabName = "germline_var_call_panel",value = "germline",
#                                  h2("Normally, germline results are here :)")
#                                  # ,
#                                  # variant_calling_table$ui("germline_var_call_tab")
#                         ),
#                         tabPanel("Structural variant calling",tabName = "structural_var_call_panel",value = "struct",
#                                  h2("Nothing here yet")
#                                  # ,
#                                  # variant_calling_table$ui("structural_var_call_tab")
#                         )
#             )
#
#     )
#     # ,
#     # tabItem(tabName = "fusion_genes",
#     #         fluidRow(
#     #           column(12,
#     #                  div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px;",
#     #                      h3("Fusion Genes analysis", style = "margin: 0;"),
#     #                      dropdown_button$ui("select_samples")))
#     #         ),
#     #         fluidRow(
#     #           column(12,
#     #                  fusion_genes_table$ui("fusion_genes_tab"))
#     #         )
#     # ),
#     # tabItem("One day, here will be EXPRESION PROFILE tab",tabName = "expression_profile"
#     # ),
#     # tabItem(h1("Network graph"),tabName = "network_graph"
#     # )
#     # )
#   )
# )
#
#
# server <- function(input, output, session) {
#   # server <- function(id) {
#
#   # # moduleServer(id, function(input, output, session) {
#   #   observeEvent(input$switchtab, {
#   #     newtab <- switch(input$tabs, "one" = "two","two" = "one")
#   #     updateTabItems(session, "tabs", newtab)
#   #   })
#
#   ### get paths to input files
#   filenames <- get_inputs()
#   print(filenames)
#   somatic_variant_calling_filenames <- filenames$var_call.somatic
#   germline_variant_calling_filenames <- filenames$var_call.germline
#   structural_variant_calling_filenames <- filenames$var_call.structural
#   fusion_genes_filenames <- filenames$fusions
#
#
#   # currentTab <- reactive({
#   #   print("current tab panel:")
#   #   print(input$variant_calling_tabs)
#   # })
#
#
#   # if(file.exists(somatic_variant_calling_filenames)){
#   #   selected_samples <- dropdown_button$server("select_samples")
#   #   variant_calling_table$server("somatic_var_call_tab", data = somatic_variant_calling_filenames, selected_samples)
#   # }
#   # if(file.exists(germline_variant_calling_filenames)){
#   #   selected_samples <- dropdown_button$server("select_samples")
#   #   variant_calling_table$server("germline_var_call_tab", data = germline_variant_calling_filenames, selected_samples)
#   #   # variant_calling_table$server("variant_calling_tabs", data = germline_variant_calling_filenames)
#   # }
#   # if(isTruthy(file.exists(structural_variant_calling_filenames))){
#   #   selected_samples <- dropdown_button$server("select_samples")
#   #   variant_calling_table$server("structural_var_call_tab", data = structural_variant_calling_filenames, selected_samples)
#   #   # variant_calling_table$server("variant_calling_tabs", data = structural_variant_calling_filenames)
#   # }
#   # if(file.exists(fusion_genes_filenames)[1]){
#   #   selected_samples <- dropdown_button$server("select_samples")
#   #   fusion_genes_table$server("fusion_genes_tab", data = fusion_genes_filenames, selected_samples)
#   # }
#
#
#   # output$pieChart <- renderPlotly({
#   #   plot_ly(
#   #     labels = c("Pathogenic", "Likely Pathogenic", "Uncertain Significance", "Benign"),
#   #     values = c(25, 35, 12, 4),
#   #     type = "pie"
#   #   )
#   # })
#
#   # observeEvent(
#   #   input$summary_varcall_pat1,{
#   #     updateTabsetPanel(session,inputId = "variant_calling_tabs",selected = "somatic_var_call_panel")
#   #     message(paste0(format(Sys.time(), format="%H:%M:%S"), "Action button"))
#   #   }
#   # )
#
#   # observeEvent(input$switch_button, {
#   #   newtab <- switch(input$sidebar, "summary" = "variant_calling","variant_calling" = "summary")
#   #   updateTabItems(session, "sidebar", newtab)
#   # })
#
#
#   # observeEvent(input$summary_varcall_pat1, {
#   #   updateTabItems(session, "sidebar_menu", selected = "variant_calling")
#   #   updateTabsetPanel(session, inputId = "variant_calling_tabs", selected = "somatic_var_call_panel")
#   #   message(paste0(format(Sys.time(), format = "%H:%M:%S"), " Action button"))
#   # })
#
#
#   # output$barPlot <- renderPlotly({
#   #   plot_ly(
#   #     x = c("GnomAD", "ClinVar", "COSMIC", "Chromothripsis", "ChimerSeq"),
#   #     y = c(150, 100, 120 ,80, 60),
#   #     type = "bar"
#   #   )
#   # })
#
#   # })
# }
#
#
# shinyApp(ui, server)


