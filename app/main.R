# ## install/unistall package from dependencies with commands:
# # rhino::pkg_install("shiny")
# # rhino::pkg_remove("dplyr")
#
# restart R session
# rstudioapi::restartSession()  # Pokud používáš RStudio
#
# ## when .js file added or changed, run this command:
# rhino::build_js()
# #
# ## run this from console when the css style is changed ##
# rhino::build_sass()
#
# ## run shiny app with command:
# # shiny::runApp()
# # shiny::shinyAppDir(".", options = list(launch.browser = TRUE))
#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(paste0(script_dir))
# working with git - checkout to different branch
# git status
# git checkout dev (git checkout -b new_branch)
# git add .
# git commit -m "Uložení aktuálních změn do nové větve"
# git push -u origin dev

box::use(
  rhino,
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,
        br,updateTabsetPanel, actionButton,imageOutput,renderImage,reactiveVal,req,fixedPanel,reactiveValues,wellPanel],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, menuSubItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems,boxDropdown,boxDropdownItem,dropdownDivider,
          navbarMenu,navbarTab],
  htmltools[tags,p,span],
  shinyWidgets[pickerInput,prettySwitch,dropdown],
  shinyjs[useShinyjs, runjs,toggle],
  utils[str],
  #bslib[accordion,accordion_panel],
  shinyBS[bsCollapse,bsCollapsePanel],
  networkD3,
  dplyr[`%>%`],
  circlize,
  webshot,
  ggplot2,
  openxlsx,
  data.table
)

box::use(
  app/prepare_main_table_and_filters[default_col,load_and_prepare,add_library_column,
                            map_column_names,map_gene_region_names,map_clin_sig_names,use_spinner
                            ],
  app/variant_ui_server,
  app/view/IGV
)

# box::use(
#   app/view/summary_2testing,
#   app/view/summary_table,
#   app/view/fusion_genes_table,
#   app/view/germline_var_call_table,
# #   # app/view/variant_calling_table,
#   app/view/expression_profile_table,
#   app/view/expression_profile_plot,
#   app/view/dropdown_button[igvDropdown_ui,igvDropdown_server,colFilterDropdown_ui,colFilterDropdown_server],
#   app/logic/patients_list[patients_list,set_patient_to_sample],
#   app/view/IGV,
# #   app/logic/load_data[load_data,get_inputs],
#   app/logic/prepare_table[colFilter],
#   app/logic/reactable_helpers[columnName_map],
#   app/view/networkGraph_cytoscape,
# 
# 
# )

# plan(multisession)#, workers = detectCores() - 1


#####################################################

## Methylace nedělají na NGS ale na čipech = genom. pozice a k tomu naměřené intenzity. Výsledkem je report. ##

#' @export
ui <- function(id){
  ns <- NS(id)
  useShinyjs()
  tags$head(
    tags$style(HTML("
    #app-plots_tabBox_box {
      box-shadow: none !important;
      border: none !important;
    }
  "))
  )
  

  dashboardPage(
    dark = NULL,
    help = NULL,
    header = dashboardHeader(
      nav = navbarMenu(
        #navbarTab("Summary 2.0 dev", tabName = ns("summary2")),
        navbarTab("Summary", tabName = ns("summary")),
        navbarTab("Network graph", tabName = ns("network_graph")),
        navbarTab("Variant calling", tabName = ns("variant_calling")),
        navbarTab("Expression profile", tabName = ns("expression_profile")),

        navbarTab("Fusion genes", tabName = ns("fusion_genes")),
        navbarTab("Hidden IGV Item", tabName = ns("hidden_igv"))

      )
    ),
    sidebar = dashboardSidebar(disable = TRUE),
          body = dashboardBody(#style = "background-color: white;",
            tabItems(
              tabItem(tabName = ns("summary")
                      ),
              tabItem(tabName = ns("variant_calling"),
                      tabBox(id = ns("variant_calling_tabs"), width = 12, collapsible = FALSE#, title = uiOutput(ns("igv_dropdown_ui")),
                             ,tabPanel("Germline small variant calling",tabName = ns("germline_var_call_panel"),value = "germline"
                              ),
                             tabPanel("Somatic small variant calling",tabName = ns("somatic_var_call_panel"),value = "somatic",
                                      ## this css changes color of selected buttons in IGV dropdown button
                                      tags$style(
                                        HTML(".btn-group > .btn.active {
                                                 background-color: skyblue;
                                                 color: white;
                                               }
                                               .btn-mygrey {
                                                 background-color: lightgray;
                                                 color: black;
                                                 }
                                              ")
                                      ),
                                      variant_ui_server$ui(ns("somatic_var_call_tab"))
                              )
                      )),
              tabItem(tabName = ns("fusion_genes")
                      ),
              tabItem(tabName = ns("expression_profile")
              ),
              tabItem(tabName = ns("network_graph")
              ),
              tabItem(tabName = ns("hidden_igv"),
                      tags$style(HTML("
                              #igv-igvDiv {
                                width: 100%;
                                height: auto;
                                border: none;
                                margin: 0 auto;
                                padding: 20px;
                                box-sizing: border-box;
                              }
                          ")),
                      box(id = ns("igv_page"), title = "IGV Viewer",width = 12, collapsible = FALSE,
                          IGV$igv_ui(ns("igv"))
                      )
              )
            )
          )
   )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    shared_data <- reactiveValues()
    variant_ui_server$server("somatic_var_call_tab",session, shared_data = shared_data)
    IGV$igv_server("igv", shared_data = shared_data)
    
#     shared_data <- reactiveValues(germline_data = reactiveVal(NULL), fusion_data = reactiveVal(NULL), 
#                                   germline_overview = list(), fusion_overview = list())
#   
#     
#     # observeEvent(input$showPlots_switch, {
#     #   toggle("plots_tabBox", condition = input$showPlots_switch)
#     # })
#     # 
# ## run summary module
#     summary_table$summaryServer("summaryUI", session)
#     
#     lapply(patients_list(), function(patient) {
#       summary_2testing$server(paste0("summary_table2_", patient),patient, shared_data)
#     })
# 
# 
#     getColFilterValues <- function(flag,expr_flag) {
#       reactive({
#         colnames_list <- colFilter(flag,expr_flag)
#         list(all_columns = colnames_list$all_columns, default_setting = colnames_list$default_setting)
#       })
#     }
# 
# #################
#   ## filter table columns dropdown button for fusion
#     all_colnames_val_fusion <- getColFilterValues("fusion")
#     output$colFilter_dropdown_ui_fusion <- renderUI({
#       req(all_colnames_val_fusion())
#       colFilterDropdown_ui(ns("colFilter_dropdown_fusion"), all_colnames_val_fusion()$all_columns, all_colnames_val_fusion()$default_setting,columnName_map("fusion"))
#     })
# 
#     ## run fusion genes module
#     samples_fuze <- set_patient_to_sample("fusion")
#     selected_columns_fusion <- colFilterDropdown_server("colFilter_dropdown_fusion", all_colnames_val_fusion()$all_columns, all_colnames_val_fusion()$default_setting)
#     lapply(names(samples_fuze), function(patient) {
#       fusion_genes_table$server(paste0("geneFusion_tab_", patient), samples_fuze[[patient]], selected_columns_fusion, columnName_map("fusion"), shared_data)
#     })
# ##################
# 
# 
#     # filter table columns dropdown button for germline
#     all_colnames_val_germline <- getColFilterValues("germline")
#     output$colFilter_dropdown_ui_germline <- renderUI({
#       req(all_colnames_val_germline())
#       colFilterDropdown_ui(ns("colFilter_dropdown_germ"), all_colnames_val_germline()$all_columns, all_colnames_val_germline()$default_setting, columnName_map("germline"))
#     })
# 
#     ## igv dropdown button for germline
#     selection_enabled <- reactiveVal(FALSE)
# 
#     # IGV dropdown button for germline
#     output$igv_dropdown_ui <- renderUI({
#       igvDropdown_ui(ns("igv_dropdown"), names(set_patient_to_sample("germline")))
#     })
# 
#     # IGV Dropdown server call, pass the reactive value
#     igvDropdown_server("igv_dropdown", session, selection_enabled)
# 
#     # Run germline varcall module
#     samples_germ <- set_patient_to_sample("germline")
#     selected_columns_germ <- colFilterDropdown_server("colFilter_dropdown_germ", all_colnames_val_germline()$all_columns, all_colnames_val_germline()$default_setting)
# 
#     lapply(names(samples_germ), function(patient) {
#       germline_var_call_table$server(paste0("germline_tab_", patient), samples_germ[[patient]], selected_columns_germ, columnName_map("germline"), selection_enabled, shared_data)
#     })
# 
# ##################
#     
#     samples_expr <- set_patient_to_sample("expression")
#     
#     # Reaktivní value for watching which panel is active
#     expr_flag <- reactive({
#       patients <- names(samples_expr)
# 
#       for (patient in patients) {
#         panel_id <- paste0("expression_profile_tabs_", patient)
#         
#         if (!is.null(input[[panel_id]])) {
#           if (input[[panel_id]] == "allGenes") {
#             return("all_genes")
#           } else if (input[[panel_id]] == "genesOfinterest") {
#             return("genes_of_interest")}}
#       }
#       return(NULL)
#     })
#     
#     all_colnames_val_expression <- reactive({
#       req(expr_flag())
#       return(getColFilterValues("expression", expr_flag())())
#     })
#     
#     output$colFilter_dropdown_ui_expression <- renderUI({
#       req(all_colnames_val_expression())
#       colFilterDropdown_ui(ns("colFilter_dropdown_expression"), all_colnames_val_expression()$all_columns, all_colnames_val_expression()$default_setting,columnName_map("expression",expr_flag(),all_colnames_val_expression()$all_columns)$dropdown_btn)
#       })
#     
#     observe({
#       req(all_colnames_val_expression())
#       selected_columns_expression <- colFilterDropdown_server("colFilter_dropdown_expression", all_colnames_val_expression()$all_columns, all_colnames_val_expression()$default_setting)
# 
#       lapply(names(samples_expr), function(patient) {
#         expression_profile_table$server_allGenes(paste0("allGenes_tab_", patient), samples_expr[[patient]],selected_columns_expression, columnName_map("expression",expr_flag(),all_colnames_val_expression()$all_columns),all_colnames_val_expression())
#         expression_profile_table$server_genesOfInterest(paste0("genesOfinterest_tab_", patient), samples_expr[[patient]],selected_columns_expression, columnName_map("expression",expr_flag(),all_colnames_val_expression()$all_columns),all_colnames_val_expression())
#         })
#     })
# 
# 
#     lapply(names(samples_expr), function(patient) {
#       expression_profile_plot$server(paste0("allGenes_plots_", patient), patient,"all_genes")
#       expression_profile_plot$server(paste0("genesOfinterest_plots_", patient), patient,"genes_of_interest")
#     })
#       
# 
#     
#    
#       
#       
#       
#       
#     
# 
# 
# ##################    
#     ## run network graph module    
#     
#     networkGraph_cytoscape$server("network_graph", shared_data)
#     
#     
# 
# 
#     
# 
# 
# 
# 
# 
# 

  
  
  # piechart_server("pieChart")
  # fusion_genes_table$piechart_server("pieChart")
  


  ##### filtering table for all patients together #####
  ##
  # selected_samples <- dropdown_button$server("select_samples")
  # fusion_genes_table$table_server("geneFusion_tab",selected_samples)
  #####################################################
  })
}

#shinyApp(ui,server)#,options = list(launch.browser = TRUE))
