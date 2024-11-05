# ## install/unistall package from dependencies with commands:
# # rhino::pkg_install("shiny")
# # rhino::pkg_remove("dplyr")
#
# ## when .js file added or changed, run this command:
# # rhino::build_js()
# #
# ## run this from console when the css style is changed ##
# # rhino::build_sass()
#
# ## run shiny app with command:
# # shiny::runApp()
# # shiny::shinyAppDir(".")
#
# script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
# setwd(paste0(script_dir))

box::use(
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,
        br,updateTabsetPanel, actionButton,imageOutput,renderImage,reactiveVal,req,fixedPanel],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, menuSubItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems],
  # plotly[plot_ly,plotlyOutput,renderPlotly,layout],
  # reactable,
  # reactable[colDef],
  htmltools[tags]
  # data.table
  # openxlsx[read.xlsx]
)

box::use(
  app/view/summary_table,
  app/view/fusion_genes_table,
  app/view/germline_var_call_table,
#   # app/view/variant_calling_table,
  app/view/expression_profile_table,
  app/view/dropdown_button[igvDropdown_ui,igvDropdown_server,colFilterDropdown_ui,colFilterDropdown_server],
  app/logic/patients_list[patients_list,set_patient_to_sample],
  app/view/IGV,
#   app/logic/load_data[load_data,get_inputs],
  app/logic/prepare_table[colFilter,columnName_map],
  app/view/networkGraph_cytoscape
#   
)

#####################################################

## Methylace nedělají na NGS ale na čipech = genom. pozice a k tomu naměřené intenzity. Výsledkem je report. ##

#' @export
ui <- function(id){
  ns <- NS(id)
  
  dashboardPage(
          header = dashboardHeader(),
          sidebar = dashboardSidebar( id = ns("sidebar"), collapsed = TRUE,
            h3( "MOII_e_117krve", style = "font-size: 20px; padding: 10px; color: #FFFFFF; "),
            sidebarMenu(id = ns("sidebar_menu"),
                        menuItem("Network graph", tabName = ns("network_graph"), icon = icon("diagram-project")),
                        menuItem("Expression profile", tabName = ns("expression_profile"), icon = icon("chart-line")),
                        menuItem("Variant calling", tabName = ns("variant_calling"), icon = icon("dna")),

                        menuItem("Fusion genes", tabName = ns("fusion_genes"), icon = icon("atom")),
                        menuItem("Hidden IGV Item", tabName = ns("hidden_igv"), icon = icon("eye-slash")),
                        menuItem("Summary",tabName = ns("summary"),icon = icon("id-card-clip"))
                  
                  )),
          body = dashboardBody(
            tabItems(
              tabItem(h3("SUMMARY"),tabName = ns("summary"),
                      fluidRow(
                          summary_table$summaryUI(ns("summaryUI"))
                          # do.call(tagList, lapply(patients_list(), function(patient) {
                          #   summary_table$summaryUI(paste0("summaryUI_", patient), patient)
                          # }))
                          )
                      ),
              tabItem(tabName = ns("variant_calling"),
                      tabBox(id = ns("variant_calling_tabs"), width = 12, collapsible = FALSE, title = uiOutput(ns("igv_dropdown_ui")),
                             tabPanel("Germline small variant calling",tabName = ns("germline_var_call_panel"),value = "germline",
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
                                      fluidPage(
                                          div(style = "width: 2.8%; position: absolute; right: 0; margin-top: 13.5px;",
                                              uiOutput(ns("colFilter_dropdown_ui_germline")))),
                                      do.call(tabsetPanel, c(id = ns("germline_patients"), lapply(names(set_patient_to_sample("germline")), function(sample) {
                                        tabPanel(title = sample, germline_var_call_table$ui(ns(paste0("germline_tab_", sample))))
                                      })))
                              ),
                             tabPanel("Somatic small variant calling",tabName = ns("somatic_var_call_panel"),value = "somatic",
                                      h2("Somatic variant calling results")
                                      # variant_calling_table$ui(ns("somatic_var_call_tab"))
                              )
                      )),
              tabItem(tabName = ns("fusion_genes"),
                      bs4Card(width = 12,headerBorder = FALSE, collapsible = FALSE,
                        fluidPage(
                          div(style = "width: 2.8%; position: absolute; right: 0; margin-top: 13.5px;",
                              uiOutput(ns("colFilter_dropdown_ui_fusion")))),
                          do.call(tabsetPanel, c(id = ns("fusion_genes_patients"), lapply(names(set_patient_to_sample("fusion")), function(sample) {
                            tabPanel(title = sample, fusion_genes_table$ui(ns(paste0("geneFusion_tab_", sample))))
                          })))
                        )
                      ),
              tabItem(tabName = ns("expression_profile"),
                    fluidPage(
                      do.call(tabsetPanel, c(id = ns("expression_profile_patients"),
                           lapply(names(set_patient_to_sample("expression")), function(patient) {
                             tabPanel(title = patient,
                                  tabBox(id = ns(paste0("expression_profile_tabs_", patient)), width = 12, collapsible = FALSE,
                                       tabPanel("Genes of Interest",
                                                tabName = ns("genesOfinterest_panel"), value = "genesOfinterest",
                                                expression_profile_table$ui_genesOfInterest(ns(paste0("genesOfinterest_tab_", patient)), patient)),
                                       tabPanel("All Genes",
                                                tabName = ns("allGenes_panel"), value = "allGenes",
                                                expression_profile_table$ui_allGenes(ns(paste0("allGenes_tab_", patient)), patient)
                                                )
                                  )
                             )
                           })))
                    )

                      # tabBox(id = "expression_profile_tabs", width = 12, collapsible = FALSE,
                      #        tabPanel("Genes of interest",tabName = "genesOfinterest_panel",value = "genesOfinterest",
                      #                 fluidPage(
                      #                   do.call(tabsetPanel, c(id = "expression_profile_patients", lapply(names(set_patient_to_sample("expression")), function(sample) {
                      #                     tabPanel(title = sample
                      #                              # expression_profile_table$ui(paste0("exprProfile_tab_", sample), sample)
                      #                              )
                      #                     })))
                      #                 )),
                      #        tabPanel("All genes",tabName = "allGenes_panel",value = "allGenes",
                      #                 fluidPage(
                      #                   do.call(tabsetPanel, c(id = "expression_profile_patients", lapply(names(set_patient_to_sample("expression")), function(sample) {
                      #                     tabPanel(title = sample
                      #                              # expression_profile_table$ui(paste0("exprProfile_tab_", sample), sample)
                      #                     )
                      #                   })))
                      #                 ))
                      # )


              ),
              tabItem(h1("Gene Interactions Network"),tabName = ns("network_graph"),
                      fluidPage(
                        networkGraph_cytoscape$ui(ns("network_graph")))
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

## run summary module
    summary_table$summaryServer("summaryUI", session)

  # lapply(patients_list(), function(patient) {
  #   summary_table$summaryServer(paste0("summaryUI_", patient), session)
  # })
#
#
    getColFilterValues <- function(flag) {
      reactive({
        colnames_list <- colFilter(flag)
        list(all_columns = colnames_list$all_columns, default_setting = colnames_list$default_setting)
      })
    }

#################
  ## filter table columns dropdown button for fusion
    all_colnames_val_fusion <- getColFilterValues("fusion")
    output$colFilter_dropdown_ui_fusion <- renderUI({
      req(all_colnames_val_fusion())
      colFilterDropdown_ui(ns("colFilter_dropdown_fusion"), all_colnames_val_fusion()$all_columns, all_colnames_val_fusion()$default_setting,columnName_map("fusion"))
    })
    
    ## run fusion genes module
    samples_fuze <- set_patient_to_sample("fusion")
    selected_columns_fusion <- colFilterDropdown_server("colFilter_dropdown_fusion", all_colnames_val_fusion()$all_columns, all_colnames_val_fusion()$default_setting)
    lapply(names(samples_fuze), function(patient) {
      fusion_genes_table$server(paste0("geneFusion_tab_", patient), samples_fuze[[patient]], selected_columns_fusion,columnName_map("fusion"))
    })
##################
    
    
    # filter table columns dropdown button for germline
    all_colnames_val_germline <- getColFilterValues("germline")
    output$colFilter_dropdown_ui_germline <- renderUI({
      req(all_colnames_val_germline())
      colFilterDropdown_ui(ns("colFilter_dropdown_germ"), all_colnames_val_germline()$all_columns, all_colnames_val_germline()$default_setting, columnName_map("germline"))
    })
    
    ## igv dropdown button for germline
    selection_enabled <- reactiveVal(FALSE)
    
    # IGV dropdown button for germline
    output$igv_dropdown_ui <- renderUI({
      igvDropdown_ui(ns("igv_dropdown"), names(set_patient_to_sample("germline")))
    })
    
    # IGV Dropdown server call, pass the reactive value
    igvDropdown_server("igv_dropdown", session, selection_enabled)
    
    # Run germline varcall module
    samples_germ <- set_patient_to_sample("germline")
    selected_columns_germ <- colFilterDropdown_server("colFilter_dropdown_germ", all_colnames_val_germline()$all_columns, all_colnames_val_germline()$default_setting)
    
    lapply(names(samples_germ), function(patient) {
      germline_var_call_table$server(paste0("germline_tab_", patient), samples_germ[[patient]], selected_columns_germ, columnName_map("germline"), selection_enabled)
    })

##################
    ## run expression profile module
    samples_expr <- set_patient_to_sample("expression")
    # lapply(names(samples_expr), function(patient) {
    #   expression_profile_table$server(paste0("exprProfile_tab_", patient), samples_expr[[patient]])
    # })
    lapply(names(samples_expr), function(patient) {
      expression_profile_table$server_genesOfInterest(paste0("genesOfinterest_tab_", patient), samples_expr[[patient]])
      expression_profile_table$server_allGenes(paste0("allGenes_tab_", patient), samples_expr[[patient]])
    })


##################    
    ## run network graph module    
    networkGraph_cytoscape$server("network_graph")
    
    


    IGV$igv_server("igv")







  
  
  # piechart_server("pieChart")
  # fusion_genes_table$piechart_server("pieChart")
  


  ##### filtering table for all patients together #####
  ##
  # selected_samples <- dropdown_button$server("select_samples")
  # fusion_genes_table$table_server("geneFusion_tab",selected_samples)
  #####################################################
  })
}

# shinyApp(ui,server,options = list(launch.browser = TRUE))
