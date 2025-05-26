#app/main.R

box::use(
  shiny[moduleServer, NS, tags, observeEvent,reactiveValues, req, HTML,tabPanel,showModal,
        modalDialog, modalButton],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, tabItems,
          tabItem, box, tabBox, navbarMenu,navbarTab, updateTabItems],
  htmltools[tags],
  shinyjs[useShinyjs],
)

box::use(
  app/view/variant_ui_server,
  app/view/IGV,
  app/logic/igv_helper[start_static_server,stop_static_server]
)

#' @export
ui <- function(id){
  ns <- NS(id)
  useShinyjs()
  tags$head(tags$style(HTML("#app-plots_tabBox_box {box-shadow: none !important; border: none !important;}
                            "))
  )
  dashboardPage(
    dark = NULL,
    help = NULL,
    header = dashboardHeader(
      nav = navbarMenu(
        inputId = ns("main_navbar"),
        navbarTab("Variant calling", tabName = ns("variant_calling")),
        navbarTab("IGV", tabName = ns("hidden_igv")),
        navbarTab("Expression profile", tabName = ns("expression_profile")),
        navbarTab("Network graph", tabName = ns("network_graph")),
        navbarTab("Fusion genes", tabName = ns("fusion_genes")),
        navbarTab("Summary", tabName = ns("summary"))
      )
    ),
    sidebar = dashboardSidebar(disable = TRUE),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = ns("summary")),
        tabItem(tabName = ns("variant_calling"),
                tabBox(id = ns("variant_calling_tabs"), width = 12, collapsible = FALSE, selected = "somatic",
                       tabPanel("Germline small variant calling",tabName = ns("germline_var_call_panel"),value = "germline"),
                       tabPanel("Somatic small variant calling",tabName = ns("somatic_var_call_panel"),value = "somatic",
                                tags$style(HTML(".btn-group > .btn.active {background-color: skyblue; color: white;}
                                                 .btn-mygrey {background-color: lightgray; color: black;}
                                                ")),
                                variant_ui_server$ui(ns("somatic_var_call_tab"))
                                )
                )),
        tabItem(tabName = ns("fusion_genes")),
        tabItem(tabName = ns("expression_profile")),
        tabItem(tabName = ns("network_graph")),
        tabItem(tabName = ns("hidden_igv"),
                tags$style(HTML("#igv-igvDiv {width: 100%; height: auto; border: none; margin: 0 auto; padding: 20px; box-sizing: border-box;}
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
    variant_ui_server$server("somatic_var_call_tab", parent_session = session, shared_data = shared_data)
    
    start_static_server(dir = "/home/annamo/sequiaViz/input_files/bam")
    IGV$igv_server("igv", shared_data = shared_data)
    session$onSessionEnded(function() {
      stop_static_server()
    })
  })
}

