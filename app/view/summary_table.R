box::use(
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,span,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,br,updateTabsetPanel, actionButton],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems],
  reactable,
  reactable[colDef],
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames,fwrite],
  openxlsx[read.xlsx,getSheetNames]
  # shiny.gosling
  # plotly[renderPlotly, plot_ly,plotlyOutput]
  # plotly[plot_ly,plotlyOutput,renderPlotly],
  # magrittr,
  # data.table,htmltools,
  # billboarder[bb_donutchart,billboarderOutput,renderBillboarder]
)

# box::use(
#   app/logic/load_data[get_inputs,load_data],
#   # app/logic/prepare_table[prepare_fusion_genes_table],
# )

# summaryUI <- fluidPage(
#' @export
summaryUI <- function(id){
  ns <- NS(id)
  fluidRow(
    bs4Card(title = "Patient: DZ1601", icon=icon("person"),collapsible = FALSE,
            tags$head(
              tags$style(HTML("
                            .icon {
                              font-size: 20px;
                              margin-right: 10px;
                            }
                            .icon-green {
                              color: #00a65a;
                            }
                            .icon-red {
                              color: #ff0000;
                            }
                            .category {
                              font-weight: bold;

                              margin-bottom: 5px;
                            }
                            .item {
                              margin-left: 40px;
                            }
                          ")) #                              text-decoration: underline;
            ),
              fluidRow(
                column(6,
                       div(
                         HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                         span("Somatic var call", class = "category"),
                         tags$div(class = "item", "Variants for reviw: 1124"),
                         tags$div(class = "item", "Mutation load normal: 0.46"),
                         tags$div(class = "item", "Mutation load foundationOne: 0")
                         # tags$div(class = "item", tags$table(border = 0, 
                         #                        tags$tr(
                         #                          tags$td(align = "center", ""),
                         #                          tags$td(align = "center", "normal"),
                         #                          tags$td(align = "center", "FoundationOne")
                         #                        ),
                         #                        tags$tr(
                         #                          tags$td(align = "center", "Mutation load"),
                         #                          tags$td(align = "center", "0.46"),
                         #                          tags$td(align = "center", "0")
                         #                        )
                         #                      )
                       ),
                       br(),
                       div(
                         HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                         span("Germline var call", class = "category"),
                         tags$div(class = "item", "Variants for reviw: 7"),  # aplikuju všechny filtery + všechno cgc germline = true + všechny trusigh které jsou true a cgc není true
                         tags$div(class = "item", "Pathogenic and likely-pathogenic variants: 4") # podle clinvar_sig sloupce
                       ),
                       br(),
                       div(
                         HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                         span("Fusion genes", class = "category"),
                         tags$div(class = "item", "Fused genes: 1"), # high confidence ?
                         tags$div(class = "item", "Potentially fused genes: 15") # medium + low + NA confidence
                       ),
                       br(),
                       div(
                         HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                         span("Expression profile", class = "category"),
                         tags$div(class = "item", "Over-expressed genes: 6"), # asi tabulka - rozdělení podle FC a k tomu alreded pathways
                         tags$div(class = "item", "Altered pathways: 5")
                       )
                ),
                column(6,tags$img(src = "potential_circos1.png", style = "width:100%; height:auto;display:inline-block; vertical-align:middle;"))
              )

            # fluidRow(column(11,tags$img(src = "potential_circos1.png", style = "width:100%; height:auto;display:inline-block; vertical-align:middle;")),
            #          column(1, actionButton(ns("back2fusion_tab"),label = NULL,icon=icon("eye"))))
            # reactable$reactableOutput("fusion_genes_tab")
            
    ),
    bs4Card(title = "Patient: LK0302", icon=icon("person"),collapsible = FALSE,
            fluidRow(
              column(6,
                     div(
                       HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                       span("Somatic var call", class = "category"),
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                       span("Germline var call", class = "category")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Fusion genes", class = "category"),
                       tags$div(class = "item", "Fused genes: 1"),
                       tags$div(class = "item", "Potentially fused genes: 17")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                       span("Expression profile", class = "category")
                     )
              ),
              column(6,tags$img(src = "potential_circos3.png", style = "width:102%; height:auto;display:inline-block; vertical-align:middle;"))
            )
    ),
    bs4Card(title = "Patient: MR1507", icon=icon("person"),collapsible = FALSE,
            fluidRow(
              column(6,
                     div(
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Somatic var call", class = "category"),
                       tags$div(class = "item", "Variants for reviw: 1124"),
                       tags$div(class = "item", "Mutation load normal: 1.06"),
                       tags$div(class = "item", "Mutation load foundation one: 2.57")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Germline var call", class = "category"),
                       tags$div(class = "item", "Variants for reviw: 7"),
                       tags$div(class = "item", "Pathogenic and likely-pathogenic variants: 2")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Fusion genes", class = "category"),
                       tags$div(class = "item", "Fused genes: 0"),
                       tags$div(class = "item", "Potentially fused genes: 18")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Expression profile", class = "category"),
                       tags$div(class = "item", "Over-expressed genes: 13"), # asi tabulka - rozdělení podle FC a k tomu alreded pathways
                       tags$div(class = "item", "Altered pathways: 7")
                     )
              ),
              column(6,tags$img(src = "potential_circos2.png", style = "width:100%; height:auto;display:inline-block; vertical-align:middle;"))
            )
    ),
    bs4Card(title = "Patient: VH0452", icon=icon("person"),collapsible = FALSE,
            fluidRow(
              column(6,
                     div(
                       # HTML('<span class="icon">&#x2705;</span>'),
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Somatic var call", class = "category"),
                       tags$div(class = "item", "Variants for reviw: 1124"),
                       tags$div(class = "item", "Mutation load normal: 2.28"),
                       tags$div(class = "item", "Mutation load foundation one: 2.57")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Germline var call", class = "category"),
                       tags$div(class = "item", "Variants for reviw: 7"),
                       tags$div(class = "item", "Pathogenic and likely-pathogenic variants: 2")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Fusion genes", class = "category"),
                       tags$div(class = "item", "Fused genes: 1"),
                       tags$div(class = "item", "Potentially fused genes: 12")
                     ),
                     br(),
                     div(
                       HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                       span("Expression profile", class = "category")
                     )
              ),
              column(6,tags$img(src = "potential_circos4.png", style = "width:102%; height:auto;display:inline-block; vertical-align:middle;"))
            )
    )
  )
}
      # bs4Card(title = paste0("Patient: ",patient), icon=icon("person"),status = "primary",
              # billboarderOutput(ns("donutChart")),
              # plotlyOutput(ns("pieChart")),
              # box(title = "Fusion genes", width = 12, icon = icon("atom"),
              #     fluidRow(
              #       column(11,descriptionBlock("Dashboard zastoupení v Gnomad, Cosmic,atd.",rightBorder = FALSE)),
              #       column(1, actionButton(ns("back2fusion_tab"),label = NULL,icon=icon("eye"))),
              #       )
              #     ),
              # box(title = "Variant calling", width = 12, icon = icon("dna"),
              #     fluidRow(
              #       column(11,descriptionBlock("Dashboard frekvence variant v Gnomad, Cosmic,atd.",rightBorder = FALSE)),
              #       column(1, actionButton(inputId = ns("back2variant_tab"),label = NULL,icon=icon("eye")))
              #       # actionButton(ns("summary_varcall_pat2"),label = NULL,icon("eye"),class = "btn btn-primary btn-sm"),
               #     ))
    #   # bs4Card(title = "Patient: LK0302", status = "primary", icon=icon("person"),
    #   #         br(),
    #   #         boxProfile(title = "Variant calling",
    #   #                    bordered = FALSE,
    #   #                    boxProfileItem(
    #   #                      title = "Dashboard",
    #   #                      description = "Dashboard frekvence variant v Gnomad, Cosmic,atd.")),
    #   #         br(),
    #   #         boxProfile(title = "Fusion genes",
    #   #                    bordered = FALSE,
    #   #                    boxProfileItem(
    #   #                      title = "Dashboard",
    #   #                      description = "Dashboard list databází Gnomad, Cosmic,atd.")))

     # )
# }
# )
#' @export
summaryServer <- function(id, parent_session){
  moduleServer(id, function(input, output, session) {
# summaryServer <- function(input, output, session) {


  observeEvent(input$back2fusion_tab, {
    updateTabItems(parent_session, "sidebar_menu", "fusion_genes")
  })

  observeEvent(input$back2variant_tab, {
      updateTabItems(parent_session, "sidebar_menu", "variant_calling")
    })



  })
}
# 
# ui <- fluidPage(
#   summaryUI("xx")
# )
# server <- function(input, output, session){
#   summaryServer("xx")
# }
# shinyApp(ui,server,options = list(launch.browser = TRUE))
