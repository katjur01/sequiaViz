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
  div(
    class = "summary-card-container",
  fluidRow(

      bs4Card(title = "Patient: DZ1601", icon=icon("person"),collapsible = FALSE,
              div(
                class = "summary-content",
                # fluidRow(
                #   column(6,
                div(
                  class = "text-content",
                     HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                     span("Somatic var call", class = "category"),
                     tags$div(class = "item", "Variants for reviw: 1124"),
                     tags$div(class = "item", "Mutation load normal: 0.46"),
                     tags$div(class = "item", "Mutation load foundationOne: 0"),
                  br(),
                     HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                     span("Germline var call", class = "category"),
                     tags$div(class = "item", "Variants for reviw: 7"),  # aplikuju všechny filtery + všechno cgc germline = true + všechny trusigh které jsou true a cgc není true
                     tags$div(class = "item", "Pathogenic and likely-pathogenic variants: 4"), # podle clinvar_sig sloupce
                  br(),
                     HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                     span("Fusion genes", class = "category"),
                     tags$div(class = "item", "Fused genes: 1"), # high confidence ?
                     tags$div(class = "item", "Potentially fused genes: 15"), # medium + low + NA confidence
                  br(),
                     HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                     span("Expression profile", class = "category"),
                     tags$div(class = "item", "Over-expressed genes: 6"), # asi tabulka - rozdělení podle FC a k tomu alreded pathways
                     tags$div(class = "item", "Altered pathways: 5")
                  ),
                div(
                  class = "image-content",
                  # tags$img(src = "potential_circos1.png", class = "responsive-image")
                  tags$img(src = "potential_circos1.png", style = "width:100%; height:auto;display:inline-block; vertical-align:middle;")
                )
              )
      ),
      bs4Card(title = "Patient: LK0302", icon=icon("person"),collapsible = FALSE,
              div(
                class = "summary-content",
                div(
                  class = "text-content",
                # fluidRow(
                #   column(6,
                   HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                   span("Somatic var call", class = "category"),
                 br(),
                   HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                   span("Germline var call", class = "category"),
                 br(),
                   HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                   span("Fusion genes", class = "category"),
                   tags$div(class = "item", "Fused genes: 1"),
                   tags$div(class = "item", "Potentially fused genes: 17"),
                 br(),
                   HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                   span("Expression profile", class = "category")
                  ),
                div(
                  class = "image-content",
                  # tags$img(src = "potential_circos3.png", class = "responsive-image")
                  tags$img(src = "potential_circos3.png", style = "width:102%; height:auto;display:inline-block; vertical-align:middle;")
                )
              )
      ),
      bs4Card(title = "Patient: MR1507", icon=icon("person"),collapsible = FALSE,
              div(
                class = "summary-content",
                # fluidRow(
                #   column(6,
                  div(
                    class = "text-content",
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Somatic var call", class = "category"),
                       tags$div(class = "item", "Variants for reviw: 1124"),
                       tags$div(class = "item", "Mutation load normal: 1.06"),
                       tags$div(class = "item", "Mutation load foundation one: 2.57"),
                     br(),
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Germline var call", class = "category"),
                       tags$div(class = "item", "Variants for reviw: 7"),
                       tags$div(class = "item", "Pathogenic and likely-pathogenic variants: 2"),
                     br(),
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Fusion genes", class = "category"),
                       tags$div(class = "item", "Fused genes: 0"),
                       tags$div(class = "item", "Potentially fused genes: 18"),
                     br(),
                       HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                       span("Expression profile", class = "category"),
                       tags$div(class = "item", "Over-expressed genes: 13"), # asi tabulka - rozdělení podle FC a k tomu alreded pathways
                       tags$div(class = "item", "Altered pathways: 7")
                  ),
                div(
                  class = "image-content",
                  # tags$img(src = "potential_circos2.png", class = "responsive-image")
                  tags$img(src = "potential_circos2.png", style = "width:100%; height:auto;display:inline-block; vertical-align:middle;")
                )
              )
      ),
      bs4Card(title = "Patient: VH0452", icon=icon("person"),collapsible = FALSE,
              div(
                class = "summary-content",
                # fluidRow(
                #   column(6,
                  div(
                    class = "text-content",
                           HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                           span("Somatic var call", class = "category"),
                           tags$div(class = "item", "Variants for reviw: 1124"),
                           tags$div(class = "item", "Mutation load normal: 2.28"),
                           tags$div(class = "item", "Mutation load foundation one: 2.57"),
                         br(),
                           HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                           span("Germline var call", class = "category"),
                           tags$div(class = "item", "Variants for reviw: 7"),
                           tags$div(class = "item", "Pathogenic and likely-pathogenic variants: 2"),
                         br(),
                           HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
                           span("Fusion genes", class = "category"),
                           tags$div(class = "item", "Fused genes: 1"),
                           tags$div(class = "item", "Potentially fused genes: 12"),
                         br(),
                           HTML('<span class="icon icon-red"><i class="fa-solid fa-square-xmark"></i></span>'),
                           span("Expression profile", class = "category")
                  ),
                div(
                  class = "image-content",
                  # tags$img(src = "potential_circos4.png", class = "responsive-image")
                  tags$img(src = "potential_circos4.png", style = "width:102%; height:auto;display:inline-block; vertical-align:middle;")
                )
        )
      )
    )
  )
}

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
