
box::use(
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,
        fluidRow,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,br,updateTabsetPanel, actionButton],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems]
  # plotly[renderPlotly, plot_ly,plotlyOutput]
  ,plotly[plot_ly,plotlyOutput,renderPlotly],
  # magrittr,
  # data.table,htmltools,
  # billboarder[bb_donutchart,billboarderOutput,renderBillboarder]
)

# box::use(
#   app/logic/patients_list[patients_list]
# )
# piechart_server <- function(id, selected_samples) {
#   moduleServer(id, function(input, output, session) {
#     
#     #     data <- input_data()
#     #     
#     #     confidence_counts <- data[, .N, by = arriba.confidence]
#     # 
#     stars <- data.frame(
#       package = c("high", "medium", "low", "NA"),
#       stars = c(3, 1, 44, 23)
#     )
#     #     
#     output$donutChart <- renderBillboarder({
#       billboarder() %>%
#         bb_donutchart(data = stars, title = "Distribution of Arriba Confidence Levels")
#     })
#     
#     output$pieChart <- renderPlotly({
#       plot_ly(
#         labels = c("high", "medium", "low", "NA"),
#         values = c(3, 1, 44, 23),
#         type = "pie"
#       )
#     })
#   })
# }
summaryUI <- function(id,patient){
  ns <- NS(id)
  fluidRow(
        # billboarderOutput(ns("donutChart")),
        column(6, plotlyOutput(ns("pieChart_conf"))),
        column(6, plotlyOutput(ns("barplot_conf")))

  )
}


summaryServer <- function(id, parent_session){
  moduleServer(id, function(input, output, session) {
    
    # confidence <- data.frame(
    #   categories = c("high", "medium", "low", "NA"),
    #   counts = c(3, 1, 44, 23),
    #   color = c("red","orange","lightblue2","gray")
    # )
    
    output$pieChart_conf <- renderPlotly({
      plot_ly(
        labels = c("high", "medium", "low", "NA"),
        values = c(3, 1, 44, 23),
        marker = list(colors = c("red","orange","lightblue","lightgray")),
        textinfo = 'value',
        sort = FALSE,
        type = "pie"
      ) %>% layout(
          title = 'Distribution of Arriba Confidence Levels')
    })
    
    output$barplot_conf <- renderPlotly({
      plot_ly(
        x = factor(c("high", "medium", "low", "NA"), levels = c("high", "medium", "low", "NA")),
        y = c(3, 1, 44, 23),
        marker = list(color = c("red","orange","lightblue","lightgray")),
        text = c(3, 1, 44, 23),
        textposition = 'auto',
        type = "bar"
      ) %>% layout(
        title = 'Distribution of Arriba Confidence Levels',
        xaxis = list(title = 'Confidence'),
        yaxis = list(title = 'Count'))
      
    
    })

    # site_data <- dt[,.N,by=.(arriba.site1,arriba.site2)]
    # site_data[is.na(arriba.site1), arriba.site1 := "NA"]
    # site_data[is.na(arriba.site2), arriba.site2 := "NA"]
    site_data <- data.table(
      arriba.site1 = c(NA, "CDS", "CDS", "intron", "CDS/splice-site", "CDS", "intron", "exon", "3'UTR", "CDS", "intron", "CDS", "intron", "3'UTR", "intron"),
      arriba.site2 = c(NA, "CDS", "intron", "intergenic", "CDS/splice-site", "intergenic", "exon/splice-site", "CDS", "CDS", "exon", "CDS", "3'UTR", "exon", "intergenic", "intron"),
      N = c(16, 8, 3, 2, 5, 14, 1, 2, 2, 3, 2, 4, 1, 2, 1)
    )
    # site_data[, arriba.site1 := factor(arriba.site1, levels = unique(arriba.site1))]
    # site_data[, arriba.site2 := factor(arriba.site2, levels = unique(arriba.site2))]
    # 
    # site_combined <- site_data[, .(Total = sum(N)), by = .(arriba.site1, arriba.site2)]
    # 
    # # Vytvoření barplotu
    # plot_ly(site_combined, x = ~arriba.site1, y = ~Total, color = ~arriba.site2, type = 'bar', colors = c("#DF536B", "#61D04F", "#2297E6", "#28E2E5")) %>%
    #   layout(
    #     title = 'Distribution of Arriba Sites',
    #     xaxis = list(title = 'Site 1'),
    #     yaxis = list(title = 'Count'),
    #     barmode = 'group'  # Nastavení skupinového zobrazení
    #   ) %>%
    #   config(displayModeBar = FALSE)  # Zakázat ovládací panel grafu
    ####
    # site1_counts <- site_data[, .(Total = sum(N)), by = arriba.site1]
    # site2_counts <- site_data[, .(Total = sum(N)), by = arriba.site2]
    # 
    # bar1 <- plot_ly(
    #   site1_counts,
    #   x = ~arriba.site1,
    #   y = ~Total,
    #   type = 'bar',
    #   name = 'Site 1',
    #   marker = list(color = 'rgb(55, 83, 109)')
    # )
    # 
    # # Vytvoření barplotu pro arriba.site2
    # bar2 <- plot_ly(
    #   site2_counts,
    #   x = ~arriba.site2,
    #   y = ~Total,
    #   type = 'bar',
    #   name = 'Site 2',
    #   marker = list(color = 'rgb(26, 118, 255)')
    # )
    # 
    # # Kombinace obou barplotů vedle sebe pro porovnání
    # subplot(bar1, bar2, nrows = 1, shareY = TRUE)
    
    site1_counts <- site_data[, .(Total = sum(N)), by = arriba.site1]
    site2_counts <- site_data[, .(Total = sum(N)), by = arriba.site2]
    
    # Přejmenování sloupců pro sloučení
    setnames(site1_counts, old = "arriba.site1", new = "site")
    setnames(site2_counts, old = "arriba.site2", new = "site")
    
    # Přidání sloupce s názvem skupiny
    site1_counts[, group := "Site 1"]
    site2_counts[, group := "Site 2"]
    combined_data <- rbind(site1_counts, site2_counts, use.names = TRUE)
    
    # Vytvoření barplotu
    plot_ly(
      data = combined_data,
      x = ~group,
      y = ~Total,
      type = 'bar',
      color = ~site,
      colors = c("red", "orange", "lightblue2", "gray", "green", "blue", "purple", "pink"),
      text = ~Total,
      textposition = 'auto'
    ) %>%
      layout(
        title = 'Comparison of Arriba Sites',
        xaxis = list(title = 'Group'),
        yaxis = list(title = 'Count'),
        barmode = 'group',
        legend = list(title = list(text = 'Site Type'))
      )
    
  })
}


ui <- dashboardPage(
    header = dashboardHeader(),
    sidebar = dashboardSidebar( id = "sidebar", collapsed = TRUE,
                                h3( "MOII_e_117krve", style = "font-size: 20px; padding: 10px; color: #FFFFFF; "),
                                bs4Dash::sidebarMenu(id = "sidebar_menu",
                                                     bs4Dash::menuItem("Summary",tabName = "summary",icon = icon("id-card-clip")))),
    body = dashboardBody(
      tabItems(
        tabItem(tabName = "summary",
                h3("SUMMARY"),
                summaryUI("summaryUI"))
      )
    )
)


server <- function(input, output, session) {

  summaryServer("summaryUI",session)

}

shinyApp(ui = ui,server = server)
