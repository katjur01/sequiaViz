box::use(
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,span,textOutput,renderText,req,
        fluidRow,fluidPage,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,br,updateTabsetPanel, actionButton],
  bs4Dash[dashboardPage, dashboardHeader, dashboardSidebar, dashboardBody, sidebarMenu, menuItem, dashboardControlbar,tabItems, tabItem, bs4Card,infoBox,tabBox,tabsetPanel,bs4ValueBox,
          controlbarMenu,controlbarItem,column,box,boxLabel,descriptionBlock,boxProfile,boxProfileItem,attachmentBlock,boxComment,userBlock,updateTabItems],
  reactable,
  reactable[colDef],
  data.table[fread,as.data.table,rbindlist,tstrsplit,setcolorder,setnames,fwrite, uniqueN],
  openxlsx[read.xlsx,getSheetNames],
  htmltools[hr,strong,h5]
  # shiny.gosling
  # plotly[renderPlotly, plot_ly,plotlyOutput]
  # plotly[plot_ly,plotlyOutput,renderPlotly],
  # magrittr,
  # data.table,htmltools,
  # billboarder[bb_donutchart,billboarderOutput,renderBillboarder]
)

box::use(
  # app/logic/load_data[get_inputs,load_data],
  app/logic/patients_list[set_patient_to_sample],
  app/logic/prepare_table[get_tissue_list],
)


ui <- function(id){
  ns <- NS(id)
  hasData <- TRUE
  tagList(

      # descriptionBlock(
      #   number = "17%", 
      #   numberColor = "pink", 
      #   numberIcon = icon("caret-up"),
      #   header = "$35,210.43", 
      #   text = "TOTAL REVENUE", 
      #   rightBorder = TRUE,
      #   marginBottom = FALSE
      # )
    
    # column(4,
       # div(class = "text-content",
          # HTML('<span class="icon icon-green"><i class="fas fa-square-check"></i></span>'),
          # span("Somatic var call", class = "category"),
          # tags$div(textOutput(ns("mutationNormal"))),
          # tags$div(textOutput(ns("mutationFoundation"))),
          # tags$div(textOutput(ns("for_review_som"))),
          # tags$div(textOutput(ns("clinvar_N_som"))),
    fluidRow(
      # Levý sloupec – boxy (všechny boxy se zobrazí jeden vedle druhého, případně můžete je obalit do vlastního fluidRow, pokud je chcete mít vertikálně)
      column(8,
        fluidRow(
          div(class = "summary-box somatic-infobox",
              box(
                title = span("Somatic var call", class = "category"),
                tags$div(textOutput(ns("mutationNormal"))),
                tags$div(textOutput(ns("mutationFoundation"))),
                tags$div(textOutput(ns("for_review_som"))),
                tags$div(textOutput(ns("clinvar_N_som"))),
                icon = HTML('<span class="icon icon-green" title="Analysis available"><i class="fa-solid fa-circle-check"></i></span>'),
                elevation = 2,
                collapsible = FALSE,
                headerBorder = FALSE,
                style = "height:136px; overflow:auto;",
                width = 12  # box zabere celou šířku levého sloupce
              )
          ),
          div(class = "summary-box germline-infobox", #class = paste("summary-box germline-infobox", if(!hasData) "no_data" else ""),
            box(
              title = span("Germline var call", class = "category"),
              tags$div(textOutput(ns("for_review_germ"))),
              tags$div(textOutput(ns("clinvar_N_germ"))),
              icon = HTML('<span class="icon icon-gray" title="Analysis not available"><i class="fa-solid fa-circle-xmark"></i></span>'),
              elevation = 2,
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height:136px; overflow:auto;",
              width = 12  # box zabere celou šířku levého sloupce
            )
          ),
          div(class = "summary-box fusion-infobox",
            box(
              title = span("Fusion genes", class = "category"),
              tags$div(textOutput(ns("high_confidence"))),
              tags$div(textOutput(ns("potencially_fused"))),
              icon = HTML('<span class="icon icon-green" title="Analysis available"><i class="fa-solid fa-circle-check"></i></span>'),
              elevation = 2,
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height:136px; overflow:auto;",
              width = 12
            )
          ),
          div(class = "summary-box expression-infobox",
            box(
              title = span("Expression profile", class = "category"),
              tags$div(textOutput(ns("tissues"))),
              tags$div(class = "item", "Over-expressed genes: 41"),
              tags$div(class = "item", "Under-expressed genes: 21"),
              tags$div(class = "item", "Altered pathways: 5"),
              icon = HTML('<span class="icon icon-green" title="Analysis available"><i class="fa-solid fa-circle-check"></i></span>'),
              color = "teal",
              elevation = 2,
              collapsible = FALSE,
              headerBorder = FALSE,
              style = "height:136px; overflow:auto;",
              width = 12
            )
          )
        )
      ),
      # Pravý sloupec – obrázek
      column(4
        # div(
        #   # class = "image-content",
        #   tags$img(src = "potential_circos1.png",
        #            style = "width:70%; height:auto; display:inline-block; vertical-align:middle;")
        # )
      )
    ),
    
    fluidRow(
      column(7,
        hr(),
        uiOutput(ns("germline_boxes")),
        hr(),
        uiOutput(ns("fusion_boxes"))
      )
    )
    
  )
}

server <- function(id, patient, shared_data){
  moduleServer(id, function(input, output, session){
    

    
    #####################
    ### Card overview ###
    #####################
    
    output$tissues <- renderText({
      paste0("Tissue comparison: ",uniqueN(get_tissue_list()))
    })
    
    # Pomocná funkce, která vrátí odpovídající klíč pro overview podle jména pacienta
    getMatchingPatientKey <- function(patient, tag) {
      samples <- set_patient_to_sample(tag)  # Např. c("DZ1601krev", "MR1507krev", "VH0452krev")
      matching_sample <- samples[grepl(patient, samples)]
      if (length(matching_sample) == 0) return(NULL)
      matching_sample[1]
    }
    
    
    output$for_review_germ <- renderText({
      key <- getMatchingPatientKey(patient,"germline")
      if (is.null(key)) return("Variants for review: Not available")
      overview_data <- shared_data$germline_overview[[ key ]]
      if (is.null(overview_data)) {
        return("Variants for review: NA")
      } else {
        paste("Variants for review: ", overview_data$for_review)
      }
    })
    
    output$clinvar_N_germ <- renderText({
      key <- getMatchingPatientKey(patient,"germline")
      if (is.null(key)) return("Pathogenic and likely-pathogenic variants: Not available")
      overview_data <- shared_data$germline_overview[[ key ]]
      if (is.null(overview_data)) {
        return("Pathogenic and likely-pathogenic variants: NA")
      } else {
        paste("Pathogenic and likely-pathogenic variants: ", overview_data$clinvar_N)
      }
    })
    
    
    output$high_confidence <- renderText({
      key <- getMatchingPatientKey(patient,"fusion")
      if (is.null(key)) return("Fused genes with high confidence: Not available")
      overview_data <- shared_data$fusion_overview[[ key ]]
      if (is.null(overview_data)) {
        return("Fused genes with high confidence: NA")
      } else {
        paste("Fused genes with high confidence: ", overview_data$high_confidence)
      }
    })

    output$potencially_fused <- renderText({
      key <- getMatchingPatientKey(patient,"fusion")
      if (is.null(key)) return("Potencially fused genes: Not available")
      overview_data <- shared_data$fusion_overview[[ key ]]
      if (is.null(overview_data)) {
        return("Potencially fused genes: NA")
      } else {
        paste("Potencially fused genes: ", overview_data$potencially_fused)
      }
    })
    
    mutation_load <- reactive({
      dt <- as.data.table(read.xlsx("~/Desktop/sequiaViz/input_files/MOII_e117/117_WES_somatic/mutation_loads.xlsx"))
      dt[,normal:= as.numeric(gsub(",", ".", normal))]
      dt[,foundation_one:= as.numeric(gsub(",", ".", foundation_one))]
      dt
    })
    
    
    # 2) Zobrazení počtu variant pro kontrolu
    output$for_review_som <- renderText({
      paste("Variants for review: ")#, variantsCount())
    })
    output$clinvar_N_som <- renderText({
      paste0("Pathogenic and likely-pathogenic variants: ")
    })
    # 3) Zobrazení mutation load pro 'normal'
    output$mutationNormal <- renderText({
      row <- mutation_load()[sample == patient, ]
      
      if (nrow(row) == 1) {
        paste("Mutation load normal:", row$normal)
      } else {
        "Mutation load normal: N/A"
      }
    })
    
    # 4) Zobrazení mutation load pro 'foundationOne'
    output$mutationFoundation <- renderText({
      row <- mutation_load()[sample == patient, ]
      
      if (nrow(row) == 1) {
        paste("Mutation load foundationOne:", row$foundation_one)
      } else {
        "Mutation load foundationOne: N/A"
      }
    })
    
    
    
    #################################################
    ### Selected variant or fusion data + buttons ###
    #################################################

    output$somatic_boxes <- renderUI({
      som_vars <- as.data.table(shared_data$somatic_data())
      
      if (is.null(som_vars) || nrow(som_vars) == 0) {
        tags$div("No somatic variants selected")
      } else {
        
        som_vars <- som_vars[grepl(patient, sample)]
        message("## som_vars after filtering: ", som_vars)
        
        if (nrow(som_vars) == 0) return(NULL)

        boxes <- lapply(1:nrow(som_vars), function(i) {
          variant <- som_vars[i, ]
          div(class = "summary-box somatic-infobox",
              box(
                title = paste0(variant$Gene_symbol),
                solidHeader = TRUE, collapsed = TRUE, width = 12,
                tags$p(strong("Detail:"), variant$var_name),
                tags$p(strong("Detail:"), variant$additional_info)
              )
          )
        })
        
        tagList(boxes)
        
      }
    })
    
    # Dynamicky generované karty pro germline varianty
    output$germline_boxes <- renderUI({
      germ_vars <- as.data.table(shared_data$germline_data())
      
      if (is.null(germ_vars) || nrow(germ_vars) == 0) {
        # return(NULL)
        tags$div("No germline variants selected")
      } else {
        
      germ_vars <- germ_vars[grepl(patient, sample)]
      message("## germ_vars after filtering: ", germ_vars)
      
      if (nrow(germ_vars) == 0) return(NULL)
      
      boxes <- lapply(1:nrow(germ_vars), function(i) {
        variant <- germ_vars[i, ]
        
        div(class = "germline-box",
          box(
            title = HTML(
              paste0(
                variant$Gene_symbol,
                '<span style="display:inline-block; vertical-align:middle; margin:0 8px; border-left:1px solid #ccc; height:18px;"></span>',
                '<span style="font-size:14px; font-weight:normal; ">missence variant</span>',
                '<span style="display:inline-block; vertical-align:middle; margin:0 8px; border-left:1px solid #ccc; height:18px;"></span>',
                '<span class="clinvar-tag clinvar-pathogenic">Pathogenic</span>'
              )
            ),
            solidHeader = TRUE, collapsed = TRUE, width = 12,
            fluidRow(
              column(3,
                     tags$p(strong("Variant info: ")),
                     tags$p("Location: chr19:45352801"),
                     fluidRow(
                       column(2,
                              tags$p("Ref: C")
                       ),
                       column(2,
                              tags$p("Alt: G")
                       )
                     )
              ),
              column(3,

                     fluidRow(
                       column(6,
                              tags$p("HGVSc: c.1847G>C"),
                              tags$p("HGVSp: p.R616P"),
                              tags$p("Variant type: SNV")
                       )
                     )
                   ),
              column(3,
                    tags$p(strong("Frequency: ")),
                    tags$p("Alelic: 0.28"),
                    tags$p("GnomAD: 0.0000027")
                ),
              column(3)
            )
            
            
          )
        )
      })
      
      tagList(boxes) # Vrátíme seznam boxů jako tagList
      
      }
    })
    
    output$fusion_boxes <- renderUI({
              
      fusion_vars <- as.data.table(shared_data$fusion_data())
      
      if (is.null(fusion_vars) || nrow(fusion_vars) == 0) {
        # return(NULL)
        tags$div("No germline variants selected")
      } else {
        
      
      fusion_vars <- fusion_vars[grepl(patient, sample)]
      message("## fusion_vars after filtering: ", fusion_vars)
      
      if (nrow(fusion_vars) == 0) return(NULL)
      
      # Pro každou variantu vytvoříme box (kartu)
      boxes <- lapply(1:nrow(fusion_vars), function(i) {
        fusion <- fusion_vars[i, ]
        div(class = "fusion-box",
          box(
            title = paste0(fusion$gene1," - ", fusion$gene2),
            solidHeader = TRUE, collapsed = TRUE, width = 12, boxToolSize = "xs",
            fluidRow(
              column(3,
                     tags$p(strong("Frame: "), "inframe"),
                     tags$p(strong("Coverage: "), "35"),
                     ),
              column(3,
                     tags$p(strong("Gene1: "), fusion$gene1),
                     tags$p("Gene id: ENS000000003"),
                     tags$p("Position: chr11:118482495"),
                     tags$p("Site: CDS/splice-site")
              ),
              column(3,
                     tags$p(strong("Gene1: "), fusion$gene2),
                     tags$p("Gene id: ENS00001115680"),
                     tags$p("Position: chr9:20365744"),
                     tags$p("Site: intron")
              ),
              column(3)
            )

          )
        )
      })
      
      tagList(boxes) # Vrátíme seznam boxů jako tagList
      
      }
      
    })
    

    
    
  })
}

# ui <- fluidPage(
#   UI("xx")
# )
# server <- function(input, output, session){
#   SERVER("xx","DZ1601")
# }
# shinyApp(ui,server,options = list(launch.browser = TRUE))