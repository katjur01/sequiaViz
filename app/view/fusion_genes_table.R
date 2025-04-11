# app/view/fusion_genes_table.R

box::use(
  shiny[moduleServer,NS,h3,tagList,div,textInput,renderPrint,reactive,observe,observeEvent,icon,mainPanel,titlePanel,isolate,
        uiOutput,renderUI,HTML,req,reactiveVal,column,fluidRow],
  reactable,
  reactable[reactable,colDef,reactableOutput,renderReactable,JS,getReactableState],
  htmltools[tags,p],
  bs4Dash[actionButton,bs4Card],
  shinyjs[useShinyjs,runjs,hide,show],
  reactablefmtr[pill_buttons,icon_assign],
  data.table[fifelse,setcolorder],
  shinyalert[shinyalert,useShinyalert],
  data.table[data.table,uniqueN],
)
box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_fusion_genes_table,prepare_arriba_images], 
  app/logic/waiters[use_spinner],
  app/logic/reactable_helpers[generate_columnsDef]
)

# Load and process data table
input_data <- function(sample){
  filenames <- get_inputs("per_sample_file")
  # message("Loading data for fusion: ", filenames$fusions)
  data <- prepare_fusion_genes_table(load_data(filenames$fusions,"fusion",sample),sample)
  return(data)
}

##############  pozn  #####################
#ploty budu dělat pomocí balíku 
#nicméně je problém s instalací XML knihovny. 
#Důvod a co mám dělat je zde:
#  https://support.bioconductor.org/p/52539/
##########################################
#' @export
ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    # div(
    #   class = "fusion-table-contant",
      use_spinner(reactable$reactableOutput(ns("fusion_genes_tab"))),
    # )
    tags$br(),
    actionButton(ns("selectFusion_button"), "Select fusion as causal", status = "info"),
    tags$br(),
    fluidRow(
      column(3,reactableOutput(ns("selectFusion_tab")))
    ),
    tags$br(),
    fluidRow(
      column(1,actionButton(ns("delete_button"), "Delete fusion", status = "danger")),
      column(1,),
      column(1,actionButton(ns("confirm_btn"), "Confirm fusion", status = "success"))
    )

    # actionButton(ns("delete_button"), "Delete fusion", status = "danger"),
    # actionButton(ns("confirm_btn"), "Confirm fusion", status = "success"),


    
  )

}

#' @export
server <- function(id, selected_samples, selected_columns, column_mapping, shared_data) {
  moduleServer(id, function(input, output, session) {
    # prepare_arriba_images(selected_samples)
    
  # Call loading function to load data
    dt <- reactive({
      message("Loading input data for fusion")
      input_data(selected_samples) 
    })

    
    # observe({
    #   req(dt())
    #   overview_dt <- data.table(
    #     high_confidence = uniqueN(dt()[arriba.confidence %in% "high"]),
    #     potencially_fused = uniqueN(dt()[arriba.confidence %in% c("medium", "low", NA)]))
    #   shared_data$fusion_overview[[ selected_samples ]] <- overview_dt
    # })

    
    
  # Call generate_columnsDef to generate colDef setting for reactable
    column_defs <- reactive({
      message("Generating colDef for fusion")
      req(selected_columns())
      generate_columnsDef(names(dt()), selected_columns(), "fusion", column_mapping, session)
    })
    
    # # Reactive value to store selected rows
    selected_fusions <- reactiveVal(data.frame(gene1 = character(), gene2 = character()))
    
    output$fusion_genes_tab <- renderReactable({
      pathogenic_fusions <- selected_fusions() # seznam fúzí, které byly označeny jako patogenní
      
      message("Rendering Reactable for fusion")
      # reactable(as.data.frame(dt()),
        reactable(dt(),
                          columns = column_defs(),
                          class = "fusion-table",
                          resizable = TRUE,
                          showPageSizeOptions = TRUE,
                          pageSizeOptions = c(10, 20, 50, 100),
                          defaultPageSize = 10,
                          striped = TRUE,
                          wrap = FALSE,
                          highlight = TRUE,
                          outlined = TRUE,
                          defaultColDef = colDef(
                            align = "center",
                            sortNALast = TRUE
                          ),
                          defaultSorted = list("arriba.confidence" = "asc","arriba.called" = "desc","starfus.called" = "desc"),
                          details = function(index) {
                            # row <- data[index, ]
                            svg_file <- dt()$svg_path[index]
                            png_file <- dt()$png_path[index]
                            tags$div(
                              style = "display: flex; align-items: center;",
                              if (file.exists(paste0("www/",svg_file))) {
                                tags$img(src = svg_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top;")
                              } else {
                                tags$strong("Starfusion doesn't provide this picture.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                              },
                              if (file.exists(paste0("www/",png_file))) {
                                tags$img(src = png_file, style = "width:50%; height:auto; display:inline-block; vertical-align:top; margin-bottom:40px;")
                              } else {
                                tags$strong("IGV didn't snapshot this position.", style = "width:10%; height:auto; display:inline-block; vertical-align:middle; margin-left: 20px; font-weight: bold; text-align: center; margin-top:40px; margin-bottom:40px;")
                              }
                            )
                          },
                          rowStyle = function(index) {
                            gene1_in_row <- dt()$gene1[index]
                            gene2_in_row <- dt()$gene2[index]
                            
                            # Pokud je aktuální fúze v seznamu vybraných fúzí, obarvíme ji
                            if ((gene1_in_row %in% pathogenic_fusions$gene1 & 
                                 gene2_in_row %in% pathogenic_fusions$gene2) |
                                (gene1_in_row %in% pathogenic_fusions$gene2 & 
                                 gene2_in_row %in% pathogenic_fusions$gene1)) {
                              list(backgroundColor = "#B5E3B6", fontWeight = "bold")
                            } else {
                              NULL
                            }
                          },
                          #onClick = "expand",
                          selection = "multiple",
                          onClick = JS("function(rowInfo, column, event) {
                                          if (event.target.classList.contains('rt-expander') || event.target.classList.contains('rt-expander-button')) {
                                              rowInfo.toggleRowExpanded();  
                                          } else {
                                              rowInfo.toggleRowSelected();
                                        }}"),
                          elementId = "tbl-fusion"
        )
  })
    
    
    # Sledování vybraného řádku a varianty
    selected_fusion <- reactive({
      selected_row <- getReactableState("fusion_genes_tab", "selected")
      req(selected_row)
      
      dt()[selected_row, c("gene1","gene2")]  # Získání fúze z vybraného řádku
      message("data fusion tab: ", dt()[selected_row, c("gene1","gene2")])
      # var <- filtered_data()[selected_row, c("var_name","Gene_symbol")]  # Získání varianty z vybraného řádku
      # var$remove <- NA
      
    })
    
    # Akce po kliknutí na tlačítko pro přidání fúze
    observeEvent(input$selectFusion_button, {
      selected_rows <- getReactableState("fusion_genes_tab", "selected")
      req(selected_rows)
      
      new_variants <- dt()[selected_rows, c("gene1","gene2")]  # Získání vybraných fúzí
      new_variants$sample <- selected_samples
      current_variants <- selected_fusions()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$gene1 %in% current_variants$gene1 &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$gene2 %in% current_variants$gene2), ]
      
      if (nrow(new_unique_variants) > 0) {      # Přidáme pouze unikátní varianty
        selected_fusions(rbind(current_variants, new_unique_variants))
        # shared_data$fusion_data(selected_fusions())
        # message("Updated fusion_data in shared_data:", shared_data$fusion_data())
      }
      
      # Aktualizace globální proměnné shared_data$germline_data:
      global_data <- shared_data$fusion_data()
      
      if (is.null(global_data) || nrow(global_data) == 0 || !("sample" %in% names(global_data))) {
        global_data <- data.table(
          sample = character(),
          gene1 = character(),
          gene2 = character()
        )
      }
      message("## selected_fusions(): ", selected_fusions())
      message("## global_data: ", global_data)
      # Odstraníme data, která patří právě tomuto pacientovi
      global_data <- global_data[sample != selected_samples]
      
      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbind(global_data, selected_fusions())
      shared_data$fusion_data(updated_global_data)
      message("## shared_data$fusion_data(): ", shared_data$fusion_data())
    })
    
    output$selectFusion_tab <- renderReactable({
      variants <- selected_fusions()
      if (nrow(variants) == 0) {
        return(NULL)
      }
      
      reactable(
        variants,
        columns = list(
          gene1 = colDef(name = "Gene1"),
          gene2 = colDef(name = "Gene1")
          # remove = colDef(
          #   name = "",
          #   cell = function(value, index) {
          #     # actionButton(session$ns("delete"), label = "", class = "btn btn-danger btn-sm", icon = icon("remove"))
          #     # actionButton(session$ns(paste0("remove_", index)), label = "", class = "btn btn-danger btn-sm", icon = icon("remove"))
          #   })
        ),
        selection = "multiple", onClick = "select"
      )
    })
    
    observeEvent(input$delete_button, {
      rows <- getReactableState("selectFusion_tab", "selected")
      req(rows)
      current_variants <- selected_fusions()
      updated_variants <- current_variants[-rows, ]
      
      selected_fusions(updated_variants)
      shared_data$fusion_data(updated_variants)
      
      session$sendCustomMessage("resetReactableSelection",selected_fusions())
      
      if (nrow(selected_fusions()) == 0) {
        hide("confirm_btn")
        hide("delete_button")
      }
    })
    

    fusion_selected <- reactiveVal(FALSE)
    
    # Při stisku tlačítka pro výběr fúze
    observeEvent(input$selectFusion_button, {
      if (nrow(selected_fusions()) == 0) {
        # Pokud nejsou vybrány žádné řádky, zůstaň u původního stavu
        fusion_selected(FALSE)
        hide("confirm_btn")
        hide("delete_button")
        
        shinyalert(
          title = "No fusion selected",
          text = "Please select the potentially causal fusions from table above.",
          type = "warning",
          showCancelButton = FALSE,
          confirmButtonText = "OK",
          callbackR = function(value) {
            # value bude TRUE pro OK, FALSE pro "Go to variant"
            if (!value) {
              # updateTabItems(session = session$userData$parent_session,  # použijeme parent session
              #                inputId = "sidebar_menu",  # bez namespace
              #                selected = "fusion_genes")
            }})
      } else {
        # Pokud jsou nějaké řádky vybrány, nastav fusion_selected na TRUE
        fusion_selected(TRUE)
        
        # Zobraz tlačítka pomocí shinyjs
        show("confirm_btn")
        show("delete_button")
      }
    })
    
    hide("confirm_btn")
    hide("delete_button")

    
    
    # observeEvent(input$igvButton_click, {
    #   runjs("
    #     var newWindow = window.open(window.location.href.split('#')[0] + '#shiny-tab-hidden_igv', '_blank');
    #     newWindow.onload = function() {
    #       newWindow.Shiny.setInputValue('go_to_hidden_igv', true);
    #     }
    #   ")
    # })

    
    # observeEvent(input$openIGVButton, {
    #     IGV$server("igv")
    # 
    # })
#     
#     # observeEvent(input$igvButton_click, {
#     #   IGV$server("igv")
#     #   # system("open -a /Users/katerinajuraskova/Desktop/IGV.app -b /Users/katerinajuraskova/Desktop/sequiaViz/batch_file_2.txt")
#     #   # system("npx http-server -a localhost ./ -p 8080", wait = FALSE) # -p 8080
#     #   # runjs("window.open('http://localhost:8080/www/igv_web_app/', '_blank');")
#     # })

  })
}


#
# fusionGenes_Ui <- fluidPage(
#   piechart_input("plots")
#   # table_ui("geneFusion_tab")
# )
#
# fusionGenes_Server <- function(input, output, session){
#   piechart_server("plots")
#   # table_server("geneFusion_tab")
# }
#
# shinyApp(ui = fusionGenes_Ui, server = fusionGenes_Server)
