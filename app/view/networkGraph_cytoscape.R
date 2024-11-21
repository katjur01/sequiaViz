# shiny::shinyAppDir(".", options = list(launch.browser = TRUE))


box::use(
  shiny[NS, moduleServer, observeEvent, observe, tagList, fluidPage, fluidRow, column, textInput, updateTextInput, actionButton, selectInput, reactive, req,reactiveVal,conditionalPanel,verbatimTextOutput,
        renderPrint,renderText,textOutput,htmlOutput,uiOutput,renderUI,icon,textAreaInput,updateTextAreaInput,isolate,isTruthy],
  httr[GET, status_code, content],
  htmltools[h3, h4, tags, div,HTML,p],
  jsonlite[fromJSON, toJSON,read_json],
  cyjShiny[cyjShinyOutput, renderCyjShiny, cyjShiny, dataFramesToJSON, selectNodes,setNodeAttributes,selectFirstNeighbors,fit,fitSelected,clearSelection,getSelectedNodes],
  data.table[fread,setnames],
  readxl[read_excel],
  graph[nodes],
  reactable[reactable,colDef,renderReactable,reactableOutput,JS],
  shinyWidgets[radioGroupButtons,pickerInput,searchInput,updatePickerInput,prettySwitch],
  shinyjs[hidden,useShinyjs,toggle,hide]
)

box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/waiters[use_spinner],
  app/logic/networkGraph_helper[get_string_interactions,prepare_cytoscape_network,get_pathway_list,get_tissue_list]
)


input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file")
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag)
  return(data)
}

ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/webcola@3.4.0/WebCola/cola.min.js"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/cytoscape-cola@2.5.1/cytoscape-cola.min.js"),
      tags$script(src="https://unpkg.com/layout-base/layout-base.js"),
      tags$script(src="https://unpkg.com/cose-base/cose-base.js"),
      tags$script(src="https://unpkg.com/cytoscape-fcose/cytoscape-fcose.js"),
      # tags$script(src="https://www.unpkg.com/jquery@3.0.0/dist/jquery.min.js"),
      # tags$script(src="https://unpkg.com/cytoscape-panzoom@2.5.3/cytoscape-panzoom.js"),
      tags$script(src = "static/js/cytoscape_init.js"),
      tags$script(HTML(sprintf("var cyContainerId = '%s'; var cySubsetContainerId = '%s'; var cySelectedNodesInputId = '%s'; var cyUnselectedNodes = '%s';", 
                               ns("cyContainer"), ns("cySubsetContainer"), ns("cySelectedNodes"), ns("cyUnselectedNodes"))))
    ),
    fluidRow(
      column(6,
        fluidRow(
          column(3,
                 div(pickerInput(ns("selected_pathway"), "Pathway", choices = get_pathway_list(), options = list(`live-search` = TRUE)), style = "width: 75%")),
          column(3,
            div(prettySwitch(ns("selectedVariants"), label = "Add possibly pathogenic variants", status = "primary", slim = TRUE)),
            div(prettySwitch(ns("selectedFusions"), label = "Add selected fusions", status = "primary", slim = TRUE))),
          column(2,
                 selectInput(ns("selected_layout"),"Choose layout: ", choices = c("cola","cose","fcose"),selected = "cola",width = "65%"))
        )
      ),
      column(1,),
      column(5,
         fluidRow(
           column(2,),
           # column(4,
                  # textAreaInput(ns("select_geneList"), "List of genes", placeholder = "BRCA1,E2F1,NOTCH3,TP53", rows = 1, resize = "both")
                  # ),
          # column(1,
          #        div(actionButton(ns("add_genes_btn"), label = NULL, icon = icon("plus"))),
          #        div(actionButton(ns("remove_genes_btn"), label = NULL, icon = icon("trash-can")))),
          column(4,
                 div(textAreaInput(ns("new_genes"), label = "Add new genes (comma-separated):", placeholder = "Enter gene names here...", rows = 1, resize = "both",width = "100%"), style = "width: 75%;"),
                 div(actionButton(ns("confirm_new_genes_btn"), label = "Add Genes", icon = icon("check"), width = "100%"), style = "margin-bottom: 10px; width: 75%;")),
          column(4,
                 div(pickerInput(ns("remove_genes"), "Remove genes:", choices = NULL, multiple = TRUE, options = list(`live-search` = TRUE, `actions-box` = TRUE,`multiple-separator` = ", ", `none-selected-text` = "Select gene name", 
                                                                                                                      size = 5, `width` = "100%", `virtual-scroll` = 10,`tick-icon` = "fa fa-times")),style = "width: 75%;"),
                 div(actionButton(ns("confirm_remove_genes_btn"), label = "Remove genes", icon = icon("trash-can"),width = "100%"), style = "margin-bottom: 10px; width: 75%;"))
          # column(4,
          #        div(hidden(textAreaInput(ns("new_genes"), label = "Add new genes (comma-separated):", placeholder = "Enter gene names here...", rows = 4, resize = "both")), style = "width: 90%;"),
          #        div(hidden(actionButton(ns("confirm_new_genes_btn"), label = "Add Genes", icon = icon("check"), style = "margin-bottom: 10px; width: 90%;"))),
          #        div(hidden(pickerInput(ns("remove_genes"), "Remove genes:", choices = NULL, multiple = TRUE, options = list(`live-search` = TRUE, `actions-box` = TRUE,`multiple-separator` = ", ", `none-selected-text` = "", size = 5, `width` = "100%", `virtual-scroll` = 10,`tick-icon` = "fa fa-times")))
          #            ,style = "margin-top: 35px;"),
          #        div(hidden(actionButton(ns("confirm_remove_genes_btn"), label = "Remove genes", icon = icon("trash-can"), style = "margin-bottom: 10px; width: 90%;"))))
         )
     )
   ),
    # fluidRow(
    #   column(8,),
    #   column(2,hidden(actionButton(ns("confirm_new_genes_btn"), label = "Add Genes", icon = icon("check"), style = "margin-top: 10px; width: 75%;"))),
    #   column(2,hidden(actionButton(ns("confirm_remove_genes_btn"), label = "Remove genes", icon = icon("trash-can"), style = "margin-top: 10px; width: 75%;")))
    # ),
    
   

    uiOutput(ns("js_namespace")),
    fluidRow(
      column(6,
          div(style = "display: flex; justify-content: center;",
            actionButton(ns("clearSelection_btn"), label = "Clear selection", icon = icon("eraser")),
            actionButton(ns("selectNeighbors_btn"), label = "Select first neighbors"),
            actionButton(ns("fitGraph_btn"), label = "Fit graph")))
    ),
       
    fluidRow(
      column(6,div(id = ns("cyContainer"), style = "width: 100%; height: 600px;")),
      column(1,),
      column(5,div(id = ns("cySubsetContainer"), style = "width: 100%; height: 600px;"))
    ),
   fluidRow(
     column(6,
        radioGroupButtons(ns("selected_tissue"),"Choose a tissue :",choices = get_tissue_list(),justified = TRUE))
   ),
    tab_UI(ns("tab"))
  )
}

server <- function(id) {
  moduleServer(id, function(input, output, session) {
    interactions <- reactiveVal(NULL)
    previous_selected_nodes <- reactiveVal(NULL)
    previous_combined_genes <- reactiveVal(character(0))
    
    ns <- session$ns
    dt <- input_data("MR1507","all_genes")
    
    # pathway_dt <- unique(dt[grepl("Metabolic pathways", all_kegg_paths_name, fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    # tissue_dt <- unique(pathway_dt[tissue == "Breast"])
    # interactions <- get_string_interactions(tissue_dt[, feature_name])
    # network_json <- prepare_cytoscape_network(interactions, tissue_dt[, feature_name], tissue_dt[, log2FC])


    pathway_dt <- reactive({
      req(input$selected_pathway)
      unique(dt[grepl(input$selected_pathway, all_kegg_paths_name,fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    })

    tissue_dt <- reactive({
      req(input$selected_tissue)
      req(pathway_dt())
      unique(pathway_dt()[tissue == input$selected_tissue])
    })
    
    observe({      # Fetch STRING interactions for the current tissue
      req(tissue_dt())
      message("Fetching STRING interactions")
      interactions(get_string_interactions(tissue_dt()[, feature_name]))
    })
    
    network_json <- reactive({      # Prepare the Cytoscape network using the fetched interactions and log2FC values
      req(tissue_dt(),interactions())
      prepare_cytoscape_network(interactions(), tissue_dt()[, feature_name], tissue_dt()[, log2FC])
    })
  
    output$js_namespace <- renderUI({    # Předat jmenný prostor do JavaScriptu - umožní  používat ns v JS
      tags$script(HTML(sprintf("var ns = '%s';", ns(""))))
    })
    
    ###### network observeEvents #####
    selected_genes <- reactiveVal(character(0))
                                  
    observeEvent(list(input$selected_pathway, input$selected_tissue), {
      req(input$selected_pathway, input$selected_tissue)
      message("Selected pathway: ", input$selected_pathway, ", Selected tissue: ", input$selected_tissue)
      session$sendCustomMessage("cy-init", network_json())
      previous_selected_nodes(NULL)  # Resetujeme uložený výběr uzlů
    })
    
    # Plot subset network when nodes are entered in the textInput
   observeEvent(list(input$cySelectedNodes, input$selected_pathway, input$selected_tissue, input$select_geneList), {
    req(interactions())

    # Získání seznamu genů z cySelectedNodes (výběr v grafu)
    selected_genes_from_graph <- input$cySelectedNodes
    message("Vybrané uzly z hlavního grafu: ", paste(selected_genes_from_graph, collapse = ", "))

    # Kombinace s aktuálními vybranými geny
    combined_genes <- unique(c(selected_genes_from_graph, selected_genes()))
    message("Kombinované vybrané geny: ", paste(combined_genes, collapse = ", "))

    # Získání aktuálního obsahu textového pole
    current_genes_in_textarea <- isolate(input$select_geneList)
    message("Aktuální obsah textového pole select_geneList: ", current_genes_in_textarea)

    # Podmínka pro aktualizaci pouze při změně obsahu textového pole
    if (!identical(current_genes_in_textarea, paste(combined_genes, collapse = "\n"))) {
        message("Aktualizuji textové pole select_geneList s novými hodnotami.")
        updateTextAreaInput(session, "select_geneList", value = paste(combined_genes, collapse = "\n"))
    } else {
        message("Obsah textového pole select_geneList se nezměnil, aktualizace není potřeba.")
    }

    # Porovnání kombinovaných genů s předchozími pro aktualizaci podgrafu
    previous_nodes <- previous_selected_nodes()
    message("Předchozí vybrané geny (previous_selected_nodes): ", paste(previous_nodes, collapse = ", "))
    if (!identical(combined_genes, previous_nodes)) {
        previous_selected_nodes(combined_genes)

        if (length(combined_genes) == 0) {
            message("Žádné geny nejsou vybrány. Odesílám prázdný podgraf.")
            empty_json <- toJSON(list(elements = list(nodes = list(), edges = list())), auto_unbox = TRUE)
            session$sendCustomMessage("cy-subset", empty_json)
        } else {
            message("Připravuji podgraf pro vybrané geny.")
            subnetwork_json <- prepare_cytoscape_network(interactions(), combined_genes, tissue_dt()[feature_name %in% combined_genes, log2FC])
            session$sendCustomMessage("cy-subset", subnetwork_json)
        }
    } else {
        message("Výběr uzlů se nezměnil. Podgraf se neaktualizuje.")
    }
  })

   observeEvent(selected_genes(), {
     session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = selected_genes()))
   })

    
    #########
    
    # Přidání nových genů do pickerInput po stisknutí tlačítka "Add Genes"
    observeEvent(input$confirm_new_genes_btn, {
      new_genes <- input$new_genes
      
      if (!is.null(new_genes) && length(new_genes) > 0 && is.character(new_genes)) {
        message("Přidání nových genů: ", new_genes)
        
        # Rozdělení a odstranění prázdných mezer
        new_genes <- trimws(unlist(strsplit(new_genes, ",")))
        new_genes <- new_genes[new_genes != ""]
        
        # Kombinace aktuálních vybraných genů (včetně těch z grafu) s novými geny
        combined_genes <- unique(c(selected_genes(), input$cySelectedNodes, new_genes))
        selected_genes(combined_genes)
        
        # Aktualizace textového pole
        updateTextAreaInput(session, "select_geneList", value = paste(combined_genes, collapse = "\n"))
        
        # Vymazání a skrytí textového pole pro nové geny
        updateTextAreaInput(session, "new_genes", value = "")
        # hide("new_genes")
        # hide("confirm_new_genes_btn")
      }
    })

   # Aktualizace pickerInput na základě aktuálně vybraných genů
   observe({
     req(selected_genes()) # Kontrola, zda jsou vybrané geny
     current_genes <- selected_genes()
     
     # Aktualizace pickerInput s aktuálně dostupnými geny k odstranění
     updatePickerInput(session, "remove_genes", choices = current_genes, selected = NULL)
   })
   
   
   # Odebrání vybraných genů z pickerInput
   observeEvent(input$confirm_remove_genes_btn, {
     genes_to_remove <- input$remove_genes
     
     if (!is.null(genes_to_remove) && length(genes_to_remove) > 0) {
       # Odebrání genů z aktuálního seznamu
       remaining_genes <- setdiff(selected_genes(), genes_to_remove)
       selected_genes(remaining_genes)
       
       # Aktualizace grafu
       session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = remaining_genes))
     } else {
       message("No genes selected for removal.")
     }
   })
   
    # 
    # # Sleduje změny ve vstupu select_geneList a aktualizuje pickerInput pro odstraňování genů
    # observeEvent(input$select_geneList, {
    #   # Kontrola nulového vstupu a zpracování pouze, pokud je vstup neprázdný
    #   if (!is.null(input$select_geneList) && nzchar(input$select_geneList)) {
    #     # Získání seznamu genů z textového pole a odstranění prázdných hodnot
    #     selected_genes <- trimws(unlist(strsplit(input$select_geneList, "\n")))
    #     selected_genes <- selected_genes[selected_genes != ""]  # Odstraní prázdné hodnoty
    #     
    #     # Aktualizace pickerInput s aktuálními geny, které lze odstranit
    #     updatePickerInput(session, "remove_genes", choices = selected_genes)
    #   } else {
    #     # Když je vstup prázdný, nastaví pickerInput na prázdné hodnoty
    #     updatePickerInput(session, "remove_genes", choices = character(0))
    #   }
    # })
    # 
    # observeEvent(input$add_genes_btn, {
    #   toggle("new_genes")
    #   toggle("confirm_new_genes_btn")
    # })
    # 
    # # Zobrazení a nastavení výběru pro pickerInput s možností potvrzení
    # observeEvent(input$remove_genes_btn, {
    #   # Nastaví všechny aktuálně vybrané geny pro odstranění
    #   current_genes <- trimws(unlist(strsplit(input$select_geneList, "\n")))
    #   current_genes <- current_genes[current_genes != ""]  # Odstraní prázdné hodnoty
    # 
    #   updatePickerInput(session, "remove_genes", choices = current_genes, selected = NULL)
    # 
    #   # Zobrazí pickerInput pro výběr genů k odstranění a tlačítko pro potvrzení
    #   toggle("remove_genes")
    #   toggle("confirm_remove_genes_btn")
    # })
    # 
    # # Zpracování odstranění vybraných genů po potvrzení
    # observeEvent(input$confirm_remove_genes_btn, {
    #   # Načtení vybraných genů pro odstranění
    #   genes_to_remove <- input$remove_genes
    #   
    #   if (!is.null(genes_to_remove) && length(genes_to_remove) > 0) {
    #     # Získání aktuálního seznamu genů z textového pole
    #     current_genes <- trimws(unlist(strsplit(input$select_geneList, "\n")))
    #     current_genes <- current_genes[current_genes != ""]  # Odstraní prázdné hodnoty
    #     
    #     # Vytvoření seznamu genů, které zůstanou po odstranění
    #     remaining_genes <- setdiff(current_genes, genes_to_remove)
    #     
    #     # Aktualizace `selected_genes` s novým seznamem
    #     selected_genes(remaining_genes)
    #     
    #     # Aktualizace textAreaInput s upraveným seznamem genů
    #     updateTextAreaInput(session, "select_geneList", value = paste(remaining_genes, collapse = "\n"))
    #     updatePickerInput(session, "remove_genes", choices = remaining_genes, selected = NULL)
    #     # Odebere výběr odstraněných uzlů z hlavního grafu
    #     session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = remaining_genes))
    #     
    #     # Skrytí pickerInput a potvrzovacího tlačítka
    #     # hide("remove_genes")
    #     # hide("confirm_remove_genes_btn")
    #   }
    # })
    
    #### buttons ###
    
    observeEvent(input$selected_layout, {
      session$sendCustomMessage("cy-layout",input$selected_layout)
    })
    
    observeEvent(input$clearSelection_btn, {
      message("Clearing all selections...")
      
      selected_genes(character(0))
      updateTextAreaInput(session, "select_geneList", value = "")
      session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = character(0)))
      updatePickerInput(session, "remove_genes", choices = character(0), selected = NULL)
      empty_json <- toJSON(list(elements = list(nodes = list(), edges = list())), auto_unbox = TRUE)
      session$sendCustomMessage("cy-subset", empty_json)
      
      message("All selections cleared.")
    })
    
    observeEvent(input$selectNeighbors_btn, {
      selected_gene <- input$selected_row
      session$sendCustomMessage("select-first-neighbors", list(gene = selected_gene))
    })
    
    observeEvent(input$fitGraph_btn, {
      selected_genes <- input$cySelectedNodes
      
      if (!is.null(selected_genes) && length(selected_genes) > 0) {
        message("Fitting view to selected nodes: ", paste(selected_genes, collapse = ", "))
        session$sendCustomMessage("fit-selected-nodes", list(nodes = selected_genes))
      } else {
        message("No nodes selected, fitting view to all nodes.")
        session$sendCustomMessage("fit-selected-nodes", list(nodes = NULL)) # NULL znamená vycentrování na celý graf
      }
    })
    
  
    
    tab_server("tab", tissue_dt = tissue_dt, selected_nodes = reactive(input$cySelectedNodes))
    
  })
}



tab_UI <- function(id){
  ns <- NS(id)
  fluidRow(
    column(6,reactableOutput(ns("network_tab"))),
    column(1,),
    column(5,reactableOutput(ns("subNetwork_tab")))
  )
}
tab_server <- function(id,tissue_dt,selected_nodes) {
  moduleServer(id, function(input, output, session) {
    
    ##### reactable calling #####
    output$network_tab <- renderReactable({
      message("Rendering Reactable for network")
      reactable(tissue_dt(),
                columns = list(
                  feature_name = colDef(name = "Gene name", maxWidth = 100, filterable = TRUE),
                  geneid = colDef(name = "Ensembl id", width = 140, show = F),
                  refseq_id = colDef(name = "Refseq id", maxWidth = 80, show = F),
                  fc = colDef(name = "FC", maxWidth = 100),
                  log2FC = colDef(show = F),
                  p_adj = colDef(show = F),
                  all_kegg_paths_name = colDef(name = "Pathway name", minWidth = 140, resizable = TRUE),
                  tissue = colDef(show = F),
                  sample = colDef(show = F)
                ),
                defaultPageSize = 10,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(10, 20, 50, 100),
                onClick = JS(sprintf("function(rowInfo, column) {
                  console.log('Clicked row gene name: ' + rowInfo.row.feature_name);
                  Shiny.setInputValue('%s', { gene: rowInfo.row.feature_name }, { priority: 'event' });
                  backgroundColor: '#ADD8E6';
                }", session$ns("selected_row"))),
                striped = TRUE,
                wrap = FALSE,
                highlight = TRUE,
                outlined = TRUE)
    })
    
    observeEvent(input$selected_row, {
      selectedRow <- input$selected_row
      message("Vybraný gen z tabulky: ", selectedRow)
      
      # Pošlete zprávu do JavaScriptu pro vybrání uzlu
      session$sendCustomMessage("cy-add-node-selection", list(gene = selectedRow))
      session$sendCustomMessage("highlight-row", list(gene = selectedRow))
    })
    
    
    observe({
      req(selected_nodes())
      req(tissue_dt())
      
      if (length(selected_nodes()) > 0) {
        message("subNetwork input: ", paste(selected_nodes(), collapse = ", "))
        
        subTissue_dt <- unique(tissue_dt()[feature_name %in% selected_nodes()])
        message("##### subNettwork input: ", subTissue_dt)
        
        output$subNetwork_tab <- renderReactable({
          message("Rendering Reactable for subNetwork")
          reactable(subTissue_dt,
                    columns = list(
                      feature_name = colDef(name = "Gene name", maxWidth = 100),
                      geneid = colDef(name = "Ensembl id", width = 140, show = F),
                      refseq_id = colDef(name = "Refseq id", maxWidth = 80, show = F),
                      fc = colDef(name = "FC", maxWidth = 100),
                      log2FC = colDef(show = F),
                      p_adj = colDef(show = F),
                      all_kegg_paths_name = colDef(name = "Pathway name", minWidth = 140, resizable = TRUE),
                      tissue = colDef(show = F),
                      sample = colDef(show = F)
                    ),
                    defaultPageSize = 10,
                    showPageSizeOptions = TRUE,
                    pageSizeOptions = c(10, 20, 50, 100),
                    striped = TRUE,
                    wrap = FALSE,
                    highlight = TRUE,
                    outlined = TRUE)
        })
      } else {
        message("No nodes selected, no subnetwork table needed.")
        output$subNetwork_tab <- renderReactable({
          message("Rendering empty Reactable for subNetwork")
          reactable(data.frame())
        })
      }
    })
    

    
  })
}

# ui <- fluidPage(
#   titlePanel("Cytoscape.js v Shiny aplikaci"),
#   fui("network")
# )
# server <- function(input, output, session){
#   fserver("network")
# }
# shinyApp(ui, server, options = list(launch.browser = TRUE))


# json_to_dataframe <- function(json_str,gene_name) {
  # json_data <- fromJSON(fromJSON(network_json, simplifyVector = FALSE), simplifyVector = FALSE)
#   json_data$elements$nodes <- lapply(json_data$elements$nodes, function(node) {
#     if (node$data$name == gene_name) {
#       node$data$highlighted <- "yes"
#     }
#     return(node)
#   })
#   node_data <- lapply(json_data$elements$nodes, function(node) {
#     node$data
#   })
#   nodes_df <- bind_rows(node_data)
#   return(nodes_df)
# }
# json_to_dataframe(network_json,input$selected_row)

# library(shiny)
# library(rhino)
# library(htmltools)
# 
# fui <- function(id){
#     ns <- NS(id)
#     tagList(
# 
#       tags$head(
#         tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/cytoscape/3.30.1/cytoscape.min.js"),
#         tags$script(src = "static/js/cyjShiny_handlers.js")
#       ),
#       sidebarLayout(
#         sidebarPanel(),
#         mainPanel(
#           div(id = ns("cy"), style = "width: 600px; height: 600px;")
#         )
#       )
#     )
# }
# 
# fserver <- function(id) {
#   moduleServer(id, function(input, output, session) {
#     observe({
#       session$sendCustomMessage(type = 'initCytoscape', message = list())
#     })
#   })
# }
# 
# ui <- fluidPage(
#   titlePanel("Cytoscape.js v Shiny aplikaci"),
#   fui("network")
# )
# server <- function(input, output, session){
#   fserver("network")
# }
# shinyApp(ui, server, options = list(launch.browser = TRUE))

