# shiny::shinyAppDir(".", options = list(launch.browser = TRUE))


box::use(
  shiny[NS, moduleServer, observeEvent, observe, tagList, fluidPage, fluidRow, column, textInput, updateTextInput, actionButton, selectInput, reactive, req,reactiveVal,conditionalPanel,
        verbatimTextOutput, renderPrint,renderText,textOutput,htmlOutput,uiOutput,renderUI,icon,textAreaInput,updateTextAreaInput,isolate,isTruthy,debounce,getDefaultReactiveDomain,
        outputOptions],
  httr[GET, status_code, content],
  bs4Dash[updateTabItems,addPopover],
  htmltools[h3, h4, h6, tags, div,HTML,p],
  jsonlite[fromJSON, toJSON,read_json],
  cyjShiny[cyjShinyOutput, renderCyjShiny, cyjShiny, dataFramesToJSON, selectNodes,setNodeAttributes,selectFirstNeighbors,fit,fitSelected,clearSelection,getSelectedNodes],
  data.table[fread,setnames,as.data.table,data.table,copy,rbindlist,setDF],
  stats[aggregate,rnorm],
  readxl[read_excel],
  graph[nodes],
  reactable[reactable,colDef,renderReactable,reactableOutput,JS],
  shinyWidgets[radioGroupButtons,pickerInput,searchInput,updatePickerInput,prettySwitch,dropdown,updatePrettySwitch,actionBttn],
  shinyjs[useShinyjs],
  shinyalert[shinyalert,useShinyalert],
  utils[head]
)

# Function to generate unique container ID for a patient
getContainerId <- function(patientId, isSubset = FALSE) {
  if (is.null(patientId) || patientId == "") {
    stop("patientId is undefined or empty")
  }
  return(paste0("cy-", patientId, "-", ifelse(isSubset, "subset", "main")))
}

box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[get_tissue_list],
  app/logic/waiters[use_spinner],
  app/logic/networkGraph_helper[get_string_interactions,prepare_cytoscape_network,get_pathway_list],
  app/view/networkGraph_tables,
)


input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file")
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag)
  return(data)
}



ui <- function(id,patient) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    tags$head(
      tags$script(src = "static/js/app.min.js"),
      tags$script(src = "static/js/cytoscape_init_ai.js"),
      tags$script(HTML(sprintf("var cyContainerId = '%s'; var cySubsetContainerId = '%s'; var cySelectedNodesInputId = '%s';", 
                               ns("cyContainer"), ns("cySubsetContainer"), ns("cySelectedNodes"))))
    ),
    fluidRow(
      column(8,
             fluidRow(
               column(3,
                      div(style = "width: 75%;",
                          div(style = "display: flex; flex-direction: column; width: 100%; align-items: flex-start;", 
                              tags$label("Pathway:", style = "margin-bottom: 5px; align-self: flex-start;"),
                              pickerInput(ns("selected_pathway"), NULL, selected = "EGFR tyrosine kinase inhibitor resistance",choices = get_pathway_list("all_genes"), options = list(`live-search` = TRUE), width = "100%")),
                          div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%; margin-top: 10px;", 
                              tags$label("Choose layout:", style = "align-self: flex-start;"),
                              tags$div(id = ns("helpPopover_layout"),tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;"))),
                          selectInput(ns("selected_layout"), NULL, choices = c("cola", "fcose"), selected = "cola", width = "100%"))),
               column(3.3,
                      div(style = "display: flex; align-items: center; gap: 10px;",
                          prettySwitch(ns("selectedSomVariants"), label = "Add possibly pathogenic somatic variants", status = "primary", slim = TRUE),
                      ),
                      div(style = "display: flex; align-items: center; gap: 10px;",
                          prettySwitch(ns("selectedGermVariants"), label = "Add possibly pathogenic germline variants", status = "primary", slim = TRUE),
                      ),
                      div(style = "display: flex; align-items: center; gap: 10px;",
                          prettySwitch(ns("selectedFusions"), label = "Add selected fusions", status = "primary", slim = TRUE),
                      )) # right = TRUE, #width = "240px"
               # ,conditionalPanel(
               #   condition = sprintf("input['%s'] || input['%s']", ns("selectedGermVariants"), ns("selectedFusions")),
               #   column(6, networkGraph_tables$selectedTab_UI(ns("tab")))
               # )
             )
      ),
      # column(1,),
      column(4,
             fluidRow(
               column(6,
                      div(style = "display: flex; flex-direction: column; align-items: flex-start; width: 75%;",
                          div(style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
                              tags$label("Add new genes:"),
                              tags$div(id = ns("helpPopover_addGene"),
                                       tags$i(class = "fa fa-question fa-xs", style = "color: #2596be;")),
                          ),
                          textAreaInput(ns("new_genes"), NULL, placeholder = "Enter gene names here...", rows = 1, resize = "vertical", width = "100%"), 
                          actionButton(ns("confirm_new_genes_btn"), label = "Add Genes", icon = icon("check"), width = "100%", style = "margin-top: 10px;"))),
               column(6,
                      div(style = "display: flex; flex-direction: column; align-items: flex-start; width: 75%;",
                          tags$label("Remove genes:"),  
                          div(style = "display: flex; flex-direction: column; width: 100%;",
                              pickerInput(ns("remove_genes"), NULL, 
                                          choices = NULL, multiple = TRUE, 
                                          options = list(`live-search` = TRUE,`actions-box` = TRUE,`multiple-separator` = ", ",`none-selected-text` = "Select gene name",`width` = "100%",`virtual-scroll` = 10,`tick-icon` = "fa fa-times")), 
                              actionButton(ns("confirm_remove_genes_btn"), label = "Remove genes", icon = icon("trash-can"), width = "100%", style = "margin-top: 10px;"))))
             )
      )
    ),
    
    uiOutput(ns("js_namespace")),
    fluidRow(
      column(6,
             div(style = "display: flex; justify-content: center;",
                 actionButton(ns("clearSelection_btn"), label = "Clear selection", icon = icon("eraser")),
                 actionButton(ns("selectNeighbors_btn"), label = "Select first neighbors"),
                 actionButton(ns("fitGraph_btn"), label = "Fit graph")))
    ),
    
    fluidRow(
      column(6, div(id = getContainerId(patient), style = "width: 100%; height: 600px;")),
      column(1,),
      column(5, div(id = getContainerId(patient, TRUE), style = "width: 100%; height: 600px;"))
    ),
    fluidRow(
      column(6,div(class = "networkGraph-tissue-wrapper",
                   radioGroupButtons(ns("selected_tissue"),"Choose a tissue :",choices = get_tissue_list(),justified = TRUE)))
    ),
    fluidRow(
      column(12, networkGraph_tables$tab_UI(ns("tab")))
    )
  )
}

server <- function(id, patient, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Initialize reactive values
    interactions <- reactiveVal(NULL)
    sub_interactions <- reactiveVal(NULL)
    synchronized_nodes <- reactiveVal(character(0))
    new_genes_var <- reactiveVal(NULL)
    remove_genes_var <- reactiveVal(NULL)
    clear_all <- reactiveVal(FALSE)
    result_dt <- reactiveVal(NULL)
    selected_dt <- reactiveVal(NULL)
    
    # Load data for the current patient
    dt <- reactive({
      message("Loading input data for networkGraph for patient: ", patient)
      input_data(patient, "all_genes")
    })
    
    # Get tissue-specific data
    subTissue_dt <- reactive({
      req(input$selected_tissue)
      req(dt())
      unique(dt()[tissue == input$selected_tissue])
    })
    
    # Get pathway-specific data
    pathway_dt <- reactive({
      req(input$selected_pathway)
      req(dt())
      unique(dt()[grepl(input$selected_pathway, all_kegg_paths_name, fixed = TRUE),-c("all_kegg_gene_names","counts_tpm_round","size","mu","lower_than_p","higher_than_p","type","gene_definition")])
    })
    
    # Get tissue and pathway specific data
    tissue_dt <- reactive({
      req(input$selected_tissue)
      req(pathway_dt())
      unique(pathway_dt()[tissue == input$selected_tissue])
    })
    
    # Fetch STRING interactions
    observe({
      req(tissue_dt())
      message("Fetching STRING interactions for patient: ", patient)
      message("Genes: ", paste0(tissue_dt()[, feature_name], collapse = ","))
      
      tryCatch({
        interactions(get_string_interactions(tissue_dt()[, feature_name]))
      }, error = function(e) {
        message("Error fetching STRING interactions: ", e$message)
        interactions(NULL)
      })
    })
    
    # Prepare network JSON
    network_json <- reactive({
      req(tissue_dt(), interactions())
      message("Preparing network data for patient: ", patient)
      
      # Get the network data
      network_data <- prepare_cytoscape_network(interactions(), unique(tissue_dt()[, .(feature_name,log2FC)]))
      
      # Debug output
      message("Network data summary:")
      message("- Number of nodes: ", length(fromJSON(network_data)$elements$nodes))
      message("- Number of edges: ", length(fromJSON(network_data)$elements$edges))
      
      return(network_data)
    })
    
    # Pass namespace to JavaScript
    output$js_namespace <- renderUI({
      tags$script(HTML(sprintf("var ns = '%s';", ns(""))))
    })
    
    # Handle pathway and tissue changes
    observeEvent(list(input$selected_pathway, input$selected_tissue), {
      req(input$selected_pathway, input$selected_tissue)
      message("Selected pathway: ", input$selected_pathway, ", Selected tissue: ", input$selected_tissue, " for patient: ", patient)
      session$sendCustomMessage("cy-init", list(
        patientId = patient,
        elements = network_json(),
        layout = input$selected_layout
      ))
    })
    
    # Fetch subset interactions
    observe({
      req(subTissue_dt())
      message("Fetching subset STRING interactions for patient: ", patient)
      tryCatch({
        sub_interactions(get_string_interactions(unique(subTissue_dt()[feature_name %in% synchronized_nodes(),feature_name])))
      }, error = function(e) {
        message("Error fetching subset STRING interactions: ", e$message)
        sub_interactions(NULL)
      })
    })
    
    # Update subset graph
    observe({
      current_nodes <- synchronized_nodes()
      message("Updating subset graph for patient: ", patient, " with nodes: ", paste(current_nodes, collapse = ", "))
      
      if (length(current_nodes) == 0) {
        message("No nodes selected. Sending empty subgraph for patient: ", patient)
        empty_json <- toJSON(list(elements = list(nodes = list(), edges = list())), auto_unbox = TRUE)
        session$sendCustomMessage("cy-subset", list(
          patientId = patient,
          elements = empty_json,
          layout = input$selected_layout
        ))
        session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = list()))
      } else {
        tryCatch({
          subnetwork_json <- prepare_cytoscape_network(sub_interactions(), unique(subTissue_dt()[feature_name %in% current_nodes, .(feature_name,log2FC)]), current_nodes)
          session$sendCustomMessage("cy-subset", list(
            patientId = patient,
            elements = subnetwork_json,
            layout = input$selected_layout
          ))
          session$sendCustomMessage("update-selected-from-gene-list", list(selected_nodes = current_nodes))
        }, error = function(e) {
          message("Error preparing subnetwork: ", e$message)
        })
      }
    })
    
    ##################################
    ## Network node synchronization ##
    ##################################
    
    sync_nodes <- function(nodes_from_graph, current_genes, add_genes = NULL, remove_genes = NULL, clear_all) {
      
      ifelse(clear_all, combined <- character(0), combined <- unique(c(nodes_from_graph, current_genes)))
      
      if (!is.null(add_genes) && length(add_genes) > 0) combined <- unique(c(combined, add_genes))  # Přidání nových genů
      if (!is.null(remove_genes) && length(remove_genes) > 0) combined <- setdiff(combined, remove_genes)  # Odebrání genů
      
      combined <- combined[!is.na(combined) & combined != ""]  # Odstranění prázdných hodnot
      changed <- !setequal(synchronized_nodes(), combined)  # Kontrola změny
      
      return(list(updated_nodes = combined, changed = changed))  # Návrat aktualizovaných uzlů a informace o změně
    }
    
    
    ########################
    ## Network UI buttons ##
    ########################
    
    observeEvent(input$selected_layout, {
      session$sendCustomMessage("cy-layout", list(
        patientId = patient,
        layout = input$selected_layout
      ))
    })
    
    observeEvent(input$clearSelection_btn, {
      session$sendCustomMessage("clear-selection", list(
        patientId = patient
      ))
    })
    
    observeEvent(input$selectNeighbors_btn, {
      session$sendCustomMessage("select-first-neighbors", list(
        patientId = patient
      ))
    })
    
    observeEvent(input$fitGraph_btn, {
      selected_genes <- input$cySelectedNodes
      
      if (!is.null(selected_genes) && length(selected_genes) > 0) {
        message("Fitting view to selected nodes: ", paste(selected_genes, collapse = ", "))
        session$sendCustomMessage("fit-selected-nodes", list(
          patientId = patient,
          nodes = selected_genes
        ))
      } else {
        message("No nodes selected, fitting view to all nodes.")
        session$sendCustomMessage("fit-selected-nodes", list(
          patientId = patient,
          nodes = NULL
        )) # NULL znamená vycentrování na celý graf
      }
    })

    #################################################
    ### Selected variant or fusion data + buttons ###
    #################################################
    
    observe({
      germ_vars <- as.data.table(shared_data$germline_data())
      fusions <- as.data.table(shared_data$fusion_data())
      print(germ_vars)
      print(fusions)
      tissue_table <- copy(tissue_dt())  # Výchozí tabulka
      
      # Výchozí hodnota pro selected_dt
      selected_dt(NULL)
      
      if ((is.null(germ_vars) || nrow(germ_vars) == 0) &&
          (is.null(fusions) || nrow(fusions) == 0)) {
        # Ani varianty, ani fúze nejsou vybrány
        message("No germline variants or fusions selected.")
        result_dt(tissue_table)
      } else {
        # Kombinujeme varianty a fúze do základní tabulky
        if (!is.null(germ_vars) && nrow(germ_vars) > 0) {
          # Zpracování germline variant
          selected_variants <- germ_vars[, .(Gene_symbol, variant = var_name)]
          selected_variants <- unique(selected_variants, by = "Gene_symbol")
          tissue_table <- merge(tissue_table, selected_variants, by.x = "feature_name", by.y = "Gene_symbol", all.x = TRUE)
        }
        
        if (!is.null(fusions) && nrow(fusions) > 0) {
          # Zpracování fúzí
          fusions <- fusions[, .(Gene_symbol = c(gene1, gene2), fusion = paste(paste(gene1, gene2, sep = "-"), collapse = ", "))]
          fusions <- unique(fusions, by = "Gene_symbol")
          tissue_table <- merge(tissue_table, fusions, by.x = "feature_name", by.y = "Gene_symbol", all.x = TRUE)
        }
        
        # Aktualizace result_dt
        result_dt(tissue_table)
        message("Updated result_dt with germline variants and/or fusions.")
      }
      
      # Vytvoření selected_dt
      if ((!is.null(germ_vars) && nrow(germ_vars) > 0) || (!is.null(fusions) && nrow(fusions) > 0)) {
        selected_variants <- if (!is.null(germ_vars) && nrow(germ_vars) > 0) {
          unique(germ_vars[,var_name := "yes"])
        } else {
          data.frame(Gene_symbol = character(0))
        }
        
        selected_fusions <- if (!is.null(fusions) && nrow(fusions) > 0) {
          unique(fusions[,fusion := "yes"])
        } else {
          data.frame(Gene_symbol = character(0))
        }
        combined_selected <- merge(selected_variants, selected_fusions,by = "Gene_symbol", all = TRUE)
        pathways_info <- subTissue_dt()[feature_name %in% combined_selected$Gene_symbol, .(Gene_symbol = feature_name, all_kegg_paths_name)]
        combined_selected <- merge(combined_selected, pathways_info, by = "Gene_symbol", all.x = TRUE)
        setDF(combined_selected)  # Převod na data frame
        
        # Aktualizace selected_dt
        selected_dt(combined_selected)
      } else {
        selected_dt(data.frame(Gene_symbol = character(0), variant = character(0), fusion = character(0), all_kegg_paths_name = character(0)))
      }
    })
    
    
    observeEvent(list(input$selectedGermVariants, input$selectedFusions, input$selected_pathway, input$selected_tissue), {
      # Aktualizace pro germline varianty
      if (input$selectedGermVariants) {
        if ("var_name" %in% names(selected_dt())) {
          selected_dt <- as.data.table(selected_dt())
          germline_variants <- as.character(unique(selected_dt[!is.na(var_name), Gene_symbol]))
          
          print(germline_variants)
          if (length(germline_variants) == 0) {
            updatePrettySwitch(session, "selectedGermVariants", value = FALSE) # Reset prettySwitch na FALSE
            
            shinyalert(
              title = "No variants selected",
              text = "You don't have any variants selected.",
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "OK",
              cancelButtonText = "Go to variant",
              callbackR = function(value) {
                # value bude TRUE pro OK, FALSE pro "Go to variant"
                if (!value) {
                  # updateTabItems(session = session$userData$parent_session,  # použijeme parent session
                  #                inputId = "sidebar_menu",  # bez namespace
                  #                selected = "fusion_genes")
                }})
          } else {
            message("Adding border for germline variant nodes:", paste(germline_variants, collapse = ", "))
            session$sendCustomMessage("variant-border", list(
              patientId = patient,
              type = "germline",
              nodes = germline_variants
            ))
          }
        } else {
          # Reset prettySwitch na FALSE
          updatePrettySwitch(session, "selectedGermVariants", value = FALSE)
          
          shinyalert(
            title = "No variants selected",
            text = "No germline variants are currently selected as possibly pathogenic.",
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonText = "OK",
            cancelButtonText = "Go to variant",
            callbackR = function(value) {
              if (!value) {}})
        }
        
      } else {
        message("Removing border for germline variant nodes.")
        session$sendCustomMessage("variant-border", list(
          patientId = patient,
          type = "germline",
          nodes = character(0)
        ))
      }
      
      # Aktualizace pro fúze
      if (input$selectedFusions) {
        
        # Kontrola existence sloupce fusion
        if ("fusion" %in% names(selected_dt())) {
          # fusion_nodes <- selected_dt()[!is.na("fusion"), "Gene_symbol"]
          selected_dt <- as.data.table(selected_dt())
          fusion_nodes <- as.character(unique(selected_dt[!is.na(fusion), Gene_symbol]))
          
          if (length(fusion_nodes) == 0) {
            updatePrettySwitch(session, "selectedFusions", value = FALSE)
            shinyalert(
              title = "No fusions selected",
              text = "You don't have any fusions selected.",
              type = "warning",
              showCancelButton = TRUE,
              confirmButtonText = "OK",
              cancelButtonText = "Go to fusion",
              callbackR = function(value) {
                if (!value) {}}
            )
          } else {
            message("Adding border for fusion nodes:", paste(fusion_nodes, collapse = ", "))
            session$sendCustomMessage("variant-border", list(
              patientId = patient,
              type = "fusion",
              nodes = fusion_nodes
            ))
            print(selected_dt())
          }
        } else {
          updatePrettySwitch(session, "selectedFusions", value = FALSE)
          shinyalert(
            title = "No fusions selected",
            text = "No fusion genes are currently selected as possibly pathogenic.",
            type = "warning",
            showCancelButton = TRUE,
            confirmButtonText = "OK",
            cancelButtonText = "Go to fusion",
            callbackR = function(value) {
              if (!value) {}})
        }
      } else {
        message("Removing border for fusion nodes.")
        session$sendCustomMessage("variant-border", list(
          patientId = patient,
          type = "fusion",
          nodes = character(0)
        ))
      }
    })
    
    ##############
    ### others ###
    ##############
    
    networkGraph_tables$tab_server(ns("tab"), tissue_dt = reactive(result_dt()), subTissue_dt, selected_nodes = synchronized_nodes, selected_dt, patient)
    
    addPopover(id = "helpPopover_addGene",options = list(title = "Write comma-separated text:",content = "example: BRCA1, TP53, FOXO3",placement = "right",trigger = "hover"))
    addPopover(id = "helpPopover_layout",options = list(title = "Layout options:",placement = "right",trigger = "hover",
                                                        content = "cola – Ideal for hierarchical structures and smaller graphs. FCOSE – Best for large and complex networks."))
    
    # Remove duplicate container definitions
    output$cy <- renderUI({ NULL })
    output$cySubset <- renderUI({ NULL })
  })
}


