# app/view/germline_var_call_table.R

#########################################################################################################
## pozn. reactable.extras je opravdu rychlejší, ale nefungují nějaké reactable parametry jako např. 
## showPageSizeOptions, pageSizeOptions a defaultPageSize
## Také nefunguje balík jako reactablefmtr, což je velká škoda. 
#########################################################################################################

# dt <- openxlsx::read.xlsx("../AK1860krev.germ_variants.xlsx")

box::use(
  shiny[moduleServer,NS,h2,h3,tagList,div,tabsetPanel,tabPanel,observeEvent,fluidPage,fluidRow, reactive,icon,textInput,isTruthy,verbatimTextOutput,
        sliderInput,showModal,modalDialog,modalButton,column,uiOutput,renderUI,textOutput,renderText,reactiveVal,req,observe,outputOptions,checkboxInput,
        renderPrint,getDefaultReactiveDomain],
  bs4Dash[actionButton, box,popover,addPopover],
  reactable,
  reactable[reactable,reactableOutput,renderReactable,colDef,colGroup,JS,getReactableState],
  # reactable.extras[reactable_extras_ui,reactable_extras_server],
  htmltools[tags,HTML],
  app/logic/patients_list[set_patient_to_sample],
  shinyWidgets[prettyCheckbox,searchInput,pickerInput, dropdown,actionBttn,pickerOptions],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[data.table,uniqueN],
  # reactablefmtr
)

box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_germline_table],
  app/logic/patients_list[sample_list_germ],
  app/logic/waiters[use_spinner],
  app/logic/reactable_helpers[selectFilter,minRangeFilter,filterMinValue,generate_columnsDef]
)

# Load and process data table
input_data <- function(sample){
  filenames <- get_inputs("per_sample_file")
  message("Loading data for germline: ", filenames$var_call.germline)
  data <- prepare_germline_table(load_data(filenames$var_call.germline,"varcall",sample))
  return(data)
}


ui <- function(id) {
  ns <- NS(id)
  useShinyjs()
  tagList(
    use_spinner(reactableOutput(ns("germline_var_call_tab"))),
    tags$br(),
    tags$div(id = ns("checkbox_popover"), style = "width:245px; position: absolute; right: 10;", #margin-top: 13.5px;
             checkboxInput(ns("fullTable_checkbox"),label = "Keep pre-filtered variant table",value = TRUE)),
    tags$br(),
    div(style = "display: flex; justify-content: space-between; align-items: top; width: 100%;",
      div(
        actionButton(ns("selectPathogenic_button"), "Select variants as possibly pathogenic", status = "info"),
        tags$br(),
        fluidRow(
          column(5,reactableOutput(ns("selectPathogenic_tab")))),
        tags$br(),
        fluidRow(
          column(1,actionButton(ns("delete_button"), "Delete variants", icon = icon("trash-can"))))
      ),
      dropdown(ns("igv_dropdownButton"), label = "IGV", status = "primary", icon = icon("play"), right = TRUE, size = "md",#width = 230, 
               pickerInput(ns("idpick"), "Select patients for IGV:", choices = sample_list_germ(), options = pickerOptions(actionsBox = FALSE, size = 4, maxOptions = 4, dropupAuto = FALSE, maxOptionsText = "Select max. 4 patients"),multiple = TRUE),
               div(style = "display: flex; justify-content: center; margin-top: 10px;",
                   actionBttn(ns("go2igv_button"), label = "Go to IGV", style = "stretch", color = "primary", size = "sm", individual = TRUE)
               )
      )
    )
  )
}

server <- function(id, selected_samples, selected_columns, column_mapping, selection_enabled, shared_data) {
  moduleServer(id, function(input, output, session) {
    

    
    # Call loading function to load data
    data <- reactive({
      message("Loading input data for germline")
      input_data(selected_samples)
    })
    
    observe({
        req(data())
        overview_dt <- data.table(
            clinvar_N = uniqueN(data()[clinvar_sig %in% c("Pathogenic", "Likely_pathogenic", "Pathogenic/Likely_pathogenic",
                                                          "Pathogenic_(VUS)", "Likely_pathogenic (VUS)", "Pathogenic_(VUS)")]),
            for_review = uniqueN(data()[gnomAD_NFE <= 0.01 & coverage_depth > 10 & Consequence != "synonymous_variant" &
                                          (gene_region == "exon" | gene_region == "splice")]))
        shared_data$germline_overview[[ selected_samples ]] <- overview_dt
    })

    
    # Call generate_columnsDef to generate colDef setting for reactable
    column_defs <- reactive({
      req(selected_columns())
      generate_columnsDef(names(filtered_data()), selected_columns(), "germline", column_mapping, session)
    })
    
    filtered_data <- reactive({
      message("Before filtering")
      print(input$fullTable_checkbox)
      
      dt <- data()
      
      if (isTruthy(input$fullTable_checkbox)) {
        dt <- dt[gnomAD_NFE <= 0.01 & coverage_depth > 10 & Consequence != "synonymous_variant"]
        dt <- dt[gene_region == "exon" | gene_region == "splice"]
        message("Filtered data for germline")
      } else {
        message("Full data for germline")
      }
      as.data.frame(dt)
    })
    
    # # Reactive value to store selected rows
    selected_variants <- reactiveVal(data.frame(patient = character(),var_name = character(), Gene_symbol = character()))
    
    # Render reactable with conditional selection
    output$germline_var_call_tab <- renderReactable({
      message("Rendering Reactable for germline")
      filtered_data <- filtered_data() # tvoje data pro hlavní tabulku
      pathogenic_variants <- selected_variants() # seznam variant, které byly označeny jako patogenní
      
      
      reactable(
        # filtered_data(),
        filtered_data,
        columns = column_defs(),
        resizable = TRUE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 20, 50, 100),
        defaultPageSize = 20,
        striped = TRUE,
        wrap = FALSE,
        highlight = TRUE,
        outlined = TRUE,
        defaultColDef = colDef(align = "center", sortNALast = TRUE),
        defaultSorted = list("CGC_Germline" = "desc", "trusight_genes" = "desc", "fOne" = "desc"),
        rowStyle = function(index) {
          gene_in_row <- filtered_data$Gene_symbol[index]
          var_in_row <- filtered_data$var_name[index]
          if (var_in_row %in% pathogenic_variants$var_name &           # Pokud je aktuální řádek v seznamu patogenních variant, zvýrazníme ho
              gene_in_row %in% pathogenic_variants$Gene_symbol) {
            list(backgroundColor = "#B5E3B6",fontWeight = "bold")
          } else {
            NULL
          }
        },
        # columnGroups = list(
        #   colGroup(name = "Databases", columns = c("gnomAD_NFE", "clinvar_sig", "snpDB", "CGC_Germline", "trusight_genes", "fOne")),
        #   colGroup(name = "Annotation", columns = c("Consequence", "HGVSc", "HGVSp", "all_full_annot_name"))
        # ),
        selection = "multiple",
        onClick = JS("function(rowInfo, column, event) {
                        if (event.target.classList.contains('rt-expander') || event.target.classList.contains('rt-expander-button')) {
                        } else {
                            rowInfo.toggleRowSelected();}}"),
        class = "germline-table",
        elementId = "tbl-germline"
      )
    })
    
    # Sledování vybraného řádku a varianty
    selected_variant <- reactive({
      selected_row <- getReactableState("germline_var_call_tab", "selected")
      req(selected_row)
      
        filtered_data()[selected_row, c("var_name","Gene_symbol")]  # Získání varianty z vybraného řádku
        message("data germline tab: ", filtered_data()[selected_row, c("var_name","Gene_symbol")])
        # var <- filtered_data()[selected_row, c("var_name","Gene_symbol")]  # Získání varianty z vybraného řádku
        # var$remove <- NA
    
    })
    
    # Akce po kliknutí na tlačítko pro přidání varianty
    observeEvent(input$selectPathogenic_button, {
      selected_rows <- getReactableState("germline_var_call_tab", "selected")
      req(selected_rows)
      
      new_variants <- filtered_data()[selected_rows, c("var_name", "Gene_symbol","variant_freq","coverage_depth", "Consequence",
                                                       "HGVSc","HGVSp","variant_type","Feature", "clinvar_sig","gnomAD_NFE")]  # Získání vybraných variant
      new_variants$sample <- selected_samples

      current_variants <- selected_variants()  # Stávající přidané varianty
      new_unique_variants <- new_variants[!(new_variants$var_name %in% current_variants$var_name &       # Porovnání - přidáme pouze ty varianty, které ještě nejsou v tabulce
                                              new_variants$Gene_symbol %in% current_variants$Gene_symbol), ]

      if (nrow(new_unique_variants) > 0) selected_variants(rbind(current_variants, new_unique_variants))
      
      # Aktualizace globální proměnné shared_data$germline_var:
      global_data <- shared_data$germline_var()
      
      if (is.null(global_data) || nrow(global_data) == 0 || !("sample" %in% names(global_data))) {
        global_data <- data.table(
          sample = character(),
          var_name = character(),
          Gene_symbol = character(),
          variant_freq= character(),
          coverage_depth = character(),
          Consequence = character(),
          HGVSc = character(),
          HGVSp = character(),
          variant_type = character(),
          Feature = character(),
          clinvar_sig = character(), #(round(variant_freq * coverage_depth))/coverage_depth
          gnomAD_NFE = character()
        )
      }
      message("## selected_variants(): ", selected_variants())
      message("## global_data: ", global_data)
      # Odstraníme data, která patří právě tomuto pacientovi
      global_data <- global_data[sample != selected_samples]
      
      # Přidáme nově aktualizované lokální data daného pacienta
      updated_global_data <- rbind(global_data, selected_variants())
      shared_data$germline_var(updated_global_data)
      message("## shared_data$germline_var(): ", shared_data$germline_var())
    })
    


    output$selectPathogenic_tab <- renderReactable({
      variants <- selected_variants()
      if (nrow(variants) == 0) {
        return(NULL)
      }
      
      reactable(
        variants,
        columns = list(
          var_name = colDef(name = "Variant name"),
          Gene_symbol = colDef(name = "Gene name")
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
      rows <- getReactableState("selectPathogenic_tab", "selected")
      req(rows)
      current_variants <- selected_variants()
      updated_variants <- current_variants[-rows, ]
      selected_variants(updated_variants)
      shared_data$germline_var(updated_variants)
      session$sendCustomMessage("resetReactableSelection",selected_variants())
      
      if (nrow(selected_variants()) == 0) {
        hide("confirm_btn")
        hide("delete_button")
      }
    })

    # variant_selected <- reactiveVal(FALSE)
    
    # Při stisku tlačítka pro výběr fúze
    observeEvent(input$selectPathogenic_button, {
      if (nrow(selected_variants()) == 0) {
        # Pokud nejsou vybrány žádné řádky, zůstaň u původního stavu
        # variant_selected(FALSE)
        hide("confirm_btn")
        hide("delete_button")
        
        shinyalert(
          title = "No variant selected",
          text = "Please select the potentially pathogenic variants from table above.",
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
        # variant_selected(TRUE)
        
        # Zobraz tlačítka pomocí shinyjs
        show("confirm_btn")
        show("delete_button")
      }
    })
    
    hide("confirm_btn")
    hide("delete_button")
    
    
    addPopover(id = "checkbox_popover",
               options = list(
                 title = "Current filters:",
                 content = "gnomAD NFE <= 0.01, Coverage > 10, Consequence w/o synonymous_variant, Gene region is exon or splice",
                 placement = "bottom",
                 trigger = "hover"))
    
    
    
    #############
    ## run IGV ##
    #############
    
    observeEvent(input$go2igv_button, {
      selected_empty <- is.null(selected_variants()) || nrow(selected_variants()) == 0
      bam_empty <- is.null(shared_data$germline_bam) || length(shared_data$germline_bam) == 0
      
      if (selected_empty || bam_empty) {
        showModal(modalDialog(
          title = "Missing input",
          "You have not selected variants or patients for visualization. Please return to the Germline variant calling tab and define them.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        
      } else {
        shared_data$navigation_context("germline")   # odkud otevíráme IGV
        
        bam_path <- get_inputs("bam_file")
        bam_list <- lapply(input$idpick, function(id_val) {
            full_path <- grep(paste0(id_val, ".*\\.bam$"), bam_path$dna.normal_bam, value = TRUE)
            list(name = id_val, file = sub(bam_path$path_to_folder, ".", full_path, fixed = TRUE))  # relativní cesta)
        })
        
        shared_data$germline_bam(bam_list)
        message("✔ Assigned germline_bam: ",paste(sapply(bam_list, `[[`, "file"), collapse = ", "))
        
        shinyjs::runjs("document.querySelector('[data-value=\"app-hidden_igv\"]').click();")
      }
    })
    
    
    
  })
}





