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
        renderPrint,getDefaultReactiveDomain,selectInput,downloadButton,numericInput,updateNumericInput],
  bs4Dash[actionButton, box,popover,addPopover],
  reactable,
  reactable[reactable,reactableOutput,renderReactable,colDef,colGroup,JS,getReactableState],
  # reactable.extras[reactable_extras_ui,reactable_extras_server],
  htmltools[tags,HTML],
  app/logic/patients_list[set_patient_to_sample],
  shinyWidgets[prettyCheckbox,prettyCheckboxGroup,updatePrettyCheckboxGroup,searchInput,pickerInput, dropdown,actionBttn,pickerOptions,dropdownButton],
  shinyalert[shinyalert,useShinyalert],
  shinyjs[useShinyjs,hide,show],
  data.table[data.table,as.data.table,uniqueN],
  stats[setNames],
  # reactablefmtr
)

box::use(
  app/logic/load_data[get_inputs,load_data],
  app/logic/prepare_table[prepare_germline_table],
  app/logic/patients_list[sample_list_germ],
  app/logic/waiters[use_spinner],
  app/logic/reactable_helpers[selectFilter,minRangeFilter,filterMinValue,create_clinvar_filter,create_consequence_filter],
  app/logic/filter_columns[getColFilterValues,map_checkbox_names,colnames_map_list,generate_columnsDef]
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
    fluidRow(
      div(style = "width: 100%; text-align: right;",
        dropdownButton(label = NULL,right = TRUE,width = "240px",icon = HTML('<i class="fa-solid fa-download download-button"></i>'),
                       selectInput(ns("export_data_table"), "Select data:", choices = c("All data" = "all", "Filtered data" = "filtered")),
                       selectInput(ns("export_format_table"), "Select format:", choices = c("CSV" = "csv", "TSV" = "tsv", "Excel" = "xlsx")),
                       downloadButton(ns("Table_download"),"Download")),
        uiOutput(ns("filterTab")))),
    use_spinner(reactableOutput(ns("germline_var_call_tab"))),
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

server <- function(id, selected_samples, shared_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Call loading function to load data
    data <- reactive({
      message("Loading input data for germline")
      input_data(selected_samples)
    })
    
    # observe({
    #     req(data())
    #     overview_dt <- data.table(
    #         clinvar_N = uniqueN(data()[clinvar_sig %in% c("Pathogenic", "Likely_pathogenic", "Pathogenic/Likely_pathogenic",
    #                                                       "Pathogenic_(VUS)", "Likely_pathogenic (VUS)", "Pathogenic_(VUS)")]),
    #         for_review = uniqueN(data()[gnomAD_NFE <= 0.01 & coverage_depth > 10 & Consequence != "synonymous_variant" &
    #                                       (gene_region == "exon" | gene_region == "splice")]))
    #     shared_data$germline_overview[[ selected_samples ]] <- overview_dt
    # })

    colnames_list <- getColFilterValues("germline") # gives list of all_columns and default_columns
    map_list <- colnames_map_list("germline") # gives list of all columns with their column definitions
    mapped_checkbox_names <- map_checkbox_names(map_list) # gives list of all columns with their display names for checkbox
    
    
  
    output$filterTab <- renderUI({
      req(data())
      req(map_list)
      filterTab_ui(ns("filterTab_dropdown"),data(), colnames_list$default_columns, mapped_checkbox_names)
    })
    
    
    filter_state <- filterTab_server("filterTab_dropdown",colnames_list)
    
    selected_coverage_depth <- reactiveVal(NULL)
    selected_gnomAD_min  <- reactiveVal(NULL)
    selected_gene_region <- reactiveVal(NULL)
    selected_clinvar_sig <- reactiveVal(NULL)
    selected_consequence <- reactiveVal(NULL)
    selected_columns <- reactiveVal(colnames_list$default_columns)
    selected_variants <- reactiveVal(data.frame(patient = character(),var_name = character(), Gene_symbol = character()))

    
    column_defs <- reactive({
      req(data())
      req(selected_columns())
      generate_columnsDef(names(data()), selected_columns(), "germline", map_list)
    })
    
    filtered_data <- reactive({
      req(data())
      dt <- data()

      if (!is.null(selected_coverage_depth())) {
        dt <- dt[selected_coverage_depth() <= coverage_depth, ]
      }
      if (!is.null(selected_gnomAD_min())) {
        dt <- dt[gnomAD_NFE <= selected_gnomAD_min()]
      }
      if (!is.null(selected_gene_region()) && length(selected_gene_region()) > 0) {
        dt <- dt[gene_region %in% selected_gene_region(), ]
      }
      if (!is.null(selected_clinvar_sig()) && length(selected_clinvar_sig()) > 0) {
        dt <- create_clinvar_filter(dt, selected_clinvar_sig())
      }
      if (!is.null(selected_consequence()) && length(selected_consequence()) > 0) {
        dt <- create_consequence_filter(dt, selected_consequence())
      }
      return(dt)
    })

    
    # Render reactable with conditional selection
    output$germline_var_call_tab <- renderReactable({
      req(filtered_data())
      req(column_defs())
      message("Rendering Reactable for germline")
      filtered_data <- filtered_data() # tvoje data pro hlavní tabulku
      pathogenic_variants <- selected_variants() # seznam variant, které byly označeny jako patogenní
      
      
      reactable(
        as.data.frame(filtered_data),
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
        class = "germline-table"
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

    # Při stisku tlačítka pro výběr varianty
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
    
    
    
    observeEvent(filter_state$confirm(), {
      message("🟢 Confirm button was clicked")
      selected_coverage_depth(filter_state$coverage_depth())
      selected_gnomAD_min(filter_state$gnomAD_min())
      selected_gene_region(filter_state$gene_region())
      selected_clinvar_sig(filter_state$clinvar_sig())
      selected_consequence(filter_state$consequence())
      selected_columns(filter_state$selected_columns())
    })

    
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





filterTab_server <- function(id,colnames_list) {
  moduleServer(id, function(input, output, session) {
    
    observe({
      if(isTruthy(is.na(input$coverage_depth))) updateNumericInput(session, "coverage_depth", value = 10)
    })
    observe({
      if(isTruthy(is.na(input$gnomAD_min))) updateNumericInput(session, "gnomAD_min", value = 0.01)
    })
    
    
    observeEvent(input$show_all, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$all_columns)
    })
    
    observeEvent(input$show_default, {
      updatePrettyCheckboxGroup(session, "colFilter_checkBox", selected = colnames_list$default_columns)
    })
    
    return(list(
      confirm = reactive(input$confirm_btn),
      coverage_depth = reactive(input$coverage_depth),
      gnomAD_min = reactive(input$gnomAD_min),
      gene_regions = reactive(input$gene_regions),
      clinvar_sig = reactive(input$clinvar_sig),
      consequence = reactive(input$consequence),
      selected_columns = reactive(input$colFilter_checkBox)
      
    ))
  })
}


filterTab_ui <- function(id,data, default_columns, mapped_checkbox_names){
  ns <- NS(id)
  
  filenames <- get_inputs("per_sample_file")
  file_paths <- filenames$var_call.germline[1]
  patient_names <- substr(basename(file_paths), 1, 6)
  consequence_split <- unique(unlist(unique(data$consequence_trimws)))
  consequence_list <- sort(unique(ifelse(is.na(consequence_split) | consequence_split == "", "missing value", consequence_split)))
  clinvar_split <- unique(unlist(unique(data$clinvar_trimws)))
  clinvar_list <- sort(unique(ifelse(is.na(clinvar_split) | clinvar_split == "", "missing value", clinvar_split)))
  
  tagList(
    tags$head(tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css"),
              tags$style(HTML(".dropdown-toggle {border-radius: 0; padding: 0; background-color: transparent; border: none; float: right;margin-top -1px;}
                    .checkbox label {font-weight: normal !important;}
                    .checkbox-group .checkbox {margin-bottom: 0px !important;}
                    .my-blue-btn {background-color: #007bff;color: white;border: none;}
                    .dropdown-menu .bootstrap-select .dropdown-toggle {border: 1px solid #ced4da !important; background-color: #fff !important;
                      color: #495057 !important; height: 38px !important; font-size: 16px !important; border-radius: 4px !important;
                      box-shadow: none !important;}
                    .sw-dropdown-content {border: 1px solid #ced4da !important; border-radius: 4px !important; box-shadow: none !important;
                      background-color: white !important;}
                    .glyphicon-triangle-bottom {font-size: 12px !important; line-height: 12px !important; vertical-align: middle;}
                    .glyphicon-triangle-bottom {display: none !important; width: 0 !important; margin: 0 !important; padding: 0 !important;}
                    #app-germline_var_call_tab-igv_dropdownButton {width: 230px !important; height: 38px !important; font-size: 16px !important;}
                    "))
    ),
    dropdownButton(
      label = NULL,
      right = TRUE,
      # width = "480px",
      icon = HTML('<i class="fa-solid fa-filter download-button"></i>'),
      
      fluidRow(style = "display: flex; align-items: stretch;",
               column(8,
                      box(width = 12,title = tags$div(style = "padding-top: 8px;","Filter data by:"),closable = FALSE, collapsible = FALSE,style = "height: 100%;",
                          fluidRow(
                            column(6, numericInput(ns("coverage_depth"), tags$strong("Coverage min"), value = 10, min = 0, max = 1000)),
                            column(6, numericInput(ns("gnomAD_min"), tags$strong("gnomAD NFE min"), value = 0.01, min = 0, max = 1))
                          ),
                          div(class = "card-body two-col-checkbox-group",
                            div(
                              div(class = "two-col-checkbox-group", style = "margin-bottom: 15px;",
                                  prettyCheckboxGroup(ns("gene_regions"), label = tags$strong("Gene region"), icon = icon("check"), status = "primary", outline = FALSE,
                                                      choices = unique(data$gene_region),selected = c("exon","intron"))),#unique(data$gene_region)
                              div(class = "two-col-checkbox-group",
                                  prettyCheckboxGroup(ns("clinvar_sig"),label = tags$strong("ClinVar significance"),icon = icon("check"),status = "primary",outline = FALSE,
                                                      selected = unique(clinvar_list),
                                                      choices = setNames(clinvar_list, clinvar_list)))),
                            div(class = "two-col-checkbox-group",
                                prettyCheckboxGroup(ns("consequence"),label = tags$strong("Consequence"),icon = icon("check"),status = "primary",outline = FALSE,
                                                    selected = setdiff(consequence_list, "synonymous_variant"),
                                                    choices = setNames(consequence_list, consequence_list)))
                      ))
               ),
               column(4,
                      box(width = 12,title = tags$div(style = "padding-top: 8px;","Select columns:"),closable = FALSE,collapsible = FALSE,height = "100%",
                          div(class = "two-col-checkbox-group",
                              prettyCheckboxGroup(
                                inputId = ns("colFilter_checkBox"),
                                label = NULL,
                                choices = mapped_checkbox_names[order(mapped_checkbox_names)],
                                selected = default_columns,
                                icon = icon("check"),
                                status = "primary",
                                outline = FALSE
                              )
                          ),
                          div(style = "display: flex; gap: 10px; width: 100%;",
                              actionButton(inputId = ns("show_all"), label = "Show All", style = "flex-grow: 1; width: 0;"),
                              actionButton(inputId = ns("show_default"), label = "Show Default", style = "flex-grow: 1; width: 0;"))
                      )
               )
      ),
      
      div(style = "display: flex; justify-content: center; margin-top: 10px;",
          actionBttn(ns("confirm_btn"),"Apply changes",style = "stretch",color = "success",size = "md",individual = TRUE,value = 0))
    )
  )
}

