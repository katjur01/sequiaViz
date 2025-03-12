box::use(
  shiny[moduleServer,NS,tagList,fluidRow,fluidPage,column,tabPanel,reactive,req,
        updateSelectInput,selectInput,numericInput,actionButton,renderPlot,plotOutput],
  reactable,
  reactable[colDef,reactableOutput,renderReactable,reactable,colGroup],
  htmltools[tags],
  plotly[plotlyOutput,renderPlotly],
  reactablefmtr[pill_buttons,data_bars],
  utils[head],
  plotly[plot_ly]
  
)

box::use(
  app/logic/plots[prepare_barPlot_data,create_barPlot], 
  app/logic/waiters[use_spinner],
  app/logic/load_data[get_inputs,load_data],
  app/logic/reactable_helpers[generate_columnsDef,custom_colGroup_setting],
  app/logic/prepare_table[prepare_expression_table,set_pathway_colors,get_tissue_list],
  app/logic/plots[prepare_volcano,plot_volcano],
)

# Load and process data table
input_data <- function(sample,expr_flag){
  input_files <- get_inputs("per_sample_file") 
  # message("Loading data for expression profile: ", filenames$expression.files)
  data <- load_data(input_files$expression.files,"expression",sample,expr_flag) #expr_flag = "all_genes"|"genes_of_interest" #sample = "MR1507"
  dt <- prepare_expression_table(data,expr_flag)
  return(dt)
}


ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      selectInput(ns("selected_tissue"), "Vyber tkáň:", choices = "Blood"),  # Naplníme dynamicky
      numericInput(ns("padj_cutoff"), "p-adj cutoff:", value = 0.05, min = 0, step = 0.01),
      numericInput(ns("logfc_cutoff"), "log2FC cutoff:", value = 1, min = 0, step = 0.1),
      numericInput(ns("top_n"), "Počet popisků genů:", value = 20, min = 0, step = 1),
      actionButton(ns("update_plot"), "Aktualizovat graf")
    ),
    fluidRow(
      plotOutput(ns("volcano_plot"))
    )
  )
}



server <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive({
      input_data("MR1507", "all_genes") 
    })
    

    
    
    
    # Přidání popisků pro top geny
    
    
    
    
    volcano <- reactive({
      dt <- input.data()
      padj.cutoff <- as.numeric(input$padj.cutoff)
      fc.cutoff <- as.numeric(input$fc.cutoff)
      logfc.cutoff <- log2(fc.cutoff)
      nlabels <- input$nlabels
      genelist <- unlist(strsplit(input$genelist,split = ",",fixed = TRUE))
      
      setorder(dt, padj, pvalue, na.last = T) # make sure it is correctly ordered
      dtextract <- unique(rbind(dt[padj<padj.cutoff,][1:nlabels,],dt[Ensembl_Id %in% genelist | Feature_name %in% genelist]))
      new.nlabels <- length(dtextract$Ensembl_Id)
      
      sigtabl <- data.table(sig=c("sig","down","up","nsig","na"))
      dtt<-dt[, .(count = .N), by = sig]
      dtt<-merge(sigtabl, dtt, by="sig", all.x=T)
      dtt<-dtt[,sig := factor(sig,levels = c("sig","down","up","nsig","na"))]
      dtt<-dtt[is.na(count)==T, count:=0]
      
      volkan <- ggplot(dt, aes(log2FC, -log10(padj), text=Feature_name)) +
        geom_point(aes(col=factor(sig, levels = c("sig","down","up","nsig","na"))), size=input$point_size) +
        scale_color_manual(name="",values=c(input$col_sig, input$col_down, input$col_up, input$col_nsig, input$col_na),
                           labels=c(paste0("Significant (",dtt[sig=="sig"]$count,")"),
                                    paste0("Sig. Down (",dtt[sig=="down"]$count,")"),
                                    paste0("Sig. Up (",dtt[sig=="up"]$count,")"),
                                    paste0("Not Sig. (",dtt[sig=="nsig"]$count,")"),
                                    paste0("NA (",dtt[sig=="na"]$count,")")
                           ), drop=F) +
        theme_bw()
      
      #volkan <- volkan + geom_text_repel(data=dtextract, aes(label=Gene), size=3, max.overlaps = new.nlabels)+
      volkan <- volkan + geom_vline(xintercept = 0, linetype = "solid", colour=input$col_zero)+
        geom_vline(xintercept = c(logfc.cutoff, -logfc.cutoff), linetype = input$linetype_FC, colour=input$col_FC, alpha=input$alpha_FC)+
        geom_hline(yintercept = -log10(padj.cutoff), linetype = input$linetype_padj, colour=input$col_padj, alpha=input$alpha_padj)+
        #ggtitle(paste0("Volcanoplot ",input$comparison,"\n top ", nlabels, " genes"))+
        ggtitle("Volcanoplot")+
        theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(face="bold"))
      
      volk<-list(volkan=volkan,dtextract=dtextract,new.nlabels=new.nlabels)
      volk
      
    })
    
    output$volcano_ly <- renderPlotly({
      volk<-volcano()
      ggplotly(volk$volkan, tooltip=c("Feature_name","log2FC")) })
    
    
    
  })
}

expUI <- fluidPage(
  ui("exp1")
)

expSerever <- function(input, output, session) {
  server("exp1")
}

shinyApp(expUI, expSerever)
