
box::use(
  shiny[h1,h2,h3,bootstrapPage,div,moduleServer,NS,renderUI,tags,uiOutput,icon,observeEvent,observe,reactive,isTruthy,
        fluidRow,mainPanel,tabPanel,titlePanel,tagList,HTML,textInput,sidebarLayout,sidebarPanel,includeScript,br,updateTabsetPanel, actionButton],
  shiny.gosling[renderGosling,arrange_views,compose_view,add_single_track,track_data,visual_channel_x,visual_channel_row,
                visual_channel_color,visual_channel_tooltips,visual_channel_tooltip,use_gosling,goslingOutput,gosling],
  openxlsx[read.xlsx],
  VariantAnnotation[readVcf],
  StructuralVariantAnnotation[breakpointRanges]
)

# library(StructuralVariantAnnotation)
# library(ggbio)
# library(shiny)
library(shiny.gosling)
use_gosling()

# https://www.bioconductor.org/packages/release/bioc/html/shiny.gosling.html
# https://appsilon.github.io/shiny.gosling/articles/intro.html
# https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-14-244
# https://bioconductor.org/packages/release/bioc/vignettes/chimeraviz/inst/doc/chimeraviz-vignette.html#building-the-overview-plot

# Načtení dat
data <- read.xlsx("../../input_files/MOII_e117/117_fusions/results/DZ1601fuze_fusions.xlsx")

# load vcf file
colo829_vcf <- VariantAnnotation::readVcf(
  system.file(
    "extdata",
    "COLO829T.purple.sv.ann.vcf.gz",
    package = "StructuralVariantAnnotation"
  )
)
colo829_bpgr <- breakpointRanges(colo829_vcf)

gr_circos <- colo829_bpgr[seqnames(colo829_bpgr) %in% seqlevels(biovizBase::hg19sub)]
seqlevels(gr_circos) <- seqlevels(biovizBase::hg19sub)
mcols(gr_circos)$to.gr <- granges(partner(gr_circos))

# nastavujeme vnější kruh podle vcf
track1 <- add_single_track(
  id = "track1",
  title = "Quality",
  data = track_data_gr(
    gr_circos, chromosomeField = "seqnames",
    genomicFields = c("start", "end")
  ),
  mark = "bar",
  x = visual_channel_x(field = "start", type = "genomic", axis = "bottom"),
  y = visual_channel_y(field = "QUAL", type = "quantitative", axis = "right"),
  color = visual_channel_color(
    value = "#BB3114"
  ),
  width = 700,
  height = 100
)

# nastavujeme začátek a konec sekvencí prostředního kruhu
track2 <- add_single_track(
  id = "track2",
  title = "REF",
  data = track_data_gr(
    gr_circos, chromosomeField = "seqnames",
    genomicFields = c("start", "end")
  ),
  mark = "rect",
  strokeWidth = visual_channel_stroke_width(
    value = 0.5
  ),
  stroke = visual_channel_stroke(
    field = "REF",
    type = "nominal",
    domain = list("A", "C", "G", "T"),
    range = list("#73A97D", "#C1BB78", "#1F5E89", "#CF784B")
  ),
  x = visual_channel_x(field = "start", type = "genomic"),
  xe = visual_channel_x(field = "end", type = "genomic"),
  width = 700,
  height = 100
)


## vnitřní kruh, ukazujeme změnu sekvencí
# 
# track3 <- add_single_track(
#   id = "track3",
#   title = "Highlight similarities of Chr 10 with others",
#   data = track_data_gr(
#     gr_circos, chromosomeField = "to.gr.seqnames",
#     genomicFields = c("to.gr.start", "to.gr.end"),
#     longToWideId = "event"
#   ),
#   alignment = "overlay",
#   opacity = visual_channel_opacity(
#     value = 0.4
#   ),
#   tracks = add_multi_tracks(
#     add_single_track(
#       dataTransform = track_data_transform(
#         type = "filter", field = "to.gr.seqnames", oneOf = list("10"), not = TRUE
#       ),
#       mark = "withinLink",
#       x = visual_channel_x(
#         field = "to.gr.start", type = "genomic"
#       ),
#       xe = visual_channel_x(
#         field = "to.gr.start_2", type = "genomic"
#       ),
#       x1 = visual_channel_x(
#         field = "to.gr.end", type = "genomic"
#       ),
#       x1e = visual_channel_x(
#         field = "to.gr.end_2", type = "genomic"
#       ),
#       stroke = visual_channel_stroke(
#         value = "lightgray"
#       ),
#       strokeWidth = visual_channel_stroke_width(
#         value = 1
#       )
#     ),
#     add_single_track(
#       dataTransform = track_data_transform(
#         type = "filter", field = "to.gr.seqnames", oneOf = list("10")
#       ),
#       mark = "withinLink",
#       x = visual_channel_x(
#         field = "to.gr.start", type = "genomic"
#       ),
#       xe = visual_channel_x(
#         field = "to.gr.start_2", type = "genomic"
#       ),
#       x1 = visual_channel_x(
#         field = "to.gr.end", type = "genomic"
#       ),
#       x1e = visual_channel_x(
#         field = "to.gr.end_2", type = "genomic"
#       ),
#       stroke = visual_channel_stroke(
#         field = "to.gr.seqnames_2",
#         type = "nominal",
#         range = c(
#           "#E79F00", "#029F73", "#0072B2", "#CB7AA7", "#D45E00",
#           "#57B4E9", "#EFE441"
#         )
#       ),
#       strokeWidth = visual_channel_stroke_width(
#         value = 1.5
#       )
#     )
#   ),
#   width = 700,
#   height = 200
# )

single_composed_track <- compose_view(
  title = "Circos",
  subtitle = "http://circos.ca/intro/genomic_data/",
  layout = "circular",
  static = TRUE,
  spacing = 1,
  centerRadius = 0.3,
  alignment = "stack",
  multi = TRUE,
  tracks = add_multi_tracks(track1,track2)
  # tracks = add_multi_tracks(track1, track2, track3)
)

ui <- fluidPage(
  use_gosling(),
  fluidRow(
    column(6, goslingOutput("gosling_plot")),
    column(
      1, br(), actionButton(
        "download_pdf",
        "PDF",
        icon = icon("cloud-arrow-down")
      )
    )
  )
)

server <- function(input, output, session) {
  output$gosling_plot <- renderGosling({
    gosling(
      component_id = "component_1",
      single_composed_track
    )
  })
  
  observeEvent(input$download_pdf, {
    export_pdf(component_id = "component_1")
  })
}

shinyApp(ui, server)

