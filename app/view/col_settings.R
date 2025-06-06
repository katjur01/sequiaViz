# app/view/col_settings.R

box::use(
  reactable[colDef]
)

# Define default parameters of standard columns
#' @export
default_col <- function(){
  default_columns <- list(
    "var_name" = colDef(sticky='left', minWidth=140,filterable = TRUE, name = 'Variant name'),
    "library"=colDef(sticky='left',header = "In library"),
    "alarm" = colDef(show=FALSE,name="Alarm"),
    "full_annot_name" = colDef(show=FALSE,name="Annotated name",minWidth = 240,),
    "var_gen_coord" = colDef(show=FALSE,name="Variant coordinates",minWidth = 240,filterable=TRUE),
    "variant_type" = colDef(filterable = TRUE, name = 'Variant type',show=FALSE,minWidth=110),
    "Gene_symbol" = colDef(sticky = 'left',filterable = TRUE, minWidth=110,name = 'Gene symbol'),
    "HGVSp" = colDef(minWidth=120,maxWidth=250,name="HGVSp"),
    "HGVSc" = colDef(minWidth=120,maxWidth=250,name="HGVSc"),
    "tumor_variant_freq" = colDef(name="Tumor variant frequency",minWidth=200, filterable = TRUE),
    "tumor_depth" = colDef(name = "Tumor depth",minWidth=110),
    "normal_variant_freq" = colDef(show=FALSE,name="Normal variant frequency",minWidth=200),
    "normal_depth" = colDef(show=FALSE,name = "Normal depth",minWidth=120),
    "Called_by"	= colDef(show=FALSE,name="Called by"),
    "1000g_EUR_AF" = colDef(show=FALSE,name="1000G EUR AF",minWidth=130),
    "gnomAD_NFE" = colDef(name = "GnomAD NFE",filterable=FALSE,minWidth=110),
    "snpDB" = colDef(name="SnpDB",filterable=TRUE),
    "COSMIC" = colDef(name="COSMIC",filterable=TRUE),
    "HGMD" = colDef(name="HGMD"),
    "NHLBI_ESP" = colDef(show=FALSE,name="NHLBI ESP",minWidth=110),
    "clinvar_sig" = colDef(name = "ClinVar significance", filterable = TRUE,minWidth=180),
    "clinvar_DBN" = colDef(show=TRUE,name="ClinVar DBN",filterable = TRUE,minWidth=110),
    "fOne" = colDef(name="fOne"),
    "md-anderson" = colDef(show=FALSE,name="MD Anderson",minWidth=110),
    "trusight_genes" = colDef(show=FALSE,name="TruSight genes",minWidth=130),
    "CGC_Somatic" = colDef(name="CGC Somatic",minWidth=120),
    "CGC_Tumour_Somatic" = colDef(show=FALSE,name="CGC Tumour",minWidth=110),
    "PolyPhen" = colDef(show=FALSE,name="PolyPhen",minWidth=190),
    "SIFT" = colDef(show=FALSE,name="SIFT",minWidth=180),
    "gene_region" = colDef(name="Gene region",filterable = TRUE, minWidth=110),
    "IMPACT" = colDef(show=FALSE,name="Impact",filterable=TRUE),
    "Consequence" = colDef(name = "Consequence",minWidth=140,filterable=TRUE),
    "EXON" = colDef(show=FALSE, name= "Exon"),
    "INTRON" = colDef(show=FALSE, name = "Intron"),
    "Feature" = colDef(show=FALSE, name = "Feature",minWidth=180,filterable=TRUE),
    "Feature_type" = colDef(show=FALSE, name = "Feature type",filterable=TRUE,minWidth=110),
    "Annotation source" = colDef(show=FALSE, name = "Annotation source",minWidth=160),
    "all_full_annot_name" = colDef(name = "Full annotated name", minWidth = 240,show=TRUE),
    "Gene" = colDef(show=FALSE, name = "Gene")
  )
  return(default_columns)
}