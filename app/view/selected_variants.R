# app/selected_variants.R

box::use(
  shiny[req, tagList, fluidRow, column, br, actionButton],
  reactable[reactable, colDef, reactableOutput, getReactableState],
  dplyr[anti_join]
)

# UI komponenta: tabulka a tlačítko
#' @export
render_selected_variants_ui <- function(ns, selected_data_actual_patient) {
  df <- selected_data_actual_patient()
  req(!is.null(df), nrow(df) > 0)
  tagList(
    br(),
    fluidRow(
      column(width = 10, reactableOutput(ns("selected_variants_table"))),
      column(width = 3)
    ),
    br(),
    actionButton(ns("delete_variant"), "Delete variant")
  )
}

# Tabulka vybraných variant
#' @export
render_selected_variants_table <- function(df) {
  reactable(df,
            selection = "multiple",
            onClick = "select",
            highlight = TRUE,
            wrap = FALSE,
            striped = TRUE,
            defaultColDef = colDef(resizable = TRUE, show = TRUE, align = "center"),
            columns = list(
              "var_name" = colDef(name = "Variant name", minWidth = 140),
              "Gene_symbol" = colDef(name = "Gene symbol", minWidth = 110),
              "tumor_variant_freq" = colDef(show = FALSE),
              "Consequence" = colDef(minWidth = 140),
              "clinvar_sig" = colDef(name = "ClinVar significance", minWidth = 140),
              "position1" = colDef(show = FALSE),
              "patients" = colDef(name = "Detected for patient", minWidth = 120, show = FALSE)
            )
  )
}

# Odstranění variant
#' @export
handle_delete_variant <- function(selected_data, selected_data_actual_patient, shared_data) {
  selected_idx <- getReactableState("selected_variants_table", "selected")
  if (is.null(selected_idx) || length(selected_idx) == 0) return(NULL)
  df_patient <- selected_data_actual_patient()
  selected_rows <- df_patient[selected_idx, ]
  df_all <- selected_data()
  df_all_new <- anti_join(df_all, selected_rows, by = colnames(df_all))
  df_patient_new <- anti_join(df_patient, selected_rows, by = colnames(df_patient))
  selected_data(df_all_new)
  selected_data_actual_patient(df_patient_new)
  shared_data$selected_variants <- df_all_new
}

# Přidání vybraných variant
#' @export
handle_confirm_selected <- function(tabset_id, patients, filtered_data, selected_data, selected_data_actual_patient, shared_data) {
  selected_tab_id <- which(patients == tabset_id)
  selected_idx <- getReactableState(paste0("my_table", selected_tab_id), "selected")
  if (is.null(selected_idx) || length(selected_idx) == 0) return(NULL)
  df <- filtered_data[[selected_tab_id]]()
  df <- df[selected_idx, c("var_name", "tumor_variant_freq", "Gene_symbol", "Consequence", "clinvar_sig"), drop = FALSE]
  df$position1 <- sub("^([^_]+)_([^_]+)_.*", "\\1:\\2", df$var_name)
  df$patients <- tabset_id
  combined <- as.data.frame(rbind(selected_data(), df))
  combined <- combined[!duplicated(combined[c("var_name", "Gene_symbol", "patients")]), ]
  selected_data(combined)
  selected_data_actual_patient(subset(combined, patients == tabset_id))
  shared_data$selected_variants <- combined
}
