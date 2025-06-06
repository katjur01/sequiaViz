#app/view/somatic_var_call_plot.R

# Load and process data table
input_data <- function(sample){
  filenames <- get_inputs("per_sample_file")
  message("Loading data for somatic: ", filenames$var_call.somatic)
  data <- prepare_somatic_table(load_data(filenames$var_call.somatic,"varcall",sample))
  return(data)
}
