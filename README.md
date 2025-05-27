# Shiny App for Visualization of Somatic Variant Analysis Data

This Shiny application is designed to provide a simple and intuitive interface for visualizing results of small somatic variant analyses. It offers:

- A clear and interactive data table with advanced filtering options  
- Visualizations including:
  - Sankey diagram
  - VAF (Variant Allele Frequency) plot
  - Pie charts showing variant impact based on various annotations  
- Export options for all displayed graphs and table data  
- Integration with IGV for direct variant visualization

## Requirements

To run the application successfully, you need:

- All R packages listed in `dependencies.R`
- A Linux-based operating system
- `npx` (Node.js package runner)
- A Chromium-based web browser

## Input Data

Input files must:

- Be named using **only the patient identifier**
- Be placed in the appropriate subfolders of the `input_files/` directory
  
The `input_files/` directory should aldo include a file containing variant-gene-pathway mapping information from the KEGG database. (by default named: `kegg_table.tsv`)
