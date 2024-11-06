
box::use(
  jsonlite[fromJSON, toJSON],
  data.table[fread,setnames],
  readxl[read_excel],
  httr[GET, status_code, content],
  cyjShiny[dataFramesToJSON],
)
box::use(
  app/logic/load_data[get_inputs]
)

# Funkce pro získání interakcí mezi proteiny z STRING API
#' @export
get_string_interactions <- function(proteins, species = 9606, chunk_size = 100) {
  # Funkce pro odesílání jednotlivých požadavků
  fetch_interactions <- function(protein_chunk) {
    base_url <- "https://string-db.org/api/json/network?"
    query <- paste0("identifiers=", paste(protein_chunk, collapse = "%0D"), "&species=", species)
    url <- paste0(base_url, query)
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      content <- fromJSON(content(response, as = "text"))
      return(content)
    } else {
      stop("Request failed with status: ", status_code(response))
    }
  }
  
  # Rozdělení proteinů na bloky podle chunk_size (občas je proteinů moc)
  protein_chunks <- split(proteins, ceiling(seq_along(proteins) / chunk_size))
  all_interactions <- do.call(rbind, lapply(protein_chunks, fetch_interactions))
  
  return(all_interactions)
}

#' @export
prepare_cytoscape_network <- function(interactions, proteins, fc_values) {
  # Získání uzlů z interakcí
  interaction_nodes <- unique(c(interactions$preferredName_A, interactions$preferredName_B))
  all_nodes <- unique(c(interaction_nodes, proteins))
  
  # Spočítání stupně (degree) pro každý uzel - singletony mají stupen 0
  degrees <- table(c(interactions$preferredName_A, interactions$preferredName_B))
  degree_values <- sapply(all_nodes, function(x) ifelse(x %in% names(degrees), degrees[x], 0))
  
  # Příprava uzlů včetně fold-change hodnot, fc a label
  node_data <- data.frame(
    id = all_nodes,
    name = all_nodes,
    label = all_nodes,
    log2FC = ifelse(all_nodes %in% proteins, fc_values[match(all_nodes, proteins)], NA),  # Přidání sloupce fc
    degree = degree_values,  # Přidání stupně (degree) uzlu
    stringsAsFactors = FALSE
  )
  
  # Příprava hran (interakcí)
  edges <- data.frame(
    source = interactions$preferredName_A,
    target = interactions$preferredName_B,
    interaction = "interaction",  # Obecný popis interakce
    stringsAsFactors = FALSE
  )
  
  # Generování JSON pro cyjShiny
  # network_json <- toJSON(dataFramesToJSON(edges, node_data), auto_unbox = TRUE)

  node_data <- node_data[match(proteins, node_data$id, nomatch = 0), ]
  edges <- edges[edges$source %in% proteins & edges$target %in% proteins, ]
  
  json_data <- list(
    elements = list(
      nodes = lapply(seq_len(nrow(node_data)), function(i) {
        list(data = as.list(node_data[i, ]))
      }),
      edges = lapply(seq_len(nrow(edges)), function(i) {
        list(data = as.list(edges[i, ]))
      })
    )
  )
  
  network_json <- toJSON(json_data, auto_unbox = TRUE)

  return(network_json)
}

#' @export
get_pathway_list <- function(){
  dt <- fread("input_files/kegg_tab.tsv")
  pathway_list <- sort(unique(dt$kegg_paths_name))
  return(pathway_list)
}

#' @export
get_tissue_list <- function(){
  input_files <- get_inputs("per_sample_file")
  tissue_list <- sort(unique(gsub(".*/RNAseq21_NEW/[^/]+/([^_]+)_.*", "\\1", input_files$expression.file)))
  return(tissue_list)
}