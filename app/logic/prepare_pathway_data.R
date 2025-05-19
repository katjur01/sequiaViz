#
# genes <- fread("sequiaViz/input_files/TNBC_gene_score_RNA.tsv")
#
# var_entries <- fromJSON("/home/katka/PhD/pathway_mining/entries.json")
# entries <-rbindlist(var_entries)
#
# var_relations <- fromJSON("/home/katka/PhD/pathway_mining/relations.json", flatten = TRUE)
# relations <-rbindlist(var_relations)
#
# var_hsa05224 <- fromJSON("/home/katka/PhD/pathway_mining/hsa05224.json", flatten = TRUE)
# path_hsa05224 <-rbindlist(var_hsa05224,fill=TRUE)
#

box::use(
  cyjShiny[dataFramesToJSON],
  jsonlite[toJSON],
  KEGGREST[keggGet],
  igraph[graph_from_data_frame,V,E,ends],
)


#' @export
pathway_input <- function(){
    # # Fetch pathway data for a specific pathway (e.g., hsa04110 - Cell cycle)
    # # pathway_id <- "hsa04110" # cell cycle
    # pathway_id <- "hsa05224" # breast cancer
    # pathway <- keggGet(pathway_id)[[1]]
    #
    # # Extract the graph information
    # nodes <- pathway$PATHWAY_MAP$nodes
    # edges <- pathway$PATHWAY_MAP$edges
    #
    # # Convert to a data frame for nodes and edges
    # node_df <- data.frame(id = names(nodes), label = unlist(nodes))
    # edge_df <- data.frame(source = edges$source, target = edges$target, interaction = edges$interaction)
    #
    # # Create an igraph object
    # g <- graph_from_data_frame(d = edge_df, vertices = node_df, directed = TRUE)
    #
    #
    #
    # # Convert the graph to a list suitable for conversion to JSON
    # igraph_to_cyj <- function(g) {
    #   nodes <- lapply(V(g), function(v) {
    #     list(data = list(id = v$name, label = v$label))
    #   })
    #
    #   edges <- lapply(E(g), function(e) {
    #     list(data = list(source = ends(g, e)[1], target = ends(g, e)[2]))
    #   })
    #
    #   list(nodes = nodes, edges = edges)
    # }
    #
    # cyj_data <- igraph_to_cyj(g)
    # cyj_json <- toJSON(cyj_data, auto_unbox = TRUE)
    #
    # return(cyj_json)

    tbl_nodes <- data.frame(id=c("A", "B", "C"),
                            size=c(10, 20, 30),
                            stringsAsFactors=FALSE)

    # Must have the interaction column
    tbl_edges <- data.frame(source=c("A", "B", "C"),
                            target=c("B", "C", "A"),
                            interaction=c("inhibit", "stimulate", "inhibit"),
                            stringsAsFactors=FALSE)

    graph_json <- toJSON(dataFramesToJSON(tbl_edges, tbl_nodes), auto_unbox=TRUE)
    return(graph_json)
}

