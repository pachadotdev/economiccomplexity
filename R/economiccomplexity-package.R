#' @importFrom stats cor aggregate setNames
#' @importFrom igraph graph_from_adjacency_matrix mst
#'  degree delete.edges graph.difference graph.union remove.edge.attribute E E<-
#' @importFrom Rdpack reprompt
#' @useDynLib economiccomplexity, .registration = TRUE
#' @keywords internal
"_PACKAGE"

#' World Trade Averages for the Period 1998-2000
#'
#' A data frame that summarizes all the products that different countries exported to the rest of the world. This data uses the SITC revision 2 classification with four digits product codes. The unit is year 2000 USD.
#'
#' @docType data
#' @usage data(world_trade_avg_1998_to_2000)
#' @format A data frame with 124,336 rows and 3 columns.
#' @examples
#' data(world_trade_avg_1998_to_2000)
#' head(world_trade_avg_1998_to_2000)
#' @keywords datasets
"world_trade_avg_1998_to_2000"

#' World Trade Per-Capita GDP for the Period 1998-2000
#'
#' A data frame that summarizes the per-capita GDP of different countries. The unit is year 2000 USD.
#'
#' @docType data
#' @usage data(world_gdp_avg_1998_to_2000)
#' @format A data frame with 240 rows and 2 columns.
#' @examples
#' data(world_gdp_avg_1998_to_2000)
#' head(world_gdp_avg_1998_to_2000)
#' @keywords datasets
"world_gdp_avg_1998_to_2000"

#' Example Outputs of the Functions within the Package
#'
#' List of different computations obtained by using the functions within the package.
#'
#' @docType data
#' @usage data(economiccomplexity_output)
#' @format Lists of vectors, matrices, and graphs.
#' @examples
#' data(economiccomplexity_output)
#' economiccomplexity_output$balassa_index
#' economiccomplexity_output$complexity_measures$complexity_index_country
#' @keywords datasets
"economiccomplexity_output"
