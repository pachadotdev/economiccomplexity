#' Networks
#'
#' @export
#' @param proximity_countries matrix or tibble/data.frame, if d is a tibble/data.frame it must contain the columns
#' from (character/factor), to (character/factor) and value (numeric), if it is a matrix it must be a
#' numeric matrix with countries in row names and column names
#' @param proximity_products matrix or tibble/data.frame, if d is a tibble/data.frame it must contain the columns
#' from (character/factor), to (character/factor) and value (numeric), if it is a matrix it must be a
#' numeric matrix with products in row names and column names
#' @param countries_cutoff all the values lower than the specified countries_cutoff will be converted to 0 and excluded
#' from the countries network (default set to 0.2)
#' @param products_cutoff all the values lower than the specified products_cutoff will be converted to 0 and excluded
#' from the products network (default set to 0.4)
#' @param tbl_output when set to TRUE the output will be a tibble instead of a graph (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom igraph graph_from_data_frame mst as_data_frame simplify
#' @importFrom rlang sym
#' @examples
#' networks(
#'  proximity_countries =
#'   package_output_demo$proximity_matrix$proximity_countries,
#'  proximity_products =
#'   package_output_demo$proximity_matrix$proximity_products,
#'  tbl_output = TRUE
#' )
#' @references
#' For more information on networks such as the product space and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#' @keywords functions

networks <- function(proximity_countries, proximity_products, countries_cutoff = 0.2,
                     products_cutoff = 0.4, tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(proximity_countries) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix", "dsCMatrix") == FALSE) &
      all(class(proximity_products) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix", "dsCMatrix") == FALSE)) {
    stop("proximity_countries and proximity_products must be tibble/data.frame or dense/sparse matrix")
  }

  if (!is.numeric(countries_cutoff) & !is.numeric(products_cutoff)) {
    stop("countries_cutoff & products_cutoff must be numeric")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be matrix or tibble")
  }

  # arrange country matrix ----
  if (any(class(proximity_countries) %in% c("dgeMatrix", "dgCMatrix", "dsCMatrix") == TRUE)) {
    proximity_countries <- as.matrix(proximity_countries)
  }

  if (is.matrix(proximity_countries)) {
    proximity_countries[upper.tri(proximity_countries, diag = TRUE)] <- 0
    row_names <- rownames(proximity_countries)

    proximity_countries <- proximity_countries %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = row_names) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)
  }

  # arrange products matrix ----
  if (any(class(proximity_products) %in% c("dgeMatrix", "dgCMatrix", "dsCMatrix") == TRUE)) {
    proximity_products <- as.matrix(proximity_products)
  }

  if (is.matrix(proximity_products)) {
    proximity_products[upper.tri(proximity_products, diag = TRUE)] <- 0
    row_names <- rownames(proximity_products)

    proximity_products <- proximity_products %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = row_names) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)
  }

  # compute countries network ----
  proximity_countries <- dplyr::mutate(proximity_countries, value = -1 * !!sym("value"))

  c_g <- igraph::graph_from_data_frame(proximity_countries, directed = FALSE)
  c_mst <- igraph::mst(c_g, weights = proximity_countries$value, algorithm = "prim")
  c_mst <- igraph::as_data_frame(c_mst)

  c_g_nmst <- proximity_countries %>%
    dplyr::filter(!!sym("value") <= -1 * countries_cutoff) %>%
    dplyr::anti_join(c_mst, by = c("from", "to"))

  c_g <- dplyr::bind_rows(c_mst, c_g_nmst)
  c_g <- dplyr::mutate(c_g, value = -1 * !!sym("value"))

  c_g <- igraph::graph_from_data_frame(c_g, directed = FALSE)
  c_g <- igraph::simplify(c_g,
                          remove.multiple = TRUE, remove.loops = TRUE,
                          edge.attr.comb = "first"
  )

  # compute products network ----
  proximity_products <- dplyr::mutate(proximity_products, value = -1 * !!sym("value"))

  p_g <- igraph::graph_from_data_frame(proximity_products, directed = FALSE)
  p_mst <- igraph::mst(p_g, weights = proximity_products$value, algorithm = "prim")
  p_mst <- igraph::as_data_frame(p_mst)

  p_g_nmst <- proximity_products %>%
    dplyr::filter(!!sym("value") <= -1 * products_cutoff) %>%
    dplyr::anti_join(p_mst, by = c("from", "to"))

  p_g <- dplyr::bind_rows(p_mst, p_g_nmst)
  p_g <- dplyr::mutate(p_g, value = -1 * !!sym("value"))

  p_g <- igraph::graph_from_data_frame(p_g, directed = FALSE)
  p_g <- igraph::simplify(p_g,
                          remove.multiple = TRUE, remove.loops = TRUE,
                          edge.attr.comb = "first"
  )

  if (tbl_output == TRUE) {
    c_g <- igraph::as_data_frame(c_g) %>% dplyr::as_tibble()
    p_g <- igraph::as_data_frame(p_g) %>% dplyr::as_tibble()
  }

  return(list(network_countries = c_g, network_products = p_g))
}
