#' Networks
#'
#' @export
#' @param country_proximity matrix or tibble/data.frame, if d is a tibble/
#' data.frame it must contain the columns from (character/factor), to
#' (character/factor) and value (numeric), if it is a matrix it must be a
#' numeric matrix with countries in row names and column names
#' @param product_proximity matrix or tibble/data.frame, if d is a
#' tibble/data.frame it must contain the columns from (character/factor), to
#' (character/factor) and value (numeric), if it is a matrix it must be a
#' numeric matrix with products in row names and column names
#' @param countries_cutoff all the values lower than the specified
#' countries_cutoff will be converted to 0 and excluded from the countries
#' network (default set to 0.2)
#' @param products_cutoff all the values lower than the specified
#' products_cutoff will be converted to 0 and excluded from the products network
#' (default set to 0.4)
#' @param tbl_output when set to TRUE the output will be a tibble instead of a
#' graph (default set to FALSE)
#' @param compute by default set to "both", it can also be "country" or
#' "product"
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom igraph graph_from_data_frame mst as_data_frame simplify
#' @importFrom rlang sym
#' @examples
#' networks(
#'  country_proximity =
#'   package_output_demo$proximity_matrix$country_proximity,
#'  product_proximity =
#'   package_output_demo$proximity_matrix$product_proximity,
#'  tbl_output = TRUE
#' )
#' @references
#' For more information on networks such as the product space and its
#' applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

networks <- function(country_proximity,
                     product_proximity,
                     countries_cutoff = 0.2,
                     products_cutoff = 0.4,
                     tbl_output = FALSE,
                     compute = "both") {
  # sanity checks ----
  if (all(class(country_proximity) %in% c("data.frame", "matrix",
        "dgeMatrix", "dgCMatrix", "dsCMatrix") == FALSE) &
      all(class(product_proximity) %in% c("data.frame", "matrix",
        "dgeMatrix", "dgCMatrix", "dsCMatrix") == FALSE)) {
    stop("country_proximity and product_proximity must be tibble/data.frame
          or dense/sparse matrix")
  }

  if (!is.numeric(countries_cutoff) & !is.numeric(products_cutoff)) {
    stop("countries_cutoff & products_cutoff must be numeric")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be matrix or tibble")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("compute must be both, country or product")
  }

  if (compute == "both") {
    compute2 <- c("country","product")
  } else {
    compute2 <- compute
  }

  if (any("country" %in% compute2) == TRUE) {
    # arrange country matrix ----
    if (any(class(country_proximity) %in% c("dgeMatrix", "dgCMatrix",
                                              "dsCMatrix") == TRUE)) {
      country_proximity <- as.matrix(country_proximity)
    }

    if (is.matrix(country_proximity)) {
      country_proximity[upper.tri(country_proximity, diag = TRUE)] <- 0
      row_names <- rownames(country_proximity)

      country_proximity <- country_proximity %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute countries network ----
    country_proximity <- dplyr::mutate(country_proximity,
                                         value = -1 * !!sym("value"))

    c_g <- igraph::graph_from_data_frame(country_proximity, directed = FALSE)

    c_mst <- igraph::mst(c_g, weights = country_proximity$value,
                         algorithm = "prim")
    c_mst <- igraph::as_data_frame(c_mst)

    c_g_nmst <- country_proximity %>%
      dplyr::filter(!!sym("value") <= -1 * countries_cutoff) %>%
      dplyr::anti_join(c_mst, by = c("from", "to"))

    c_g <- dplyr::bind_rows(c_mst, c_g_nmst)
    c_g <- dplyr::mutate(c_g, value = -1 * !!sym("value"))

    c_g <- igraph::graph_from_data_frame(c_g, directed = FALSE)
    c_g <- igraph::simplify(c_g,
                            remove.multiple = TRUE, remove.loops = TRUE,
                            edge.attr.comb = "first"
    )

    if (tbl_output == TRUE) {
      c_g <- igraph::as_data_frame(c_g) %>% dplyr::as_tibble()
    }
  } else {
    c_g = NULL
  }

  if (any("product" %in% compute2) == TRUE) {
    # arrange products matrix ----
    if (any(class(product_proximity) %in% c("dgeMatrix", "dgCMatrix",
                                             "dsCMatrix") == TRUE)) {
      product_proximity <- as.matrix(product_proximity)
    }

    if (is.matrix(product_proximity)) {
      product_proximity[upper.tri(product_proximity, diag = TRUE)] <- 0
      row_names <- rownames(product_proximity)

      product_proximity <- product_proximity %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute products network ----
    product_proximity <- dplyr::mutate(product_proximity,
                                       value = -1 * !!sym("value"))

    p_g <- igraph::graph_from_data_frame(product_proximity, directed = FALSE)

    p_mst <- igraph::mst(p_g, weights = product_proximity$value,
                         algorithm = "prim")
    p_mst <- igraph::as_data_frame(p_mst)

    p_g_nmst <- product_proximity %>%
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
      p_g <- igraph::as_data_frame(p_g) %>% dplyr::as_tibble()
    }
  } else {
    p_g = NULL
  }

  return(list(country_network = c_g, product_network = p_g))
}
