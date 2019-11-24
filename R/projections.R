#' @title Country-product bipartite network projections
#'
#' @description \code{rca} computes complexity indices following the definitions
#' from \insertCite{measuringcomplexity2015;textual}{economiccomplexity}
#'
#' @details Given a matrix of \eqn{C\times C} with country proximity values and
#' a matrix of \eqn{P\times P} with product proximity values, this function
#' obtains a simplified network in two steps:
#' 1) creates a network skeleton by appliying the minimum spanning tree
#' algorithm from the igraph package, but multiplying proximities by minus one,
#' so that the strongest links are considered for the network skeleton
#' 2) append additional links to the skeleton by aading the links with proximity
#' values above a user-defined cutoff
#'
#' @param proximity_c matrix or data frame with product proximity values
#' @param proximity_p matrix or data frame with country proximity values
#' @param cutoff_c all the links with a proximity below this value will be
#' removed from the country projection (by default is 0.2)
#' @param cutoff_p all the links with a proximity below this value will be
#' removed from the product projection (by default is 0.4)
#' @param compute "country", "product" or "both" (default) projections
#' @param tbl TRUE (default) returns a data.frame and FALSE returns a matrix
#' @param from_c column containing origin (applies only if proximity_c is a
#' data.frame)
#' @param to_c column containing destination (applies only if proximity_c is a
#' data.frame)
#' @param value_c column containing proximity values (applies only if
#' proximity_c is a data.frame)
#' @param from_p column containing origin (applies only if proximity_p is a
#' data.frame)
#' @param to_p column containing destination (applies only if proximity_p is a
#' data.frame)
#' @param value_p column containing proximity values (applies only if
#' proximity_p is a data.frame)
#'
#' @references
#' For more information about visualizing bipartite networks projections see:
#'
#' \insertRef{human2007}{economiccomplexity}
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' and the references therein.
#'
#' @examples
#' projections(
#'   ec_output_demo$proximity$proximity_c,
#'   ec_output_demo$proximity$proximity_p
#' )
#'
#' @return A list with two data frames or matrices.
#'
#' @seealso \code{\link[economiccomplexity]{proximity}}
#'
#' @keywords functions
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble select filter mutate rename bind_rows
#' @importFrom tidyr gather
#' @importFrom igraph graph_from_data_frame mst as_data_frame simplify
#' @importFrom rlang sym syms
#'
#' @export

projections <- function(proximity_c,
                        proximity_p,
                        cutoff_c = 0.2,
                        cutoff_p = 0.4,
                        compute = "both",
                        tbl = TRUE,
                        from_c = "from",
                        to_c = "to",
                        value_c = "value",
                        from_p = "from",
                        to_p = "to",
                        value_p = "value") {
  # sanity checks ----
  if (all(class(proximity_c) %in% c("data.frame", "matrix", "dgeMatrix",
    "dgCMatrix", "dsCMatrix") == FALSE) &
    all(class(proximity_p) %in% c("data.frame", "matrix", "dgeMatrix",
      "dgCMatrix", "dsCMatrix") == FALSE)) {
    stop("proximity_c and proximity_p must be data frames or matrices")
  }

  if (!is.numeric(cutoff_c) & !is.numeric(cutoff_p)) {
    stop("cutoff_c and cutoff_p must be numeric")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be TRUE or FALSE")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("compute must be 'both', 'country' or 'product'")
  }

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  if (any("country" %in% compute2) == TRUE) {
    # arrange country matrix ----
    if (any(class(proximity_c) %in% c("dgeMatrix",
      "dgCMatrix", "dsCMatrix") == TRUE)) {
      proximity_c <- as.matrix(proximity_c)
    }

    if (is.matrix(proximity_c)) {
      proximity_c[upper.tri(proximity_c, diag = TRUE)] <- 0
      row_names <- rownames(proximity_c)

      proximity_c <- proximity_c %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute country network ----
    proximity_c <- proximity_c %>%
      dplyr::select(!!!syms(c(from_c, to_c, value_c))) %>%
      dplyr::rename(from = from_c, to = to_c, value = value_c) %>%
      dplyr::mutate(value = -1 * !!sym("value"))

    c_g <- igraph::graph_from_data_frame(proximity_c, directed = FALSE)

    c_mst <- igraph::mst(c_g, weights = proximity_c$value, algorithm = "prim")
    c_mst <- igraph::as_data_frame(c_mst)

    c_g_nmst <- proximity_c %>%
      dplyr::filter(!!sym("value") <= -1 * cutoff_c) %>%
      dplyr::anti_join(c_mst, by = c("from", "to"))

    c_g <- dplyr::bind_rows(c_mst, c_g_nmst)
    c_g <- dplyr::mutate(c_g, value = -1 * !!sym("value"))

    c_g <- igraph::graph_from_data_frame(c_g, directed = FALSE)
    c_g <- igraph::simplify(c_g,
      remove.multiple = TRUE, remove.loops = TRUE,
      edge.attr.comb = "first"
    )

    if (tbl == TRUE) {
      c_g <- igraph::as_data_frame(c_g) %>% dplyr::as_tibble()
    }
  } else {
    c_g <- NULL
  }

  if (any("product" %in% compute2) == TRUE) {
    # arrange products matrix ----
    if (any(class(proximity_p) %in% c("dgeMatrix",
      "dgCMatrix", "dsCMatrix") == TRUE)) {
      proximity_p <- as.matrix(proximity_p)
    }

    if (is.matrix(proximity_p)) {
      proximity_p[upper.tri(proximity_p, diag = TRUE)] <- 0
      row_names <- rownames(proximity_p)

      proximity_p <- proximity_p %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute products network ----
    proximity_p <- proximity_p %>%
      dplyr::select(!!!syms(c(from_p, to_p, value_p))) %>%
      dplyr::rename(from = from_p, to = to_p, value = value_p) %>%
      dplyr::mutate(value = -1 * !!sym("value"))

    p_g <- igraph::graph_from_data_frame(proximity_p, directed = FALSE)

    p_mst <- igraph::mst(p_g, weights = proximity_p$value, algorithm = "prim")
    p_mst <- igraph::as_data_frame(p_mst)

    p_g_nmst <- proximity_p %>%
      dplyr::filter(!!sym("value") <= -1 * cutoff_p) %>%
      dplyr::anti_join(p_mst, by = c("from", "to"))

    p_g <- dplyr::bind_rows(p_mst, p_g_nmst)
    p_g <- dplyr::mutate(p_g, value = -1 * !!sym("value"))

    p_g <- igraph::graph_from_data_frame(p_g, directed = FALSE)
    p_g <- igraph::simplify(p_g,
      remove.multiple = TRUE, remove.loops = TRUE,
      edge.attr.comb = "first"
    )

    if (tbl == TRUE) {
      p_g <- igraph::as_data_frame(p_g) %>% dplyr::as_tibble()
    }
  } else {
    p_g <- NULL
  }

  return(
    list(
      projection_c = c_g,
      projection_p = p_g
    )
  )
}
