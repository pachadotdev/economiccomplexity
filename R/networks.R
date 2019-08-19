#' Networks
#'
#' @export
#'
#' @param pc matrix or tibble/data.frame, if d is a tibble/
#' data.frame it must contain the columns from (character/factor), to
#' (character/factor) and value (numeric), if it is a matrix it must be a
#' numeric matrix with countries in row names and column names
#' @param pp matrix or tibble/data.frame, if d is a
#' tibble/data.frame it must contain the columns from (character/factor), to
#' (character/factor) and value (numeric), if it is a matrix it must be a
#' numeric matrix with products in row names and column names
#' @param cutoff_c all the values below the specified
#' cutoff_c will be converted to 0 and excluded from the countries
#' network (default set to 0.2)
#' @param cutoff_p all the values below the specified
#' cutoff_p will be converted to 0 and excluded from the products network
#' (default set to 0.4)
#' @param tbl when set to TRUE the output will be a tibble instead of a
#' graph (default set to FALSE)
#' @param compute by default set to "both", it can also be "country" or
#' "product"
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom igraph graph_from_data_frame mst as_data_frame simplify
#' @importFrom rlang sym
#'
#' @examples
#' ec_networks(
#'   pc = ec_output_demo$proximity_tbl$proximity_c,
#'   pp = ec_output_demo$proximity_tbl$proximity_p,
#'   tbl = TRUE
#' )
#'
#' @references
#' For more information on networks such as the product space and its
#' applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

ec_networks <- function(pc,
                        pp,
                        cutoff_c = 0.2,
                        cutoff_p = 0.4,
                        tbl = FALSE,
                        compute = "both") {
  # sanity checks ----
  if (all(class(pc) %in% c(
    "data.frame", "matrix",
    "dgeMatrix", "dgCMatrix", "dsCMatrix"
  ) == FALSE) &
    all(class(pp) %in% c(
      "data.frame", "matrix",
      "dgeMatrix", "dgCMatrix", "dsCMatrix"
    ) == FALSE)) {
    stop("pc and pp must be tibble/data.frame
          or dense/sparse matrix")
  }

  if (!is.numeric(cutoff_c) & !is.numeric(cutoff_p)) {
    stop("cutoff_c & cutoff_p must be numeric")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be matrix or tibble")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("compute must be both, country or product")
  }

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  if (any("country" %in% compute2) == TRUE) {
    # arrange country matrix ----
    if (any(class(pc) %in% c(
      "dgeMatrix", "dgCMatrix",
      "dsCMatrix"
    ) == TRUE)) {
      pc <- as.matrix(pc)
    }

    if (is.matrix(pc)) {
      pc[upper.tri(pc, diag = TRUE)] <- 0
      row_names <- rownames(pc)

      pc <- pc %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute countries network ----
    pc <- dplyr::mutate(pc,
      value = -1 * !!sym("value")
    )

    c_g <- igraph::graph_from_data_frame(pc, directed = FALSE)

    c_mst <- igraph::mst(c_g,
      weights = pc$value,
      algorithm = "prim"
    )
    c_mst <- igraph::as_data_frame(c_mst)

    c_g_nmst <- pc %>%
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
    if (any(class(pp) %in% c(
      "dgeMatrix", "dgCMatrix",
      "dsCMatrix"
    ) == TRUE)) {
      pp <- as.matrix(pp)
    }

    if (is.matrix(pp)) {
      pp[upper.tri(pp, diag = TRUE)] <- 0
      row_names <- rownames(pp)

      pp <- pp %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(from = row_names) %>%
        tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
        dplyr::filter(!!sym("value") > 0)
    }

    # compute products network ----
    pp <- dplyr::mutate(pp,
      value = -1 * !!sym("value")
    )

    p_g <- igraph::graph_from_data_frame(pp, directed = FALSE)

    p_mst <- igraph::mst(p_g,
      weights = pp$value,
      algorithm = "prim"
    )
    p_mst <- igraph::as_data_frame(p_mst)

    p_g_nmst <- pp %>%
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

  return(list(network_country = c_g, network_product = p_g))
}
