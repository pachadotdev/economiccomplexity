#' Networks
#'
#' @export
#' @param pc matrix or tibble
#' @param pp matrix or tibble
#' @param c_cutoff numeric
#' @param p_cutoff numeric
#' @importFrom methods as
#' @importFrom magrittr %>%
#' @importFrom dplyr as_tibble filter mutate bind_rows
#' @importFrom tidyr gather
#' @importFrom igraph graph_from_data_frame mst as_data_frame
#' @importFrom rlang sym
#' @examples
#' rca <- rca(d = world_trade_1980, c = "reporter_iso",
#'     p = "product_code", x = "export_value_usd")
#'
#' indices <- indices(rca, method = "reflections", maxiter = 20,
#'     output = "matrix")
#'
#' m <- indices$m
#' kc0 <- indices$kc0
#' kp0 <- indices$kp0
#'
#' proximity <- proximity(m, kc = kc0, kp = kp0, output = "matrix")
#' @keywords functions

networks <- function(pc, pp, c_cutoff = 0.35, p_cutoff = 0.55) {
  if (!output %in% c("matrix", "tibble")) {
    stop()
  }

  if (!is.data.frame(pc) & !is.data.frame(pp)) {
    # countries
    pc <- as.matrix(pc)
    pc[upper.tri(pc, diag = T)] <- 0
    row_names <- rownames(pc)

    pc <- as.matrix(pc) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = row_names) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)

    # products
    pp <- as.matrix(pp)
    pp[upper.tri(pp, diag = T)] <- 0
    row_names <- rownames(pp)

    pp <- as.matrix(pp) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(from = row_names) %>%
      tidyr::gather(!!sym("to"), !!sym("value"), -!!sym("from")) %>%
      dplyr::filter(!!sym("value") > 0)
  }

  # countries
  pc <- mutate(pc, value = -1 * !!sym("value"))

  c_g <- igraph::graph_from_data_frame(pc, directed = F)
  c_mst <- igraph::mst(c_g, weights = pc$value, algorithm = "prim")
  c_mst <- igraph::as_data_frame(c_mst)

  c_g_nmst <- pc %>%
    dplyr::filter(!!sym("value") <= -1 * c_cutoff) %>%
    dplyr::anti_join(c_mst, by = c("from", "to"))

  c_g <- dplyr::bind_rows(c_mst, c_g_nmst)
  c_g <- igraph::graph_from_data_frame(c_g, directed = F)
  c_g <- igraph::simplify(c_g, remove.multiple = TRUE, remove.loops = TRUE,
                          edge.attr.comb = "first")

  # products
  pp <- mutate(pp, value = -1 * !!sym("value"))

  p_g <- igraph::graph_from_data_frame(pp, directed = F)
  p_mst <- igraph::mst(p_g, weights = pp$value, algorithm = "prim")
  p_mst <- igraph::as_data_frame(p_mst)

  p_g_nmst <- pp %>%
    dplyr::filter(!!sym("value") <= -1 * p_cutoff) %>%
    dplyr::anti_join(p_mst, by = c("from", "to"))

  p_g <- dplyr::bind_rows(p_mst, p_g_nmst)
  p_g <- igraph::graph_from_data_frame(p_g, directed = F)
  p_g <- igraph::simplify(p_g, remove.multiple = TRUE, remove.loops = TRUE,
                          edge.attr.comb = "first")

  networks <- list(countries_network = c_g, products_network = p_g)
  return(networks)
}
