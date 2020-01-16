#' Projections of a Bipartite Network
#'
#' @description \code{proximity()} computes two graphs obtained after the
#' proximity matrices that are particularly useful to visualize product-product
#' and country-country similarity.
#'
#' @details The current implementation follows
#' \insertCite{atlas2014}{economiccomplexity} to create simplifiedgraphs
#' that correspond to a simplification of the proximity matrices. The result is
#' obtained by iterating and reducing links until the desired average number of
#' links per node is obtained, or a spaning tree after the strongest links is
#' returned when is not possible to return the desired network.
#'
#' @return A list of two graphs.
#'
#' @param proximity_country (Type: dgCMatrix) the output from
#' \code{proximity()}) or an equivalent arrangement.
#' @param proximity_product (Type: dgCMatrix) the output from
#' \code{proximity()}) or an equivalent arrangement.
#' @param avg_links average number of connections for the projections.
#' By default this is set to \code{4}.
#' @param tolerance tolerance for proximity variation on each iteration until
#' obtaining the desired average number of connections.
#' By default this is set to \code{0.05}.
#' @param compute (Type: character) the proximity to compute. By default this is
#' \code{"both"} (both projections) but it can also be \code{"country"}
#' or \code{"product"}.
#'
#' @importFrom igraph graph_from_adjacency_matrix mst degree delete.edges graph.difference E E<- graph.union
#'
#' @examples
#' projections(
#'   proximity_country = economiccomplexity_output$proximity$proximity_country,
#'   proximity_product = economiccomplexity_output$proximity$proximity_product
#' )
#' @references
#' For more information see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' and the references therein.
#'
#' @keywords functions
#'
#' @export

projections <- function(proximity_country, proximity_product,
                        avg_links = 4, tolerance = 0.05, compute = "both") {
  # sanity checks ----
  if (class(proximity_country) != "dsCMatrix" |
    class(proximity_product) != "dsCMatrix") {
    stop("'proximity_country' and 'proximity_product' must be dsCMatrix")
  }

  if (!is.numeric(avg_links)) {
    stop("'avg_links' must be numeric")
  }

  if (!any(compute %in% c("both", "country", "product"))) {
    stop("'compute' must be 'both', 'country' or 'product'")
  }

  if (compute == "both") {
    compute2 <- c("country", "product")
  } else {
    compute2 <- compute
  }

  trim_network <- function(proximity_d, avg_d) {
    # compute network ----
    proximity_d <- (-1) * proximity_d

    proximity_g <- graph_from_adjacency_matrix(proximity_d, weighted = TRUE, mode = "undirected", diag = FALSE)

    proximity_mst <- mst(proximity_g, algorithm = "prim")

    threshold <- 0
    avg_links_n <- FALSE

    while (avg_links_n == FALSE) {
      if (threshold < 1) {
        message(sprintf("%s threshold...", threshold))

        proximity_nmst <- delete.edges(proximity_g, which(abs(E(proximity_g)$weight) <= threshold))
        proximity_nmst <- graph.difference(proximity_nmst, proximity_mst)

        proximity_g <- graph.union(proximity_mst, proximity_nmst)
        E(proximity_g)$weight <- pmin(E(proximity_g)$weight_1, E(proximity_g)$weight_2, na.rm = T)

        avg_links_n <- ifelse(mean(degree(proximity_g)) <= avg_d, TRUE, FALSE)
        threshold <- threshold + tolerance

        if (avg_links_n == TRUE) {
          message(sprintf("%s threshold achieves the avg number of connections", threshold))
          E(proximity_g)$weight <- (-1) * E(proximity_g)$weight
          return(proximity_g)
        }
      } else {
        warning("no threshold achieves the avg number of connections\nreturning maximum spanning tree")
        avg_links_n <- TRUE
        E(proximity_mst)$weight <- (-1) * E(proximity_mst)$weight
        return(proximity_mst)
      }
    }
  }

  if (any("country" %in% compute2) == TRUE) {
    message("computing product projection...")
    message(rep("-", 50))
    xg <- trim_network(proximity_country, avg_links)
  } else {
    xg <- NULL
  }

  if (any("product" %in% compute2) == TRUE) {
    message("computing product projection...")
    message(rep("-", 50))
    yg <- trim_network(proximity_product, avg_links)
  } else {
    yg <- NULL
  }

  return(
    list(
      network_country = xg,
      network_product = yg
    )
  )
}
