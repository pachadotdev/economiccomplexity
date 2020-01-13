#' Projections of a bipartite network
#'
#' @description TBD
#'
#' @details TBD
#'
#' @param proximity_source a data frame containing proximity
#' values for the elements of set X (e.g. proximity_source from \code{proximity()})
#' @param proximity_target a data frame containing proximity
#' values for the elements of set Y (e.g. proximity_target from \code{proximity()})
#' @param avg_links average number of connections for the projection of X
#' (default set to 4)
#' @param tolerance tolerance for proximity variation on each iteration until
#' obtaining the desired average number of connections (default set to 0.05)
#' @param compute which projection to compute. By default is "both" (both
#' projections) but it can also be "source" or "target".
#'
#' @importFrom igraph graph_from_adjacency_matrix graph_from_data_frame mst as_data_frame degree delete.edges graph.difference E E<-
#'
#' @examples
#' projections(
#'   proximity_source = economiccomplexity_output$proximity$proximity_source,
#'   proximity_target = economiccomplexity_output$proximity$proximity_target
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

projections <- function(proximity_source, proximity_target,
                        avg_links = 4, tolerance = 0.05, compute = "both") {
  # sanity checks ----
  if (class(proximity_source) != "dsCMatrix" |
    class(proximity_target) != "dsCMatrix") {
    stop("'proximity_source' and 'proximity_target' must be dtCMatrix")
  }

  if (!is.numeric(avg_links)) {
    stop("'avg_links' must be numeric")
  }

  if (!any(compute %in% c("both", "source", "target"))) {
    stop("'compute' must be 'both', 'source' or 'target'")
  }

  if (compute == "both") {
    compute2 <- c("source", "target")
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

        proximity_g_nmst <- delete.edges(proximity_g, which(abs(E(proximity_g)$weight) <= threshold))
        proximity_g_nmst <- graph.difference(proximity_g_nmst, proximity_mst)

        proximity_g <- rbind(
          as_data_frame(proximity_mst),
          as_data_frame(proximity_g_nmst)
        )

        proximity_g <- graph_from_data_frame(proximity_g, directed = F)

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

  if (any("source" %in% compute2) == TRUE) {
    message("computing target projection...")
    message(rep("-", 50))
    xg <- trim_network(proximity_source, avg_links)
  } else {
    xg <- NULL
  }

  if (any("target" %in% compute2) == TRUE) {
    message("computing target projection...")
    message(rep("-", 50))
    yg <- trim_network(proximity_target, avg_links)
  } else {
    yg <- NULL
  }

  return(
    list(
      network_source = xg,
      network_target = yg
    )
  )
}
