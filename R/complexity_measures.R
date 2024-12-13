#' Complexity Measures
#'
#' @description \code{complexity_measures()} computes the Economic Complexity
#' Index and the Product Complexity Index.
#'
#' @details The current implementation follows
#' \insertCite{measuringcomplexity2015}{economiccomplexity} to obtain different
#' alternatives that account for diversification in bipartite relations.
#'
#' @return A list of two named numeric vectors.
#'
#' @param balassa_index (Type: matrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param method (Type: character) one of these methods: fitness,
#' reflections or eigenvalues. By default this is set to \code{"fitness"}.
#' @param iterations (Type: numeric) the number of iterations to use.
#' By default this is set to \code{20}.
#' @param extremality (Type: numeric) the parameter to use in the fitness
#' method. The other methods don't use this parameter.
#' By default this is set to \code{1}.
#'
#' @importFrom stats cor
#'
#' @examples
#' co <- complexity_measures(economiccomplexity_output$balassa_index)
#'
#' # partial view of indexes
#' n <- seq_len(5)
#' co$complexity_index_country[n]
#' co$complexity_index_product[n]
#'
#' @references
#' For more information on this index see:
#'
#' \insertRef{measuringcomplexity2015}{economiccomplexity}
#'
#' and the references therein.
#'
#' @keywords functions
#'
#' @export
complexity_measures <- function(balassa_index, method = "fitness",
                                iterations = 20, extremality = 1) {
  # sanity checks ----
  if (!(any(class(balassa_index) %in% "matrix") == TRUE)) {
    stop("'balassa_index' must be a matrix")
  }

  if (!(any(method %in% c("fitness", "reflections", "eigenvalues")) == TRUE)) {
    stop("'method' must be 'fitness', 'reflections' or 'eigenvalues'")
  }

  if (!is.integer(iterations)) {
    iterations <- as.integer(iterations)

    if (iterations < 2L) {
      stop("'iterations' must be integer and >= 2")
    }
  }

  if (iterations %% 2 != 0) {
    iterations <- iterations + 1
    warning(paste("'iterations' was changed to 'iterations + 1' to work with",
                  "an even number of iterations"))
  }

  # compute complexity measures ----
  rows_balassa_index <- rowSums(balassa_index)[rowSums(balassa_index) > 0]
  cols_balassa_index <- colSums(balassa_index)[colSums(balassa_index) > 0]

  balassa_index <- balassa_index[
    rownames(balassa_index) %in% names(rows_balassa_index),
    colnames(balassa_index) %in% names(cols_balassa_index)
  ]

  if (method == "fitness") {
    return(fitness_method_(balassa_index, iterations, extremality))
  }

  if (method == "reflections") {
    return(reflections_method_(balassa_index, iterations))
  }

  if (method == "eigenvalues") {
    # to check if a sign correction is needed
    reflections_output <- reflections_method_(balassa_index, iterations)
    eigenvalues_output <- eigenvalues_method_(balassa_index, iterations)
    
    # correct complexity_index_country sign when required
    if (isTRUE(cor(eigenvalues_output$complexity_index_country,
      reflections_output$complexity_index_country,
      use = "pairwise.complete.obs") < 0)) {
      message("applying sign correction to country index...")
      eigenvalues_output$complexity_index_country <- (-1) *
        eigenvalues_output$complexity_index_country
    }

    # correct complexity_index_product sign when required
    if (isTRUE(cor(eigenvalues_output$complexity_index_product,
      reflections_output$complexity_index_product,
      use = "pairwise.complete.obs") < 0)) {
      message("applying sign correction to product index...")
      eigenvalues_output$complexity_index_product <- (-1) *
        eigenvalues_output$complexity_index_product
    }

    return(eigenvalues_output)
  }
}
