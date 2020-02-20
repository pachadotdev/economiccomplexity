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
#' @param balassa_index (Type: dgCMatrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param method (Type: character) one of these methods: fitness,
#' reflections or eigenvalues. By default this is set to \code{"fitness"}.
#' @param iterations (Type: numeric) the number of iterations to use.
#' By default this is set to \code{20}.
#' @param extremality (Type: numeric) the parameter to use in the fitness
#' method. The other methods don't use this parameter.
#' By default this is set to \code{1}.
#'
#' @importFrom Matrix rowSums colSums
#' @importFrom stats cor
#'
#' @examples
#' co <- complexity_measures(economiccomplexity_output$balassa_index)
#'
#' # partial view of indexes
#' co$complexity_index_country[1:5]
#' co$complexity_index_product[1:5]
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
complexity_measures <- function(balassa_index, method = "fitness", iterations = 20, extremality = 1) {
  # sanity checks ----
  if (!(any(class(balassa_index) %in% "dgCMatrix") == TRUE)) {
    stop("'balassa_index' must be a dgCMatrix")
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
    warning("'iterations' was changed to 'iterations + 1' to work with an even number of iterations")
  }

  # compute complexity measures ----
  if (method == "fitness") {
    fitness_output <- fitness_method(balassa_index, iterations, extremality)
    xci <- fitness_output$xci
    yci <- fitness_output$yci
  }

  if (method == "reflections") {
    reflections_output <- reflections_method(balassa_index, iterations)
    xci <- reflections_output$xci
    yci <- reflections_output$yci
  }

  if (method == "eigenvalues") {
    # to check if a sign correction is needed
    reflections_output <- reflections_method(balassa_index, iterations)
    xci_r <- reflections_output$xci
    yci_r <- reflections_output$yci

    eigenvalues_output <- eigenvalues_method(balassa_index, iterations)
    xci <- eigenvalues_output$xci
    yci <- eigenvalues_output$yci

    # correct xci sign when required
    if (isTRUE(cor(xci, xci_r, use = "pairwise.complete.obs") < 0)) {
      xci <- (-1) * xci
    }

    # correct yci sign when required
    if (isTRUE(cor(yci, yci_r, use = "pairwise.complete.obs") < 0)) {
      yci <- (-1) * yci
    }
  }

  return(
    list(
      complexity_index_country = xci,
      complexity_index_product = yci
    )
  )
}

#' Fitness Method
#' @param balassa_index (Type: dgCMatrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param iterations (Type: numeric) the number of iterations to use.
#' By default this is set to \code{20}.
#' @param extremality (Type: numeric) the parameter to use in the fitness
#' method. The other methods don't use this parameter.
#' By default this is set to \code{1}.
#' @importFrom Matrix Matrix crossprod
#' @importFrom stats setNames
#' @keywords internal
fitness_method <- function(balassa_index, iterations, extremality) {
  # create empty matrices
  kx <- Matrix(0,
               nrow = nrow(balassa_index), ncol = iterations,
               sparse = TRUE
  )

  ky <- Matrix(0,
               nrow = ncol(balassa_index), ncol = iterations,
               sparse = TRUE
  )

  # fill the first columns with rowSums(balassa_index) and colSums(balassa_index) to start iterating
  kx[, 1] <- 1
  ky[, 1] <- 1

  # compute cols 2 to "no. of iterations" by iterating from col 1
  for (j in 2:ncol(kx)) {
    kx[, j] <- balassa_index %*% ky[, (j - 1)]
    kx[, j] <- kx[, j] / mean(kx[, j])

    ky[, j] = 1 / (crossprod(balassa_index,  (1 / kx[, (j - 1)])^extremality))^(1 / extremality)
    ky[, j] <- ky[, j] / mean(ky[, j])
  }

  return(
    list(
      xci = setNames(
        kx[, iterations],
        rownames(balassa_index)
      ),
      yci = setNames(
        ky[, iterations],
        colnames(balassa_index)
      )
    )
  )
}

#' Reflections Method
#' @param balassa_index (Type: dgCMatrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param iterations (Type: numeric) the number of iterations to use.
#' By default this is set to \code{20}.
#' @importFrom Matrix Matrix crossprod
#' @importFrom stats sd setNames
#' @keywords internal
reflections_method <- function(balassa_index, iterations) {
  # create empty matrices
  kx <- Matrix(0,
               nrow = nrow(balassa_index), ncol = iterations,
               sparse = TRUE
  )

  ky <- Matrix(0,
               nrow = ncol(balassa_index), ncol = iterations,
               sparse = TRUE
  )

  # fill the first columns with rowSums(balassa_index) and colSums(balassa_index) to start iterating
  kx[, 1] <- rowSums(balassa_index)
  ky[, 1] <- colSums(balassa_index)

  # compute cols 2 to "no. of iterations" by iterating from col 1
  for (j in 2:ncol(kx)) {
    kx[, j] <- (balassa_index %*% ky[, (j - 1)]) / rowSums(balassa_index)
    ky[, j] <- (crossprod(balassa_index, kx[, (j - 1)])) / colSums(balassa_index)
  }

  # xci is of odd order and normalized
  # yci is of even order and normalized
  return(
    list(
      xci = setNames(
        (kx[, iterations - 1] - mean(kx[, iterations - 1])) / sd(kx[, iterations - 1]),
        rownames(balassa_index)
      ),
      yci = setNames(
        (ky[, iterations] - mean(ky[, iterations])) / sd(ky[, iterations]),
        colnames(balassa_index)
      )
    )
  )
}

#' Eigenvalues Method
#' @param balassa_index (Type: dgCMatrix) the output from
#' \code{balassa_index()}) or an equivalent arrangement.
#' @param iterations (Type: numeric) the number of iterations to use.
#' By default this is set to \code{20}.
#' @importFrom Matrix Matrix t
#' @importFrom stats sd setNames
#' @keywords internal
eigenvalues_method <- function(balassa_index, iterations) {
  # compute eigenvalues for xci
  xci <- eigen((balassa_index / rowSums(balassa_index)) %*% (t(balassa_index) / colSums(balassa_index)))
  xci <- Re(xci$vectors[, 2])

  # compute eigenvalues for yci
  yci <- eigen((t(balassa_index) / colSums(balassa_index)) %*% (balassa_index / rowSums(balassa_index)))
  yci <- Re(yci$vectors[, 2])

  return(
    list(
      xci = setNames(
        (xci - mean(xci)) / sd(xci),
        rownames(balassa_index)
      ),
      yci = setNames(
        (yci - mean(yci)) / sd(yci),
        colnames(balassa_index)
      )
    )
  )
}
