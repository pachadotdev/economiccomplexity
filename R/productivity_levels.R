#' Productivity Levels
#'
#' @description foo
#'
#' @details bar
#'
#' @return
#'
#' @param d1 foo
#' @param d2 bar
#' @param country country
#' @param product product
#' @param value value
#'
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom stats setNames
#'
#' @examples
#' set.seed(1810)
#' d1 <- galactic_federation
#' d2 <- setNames(rnorm(1:nrow(galactic_federation), 1000, 200), rownames(galactic_federation))
#' pl <- productivity_levels(d1, d2)
#'
#' @references
#' For more information on prody and its applications see:
#'
#' \insertRef{exportmatters2005}{economiccomplexity}
#'
#' @keywords functions
#'
#' @export

productivity_levels <- function(d1, d2,
                                country = "country", product = "product", value = "value") {
  # sanity checks ----
  if (all(class(d1) %in% c("data.frame", "matrix", "dgCMatrix") == FALSE)) {
    stop("'d1' must be a data.frame, matrix or dgCMatrix")
  }

  if (all(class(d2) %in% c("data.frame", "numeric") == FALSE)) {
    stop("'d2' must be a data.frame or numeric")
  }

  # tidy input data d1 ----
  if (any(class(d1) %in% "data.frame")) {
    d1 <- country_product_aggregation(d1, country, product, value)
    d1 <- dataframe_to_matrix(d1, country, product, value)
  }

  if (class(d1) == "matrix") {
    d1 <- Matrix(d1, sparse = TRUE)
  }

  # tidy input data d2 ----
  if (any(class(d2) %in% "data.frame")) {
    d2 <- country_aggregation(d2, country, value)
    d2 <- setNames(as.numeric(d2$value), d2$country)
  }

  intersect_country_1 <- rownames(d1) %in% names(d2)
  intersect_country_1 <- intersect_country_1[intersect_country_1 == TRUE]

  intersect_country_2 <- names(d2) %in% rownames(d1)
  intersect_country_2 <- intersect_country_2[intersect_country_2 == TRUE]

  if (length(intersect_country_1) != length(intersect_country_2)) {
    warning("'d1' and 'd2' don\'t have the same number of country-side elements, some elements will be dropped")
  }

  d1 <- d1[rownames(d1) %in% names(d2), ]
  d2 <- d2[names(d2) %in% rownames(d1)]

  p1 <- d1 / rowSums(d1)
  p2 <- colSums(p1)

  prody <- (t(p1) %*% d2) / p2
  expy <- p1 %*% prody

  prody <- setNames(as.numeric(prody), rownames(prody))
  expy <- setNames(as.numeric(expy), rownames(expy))

  return(
    list(
      productivity_level_country = expy,
      productivity_level_product = prody
    )
  )
}
