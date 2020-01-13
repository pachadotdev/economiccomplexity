#' Productivity Levels
#'
#' @export
#' @param d1 tibble/data.frame in long format, it must contain the columns country (character/factor),
#' product (character/factor) and export value (numeric)
#' @param d2 tibble/data.frame in long format, it must contain the columns country (character/factor),
#' and GDP per capita (numeric)
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom stats setNames
#' @examples
#' pl <- productivity_levels(
#'   d1 = world_trade_2017,
#'   d2 = world_gdp_and_population_2017
#' )
#' @references
#' For more information on prody and its applications see:
#'
#' \insertRef{exportmatters2005}{economiccomplexity}
#' @keywords functions

productivity_levels <- function(d1 = NULL, d2 = NULL) {
  # sanity checks ----
  if (all(class(d1) %in% c("data.frame") == FALSE)) {
    stop("d1 must be a tibble/data.frame")
  }

  if (all(class(d2) %in% c("data.frame") == FALSE)) {
    stop("d2 must be a tibble/data.frame")
  }

  # tidy input data d1 ----
  if (any(class(d1) %in% "data.frame")) {
    d1 <- source_target_aggregation(d1, source, target, value)
    d1 <- dataframe_to_matrix(d1, source, target, value)
  }

  # tidy input data d2 ----
  if (any(class(d1) %in% "data.frame")) {
    d2 <- source_aggregation(d2, source, value)
    d2 <- setNames(as.numeric(d2$value), d2$source)
  }

  # create exports-gdp table ----
  m <- d1 %>%
    tidyr::spread(!!sym(p1), !!sym("vcp")) %>%
    dplyr::inner_join(d2, by = stats::setNames(c2, c1))

  if (nrow(m) < nrow(unique(d1[, c1]))) {
    warning("Joining d1 and d2 resulted in a table with less reporting countries than those in d1.")
  }

  if (nrow(m) < nrow(d2)) {
    warning("Joining d1 and d2 resulted in a table with less reporting countries than those in d2.")
  }

  # convert m to matrix ----
  m_rownames <- dplyr::select(m, !!sym(c1)) %>% dplyr::pull()

  m2 <- dplyr::select(m, !!sym(v2)) %>% dplyr::pull()

  m <- dplyr::select(m, -!!sym(c1), -!!sym(v2)) %>% as.matrix()
  m[is.na(m)] <- 0
  m <- Matrix::Matrix(m, sparse = TRUE)

  rownames(m) <- m_rownames

  p1 <- mat / rowSums(mat)
  p2 <- colSums(p1)

  prody <- (t(p1) %*% mat2) / p2

  expy <- ((t(p1 %*% mat2)) / p2) %*% t(p1)

  return(
    list(
      economies_productivity_level = expy,
      countries_productivity_level = prody
    )
  )
}
