#' Revealed Comparative Advantage (RCA)
#'
#' @export
#' @param d1 tibble/data.frame in long format, it must contain the columns country (character/factor),
#' product (character/factor) and export value (numeric)
#' @param d2 tibble/data.frame in long format, it must contain the columns country (character/factor),
#' and GDP per capita (numeric)
#' @param c1 string to indicate the column that contains exporting countries in d1 (e.g. "reporter_iso")
#' @param p1 string to indicate the column that contains exported products in d1 (e.g. "product_code")
#' @param v1 string to indicate the column that contains traded values in d1(e.g. "trade_value_usd")
#' @param c2 string to indicate the column that contains exporting countries in d2 (e.g. "reporter_iso")
#' @param v2 string to indicate the column that contains GDP per capita in d2 (e.g. "gdp_pc")
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches rename pull inner_join
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom stats setNames
#' @importFrom rlang sym syms
#' @examples
#' prody <- prody(
#'     d1 = world_trade_2017, c1 = "reporter_iso", p1 = "product_code", v1 = "export_value_usd",
#'     d2 = gdp_and_population_2017, c2 = "reporter_iso", v2 = "gdp_pc_usd"
#' )
#' @keywords functions

prody <- function(d1 = NULL, c1 = NULL, p1 = NULL, v1 = NULL,
                  d2 = NULL, c2 = NULL, v2 = NULL,
                  tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(d1) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix") == FALSE)) {
    stop("d1 must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(d2) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix") == FALSE)) {
    stop("d1 must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!is.character(c1) & !is.character(p1) & !is.character(v1)) {
    stop("c1, p1 and v1 must be character")
  }

  if (!is.character(c2) & !is.character(v2)) {
    stop("c2 and v2 must be character")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be matrix or tibble")
  }

  # TODO: d1 or d2 is a matrix ----
  # tidy input data d1 ----
  d1 <- d1 %>%
    # Sum by country and product
    dplyr::group_by(!!!syms(c(c1,p1))) %>%
    dplyr::summarise(vcp = sum(!!sym(v1), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vcp") > 0) %>%
    dplyr::select(!!!syms(c(c1,p1,"vcp")))

  # tidy input data d2 ----
  d2 <- d2 %>%
    dplyr::select(!!!syms(c(c2,v2))) %>%
    dplyr::filter(!!sym(v2) > 0)

  # create exports-gdp table
  m <- d1 %>%
    tidyr::spread(!!sym(p1), !!sym("vcp")) %>%
    dplyr::inner_join(d2, by = stats::setNames(c2, c1))

  if (nrow(m) > nrow(d1)) {
    warning("Joining d1 and d2 resulted in a table with more reporting countries than those in d1.")
  }

  if (nrow(m) > nrow(d2)) {
    warning("Joining d1 and d2 resulted in a table with more reporting countries than those in d2.")
  }

  m_rownames <- dplyr::select(m, !!sym(c1)) %>% dplyr::pull()

  m2 <- dplyr::select(m, !!sym(v2)) %>% dplyr::pull()

  m <- dplyr::select(m, -!!sym(c1), -!!sym(v2)) %>% as.matrix()
  m[is.na(m)] <- 0
  m <- Matrix::Matrix(m, sparse = TRUE)

  rownames(m) <- m_rownames

  m <- Matrix::t(Matrix::t(m / Matrix::rowSums(m)) / (Matrix::colSums(m) / sum(m)))
  m <- Matrix::colSums(m * m2) / Matrix::colSums(m)

  if (tbl_output == TRUE) {
    m <- tibble::enframe(m) %>%
      dplyr::filter(!!sym("value") > 0)
  }

  return(m)
}
