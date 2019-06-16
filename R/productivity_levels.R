#' Productivity Levels
#'
#' @export
#' @param trade_data tibble/data.frame in long format, it must contain the columns country (character/factor),
#' product (character/factor) and export value (numeric)
#' @param c1 string to indicate the column that contains exporting countries in trade_data (e.g. "reporter_iso")
#' @param p1 string to indicate the column that contains exported products in trade_data (e.g. "product_code")
#' @param v1 string to indicate the column that contains traded values in trade_data(e.g. "trade_value_usd")
#' @param gdp_data tibble/data.frame in long format, it must contain the columns country (character/factor),
#' and GDP per capita (numeric)
#' @param c2 string to indicate the column that contains exporting countries in gdp_data (e.g. "reporter_iso")
#' @param v2 string to indicate the column that contains GDP per capita in gdp_data (e.g. "gdp_pc")
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches rename pull inner_join
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom stats setNames
#' @importFrom rlang sym syms
#' @examples
#' pl <- productivity_levels(
#'   trade_data = world_trade_2017, c1 = "reporter_iso", p1 = "product_code", v1 = "export_value_usd",
#'   gdp_data = world_gdp_and_population_2017, c2 = "reporter_iso", v2 = "gdp_pc_usd"
#' )
#' @references
#' For more information on prody and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' \insertRef{exportmatters2005}{economiccomplexity}
#' @keywords functions

productivity_levels <- function(trade_data = NULL, c1 = NULL, p1 = NULL, v1 = NULL,
                                gdp_data = NULL, c2 = NULL, v2 = NULL,
                                tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(trade_data) %in% c("data.frame") == FALSE)) {
    stop("trade_data must be a tibble/data.frame")
  }

  if (all(class(gdp_data) %in% c("data.frame") == FALSE)) {
    stop("gdp_data must be a tibble/data.frame")
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

  # tidy input data trade_data ----
  trade_data <- trade_data %>%
    # Sum by country and product
    dplyr::group_by(!!!syms(c(c1, p1))) %>%
    dplyr::summarise(vcp = sum(!!sym(v1), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vcp") > 0) %>%
    dplyr::select(!!!syms(c(c1, p1, "vcp")))

  # tidy input data gdp_data ----
  gdp_data <- gdp_data %>%
    dplyr::select(!!!syms(c(c2, v2))) %>%
    dplyr::filter(!!sym(v2) > 0)

  # create exports-gdp table ----
  m <- trade_data %>%
    tidyr::spread(!!sym(p1), !!sym("vcp")) %>%
    dplyr::inner_join(gdp_data, by = stats::setNames(c2, c1))

  if (nrow(m) < nrow(unique(trade_data[, c1]))) {
    warning("Joining trade_data and gdp_data resulted in a table with less reporting countries than those in trade_data.")
  }

  if (nrow(m) < nrow(gdp_data)) {
    warning("Joining trade_data and gdp_data resulted in a table with less reporting countries than those in gdp_data.")
  }

  # convert m to matrix ----
  m_rownames <- dplyr::select(m, !!sym(c1)) %>% dplyr::pull()

  m2 <- dplyr::select(m, !!sym(v2)) %>% dplyr::pull()

  m <- dplyr::select(m, -!!sym(c1), -!!sym(v2)) %>% as.matrix()
  m[is.na(m)] <- 0
  m <- Matrix::Matrix(m, sparse = TRUE)

  rownames(m) <- m_rownames

  prody <- Matrix::t(Matrix::t(m / Matrix::rowSums(m)) / (Matrix::colSums(m) / sum(m)))
  prody <- Matrix::colSums(prody * m2) / Matrix::colSums(prody)

  expy <- Matrix::rowSums((m / Matrix::rowSums(m)) * prody)

  if (tbl_output == TRUE) {
    prody <- tibble::enframe(prody) %>%
      dplyr::filter(!!sym("value") > 0)

    expy <- tibble::enframe(expy) %>%
      dplyr::filter(!!sym("value") > 0)
  }

  return(
    list(
      economies_productivity_level = expy,
      products_productivity_level = prody
    )
  )
}
