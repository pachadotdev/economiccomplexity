#' Productivity Levels
#'
#' @export
#' @param trade_data matrix or tibble/data.frame (e.g. \code{world_trade_2017}). If the input is a matrix it
#' must be a zero/one matrix with countries in rows and products in columns.
#' If the input is a tibble/data.frame it must contain at least three columns with countries, products and
#' values.
#' @param c1 string to indicate the column that contains exporting countries in revealed_comparative_advantage
#' (set to "country" by default)
#' @param p1 string to indicate the column that contains exported products in revealed_comparative_advantage
#' (set to "product" by default)
#' @param v1 string to indicate the column that contains traded values in revealed_comparative_advantage
#' (set to "value" by default)
#' @param gdp_data vector or tibble/data.frame (e.g. \code{world_gdp_and_population_2017}).
#' If the input is a vector it must be a numeric vector with optional names.
#' If the input is a tibble/data.frame it must contain at least two columns with countries and values.
#' @param c2 string to indicate the column that contains exporting countries in revealed_comparative_advantage
#' (set to "country" by default)
#' @param v2 string to indicate the column that contains traded values in revealed_comparative_advantage
#' (set to "value" by default)
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches rename pull inner_join
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom stats setNames
#' @importFrom rlang sym syms
#' @examples
#' pl <- productivity_levels(
#'   trade_data = services_trade_2016$services_trade_2016_matrix,
#'   c1 = "country",
#'   p1 = "product",
#'   v1 = "value",
#'   gdp_data = gdp_pc_2016$gdp_pc_2016_numeric,
#'   c2 = "country",
#'   v2 = "value"
#' )
#' @references
#' For more information on prody and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' \insertRef{exportmatters2005}{economiccomplexity}
#' @keywords functions

productivity_levels <- function(trade_data = NULL,
                                c1 = "country",
                                p1 = "product",
                                v1 = "value",
                                gdp_data = NULL,
                                c2 = "country",
                                v2 = "value",
                                tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(trade_data) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix", "dgCMatrix") == FALSE)) {
    stop("trade_data must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (all(class(gdp_data) %in% c("data.frame", "numeric") == FALSE)) {
    stop("gdp_data must be a tibble/data.frame or numeric")
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

  # convert trade_data from matrix to tibble ----
  if (any(class(trade_data) %in% c("dgeMatrix", "dsCMatrix", "dgCMatrix"))) {
    trade_data <- as.matrix(trade_data)
  }

  if (!is.data.frame(trade_data)) {
    trade_data_rownames <- rownames(trade_data)

    trade_data <- as.data.frame(trade_data) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(!!sym("country") := trade_data_rownames) %>%
      tidyr::gather(!!sym("product"), !!sym("value"), -!!sym("country"))
  }

  # convert gdp_data from tibble to numeric ----
  if (!is.data.frame(gdp_data)) {
    gdp_data <- tibble::enframe(gdp_data)
    colnames(gdp_data) <- c(c2,v2)
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

  # create trade-gdp table ----
  trade_gdp <- trade_data %>%
    tidyr::spread(!!sym(p1), !!sym("vcp")) %>%
    dplyr::inner_join(gdp_data, by = stats::setNames(c2, c1))

  if (nrow(trade_gdp) < nrow(unique(trade_data[, c1]))) {
    warning("Joining trade_data and gdp_data resulted in a table with less reporting countries than those in trade_data.")
  }

  if (nrow(trade_gdp) < nrow(gdp_data)) {
    warning("Joining trade_data and gdp_data resulted in a table with less reporting countries than those in gdp_data.")
  }

  # convert trade_gdp to matrix ----
  trade_gdp_rownames <- dplyr::select(trade_gdp, !!sym(c1)) %>% dplyr::pull()

  gdp <- dplyr::select(trade_gdp, !!sym(v2)) %>% dplyr::pull()

  trade <- dplyr::select(trade_gdp, -!!sym(c1), -!!sym(v2)) %>% as.matrix()
  trade[is.na(trade)] <- 0
  trade <- Matrix::Matrix(trade, sparse = TRUE)

  rownames(trade) <- trade_gdp_rownames

  prody <- Matrix::t(Matrix::t(trade / Matrix::rowSums(trade)) / (Matrix::colSums(trade) / sum(trade)))
  prody <- Matrix::colSums(prody * gdp) / Matrix::colSums(prody)

  expy <- Matrix::rowSums((trade / Matrix::rowSums(trade)) * prody)

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
