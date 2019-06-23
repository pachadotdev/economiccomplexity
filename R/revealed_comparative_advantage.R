#' Revealed Comparative Advantage (RCA)
#'
#' @export
#' @param trade_data tibble/data.frame in long format, it must contain the columns country (character/factor),
#' product (character/factor) and export value (numeric)
#' @param country string to indicate the column that contains exporting countries (e.g. "reporter_iso")
#' @param product string to indicate the column that contains exported products (e.g. "product_code")
#' @param value string to indicate the column that contains traded values (e.g. "trade_value_usd")
#' @param discrete when set to TRUE it will convert all the Revealed Comparative Advantage values
#' to zero or one based on the cutoff value (default set to TRUE)
#' @param cutoff when set to TRUE all the values lower than the specified cutoff will be
#' converted to zero and to one in other case, numeric (default set to 1)
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches rename pull as_tibble
#' @importFrom tidyr spread gather
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym syms :=
#' @examples
#' revealed_comparative_advantage(
#'  trade_data = services_trade_2016$services_trade_2016_matrix,
#'  tbl_output = TRUE
#' )
#' @references
#' For more information on revealed comparative advantage and its uses see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#' @keywords functions

revealed_comparative_advantage <- function(trade_data = NULL,
                                           country = "country",
                                           product = "product",
                                           value = "value",
                                           cutoff = 1,
                                           discrete = TRUE,
                                           tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(trade_data) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix", "dgCMatrix") == FALSE)) {
    stop("trade_data must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!is.character(country) & !is.character(product) & !is.character(value)) {
    stop("country, product and value must be character")
  }

  if (!is.logical(discrete)) {
    stop("discrete must be TRUE or FALSE")
  }

  if (!is.numeric(cutoff)) {
    stop("cutoff must be numeric")
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

  # aggregate input trade_data by c and p ----
  trade_data <- trade_data %>%
    # Sum by country and product
    dplyr::group_by(!!!syms(c(country, product))) %>%
    dplyr::summarise(vcp = sum(!!sym(value), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vcp") > 0)

  # compute RCA in tibble form ----
  trade_data <- trade_data %>%
    # Sum by country
    dplyr::group_by(!!sym(country)) %>%
    dplyr::mutate(sum_c_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

    # Sum by product
    dplyr::group_by(!!sym(product)) %>%
    dplyr::mutate(sum_p_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

    # Compute RCA
    dplyr::ungroup() %>%
    dplyr::mutate(
      sum_c_p_vcp = sum(!!sym("vcp"), na.rm = TRUE),
      value = (!!sym("vcp") / !!sym("sum_c_vcp")) / (!!sym("sum_p_vcp") / !!sym("sum_c_p_vcp"))
    ) %>%
    dplyr::select(-dplyr::matches("vcp")) %>%

    # Rename columns
    dplyr::rename(country = !!sym(country), product = !!sym(product))

  if (discrete == T) {
    trade_data <- trade_data %>%
      mutate(value = ifelse(!!sym("value") > cutoff, 1, 0))
  }

  if (tbl_output == FALSE) {
    trade_data <- trade_data %>%
      tidyr::spread(!!sym(product), !!sym("value"))

    trade_data_rownames <- dplyr::select(trade_data, !!sym(country)) %>% dplyr::pull()

    trade_data <- dplyr::select(trade_data, -!!sym(country)) %>% as.matrix()
    trade_data[is.na(trade_data)] <- 0
    rownames(trade_data) <- trade_data_rownames
    trade_data <- Matrix::Matrix(trade_data, sparse = TRUE)
  }

  return(trade_data)
}
