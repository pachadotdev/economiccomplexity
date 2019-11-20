#' Revealed Comparative Advantage (RCA)
#'
#' @export
#'
#' @description Given a \eqn{C\times P} matrix (C for "countries"
#' and P for "products") or an equivalent 3-columns data.frame with exported
#' values (X) as input, this function computes RCA metric.
#' The equation used in this function is:
#' \deqn{RCA_{cp} = \frac{X_{cp}}{\sum_c X_{cp}} /
#' \frac{\sum_p X_{cp}}{\sum_{c}\sum_{p} X_{cp}}}
#'
#' @param data matrix or data.frame with traded values
#' @param country column containing countries (applies only if d is a
#' data.frame)
#' @param product column containing products (applies only if d is a
#' data.frame)
#' @param value column containing traded values (applies only if d is a
#' data.frame)
#' @param discrete convert to one all the values above a cutoff and zero
#' otherwise (by default is TRUE)
#' @param cutoff all the values below the specified will be converted to zero
#' (by default is 1)
#' @param tbl by default the output is a data.frame unless this is changed to
#' FALSE, in which case the output is a matrix
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches rename
#' pull as_tibble if_else
#' @importFrom tidyr spread gather
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym syms :=
#'
#' @examples
#' ec_rca(ec_trade_1962)
#' @references
#' For more information on revealed comparative advantage and its uses see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

ec_rca <- function(data,
                   country = "country",
                   product = "product",
                   value = "value",
                   discrete = TRUE,
                   cutoff = 1,
                   tbl = TRUE) {
  # sanity checks ----
  if (all(class(data) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
    "dgCMatrix") == FALSE)) {
    stop("data must be a tibble/data.frame or a dense/sparse matrix")
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

  if (!is.logical(tbl)) {
    stop("tbl must be matrix or tibble")
  }

  # convert data from matrix to tibble ----
  if (any(class(data) %in% c("dgeMatrix", "dsCMatrix", "dgCMatrix"))) {
    data <- as.matrix(data)
  }

  if (!is.data.frame(data)) {
    data_rownames <- rownames(data)

    data <- as.data.frame(data) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(!!sym(country) := data_rownames) %>%
      tidyr::gather(!!sym(product), !!sym(value), -!!sym(country))
  }

  # aggregate input data by c and p ----
  data <- data %>%
    # Sum by c and p
    dplyr::group_by(!!!syms(c(country, product))) %>%
    dplyr::summarise(vcp = sum(!!sym(value), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vcp") > 0)

  # compute RCA in tibble form ----
  data <- data %>%
    # Sum by c
    dplyr::group_by(!!sym(country)) %>%
    dplyr::mutate(sum_c_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

    # Sum by p
    dplyr::group_by(!!sym(product)) %>%
    dplyr::mutate(sum_p_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

    # Compute RCA
    dplyr::ungroup() %>%
    dplyr::mutate(
      sum_c_p_vcp = sum(!!sym("vcp"), na.rm = TRUE),
      value = (!!sym("vcp") / !!sym("sum_c_vcp")) / (!!sym("sum_p_vcp") /
        !!sym("sum_c_p_vcp"))
    ) %>%
    dplyr::select(-dplyr::matches("vcp")) %>%

    # Rename columns
    dplyr::rename(country = !!sym(country), product = !!sym(product))

  if (discrete == TRUE) {
    data <- data %>%
      dplyr::mutate(!!sym("value") := dplyr::if_else(!!sym("value") >
        cutoff, 1, 0))
  }

  if (tbl == FALSE) {
    data <- data %>%
      tidyr::spread(!!sym("product"), !!sym("value"))

    data_rownames <- dplyr::select(data, !!sym("country")) %>%
      dplyr::pull()

    data <- dplyr::select(data, -!!sym("country")) %>% as.matrix()
    data[is.na(data)] <- 0
    rownames(data) <- data_rownames
    data <- Matrix::Matrix(data, sparse = TRUE)
  }

  return(data)
}
