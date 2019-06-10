#' Revealed Comparative Advantage (RCA)
#'
#' @export
#' @param d tibble/data.frame in long format, it must contain the columns country (character/factor),
#' product (character/factor) and export value (numeric)
#' @param c string to indicate the column that contains exporting countries (e.g. "reporter_iso")
#' @param p string to indicate the column that contains exported products (e.g. "product_code")
#' @param v string to indicate the column that contains traded values (e.g. "trade_value_usd")
#' @param discrete when set to TRUE it will convert all the Revealed Comparative Advantage values
#' to zero or one based on the cutoff value (default set to TRUE)
#' @param cutoff when set to TRUE all the values lower than the specified cutoff will be
#' converted to zero and to one in other case, numeric (default set to 1)
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches rename pull
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym syms
#' @examples
#' rca <- revealed_comparative_advantage(d = world_trade_2017, c = "reporter_iso",
#'     p = "product_code", v = "export_value_usd")
#' @keywords functions

revealed_comparative_advantage <- function(d = NULL, c = NULL, p = NULL, v = NULL,
                                           cutoff = 1, discrete = TRUE, tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(d) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix") == FALSE)) {
    stop("d must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!is.character(c) & !is.character(p) & !is.character(v)) {
    stop("c, p and v must be character")
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

  # aggregate input data by c and p ----
  d <- d %>%
    # Sum by country and product
    dplyr::group_by(!!!syms(c(c,p))) %>%
    dplyr::summarise(vcp = sum(!!sym(v), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vcp") > 0)

  # compute RCA in matrix form ----
  if (tbl_output == FALSE) {
    m <- d %>%
      dplyr::select(!!!syms(c(c,p,"vcp"))) %>%
      tidyr::spread(!!sym(p), !!sym("vcp"))

    m_rownames <- dplyr::select(m, !!sym(c)) %>% dplyr::pull()

    m <- dplyr::select(m, -!!sym(c)) %>% as.matrix()
    m[is.na(m)] <- 0
    m <- Matrix::Matrix(m, sparse = TRUE)

    rownames(m) <- m_rownames

    m <- Matrix::t(Matrix::t(m / Matrix::rowSums(m)) / (Matrix::colSums(m) / sum(m)))
    m <- Matrix::Matrix(m, sparse = TRUE)

    if (discrete == T) {
      m[m <= cutoff] <- 0
      m[m > cutoff] <- 1
    }

    return(m)
  }

  # compute RCA in data.frame form ----
  if (tbl_output == TRUE) {
    d <- d %>%
      # Sum by country
      dplyr::group_by(!!sym(c)) %>%
      dplyr::mutate(sum_c_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

      # Sum by product
      dplyr::group_by(!!sym(p)) %>%
      dplyr::mutate(sum_p_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

      # Compute RCA
      dplyr::ungroup() %>%
      dplyr::mutate(
        sum_c_p_vcp = sum(!!sym("vcp"), na.rm = TRUE),
        value = (!!sym("vcp") / !!sym("sum_c_vcp")) / (!!sym("sum_p_vcp") / !!sym("sum_c_p_vcp"))
      ) %>%

      dplyr::select(-dplyr::matches("vcp")) %>%

      # Rename columns
      dplyr::rename(country = !!sym(c), product = !!sym(p))

    if (discrete == T) {
      d <- d %>%
        mutate(value = ifelse(!!sym("value") > cutoff, 1, 0))
    }

    return(d)
  }
}
