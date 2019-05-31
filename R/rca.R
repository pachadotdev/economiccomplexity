#' RCA (Revealed Comparative Advantage)
#'
#' @export
#' @param d "tibble" (or "data frame") in long format
#' @param c column that contains exporting countries (e.g. "reporter_iso")
#' @param p column that contains products (e.g. "product_code)
#' @param x column that contains traded values (e.g. "trade_value_usd")
#' @param output "tibble" or "matrix"
#' @param discrete T or F
#' @param cutoff 1 by default
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by mutate summarise matches
#' @importFrom purrr as_vector
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym syms
#' @examples
#' rca <- rca(d = world_trade_1980, c = "reporter_iso",
#'     p = "product_code", x = "export_value_usd")
#' @keywords functions

rca <- function(d = NULL, c = NULL, p = NULL, x = NULL, discrete = T, cutoff = 1, output = "matrix") {
  d2 <- d %>%
    # Sum by country and product
    dplyr::group_by(!!!syms(c(c,p))) %>%
    dplyr::summarise(xcp = sum(!!sym(x), na.rm = TRUE)) %>%
    dplyr::filter(!!sym("xcp") > 0)

  if (output == "tibble") {
     d2 <- d2 %>%
      # Sum by country
      dplyr::group_by(!!sym(c)) %>%
      dplyr::mutate(sum_c_xcp = sum(!!sym("xcp"), na.rm = TRUE)) %>%

      # Sum by product
      dplyr::group_by(!!sym(p)) %>%
      dplyr::mutate(sum_p_xcp = sum(!!sym("xcp"), na.rm = TRUE)) %>%

      # Compute RCA
      dplyr::ungroup() %>%
      dplyr::mutate(
        sum_c_p_xcp = sum(!!sym("xcp"), na.rm = TRUE),
        rca = (!!sym("xcp") / !!sym("sum_c_xcp")) / (!!sym("sum_p_xcp") / !!sym("sum_c_p_xcp"))
      ) %>%

      # Remove intermediate columns
      dplyr::select(-dplyr::matches("xcp"))

     if (discrete == T) {
       d2 <- d2 %>%
         mutate(rca = ifelse(rca <= cutoff, 0, 1))
     }

     return(d2)
  }

  if (output == "matrix") {
    m <- d2 %>%
      dplyr::select(!!!syms(c(c,p,"xcp"))) %>%
      tidyr::spread(!!sym(p), !!sym("xcp")) %>%
      dplyr::ungroup()

    row_names <- dplyr::select(m, !!sym(c)) %>% purrr::as_vector()

    m <- dplyr::select(m, -!!sym(c)) %>% as.matrix()
    m[is.na(m)] <- 0
    m <- Matrix::Matrix(m, sparse = TRUE)

    rownames(m) <- row_names

    m <- Matrix::t(Matrix::t(m / Matrix::rowSums(m)) / (Matrix::colSums(m) / sum(m)))
    m <- Matrix::Matrix(m, sparse = TRUE)

    if (discrete == T) {
      m[m <= cutoff] <- 0
      m[m > cutoff] <- 1
    }

    return(m)
  }
}
