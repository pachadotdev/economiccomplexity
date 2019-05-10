#' Computes RCA (Revealed Comparative Advantage)
#'
#' @export
#' @param d a tibble (or data frame) in long format
#' @param c column that contains exporting countries (e.g. "reporter_iso")
#' @param p column that contains products (e.g. "product_code)
#' @param v column that contains traded values (e.g. "trade_value_usd")
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by mutate
#' @importFrom rlang sym syms
#' @examples
#' rca(d = fantasy_world_long, c = "country", p = "product")
#' @keywords functions

rca <- function(d = NULL, c = NULL, p = NULL, v = NULL) {
  d %>%
    # Sum by country and product
    dplyr::group_by(!!!syms(c(c,p))) %>%
    dplyr::mutate(xcp = sum(!!sym(v), na.rm = TRUE)) %>%

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
}
