globalVariables(
  c(
    "pairs",
    "select",
    "sum_c_p_xcp",
    "sum_c_xcp",
    "sum_p_xcp",
    "xcp"
  )
)

#' Computes RCA (Revealed Comparative Advantage)
#'
#' @export
#' @param data a tibble (or data frame) in long format
#' @param c name of column that contains countries (by default is "country")
#' @param p name of column that contains products (by default is "product")
#' @param value name of column that contains trade values (by default is "export_val")
#' @importFrom magrittr %>%
#' @importFrom dplyr select_ select mutate group_by ungroup filter distinct
#' @importFrom tidyr unite
#' @importFrom stats setNames
#' @examples
#' # Demo dataset 'Fantasy World'
#' rca(fantasy_world_long)
#' @keywords functions

rca <- function(data = data, c = "country", p = "product", value = "export_val") {
  data_rca <- data %>%
    select_(.dots = c(c, p, value)) %>%
    setNames(c("c", "p", "value")) %>%
    unite(pairs, c, p, remove = FALSE) %>%
    group_by(p) %>% # Sum by categories in P
    mutate(sum_p_xcp = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(c) %>% # Sum by categories in C
    mutate(sum_c_xcp = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(pairs) %>% # Sum by pairs (C,P)
    mutate(xcp = sum(value, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(pairs, .keep_all = TRUE) %>%
    select(-pairs) %>%
    filter(sum_c_xcp > 0) %>%
    mutate(sum_c_p_xcp = sum(xcp, na.rm = TRUE)) %>%  # Total sum
    mutate(rca = (xcp / sum_c_xcp) / (sum_p_xcp / sum_c_p_xcp)) %>%  # Compute RCA
    select(c, p, value, rca)
  return(data_rca)
}
