cp_rca_matrix <- function (d,c,p,v) {
  d %>%
    dplyr::select(!!!syms(c(c,p,v))) %>%
    dplyr::mutate(!!sym(v) := ifelse(!!sym(v) > 1, 1, 0)) %>%
    tidyr::spread(!!sym(p), !!sym(v))
}
