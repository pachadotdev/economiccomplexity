#' Countries position
#'
#' @export
#'
#' @param rca matrix or tibble/data.frame (e.g. the
#' output of \code{revealed_comparative_advantage()}).
#' If the input is a matrix it must be a zero/one matrix with countries in rows
#' and products in columns.
#' If the input is a tibble/data.frame it must contain at least three columns
#' with countries, products and values.
#' @param c1 string to indicate the column that contains exporting
#' countries in rca (set to "country" by default)
#' @param p1 string to indicate the column that contains exported products
#' in rca (set to "product" by default)
#' @param v1 string to indicate the column that contains traded values in
#' rca (set to "value" by default)
#' @param pp matrix or tibble/data.frame (e.g. the output of
#' \code{proximity_matrices()}).
#' If the input is a matrix it must be a numeric matrix with products in both
#' rows and columns.
#' If the input is a tibble/data.frame it must contain at least three columns
#' columns with products (twice) and values.
#' @param p21 string to indicate the first column that contains exported
#' products in pp (set to "from" by default)
#' @param p22 string to indicate the second column that contains exported
#' products in pp (set to "to" by default)
#' @param v2 string to indicate the column that contains proximity values in
#' pp (set to "value" by default)
#' @param pci numeric vector or tibble/data.frame with the product complexity
#' index (e.g. the product index from \code{ec_complexity_measures()}).
#' If the input is a vector it must be numeric with optional names.
#' If the input is a tibble/data.frame it must contain at least two columns
#' with products and values.
#' @param p3 string to indicate the column that contains exported products
#' in pp (e.g. "product")
#' @param v3 string to indicate the column that contains proximity values in
#' pp (e.g. "value") from \code{ec_complexity_measures()})
#' @param tbl logical value to use tibble output instead of a matrix
#' output (set to FALSE by default)
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select pull
#' @importFrom tidyr spread
#' @importFrom Matrix Matrix t rowSums colSums
#' @importFrom rlang sym
#'
#' @examples
#' ec_countries_position(
#'   rca = ec_output_demo$rca_tbl,
#'   c1 = "country",
#'   p1 = "product",
#'   v1 = "value",
#'   pp = ec_output_demo$proximity_tbl$proximity_p,
#'   p21 = "from",
#'   p22 = "to",
#'   v2 = "value",
#'   pci = ec_output_demo$complexity_measures_tbl$complexity_index_p,
#'   p3 = "product",
#'   v3 = "value",
#'   tbl = TRUE
#' )
#' @references
#' For more information on proximity distance, complexity outlook, complexity
#' outlook gain and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

ec_countries_position <- function(rca, pp, pci = NULL) {
  dcp <- t(t((1 - rca) %*% pp) / rowSums(pp))

  coc <- ((1 - dcp) * (1 - rca)) %*% pci

  cogc <- Matrix(0, nrow = nrow(rca), ncol = ncol(rca), sparse = T, dimnames = list(rownames(rca), colnames(rca)))

  for (i in 1:nrow(rca)) {
    p1 <- t((t(pp / colSums(pp)) * (1 - rca[i, ])) %*% pci)
    p2 <- dcp[i, ] * pci
    cogc[i, ] <- p1 - p2
  }

  return(
    list(
      proximity_distance = dcp,
      complexity_outlook = coc,
      complexity_outlook_gain = cogc
    )
  )
}
