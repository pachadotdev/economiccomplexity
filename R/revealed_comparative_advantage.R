#' Revealed Comparative Advantage (RCA)
#'
#' @export
#' @param data tibble/data.frame in long format, it must contain the
#' columns c (character/factor), p (character/factor) and export
#' v (numeric)
#' @param c string to indicate the column that contains exporting
#' countries (e.g. "reporter_iso")
#' @param p string to indicate the column that contains exported ps
#' (e.g. "p_code")
#' @param v string to indicate the column that contains traded vs
#' (e.g. "trade_v_usd")
#' @param discrete when set to TRUE it will convert all the Revealed Comparative
#' Advantage vs
#' to zero or one based on the cutoff v (default set to TRUE)
#' @param cutoff when set to TRUE all the vs lower than the specified cutoff
#' will be converted to zero and to one in other case, numeric (default set
#' to 1)
#' @param tbl when set to TRUE the output will be a tibble instead of a
#' matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select group_by ungroup mutate summarise matches rename
#' pull as_tibble if_else
#' @importFrom tidyr spread gather
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym syms :=
#'
#' @examples
#' ec_rca(
#'   data = ec_trade_1962,
#'   tbl = TRUE
#' )
#'
#' @references
#' For more information on revealed comparative advantage and its uses see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' @keywords functions

ec_rca <- function(data = NULL,
                   c = "country",
                   p = "product",
                   v = "value",
                   cutoff = 1,
                   discrete = TRUE,
                   tbl = FALSE) {
  # sanity checks ----
  if (all(class(data) %in% c(
    "data.frame", "matrix", "dgeMatrix",
    "dsCMatrix", "dgCMatrix"
  ) == FALSE)) {
    stop("data must be a tibble/data.frame or a dense/sparse matrix")
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
      dplyr::mutate(!!sym(c) := data_rownames) %>%
      tidyr::gather(!!sym(p), !!sym(v), -!!sym(c))
  }

  # aggregate input data by c and p ----
  data <- data %>%
    # Sum by c and p
    dplyr::group_by(!!!syms(c(c, p))) %>%
    dplyr::summarise(vcp = sum(!!sym(v), na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!sym("vcp") > 0)

  # compute RCA in tibble form ----
  data <- data %>%
    # Sum by c
    dplyr::group_by(!!sym(c)) %>%
    dplyr::mutate(sum_c_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

    # Sum by p
    dplyr::group_by(!!sym(p)) %>%
    dplyr::mutate(sum_p_vcp = sum(!!sym("vcp"), na.rm = TRUE)) %>%

    # Compute RCA
    dplyr::ungroup() %>%
    dplyr::mutate(
      sum_c_p_vcp = sum(!!sym("vcp"), na.rm = TRUE),
      v = (!!sym("vcp") / !!sym("sum_c_vcp")) / (!!sym("sum_p_vcp") /
        !!sym("sum_c_p_vcp"))
    ) %>%
    dplyr::select(-dplyr::matches("vcp")) %>%

    # Rename columns
    dplyr::rename(c = !!sym(c), p = !!sym(p))

  if (discrete == TRUE) {
    data <- data %>%
      dplyr::mutate(!!sym("v") := dplyr::if_else(!!sym("v") >
        cutoff, 1, 0))
  }

  if (tbl == FALSE) {
    data <- data %>%
      tidyr::spread(!!sym("p"), !!sym("v"))

    data_rownames <- dplyr::select(data, !!sym("c")) %>%
      dplyr::pull()

    data <- dplyr::select(data, -!!sym("c")) %>% as.matrix()
    data[is.na(data)] <- 0
    rownames(data) <- data_rownames
    data <- Matrix::Matrix(data, sparse = TRUE)
  } else {
    data <- data %>%
      dplyr::rename(
        "country" = !!sym("c"),
        "product" = !!sym("p"),
        "value" = !!sym("v")
      )
  }

  return(data)
}
