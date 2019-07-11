#' Complexity Measures
#'
#' @export
#' @param revealed_comparative_advantage matrix or tibble/data.frame (e.g. the output of
#' \code{revealed_comparative_advantage()}).
#' If the input is a matrix it must be a zero/one matrix with countries in rows and products in columns.
#' If the input is a tibble/data.frame it must contain at least three columns with countries, products and
#' values.
#' @param country string to indicate the column that contains exporting countries in revealed_comparative_advantage
#' (set to "country" by default)
#' @param product string to indicate the column that contains exported products in revealed_comparative_advantage
#' (set to "product" by default)
#' @param value string to indicate the column that contains traded values in revealed_comparative_advantage
#' (set to "value" by default)
#' @param method string to indicate to use one of these methods: reflections, eigenvalues or fitness
#' (set to "fitness" by default)
#' @param keep_atlas logical value to remove the countries not ranked in The Atlas of Economic Complexity
#' (set to FALSE by default)
#' @param iterations number of iterations to use in the reflections method (set to 20 by default)
#' @param extremality numeric coefficient to use in the fitness method (set to 1 by default)
#' @param tbl_output logical value to use tibble output instead of a matrix output (set to FALSE by default)
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate arrange pull
#' @importFrom tidyr gather spread
#' @importFrom tibble tibble as_tibble enframe
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym
#' @examples
#' complexity_measures(
#'  revealed_comparative_advantage =
#'   package_output_demo$revealed_comparative_advantage_matrix,
#'  tbl_output = TRUE
#' )
#' @references
#' For more information on complexity measures, indices and its applications see:
#'
#' \insertRef{atlas2014}{economiccomplexity}
#'
#' \insertRef{measuringcomplexity2015}{economiccomplexity}
#' @keywords functions

complexity_measures <- function(revealed_comparative_advantage = NULL,
                                country = "country",
                                product = "product",
                                value = "value",
                                method = "fitness",
                                iterations = 20,
                                extremality = 1,
                                keep_atlas = FALSE,
                                tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(revealed_comparative_advantage) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix", "dgCMatrix") == FALSE)) {
    stop("revealed_comparative_advantage must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!(any(method %in% c("reflections", "eigenvalues", "fitness")) == TRUE)) {
    stop("method must be reflections, eigenvalues or fitness")
  }

  if (is.integer(iterations) & !iterations >= 2) {
    stop("iterations must be integer and greater or equal to 2")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  # convert data.frame input to matrix ----
  if (is.data.frame(revealed_comparative_advantage)) {
    m <- tidyr::spread(revealed_comparative_advantage, !!sym(product), !!sym(value))
    m_rownames <- dplyr::select(m, !!sym(country)) %>% dplyr::pull()

    m <- dplyr::select(m, -!!sym(country)) %>% as.matrix()
    m[is.na(m)] <- 0
    rownames(m) <- m_rownames

    m <- Matrix::Matrix(m, sparse = TRUE)
    m <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]
  } else {
    m <- revealed_comparative_advantage[
      Matrix::rowSums(revealed_comparative_advantage) != 0,
      Matrix::colSums(revealed_comparative_advantage) != 0
    ]
  }

  # remove countries not ranked in The Atlas of Economic Complexity
  # this list comes from
  # https://github.com/tradestatistics/atlas-data/blob/master/2-scraped-tables/ranking-1-economic-complexity-index.csv
  # the country code for Romania in Comtrade is ROM, not ROU
  if (keep_atlas == TRUE) {
    atlas_countries <- c(
      "jpn", "deu", "che", "swe", "aut", "fin", "sgp", "cze", "gbr", "svn",
      "fra", "kor", "usa", "hun", "svk", "ita", "dnk", "irl", "isr", "mex", "blr", "bel", "nld",
      "hkg", "pol", "hrv", "rom", "esp", "chn", "pan", "tha", "est", "nor", "mys", "prt", "ltu",
      "srb", "bih", "lva", "bgr", "can", "ukr", "tur", "lbn", "jor", "rus", "tun", "nzl", "cri",
      "mda", "ind", "bra", "grc", "col", "zaf", "ury", "arg", "alb", "phl", "slv", "idn", "mkd",
      "egy", "dom", "gtm", "are", "vnm", "sau", "kgz", "geo", "lka", "nam", "ken", "sen", "syr",
      "tto", "mus", "chl", "aus", "zwe", "jam", "pak", "mar", "cub", "qat", "pry", "uga", "hnd",
      "per", "mdg", "omn", "kaz", "ecu", "bwa", "tza", "uzb", "nic", "khm", "civ", "gha", "bol",
      "lao", "bgd", "eth", "zmb", "mwi", "yem", "tjk", "moz", "mli", "ven", "lbr", "mng", "dza",
      "tkm", "kwt", "aze", "irn", "lby", "gab", "cmr", "nga", "gin", "png", "cog", "sdn", "ago",
      "mrt"
    )

    m <- m[rownames(m) %in% atlas_countries, ]
  }

  # compute complexity measures ----
  # diversity (kc0) and ubiquity (kp0), following the Atlas notation
  kc0 <- Matrix::rowSums(m)
  kp0 <- Matrix::colSums(m)

  # reflections is defined as a function as these steps are also used for eigenvalues method
  reflections <- function() {
    # create empty matrices
    kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = iterations, sparse = TRUE)
    kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = iterations, sparse = TRUE)

    # fill the first columns with kc0 and kp0 to start iterating
    kc[, 1] <- kc0
    kp[, 1] <- kp0

    # compute cols 2 to "no. of iterations" by iterating from col 1
    for (j in 2:ncol(kc)) {
      kc[, j] <- (m %*% kp[, (j - 1)]) * (1 / kc0)
      kp[, j] <- (Matrix::t(m) %*% kc[, (j - 1)]) * (1 / kp0)
    }

    # eci is of odd order and normalized as in the Atlas
    eci <- (kc[, iterations - 1] - base::mean(kc[, iterations - 1])) /
      stats::sd(kc[, iterations - 1])

    # pci is of even order and normalized as in the Atlas
    pci <- (kp[, iterations] - base::mean(kp[, iterations])) /
      stats::sd(kp[, iterations])

    names(eci) <- rownames(m)
    names(pci) <- colnames(m)

    return(list(eci = eci, pci = pci))
  }

  if (method == "reflections") {
    reflections_output <- reflections()
    eci <- reflections_output$eci
    pci <- reflections_output$pci
  }

  if (method == "eigenvalues") {
    # to check if a sign correction is needed
    reflections_output <- reflections()
    eci_r <- reflections_output$eci
    pci_r <- reflections_output$pci

    # compute eigenvalues for eci
    eci <- eigen((m %*% (Matrix::t(m) * (1 / kp0))) * (1 / kc0))
    eci <- Re(eci$vectors[, 2])

    # eci normalized as in the Atlas
    eci <- (eci - base::mean(eci)) / stats::sd(eci)
    names(eci) <- rownames(m)

    # correct eci sign when required
    if (isTRUE(stats::cor(eci, eci_r, use = "pairwise.complete.obs") < 0)) {
      eci <- -1 * eci
    }

    # compute eigenvalues for pci
    pci <- eigen((Matrix::t(m) %*% (m * (1 / kc0))) * (1 / kp0))
    pci <- Re(pci$vectors[, 2])

    # pci normalized as in the Atlas
    pci <- (pci - base::mean(pci)) / stats::sd(pci)
    names(pci) <- colnames(m)

    # correct pci sign when required
    if (isTRUE(stats::cor(pci, pci_r, use = "pairwise.complete.obs") < 0)) {
      pci <- -1 * pci
    }
  }

  if (method == "fitness") {
    # create empty matrices
    kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = iterations, sparse = TRUE)
    kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = iterations, sparse = TRUE)

    # fill the first columns with kc0 and kp0 to start iterating
    kc[, 1] <- 1
    kp[, 1] <- 1

    # compute cols 2 to "no. of iterations" by iterating from col 1
    for (j in 2:ncol(kc)) {
      kc[, j] <- m %*% kp[, (j - 1)]
      kc[, j] <- kc[, j] / mean(kc[, j])

      kp[, j] <- 1 / (Matrix::t(m) %*% (1 / kc[, (j - 1)])^extremality)^(1 / extremality)
      kp[, j] <- kp[, j] / mean(kp[, j])
    }

    eci <- kc[, iterations]
    pci <- kp[, iterations]

    names(eci) <- rownames(m)
    names(pci) <- colnames(m)
  }

  if (tbl_output == TRUE) {
    eci <- tibble::tibble(value = eci) %>%
      dplyr::mutate(country = names(eci)) %>%
      dplyr::select(!!sym("country"), !!sym("value")) %>%
      dplyr::arrange(-!!sym("value"))

    pci <- tibble::tibble(value = pci) %>%
      dplyr::mutate(product = names(pci)) %>%
      dplyr::select(!!sym("product"), !!sym("value")) %>%
      dplyr::arrange(-!!sym("value"))

    kc0 <- tibble::enframe(kc0) %>%
      dplyr::rename(country = !!sym("name"))

    kp0 <- tibble::enframe(kp0) %>%
      dplyr::rename(product = !!sym("name"))

    m <- as.matrix(m) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(country = rownames(m)) %>%
      tidyr::gather(!!sym("product"), !!sym("value"), -!!sym("country"))
  }

  return(
    list(
      economic_complexity_index = eci,
      product_complexity_index = pci,
      diversity = kc0,
      ubiquity = kp0
    )
  )
}
