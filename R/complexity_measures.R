#' Complexity Measures
#'
#' @export
#' @param d matrix or tibble/data.frame in long format, if d is a tibble/data.frame it must contain the columns
#' country (character/factor), product (character/factor) and discrete RCA (integer), if it is a matrix it must be a
#' zero/one matrix with countries in the row names and products in the column names
#' @param c string to indicate the column that contains exporting countries (default set to "country" that is the
#' output of revealed_comparative_advantage())
#' @param p string to indicate the column that contains exported products (default set to "product" that is the
#' output of revealed_comparative_advantage())
#' @param v string to indicate the column that contains RCA values (default set to "value" that is the
#' output of revealed_comparative_advantage())
#' @param method reflections, eigenvalues or fitness (default set to reflections)
#' methods (default set to FALSE)
#' @param keep_atlas removes the countries not ranked in The Atlas of Economic Complexity (default set to FALSE)
#' @param iterations number of iterations to use in the reflections method (default set to 20)
#' @param extremality coefficient to use in the fitness method (default set to 1)
#' @param tbl_output when set to TRUE the output will be a tibble instead of a matrix (default set to FALSE)
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate arrange pull
#' @importFrom tidyr gather spread
#' @importFrom tibble tibble as_tibble enframe
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym
#' @examples
#' complexity_measures <- complexity_measures(world_rca_2017)
#' @keywords functions

complexity_measures <- function(d = NULL, c = "country", p = "product", v = "value",
                                         method = "fitness", iterations = 20, extremality = 1,
                                         keep_atlas = FALSE, tbl_output = FALSE) {
  # sanity checks ----
  if (all(class(d) %in% c("data.frame", "matrix", "dgeMatrix", "dgCMatrix") == FALSE)) {
    stop("d must be a tibble/data.frame or a dense/sparse matrix")
  }

  if (!(any(method %in% c("reflections","eigenvalues","fitness")) == TRUE)) {
    stop("method must be reflections, eigenvalues or fitness")
  }

  if (is.integer(iterations) & !iterations >= 2) {
    stop("iterations must be integer and greater or equal to 2")
  }

  if (!is.logical(tbl_output)) {
    stop("tbl_output must be logical")
  }

  # convert data.frame input to matrix ----
  if (is.data.frame(d)) {
    m <- tidyr::spread(d, !!sym(p), !!sym(v))
    m_rownames <- dplyr::select(m, !!sym(c)) %>% dplyr::pull()

    m <- dplyr::select(m, -!!sym(c)) %>% as.matrix()
    m[is.na(m)] <- 0
    rownames(m) <- m_rownames

    m <- Matrix::Matrix(m, sparse = T)
    m <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]
  } else {
    m <- d[Matrix::rowSums(d) != 0, Matrix::colSums(d) != 0]
  }

  # remove countries not ranked in The Atlas of Economic Complexity
  # this list comes from
  # https://github.com/tradestatistics/atlas-data/blob/master/2-scraped-tables/ranking-1-economic-complexity-index.csv
  if (keep_atlas == TRUE) {
    atlas_countries <- c('jpn', 'deu', 'che', 'swe', 'aut', 'fin', 'sgp', 'cze', 'gbr', 'svn',
      'fra', 'kor', 'usa', 'hun', 'svk', 'ita', 'dnk', 'irl', 'isr', 'mex', 'blr', 'bel', 'nld',
      'hkg', 'pol', 'hrv', 'rou', 'esp', 'chn', 'pan', 'tha', 'est', 'nor', 'mys', 'prt', 'ltu',
      'srb', 'bih', 'lva', 'bgr', 'can', 'ukr', 'tur', 'lbn', 'jor', 'rus', 'tun', 'nzl', 'cri',
      'mda', 'ind', 'bra', 'grc', 'col', 'zaf', 'ury', 'arg', 'alb', 'phl', 'slv', 'idn', 'mkd',
      'egy', 'dom', 'gtm', 'are', 'vnm', 'sau', 'kgz', 'geo', 'lka', 'nam', 'ken', 'sen', 'syr',
      'tto', 'mus', 'chl', 'aus', 'zwe', 'jam', 'pak', 'mar', 'cub', 'qat', 'pry', 'uga', 'hnd',
      'per', 'mdg', 'omn', 'kaz', 'ecu', 'bwa', 'tza', 'uzb', 'nic', 'khm', 'civ', 'gha', 'bol',
      'lao', 'bgd', 'eth', 'zmb', 'mwi', 'yem', 'tjk', 'moz', 'mli', 'ven', 'lbr', 'mng', 'dza',
      'tkm', 'kwt', 'aze', 'irn', 'lby', 'gab', 'cmr', 'nga', 'gin', 'png', 'cog', 'sdn', 'ago',
      'mrt'
    )

    m <- m[rownames(m) %in% atlas_countries, ]
  }

  # compute complexity measures ----
  # diversity (kc0) and ubiquity (kp0), following the Atlas notation
  kc0 <- Matrix::rowSums(m)
  kp0 <- Matrix::colSums(m)

  if (method == "reflections") {
    # create empty matrices
    kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = iterations, sparse = T)
    kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = iterations, sparse = T)

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
  }

  if (method == "eigenvalues") {
    eci <- eigen((m %*% (Matrix::t(m) * (1 / kp0))) * (1 / kc0))
    eci <- Re(eci$vectors[, 2])

    # eci normalized as in the Atlas
    eci <- (eci - base::mean(eci)) / stats::sd(eci)

    pci <- eigen((Matrix::t(m) %*% (m * (1 / kc0))) * (1 / kp0))
    pci <- Re(pci$vectors[, 2])

    # pci normalized as in the Atlas
    pci <- (pci - base::mean(pci)) /  stats::sd(pci)
  }

  if (method == "fitness") {
    # create empty matrices
    kc <- Matrix::Matrix(0, nrow = length(kc0), ncol = iterations, sparse = T)
    kp <- Matrix::Matrix(0, nrow = length(kp0), ncol = iterations, sparse = T)

    # fill the first columns with kc0 and kp0 to start iterating
    kc[, 1] <- 1
    kp[, 1] <- 1

    # compute cols 2 to "no. of iterations" by iterating from col 1
    for (j in 2:ncol(kc)) {
      kc[, j] <- m %*% kp[, (j - 1)]
      kc[, j] <- kc[, j] / mean(kc[, j])

      kp[, j] <- 1 / (Matrix::t(m) %*% (1 /kc[, (j - 1)])^extremality)^(1 / extremality)
      kp[, j] <- kp[, j] / mean(kp[, j])
    }

    # eci is of odd order and normalized as in the Atlas
    eci <- (kc[, iterations - 1] - base::mean(kc[, iterations - 1])) /
      stats::sd(kc[, iterations - 1])

    # pci is of even order and normalized as in the Atlas
    pci <- (kp[, iterations] - base::mean(kp[, iterations])) /
      stats::sd(kp[, iterations])
  }

  names(eci) <- rownames(m)
  names(pci) <- colnames(m)

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
