#' @title Complexity measures
#'
#' @description \code{rca} computes complexity indices following the definitions
#' from \insertCite{measuringcomplexity2015;textual}{economiccomplexity}
#'
#' @details Given a \eqn{C\times P} matrix (C for "countries"
#' and P for "products") or an equivalent 3-columns data frame with RCA
#' values as input, this function implements the equations:
#' \deqn{\text{(Diversity)}\: k_{c}^{(0)} = \sum_p R_{cp}}
#' \deqn{\text{(Ubiquity)}\: k_{p}^{(0)} = \sum_c R_{cp}}
#' \deqn{\text{(Fitness)}\: \tilde{F}^{(n)}(\gamma) = \sum_{p} R_{cp}Q_{p}^{n-1}}
#' \deqn{\text{(Complexity)}\: \tilde{Q}^{(n)}(\gamma) =
#' \left[\sum_{c} R_{cp}(F_{c}^{n-1})^{-\gamma}\right]^{-1/\gamma}.}
#'
#' Besides this implementation, the function offers the reflections method
#' and eigenvalues computation as alternatives to the Fitness-Complexity
#' formulation.
#'
#' @param rca matrix or data frame with RCA values
#' @param method which method to use (by default is "fitness", it can also
#' be "reflections" or "eigenvalues")
#' @param iterations number of iterations to use in the fitness, reflections
#' and indirectly in eigenvalues method (by default is 20)
#' @param extremality numeric coefficient to use in the fitness method
#' (by default is 1)
#' @param atlas remove the countries not ranked in The Atlas of Economic
#' Complexity (by default is FALSE)
#' @param tbl TRUE (default) returns a data.frame and FALSE returns a matrix
#' @param country column containing countries (applies only if d is a
#' data.frame)
#' @param product column containing products (applies only if d is a
#' data.frame)
#' @param value column containing traded values (applies only if d is a
#' data.frame)
#'
#' @references
#' For more information about complexity measures see:
#'
#' \insertRef{measuringcomplexity2015}{economiccomplexity}
#'
#' and the references therein.
#'
#' @examples
#' complexity(ec_output_demo$rca)
#'
#' @return A list with four data frames or matrices.
#'
#' @seealso \code{\link[economiccomplexity]{rca}},
#'
#' @keywords functions
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate arrange pull rename
#' @importFrom tidyr gather spread
#' @importFrom tibble tibble as_tibble enframe
#' @importFrom Matrix Matrix rowSums colSums t
#' @importFrom rlang sym
#'
#' @export

complexity <- function(rca,
                       method = "fitness",
                       iterations = 20,
                       extremality = 1,
                       atlas = FALSE,
                       tbl = TRUE,
                       country = "country",
                       product = "product",
                       value = "value") {
  # sanity checks ----
  if (all(class(rca) %in% c("data.frame", "matrix", "dgeMatrix", "dsCMatrix",
    "dgCMatrix") == FALSE)) {
    stop("rca must be a data frame or matrix")
  }

  if (!is.character(country) & !is.character(product) & !is.character(value)) {
    stop("country, product and value must be character")
  }

  if (!(any(method %in% c("reflections", "eigenvalues", "fitness")) == TRUE)) {
    stop("method must be 'fitness', 'reflections' or 'eigenvalues'")
  }

  if (is.integer(iterations) & !iterations >= 2) {
    stop("iterations must be integer and greater or equal to 2")
  }

  if (!is.logical(tbl)) {
    stop("tbl must be TRUE or FALSE")
  }

  # convert data.frame input to matrix ----
  if (is.data.frame(rca)) {
    m <- tidyr::spread(rca, !!sym(product), !!sym(value))
    m_rownames <- dplyr::select(m, !!sym(country)) %>% dplyr::pull()

    m <- dplyr::select(m, -!!sym(country)) %>% as.matrix()
    m[is.na(m)] <- 0
    rownames(m) <- m_rownames

    m <- Matrix::Matrix(m, sparse = TRUE)
    m <- m[Matrix::rowSums(m) != 0, Matrix::colSums(m) != 0]
  } else {
    m <- rca[Matrix::rowSums(rca) != 0, Matrix::colSums(rca) != 0]
  }

  # remove countries not ranked in The Atlas of Economic Complexity
  # this list comes from
  # https://github.com/tradestatistics/atlas-data/blob/master/2-scraped-tables/
  # ranking-1-economic-complexity-index.csv
  # the c code for Romania in Comtrade is ROM, not ROU
  if (atlas == TRUE) {
    atlas_countries <- c(
      "jpn", "deu", "che", "swe", "aut", "fin", "sgp", "cze", "gbr", "svn",
      "fra", "kor", "usa", "hun", "svk", "ita", "dnk", "irl", "isr", "mex",
      "blr", "bel", "nld", "hkg", "pol", "hrv", "rom", "esp", "chn", "pan",
      "tha", "est", "nor", "mys", "prt", "ltu", "srb", "bih", "lva", "bgr",
      "can", "ukr", "tur", "lbn", "jor", "rus", "tun", "nzl", "cri", "mda",
      "ind", "bra", "grc", "col", "zaf", "ury", "arg", "alb", "phl", "slv",
      "idn", "mkd", "egy", "dom", "gtm", "are", "vnm", "sau", "kgz", "geo",
      "lka", "nam", "ken", "sen", "syr", "tto", "mus", "chl", "aus", "zwe",
      "jam", "pak", "mar", "cub", "qat", "pry", "uga", "hnd", "per", "mdg",
      "omn", "kaz", "ecu", "bwa", "tza", "uzb", "nic", "khm", "civ", "gha",
      "bol", "lao", "bgd", "eth", "zmb", "mwi", "yem", "tjk", "moz", "mli",
      "ven", "lbr", "mng", "dza", "tkm", "kwt", "aze", "irn", "lby", "gab",
      "cmr", "nga", "gin", "png", "cog", "sdn", "ago", "mrt"
    )

    m <- m[rownames(m) %in% atlas_countries, ]
  }

  # compute complexity measures ----
  # diversity (kc0) and ubiquity (kp0), following the Atlas notation
  kc0 <- Matrix::rowSums(m)
  kp0 <- Matrix::colSums(m)

  # reflections is defined as a function as these steps are also used for
  # eigenvalues method
  reflections <- function() {
    # create empty matrices
    kc <- Matrix::Matrix(0,
      nrow = length(kc0), ncol = iterations,
      sparse = TRUE
    )

    kp <- Matrix::Matrix(0,
      nrow = length(kp0), ncol = iterations,
      sparse = TRUE
    )

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
    kc <- Matrix::Matrix(0,
      nrow = length(kc0), ncol = iterations,
      sparse = TRUE
    )

    kp <- Matrix::Matrix(0,
      nrow = length(kp0), ncol = iterations,
      sparse = TRUE
    )

    # fill the first columns with kc0 and kp0 to start iterating
    kc[, 1] <- 1
    kp[, 1] <- 1

    # compute cols 2 to "no. of iterations" by iterating from col 1
    for (j in 2:ncol(kc)) {
      kc[, j] <- m %*% kp[, (j - 1)]

      kc[, j] <- kc[, j] / mean(kc[, j])

      kp[, j] <- 1 / (Matrix::t(m) %*%
        (1 / kc[, (j - 1)])^extremality)^(1 / extremality)

      kp[, j] <- kp[, j] / mean(kp[, j])
    }

    eci <- kc[, iterations]

    pci <- kp[, iterations]

    names(eci) <- rownames(m)

    names(pci) <- colnames(m)
  }

  if (tbl == TRUE) {
    eci <- tibble::tibble(value = eci) %>%
      dplyr::mutate(country = names(eci)) %>%
      dplyr::select(!!sym("country"), !!sym("value")) %>%
      dplyr::arrange(-!!sym("value"))

    pci <- tibble::tibble(value = pci) %>%
      dplyr::mutate(product = names(pci)) %>%
      dplyr::select(!!sym("product"), !!sym("value")) %>%
      dplyr::arrange(-!!sym("value"))

    kc0 <- tibble::enframe(kc0) %>%
      dplyr::rename("country" = !!sym("name"))

    kp0 <- tibble::enframe(kp0) %>%
      dplyr::rename("product" = !!sym("name"))
  }

  return(
    list(
      complexity_c = eci,
      complexity_p = pci,
      diversity = kc0,
      ubiquity = kp0
    )
  )
}
