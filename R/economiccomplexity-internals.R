#' country-product aggregation
#' @importFrom stats aggregate
#' @keywords internal
country_product_aggregation <- function(dataframe, country = "country", product = "product", value = "value") {
  dataframe <- subset(dataframe, select = c(country, product, value))
  names(dataframe) <- c("country", "product", "value")

  dataframe <- aggregate(dataframe$value, by = list(country = dataframe$country, product = dataframe$product), FUN = sum)
  names(dataframe) <- c("country", "product", "value")

  dataframe <- dataframe[dataframe$value > 0, ]

  dataframe$country <- as.factor(dataframe$country)
  dataframe$product <- as.factor(dataframe$product)

  return(dataframe)
}

#' country aggregation
#' @importFrom stats aggregate
#' @keywords internal
country_aggregation <- function(dataframe, country = "country", value = "value") {
  dataframe <- subset(dataframe, select = c(country, value))
  names(dataframe) <- c("country", "value")

  dataframe <- aggregate(dataframe$value, by = list(country = dataframe$country), FUN = sum)
  names(dataframe) <- c("country", "value")

  dataframe <- dataframe[dataframe$value > 0, ]

  dataframe$country <- as.factor(dataframe$country)

  return(dataframe)
}

#' Dataframe to matrix
#' @importFrom Matrix sparseMatrix
#' @keywords internal
dataframe_to_matrix <- function(dataframe, country = "country", product = "product", value = "value") {
  return(
    with(
      dataframe,
      sparseMatrix(
        i = as.numeric(country),
        j = as.numeric(product),
        x = value,
        dimnames = list(levels(country), levels(product))
      )
    )
  )
}
