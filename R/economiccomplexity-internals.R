#' Source-target aggregation
#' @importFrom stats aggregate
#' @keywords internal
source_target_aggregation <- function(dataframe, source = "source", target = "target", value = "value") {
  dataframe <- subset(dataframe, select = c(source, target, value))
  names(dataframe) <- c("source", "target", "value")

  dataframe <- aggregate(dataframe$value, by = list(source = dataframe$source, target = dataframe$target), FUN = sum)
  names(dataframe) <- c("source", "target", "value")

  dataframe <- dataframe[dataframe$value > 0, ]

  dataframe$source <- as.factor(dataframe$source)
  dataframe$target <- as.factor(dataframe$target)

  return(dataframe)
}

#' Dataframe to matrix
#' @importFrom Matrix sparseMatrix
#' @keywords internal
dataframe_to_matrix <- function(dataframe, source = "source", target = "target", value = "value") {
  return(
    with(
      dataframe,
      sparseMatrix(
        i = as.numeric(source),
        j = as.numeric(target),
        x = value,
        dimnames = list(levels(source), levels(target))
      )
    )
  )
}
