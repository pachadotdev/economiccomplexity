\name{services_trade_2016_tibble}
\alias{services_trade_2016_tibble}
\docType{data}
\title{
Exported services in 2016 (tibble version)
}
\description{
Example dataset with countries and the services they export. The exports are
grouped to the most aggregated categories of EBOPS2002 service codes and product data
is not included to keep the example lightweight.
}
\usage{data("services_trade_2016_tibble")}
\format{
  A data frame with 736 observations on the following 3 variables.
  \describe{
    \item{\code{country}}{a character vector}
    \item{\code{product}}{a character vector}
    \item{\code{value}}{a numeric vector}
  }
}
\source{
United Nations COMTRADE
}
\examples{
services_trade_2016_tibble
}
\keyword{datasets}
