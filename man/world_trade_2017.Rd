\name{world_trade_2017}
\alias{world_trade_2017}
\docType{data}
\title{
Dataset with world trade in 2017.
}
\description{
This dataset is a simplified version of the country-product dataset from Open Trade Statistics (tradestatistics.io) for the year 2017. The simplification consisted in keeping just three columns to provide a minimal dataset for example purposes.
}
\usage{data("world_trade_2017")}
\format{
  A data frame with 165977 observations on the following 3 variables.
  \describe{
    \item{\code{reporter_iso}}{a character vector}
    \item{\code{product_code}}{a character vector}
    \item{\code{export_value_usd}}{a numeric vector}
  }
}
\source{
Open Trade Statistics (obtained by using the tradestatistics package)
}
\examples{
world_trade_2017
}
\keyword{datasets}
