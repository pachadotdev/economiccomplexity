\name{world_gdp_and_population_2017}
\alias{world_gdp_and_population_2017}
\docType{data}
\title{
Dataset with world gdp and population in 2017 (or last reported values).
}
\description{
tbd
}
\usage{data("world_gdp_and_population_2017")}
\format{
  A data frame with 262 observations on the following 4 variables.
  \describe{
    \item{\code{reporter_iso}}{a character vector}
    \item{\code{gdp_current_usd}}{a numeric vector}
    \item{\code{population_total}}{a numeric vector}
    \item{\code{gdp_pc_usd}}{a numeric vector}
  }
}
\source{
The World Bank (obtained by using wbstats package)
}
\examples{
world_gdp_and_population_2017
}
\keyword{datasets}
