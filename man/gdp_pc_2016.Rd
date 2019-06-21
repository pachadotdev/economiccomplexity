\name{gdp_pc_2016}
\alias{gdp_pc_2016}
\docType{data}
\title{
GDP per capita in 2016 (numerica and tibble version)
}
\description{
Example dataset with countries and their gross domestic product divided by total population.
}
\usage{gdp_pc_2016}
\format{
  The format is:
List of 2
 $ gdp_pc_2016_numeric: Named num [1:80] 6183 4132 3943 3510 12654 ...
  ..- attr(*, "names")= chr [1:80] "Aruba" "Albania" "Algeria" "Angola" ...
 $ gdp_pc_2016_tibble :'data.frame':	80 obs. of  2 variables:
  ..$ country: chr [1:80] "Aruba" "Albania" "Algeria" "Angola" ...
  ..$ value  : num [1:80] 6183 4132 3943 3510 12654 ...
}
\source{
The World Bank
}
\examples{
gdp_pc_2016$gdp_pc_2016_numeric
gdp_pc_2016$gdp_pc_2016_tibble
}
\keyword{datasets}
