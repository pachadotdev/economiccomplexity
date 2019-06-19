\name{services_trade_2016_matrix}
\alias{services_trade_2016_matrix}
\docType{data}
\title{
Exported services in 2016 (matrix version)
}
\description{
Example dataset with countries and the services they export. The exports are
grouped to the most aggregated categories of EBOPS2002 service codes and product data
is not included to keep the example lightweight.
}
\usage{data("services_trade_2016_matrix")}
\format{
  The format is:
Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
  ..@ i       : int [1:736] 0 1 2 3 4 5 6 7 8 9 ...
  ..@ p       : int [1:12] 0 80 145 218 298 355 409 475 540 601 ...
  ..@ Dim     : int [1:2] 80 11
  ..@ Dimnames:List of 2
  .. ..$ : chr [1:80] "Albania" "Algeria" "Angola" "Argentina" ...
  .. ..$ : chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  ..@ x       : num [1:736] 2.00e+08 6.72e+08 3.26e+07 2.18e+09 1.88e+08 ...
  ..@ factors : list()
}
\source{
United Nations COMTRADE
}
\examples{
services_trade_2016_matrix
}
\keyword{datasets}
