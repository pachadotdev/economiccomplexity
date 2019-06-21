\name{services_trade_2016}
\alias{services_trade_2016}
\docType{data}
\title{
Exported services in 2016 (matrix and tibble version)
}
\description{
Example dataset with countries and the services they export. The exports are
grouped to the most aggregated categories of EBOPS2002 service codes and product data
is not included to keep the example lightweight.
}
\usage{services_trade_2016}
\format{
  The format is:
List of 2
 $ services_trade_2016_matrix:Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:736] 0 1 2 3 4 5 6 7 8 9 ...
  .. ..@ p       : int [1:12] 0 80 145 218 298 355 409 475 540 601 ...
  .. ..@ Dim     : int [1:2] 80 11
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:80] "Albania" "Algeria" "Angola" "Argentina" ...
  .. .. ..$ : chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  .. ..@ x       : num [1:736] 2.00e+08 6.72e+08 3.26e+07 2.18e+09 1.88e+08 ...
  .. ..@ factors : list()
 $ services_trade_2016_tibble:Classes ‘spec_tbl_df’, ‘tbl_df’, ‘tbl’ and 'data.frame':	736 obs. of  3 variables:
  ..$ country: chr [1:736] "Albania" "Algeria" "Angola" "Azerbaijan" ...
  ..$ product: chr [1:736] "1 Transportation" "1 Transportation" "1 Transportation" "1 Transportation" ...
  ..$ value  : num [1:736] 2.00e+08 6.72e+08 3.26e+07 1.10e+09 2.18e+09 ...
  ..- attr(*, "spec")=List of 3
  .. ..$ cols   :List of 24
  .. .. ..$ Classification        : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
  .. .. ..$ Year                  : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Period                : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Period Desc.          : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Aggregate Level       : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Is Leaf Code          : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Trade Flow Code       : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Trade Flow            : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
  .. .. ..$ Reporter Code         : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Reporter              : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
  .. .. ..$ Reporter ISO          : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ Partner Code          : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Partner               : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
  .. .. ..$ Partner ISO           : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ Mode of Transport Code: list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ Mode of Transport     : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ Commodity Code        : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ Commodity             : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_character" "collector"
  .. .. ..$ Netweight (kg)        : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ Gross weight (kg)     : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ Trade Value (US$)     : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. .. ..$ CIF Trade Value (US$) : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ FOB Trade Value (US$) : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_logical" "collector"
  .. .. ..$ Flag                  : list()
  .. .. .. ..- attr(*, "class")= chr [1:2] "collector_double" "collector"
  .. ..$ default: list()
  .. .. ..- attr(*, "class")= chr [1:2] "collector_guess" "collector"
  .. ..$ skip   : num 1
  .. ..- attr(*, "class")= chr "col_spec"
}
\source{
United Nations COMTRADE
}
\examples{
services_trade_2016$services_trade_2016_matrix
services_trade_2016$services_trade_2016_tibble
}
\keyword{datasets}
