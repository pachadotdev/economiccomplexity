\name{proximity_matrices_2017}
\alias{proximity_matrices_2017}
\docType{data}
\title{
Demo dataset to show the output of proximity matrices().
}
\description{
TBD
}
\usage{data("proximity_matrices_2017")}
\format{
  The format is:
List of 2
 $ countries_proximity:Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:47186] 0 1 2 3 4 5 6 7 8 9 ...
  .. ..@ p       : int [1:225] 0 220 394 606 825 1041 1260 1482 1700 1918 ...
  .. ..@ Dim     : int [1:2] 224 224
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:224] "afg" "ago" "aia" "alb" ...
  .. .. ..$ : chr [1:224] "afg" "ago" "aia" "alb" ...
  .. ..@ x       : num [1:47186] 1 0.0147 0.0137 0.1357 0.0706 ...
  .. ..@ factors : list()
 $ products_proximity :Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:1407256] 0 1 2 3 4 5 6 7 8 9 ...
  .. ..@ p       : int [1:1223] 0 1150 2350 3506 4601 5807 7014 8198 9326 10484 ...
  .. ..@ Dim     : int [1:2] 1222 1222
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:1222] "0101" "0102" "0103" "0104" ...
  .. .. ..$ : chr [1:1222] "0101" "0102" "0103" "0104" ...
  .. ..@ x       : num [1:1407256] 1 0.19 0.25 0.04 0.235 ...
  .. ..@ factors : list()
}
\examples{
data(proximity_matrices_2017)
}
\keyword{datasets}
