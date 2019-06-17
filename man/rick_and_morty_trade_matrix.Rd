\name{rick_and_morty_trade_matrix}
\alias{rick_and_morty_trade_matrix}
\docType{data}
\title{
Example dataset with fabricated numbers using Rick and Morty references (matrix version)
}
\description{
This fictional datasets includes planets from Rick and Morty as "countries" that
export different products. The currency used here is "brapples".
Please consider this just as a lightweight dataset for examples and tests.
}
\usage{data("rick_and_morty_trade_matrix")}
\format{
  The format is:
Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
  ..@ i       : int [1:54] 3 5 0 3 5 6 7 8 0 1 ...
  ..@ p       : int [1:13] 0 2 8 16 20 26 29 31 36 41 ...
  ..@ Dim     : int [1:2] 9 12
  ..@ Dimnames:List of 2
  .. ..$ : chr [1:9] "Alphabetrium" "Bird World" "Earth C-137" "Gazorpazorp" ...
  .. ..$ : chr [1:12] "Crystallized Xanthenite" "Freeze ray" "Ionic Defibulizer" "Kalaxian Crystals" ...
  ..@ x       : num [1:54] 350 2300 1000 1300 300 5000 230 200 1000 5000 ...
  ..@ factors : list()
}
\examples{
rick_and_morty_trade_matrix
}
\keyword{datasets}
