\name{proximity_output}
\alias{proximity_output}
\docType{data}
\title{
Demo dataset to show the output of proximity matrices().
}
\description{
TBD
}
\usage{data("proximity_output")}
\format{
  The format is:
List of 2
 $ countries_proximity:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	19 obs. of  3 variables:
  ..$ from : chr [1:19] "Earth C-137" "Gazorpazorp" "Gromflom Prime" "Planet Squanch" ...
  ..$ to   : chr [1:19] "Alphabetrium" "Alphabetrium" "Alphabetrium" "Alphabetrium" ...
  ..$ value: num [1:19] 0.25 0.25 0.5 0.25 0.25 0.2 0.5 0.25 0.2 0.25 ...
 $ products_proximity :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	33 obs. of  3 variables:
  ..$ from : chr [1:33] "Ionic Defibulizer" "Plumbus" "Series 9000 Brainalyzer" "Szechuan Chicken McNugget Sauce" ...
  ..$ to   : chr [1:33] "Crystallized Xanthenite" "Crystallized Xanthenite" "Crystallized Xanthenite" "Crystallized Xanthenite" ...
  ..$ value: num [1:33] 0.5 0.5 0.333 0.25 0.5 ...
}
\examples{
proximity_output
}
\keyword{datasets}
