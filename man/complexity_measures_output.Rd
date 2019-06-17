\name{complexity_measures_output}
\alias{complexity_measures_output}
\docType{data}
\title{
Example of \code{complexity_measures()} output.
}
\description{
This list is an example of the output of \code{complexity_measures()}.
}
\usage{complexity_measures_output}
\format{
  The format is:
List of 4
 $ economic_complexity_index:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	9 obs. of  2 variables:
  ..$ country: chr [1:9] "Pawn Shop Planet" "Gromflom Prime" "Alphabetrium" "Pluto" ...
  ..$ value  : num [1:9] 2.2778 0.4702 0.45 0.0433 -0.2415 ...
 $ product_complexity_index :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	12 obs. of  2 variables:
  ..$ product: chr [1:12] "Crystallized Xanthenite" "Kalaxian Crystals" "Plumbus" "Series 9000 Brainalyzer" ...
  ..$ value  : num [1:12] 2.848 0.807 0.377 -0.158 -0.246 ...
 $ diversity                :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	9 obs. of  2 variables:
  ..$ country: chr [1:9] "Alphabetrium" "Bird World" "Earth C-137" "Gazorpazorp" ...
  ..$ value  : num [1:9] 4 1 3 4 4 5 1 4 2
 $ ubiquity                 :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	12 obs. of  2 variables:
  ..$ product: chr [1:12] "Crystallized Xanthenite" "Freeze ray" "Ionic Defibulizer" "Kalaxian Crystals" ...
  ..$ value  : num [1:12] 1 2 2 1 3 2 2 3 4 2 ...
}
\examples{
  complexity_measures_output$economic_complexity_index
  complexity_measures_output$product_complexity_index
  complexity_measures_output$diversity
  complexity_measures_output$ubiquity
}
\keyword{datasets}
