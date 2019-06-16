\name{proximity_matrices_output}
\alias{proximity_matrices_output}
\docType{data}
\title{
Demo dataset to show the output of proximity matrices().
}
\description{
TBD
}
\usage{data("proximity_matrices_output")}
\format{
  The format is:
List of 2
 $ countries_proximity:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	23481 obs. of  3 variables:
  ..$ from : chr [1:23481] "ago" "aia" "alb" "and" ...
  ..$ to   : chr [1:23481] "afg" "afg" "afg" "afg" ...
  ..$ value: num [1:23481] 0.0147 0.0137 0.1357 0.0706 0.0642 ...
 $ products_proximity :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	703017 obs. of  3 variables:
  ..$ from : chr [1:703017] "0102" "0103" "0104" "0105" ...
  ..$ to   : chr [1:703017] "0101" "0101" "0101" "0101" ...
  ..$ value: num [1:703017] 0.19 0.25 0.04 0.235 0.109 ...
}
\examples{
proximity_matrices_output
}
\keyword{datasets}
