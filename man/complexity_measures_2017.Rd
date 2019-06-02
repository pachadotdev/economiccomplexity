\name{complexity_measures_2017}
\alias{complexity_measures_2017}
\docType{data}
\title{
Demo dataset to show the output of economic_complexity_measures().
}
\description{
TBD
}
\usage{data("complexity_measures_2017")}
\format{
  The format is:
List of 4
 $ economic_complexity_index:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	224 obs. of  2 variables:
  ..$ country: chr [1:224] "jpn" "che" "deu" "bvt" ...
  ..$ value  : num [1:224] 2.27 2.2 1.97 1.91 1.82 ...
 $ product_complexity_index :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1222 obs. of  2 variables:
  ..$ product: chr [1:1222] "2812" "8457" "3705" "9012" ...
  ..$ value  : num [1:1222] 2.68 2.25 2.2 2.19 2.17 ...
 $ diversity                :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	224 obs. of  2 variables:
  ..$ country: chr [1:224] "afg" "ago" "aia" "alb" ...
  ..$ value  : num [1:224] 68 10 73 140 85 109 183 92 71 20 ...
 $ ubiquity                 :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	1222 obs. of  2 variables:
  ..$ product: chr [1:1222] "0101" "0102" "0103" "0104" ...
  ..$ value  : num [1:1222] 20 42 16 25 34 55 28 23 15 27 ...
}
\examples{
data(complexity_measures_2017)
}
\keyword{datasets}
