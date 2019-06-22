\name{package_output_demo}
\alias{package_output_demo}
\docType{data}
\title{
Example outputs of all functions within the package
}
\description{
List of different economic complexity computations obtained by using the functions within the
package. The output is provided as matrix, data.frame and igraph objects, being all cases
provided with and without the option of converting to tibble from matrix or igraph.
}
\usage{package_output_demo}
\format{
  The format is:
List of 8
 $ revealed_comparative_advantage_matrix:Formal class 'dgCMatrix' [package "Matrix"] with 6 slots
  .. ..@ i       : int [1:285] 1 3 7 10 12 13 14 17 20 21 ...
  .. ..@ p       : int [1:12] 0 43 72 110 163 204 225 243 247 264 ...
  .. ..@ Dim     : int [1:2] 80 11
  .. ..@ Dimnames:List of 2
  .. .. ..$ : chr [1:80] "Albania" "Algeria" "Angola" "Argentina" ...
  .. .. ..$ : chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  .. ..@ x       : num [1:285] 1 1 1 1 1 1 1 1 1 1 ...
  .. ..@ factors : list()
 $ revealed_comparative_advantage_tibble:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	735 obs. of  3 variables:
  ..$ country: chr [1:735] "Albania" "Albania" "Albania" "Albania" ...
  ..$ product: chr [1:735] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  ..$ value  : num [1:735] 0 1 0 1 1 0 0 0 0 0 ...
 $ complexity_measures_numeric          :List of 4
  ..$ economic_complexity_index: Named num [1:80] -0.162 -0.162 -0.162 -0.162 -0.162 ...
  .. ..- attr(*, "names")= chr [1:80] "Albania" "Algeria" "Angola" "Argentina" ...
  ..$ product_complexity_index : Named num [1:11] -0.304 -0.304 -0.304 -0.304 -0.304 ...
  .. ..- attr(*, "names")= chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  ..$ diversity                : Named num [1:80] 3 5 1 5 6 1 3 2 1 4 ...
  .. ..- attr(*, "names")= chr [1:80] "Albania" "Algeria" "Angola" "Argentina" ...
  ..$ ubiquity                 : Named num [1:11] 43 29 38 53 41 21 18 4 17 2 ...
  .. ..- attr(*, "names")= chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
 $ complexity_measures_tibble           :List of 4
  ..$ economic_complexity_index:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	80 obs. of  2 variables:
  .. ..$ country: chr [1:80] "USA" "Japan" "Singapore" "Lebanon" ...
  .. ..$ value  : num [1:80] 6.2418 6.1685 -0.0886 -0.0886 -0.0886 ...
  ..$ product_complexity_index :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	11 obs. of  2 variables:
  .. ..$ product: chr [1:11] "8 Royalties and license fees" "6 Financial services" "7 Computer and information services" "9 Other business services" ...
  .. ..$ value  : num [1:11] 3.015 -0.279 -0.304 -0.304 -0.304 ...
  ..$ diversity                :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	80 obs. of  2 variables:
  .. ..$ country: chr [1:80] "Albania" "Algeria" "Angola" "Argentina" ...
  .. ..$ value  : num [1:80] 3 5 1 5 6 1 3 2 1 4 ...
  ..$ ubiquity                 :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	11 obs. of  2 variables:
  .. ..$ product: chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  .. ..$ value  : num [1:11] 43 29 38 53 41 21 18 4 17 2 ...
 $ proximity_matrix                     :List of 2
  ..$ proximity_countries:Formal class 'dsCMatrix' [package "Matrix"] with 7 slots
  .. .. ..@ i       : int [1:3285] 0 0 1 0 2 0 2 3 0 1 ...
  .. .. ..@ p       : int [1:87] 0 1 3 5 8 13 19 25 32 41 ...
  .. .. ..@ Dim     : int [1:2] 86 86
  .. .. ..@ Dimnames:List of 2
  .. .. .. ..$ : chr [1:86] "Albania" "Algeria" "Angola" "Anguilla" ...
  .. .. .. ..$ : chr [1:86] "Albania" "Algeria" "Angola" "Anguilla" ...
  .. .. ..@ x       : num [1:3285] 1 0.2 1 0.333 1 ...
  .. .. ..@ uplo    : chr "U"
  .. .. ..@ factors : list()
  ..$ proximity_products :Formal class 'dsCMatrix' [package "Matrix"] with 7 slots
  .. .. ..@ i       : int [1:61] 0 0 1 0 1 2 0 1 2 3 ...
  .. .. ..@ p       : int [1:12] 0 1 3 6 10 15 21 28 35 44 ...
  .. .. ..@ Dim     : int [1:2] 11 11
  .. .. ..@ Dimnames:List of 2
  .. .. .. ..$ : chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  .. .. .. ..$ : chr [1:11] "1 Transportation" "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" ...
  .. .. ..@ x       : num [1:61] 1 0.318 1 0.543 0.304 ...
  .. .. ..@ uplo    : chr "U"
  .. .. ..@ factors : list()
 $ proximity_tibble                     :List of 2
  ..$ proximity_countries:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	3199 obs. of  3 variables:
  .. ..$ from : chr [1:3199] "Algeria" "Angola" "Anguilla" "Argentina" ...
  .. ..$ to   : chr [1:3199] "Albania" "Albania" "Albania" "Albania" ...
  .. ..$ value: num [1:3199] 0.2 0.333 0.333 0.25 0.5 ...
  ..$ proximity_products :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	50 obs. of  3 variables:
  .. ..$ from : chr [1:50] "10 Personal, cultural, and recreational services" "11 Government services, n.i.e." "2 Travel" "3 Communications services" ...
  .. ..$ to   : chr [1:50] "1 Transportation" "1 Transportation" "1 Transportation" "1 Transportation" ...
  .. ..$ value: num [1:50] 0.318 0.543 0.422 0.545 0.364 ...
 $ network_igraph                       :List of 2
  ..$ network_countries:List of 10
  .. ..$ :List of 1
  .. .. ..$ Australia: 'igraph.vs' Named int [1:9] 2 26 39 52 56 57 58 74 76
  .. .. .. ..- attr(*, "names")= chr [1:9] "Mauritius" "Colombia" "Lebanon" "Samoa" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Mauritius: 'igraph.vs' Named int [1:8] 1 26 39 52 56 57 58 74
  .. .. .. ..- attr(*, "names")= chr [1:8] "Australia" "Colombia" "Lebanon" "Samoa" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Chile: 'igraph.vs' Named int [1:3] 19 24 75
  .. .. .. ..- attr(*, "names")= chr [1:3] "Brazil" "Kuwait" "Algeria"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ China: 'igraph.vs' Named int [1:2] 69 75
  .. .. .. ..- attr(*, "names")= chr [1:2] "Singapore" "Algeria"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Japan: 'igraph.vs' Named int 75
  .. .. .. ..- attr(*, "names")= chr "Algeria"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Angola: 'igraph.vs' Named int [1:6] 9 11 25 30 34 59
  .. .. .. ..- attr(*, "names")= chr [1:6] "Aruba" "Bahamas" "China, Macao SAR" "Dominica" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Argentina: 'igraph.vs' Named int 72
  .. .. .. ..- attr(*, "names")= chr "TFYR of Macedonia"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Armenia: 'igraph.vs' Named int [1:3] 45 72 73
  .. .. .. ..- attr(*, "names")= chr [1:3] "Myanmar" "TFYR of Macedonia" "United Rep. of Tanzania"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Aruba: 'igraph.vs' Named int [1:7] 6 11 25 30 34 41 59
  .. .. .. ..- attr(*, "names")= chr [1:7] "Angola" "Bahamas" "China, Macao SAR" "Dominica" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..$ :List of 1
  .. .. ..$ Azerbaijan: 'igraph.vs' Named int 77
  .. .. .. ..- attr(*, "names")= chr "Sri Lanka"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "c82eb14f-3398-444a-874d-f64c80bae617"
  .. ..- attr(*, "class")= chr "igraph"
  ..$ network_products :List of 10
  .. ..$ :List of 1
  .. .. ..$ 11 Government services, n.i.e.: 'igraph.vs' Named int [1:8] 2 3 4 5 6 8 9 11
  .. .. .. ..- attr(*, "names")= chr [1:8] "3 Communications services" "4 Construction services" "10 Personal, cultural, and recreational services" "2 Travel" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 3 Communications services: 'igraph.vs' Named int [1:8] 1 3 4 5 6 8 9 11
  .. .. .. ..- attr(*, "names")= chr [1:8] "11 Government services, n.i.e." "4 Construction services" "10 Personal, cultural, and recreational services" "2 Travel" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 4 Construction services: 'igraph.vs' Named int [1:8] 1 2 4 5 6 8 9 11
  .. .. .. ..- attr(*, "names")= chr [1:8] "11 Government services, n.i.e." "3 Communications services" "10 Personal, cultural, and recreational services" "2 Travel" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 10 Personal, cultural, and recreational services: 'igraph.vs' Named int [1:8] 1 2 3 5 6 8 9 11
  .. .. .. ..- attr(*, "names")= chr [1:8] "11 Government services, n.i.e." "3 Communications services" "4 Construction services" "2 Travel" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 2 Travel: 'igraph.vs' Named int [1:6] 1 2 3 4 6 11
  .. .. .. ..- attr(*, "names")= chr [1:6] "11 Government services, n.i.e." "3 Communications services" "4 Construction services" "10 Personal, cultural, and recreational services" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 5 Insurance services: 'igraph.vs' Named int [1:9] 1 2 3 4 5 7 8 9 11
  .. .. .. ..- attr(*, "names")= chr [1:9] "11 Government services, n.i.e." "3 Communications services" "4 Construction services" "10 Personal, cultural, and recreational services" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 6 Financial services: 'igraph.vs' Named int [1:2] 6 10
  .. .. .. ..- attr(*, "names")= chr [1:2] "5 Insurance services" "8 Royalties and license fees"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 7 Computer and information services: 'igraph.vs' Named int [1:7] 1 2 3 4 6 9 11
  .. .. .. ..- attr(*, "names")= chr [1:7] "11 Government services, n.i.e." "3 Communications services" "4 Construction services" "10 Personal, cultural, and recreational services" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 9 Other business services: 'igraph.vs' Named int [1:7] 1 2 3 4 6 8 11
  .. .. .. ..- attr(*, "names")= chr [1:7] "11 Government services, n.i.e." "3 Communications services" "4 Construction services" "10 Personal, cultural, and recreational services" ...
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..$ :List of 1
  .. .. ..$ 8 Royalties and license fees: 'igraph.vs' Named int 7
  .. .. .. ..- attr(*, "names")= chr "6 Financial services"
  .. .. .. ..- attr(*, "env")=<weakref>
  .. .. .. ..- attr(*, "graph")= chr "cf03afaf-26af-44e8-bba7-a867fbfa46f0"
  .. ..- attr(*, "class")= chr "igraph"
 $ network_tibble                       :List of 2
  ..$ network_countries:Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	215 obs. of  3 variables:
  .. ..$ from : chr [1:215] "Australia" "Australia" "Australia" "Australia" ...
  .. ..$ to   : chr [1:215] "Mauritius" "Colombia" "Lebanon" "Samoa" ...
  .. ..$ value: num [1:215] 1 0.75 0.75 0.75 0.75 ...
  ..$ network_products :Classes ‘tbl_df’, ‘tbl’ and 'data.frame':	36 obs. of  3 variables:
  .. ..$ from : chr [1:36] "11 Government services, n.i.e." "11 Government services, n.i.e." "11 Government services, n.i.e." "11 Government services, n.i.e." ...
  .. ..$ to   : chr [1:36] "3 Communications services" "4 Construction services" "10 Personal, cultural, and recreational services" "2 Travel" ...
  .. ..$ value: num [1:36] 0.488 0.237 0.289 0.453 0.289 ...
}
\examples{
package_output_demo$proximity_tibble$proximity_countries
package_output_demo$proximity_tibble$proximity_products
}
\keyword{datasets}
