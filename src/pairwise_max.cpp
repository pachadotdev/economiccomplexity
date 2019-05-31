#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

//' @title pairwise_max
//' @description
//' tbd
//' @name pairwise_max
//' @param n integer
//' @param k vector
//' @examples
//' #tbd
//' @export
// [[Rcpp::export]]
arma::mat pairwise_max(int n, arma::vec k) {
  // Output
  arma::mat m(n,n);

  // Complete with maximum values
  for (int i=0; i<n; i++)
    for (int j=0; j<=i; j++) {
      // Fill the lower part
      m.at(i,j) = std::max(k(i), k(j));
      // Fill the upper part
      m.at(j,i) = m.at(i,j);
    }

    return m;
}
