#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::cx_vec getEigenValues(arma::mat M) {
  arma::cx_vec eigval;
  arma::cx_mat eigvec;
  return arma::eigs_gen(eigval, eigvec, M, 1);  // find 5 eigenvalues/eigenvectors
}
