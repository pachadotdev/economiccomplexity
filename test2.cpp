// Open ts-yearly-datasets.Rproj before running this function

// Copyright (c) 2018, Mauricio \"Pacha\" Vargas
// This file is part of Open Trade Statistics project
// The scripts within this project are released under GNU General Public License 3.0
// See https://github.com/tradestatistics/ts-yearly-datasets/LICENSE for the details

#include <omp.h>
#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(openmp)]]

using namespace Rcpp;

// [[Rcpp::export]]
arma::mat proximity_countries_denominator(arma::sp_mat Mcp, arma::mat D, int cores = 1) {
  // Constants
  int M = (int) Mcp.n_rows;

  // Output
  arma::mat Phi_down(M,M);

  // Filling with ones
  Phi_down.ones();

  // Number of cores
  omp_set_num_threads(cores);

#pragma omp parallel for shared(Mcp, D, M, Phi_down) default(none)
  for (int i=0; i<M; i++)
    for (int j=0; j<=i; j++) {
      // Fill the lower part
      Phi_down.at(i,j) = std::max(D(i,0), D(j,0));
      // Fill the upper part
      Phi_down.at(j,i) = Phi_down.at(i,j);
    }

    return Phi_down;
}
