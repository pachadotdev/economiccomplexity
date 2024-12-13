#include <cpp11.hpp>
#include <cpp11armadillo.hpp>

using namespace arma;
using namespace cpp11;

///////////////////////////////////////////////////////////////////////////////
// Balassa Index
///////////////////////////////////////////////////////////////////////////////

[[cpp11::register]] sexp balassa_index_(const doubles_matrix<>& trade_matrix,
                                        const bool& discrete,
                                        const double& cutoff) {
  Mat<double> res = as_Mat(trade_matrix);

  Col<double> row_sums = sum(res, 1);
  Row<double> col_sums = sum(res, 0);
  double total_sum = as_scalar(sum(row_sums));
  res.each_col() /= row_sums;
  res.each_row() /= col_sums / total_sum;
  
  if (discrete) {
    res.transform(
        [cutoff](double val) { return (val < cutoff) ? 0.0 : 1.0; });
  }

  sexp res2 = as_cpp<sexp>(as_doubles_matrix(res));
  res2.attr("dimnames") = as_cpp<list>(trade_matrix.attr("dimnames"));

  return res2;
}

///////////////////////////////////////////////////////////////////////////////
// Complexity Measures
///////////////////////////////////////////////////////////////////////////////

[[cpp11::register]] list fitness_method_(const doubles_matrix<>& balassa_index,
                                         const int& iterations,
                                         const double& extremality) {
  Mat<double> res = as_Mat(balassa_index);

  // Create empty matrices
  Mat<double> kx(res.n_rows, iterations);
  Mat<double> ky(res.n_cols, iterations);

  // Fill the first columns with 1 to start iterating
  kx.col(0).fill(1);
  ky.col(0).fill(1);

  // Compute cols 2 to "no. of iterations" by iterating from col 1
  int j;
  for (j = 1; j < iterations; ++j) {
    kx.col(j) = res * ky.col(j - 1);
    kx.col(j) /= mean(kx.col(j));

    ky.col(j) =
        1 / pow(res.t() * pow(1 / kx.col(j - 1), extremality), 1 / extremality);
    ky.col(j) /= mean(ky.col(j));
  }

  writable::doubles xci = as_doubles(kx.col(iterations - 1));
  writable::doubles yci = as_doubles(ky.col(iterations - 1));

  // Set names for the results
  writable::list dimnames = as_cpp<list>(balassa_index.attr("dimnames"));
  xci.attr("names") = dimnames[0];
  yci.attr("names") = dimnames[1];

  return writable::list({"complexity_index_country"_nm = xci,
                         "complexity_index_product"_nm = yci});
}

[[cpp11::register]] list reflections_method_(
    const doubles_matrix<>& balassa_index, const int& iterations) {
  Mat<double> res = as_Mat(balassa_index);

  // Create empty matrices
  Mat<double> kx(res.n_rows, iterations);
  Mat<double> ky(res.n_cols, iterations);

  // Fill the first columns with rowSums(balassa_index) and
  // colSums(balassa_index) to start iterating
  Col<double> row_sums = sum(res, 1);
  Col<double> col_sums = sum(res, 0).t();
  kx.col(0) = row_sums;
  ky.col(0) = col_sums;

  // Compute cols 2 to "no. of iterations" by iterating from col 1
  for (int j = 1; j < iterations; ++j) {
    kx.col(j) = (res * ky.col(j - 1)) / row_sums;
    ky.col(j) = (res.t() * kx.col(j - 1)) / col_sums;
  }

  // xci is of odd order and normalized
  // yci is of even order and normalized
  Col<double> xci_col =
      (kx.col(iterations - 2) - mean(kx.col(iterations - 2))) /
      stddev(kx.col(iterations - 2));
  Col<double> yci_col =
      (ky.col(iterations - 1) - mean(ky.col(iterations - 1))) /
      stddev(ky.col(iterations - 1));

  writable::doubles xci = as_doubles(xci_col);
  writable::doubles yci = as_doubles(yci_col);

  // Set names for the results
  writable::list dimnames = as_cpp<list>(balassa_index.attr("dimnames"));
  xci.attr("names") = dimnames[0];
  yci.attr("names") = dimnames[1];

  return writable::list({"complexity_index_country"_nm = xci,
                         "complexity_index_product"_nm = yci});
}

[[cpp11::register]] list eigenvalues_method_(
    const doubles_matrix<>& balassa_index, const int& iterations) {
  Mat<double> res = as_Mat(balassa_index);

  Col<double> row_sums = sum(res, 1);
  Row<double> col_sums = sum(res, 0);

  Mat<double> res_row_normalized = res.each_col() / row_sums;
  Mat<double> res_col_normalized = res.each_row() / col_sums;
  
  // Compute eigenvalues for xci
  Col<cx_double> eigval_xci;
  Mat<cx_double> eigvec_xci;
  eig_gen(eigval_xci, eigvec_xci, res_row_normalized * res_col_normalized.t());
  Col<double> xci = real(eigvec_xci.col(1));

  // Compute eigenvalues for yci
  Col<cx_double> eigval_yci;
  Mat<cx_double> eigvec_yci;
  eig_gen(eigval_yci, eigvec_yci, res_col_normalized.t() * res_row_normalized);
  Col<double> yci = real(eigvec_yci.col(1));

  // Normalize xci and yci
  xci = (xci - mean(xci)) / stddev(xci);
  yci = (yci - mean(yci)) / stddev(yci);
  writable::doubles xci_r = as_doubles(xci);
  writable::doubles yci_r = as_doubles(yci);

  // Set names for the results
  writable::list dimnames = as_cpp<list>(balassa_index.attr("dimnames"));
  xci_r.attr("names") = dimnames[0];
  yci_r.attr("names") = dimnames[1];

  return writable::list({"complexity_index_country"_nm = xci_r,
                         "complexity_index_product"_nm = yci_r});
}

///////////////////////////////////////////////////////////////////////////////
// Distance
///////////////////////////////////////////////////////////////////////////////

[[cpp11::register]] sexp distance_(const doubles_matrix<>& balassa_index,
                                   const doubles_matrix<>& proximity_product) {
  Mat<double> res = as_Mat(balassa_index);
  Mat<double> res2 = as_Mat(proximity_product);

  Col<double> row_sums = sum(res2, 1);
  Mat<double> res2_normalized = res2.each_col() / row_sums;
  Mat<double> distance_matrix = (1.0 - res) * res2_normalized.t();

  sexp res3 = as_cpp<sexp>(as_doubles_matrix(distance_matrix));
  writable::list dimnames = as_cpp<list>(balassa_index.attr("dimnames"));
  res3.attr("dimnames") = dimnames;

  return res3;
}

///////////////////////////////////////////////////////////////////////////////
// Proximity
///////////////////////////////////////////////////////////////////////////////

[[cpp11::register]] list proximity_(const doubles_matrix<>& balassa_index,
                                    const std::string& compute) {
  Mat<double> res = as_Mat(balassa_index);

  Col<double> row_sums = sum(res, 1);
  Col<double> col_sums = sum(res, 0).t();

  Mat<double> prox_x;
  Mat<double> prox_y;

  // Set names for the results
  writable::list dimnames = as_cpp<list>(balassa_index.attr("dimnames"));
  writable::list res2;

  if ((compute == "country") | (compute == "both")) {
    prox_x = res * res.t();
    uword n = prox_x.n_rows;
    uword m = prox_x.n_cols;
    for (uword i = 0; i < n; ++i) {
      for (uword j = 0; j < m; ++j) {
        prox_x(i, j) /= std::max(row_sums(i), row_sums(j));
      }
    }
    sexp prox_x_r = as_cpp<sexp>(as_doubles_matrix(prox_x));
    prox_x_r.attr("dimnames") = writable::list({dimnames[0], dimnames[0]});
    res2.push_back({"proximity_country"_nm = prox_x_r});
  }

  if ((compute == "product") | (compute == "both")) {
    prox_y = res.t() * res;
    uword n = prox_y.n_rows;
    uword m = prox_y.n_cols;
    for (uword i = 0; i < n; ++i) {
      for (uword j = 0; j < m; ++j) {
        prox_y(i, j) /= std::max(col_sums(i), col_sums(j));
      }
    }
    sexp prox_y_r = as_cpp<sexp>(as_doubles_matrix(prox_y));
    prox_y_r.attr("dimnames") = writable::list({dimnames[1], dimnames[1]});
    res2.push_back({"proximity_product"_nm = prox_y_r});
  }

  return res2;
}
