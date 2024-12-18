// Generated by cpp11: do not edit by hand
// clang-format off


#include "cpp11/declarations.hpp"
#include <R_ext/Visibility.h>

// code.cpp
sexp balassa_index_(const doubles_matrix<>& trade_matrix, const bool& discrete, const double& cutoff);
extern "C" SEXP _economiccomplexity_balassa_index_(SEXP trade_matrix, SEXP discrete, SEXP cutoff) {
  BEGIN_CPP11
    return cpp11::as_sexp(balassa_index_(cpp11::as_cpp<cpp11::decay_t<const doubles_matrix<>&>>(trade_matrix), cpp11::as_cpp<cpp11::decay_t<const bool&>>(discrete), cpp11::as_cpp<cpp11::decay_t<const double&>>(cutoff)));
  END_CPP11
}
// code.cpp
list fitness_method_(const doubles_matrix<>& balassa_index, const int& iterations, const double& extremality);
extern "C" SEXP _economiccomplexity_fitness_method_(SEXP balassa_index, SEXP iterations, SEXP extremality) {
  BEGIN_CPP11
    return cpp11::as_sexp(fitness_method_(cpp11::as_cpp<cpp11::decay_t<const doubles_matrix<>&>>(balassa_index), cpp11::as_cpp<cpp11::decay_t<const int&>>(iterations), cpp11::as_cpp<cpp11::decay_t<const double&>>(extremality)));
  END_CPP11
}
// code.cpp
list reflections_method_(const doubles_matrix<>& balassa_index, const int& iterations);
extern "C" SEXP _economiccomplexity_reflections_method_(SEXP balassa_index, SEXP iterations) {
  BEGIN_CPP11
    return cpp11::as_sexp(reflections_method_(cpp11::as_cpp<cpp11::decay_t<const doubles_matrix<>&>>(balassa_index), cpp11::as_cpp<cpp11::decay_t<const int&>>(iterations)));
  END_CPP11
}
// code.cpp
list eigenvalues_method_(const doubles_matrix<>& balassa_index, const int& iterations);
extern "C" SEXP _economiccomplexity_eigenvalues_method_(SEXP balassa_index, SEXP iterations) {
  BEGIN_CPP11
    return cpp11::as_sexp(eigenvalues_method_(cpp11::as_cpp<cpp11::decay_t<const doubles_matrix<>&>>(balassa_index), cpp11::as_cpp<cpp11::decay_t<const int&>>(iterations)));
  END_CPP11
}
// code.cpp
sexp distance_(const doubles_matrix<>& balassa_index, const doubles_matrix<>& proximity_product);
extern "C" SEXP _economiccomplexity_distance_(SEXP balassa_index, SEXP proximity_product) {
  BEGIN_CPP11
    return cpp11::as_sexp(distance_(cpp11::as_cpp<cpp11::decay_t<const doubles_matrix<>&>>(balassa_index), cpp11::as_cpp<cpp11::decay_t<const doubles_matrix<>&>>(proximity_product)));
  END_CPP11
}
// code.cpp
list proximity_(const doubles_matrix<>& balassa_index, const std::string& compute);
extern "C" SEXP _economiccomplexity_proximity_(SEXP balassa_index, SEXP compute) {
  BEGIN_CPP11
    return cpp11::as_sexp(proximity_(cpp11::as_cpp<cpp11::decay_t<const doubles_matrix<>&>>(balassa_index), cpp11::as_cpp<cpp11::decay_t<const std::string&>>(compute)));
  END_CPP11
}

extern "C" {
static const R_CallMethodDef CallEntries[] = {
    {"_economiccomplexity_balassa_index_",      (DL_FUNC) &_economiccomplexity_balassa_index_,      3},
    {"_economiccomplexity_distance_",           (DL_FUNC) &_economiccomplexity_distance_,           2},
    {"_economiccomplexity_eigenvalues_method_", (DL_FUNC) &_economiccomplexity_eigenvalues_method_, 2},
    {"_economiccomplexity_fitness_method_",     (DL_FUNC) &_economiccomplexity_fitness_method_,     3},
    {"_economiccomplexity_proximity_",          (DL_FUNC) &_economiccomplexity_proximity_,          2},
    {"_economiccomplexity_reflections_method_", (DL_FUNC) &_economiccomplexity_reflections_method_, 2},
    {NULL, NULL, 0}
};
}

extern "C" attribute_visible void R_init_economiccomplexity(DllInfo* dll){
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
