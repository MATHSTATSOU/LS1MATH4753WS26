#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::depends(Rcpp)]]

/*
 * Sample variance using Welford's algorithm (numerically stable).
 * Matches R's var(x) behavior for numeric vectors when na.rm is respected.
 *
 * Args:
 *   x     : numeric vector
 *   na_rm : logical; if true, remove NA/NaN values; if false, return NA if any NA/NaN present
 *
 * Returns:
 *   double: sample variance (denominator n - 1). Returns NA_real_ if:
 *           - there are fewer than 2 non-missing observations, or
 *           - na_rm == false and any NA/NaN is present.
 */
// [[Rcpp::export]]
double cpp_var(NumericVector x, bool na_rm = false) {
  // Quick path when any NA present and na_rm == false
  if (!na_rm) {
    for (R_xlen_t i = 0; i < x.size(); ++i) {
      if (Rcpp::NumericVector::is_na(x[i]) || Rcpp::traits::is_nan<REALSXP>(x[i])) {
        return NA_REAL;
      }
    }
  }

  R_xlen_t n = 0;       // count of valid values
  long double mean = 0; // use long double for better accumulation stability
  long double M2 = 0;

  for (R_xlen_t i = 0; i < x.size(); ++i) {
    double xi = x[i];

    // Skip missing if na_rm; otherwise they were handled above
    if (na_rm && (Rcpp::NumericVector::is_na(xi) || Rcpp::traits::is_nan<REALSXP>(xi))) {
      continue;
    }

    // Welford update
    n += 1;
    long double delta = static_cast<long double>(xi) - mean;
    mean += delta / n;
    long double delta2 = static_cast<long double>(xi) - mean;
    M2 += delta * delta2;
  }

  if (n < 2) {
    return NA_REAL; // same behavior as R: var of length < 2 is NA
  }

  long double var = M2 / (n - 1); // sample variance
  return static_cast<double>(var);
}

