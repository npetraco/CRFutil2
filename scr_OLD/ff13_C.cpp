#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title        Very ver stripped down version of ff1_C. No checking, assumes unit weighted states and integer node state names starting at 1.
//' @description Built-in feature function for convenience and testing.
//'
//' @param ns      State label (integer between 1 and nss_dim)
//' @param nss_dim Node state space dimension.
//'
//' @details Very very stripped down version of ff1_C. Built for speed. Assumes state space is
//' integer valued, 1:nss_dim. Probably should use this with an R-side wrapper to sort out other
//' arguments and arbitrary state names. CAUTION! No checks performed. This function can easily
//' segfault.
//'
//' @return The feature function, which is an Rcpp integer vector.
//'
// [[Rcpp::export]]
IntegerVector ff13_C(int ns, int nss_dim) {

  IntegerVector indic_vec(nss_dim, 0);
  indic_vec[ns-1] = 1;

  return(indic_vec);
 }
