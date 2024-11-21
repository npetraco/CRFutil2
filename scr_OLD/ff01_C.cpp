#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title       Stripped down version of ff0_C. No checking. Not idiot proof.
//' @description Built-in feature function for convenience and testing.
//'
//' @param st     State label
//' @param ss_dim State space dimension. Minimum is 2 (Ising), but function doesn't check.
//' @param w_vec  Weight vector. Needs to be of length ss_dim.
//' @param st_vec Vector for state names. Needs to be of length ss_dim.
//'
//' @details Stripped down version of ff0_C. No checking. Not idiot proof. Meant to be faster. Runs about as fast as ff0 on the R side.
//'
//' @return The feature function, which is a arma vector.
//'
// [[Rcpp::export]]
arma::vec ff01_C(double st, double ss_dim, arma::vec w_vec, arma::vec st_vec) {

  arma::uvec I_vec(ss_dim);
  I_vec = (st == st_vec);

  arma::vec out_ff(ss_dim);
  out_ff = w_vec % I_vec;

  return(out_ff);
}
