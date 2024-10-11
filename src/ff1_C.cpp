#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

// Stripped down version of ff0_C. No checking. Not idiot proof.

// [[Rcpp::export]]
arma::vec ff1_C(double st, double ss_dim, arma::vec w_vec, arma::vec st_vec) {

  arma::uvec I_vec(ss_dim);
  I_vec = (st == st_vec);

  arma::vec out_ff(ss_dim);
  out_ff = w_vec % I_vec;

  return(out_ff);
}
