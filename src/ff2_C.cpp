#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

// Very stripped down version of ff0_C. No checking and assumes unit weighted states.

// [[Rcpp::export]]
arma::uvec ff2_C(double st, double ss_dim, arma::vec st_vec) {

  //arma::uvec I_vec(ss_dim);
  //I_vec = (st == st_vec);

  return(st == st_vec);
}
