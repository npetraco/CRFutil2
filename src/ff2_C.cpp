#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title       Very stripped down version of ff0_C. No checking and assumes unit weighted states.
//' @description Built-in feature function for convenience and testing.
//'
//' @param st     State label
//' @param st_vec Vector for state names. Needs to be at least two (numerical) components.
//'
//' @details Very stripped down version of ff0_C. Runs fastest of the three: ff0_C, ff1_C and ff2_C.
//'
//' @return The feature function, which is a arma unsigned vector.
//'
// [[Rcpp::export]]
arma::uvec ff2_C(double st, arma::vec st_vec) {

  return(st == st_vec);

}
