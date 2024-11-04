#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title       Very stripped down version of ff1_C. No checking, assumes unit weighted states and integer 1:(node state space dimension) node state names.
//' @description Built-in feature function for convenience and testing.
//'
//' @param ns      State label
//' @param nss_vec Vector for state names. Use in place of dots arguement.
//'
//' @details Very stripped down version of ff1_C. Needs a helper R wrapper (ff12_RC()) because of the whole dots thing.
//' Built for speed....I hope.
//'
//' @return The feature function, which is a arma unsigned vector.
//'
// [[Rcpp::export]]
NumericVector ff12_C(double ns, NumericVector nss_vec) {

  int nss_dim = nss_vec.length();
  NumericVector indic_vec(nss_dim);
  for(int i=0; i<nss_dim; i++){
    indic_vec[i] = (ns == nss_vec[i]);
  }

  return(indic_vec);

}
