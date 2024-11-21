#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title       A stripped down version of ff1_C. No checking, assumes unit weighted states and integer 1:(node state space dimension) node state names.
//' @description Built-in feature function for convenience and testing.
//'
//' @param ns      State label
//' @param nss_vec Vector for state names. Use in place of dots arguement.
//'
//' @details A stripped down version of ff1_C. Built for speed....I hope.
//'
//' @return The feature function, which is a arma unsigned vector.
//'
// [[Rcpp::export]]
NumericVector ff11_C(double ns, List dots) {

  int dim = dots["nss.dim"];
  NumericVector nss_vec = dots["nss.vec"]; // Sad..... Can we ever avoid this copy????
  NumericVector indic_vec(dim);
  for(int i=0; i<dim; i++){
    indic_vec[i] = (ns == nss_vec[i]);
  }

  return(indic_vec);

}
