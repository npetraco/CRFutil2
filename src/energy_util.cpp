#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title       One-body (node) energy function
//' @description One-body (node) energy function
//'
//' @param yA  a node state
//' @param tA  tau vector
//' @param ff  feature function
//' @param dots list of optional arguments for feature function
//'
//' @details One-body (node) energy function
//'
//' @return One-body (node) energy.
//'
//' @examples # node weights:
//'tau2 <- c( 1, -1.3)
//'tau3 <- c( 1, -1.3, 0.1)
//'
//'Eone_C(yA = 2, tA = tau3, ff = ff1_C, list(nss.dim=3))
//'Eone_C(yA = 3, tA = tau3, ff = ff1_C, list(nss.dim=3))
//'Eone_C(yA = 2, tA = tau2, ff = ff1_C)
//'Eone_C(yA = "a", tA = tau3, ff = ff1_C, list(nss.vec = c("a", "b", "c")))
//'Eone_C(yA = 2, tA = tau2, ff = ff0_C)
//'Eone_C(yA = 1, tA = tau2, ff = ff0_C)
//'Eone_C(yA = 3, tA = tau2, ff = ff0_C) # Throws error because less general ff0 used
//'
// [[Rcpp::export]]
double Eone_C(RObject yA, arma::vec tA, Function ff, Nullable<List> dots = R_NilValue) {

  double val = arma::dot(tA , as<arma::vec>(ff(yA, dots)));
  return(val);
}


//' @title       Two-body (edge) energy function
//' @description Two-body (edge) energy function
//'
//' @param yA   node A state
//' @param yB   node B state
//' @param wAB  omega matrix
//' @param ff   feature function
//' @param dots list of optional arguments for feature function
//'
//' @details Two-body (edge) energy function
//'
//' @return Two-body (edge) energy.
//'
//' @examples # edge weights:
//'omega22 <- rbind(
//'  c( 3.5, -1.4),
//'  c(-1.4,  2.5)
//')
//'
//'omega33 <- rbind(
//'  c( 3.5, -1.4,   2.5),
//'  c(-1.4,  2.5,  -0.8),
//'  c( 3.8,  0.95, -7.6)
//')
//'
//' Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff1_C, list(nss.dim=2))
//' Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff1_C, list(nss.dim=3)) # Throws error
//' Etwo_C(yA = 2, yB = 2, wAB = omega33, ff = ff1_C, list(nss.dim=3)) # Throws error
//' Etwo_C(yA = "c", yB = "b", wAB = omega33, ff = ff1_C, list(nss.vec=c("a", "b", "c")))
//'
//' Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff0_C)
//' Etwo_C(yA = 2, yB = 1, wAB = omega33, ff = ff0_C)
//' Etwo_C(yA = 2, yB = 1, wAB = omega33, ff = ff1_C) # Throws error bec nss.dim=2 assumed
//' Etwo_C(yA = 2, yB = 1, wAB = omega22, ff = ff1_C)
//'
// [[Rcpp::export]]
double Etwo_C(RObject yA, RObject yB, arma::mat wAB, Function ff, Nullable<List> dots = R_NilValue) {

 double val = as_scalar(as<arma::vec>(ff(yA, dots)).t() * wAB * as<arma::vec>(ff(yB, dots)));
 return(val);
}



