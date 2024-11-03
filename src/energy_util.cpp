#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title        XX function
//' @description XX
//'
//' @param XX    XX
//'
//' @details The function XX
//'
//' @return  XX
//'
//' @examples XXXX
//'
// [[Rcpp::export]]
double Eone_C(RObject yA, arma::vec tA, Function ff, Nullable<List> dots = R_NilValue) {

  //return arma::as_scalar(tA * as<arma::vec>(ff(yA, dots)));
  return(arma::dot(tA , as<arma::vec>(ff(yA, dots))));
}
