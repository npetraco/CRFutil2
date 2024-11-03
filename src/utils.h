#ifndef __UTILS__
#define __UTILS__

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

arma::vec compare_element(RObject,  GenericVector);
bool inQ(RObject,  GenericVector);

#endif // __UTILS__
