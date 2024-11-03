#include <Rcpp.h>

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
 List rcpp_prototypef() {
   CharacterVector x = CharacterVector::create("foo", "bar");
   NumericVector y   = NumericVector::create(0.0, 1.0);
   List z            = List::create(x, y);
   return z;
 }
