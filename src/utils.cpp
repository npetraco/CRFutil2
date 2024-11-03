#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title       Compare an element to elements in a vector
//' @description Compare an element to elements in a vector, independent of data type.
//'
//' @param element Something to compare to. Can be any single element RObject
//' @param vector A vector of things to compare element to. Can be any types.
//'
//' @details Sometime when comparing RObjects of arbitrary type doesn't work right because the wrong type
//' is guessed, i.e. element RObject cast to a hex code for a char but needs it to be interpreted as a hex
//' code for a string. This function just casts EVERYTHING to char hex codes using Rf_asChar(). This trick
//' seems to work.
//'
//' @return An (arma) indicator vector. 1 means element same as vector's element and 0 otherwise.
//'
//' @examples XXXX
//'
// [[Rcpp::export]]
arma::vec compare_element(RObject element, GenericVector vector) {

  int dim = vector.length();
  arma::vec indic_vec(dim);
  for(int i=0; i<dim; i++){
    indic_vec[i] = (Rf_asChar(element) == Rf_asChar(vector[i]));
  }

  return(indic_vec);

}


//' @title Check if element is in a vector
//' @description Check if element is in a vector, independent of data type.
//'
//' @param element Something to compare to. Can be any single element RObject
//' @param vector A vector of things to compare element to. Can be any types.
//'
//' @details I'm tired of writing this out longhand in the codes so writing a wrapper.
//'
//' @return A bool. 1 = element in vector, 0 otherwise
//'
//' @examples XXXX
//'
// [[Rcpp::export]]
bool inQ(RObject element, GenericVector vector) {

  return(sum(compare_element(element, vector)));

 }
