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
//' @param vector  A vector of things to compare element to. Can be any types.
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


//' @title row.match in C
//' @description Looks for matching rows in a matrix.
//'
//' @param x     a row vector
//' @param table a matrix to be searched for rows matching x
//'
//' @details Much faster than in R and we don't want to pass in R functions anyway.
//'
//' @return Indices of matching rows
//'
//' @examples XXXX
//'
// [[Rcpp::export]]
arma::uvec row_match(arma::Mat<int> x, arma::Mat<int> table) {

  arma::uvec matching_row_idxs;

  // Note: Below assume x is a effectively a vector with one row or one column
  // We pass it in as a matrix though. Noticed Armadillo routenes often ran faster that way......??

  // Get common first set of possible row match indices
  matching_row_idxs = intersect(find(table.col(0) == x(0)), find(table.col(1) == x(1)));

  Rcout << "got here" << endl;

  // Look over the rest and update (pairdown) the possible matching rows
  for(int i=2; i<x.size(); ++i){
    matching_row_idxs = intersect(matching_row_idxs, find(table.col(i) == x(i)));
  }

  // Note: a size 0 return means no matching row was found
  return matching_row_idxs;
}
