#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

// Built-in feature function for convenience and testing. Works pretty much like ff0 on the R side.
// Surprisingly though this is twice as slow as ff0....

// [[Rcpp::export]]
arma::vec ff0_C(RObject st, int ss_dim=2, Nullable<IntegerVector> w_vec = R_NilValue, Nullable<StringVector> st_vec = R_NilValue) {

  // Check for a weight vector
  // Cf. For detail of trick used: https://stackoverflow.com/questions/44231046/error-using-rcppnullable-with-rcpparmadillo-types
  arma::vec w_vec_loc;
  if(w_vec.isNotNull()) {
    w_vec_loc = as<arma::vec>(w_vec);
  } else{
    w_vec_loc = arma::ones(ss_dim); // Just do unit state weights if no weight vector is given
  }

  // Check for a what the names are of the node states and perform comparison
  List st_vec_loc(ss_dim);
  if(st_vec.isNotNull()) {

    //Test component type. Are they strings or numbers??
    if(TYPEOF(st_vec) == STRSXP) { // Node state names are characters

      StringVector tmp = as<StringVector>(st_vec); // Is there a better way to do this??
      if(tmp.length() != ss_dim){
        stop("Number of node states does not match node state space dimension!");
      }

      for(int i=0; i<ss_dim; i++){
        st_vec_loc[i] = Rf_mkString(tmp[i]); // Convert CHARSXP to STRSXP. Why do we have to do this though??
      }

    } else if(TYPEOF(st_vec) == REALSXP) { // Node state names are numbers

      NumericVector tmp = as<NumericVector>(st_vec);
      if(tmp.length() != ss_dim){
        stop("Number of node states does not match node state space dimension!");
      }

      for(int i=0; i<ss_dim; i++){
        st_vec_loc[i] = tmp[i];
      }

    } else{
      stop("st_vec must be a vector of characters or reals");
    }
  } else{ // Just do sequential positive integers if no node state names are given

    for(int i=0; i<ss_dim; i++){
      st_vec_loc[i] = i + 1.0;
    }

  }

  if(st_vec_loc.length() != ss_dim){
    stop("Number of node states does not match node state space dimension!");
  }

  if(w_vec_loc.size() != ss_dim){
    stop("Dimension of weight vector does not match node state space dimension!");
  }

  // Maybe get rid of this and just check in the comparison??
  if(TYPEOF(st) != TYPEOF(st_vec_loc[0])){
    stop("Type must be the same for st (node state) and st_vec (node state space)!");
  }

  arma::vec I_vec(ss_dim);
  for(int i=0; i<ss_dim; i++){
    I_vec[i] = (Rf_asChar(st) == Rf_asChar(st_vec_loc[i]));
  }

  // Check that st is somewhere in the state space
  if(sum(I_vec) == 0){
    stop("Input node state, st not found in node state space, st_vec!");
  }

  arma::vec out_ff(ss_dim);
  out_ff = w_vec_loc % I_vec;

  return(out_ff);

}
