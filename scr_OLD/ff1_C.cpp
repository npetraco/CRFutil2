#include <RcppArmadillo.h>
#include "utils.h"

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title       Feature function in Rcpp
//' @description Built-in feature function for convenience and testing. Works pretty much like ff1 on the R side.
//'
//' @param ns     state label of node (\emph{i.e.} node state)
//' @param dots   Optional arguments: \cr
//'               \describe{
//'                 \item{\code{nss.vec}}{node state space (a vector)}
//'                 \item{\code{nss.dim}}{node state space dimension}
//'                 \item{\code{w.vec}}{node state space weight vector}
//'               }
//'
//' @details The function is a handy feature function for testing. It can handle both Ising and Potts state spaces. Surprisingly though this is 2X-4X as slow as ff0.... WHY??
//'
//' @return The feature function, which is a arma vector.
//'
//' @examples XXXX
//'
// [[Rcpp::export]]
arma::vec ff1_C(RObject ns, Nullable<List> dots = R_NilValue) {

  List varg;
  if(dots.isNotNull()) {
    varg = as<List>(dots);
  }

  // Parse the "dots" optional input parameters:
  int           nss_dim;        // Node state space dimension
  GenericVector nss_vec;        // Node state space. Should be able to be numbers or names.
  arma::vec     w_vec;          // Node state weight vector
  arma::vec     I_vec(nss_dim); // Indicator vector for node state space
  if(varg.length() > 0) {
    CharacterVector varg_nms = varg.names();

    // Process node state space dimension:
    bool nss_dimQ = std::find(varg_nms.begin(), varg_nms.end(), "nss.dim") != varg_nms.end();
    bool nss_vecQ = std::find(varg_nms.begin(), varg_nms.end(), "nss.vec") != varg_nms.end();
    bool w_vecQ   = std::find(varg_nms.begin(), varg_nms.end(), "w.vec")   != varg_nms.end();

    // Check for what the names are of the node states are in the node state space
    if(nss_vecQ) {
      nss_vec = as<GenericVector>(varg["nss.vec"]); // User input a node state space
      nss_dim = nss_vec.length();                   // NOTE: overrides nss.dim if user specified it! We want that though.
    } else if(nss_dimQ){
      nss_dim = varg["nss.dim"];                    // Just do sequential positive integers if no node state names are given
      nss_vec = seq(1, nss_dim);
    } else {
      nss_dim = 2;                                  // Just assume a generic 2 dimensional node state space (Ising c(1,2)) if no nss.dim or nss.vec specified. Throw suppressible warning??
      nss_vec = seq(1, nss_dim);
    }

    // No 1D node state spaces
    if(nss_dim < 2) {
      stop("Node state space dimension, nss.dim, must be 2 or more.");
    }

    // Check for a weight vector
    // Cf. For detail of trick used: https://stackoverflow.com/questions/44231046/error-using-rcppnullable-with-rcpparmadillo-types
    if(w_vecQ) {
      w_vec = as<arma::vec>(varg["w.vec"]);
    } else{
      w_vec = arma::ones(nss_dim); // Just do unit state weights if no weight vector is given
    }

  } else { // No "dots" input parameters given so just assume generic Ising model
    nss_dim = 2;
    nss_vec = seq(1, nss_dim);
    w_vec   = arma::ones(nss_dim);
  }

  // Make sure specified node state is in node state space:
  I_vec = compare_element(ns, nss_vec);
  int ns_inQ = sum(I_vec);
  if(ns_inQ == 0){
    stop("Input node state does not match anything in node state space!");
  }

  // At this point everything should be OK. Compute the feature function
  arma::vec ff_vec(nss_dim);
  ff_vec = w_vec % I_vec;

  return ff_vec;
 }
