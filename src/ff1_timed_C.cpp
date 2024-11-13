#include <RcppArmadillo.h>
#include <RcppClock.h> // Has to come after Rcpp.h or RcppArmadillo.h
#include "utils.h"

// Not necessary since we link these libraries in the DESCRIPTOPN file??
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppClock)]]

using namespace Rcpp;
using namespace std;

//' @title       Feature function in Rcpp example with timers via RcppClock
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
//' @details Example function with timers via RcppClock for timing execution on blocks of code.
//' This is also part of experiments to explore why ff1_C runs 2X-4X as slow as ff0.
//'
//' @return The feature function, which is a arma vector.
//'
//' @examples XXXX
//'
// [[Rcpp::export]]
arma::vec ff1_times_C(RObject ns, Nullable<List> dots = R_NilValue) {

  Clock clock;

  clock.tick("sect1");
  List varg;
  if(dots.isNotNull()) {
    varg = as<List>(dots);
  }
  clock.tock("sect1");

  // Parse the "dots" optional input parameters:
  int           nss_dim;        // Node state space dimension
  GenericVector nss_vec;        // Node state space. Should be able to be numbers or names.
  arma::vec     w_vec;          // Node state weight vector
  arma::vec     I_vec(nss_dim); // Indicator vector for node state space
  clock.tick("sect2");
  if(varg.length() > 0) {
    CharacterVector varg_nms = varg.names();

    // Process node state space dimension:
    clock.tick("sect2a");
    bool nss_dimQ = std::find(varg_nms.begin(), varg_nms.end(), "nss.dim") != varg_nms.end();
    bool nss_vecQ = std::find(varg_nms.begin(), varg_nms.end(), "nss.vec") != varg_nms.end();
    bool w_vecQ   = std::find(varg_nms.begin(), varg_nms.end(), "w.vec")   != varg_nms.end();
    clock.tock("sect2a");

    // Check for what the names are of the node states are in the node state space
    clock.tick("sect2b");
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
    clock.tock("sect2b");

    // No 1D node state spaces
    clock.tick("sect2c");
    if(nss_dim < 2) {
      stop("Node state space dimension, nss.dim, must be 2 or more.");
    }
    clock.tock("sect2c");

    // Check for a weight vector
    // Cf. For detail of trick used: https://stackoverflow.com/questions/44231046/error-using-rcppnullable-with-rcpparmadillo-types
    clock.tick("sect2d");
    if(w_vecQ) {
      w_vec = as<arma::vec>(varg["w.vec"]);
    } else{
      w_vec = arma::ones(nss_dim); // Just do unit state weights if no weight vector is given
    }
    clock.tock("sect2d");

  } else { // No "dots" input parameters given so just assume generic Ising model
    nss_dim = 2;
    nss_vec = seq(1, nss_dim);
    w_vec   = arma::ones(nss_dim);
  }
  clock.tock("sect2");

  // Make sure specified node state is in node state space:
  clock.tick("sect3");
  I_vec = compare_element(ns, nss_vec);
  int ns_inQ = sum(I_vec);
  if(ns_inQ == 0){
    stop("Input node state does not match anything in node state space!");
  }
  clock.tock("sect3");

  // At this point everything should be OK. Compute the feature function
  clock.tick("sect4");
  arma::vec ff_vec(nss_dim);
  ff_vec = w_vec % I_vec;
  clock.tock("sect4");

  clock.stop("sect_times");

  return ff_vec;
 }
