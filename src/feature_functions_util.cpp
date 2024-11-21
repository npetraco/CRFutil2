#include <RcppArmadillo.h>
#include <RcppClock.h> // Has to come after Rcpp.h or RcppArmadillo.h
#include "utils.h"

// Below not necessary since we link these libraries in the DESCRIPTOPN file??:
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppClock)]]

using namespace Rcpp;
using namespace std;

//' @title       Feature function in Rcpp
//' @description Built-in feature function for convenience and testing. Works pretty much like ff0 on the R side.
//'
//' @param st     State label
//' @param ss_dim State space dimension. Default is 2 (Ising)
//' @param w_vec  Optional weight vector
//' @param st_vec Optional vector for state names
//'
//' @details The function is a handy feature function for testing. It can handle both Ising and Potts state spaces. Surprisingly though this is twice as slow as ff0....
//'
//' @return The feature function, which is a arma vector.
//'
//' @examples ff0_C(st = 3, ss_dim = 4, st_vec = c(0,1,2,3))
//' ff0_C(st = "a", st_vec = c("a","b"))
//' ff0_C(st = "ID", ss.dim = 3, st.vec = c("ID","exclude","inconclusive"))
//' ff0_C(1)
//' ff0_C(1,3)
//' ff0_C(st = "b")           # Throws error
//' ff0_C(st = 5)             # Throws error
//' ff0_C(st = 5, ss_dim = 4) # Throws error
//'
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


//' @title       Stripped down version of ff0_C. No checking. Not idiot proof.
//' @description Built-in feature function for convenience and testing.
//'
//' @param st     State label
//' @param ss_dim State space dimension. Minimum is 2 (Ising), but function doesn't check.
//' @param w_vec  Weight vector. Needs to be of length ss_dim.
//' @param st_vec Vector for state names. Needs to be of length ss_dim.
//'
//' @details Stripped down version of ff0_C. No checking. Not idiot proof. Meant to be faster. Runs 
//' about as fast as ff0 on the R side.
//'
//' @return The feature function, which is a arma vector.
//'
// [[Rcpp::export]]
arma::vec ff01_C(double st, double ss_dim, arma::vec w_vec, arma::vec st_vec) {

  arma::uvec I_vec(ss_dim);
  I_vec = (st == st_vec);

  arma::vec out_ff(ss_dim);
  out_ff = w_vec % I_vec;

  return(out_ff);
}


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


//' @title       Very stripped down version of ff0_C. No checking and assumes unit weighted states.
//' @description Built-in feature function for convenience and testing.
//'
//' @param st     State label
//' @param st_vec Vector for state names. Needs to be at least two (numerical) components.
//'
//' @details Very stripped down version of ff0_C. Runs fastest of the three: ff0_C, ff1_C and ff2_C.
//'
//' @return The feature function, which is a arma unsigned vector.
//'
// [[Rcpp::export]]
arma::uvec ff02_C(double st, arma::vec st_vec) {

  return(st == st_vec);

}


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


//' @title       Very stripped down version of ff1_C. No checking, assumes unit weighted states and integer 1:(node state space dimension) node state names.
//' @description Built-in feature function for convenience and testing.
//'
//' @param ns      State label
//' @param nss_vec Vector for state names. Use in place of dots arguement.
//'
//' @details Very stripped down version of ff1_C. Needs a helper R wrapper (ff12_RC()) because of the whole dots thing.
//' Built for speed....I hope.
//'
//' @return The feature function, which is a arma unsigned vector.
//'
// [[Rcpp::export]]
NumericVector ff12_C(double ns, NumericVector nss_vec) {

  int nss_dim = nss_vec.length();
  NumericVector indic_vec(nss_dim);
  for(int i=0; i<nss_dim; i++){
    indic_vec[i] = (ns == nss_vec[i]);
  }

  return(indic_vec);

}


//' @title        Very ver stripped down version of ff1_C. No checking, assumes unit weighted states and integer node state names starting at 1.
//' @description Built-in feature function for convenience and testing.
//'
//' @param ns      State label (integer between 1 and nss_dim)
//' @param nss_dim Node state space dimension.
//'
//' @details Very very stripped down version of ff1_C. Built for speed. Assumes state space is
//' integer valued, 1:nss_dim. Probably should use this with an R-side wrapper to sort out other
//' arguments and arbitrary state names. CAUTION! No checks performed. This function can easily
//' segfault.
//'
//' @return The feature function, which is an Rcpp integer vector.
//'
// [[Rcpp::export]]
IntegerVector ff13_C(int ns, int nss_dim) {

  IntegerVector indic_vec(nss_dim, 0);
  indic_vec[ns-1] = 1;

  return(indic_vec);
}
