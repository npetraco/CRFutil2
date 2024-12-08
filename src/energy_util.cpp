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


//' @title       Efficient one-body (node) energy function
//' @description Efficient one-body (node) energy function. No feature function required
//'
//' @param yA  a node state index
//' @param tA  tau vector
//'
//' @details One-body (node) energy function
//'
//' @return One-body (node) energy.
//'
//' @examples
 //'
 // [[Rcpp::export]]
 double Eone_e_C(int yA, NumericVector tA) {
   return(tA[yA-1]);
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


//' @title       Efficient two-body (edge) energy function
//' @description Efficient two-body (edge) energy function. No feature function required.
//'
//' @param yA   node A state index.
//' @param yB   node B state index.
//' @param wAB  omega matrix
//'
// [[Rcpp::export]]
 double Etwo_e_C(int yA, int yB, NumericMatrix wAB) {
   return( wAB(yA-1, yB-1) );
 }


//' @title       Efficient configuration energy function
//' @description Efficient configuration energy function. Uses above efficient one and two
//' body energy functions. No feature function required.
//'
//' @param yA   node A state index.
//' @param yB   node B state index.
//' @param wAB  omega matrix
//'
// [[Rcpp::export]]
double config_energy_e_C(IntegerVector config, IntegerMatrix edges_mat, List one_nlp, List two_nlp) {

  int num_nodes = config.length();
  int num_edges = edges_mat.nrow();
  //Rcout << num_edges << endl;

  // Sum One-body energies (log node-potentials)
  double e_one = 0.0;
  for(int i = 1;  i<= num_nodes; i++){
    //Rcout << Eone_e_C(config[i], one_nlp[i]) << endl;
    e_one += Eone_e_C(config[i-1], one_nlp[i-1]);
    //Rcout << e_one << endl;
  }

  // Sum Two-body energies (log edge-potentials)
  double e_two = 0.0;
  for(int i = 1; i<=num_edges; i++){
    //Rcout << "Edge: " << i << endl;
    //Rcout << "A:" << edges_mat(i-1,0) << " spin: " << config[edges_mat(i-1,0) - 1] << endl;
    //Rcout << "B:" << edges_mat(i-1,1) << " spin: " << config[edges_mat(i-1,1) - 1] << endl;

    //arma::mat ww = two_nlp[i-1];
    //int ii = config[edges_mat(i-1,0) - 1];
    //int jj = config[edges_mat(i-1,1) - 1];
    //Rcout << ww(ii, jj) << endl;
    //Rcout << as<arma::mat>(two_nlp[i-1]) << endl;

    //Rcout << Etwo_e_C(config[edges_mat(i-1,0) - 1], config[edges_mat(i-1,1) - 1], two_nlp[i-1]) << endl;
    //Rcout << "----" << endl;
    e_two += Etwo_e_C(config[edges_mat(i-1,0) - 1], config[edges_mat(i-1,1) -1], two_nlp[i-1]);
  }

  return(e_one + e_two);

 }
