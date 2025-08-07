#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace std;

//' @title        Efficient cXXXX
 //' @description Efficient XXXX. No feature function required.
 //'
 //' @param yA   node A state index.
 //' @param yB   node B state index.
 //' @param wAB  omega matrix
 //'
 // [[Rcpp::export]]
 IntegerVector phi_features_e_C(IntegerVector config, IntegerMatrix edges_mat, int num_params, arma::icube node_par, List edge_par) {

   int num_nodes = config.length();
   int num_edges = edges_mat.nrow();
   IntegerVector phi_vec(num_params);


   int phi_idx   = -1;
   for(int i = 1;  i<= num_nodes; i++){
     phi_idx = node_par(i-1,config[i-1] - 1, 0);
     //Rcout << "phi index for node " << i << " " << phi_idx << endl;

     if(phi_idx != 0) {
       phi_vec[phi_idx - 1] = 1; // ** NOTE: Assumes parameters have unique consecutive indices!
     }
   }

   for(int i = 1; i<=num_edges; i++){

     // **NOTE: Assumes parameters have unique consecutive indices!
     // Check and see if we reached the end of phi. No point in doing the rest of the edges if we did:
     if(phi_idx == num_params) {
       break;
     }

     arma::icube edge_i_par = edge_par(i-1);
     // Rcout << "edge: " << i << endl;
     // Rcout << "node1: " << edges_mat(i-1,0) << endl;
     // Rcout << "node2: " << edges_mat(i-1,1) << endl;
     // Rcout << edge_i_par << endl;

     phi_idx = edge_i_par(config[edges_mat(i-1,0) -1] - 1, config[edges_mat(i-1,1) -1] - 1, 0);
     // Rcout << "phi index for edge " << i << " " << phi_idx << endl;
     // Rcout << "==============" << endl;

     if(phi_idx != 0) {
       phi_vec[phi_idx - 1] = 1; // ** NOTE: Assumes parameters have unique consecutive indices!
     }

   }

   return(phi_vec);

 }
