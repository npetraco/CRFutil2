#' @title       Compute features phi
#' @description Utility function to compute unconditional features phi_i(config)
#'
#' @param config    configuration
#' @param edges.mat an edge index map for a graph
#' @param node.par  node parameter index matrix for a graph
#' @param edge.par  list of edge parameter index matries for a graph
#' @param ff        feature function
#' @param ...       optional arguments for feature function
#'
#' @details Assumes feature components phi_i(X), are 0,1 valued and parameters are numbered. Any
#' names for the states of the configuration are allowed so long as they are consistent with the
#' feature functions. This version is slower however.
#'
#' @return a phi vector
#'
#' @examples XXXX
#'
#' @export
phi.features <- function(config, edges.mat, node.par, edge.par, ff, ...) {  # **** NEEDS TO BE C

  num.nodes  <- length(config)
  num.edges  <- nrow(edges.mat)
  num.params <- max(unlist(list(node.par, edge.par)))
  phi.vec    <- numeric(num.params)

  phi.idx <- 0

  # Nodes: \phi_i({\bf X}) = 1-\delta_{{\bf f}^{\dagger}(X_i) {\boldsymbol \tau}_i, 0}
  for(i in 1:num.nodes){
    phi.idx <- as.numeric(ff(config[i], ...) %*% node.par[i,,1])
    #print(paste("phi index for node ", i, " ", phi.idx))
    if(phi.idx != 0) {
      phi.vec[phi.idx] <- 1 # Don't do this???? Assumes parameters have unique consecutive indices... Causing SUBTLE BUG?????
      #phi.vec[i] <- 1 #???????????
    }
    #print("phi.vec")
    #print(phi.vec)
  }

  # Edges: \phi_{k_{[ij]}}({\bf X}) = 1-\delta_{{\bf f}^{\dagger}(X_i) {\boldsymbol \omega}_{ij} {\bf f}(X_j) , 0}
  for(i in 1:num.edges) {

    # Don't do this?? Assumes parameters have unique consecutive indices...
    # # Check and see if we reached the end of phi. No point in doing the rest of the edges if we did:
    if(phi.idx == num.params) {
      break()
    }

    phi.idx <- as.numeric(ff(config[edges.mat[i,1]], ...) %*% edge.par[[i]][,,1] %*% ff(config[edges.mat[i,2]], ...))
    #print(paste("phi index for edge ", i, " ", phi.idx))
    if(phi.idx != 0) {
      phi.vec[phi.idx] <- 1
    }
  }

  return(phi.vec)
}


#' @title       Compute features phi faster
#' @description Utility function to compute unconditional features phi_i(config)
#'
#' @param config a configuration
#' @param crf    a crf object
#'
#' @details Assumes feature components phi_i(X), are 0,1 valued and parameters are contiguous and
#' consecutively numbered. More importantly, this version assume state of the configuration are
#' node state indices. That's how we avoid the slower matrix multiplies and replace them with
#' lookups.
#'
#' @return a phi vector
#'
#' @examples XXXX
#'
#' @export
phi.features.e <- function(config, crf) {

  num.nodes  <- crf$n.nodes
  num.edges  <- crf$n.edges
  num.params <- crf$num.par
  phi.vec    <- numeric(num.params)

  phi.idx <- 0

  # Nodes: \phi_i({\bf X}) = 1-\delta_{{\bf f}^{\dagger}(X_i) {\boldsymbol \tau}_i, 0}
  for(i in 1:num.nodes){
    phi.idx <- crf$node.par[i,config[i],1]
    #print(paste("phi index for node ", i, " ", phi.idx)) # comment out later
    if(phi.idx != 0) {
      phi.vec[phi.idx] <- 1 # ** NOTE: Assumes parameters have unique consecutive indices!
    }
    #print("phi.vec:") # comment out later
    #print(phi.vec)    # comment out later
  }

  # Edges: \phi_{k_{[ij]}}({\bf X}) = 1-\delta_{{\bf f}^{\dagger}(X_i) {\boldsymbol \omega}_{ij} {\bf f}(X_j) , 0}
  for(i in 1:num.edges) {

    # **NOTE: Assumes parameters have unique consecutive indices!
    # Check and see if we reached the end of phi. No point in doing the rest of the edges if we did:
    if(phi.idx == num.params) {
      break()
    }

    phi.idx <- crf$edge.par[[i]][config[crf$edges[i,1]],config[crf$edges[i,2]],1]
    #print(paste("phi index for edge ", i, " ", phi.idx)) # keep but comment out
    if(phi.idx != 0) {
      phi.vec[phi.idx] <- 1
    }
  }

  return(phi.vec)
}


#' @title       Compute model matrix for a set of configurations
#' @description Compute model matrix for a set of configurations
#'
#' @param configs.mat a set of configurations
#' @param crf         crf object
#'
#' @details This version assumes configurations are given in terms of node state indices.
#'
#' @return a MRF model matrix
#'
#' @examples XXXX
#'
#' @export
compute.model.matrix.e <- function(configs.mat, crf) {

  M.mat <- t(sapply(1:nrow(configs.mat), function(xx){phi.features.e(configs.mat[xx,], crf = crf)}))
  #e.vec <- M.mat %*% crf$par

  return(M.mat)

}
