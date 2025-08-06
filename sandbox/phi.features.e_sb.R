phi.features.e_sb <- function(config, crf) {

  num.nodes  <- crf$n.nodes
  num.edges  <- crf$n.edges
  #num.params <- max(unlist(list(crf$node.par, crf$edge.par)))
  num.params <- crf$num.par
  phi.vec    <- numeric(num.params)

  phi.idx <- 0

  # Nodes: \phi_i({\bf X}) = 1-\delta_{{\bf f}^{\dagger}(X_i) {\boldsymbol \tau}_i, 0}
  for(i in 1:num.nodes){
    #phi.idx <- as.numeric(ff(config[i]) %*% node.par[i,,1])
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

    #phi.idx <- as.numeric(ff(config[edges.mat[i,1]]) %*% edge.par[[i]][,,1] %*% ff(config[edges.mat[i,2]]))
    phi.idx <- crf$edge.par[[i]][config[crf$edges[i,1]],config[crf$edges[i,2]],1]
    #print(paste("phi index for edge ", i, " ", phi.idx)) # keep but comment out
    if(phi.idx != 0) {
      phi.vec[phi.idx] <- 1
    }
  }

  return(phi.vec)
}
