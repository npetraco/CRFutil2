#' Decorate initalized mrf-object to make potentials compatible with gRbase
#'
#' Decorate initalized mrf-object to make potentials compatible with gRbase
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
make.gRbase.potentials <- function(crf){

  node.names <- crf$node.name.tab$name

  # Decorate node potentials:
  gRbase.node.pots <- crf$node.pot.list                       #node Psi's
  gRbase.node.nlps <-log_list(crf$node.pot.list, neglogQ = T) #node psi's (one-body energies or negative log potentials)

  for(i in 1:crf$n.nodes){
    node.levs             <- list(crf$node.state.names[[i]])
    names(node.levs)      <- node.names[i]
    gRbase.node.pots[[i]] <- tabNew(node.names[i], levels=node.levs, values=c( gRbase.node.pots[[i]] ))
    gRbase.node.nlps[[i]] <- tabNew(node.names[i], levels=node.levs, values=c( gRbase.node.nlps[[i]] ))
  }
  #print(gRbase.node.pots)
  #print(gRbase.node.nlps)

  # Decorate edge potentials:
  gRbase.edge.pots <- rep(list(NULL),crf$n.edges) # edge Psi's
  gRbase.edge.nlps <- rep(list(NULL),crf$n.edges) # edge psi's (two-body energies or negative log potentials)
  for(i in 1:crf$n.edges){
    e1                    <- node.names[crf$edges[i,1]]
    e2                    <- node.names[crf$edges[i,2]]
    e1.levs               <- crf$node.state.names[[ crf$edges[i,1] ]]
    e2.levs               <- crf$node.state.names[[ crf$edges[i,2] ]]
    node.levs             <- list(e1.levs, e2.levs)
    names(node.levs)      <- c(e1,e2)
    gRbase.edge.pots[[i]] <- tabNew(c(e1,e2), levels=node.levs, values=as.numeric(crf$edge.pot[[i]]))
    gRbase.edge.nlps[[i]] <- -log(gRbase.edge.pots[[i]])
  }
  #print(gRbase.edge.pots)
  #print("==============")
  #print(gRbase.edge.nlps)

  potential.info        <- list(gRbase.node.pots,
                                gRbase.edge.pots,
                                gRbase.node.nlps,
                                gRbase.edge.nlps)
  names(potential.info) <- c("node.potentials","edge.potentials","node.energies","edge.energies")

  return(potential.info)
}
