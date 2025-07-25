#' Compute the joint distribution from the node and edge potentials using gRbase functions
#'
#' The function will XXXX
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will state probalities in gRbase table form as well as logZ
#'
#'
#' @export
distribution.from.potentials <- function(crf=NULL, gRbase.node.potentials=NULL, gRbase.edge.potentials=NULL){

  if(!is.null(crf)){ # Get the info out of the crf object
    num.nodes <- crf$n.nodes
    num.edges <- crf$n.edges
    if(is.null(crf$gR)){
      stop("A section labeled gR must be contained in the crf object with node.potentials and edge.potentials decorated with gRbase symbols. Run make.gRbase.potentials() and store the results in crf$gR!")
    } else {
      node.pots <- crf$gR$node.potentials
      edge.pots <- crf$gR$edge.potentials
    }
  } else if(is.null(gRbase.node.potentials) | is.null(gRbase.edge.potentials)) {
    stop("If no crf object is provided both gRbase.node.potentials and gRbase.edge.potentials must be input!")
  } else { # ....otherwise get the info from the input potentials
    num.nodes <- length(gRbase.node.potentials)
    num.edges <- length(gRbase.edge.potentials)
    node.pots <- gRbase.node.potentials
    edge.pots <- gRbase.edge.potentials
  }

  prod.node.pots <- tableMult(node.pots[[2]], node.pots[[1]])
  if(num.nodes > 2){
    for(i in 3:num.nodes){
      prod.node.pots <- tableMult(prod.node.pots, node.pots[[i]])
    }
  }

  if(num.edges > 2){
    prod.edge.pots <- tableMult(edge.pots[[2]], edge.pots[[1]])
    for(i in 3:num.edges){
      prod.edge.pots <- tableMult(prod.edge.pots, edge.pots[[i]])
    }
  } else { # For only two nodes there is only one edge
    prod.edge.pots <- edge.pots[[1]]
  }

  # Direct normalization:
  #state.probs <- tableMult(prod.edge.pots, prod.node.pots)
  #ZZ <- sum(state.probs)
  #state.probs <- state.probs/ZZ

  # Assume the prod pots can get a little rowdy. Normalize on log scale instead:
  log.state.prod.pots <- log(tableMult(prod.edge.pots, prod.node.pots)) # Take the log so we can use the log-sum-exp trick
  logZZ               <- logsumexp2(log.state.prod.pots)
  log.state.probs     <- log.state.prod.pots - logZZ                    # Comes out in contingency table form
  log.state.probs     <- as.data.frame(as.table(log.state.probs))       # Flatten out into one matrix

  # Un-log from logsumexp trick
  state.probs                     <- log.state.probs
  state.probs[,ncol(state.probs)] <- exp(state.probs[,ncol(state.probs)])

  # Reorder columns in node canonical order contained in crf object if it is given
  if(!is.null(crf)){

    # Order nodes (columns) and configs (rows) in case they aren't:
    csm           <- state.probs[,1:num.nodes]
    csm.ord.info <- order.configs(configs.mat = csm, crf = crf, order.nodesQ=T)

    log.state.probs <- log.state.probs[csm.ord.info$config.rearr.idxs, c(csm.ord.info$node.rearr.idxs, num.nodes+1)]
    state.probs     <- state.probs[csm.ord.info$config.rearr.idxs, c(csm.ord.info$node.rearr.idxs, num.nodes+1)]

    # print(num.nodes)
    # print(colnames(log.state.probs))
    # print(ncol(log.state.probs))
    # print("")
    # print(colnames(state.probs))
    # print(ncol(state.probs))

    colnames(log.state.probs)[num.nodes+1] <- "log.prob"
    colnames(state.probs)[num.nodes+1]     <- "prob"

    #print("HERE!!")
  }

  dist.info <- list(state.probs, log.state.probs, logZZ)
  names(dist.info) <- c("config.probs", "log.config.probs", "logZ")
  return(dist.info)

}


#' Compute the joint distribution from the node and edge energies
#'
#' The function will XXXX
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
# distribution.from.energies <- function(state.space, edges.mat, node.energies, edge.energies){
#
#   num.states      <- nrow(state.space)
#   state.energies  <- sapply(1:num.states, function(xx){config.energy.e(state.space[xx,], edges.mat, node.energies, edge.energies)})
#   logZZ           <- logsumexp2(-state.energies)
#   log.state.probs <- state.energies-logZZ
#
#   dist.info <- list(exp(-log.state.probs), logZZ)
#   names(dist.info) <- c("state.probs", "logZ")
#   return(dist.info)
#
# }
