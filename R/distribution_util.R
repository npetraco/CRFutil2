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
  #config.probs <- tableMult(prod.edge.pots, prod.node.pots)
  #ZZ <- sum(config.probs)
  #config.probs <- config.probs/ZZ

  # Assume the prod pots can get a little rowdy. Normalize on log scale instead:
  log.config.prod.pots <- log(tableMult(prod.edge.pots, prod.node.pots)) # Take the log so we can use the log-sum-exp trick
  logZZ               <- logsumexp2(log.config.prod.pots)
  log.config.probs     <- log.config.prod.pots - logZZ                    # Comes out in contingency table form
  log.config.probs     <- as.data.frame(as.table(log.config.probs))       # Flatten out into one matrix

  # Un-log from logsumexp trick
  config.probs                     <- log.config.probs
  config.probs[,ncol(config.probs)] <- exp(config.probs[,ncol(config.probs)])

  # Reorder columns in node canonical order contained in crf object if it is given
  if(!is.null(crf)){

    # Order nodes (columns) and configs (rows) in case they aren't:
    csm           <- config.probs[,1:num.nodes]
    csm.ord.info <- order.configs(configs.mat = csm, crf = crf, order.nodesQ=T)

    log.config.probs <- log.config.probs[csm.ord.info$config.rearr.idxs, c(csm.ord.info$node.rearr.idxs, num.nodes+1)]
    config.probs     <- config.probs[csm.ord.info$config.rearr.idxs, c(csm.ord.info$node.rearr.idxs, num.nodes+1)]

    colnames(log.config.probs)[num.nodes+1] <- "log.prob"
    colnames(config.probs)[num.nodes+1]     <- "prob"

  }

  dist.info <- list(config.probs, log.config.probs, logZZ)
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
distribution.from.energies <- function(crf=NULL, node.energies=NULL, edge.energies=NULL, use.gRQ=F){

  # Enumerate all the state configurations and put in canonical order.
  # **CAUTION: This can get huge. Print a message or a warning:
  all.config          <- expand.grid(crf$node.state.names)

  # Put configs in canonical order
  all.config.ord.info <- order.configs(all.config, crf, order.nodesQ=T)
  all.config          <- all.config.ord.info$config.mat

  # Transform to configuration indices needed by efficient energy functions
  all.config.sidxs <- configs.n2i(all.config, crf)

  # Compute energies of each configuration:
  config.energies <- sapply(1:nrow(all.config.sidxs), function(xx){energye(all.config.sidxs[xx,], crf)})
  logZZ          <- logsumexp2(-config.energies)
  log.prob       <- -(config.energies+logZZ)
  prob           <- exp(log.prob)

  config.probs     <- data.frame(all.config, prob)       # Tack on the configurations
  log.config.probs <- data.frame(all.config, log.prob)   # Tack on the configurations
  dist.info        <- list(config.probs, log.config.probs, logZZ)
  names(dist.info) <- c("config.probs", "log.config.probs", "logZ")

  return(dist.info)
}
