#' @title Easily update the CRFutil2 library by installing the current version from the github site
#' @description Easily update the CRFutil2 library by installing the current version from the github site
#'
#' @param XX  XX
#'
#' @details Easily update the CRFutil2 library by installing the current version from the github site
#'
#' @return The function will XX
#'
#' @examples XX
#'
#' @export
update_CRFutil2 <- function() {
  print("Updating CRFutil2")
  remotes::install_github("npetraco/CRFutil2")
  print("Done!!")
}



#' @title logsumexp trick
#' @description Log sum exp trick. Code ported from Brendon Brewer's DNest code
#'
#' @param XX XX
#'
#' @details The function will XXXX
#'
#' @return The function will XX
#'
#' @export
logsumexp <- function(logv) {
  n <- length(logv)
  max.logv <- max(logv)

  answer <- 0

  for(i in 1:n){
    answer <- answer + exp(logv[i] - max.logv)
  }
  answer <- max.logv + log(answer);

  return(answer)

}


#' @title logsumexp trick
#' @description Log sum exp trick also. A less readable but more compact version:
#'
#' @param XX XX
#'
#' @details The function will XXXX
#'
#' @return The function will XX
#'
#' @export
logsumexp2 <- function(logv)
{
  n <- length(logv)
  max.logv <- max(logv)

  answer <-  max.logv + log(cumsum(c(0,exp(logv - max.logv)))[n+1])

  return(answer)

}


#' @title Take the log of stuff (vectors, matrices, etc) in a list
#' @description XX
#'
#' @param XX XX
#'
#' @details Handy for going from node/edge potentials in list form to node/edge energies (log potentials)
#'
#' @return The function will XX
#'
#' @export
log_list <- function(a.list.for.logging, neglogQ=T) {

  if(neglogQ==T) {
    a.logged.list <- lapply(1:length(a.list.for.logging), function(xx){-log(a.list.for.logging[[xx]])})
  } else {
    a.logged.list <- lapply(1:length(a.list.for.logging), function(xx){log(a.list.for.logging[[xx]])})
  }


  if(!is.null(names(a.list.for.logging))) {
    names(a.logged.list) <- names(a.list.for.logging)
  }

  return(a.logged.list)

}


#' Exp a bunch of stuff (vectors, matrices, etc) in a list
#'
#' Handy for going from node/edge energies (-log potentials) in list to node/edge potentials
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
exp_list <- function(a.list.for.eing, negexpQ=F) {

  if(negexp==T){
    an.exp.list <- lapply(1:length(a.list.for.eing), function(xx){exp(-a.list.for.eing[[xx]])})
  } else {
    an.exp.list <- lapply(1:length(a.list.for.eing), function(xx){exp(a.list.for.eing[[xx]])})
  }

  if(!is.null(names(a.list.for.eing))) {
    names(an.exp.list) <- names(a.list.for.eing)
  }

  return(an.exp.list)

}


#' @title row.match in a matrix or table
#' @description Code from prodlim library to match a row in a matrix
#'
#' @param XX
#'
#' @details The function will XXXX
#'
#' @return The function will XX
#'
#' @export
row.match <- function (x, table, nomatch = NA)   # **********NEEDS TO BE C
{
  #if (class(table) == "matrix")
  if ("matrix" %in% class(table)) # Mod needed because class now returns all classes of object and that causes a bug. 10-15-23
    table <- as.data.frame(table)
  if (is.null(dim(x)))
    x <- as.data.frame(matrix(x, nrow = 1))
  cx <- do.call("paste", c(x[, , drop = FALSE], sep = "\r"))
  ct <- do.call("paste", c(table[, , drop = FALSE], sep = "\r"))
  match(cx, ct, nomatch = nomatch)
}


#' @title Name(s) to index(ies)
#' @description Convenience function to convert a character name to an index
#'
#' @param name.vec      One or more name elements. Usually will be state character names.
#' @param ordered.names All possible unique names in a specified order. Usually will be the names of states a node can assume.
#'
#' @details Function intended to convert a node state name to a state index for use
#' with the more efficient energy functions config.energy.e, config_energy_eC, energye
#' and energyeC. Function should also work to convert node name to node index if
#' needed. Note however that conversion is in the crf object element node.name.tab.
#'
#' @return The index of the names in name.vec
#'
#' @export
n2i <- function(name.vec, ordered.names){
  idx.vec <- sapply(1:length(name.vec), function(xx){which(ordered.names == name.vec[xx])})
  return(idx.vec)
}


#' @title Convert name(s) to index(ies) for a matrix of configurations
#' @description Convert name(s) to index(ies) for a matrix of configurations
#'
#' @param configs.mat   A matrix of configurations
#' @param crf           A crf object
#' @param node.col.lblQ Whether or not to label columns with node names
#'
#' @details Function converts node state names in configurations to node state indices.
#' The biggest reason to do this is that the more efficient energy functions can only use
#' node state indices. The inverse of this function is dress.sample().
#'
#' @return Configuration matrix in terms of node state indices
#'
#' @export
configs.n2i <- function(configs.mat, crf, node.col.lblQ=T){

  configs.idxs.mat <- sapply(1:crf$n.nodes, function(xx){n2i(configs.mat[,xx], crf$node.state.names[[xx]])})
  if(node.col.lblQ == T) {
    colnames(configs.idxs.mat) <- crf$node.name.tab$name
  }
  return(configs.idxs.mat)
}


#' @title       Dress a sample with node and state names in crf object
#' @description Dress a sample or matrix of configurations with node and state names in crf object
#'
#' @param crf     A crf object which also contains node and state names (i.e. out augmented crf object)
#' @param samples A set of configurations with state indices as names. Typically these will be a samples generated by one of the CRF sampling functions.
#'
#' @details NOTE: The function takes a configuration matrix of configurations with state
#' indices as names. The function takes this "index" configuration matrix and dresses it up with
#' node names as the columns and state names for the configuration outcome elements (the name info
#' is contained in the crf object). The sampling functions from CRF (e.g. sample.junction()) just
#' spit out an undressed matrix of samples with sample state numbers (e.g. 1, 2, ....) for the
#' configuration elements. This is needed sometimes and often more convenient, but for display
#' purposes the user may want more labels on the sample space. Hence this (cosmetic) function. The
#' inverse of this function is configs.n2i().
#'
#' @return The sample dressed with node and state names contained in the crf object.
#'
#' @examples XXXX
#'
#' @export
dress.sample <- function(crf, samples) {

  # Node names
  nod.nms <- crf$node.name.tab$name # **NOTE: columns of samples SHOULD be in the same order

  # Initialize the sample matrix that will have the node and state names:
  samples.dressed           <- samples # The state names in each column are converted in the loop below
  colnames(samples.dressed) <- nod.nms

  # Node names in the state label LIST. Get this in case the order is different than in nod.nms,
  # i.e. if the order of crf$node.name.tab$name and crf$node.state.names do not match for
  # some reason.
  nod.nms.st.list <- names(crf$node.state.names)

  # Loop over the nodes, which should be the columns of the sample.
  # **NOTE: this assumes the column numbers correspond the the order of the names in nod.nms
  for(i in 1:length(nod.nms)){
    nod.idx    <- which(nod.nms.st.list == nod.nms[i])
    nod.st.nms <- crf$node.state.names[[nod.idx]] # Should be the names of the states of the node
    #print(nod.st.nms)
    samples.dressed[,i] <- nod.st.nms[samples[,i]] # **NOTE: this assumes the state labels in samples are state indices, i.e. the sample was generated by one of the CRF sampling functions like sample.junction()
  }

  return(samples.dressed)

}


#' @title Order configurations given node state indices
#' @description Give a canonical order to a matrix of configurations via the node order (columns) using order()
#'
#' @param configs.mat   A matrix of configurations. See details section about state index names!
#' @param crf           A crf object
#' @param st.idxQ       Whether or not to change state names in configurations to state indices.
#' @param order.nodesQ  Whether or not to order the columns (i.e. nodes) in the same order as is found in the crf object.
#'
#' @details Orders the configurations column by column in node state order using the R base
#' function order. NOTE: This function requires node states to be in index form! If they are not,
#' set st.idxQ=T (By the way, that's the default.). This choice has been left as a manual switch
#' and not made an automated check to allow for the possibility of numeric state labels that are
#' NOT in order. Given the states of each node are assigned indices in the crf object, the
#' function treats each configuration an integer vector and orders them node-wise
#' (i.e. column-wise). This is handy when comparing distributions with the same configurations and
#' nodes. With order.nodesQ=T (the default) the columns will be first ordered according to whats
#' in crf$node.name.tab and then the configurations (rows) will be ordered. If order.nodesQ=F the
#' column rearrangement indices be returned as NULL. If there are no node names (i.e. no column)
#' labels and the states are state indices (i.e. if the configurations are just output from one of
#' the CRF sampling functions), set st.idxQ = F and order.nodesQ = F, otherwise an error will be
#' thrown.
#'
#' @return A list containing canonically ordered configuration matrix in terms of node state
#' indices and the re-order indices themselves.
#'
#' @export
order.configs <- function(configs.mat, crf, st.idxQ=T, order.nodesQ=T){

  configs.mat.ord <- configs.mat

  #First make sure configs.mat is in state index form using the ordering contained in crf$node.state.names
  # **NOTE: this is not automated to keep code bloat down. Maybe automate in the future however. Cf. details section.
  if(st.idxQ == T){
    configs.mat.ord <- data.frame(configs.n2i(configs.mat.ord, crf = crf, node.col.lblQ = T))
  }

  col.rearr.idxs <- NULL
  # Order nodes (columns) first if requested.
  if(order.nodesQ==T) {

    # initial node name column headers
    nnch <- colnames(configs.mat)

    # Rearrange column (node) indices to be in canonical node order contained in the crf object, i.e. the order in crf$node.name.tab:
    col.rearr.idxs  <- sapply(1:crf$n.nodes, function(xx){n2i(name.vec = crf$node.name.tab$name[xx], ordered.names = nnch)})
    configs.mat.ord <- configs.mat.ord[,col.rearr.idxs]
  }

  # Order configurations: **NOTE states of configurations must be index form for this to work right!
  row.rearr.idxs  <- do.call(order, configs.mat.ord[,1:ncol(configs.mat.ord)])
  configs.mat.ord <- configs.mat.ord[row.rearr.idxs,]
  rownames(configs.mat.ord) <- NULL # Re-set row indices

  # Reset state index names to their actual names here:
   if(st.idxQ == T){
     configs.mat.ord <- data.frame(dress.sample(crf = crf, samples = configs.mat.ord))
   }

  # Package up:
  configs.ord.info <- list(configs.mat.ord, row.rearr.idxs, col.rearr.idxs)
  names(configs.ord.info) <- c("config.mat", "config.rearr.idxs", "node.rearr.idxs")

  return(configs.ord.info)

}
