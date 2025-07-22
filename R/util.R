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
#' node state indices.
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


#' @title Order configurations given node state indices
#' @description Give a canonical order to a matrix of configurations via the node order (columns) using order()
#'
#' @param configs.mat   A matrix of configurations
#' @param crf           A crf object
#' @param order.nodesQ  Whether or not to order the columns (i.e. nodes) in the same order as is found in the crf object.
#'
#' @details Orders the configurations column by column in node state order using the R base
#' function order. Given the states of each node are assigned indices in the crf object, the
#' function treats each configuration an integer vector and orders them node-wise
#' (i.e. column-wise). This is handy when comparing distributions with the same configurations and
#' nodes. By default it's assumed the configurations are sent in already in the column order you
#' want (order.nodesQ=F). With order.nodesQ=T the columns will be first ordered according to whats
#' in crf$node.name.tab and then the configurations (rows) will be ordered. If order.nodesQ=T the
#' column rearrangment indices will also be returned.
#'
#' @return A list containing canonically ordered configuration matrix in terms of node state
#' indices and the re-order indices themselves.
#'
#' @export
order.configs <- function(configs.mat, crf, order.nodesQ=F){

  configs.mat.ord <- configs.mat

  col.rearr.idxs <- NULL
  if(order.nodesQ==T) {

    # initial node name column headers
    nnch <- colnames(configs.mat)[1:(ncol(configs.mat)-1)]

    # Rearrange column (node) indices to be in canonical node order contained in the crf object, i.e. the order in crf$node.name.tab:
    col.rearr.idxs  <- sapply(1:crf$n.nodes, function(xx){n2i(name.vec = crf$node.name.tab$name[xx], ordered.names = nnch)})
    configs.mat.ord <- configs.mat.ord[,col.rearr.idxs]
  }

  row.rearr.idxs  <- do.call(order, configs.mat.ord[,1:ncol(configs.mat.ord)])
  configs.mat.ord <- configs.mat.ord[row.rearr.idxs,]
  rownames(configs.mat.ord) <- NULL # Re-set row indices

  configs.ord.info <- list(configs.mat.ord, row.rearr.idxs, col.rearr.idxs)
  names(configs.ord.info) <- c("config.mat", "config.rearr.idxs", "node.rearr.idxs")

  return(configs.ord.info)

}
