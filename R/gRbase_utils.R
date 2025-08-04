#' @title Decorate initialized crf-object to make potentials compatible with gRbase
#' @description Decorate initialized crf-object to make potentials and energies compatible with gRbase
#'
#' @param crf      a crf object with the potentials containing something
#' @param includeQ whether or not to write gRbase formatted potentials and energies to the crf object
#'
#' @details Decorate initialized crf-object potentials to make compatible with gRbase functions. Also
#' properly decorated energies (negative log potentials) are generated. If the user desires to include
#' the decorated potentials/energies in the crf object, set includeQ=T (the default). They will be
#' included in a section labeled gR. If includeQ=F the potentials and energies are simply returned
#' as a list.
#'
#' @return See details
#'
#' @examples XX
#'
#' @export
make.gRbase.potentials <- function(crf, includeQ=T){

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

  if(includeQ == T){
    crf$gR <- potential.info
    print("gRbase dressed potentials and energies written to crf object.")
  } else {
    return(potential.info)
  }

}


#' @title Slightly augmented version of gRbase tabNew to be used directly with a crf object
#' @description Slightly augmented version of gRbase tabNew to be used in making new tables directly with a crf object
#'
#' @param node.name.vec node names for the gRbase decorated table
#' @param vals          values to go into gRbase decorated table
#' @param crf           a crf object
#' @param ...           extra arguments to tabNew
#'
#' @details A slightly augmented version of gRbase tabNew which will pull the node state names out
#' of the crf object and set everything up fairly automatically to make a new, properly decorated
#' table that is compatible with gRbase functions. The input values can be in the form of a
#' data frame (e.g. a bunch of configurations), a table which is a contingency table or just a
#' vector of values.
#'
#' @return XX
#'
#' @examples XX
#'
#' @export
tabNew.crf <- function(node.name.vec, vals, crf, ...){

  node.name.idx <- NULL
  for(i in 1:length(node.name.vec)){
    node.name.idx <- c(node.name.idx, which(names(crf$node.state.names) == node.name.vec[i]))
  }
  #print(node.name.idx)

  # Gather node levels out of list in crf object into a sublist
  node.levs <- do.call(list, crf$node.state.names[node.name.idx])

  # Process values for table (This could be done cleaner........)
  if(class(vals) == "data.frame"){
    vals.loc <- as.numeric(do.call(table, vals))
  } else if(class(vals) == "table"){
    vals.loc <- as.numeric(vals)
  } else if(class(vals) == "numeric"){
    vals.loc <- as.numeric(vals)
  } else {
    stop("vals must be entered in as a table, numeric vector or data frame!") # Add matrix and array??
  }

  gRtab <- tabNew(node.name.vec, levels=node.levs, values=as.numeric(vals.loc), ...)
  return(gRtab)

}
