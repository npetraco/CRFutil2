#' @title       Instantiate an empty field
#' @description Instantiate an empty field. Should work for Ising and Potts-like models.
#'
#' @param graph.eq XX
#' @param adj.mat XX
#' @param num.states Scalar or vector specifying the number of states per node.
#'                   If a scalar, all nodes assumed to have that number of states.
#'                   Minimum magnitude is 2 states per node (Ising-like).
#' @param parameterization.typ Type of parameterization for the potentials. Can be general, standard, flexible.
#' @param node.par XX
#' @param edge.par XX
#' @param plotQ XX
#'
#' @details Instantiates an empty field. Should work for Ising and Potts-type models under all
#' parameterizations. Potentials are not filled in however. Use insert.params.and.pots() plunck in
#' generated random parameters/potentials. Or you can add in your own later. NOTE: this assumes only
#' one and two-body potentials (n.nf=1, n.ef=1, cf. make.features()).
#'
#' @return A crf object.
#'
#' @examples XXXX
#'
#' @export
make.empty.field <- function(graph.eq=NULL, adj.mat=NULL, num.states = 2, state.names = NULL, parameterization.typ="standard", node.par=NULL, edge.par=NULL, plotQ=FALSE) {

  if(is.null(graph.eq) & is.null(adj.mat)){
    stop("Specify a model!")
  }

  if(!is.null(graph.eq) & !is.null(adj.mat)){
    stop("Specify model as either a graph eq OR an adjacency matrix.")
  }

  # Make empty crf object
  if(!is.null(graph.eq)){
    adjm <- ug(graph.eq, result="matrix")
  } else {
    adjm <- adj.mat
  }
  new.crf         <- make.crf(adjm, num.states)
  new.crf         <- make.features(new.crf)
  new.crf$adj.mat <- adjm # Tack on adjacency matrix in case it's ever needed.

  # Node name translation table:
  node.names              <- colnames(adjm)
  node.name.tab           <- data.frame(1:length(node.names), node.names)
  colnames(node.name.tab) <- c("idx", "name")
  new.crf$node.name.tab   <- node.name.tab

  # Node state space:
  if(length(num.states) == 1) {                        # If only 1 number is input to specify the number of states per node, assumes all nodes have that number of states.
    num.states.loc <- rep(num.states, new.crf$n.nodes) # Should be the same as new.crf$n.states
  } else {                                             # Variable number of states per node
    num.states.loc <- num.states                       # Should also be the same as new.crf$n.states
  }

  # State names for each node:
  node.state.names <- rep(list(NULL), new.crf$n.nodes)
  if(is.null(state.names)){
    for(i in 1:new.crf$n.nodes){
      node.state.names[[i]] <- 1:new.crf$n.states[i] # If no node names are given, just make them contiguous positive integers
    }
  } else {
    if(class(state.names) != "list") {
      stop("state.names must be NULL or a list")
    }
    if(length(state.names) != new.crf$n.nodes) {
      stop("Length of state.names list must be equal to the number of nodes!")
    }

    for(i in 1:new.crf$n.nodes){
      node.state.names[[i]] <- state.names[[i]]
    }

  }
  names(node.state.names)  <- new.crf$node.name.tab$name
  new.crf$node.state.names <- node.state.names


  # Alternative list storage for node info. This version doesn't have to hold on to all the NAs for node state spaces of different sizes.
  new.crf$node.pot.list <- rep(list(NULL), new.crf$n.nodes)
  new.crf$node.par.list <- rep(list(NULL), new.crf$n.nodes)

  # Node parameterization types:
  if(parameterization.typ %in% c("general", "standard", "flexible")) {
    # Parameters per node = num node states - 1
    param.count <- 1
    for(i in 1:new.crf$n.nodes){
      a.tau.vec                           <- rep(NA, new.crf$max.state) # NAs are mean those parameters don't appear for the node because they are not required as per the node's state space. Eg, of the node state space dimention is 2 there is no need for a third parameter.
      a.tau.param.idxs                    <- param.count:(param.count+num.states.loc[i] - 1)
      a.tau.param.idxs[num.states.loc[i]] <- 0                      # The last parameter for the state space of the node is redundant, so set the index to 0.
      param.count                         <- max(a.tau.param.idxs) + 1
      a.tau.vec[1:num.states.loc[i]]      <- a.tau.param.idxs
      new.crf$node.par[i,,1]              <- a.tau.vec

      # Initialize node potentials to necessary number of 1s. NAs go where there is no node parameter.
      a.init.tau.pot                          <- rep(1, num.states.loc[i])
      new.crf$node.pot[i,]                    <- rep(NA, new.crf$max.state)
      new.crf$node.pot[i,1:num.states.loc[i]] <- a.init.tau.pot

      # Alternative storage for node pots and pars:
      new.crf$node.par.list[[i]] <- a.tau.param.idxs
      new.crf$node.pot.list[[i]] <- a.init.tau.pot

    }

  } else {
    stop("No other node parameterization types are availible at this point.")
  }


  # Edge parameterization types:
  if(parameterization.typ == "general") {

    # General edge parameterization:
    # Parameters per edge = num rows * num cols - 1
    for(i in 1:new.crf$n.edges){
      #print(paste("Edge#:", i))
      num.pars.this.edge                            <- prod(dim(new.crf$edge.par[[i]][,,1])) # Its actually 1 minus this but we correct at end.
      this.edge.par.mat                             <- t(array(param.count:(param.count + num.pars.this.edge - 1), rev(dim(new.crf$edge.par[[i]][,,1])))) # parameter indices
      this.edge.par.mat[nrow(this.edge.par.mat), ]  <- this.edge.par.mat[nrow(this.edge.par.mat), ] - 1 # Fix last row
      this.edge.par.mat[nrow(this.edge.par.mat), 1] <- 0                                                # Take bottom left corner to be the reference parameter
      param.count                                   <- max(this.edge.par.mat) + 1                       # Increment for the next parameter matrix
      new.crf$edge.par[[i]][,,1]                    <- this.edge.par.mat
    }

  } else if(parameterization.typ %in% c("standard", "flexible")) {

    param.count <- max(new.crf$node.par, na.rm = T) # Get current number of node parameters as reference to start
    for(i in 1:new.crf$n.edges){
      edge.par.i.loc             <- new.crf$edge.par[[i]][,,1]
      edge.par.i.loc             <- edge.param.indices.helper(nr=nrow(edge.par.i.loc), nc=ncol(edge.par.i.loc), pmax.idx = param.count+1, diagonal.elements=parameterization.typ)
      param.count                <- max(edge.par.i.loc)
      new.crf$edge.par[[i]][,,1] <- edge.par.i.loc
    }

  } else {
    stop("No other node parameterization types are availible at this point.")
  }


  # XXXXX PUT ISING MODELS HERE XXXXXXX


  # Gather all unique parameter indices. Drop any 0s as they are just placeholders:
  par.idxs <- sort(unique(unlist(c(new.crf$node.par.list, new.crf$edge.par))))[-1]
  num.par  <- max(par.idxs)

  par.idxs.contiguousQ <- ((1:num.par) == par.idxs) # Check that the parameter indices are contiuous and that we didn't skip any
  if( sum(par.idxs.contiguousQ) != num.par ){
    param.idx.err.mat           <- cbind(1:num.par, par.idxs)
    colnames(param.idx.err.mat) <- c("number", "parameter index")
    stop("Something isn't right. Parameter indices are not contiguous.......")
  }
  new.crf$num.par       <- num.par
  new.crf$node.par.idxs <- sort(unique(unlist( new.crf$node.par.list )))[-1]
  new.crf$edge.par.idxs <- sort(unique(unlist( new.crf$edge.par )))[-1]


  # Plot graph
  if(plotQ==TRUE){
    new.crf.gp <- graph_from_adjacency_matrix(new.crf$adj.mat, mode = "undirected")
    if(!is.null(dev.list())){
      dev.off()
    }
    iplot(new.crf.gp)
  }

  return(new.crf)

}


#' @title       Input a parameter vector into parameter and potential matrices and vectors of crf object
#' @description XX
#'
#' @param crf a crf object
#' @param param.samp a sample of parameter space or parameter vector
#'
#' @details Input a parameter vector into parameter and potential matrices of crf object
#'
#' @return Nothing.
#'
#' @examples XXXX
#'
#' @export
insert.params.and.pots <- function(crf, params.samp) {

  # Insert node potentials
  for(i in 1:crf$n.nodes){
    node.par.idx <- crf$node.par.list[[i]]
    node.pot     <- crf$node.pot.list[[i]]
    for(j in 1:length(node.par.idx)){
      if(node.par.idx[j] != 0){
        node.pot[j] <- exp( -params.samp[ node.par.idx[j] ]) # pot = exp(-param)
      }
    }
    crf$node.pot.list[[i]]             <- node.pot
    crf$node.pot[i,1:length(node.pot)] <- node.pot
  }

  # Insert edge potentials
  for(i in 1:crf$n.edges){
    edge.par <- crf$edge.par[[i]][,,1]
    edge.pot <- crf$edge.pot[[i]]

    for(j in 1:nrow(edge.par)) {
      for(k in 1:ncol(edge.par)) {
        if(edge.par[j,k] !=0) {
          edge.pot[j,k] <- exp( -params.samp[ edge.par[j,k] ] ) # pot = exp(-param)
        }
      }
    }

    crf$edge.pot[[i]] <- edge.pot

  }

  # Insert parameters
  crf$par <- params.samp

  print("Parameters and potentials inserted into crf object.")

}


#' @title       Print out all contents of a crf object
#' @description Print out all contents of a crf object
#'
#' @param crf A crf object
#'
#' @details The function dumps out the contents of a crf object
#'
#' @return Nothing.
#'
#' @examples XX
#'
#' @export
dump.crf <- function(crf){

  crf.attrib.nms <- names(crf)

  for(i in 1:length(crf.attrib.nms)){
    print("----------------")
    print(crf.attrib.nms[i])
    print(crf[[crf.attrib.nms[i]]])
  }

}


#' @title  Decorate initalized potentials in crf object to be compatible with gRbase
#' @description XX
#'
#' @param crf a crf object
#'
#' @details Decorates the node and edge potentials with naming conventions useful
#' with or required by gRbase functionality.
#'
#' @return A list with the decorated potentials and their corresponding energies.
#'
#' @examples XXXX
#'
#' @export
make.gRbase.potentials <- function(crf){

  # Decorate node potentials:
  gRbase.node.potentials      <- rep(list(NULL),crf$n.nodes) #node Psi's
  gRbase.node.nlog.potentials <- rep(list(NULL),crf$n.nodes) #node psi's (one-body energies)
  for(i in 1:crf$n.nodes){
    node.levs                        <- list(crf$node.state.names[[i]])
    names(node.levs)                 <- crf$node.name.tab$name[i]
    gRbase.node.potentials[[i]]      <- tabNew(crf$node.name.tab$name[i], levels=node.levs, values=crf$node.pot.list[[i]])
    gRbase.node.nlog.potentials[[i]] <- -log(gRbase.node.potentials[[i]])
  }

  # Decorate edge potentials:
  gRbase.edge.potentials      <- rep(list(NULL),crf$n.edges) # edge Psi's
  gRbase.edge.nlog.potentials <- rep(list(NULL),crf$n.edges) # edge psi's (two-body energies)
  for(i in 1:crf$n.edges){
    e1                               <- crf$node.name.tab$name[crf$edges[i,1]]
    e2                               <- crf$node.name.tab$name[crf$edges[i,2]]
    node.levs                        <- list(crf$node.state.names[[crf$edges[i,1]]],
                                             crf$node.state.names[[crf$edges[i,2]]])
    names(node.levs)                 <- c(e1,e2)
    gRbase.edge.potentials[[i]]      <- tabNew(c(e1,e2), levels=node.levs, values=as.numeric(crf$edge.pot[[i]]))
    gRbase.edge.nlog.potentials[[i]] <- -log(gRbase.edge.potentials[[i]])
  }

  potential.info        <- list(gRbase.node.potentials,
                                gRbase.edge.potentials,
                                gRbase.node.nlog.potentials,
                                gRbase.edge.nlog.potentials)
  names(potential.info) <- c("node.potentials","edge.potentials","node.energies","edge.energies")

  return(potential.info)

}


#---------------------------------------------------------------------------
# Internal auxiliary stuff to help out above for \em{slightly} improved readability
#---------------------------------------------------------------------------

# To make the more symmetric edge paramaterization indices:
# wii == wjj
# wii != wjj
# wij == wji
# wij != wji
# Rectangular edge parameterization matrices (i.e. when the node state spaces
# between the nodes are different sizes) are indexed symmetrically in the (top-left corner)
# square part and sequentially, in row-major order, in the complement.
# @export
edge.param.indices.helper <- function(nr, nc, pmax.idx, diagonal.elements="standard") {

  im    <- array(-1, c(nr,nc))   # parameter index matrix

  if(nr < nc) {                  # ** rectangular: more columns than rows
    im.sq <- im[1:nr, 1:nr]      # square part
    im.cp <- im[1:nr, (nr+1):nc] # complement
    # If complement is just a column:
    if( is.null(dim(im.cp)) ){
      dim(im.cp) <- c(length(im.cp), 1) # give the vector a dimensional argument
    }
  } else if(nr > nc) {           # ** rectangular: more rows than columns
    im.sq <- im[1:nc, 1:nc]      # square part
    im.cp <- im[(nc+1):nr, 1:nc] # complement
    # If complement is just a row:
    if( is.null(dim(im.cp)) ){
      dim(im.cp) <- c(1, length(im.cp)) # give the vector a dimensional argument
    }
  } else {                       # ** square
    im.sq <- im[1:nr, 1:nc]      # nr == nc so parameter index matrix is only square
  }

  # Handel the square part of the parameter index matrix im.sq
  nsq   <- nrow(im.sq) # square part dimension
  if(diagonal.elements == "standard"){
    # if diagonal elements equal ii == jj desired
    count <- pmax.idx
    for(i in 1:nsq){
      for(j in 1:nsq){
        if(i>j) {
          im.sq[i,j] <- count
          count <- count + 1
        }
      }
    }
    # Adjust the last row so bottom left corner is 0 and re-index:
    im.sq[nsq,]  <- im.sq[nsq,] - 1
    im.sq        <- im.sq + 1
    im.sq[nsq,1] <- 0
    diag(im.sq)  <- pmax.idx

  } else if(diagonal.elements == "flexible"){
    # if diagonal elements not equal ii != jj desired
    count <- pmax.idx
    for(i in 1:nsq){
      for(j in 1:nsq){
        if(i>=j) {
          im.sq[i,j] <- count
          count      <- count + 1
        }
      }
    }
    im.sq[nsq,]  <- im.sq[nsq,] - 1
    im.sq[nsq,1] <- 0

  } else {
    stop("diagonal.elements must be standard or flexible")
  }

  # Symmetrize here before handling complement:
  im.sq[upper.tri(im.sq)] <- t(im.sq)[upper.tri(im.sq)]

  # Handel the rectangular complement im.cp
  if(nr < nc) {
    count <- max(im.sq) + 1
    im.cp <- matrix( count:(count + prod(dim(im.cp)) - 1), nrow(im.cp), ncol(im.cp), byrow=T ) # Use row-major order
    im    <- cbind(im.sq,im.cp)
  } else if(nr > nc) {
    count <- max(im.sq) + 1
    im.cp <- matrix( count:(count + prod(dim(im.cp)) - 1), nrow(im.cp), ncol(im.cp), byrow=T ) # Use row-major order
    im    <- rbind(im.sq,im.cp)
  } else {
    im <- im.sq # parameter matrix is square so no complement
  }

  return(im)

}
