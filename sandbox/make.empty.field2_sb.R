# Add custom parameterization through node.par and edge.par

make.empty.field2_sb <- function(graph.eq=NULL, adj.mat=NULL, num.states = 2, state.names = NULL, parameterization.typ="standard", node.par=NULL, edge.par=NULL, plotQ=FALSE) {

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

  # Edge translation table:
  new.crf$edge.name.tab <- data.frame(
    1:new.crf$n.edges,                            # edge index
    new.crf$edges,                                # node indices of edge
    cbind(
      new.crf$node.name.tab[new.crf$edges[,1],2], # node names of edge
      new.crf$node.name.tab[new.crf$edges[,2],2]
    )
  )
  colnames(new.crf$edge.name.tab) <- c("edge.idx","n1.idx","n2.idx","n1.name","n2.name")

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
      a.tau.vec                           <- rep(NA, new.crf$max.state) # NAs are mean those parameters don't appear for the node because they are not required as per the node's state space. Eg, of the node state space dimension is 2 there is no need for a third parameter.
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

  } else if(parameterization.typ == "custom"){

    if((is.null(node.par)) | (is.null(edge.par))){
      stop("node.par and edge.par need to be defined for custom parameterization!")
    }

    for(i in 1:new.crf$n.nodes){
      new.crf$node.par[i,,1] <- node.par[i,,1]

      # Initialize node potentials to necessary number of 1s. NAs go where there is no node parameter.
      a.init.tau.pot                          <- rep(1, num.states.loc[i])
      new.crf$node.pot[i,]                    <- rep(NA, new.crf$max.state)
      new.crf$node.pot[i,1:num.states.loc[i]] <- a.init.tau.pot

      # Alternative storage for node pots and pars:
      new.crf$node.par.list[[i]] <- node.par[i,,1]    # NA rm?? Also account for node.par entered as a list??
      new.crf$node.pot.list[[i]] <- a.init.tau.pot
    }

  } else {
    stop("parameterization.typ must be standard, flexible, general and custom. No other node parameterization types are availible at this point.")
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

  } else if(parameterization.typ=="custom"){

    #XXXXXXXXXXXXXXX CUSTOM EDGE PARAMETERIZATION HERE
    #print("Edge custom parameterization here!")
    for(i in 1:new.crf$n.edges){
      new.crf$edge.par[[i]][,,1] <- edge.par[[i]]
    }

  } else {
    stop("parameterization.typ must be standard, flexible, general and custom. No other node parameterization types are availible at this point.")
  }

  # Gather all unique parameter indices. Drop any 0s as they are just placeholders:
  par.idxs <- sort(unique(unlist(c(new.crf$node.par.list, new.crf$edge.par))))[-1]
  num.par  <- max(par.idxs)

  par.idxs.contiguousQ <- ((1:num.par) == par.idxs) # Check that the parameter indices are contiguous and that we didn't skip any
  if( sum(par.idxs.contiguousQ) != num.par ){
    param.idx.err.mat           <- cbind(1:num.par, par.idxs)
    colnames(param.idx.err.mat) <- c("number", "parameter index")
    print(param.idx.err.mat)
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
