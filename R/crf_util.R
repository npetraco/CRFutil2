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
#' parameterizations. Potentials are not filled in however. Use XXXX to generate random potentials
#' or add in your own later.
#'
#' @return A crf object.
#'
#' @examples XXXX
#'
#' @export
make.empty.field <- function(graph.eq=NULL, adj.mat=NULL, num.states = 2, parameterization.typ="standard", node.par=NULL, edge.par=NULL, plotQ=FALSE) {

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

  # If only 1 number is input to specify the number of states per node. Assumes all nodes have the same number of states.
  if(length(num.states) == 1) {
    num.states.loc <- rep(num.states, new.crf$n.nodes) # Should be the same as new.crf$n.states
  } else {
    num.states.loc <- num.states                       # Should be the same as new.crf$n.states
  }

  # Alternative list storage for node info. Don't have to hold on to all the NAs for node state spaces of different sizes.
  new.crf$node.pot.list <- rep(list(NULL), new.crf$n.nodes)
  new.crf$node.par.list <- rep(list(NULL), new.crf$n.nodes)

  # Parameterization types:
  if(parameterization.typ == "general") {

    # General parameterization:
    # Node parameters:
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

  }


  # Plot graph
  if(plotQ==TRUE){
    new.crf.gp <- graph_from_adjacency_matrix(adjm, mode = "undirected")
    if(!is.null(dev.list())){
      dev.off()
    }
    iplot(new.crf.gp)
  }

  return(new.crf)

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
edge.param.indices.helper <- function(nr, nc, pmax.idx, iijj.eqQ=T) {

  im    <- array(-1, c(nr,nc))   # parameter index matrix

  if(nr < nc) {                  # ** rectangular: more columns than rows
    im.sq <- im[1:nr, 1:nr]      # square part
    im.cp <- im[1:nr, (nr+1):nc] # complement
  } else if(nr > nc) {           # ** rectangular: more rows than columns
    im.sq <- im[1:nc, 1:nc]      # square part
    im.cp <- im[(nc+1):nr, 1:nc] # complement
  } else {                       # ** square
    im.sq <- im[1:nr, 1:nc]      # nr == nc so parameter index matrix is only square
  }

  # Handel the square part of the parameter index matrix im.sq
  nsq   <- nrow(im.sq) # square part dimension
  if(iijj.eqQ == T){
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
    #im.sq[nr,1] <- 0
    im.sq       <- im.sq + 1
    im.sq[nsq,1] <- 0
    diag(im.sq) <- pmax.idx

  } else {
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

  }

  # Symmetrize here before handeling complenent:
  im.sq[upper.tri(im.sq)] <- t(im.sq)[upper.tri(im.sq)]
  #print(im.sq)

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
