#' @title       Feature function
#' @description The function is a handy feature function for testing.
#'
#' @param ns     state label of node (\emph{i.e.} node state)
#' @param ...    Optional arguments: \cr
#'               \describe{
#'                 \item{\code{nss.vec}}{node state space (a vector)}
#'                 \item{\code{nss.dim}}{node state space dimension}
#'                 \item{\code{w.vec}}{node state space weight vector}
#'               }
#'
#' @details The function is a handy feature function for testing. It can handle both Ising and Potts state spaces.
#'  NOTE: All optional arguments must be labeled with their name.
#'
#' @return The feature function, which is a vector.
#'
#' @examples ff1(ns = 3, nss.dim = 4, nss.vec = c(0,1,2,3))
#' ff1(ns = "a", nss.vec = c("a","b"))
#' ff1(ns = "ID", nss.dim = 3, nss.vec = c("ID","exclude","inconclusive"))
#' ff1(1)
#' ff1(1,3)                 # Ambiguous. Optional arguments must be labeled with their name.
#' ff1(ns = "b")            # Throws error
#' ff1(ns = 5)              # Throws error
#' ff1(ns = 5, nss.dim = 4) # Throws error
#'
ff1 <- function(ns, ...) {

  varg     <- list(...)
  varg.nms <- names(varg)

  # Process node state space dimension:
  if("nss.dim" %in% varg.nms) {
    nss.dim <- varg$nss.dim         # User input a node state space dimension
  } else if("nss.vec" %in% varg.nms) {
    nss.dim <- length(varg$nss.vec) # User didn't input node state space dimension, but did specify node state space
  } else {
    nss.dim <- 2                    # Just assume 2 dimensional node state space (Ising) if no nss.dim or nss.vec specified. Throw supressible warning??
  }

  if(nss.dim < 2) {
    stop("Node state space dimension, nss.dim, must be 2 or more.")
  }

  # Check for a node state space weight vector:
  if("w.vec" %in% varg.nms) {
    w.vec <- varg$w.vec        # User input a node state weight vector
  } else {
    w.vec <- array(1, nss.dim) # Just do unit node state weights if no weight vector is given
  }

  # Check for a what the names are of the node states are in the node state space
  if("nss.vec" %in% varg.nms) {
    nss.vec <- varg$nss.vec # User input a node state names
  } else {
    nss.vec <- 1:nss.dim    # Just do sequential positive integers if no node state names are given
  }

  if(length(nss.vec) != nss.dim){
    stop("Number of node states does not match node state space dimension!")
  }

  if(length(w.vec) != nss.dim){
    stop("Dimension of weight vector does not match node state space dimension!")
  }

  if(!(ns %in% nss.vec)){
    stop("Input node state does not match anything in node state space!")
  }

  # At this point everything should be OK. Compute the feature function (a vector, like a spinor but without the SU(N) invariance :o)
  ff.vec <- w.vec*as.numeric(ns == nss.vec)
  return(ff.vec)

}
