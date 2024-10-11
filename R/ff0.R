#' @title       Handy feature function for testing
#' @description The function is a handy feature function for testing.
#'
#' @param st     State label
#' @param ss.dim State space dimension. Default is 2 (Ising)
#' @param w.vec  Optional weight vector
#' @param st.vec Optional vector for state names
#'
#' @details The function is a handy feature function for testing. It can handle both Ising and Potts state spaces.
#'
#' @return The feature function, which is a vector.
#'
#' @examples ff0(st = 3, ss.dim = 4, st.vec = c(0,1,2,3))
#' ff0(st = "a", st.vec = c("a","b"))
#' ff0(st = "ID", ss.dim = 3, st.vec = c("ID","exclude","inconclusive"))
#' ff0(1)
#' ff0(1,3)
#' ff0(st = "b")           # Throws error
#' ff0(st = 5)             # Throws error
#' ff0(st = 5, ss.dim = 4) # Throws error
#'
#' @export
ff0 <- function(st, ss.dim=NULL, w.vec=NULL, st.vec=NULL) {

  if(is.null(ss.dim)) {
    if(!is.null(st.vec)) {
      ss.dim.loc = length(st.vec)
    } else {
      #stop("If state space dimension, ss.dim, is NULL, state space, st.vec, cannot be NULL!")
      ss.dim.loc <- 2 # Just assume 2 dimensional node state space if no ss.dim or st.vec specified. Throw supressible warning??
    }
  } else {
    ss.dim.loc <- ss.dim
  }

  if(ss.dim.loc < 2) {
    stop("State space dimension, ss.dim, must be 2 or more.")
  }

  # Check for a weight vector
  if(is.null(w.vec)){
    w.vec.loc = array(1, ss.dim.loc) # Just do unit state weights if no weight vector is given
  } else {
    w.vec.loc = w.vec
  }

  # Check for a what the names are of the node states
  if(is.null(st.vec)){
    st.vec.loc = 1:ss.dim.loc # Just do sequential positive integers if no node state names are given
  } else {
    st.vec.loc = st.vec
  }

  if(length(st.vec.loc) != ss.dim.loc){
    stop("Number of node states does not match node state space dimension!")
  }

  if(length(w.vec.loc) != ss.dim.loc){
    stop("Dimension of weight vector does not match node state space dimension!")
  }

  if(!(st %in% st.vec.loc)){
    stop("Input node state does not match anything in node state space!")
  }

  out.ff <- w.vec.loc*as.numeric(st == st.vec.loc)
  return(out.ff)

}
