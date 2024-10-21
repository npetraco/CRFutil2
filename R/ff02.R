#' @title       Feature function
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
ff02 <- function(st, ...) {

  #ss.dim=NULL, w.vec=NULL, st.vec=NULL
  inp.dat <- list(...)
  print(inp.dat)
  print(names(inp.dat))

}
