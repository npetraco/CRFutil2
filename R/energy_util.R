#' @title       One-body (node) energy function
#' @description One-body (node) energy function
#'
#' @param yA  a node state
#' @param tA  tau vector
#' @param ff  feature function
#' @param ... optional arguments for feature function
#'
#' @details One-body (node) energy function
#'
#' @return One-body (node) energy.
#'
#' @examples # node weights:
#'tau2 <- c( 1, -1.3)
#'tau3 <- c( 1, -1.3, 0.1)
#'
#'Eone(yA = 2, tA = tau3, ff = ff1, nss.dim=3)
#'Eone(yA = 3, tA = tau3, ff = ff1, nss.dim=3)
#'Eone(yA = 2, tA = tau2, ff = ff1)
#'Eone(yA = "a", tA = tau3, ff = ff1, nss.vec = c("a", "b", "c"))
#'Eone(yA = 2, tA = tau2, ff = ff0)
#'Eone(yA = 1, tA = tau2, ff = ff0)
#'Eone(yA = 3, tA = tau2, ff = ff0) # Throws error because less general ff0 used
#'
#' @export
Eone  <- function(yA, tA, ff, ...){
  e.one <- tA %*% ff(yA, ...)
  return(e.one)
}


#' @title       Two-body (edge) energy function
#' @description Two-body (edge) energy function
#'
#' @param yA   node A state
#' @param yB   node B state
#' @param wAB  omega matrix
#' @param ff   feature function
#' @param ...  optional arguments for feature function
#'
#' @details Two-body (edge) energy function
#'
#' @return Two-body (edge) energy.
#'
#' @examples # edge weights:
#'omega22 <- rbind(
#'  c( 3.5, -1.4),
#'  c(-1.4,  2.5)
#')
#'
#'omega33 <- rbind(
#'  c( 3.5, -1.4,   2.5),
#'  c(-1.4,  2.5,  -0.8),
#'  c( 3.8,  0.95, -7.6)
#')
#'
#' Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1, nss.dim=2)
#' Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1, nss.dim=3) # Throws error
#' Etwo(yA = 2, yB = 2, wAB = omega33, ff = ff1, nss.dim=3)
#' Etwo(yA = "c", yB = "b", wAB = omega33, ff = ff1, nss.vec=c("a", "b", "c"))
#'
#' Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff0)
#' Etwo(yA = 2, yB = 1, wAB = omega33, ff = ff0)
#' Etwo(yA = 2, yB = 1, wAB = omega33, ff = ff1) # Throws error bec nss.dim=2 assumed
#' Etwo(yA = 2, yB = 1, wAB = omega22, ff = ff1)
#'
#' @export
Etwo  <- function(yA, yB, wAB, ff, ...){
  e.two <- ff(yA, ...) %*% wAB %*% ff(yB, ...)
  return(e.two)
}


#' @title       Energy function for a configuration of states
#' @description Compute total energy of a configuration. Assumes node/edge energies are both entered as lists.
#'
#' @param config    a node configuration (configuration state) vector
#' @param edges.mat matrix of connected node edges
#' @param two.lgp   negative-log node potentials (one-body "energies"), entered as a list
#' @param two.lgp   negative-log edge potentials (two-body "energies"), entered as a list
#' @param ff        The feature function
#' @param ...       Optional arguments for feature function.
#'
#' @details Assumes node/edge energies are both entered as lists.
#'
#' @return The function will XX
#'
#' @export
config.energy <- function(config, edges.mat, one.nlp, two.nlp, ff) {

  num.nodes <- length(config)
  num.edges <- nrow(edges.mat)

  # # Sum One-body energies (log node-potentials)
  # e.one <- 0
  # for(i in 1:num.nodes){
  #   e.one <- e.one + Eone(config[i], one.lgp[[i]], ff)
  # }
  #
  # # Sum Two-body energies (log edge-potentials)
  # e.two <- 0
  # for(i in 1:num.edges){
  #   e.two <- e.two + Etwo(config[edges.mat[i,1]], config[edges.mat[i,2]], two.lgp[[i]], ff)
  # }
  #
  # ener <- as.numeric(e.one + e.two)
  #
  # return(ener)
}

