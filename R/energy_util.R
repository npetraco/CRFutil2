#' @title       One-body (node) energy function
#' @description One-body (node) energy function
#'
#' @param yA A node state
#' @param tA Tau vector
#' @param ff Feature function
#'
#' @details One-body (node) energy function
#'
#' @return One-body (node) energy.
#'
#' @examples XXXXX
#'
#' @export
Eone  <- function(yA, tA, ff){ tA %*% ff(yA) }


#' @title       Two-body (edge) energy function
#' @description Two-body (edge) energy function
#'
#' @param yA  A node state
#' @param yB  A node state
#' @param wAB Tau vector
#' @param ff  Feature function
#'
#' @details Two-body (edge) energy function.
#'
#' @return Two-body (edge) energy.
#'
#' @examples XXXXX
#'
#' @export
Etwo  <- function(yA, yB, wAB, ff){ ff(yA) %*% wAB %*% ff(yB) }


#' Energy function for a configuration of states
#'
#' Compute total energy of a configuration. Assumes node/edge energies are in gRbase format   *****GENERALIZE
#'
#' The function will XXXX
#'
#' @param config    A node configuration (state) vector
#' @param edges.mat Matrix of connected node edges
#' @param two.lgp   Log node potentials (one-body energies)
#' @param two.lgp   Log edge potentials (two-body energies)
#' @param ff        The feature function
#' @return The function will XX
#'
#'
#' @export
config.energy <- function(config, edges.mat, one.lgp, two.lgp, ff) {

  num.nodes <- length(config)
  num.edges <- nrow(edges.mat)

  # Sum One-body energies (log node-potentials)
  e.one <- 0
  for(i in 1:num.nodes){
    e.one <- e.one + Eone(config[i], one.lgp[[i]], ff)
  }

  # Sum Two-body energies (log edge-potentials)
  e.two <- 0
  for(i in 1:num.edges){
    e.two <- e.two + Etwo(config[edges.mat[i,1]], config[edges.mat[i,2]], two.lgp[[i]], ff)
  }

  ener <- as.numeric(e.one + e.two)

  return(ener)
}
