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


#' @title       Efficient one-body (node) energy function
#' @description Efficient one-body (node) energy function
#'
#' @param yA  a node state
#' @param tA  tau vector
#'
#' @details Efficient one-body (node) energy function. No feature function (ff) to pass in. Just does
#' the energy calculation as a look-up and not a matrix multiply. Assumes state names are just INDICES
#' (i.e. yA = 1, 2, 3, ......) and all state weights are 1.
#'
#' @return One-body (node) energy.
#'
#' @examples XXXX
#'
#' @export
Eone.e  <- function(yA, tA){
  e.one <- tA[yA]
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


#' @title       Efficient two-body (edge) energy function
#' @description Efficient two-body (edge) energy function
#'
#' @param yA   node A state
#' @param yB   node B state
#' @param wAB  omega matrix
#'
#' @details Efficient two-body (edge) energy function. No feature function (ff) to pass in. Just does
#' the energy calculation as a look-up and not a matrix multiply. Assumes state names are just INDICES
#' (i.e. yA , yB= 1, 2, 3, ......) and all state weights are 1.
#'
#' @return Two-body (edge) energy.
#'
#' @examples XXXX
#'
#' @export
Etwo.e  <- function(yA, yB, wAB){
  e.two <- wAB[yA,yB]
  return(e.two)
}


#' @title       Energy function for a configuration of states
#' @description Compute total energy of a configuration. Assumes node/edge energies are both entered as lists.
#'
#' @param config    a node configuration (configuration state) vector
#' @param edges.mat matrix of connected node edges
#' @param one.lgp   negative-log node potentials (one-body "energies"), entered as a list
#' @param two.lgp   negative-log edge potentials (two-body "energies"), entered as a list
#' @param ff        The feature function
#' @param ...       Optional arguments for feature function.
#'
#' @details Assumes node/edge energies are both entered as lists.
#'
#' @return The function will XX
#'
#' @export
config.energy <- function(config, edges.mat, one.nlp, two.nlp, ff, ...) {

  config.loc <- as.vector(as.matrix(config)) # Guarantee that the config is a vector. Other data types, particularly dataframes, can cause misinterpretation issues.
  num.nodes  <- length(config.loc)
  num.edges  <- nrow(edges.mat)

  # Sum One-body energies (log node-potentials)
  e.one <- 0
  for(i in 1:num.nodes){
    e.one <- e.one + Eone(config.loc[i], one.nlp[[i]], ff, ...)
  }

  # Sum Two-body energies (log edge-potentials)
  e.two <- 0
  for(i in 1:num.edges){
    e.two <- e.two + Etwo(config.loc[edges.mat[i,1]], config.loc[edges.mat[i,2]], two.nlp[[i]], ff, ...)
  }

  ener <- as.numeric(e.one + e.two)

  return(ener)
}


#' @title       Efficient energy function for a configuration of states
#' @description Compute total energy of a configuration. Uses efficient one and two body energy functions.
#' Assumes node/edge energies are both entered as lists.
#'
#' @param config    a node configuration (configuration state) vector
#' @param edges.mat matrix of connected node edges
#' @param two.lgp   negative-log node potentials (one-body "energies"), entered as a list
#' @param two.lgp   negative-log edge potentials (two-body "energies"), entered as a list
#'
#' @details Uses efficient one and two body energy functions. No need to pass in feature function.
#' Assumes node/edge energies are both entered as lists.
#'
#' @return The function will XX
#'
#' @export
config.energy.e <- function(config, edges.mat, one.nlp, two.nlp) {

  config.loc <- as.vector(as.matrix(config)) # Guarantee that the config is a vector. Other data types, particularly dataframes, can cause misinterpretation issues.
  num.nodes  <- length(config.loc)
  num.edges  <- nrow(edges.mat)

  # Sum One-body energies (log node-potentials)
  e.one <- 0
  for(i in 1:num.nodes){
    #print(Eone.e(config.loc[i], one.nlp[[i]]))
    e.one <- e.one + Eone.e(config.loc[i], one.nlp[[i]])
  }

  # Sum Two-body energies (log edge-potentials)
  e.two <- 0
  for(i in 1:num.edges){
    #print(Etwo.e(config.loc[edges.mat[i,1]], config.loc[edges.mat[i,2]], two.nlp[[i]]))
    e.two <- e.two + Etwo.e(config.loc[edges.mat[i,1]], config.loc[edges.mat[i,2]], two.nlp[[i]])
  }

  #print("-------------")

  ener <- as.numeric(e.one + e.two)

  return(ener)
}


#' @title       Energy function wrapper
#' @description Wrapper for energy function
#'
#' @param config a configuration
#' @param crf    a crf object
#' @param ff     the feature function
#' @param ...    optional arguments for feature function.
#'
#' @details The function is a wrapper for config.energy. Easier input.
#'
#' @return Energy of the configuration.
#'
#' @examples XXXX
#'
#' @export
energyf <- function(config, crf, ff, ...) {

  #config.energy(config, edges.mat, one.nlp, two.nlp, ff, ...)
  en.val <- config.energy(
    config    = config,
    edges.mat = crf$edges,
    one.nlp   = crf$node.nlp.list,   # ?????????????????????????
    two.nlp   = crf$edge.nlp,        # use same order as edges!
    ff        = ff, ...)

  return(en.val)

}


#' @title       Energy function wrapper
#' @description Wrapper for energy function
#'
#' @param config a configuration
#' @param crf    a crf object
#'
#' @details The function is a wrapper for config.energy.e. Easier input. **NOTE: assumes
#' state names contained in the crf object are positive integers, i.e. 1, 2, 3, ..., etc.
#'
#' @return Energy of the configuration.
#'
#' @examples XXXX
#'
#' @export
energye <- function(config, crf) {

  en.val <-config.energy.e(
    config    = config,
    edges.mat = crf$edges,
    one.nlp   = crf$node.nlp.list,
    two.nlp   = crf$edge.nlp)

  return(en.val)
}


#' @title       Energy function wrapper
#' @description Wrapper for energy function which uses phi vector for a config to compute energy
#'
#' @param config a configuration
#' @param crf    a crf object
#'
#' @details Compute energy using phi vector for configuration: E({\bf X}) = {\boldsymbol \theta}^{\dagger} {\boldsymbol \phi}({\bf X}) **NOTE: assumes
#' state names contained in the crf object are positive integers, i.e. 1, 2, 3, ..., etc.
#'
#' @return Energy of the configuration.
#'
#' @examples XXXX
#'
#' @export
energyphe <- function(config, crf) {
  #phi <- phi.features.e(config=config, crf=crf)
  #en  <- sum(phi * crf$par)
  en <- sum(phi.features.e(config=config, crf=crf) * crf$par)

  return(en)
}


#' @title       C Energy function wrapper
#' @description Wrapper for C version of the energy function
#'
#' @param config a configuration
#' @param crf    a crf object
#'
#' @details The function is a wrapper for config_energy_e_C. Easier input. **NOTE: assumes
#' state names contained in the crf object are positive integers, i.e. 1, 2, 3, ..., etc.
#'
#' @return Energy of the configuration.
#'
#' @examples XXXX
#'
#' @export
energyeC <- function(config, crf) {
  en.val <-config_energy_e_C(
    config    = config,
    edges_mat = crf$edges,
    one_nlp   = crf$node.nlp.list, # ?????????????????????????
    two_nlp   = crf$edge.nlp)

  return(en.val)
}
