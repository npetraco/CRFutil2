#' @title       Plot marginal sample
#' @description Plot the sample of states at each node
#'
#' @param samples The samples
#' @param samples a crf object
#'
#' @details The function will plot the counts of each state at each node.
#'
#' @return Nothing.
#'
#' @examples XXXX
#'
#' @export
plot.marginal.sample <- function(samples, crf) {

  num.nodes      <- crf$n.nodes
  num.samps      <- nrow(samples)
  node.names     <- crf$node.name.tab[,2]
  state.names    <- crf$node.state.names
  num.states.vec <- crf$n.states

  # Construct (marginal) table of state counts at each node:
  all.state.names.vec <- unique(unlist(state.names))

  if(F %in% (sort(unique(as.vector(samples))) == sort(all.state.names.vec)) ) {
    print("===============================")
    print("State names in sample:")
    print(sort(unique(as.vector(samples))))
    print("===============================")
    print("State names in crf object:")
    print(sort(all.state.names.vec))
    print("===============================")
    stop("State names in sample must be the same as state names in crf object!")
  }
  #print( sort(unique(as.vector(samples))) == sort(all.state.names.vec) )
  #print(F %in% sort(unique(as.vector(samples))) == sort(all.state.names.vec))

  tabl <- array(0, dim = c(length(all.state.names.vec), num.nodes) )

  for(i in 1:num.nodes){
    for(j in 1:num.states.vec[i]){
      st  <- state.names[[i]][j]    # State name at node i
      cnt <- sum(samples[,i] == st) # State occurrence count at each node

      tabl.st.idx <- which(all.state.names.vec == st) # So states on nodes can be in any order
      tabl[tabl.st.idx,i] <- cnt

      #print(paste0("Node: ", node.names[i], "  st: ", st, "(", tabl.st.idx, ")", "  #= ", cnt))
    }
  }
  tabllabels        <- list()
  tabllabels$states <- all.state.names.vec
  tabllabels$nodes  <- node.names
  dimnames(tabl)    <- tabllabels
  #print(tabl)

  print(addmargins(tabl, margin = c(1)))
  print(tabl/num.samps)

  # Bar colors should just alternate between green and red across the states. At some point make this all prettier.
  barplot(tabl, ylab="Frequency", xlab="nodes", main="State Distributions", col=c("green", "red" ), beside=TRUE, width=0.3)

}


#' @title       Plot configuration sample
#' @description Plot the sample of configurations
#'
#' @param samples the samples
#' @param samples a crf object
#'
#' @details The function will plot the counts of each unique configuration of states in a sample of configurations.
#'
#' @return Nothing.
#'
#' @examples XXXX
#'
#' @export
plot.configuration.sample <- function(samples, num.top.configs=3) {

  # Configuration (aggregated) frequency table
  config.cnts <- as.data.frame(ftable(data.frame(samples)))
  cnts        <- config.cnts[,ncol(config.cnts)]
  plot(1:nrow(config.cnts), cnts, typ="h", ylab="counts", xlab="config #", ylim=c(0,max(cnts)))

  print(config.cnts)
  print(paste0("Top ", num.top.configs, " configs:"))
  print(config.cnts[ order(cnts, decreasing = T)[1:num.top.configs], ])

}
