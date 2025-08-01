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
plot.marginal.sample_OLD <- function(samples, crf) {

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
  # Set up layout for plots:
  plot.rows <- floor(sqrt(num.nodes))
  plot.cols <- ceiling(sqrt(num.nodes))
  if( (plot.rows*plot.cols) < num.nodes) {
    plot.rows <- plot.rows + 1 # if prod < num.nodes add 1 to plot.rows which should be the smaller
  }

  # Loop over nodes and state names at each node.
  # Count the number of times each state at a node occurs.
  # Plot a bar plot for the counts at each node:
  par(mfrow=c(plot.rows, plot.cols))
  for(i in 1:num.nodes){
    st.cnt.vec <- NULL
    for(j in 1:length(state.names[[i]])) {
      st.cnt <- sum(state.names[[i]][j] == samples[,i])
      #print(paste0("Node:", node.names[i], " state:", state.names[[i]][j], " #=", st.cnt))
      st.cnt.vec <- c(st.cnt.vec, st.cnt)
    }
    # Bar colors should just alternate between green and red across the states. At some point make this all prettier.
    barplot(st.cnt.vec, ylab="Frequency", names.arg=state.names[[i]], main=node.names[i], col=c("green", "red" ) )

    print(paste0("Node:", node.names[i]))
    st.nme.tbl           <- data.frame(state.names[[i]], st.cnt.vec)
    colnames(st.nme.tbl) <- c("state", "count")
    print(st.nme.tbl)
  }
  par(mfrow=c(1,1))

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
plot.configuration.sample <- function(samples, crf=NULL, num.top.configs=3) {

  # Configuration (aggregated) frequency table
  config.cnts <- as.data.frame(ftable(data.frame(samples)))

  # If a crf object is input put configs in canonical order:
  if(!is.null(crf)){
    cnts            <- config.cnts[,ncol(config.cnts)]
    config.ord.info <- order.configs(config.cnts[,1:crf$n.nodes], crf = crf) # **NOTE: May cause problems if user sends in an undressed sample of configs
    config.cnts     <- config.ord.info$config.mat
    config.cnts     <- data.frame(config.cnts, cnts[config.ord.info$config.rearr.idxs])
  }
  #print(config.cnts)

  cnts        <- config.cnts[,ncol(config.cnts)] # Separate out config count for plotting
  plot(1:nrow(config.cnts), cnts, typ="h", ylab="counts", xlab="config #", ylim=c(0,max(cnts)))

  print(config.cnts)
  print(paste0("Top ", num.top.configs, " configs:"))
  print(config.cnts[ order(cnts, decreasing = T)[1:num.top.configs], ])

}
