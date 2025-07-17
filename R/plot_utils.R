#' @title       Plot marginal sample
#' @description Plot the sample of states at each node
#'
#' @param samples The samples
#'
#' @details The function will plot the counts of each state at each node.
#'
#' @return Nothing.
#'
#' @examples XXXX
#'
#' @export
plot.marginal.sample <- function(samples, ordered.state.names=NULL) {

  num.nodes <- ncol(samples)
  num.samps <- nrow(samples)

  node.names  <- colnames(samples)
  if(is.null(ordered.state.names)){ # If no state names are stipulated:
    st.lbl.class <- class(samples[1]) # Grab the first state of the first node and get it's class. **NOTE: Assume all the rest of the state labels are the same class.
    if((st.lbl.class == "factor") | (st.lbl.class == "character")) {
      print("here4")
      state.names <- 1:length(unique(as.vector(samples))) # We re-label the states numerically in this case to prevent possible re-ordering of the character names if we used unique instead
      #state.names <- unique(as.vector(samples))
    } else if((st.lbl.class == "numeric") | (st.lbl.class == "integer")) {
      state.names <- 1:max(samples) # If the state labels are numeric, assume they are contiguous and ordered. **NOTE: this is a bad assumption with weird numbering schemes I could imagine may be useful in some cases; hence the warning.
      #state.names <- unique(as.vector(samples))
      warning("Assuming state names are numerical, contiguous and ordered. That's ok with you, right?")
    } else {
      stop("State labels must be factor, character, numeric or integer!")
    }
    #
  } else {
    state.names <- ordered.state.names
  }
  #print(node.names)
  #print(state.names)

  # if(is.null(node.names)){
  #   tabl <- table(as.numeric(samples), as.numeric(gl(num.nodes, num.samps)), dnn=c("states", "nodes"))
  # } else {
  #   tabl <- table(samples, as.vector(sapply(1:num.nodes, function(xx){c(rep(node.names[xx], nrow(samples)))})), dnn=c("states", "nodes"))
  #   #print(tabl)
  # }

  print(node.names)
  tabl <- table(as.vector(samples), as.numeric(gl(num.nodes, num.samps)), dnn=c("states", "nodes"))
  # Still reversing when using characters in samples. REPLACE ABOVE WITH ACTUAL MATRIX ELEMENTS.
  # Do as a loop


  print(node.names)
  if(is.null(node.names)){
    colnames(tabl) <- 1:num.nodes
  } else {
    colnames(tabl) <- node.names
  }

  # if(!is.null(state.names)){
  #   rownames(tabl) <- state.names
  # }

  print(state.names)
  rownames(tabl) <- state.names

  print(addmargins(tabl))

  # Bar colors should just alternate between green and red across the states. At some point make this all prettier.
  barplot(tabl, ylab="Frequency", xlab="nodes", main="State Distributions", col=c("green", "red" ), beside=TRUE, width=.3)
  #legend("right", title="States", legend= c(1,2), fill =c("turquoise4", "turquoise2"), box.lty=0)

}


#' @title       Plot marginal sample
#' @description Plot the sample of states at each node
#'
#' @param samples The samples
#'
#' @details The function will plot the counts of each state at each node.
#'
#' @return Nothing.
#'
#' @examples XXXX
#'
#' @export
plot.marginal.sample2 <- function(samples, crf) {

  num.nodes      <- crf$n.nodes
  num.samps      <- nrow(samples)
  node.names     <- crf$node.name.tab[,2]
  state.names    <- crf$node.state.names
  num.states.vec <- crf$n.states

  # Construct (marginal) table of state counts at each node:
  all.state.names.vec <- unique(unlist(crf$node.state.names))
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

  # Bar colors should just alternate between green and red across the states. At some point make this all prettier.
  barplot(tabl, ylab="Frequency", xlab="nodes", main="State Distributions", col=c("green", "red" ), beside=TRUE, width=0.3)

}
