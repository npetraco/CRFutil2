#' Spit out LaTeX for parameter and potential matrices.
#'
#'
#' The function will XXXX
#'
#' @param XX The XX
#' @return The function will XX
#'
#'
#' @export
model.latex <- function(crf, type="parameter", symbols="theta", indices="vector") {

  #type=c("parameter","potential"), symbols=c("tau.omega","theta","values"), indices=c("matrix","vector","none")

  # Nodes
  if(indices %in% c("matrix","vector")){
    node.par.list <- crf$node.par.list
    for(i in 1:length(node.par.list)){
      node.idxs <- node.par.list[[i]]
      if(symbols=="tau.omega"){
        elem.symb="\tau"
      } else if(symbols=="theta"){
        elem.symb="\theta"
      } else {
        stop("symbols must be tau.omega or theta for node matrix/vector indices")
      }

      # Write to file instead and read back in??
      elem.symb <- paste0(elem.symb, "_", node.idxs)
      elem.symb[which(node.idxs == 0)] <- "0"
      elem.symb <- paste(elem.symb, collapse=" \\ \n ")
      print(elem.symb)
      #print(cat(elem.symb))
    }

  } else if(indices == "values") {
    node.pot.list <- crf$node.pot.list
  } else {
    stop("indices must be matrix, vector or values")
  }


  # Edges

}
