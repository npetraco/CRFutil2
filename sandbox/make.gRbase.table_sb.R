make.gRbase.table_sb <- function(node.name.vec, vals, crf, ...){

  node.name.idx <- NULL
  for(i in 1:length(node.name.vec)){
    node.name.idx <- c(node.name.idx, which(names(crf$node.state.names) == node.name.vec[i]))
  }
  #print(node.name.idx)

  # Gather node levels out of list in crf object into a sublist
  node.levs <- do.call(list, crf$node.state.names[node.name.idx])

  # Process values for table (This could be done cleaner........)
  if(class(vals) == "data.frame"){
    vals.loc <- as.numeric(do.call(table, vals))
  } else if(class(vals) == "table"){
    vals.loc <- as.numeric(vals)
  } else if(class(vals) == "numeric"){
    vals.loc <- as.numeric(vals)
  } else {
    stop("vals must be entered in as a table, numeric vector or data frame!")
  }

  gRtab <- tabNew(node.name.vec, levels=node.levs, values=as.numeric(vals.loc), ...)
  return(gRtab)

}

