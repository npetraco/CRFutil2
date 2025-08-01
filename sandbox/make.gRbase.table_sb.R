make.gRbase.table_sb <- function(node.name.vec, crf){

  node.name.idx <- NULL
  for(i in 1:length(node.name.vec)){
    node.name.idx <- c(node.name.idx, which(names(crf$node.state.names) == node.name.vec[i]))
  }
  print(node.name.idx)


}

