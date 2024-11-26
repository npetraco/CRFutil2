edge.param.indices.helper(7,4, pmax.idx = 3, iijj.eqQ = T)
edge.param.indices.helper(4,7, pmax.idx = 3, iijj.eqQ = T)
edge.param.indices.helper(7,7, pmax.idx = 3, iijj.eqQ = T)

edge.param.indices.helper(2,2, pmax.idx = 3, iijj.eqQ = F)
edge.param.indices.helper(3,3, pmax.idx = 3, iijj.eqQ = F)

edge.param.indices.helper(
  nr=3,
  nc=2,
  pmax.idx = 27,
  diagonal.elements="standard")
tv <- c(-1,-1)
class(tv)
dim(tv) <- c(1,2)
dim(tv)
class(tv)
nrow(tv)
ncol(tv)


#grphf <- ~A:B
#grphf <- ~A:B + B:C + C:D + D:A
#grphf <- ~A:B + B:C + C:A + D:A + D:B
grphfa <- ~A:B + B:C + C:A + D:A + D:B + D:E + E:FF
grphfb <- ~1:2 + 2:3 + 3:1 + 4:1 + 4:2 + 4:5 + 5:6
adj   <- ug(grphf, result = "matrix")

mdl <- make.empty.field(
  graph.eq   = grphfa,
  adj.mat    = NULL,
  num.states = c(3,2,4,3,4,2),
  #parameterization.typ="standard",
  parameterization.typ="flexible",
  #parameterization.typ="general",
  node.par=NULL,
  edge.par=NULL,
  plotQ=T)
dump.crf(mdl)


mdl2 <- make.empty.field(
  graph.eq   = grphfb,
  adj.mat    = NULL,
  num.states = 3,
  parameterization.typ="standard",
  #parameterization.typ="flexible",
  #parameterization.typ="general",
  node.par=NULL,
  edge.par=NULL,
  plotQ=T)
dump.crf(mdl2)
mdl2$edge.par
