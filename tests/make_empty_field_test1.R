#grphf <- ~A:B
#grphf <- ~A:B + B:C + C:D + D:A
#grphf <- ~A:B + B:C + C:A + D:A + D:B
grphfa <- ~A:B + B:C + C:A + D:A + D:B + D:E + E:FF
grphfb <- ~1:2 + 2:3 + 3:1 + 4:1 + 4:2 + 4:5 + 5:6
adj   <- ug(grphf, result = "matrix")

mdl <- make.crf(adj, n.states = c(3,2,4,3,4,2))
dump.crf(mdl)
mdl2 <- make.features(mdl, n.nf = 1, n.ef = 1) # Only ever want this I think.
dump.crf(mdl2)
mdl2$node.par

#mdl3 <- make.par(mdl2, 7)
#dump.crf(mdl3)

mdl <- make.empty.field(
  graph.eq   = grphf,
  adj.mat    = NULL,
  num.states = c(3,2,4,3,4,2),
  parameterization.typ="general",
  node.par=NULL,
  edge.par=NULL,
  plotQ=T)
mdl$node.name.tab[,1]
mdl$node.name.tab[,2]
mdl$node.par
mdl$node.pot

dump.crf(mdl)
ug(grphf, result="matrix")


mdl2 <- make.empty.field(
  graph.eq   = grphfb,
  adj.mat    = NULL,
  num.states = 2,
  parameterization.typ="general",
  node.par=NULL,
  edge.par=NULL,
  plotQ=T)
dump.crf(mdl2)
