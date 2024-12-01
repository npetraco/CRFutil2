library(CRFutil2)

grphf <- ~A:B + B:C + C:A
adj   <- ug(grphf, result = "matrix")

tri <- make.empty.field(
  graph.eq   = grphf,
  adj.mat    = NULL,
  #num.states = 2,
  #num.states = 4,
  num.states = c(4,3,5),
  #parameterization.typ="general",
  parameterization.typ="standard",
  #parameterization.typ="flexible",
  node.par=NULL,
  edge.par=NULL,
  plotQ=T)
dump.crf(tri)

insert.params.and.pots(tri, params.samp = rnorm(n = tri$num.par, mean = 10, sd = 30))
dump.crf(tri)

model.latex(tri, type="parameter", symbols="theta", indices="matrix")
