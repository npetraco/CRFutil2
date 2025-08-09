library(CRFutil2)

# Graph formula for simple two node field:
grph <- ~A:B+A:C+B:C
adj   <- ug(grphf, result = "matrix")

# Define node state space:
#s1 <- 0; s2 <- 1
#s1 <- 1; s2 <- 2
s1 <- "up"; s2 <- "dn"

tri <- make.empty.field(
  graph.eq             = grph,
  num.states           = 2,
  state.names          = rep(list(c(s1,s2)), 3),
  parameterization.typ = "standard",
  plotQ                = T)
dump.crf(tri)

config.mat <- rbind(
  c(s1,s1,s1),
  c(s1,s2,s1),
  c(s1,s1,s1)
)
config.mat.i <- configs.n2i(config.mat, crf = tri)

# Compute MRFs sufficent statistics:
colSums(compute.model.matrix.e(config.mat.i, tri)) # With CRFutil2
mrf.stat(crf = tri, instances = config.mat.i)      # With CRF
