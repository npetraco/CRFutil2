library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+D:E
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
s1 <- 1; s2 <- 2

star  <- make.empty.field(graph.eq             = grphf,
                          num.states           = 2,
                          state.names          = rep(list(c(s1,s2)), nrow(adj)),
                          parameterization.typ = "standard",
                          plotQ                = T)

# Make up some random parameters and insert them into the model:
set.seed(1)
star.rthetas <- rnorm(n = star$num.par, mean = 0, sd = 1)
insert.params.and.pots(star, params.samp = star.rthetas)
dump.crf(star)

# Enumerate all the state configurations
config.mat <- expand.grid(c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2))
colnames(config.mat) <- c("A","B","C","D","E")

# NEW: Get energies from potentials and decorate both with gRbase annotations:
star.params   <- make.gRbase.potentials(crf=star)
star.node.en  <- star.params$node.energies
star.edge.en  <- star.params$edge.energies
star.node.pot <- star.params$node.potentials
star.edge.pot <- star.params$edge.potentials

