library(CRFutil2)

grphf <- ~A:B + B:C + C:A
adj   <- ug(grphf, result = "matrix")
tri   <- make.empty.field(graph.eq = grphf, num.states = 3, parameterization.typ="standard", plotQ=T)

# Make up some random parameters and insert them into the model:
set.seed(1)
tri.rthetas <- rnorm(n = tri$num.par, mean = 0, sd = 1)
insert.params.and.pots(tri, params.samp = tri.rthetas)
dump.crf(tri)

# Define node state space and configuration space:
config.mat <- expand.grid(tri$node.state.names)                           # All configurations
config.mat <- order.configs(config.mat, tri, order.nodesQ = T)$config.mat # Put in canonical order

# Compute energies of all configs:
config.energies <- sapply(1:nrow(config.mat), function(xx){energye(config.mat[xx,], tri)})

# Compute configuration probabilities:
prodPots <- exp(-config.energies) # Product Potentials
Z        <- sum(prodPots)         # The Partition function (by brute force...)
Prs      <-prodPots/Z             # Configuration probabilities

Z                                                 # Partition function value
sum(Prs)                                          # Are the probabilities normalized?
cbind(config.mat, config.energies, prodPots, Prs) # Put results in a nice table
plot(1:nrow(config.mat), Prs, typ="h", xlab="X config #", ylab="Pr(X)", main="Configuration Probabilities")
