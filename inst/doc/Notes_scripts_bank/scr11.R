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
s1 <- 1
s2 <- 2
s3 <- 3

config.mat <- as.matrix(expand.grid(c(s1,s2,s3),c(s1,s2,s3),c(s1,s2,s3)))
colnames(config.mat) <- c("A","B","C")
config.mat

# Convenience wrapper to compute energies of all configs
ener.func <- function(config) {
  engy <- config.energy(
    config    = config,
    edges.mat = tri$edges,
    one.nlp   = tri$node.pot.list,
    two.nlp   = tri$edge.pot, # use same order as edges!
    ff        = ff1,
    nss.vec   = c(s1,s2,s3))
  return(engy)
}

# All configuration energies and probabilities:
config.energies <- sapply(1:nrow(config.mat), function(xx){ener.func(config.mat[xx,])})
prodPots        <- exp(-config.energies) # Product Potentials
Z               <- sum(prodPots)         # The Partition function (by brute force...)
Prs             <-prodPots/Z             # Configuration probabilities

Z                                        # Partition function value
sum(Prs)                                 # Are the probabilities normalized?
cbind(config.mat, config.energies, prodPots, Prs)
plot(1:nrow(config.mat), Prs, typ="h", xlab="X config #", ylab="Pr(X)", main="Configuration Probabilities")
