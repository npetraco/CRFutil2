library(CRFutil2)

grphf <- ~A:B + B:C + C:A
adj   <- ug(grphf, result = "matrix")
tri   <- make.empty.field(graph.eq = grphf, num.states = 3, parameterization.typ="standard", plotQ=T)

# Make up some random parameters and insert them into the model:
set.seed(1)
tri.rthetas <- rnorm(n = tri$num.par, mean = 0, sd = 1)
insert.params.and.pots(tri, params.samp = tri.rthetas)
dump.crf(tri)

# Define states and feature function:
s1 <- 1
s2 <- 2
s3 <- 3

# Enumerate all the state configurations
config.mat <- as.matrix(expand.grid(c(s1,s2,s3),c(s1,s2,s3),c(s1,s2,s3)))
colnames(config.mat) <- c("A","B","C")
config.mat

# Convenience wrapper to compute energies of all configs
ener.func <- function(config) {
  engy <- config.energy(
      config    = config,
      edges.mat = tri$edges,
      one.nlp   = log_list(tri$node.pot.list, neglogQ = T),
      two.nlp   = log_list(tri$edge.pot, neglogQ = T), # use same order as edges!
      ff        = ff1,
      nss.vec   = c(s1,s2,s3))
  return(engy)
}
# Convenience wrapper to compute energies of all configs
ener.func.e <- function(config) {
  engy <-config.energy.e(
    config    = config,
    edges.mat = tri$edges,
    one.nlp   = log_list(tri$node.pot.list, neglogQ = T),
    two.nlp   = log_list(tri$edge.pot, neglogQ = T)) # use same order as edges!
  return(engy)
}
# Convenience wrapper to compute energies of all configs
ener.func.eC <- function(config) {
  engy <-config_energy_e_C(
    config    = config,
    edges_mat = tri$edges,
    one_nlp   = log_list(tri$node.pot.list, neglogQ = T),
    two_nlp   = log_list(tri$edge.pot, neglogQ = T)) # use same order as edges!
  return(engy)
}


# Use to compute all configuration energies:
config.energies    <- sapply(1:nrow(config.mat), function(xx){ener.func(config.mat[xx,])})
config.energies.e  <- sapply(1:nrow(config.mat), function(xx){ener.func.e(config.mat[xx,])})
config.energies.eC <- sapply(1:nrow(config.mat), function(xx){ener.func.eC(config.mat[xx,])})
cbind(config.mat, config.energies, config.energies.e, config.energies.eC)
config.energies - config.energies.e
config.energies - config.energies.eC

# All configuration energies:
prodPots        <- exp(-config.energies) # Product Potentials
Z               <- sum(prodPots)         # The Partition function (by brute force...)
Prs             <-prodPots/Z             # Configuration probabilities

Z                                        # Partition function value
sum(Prs)                                 # Are the probabilities normalized?
cbind(config.mat, config.energies, prodPots, Prs)
plot(1:nrow(config.mat), Prs, typ="h", xlab="X config #", ylab="Pr(X)", main="Configuration Probabilities")

# tri$node.pot.list[[1]][2]+
# tri$node.pot.list[[2]][2]+
# tri$node.pot.list[[3]][3]+
# tri$edge.pot[[1]][2,2]+
# tri$edge.pot[[2]][2,3]+
# tri$edge.pot[[3]][2,3]
# 2 2 3        3.651126 2.596189e-02 3.156761e-01
#

config.mat[1,]
tri$node.pot.list[[1]]
config.mat[1,1]
Eone_e_C(config.mat[1,1], tri$node.pot.list[[1]])
Etwo_e_C(config.mat[1,1], config.mat[1,2], tri$edge.pot[[1]])

config_energy_e_C(config    = config.mat[1,],
                  edges_mat = tri$edges,
                  one_nlp   = tri$node.pot.list,
                  two_nlp   = tri$edge.pot)

config.energy(
  config    = config.mat[1,],
  edges.mat = tri$edges,
  one.nlp   = tri$node.pot.list,
  two.nlp   = tri$edge.pot,
  ff        = ff1,
  nss.vec   = c(s1,s2,s3))

config.energy.e(
  config=config.mat[1,],
  edges.mat = tri$edges,
  one.nlp=tri$node.pot.list,
  two.nlp = tri$edge.pot)
