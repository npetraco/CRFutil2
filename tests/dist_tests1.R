library(CRFutil2)
library(DescTools)

# Test distribution functions

# Graph formula for simple two node field:
grphf <- ~A:B
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
#s1 <- 0; s2 <- 1
s1 <- 1; s2 <- 2
#s1 <- "up"; s2 <- "dn"

m2n  <- make.empty.field(graph.eq             = grphf,
                         num.states           = 2,
                         state.names          = list(c(s1,s2), c(s1,s2)),
                         parameterization.typ = "general",
                         plotQ                = T)
par.vec <- -log(c(1,1,30,5,10)) # A, B, A-B potentials of Koller misconception cf. pp. 104, fig 4.1a
insert.params.and.pots(m2n, params.samp = par.vec)
dump.crf(m2n)
bels <- infer.junction(m2n)
bels

# Get a sample for empirical tests
m2n.samp <- sample.crf(m2n, 5000, sample.junction, dress.sampleQ = T)
plot.marginal.sample(m2n.samp, m2n)
plot.configuration.sample(m2n.samp)

# Association
m2n.ct <- xtabs(~., data=data.frame(m2n.samp))
CramerV(m2n.ct)

#
config.mat <- as.matrix(expand.grid(c(s1,s2),c(s1,s2)))
colnames(config.mat) <- c("A","B")
config.mat

# XXXXXXXXX
config.mat[1,]


# Convenience wrapper to compute energies of all configs
ener.func <- function(config) {
  engy <- config.energy(
    config    = config,
    edges.mat = m2n$edges,
    one.nlp   = m2n$node.pot.list,
    two.nlp   = m2n$edge.pot, # use same order as edges!
    ff        = ff1,
    nss.vec   = c(s1,s2))
  return(engy)
}
# Convenience wrapper to compute energies of all configs
ener.func.e <- function(config) {
  engy <-config.energy.e(
    config    = config,
    edges.mat = m2n$edges,
    one.nlp   = m2n$node.pot.list,
    two.nlp   = m2n$edge.pot)
  return(engy)
}
# Convenience wrapper to compute energies of all configs
ener.func.eC <- function(config) {
  engy <-config_energy_e_C(
    config    = config,
    edges_mat = m2n$edges,
    one_nlp   = m2n$node.pot.list,
    two_nlp   = m2n$edge.pot)
  return(engy)
}


# Use to compute all configuration energies:
config.energies    <- sapply(1:nrow(config.mat), function(xx){ener.func(config.mat[xx,])})
config.energies.e  <- sapply(1:nrow(config.mat), function(xx){ener.func.e(config.mat[xx,])})
config.energies.eC <- sapply(1:nrow(config.mat), function(xx){ener.func.eC(config.mat[xx,])})
cbind(config.mat, config.energies, config.energies.e, config.energies.eC)
config.energies - config.energies.e
config.energies - config.energies.eC


prodPots        <- exp(-config.energies) # Product Potentials
Z               <- sum(prodPots)         # The Partition function (by brute force...)
Prs             <- prodPots/Z            # Configuration probabilities

prodPots.e        <- exp(-config.energies.e) # Product Potentials
Z.e               <- sum(prodPots.e)         # The Partition function (by brute force...)
Prs.e             <- prodPots.e/Z.e            # Configuration probabilities

prodPots.eC        <- exp(-config.energies.eC) # Product Potentials
Z.eC               <- sum(prodPots.eC)         # The Partition function (by brute force...)
Prs.eC             <- prodPots.eC/Z.eC            # Configuration probabilities

Prs
Prs.e
Prs.eC
bels$edge.bel

# SOMETHINGS NOT RIGHT!!!!! Do Koller and Schmidt models for troubleshoot
