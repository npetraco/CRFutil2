library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+D:E
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
#s1 <- "up"; s2 <- "dn"
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

# NEW: Compute Probs of all configurations and logZ from energies:
dist.en.info <- distribution.from.energies(star)

# NEW: As a check compute Probs of all configurations and logZ from potentials as well:
# First decorate  potentials and energies with gRbase annotations to use with gRbase functionality:
star$gR <- make.gRbase.potentials(crf=star)
dist.pot.info <- distribution.from.potentials(star)

# Check that we get the same configuration probs:
Pr.en  <- dist.en.info$config.probs$prob
Pr.pot <- dist.pot.info$config.probs$prob
Pr.en-Pr.pot # Should all be 0 or close to it.

# Config probs by hand with configuration energies:
X   <- expand.grid(star$node.state.names)                     # All configuraitons
X   <- order.configs(X, star, order.nodesQ = T)$config.mat    # Put in canonical order
E.X <- sapply(1:nrow(X), function(xx){energye(X[xx,], star)}) # Compute config energies

prodPots.X <- exp(-E.X)
Z          <- sum(prodPots.X)
Pr.X       <- 1/Z*prodPots.X

# Put results in a nice table:
data.frame(X,E.X,prodPots.X,Pr.en,Pr.pot,Pr.X)
