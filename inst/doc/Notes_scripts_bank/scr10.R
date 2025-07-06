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


# ******* SOMETHING IS WRONG?? ********

# NEW: Compute Probs of all configurations and logZ from energies:
dist.en.info <- distribution.from.energies(
  state.space   = config.mat,
  edges.mat     = star$edges,
  node.energies = star.node.en,
  edge.energies = star.edge.en)
Pr.en <- dist.en.info$state.probs
Pr.en

# NEW: As a check compute Probs of all configurations and logZ from potentials as well:
dist.pot.info <- distribution.from.potentials(
  gRbase.node.potentials = star.node.pot,
  gRbase.edge.potentials = star.edge.pot)
dist.pot.info$state.probs
Pr.pot <- dist.pot.info$state.probs


Pr.pot2 <- as.data.frame(as.table(Pr.pot))
Pr.pot2[,6]
Pr.en
gR.idxs <- Pr.pot2[,order(names(dimnames(Pr.pot)))]

# Rearrange gRbase state order to be in the same order as config.mat:
rearrange.idxs <- sapply(1:nrow(config.mat),
                         function(xx){row.match(config.mat[xx,], table = gR.idxs)})

# Columns the same?
cbind(Pr.pot2[rearrange.idxs,6],Pr.en, Pr.pot2[rearrange.idxs,6]-Pr.en)

# Put in a nice table:
prodPots        <- Pr.en*exp(dist.en.info$logZ) # work backwards
config.energies <- log(prodPots)                # work backwards
cbind(config.mat, config.energies, prodPots, Pr.en, Pr.pot2[rearrange.idxs,6])


