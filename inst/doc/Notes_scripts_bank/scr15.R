library(CRFutil2)
library(plot.matrix)

# Graph formula for Star field:
grph <- ~A:B+B:C+C:D+D:A+D:B+C:E

# Define node state space and configuration space:
sA1 <- "A1"; sA2 <- "A2"
sB1 <- "B1"; sB2 <- "B2"; sB3 <- "B3"
sC1 <- 1;    sC2 <- 2
sD1 <- "D1"; sD2 <- "D2"; sD3 <- "D3"; sD4 <- "D4"
sE1 <- 1;    sE2 <- 3;    sE3 <- 2

wp  <- make.empty.field(graph.eq    = grph,
                        num.states  = c(2,3,2,4,3),
                        state.names = list(
                          c(sA1,sA2),
                          c(sB1,sB2,sB3),
                          c(sC1,sC2),
                          c(sD1,sD2,sD3,sD4),
                          c(sE1,sE2,sE3)),
                        parameterization.typ = "standard",
                        plotQ                = T)

# Make up some random parameters and insert them into the model:
set.seed(1)
wp.rthetas <- rnorm(n = wp$num.par, mean = 0, sd = 1)
insert.params.and.pots(wp, params.samp = wp.rthetas)

# Decorate  potentials and energies with gRbase annotations to use later:
make.gRbase.potentials(crf=wp, includeQ = T)
dump.crf(wp)

# Generate and order all configurations in node state index form:
X   <- order.configs(configs.mat=expand.grid(wp$node.state.names), crf = wp, st.idxQ=T, order.nodesQ=T)$config.mat
X.i <- configs.n2i(X, crf = wp)

# a. Compute the full model matrix containing features for all possible configs:
# {\boldsymbol \phi} = {\bf M}_{\boldsymbol \phi}
M.phi <- compute.model.matrix.e(X.i, wp)
plot(M.phi, xlab="theta#", ylab="config#", key=NULL)

# There are 2 nodes with 2 states, 2 nodes with three states and 1 node with 4 states
# so there should be this many rows in M.phi:
2^2 * 3^2 * 4^1
nrow(M.phi) # Same?

# b. All configuration energies:
# {\boldsymbol E} = {\bf M}_{\boldsymbol \phi}{\boldsymbol \theta}
E.all <- as.vector(M.phi %*% wp$par)
plot(E.all, typ="h", main="All Configuration Energies", xlab="config#")

# c. All configuration probabilities:
dist.pot.info <- distribution.from.potentials(wp)
prx <- dist.pot.info$config.probs$prob
plot(prx, typ="h", main="All Configuration Probabilities", xlab="config#")

# d. All parameter energies for each configuration:
# {\boldsymbol \epsilon} = \text{diag}({\boldsymbol \theta}) \ {\boldsymbol \phi}
emat <- M.phi %*% diag(wp$par)
plot(emat, xlab="theta#", ylab="config#", main="Parameter Energies")

# e. Expected number of features:
# {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}] = {\bf M}_{\boldsymbol \phi}^\dagger {\bf Pr}_{\bf X}
mn.phi <- as.numeric(t(M.phi) %*% prx)
plot(mn.phi, typ="h", main="Expected Number of Features", xlab="theta#")

# f. Expected parameter energies:
#\langle{\boldsymbol \epsilon}\rangle = {\boldsymbol \theta} \odot {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}]
mn.e  <- wp$par * mn.phi                           # OK. Just a vector-vector Hadamard
plot(mn.e, typ="h", main="Mean Parameter Energies", xlab="theta#")

# g. Parameter potentials matrix:
# {\boldsymbol \Psi}_{\boldsymbol \epsilon} = e^{-{\boldsymbol \epsilon}}
Psi.e <- exp(-emat)
plot(Psi.e, xlab="theta#", ylab="config#", main="Parameter Potentials")

# h. Mean parameter potentials
# \langle e^{-{\boldsymbol \epsilon}} \rangle = \langle {\boldsymbol \Psi}_{\boldsymbol \epsilon} \rangle = {\boldsymbol \Psi}_{\boldsymbol \epsilon}^\dagger \ {\bf Pr}_{\bf X}
mn.Psi.e <- as.numeric(t(Psi.e) %*% prx)
plot(mn.Psi.e, typ="h", ylim=c(0,max(mn.Psi.e)), main="Mean Parameter Potentials", xlab="theta#")

# i. Check that the product of parameter potentials for a configuration is equivalent to
# the prodPot (i.e. Boltzmann factor) for the configuration.
# e^{-E({\bf X})} = \prod_{k=1}^{w}  e^{-\epsilon_k({\bf X})}
prodPots   <- exp(-E.all)
prod.Psi.e <- apply(Psi.e, MARGIN = 1, FUN = prod)
d          <- prodPots - prod.Psi.e
plot(d, typ="h", main="Delta All Product Potentials", xlab="config#")
d
max(d)
min(d)
