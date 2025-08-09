library(CRFutil2)

# Graph formula for field:
grph <- ~A:B+B:C+C:D+D:A+D:B+C:E
adj  <- ug(grph, result = "matrix")

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

# Compute the full model matrix containing features for all possible configs:
# {\boldsymbol \phi} = {\bf M}_{\boldsymbol \phi}
M.phi <- compute.model.matrix.e(X.i, wp)

# All configuration energies:
# {\boldsymbol E} = {\bf M}{\boldsymbol \theta}
E.all <- M.phi %*% wp$par
plot(E.all, typ="h", main="All Configuration Energies")

# All configuration probabilities:
dist.pot.info <- distribution.from.potentials(wp)
prx <- dist.pot.info$config.probs$prob
plot(prx, typ="h", main="All Configuration Probabilities")

# Expected number of features:
# {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}] = {\bf M}_{\boldsymbol \phi}^\dagger {\bf Pr}_{\bf X}
mn.phi <- t(M.phi) %*% prx
plot(mn.phi, typ="h", main="Expected Number of Features")

# Mean parameter energies:
#\langle{\boldsymbol \epsilon}\rangle = {\boldsymbol \theta} \odot {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}]
mn.e <- wp$par * mn.phi
plot(mn.e, typ="h", main="Mean Parameter Energies")

# Parameter energies matrix:
# {\boldsymbol \epsilon} = {\boldsymbol \theta} \odot {\boldsymbol \phi}
emat <- wp$par * M.phi
emat

# Parameter potentials matrix:
# {\boldsymbol \Psi}_{\boldsymbol \epsilon} = e^{-{\boldsymbol \epsilon}} = e^{-{\boldsymbol \theta} \odot {\boldsymbol \phi}}
Psi.e <- exp(-emat)
Psi.e

# Mean parameter potentials:
# \langle e^{-{\boldsymbol \epsilon}} \rangle = \langle {\boldsymbol \Psi}_{\boldsymbol \epsilon} \rangle = {\boldsymbol \Psi}_{\boldsymbol \epsilon}^\dagger \ {\bf Pr}_{\bf X}
mn.pp <- t(Psi.e) %*% prx
plot(mn.pp, typ="h", ylim=c(0,max(mn.pp)), main="Mean Parameter Potentials")

