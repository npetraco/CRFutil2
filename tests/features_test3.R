library(CRFutil2)

# Graph formula for Star field:
grph <- ~A:B+B:C+C:D+D:A+D:B+C:E

# Define node state space and configuration space:
sA1 <- "A1"; sA2 <- "A2"
sB1 <- "B1"; sB2 <- "B2"; sB3 <- "B3"
sC1 <- 1;    sC2 <- 2
sD1 <- "D1"; sD2 <- "D2"; sD3 <- "D3"; sD4 <- "D4"
sE1 <- 1;    sE2 <- 3;    sE3 <- 2

tg  <- make.empty.field(graph.eq    = grph,
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
tg.rthetas <- rnorm(n = tg$num.par, mean = 0, sd = 1)
insert.params.and.pots(tg, params.samp = tg.rthetas)

# Decorate  potentials and energies with gRbase annotations to use later:
make.gRbase.potentials(crf=tg, includeQ = T)
dump.crf(tg)

# Generate and order all configurations in node state index form:
X   <- order.configs(configs.mat=expand.grid(tg$node.state.names), crf = tg, st.idxQ=T, order.nodesQ=T)$config.mat
X.i <- configs.n2i(X, crf = tg)

# Abbv for checks:
theta.vec <- tg$par
N         <- nrow(X.i)
w         <- tg$n.par



# a. Compute the full model matrix containing features for all possible configs:
# {\boldsymbol \phi} = {\bf M}_{\boldsymbol \phi}
M.phi <- compute.model.matrix.e(X.i, tg)

# There are 2 nodes with 2 states, 2 nodes with three states and 1 node with 4 states
# so there should be this many rows in M:
2^2 * 3^2 * 4^1
nrow(M.phi) # Same?



# b. All configuration energies:
# {\boldsymbol E} = {\bf M}{\boldsymbol \theta}
E.all <- M.phi %*% tg$par
plot(E.all, typ="h", main="All Configuration Energies")

# Check config energies
E.all.c1 <- sapply(1:nrow(X.i), function(xx){energyeC(config = X.i[xx,], crf = tg)})
d <- E.all.c1 - E.all
plot(d, typ="h", main="Delta All Configuration Energies")
d
max(d)
min(d)



# c. All configuration probabilities:
dist.pot.info <- distribution.from.potentials(tg)
prx <- dist.pot.info$config.probs$prob
plot(prx, typ="h", main="All Configuration Probabilities")

# Check configuration probabilities:
prx.c1 <- exp(-E.all.c1)/sum(exp(-E.all.c1))
d <- prx-prx.c1
plot(d, typ="h", main="Delta All Configuration Energies")
d
max(d)
min(d)



# d. All parameter energies for each configuration:
# {\boldsymbol \epsilon} = \text{diag}({\boldsymbol \theta}) \ {\boldsymbol \phi}
emat <- M.phi %*% diag(theta.vec)

# Check all parameter energies
emat.c1 <- array(NA, c(N,w))
for(n in 1:N) {
  for(k in 1:w) {
    emat.c1[n,k] <- theta.vec[k] * M.phi[n,k]
  }
}

d <- as.numeric(emat-emat.c1)
plot(d, typ="h", main="Delta All Parameter Energies")
d
max(d)
min(d)

# Check that the product of parameter potentials are the same as the prodPots for each config
pp    <- exp(-E.all)
pp.c1 <- array(1,c(N))
for(n in 1:N) {
  for(k in 1:w) {
    pp.c1[n] <- pp.c1[n] * exp(-emat.c1[n,k])
  }
}
d <- pp - pp.c1
plot(d, typ="h", main="Delta All Product Potentials")
d
max(d)
min(d)



# e. Expected number of features:
# {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}] = {\bf M}_{\boldsymbol \phi}^\dagger {\bf Pr}_{\bf X}
mn.phi <- as.numeric(t(M.phi) %*% prx)
plot(mn.phi, typ="h", main="Expected Number of Features")

# Check expected number of features
mn.phi.c1 <- array(0, c(w))
for(k in 1:w) {
  for(n in 1:N) {
    mn.phi.c1[k] <- mn.phi.c1[k] + M.phi[n,k]*prx.c1[n]
  }
}

d <- mn.phi - mn.phi.c1
plot(d, typ="h", main="Delta All Mean Features")
d
max(d)
min(d)



# f. Expected parameter energies for each configuration:
#\langle{\boldsymbol \epsilon}\rangle = {\boldsymbol \theta} \odot {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}]
mn.e  <- tg$par * mn.phi                                               # OK. Just a vector-vector Hadamard
plot(mn.e, typ="h", main="Mean Parameter Energies")

# Check expected parameter energies for each configuration
mn.e.c1 <- array(0, c(w))
for(k in 1:w) {
  for(n in 1:N) {
    mn.e.c1[k] <- mn.e.c1[k] + theta.vec[k] * M.phi[n,k] * prx.c1[n]
  }
}

d <- mn.e - mn.e.c1
plot(d, typ="h", main="Delta All Mean Parameter Energies")
d
max(d)
min(d)



# g. Parameter potentials matrix:
# {\boldsymbol \Psi}_{\boldsymbol \epsilon} = e^{-{\boldsymbol \epsilon}} = e^{-{\boldsymbol \theta} \odot {\boldsymbol \phi}}
Psi.e <- exp(-emat)

# Check Parameter potentials matrix
Psi.e.c1 <- array(NA, c(N,w))
for(n in 1:N) {
  for(k in 1:w) {
    Psi.e.c1[n,k] <- exp(-emat.c1[n,k])
  }
}

d <- as.numeric(Psi.e - Psi.e.c1)
plot(d, typ="h", main="Delta All Parameter Potentials")
d
max(d)
min(d)



# f. Mean parameter potentials
# \langle e^{-{\boldsymbol \epsilon}} \rangle = \langle {\boldsymbol \Psi}_{\boldsymbol \epsilon} \rangle = {\boldsymbol \Psi}_{\boldsymbol \epsilon}^\dagger \ {\bf Pr}_{\bf X}
mn.Psi.e <- as.numeric(t(Psi.e) %*% prx)
plot(mn.Psi.e, typ="h", ylim=c(0,max(mn.Psi.e)), main="Mean Parameter Potemtials")

# Check mean parameter potentials
mn.Psi.e.c1 <- array(0, c(w))
for(k in 1:w){
  for(n in 1:N){
    mn.Psi.e.c1[k] <- mn.Psi.e.c1[k] + exp(-emat.c1[n,k])*prx.c1[n]
  }
}

d <- mn.Psi.e - mn.Psi.e.c1
plot(d, typ="h", main="Delta All Parameter Potentials")
d
max(d)
min(d)
