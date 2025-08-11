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

# Compute the full model matrix containing features for all possible configs:
# {\boldsymbol \phi} = {\bf M}_{\boldsymbol \phi}
M.phi <- compute.model.matrix.e(X.i, tg)

theta.vec <- tg$par
N         <- nrow(X.i)
w         <- tg$n.par

# Check config energies
Eall.1 <- sapply(1:nrow(X.i), function(xx){energyeC(config = X.i[xx,], crf = tg)})
Eall.2 <- M.phi %*% theta.vec
d <- Eall.1 - Eall.2
plot(d, typ="h", main="Delta All Configuration Energies")
d
max(d)
min(d)

# Check Product Potentials equivalence:
# e^{-E({\bf X}_n)} = \prod_{k=1}^{w}  e^{-\epsilon_k({\bf X}_n)}
emat1 <- array(NA, c(N,w))
for(n in 1:N) {
  for(k in 1:w) {
    emat1[n,k] <- theta.vec[k] * M.phi[n,k]
  }
}

pp1 <- exp(-Eall.1)
pp2 <- array(1,c(N))
for(n in 1:N) {
  for(k in 1:w) {
    pp2[n] <- pp2[n] * exp(-emat1[n,k])
  }
}
d <- pp1 - pp2
plot(d, typ="h", main="Delta All Product Potentials")
d
max(d)
min(d)


# Check Mean features:
# {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}] = {\bf M}_{\boldsymbol \phi}^\dagger {\bf Pr}_{\bf X}
prx1    <- exp(-Eall.1)/sum(exp(-Eall.1))
mn.phi1 <- array(0, c(w))
for(k in 1:w) {
  for(n in 1:N) {
    mn.phi1[k] <- mn.phi1[k] + M.phi[n,k]*prx1[n]
  }
}

dist.pot.info <- distribution.from.potentials(tg)
prx2          <- dist.pot.info$config.probs$prob
mn.phi2       <- as.numeric(t(M.phi) %*% prx2)

mn.phi1
mn.phi2
d <- mn.phi1 - mn.phi2
plot(d, typ="h", main="Delta All Mean Features")
d
max(d)
min(d)


# Check Mean parameter energies:
# \langle{\boldsymbol \epsilon}\rangle = {\boldsymbol \theta} \odot {\bf E}_{\boldsymbol \theta}[{\boldsymbol \phi}]
mn.e1 <- array(0, c(w))
for(k in 1:w) {
  for(n in 1:N) {
    mn.e1[k] <- mn.e1[k] + theta.vec[k] * M.phi[n,k] * prx1[n]
  }
}

mn.e2  <- theta.vec * mn.phi2  # as a vector-vector Hadamard, so should be OK
mn.e2a <- diag(theta.vec) %*% mn.phi2
mn.e2b <- mn.phi2 %*% diag(theta.vec)
mn.e2a - t(mn.e2b)

t(mn.phi2)
t(mn.phi2) %*% diag(theta.vec)
t(t(mn.phi2)) %*% diag(theta.vec)
diag(theta.vec) %*% t(t(mn.phi2))

mn.e1
mn.e2
d <- mn.e1 - mn.e2
plot(d, typ="h", main="Delta All Mean Parameter Energies")
d
max(d)
min(d)


# Check:
# {\boldsymbol \epsilon} = {\boldsymbol \theta} \odot {\boldsymbol \phi}
emat1 <- array(NA, c(N,w)) # From above
for(n in 1:N) {
  for(k in 1:w) {
    emat1[n,k] <- theta.vec[k] * M.phi[n,k]
  }
}

emat2 <- theta.vec * M.phi # **** Naive Hadamard: THIS GIVES THE WRONG RESULT!!!!!!!!!!

dim(emat1)
dim(emat2)
emat1 - emat2
d <- as.numeric(emat1 - emat2)
d
plot(d, typ="h", main="Delta All Parameter Energies") # HUH?????
max(d)
min(d)

theta.vec * M.phi[1,]
emat1[1,]
emat2[1,] # HUH?????

t(t(theta.vec)) * t(M.phi)
t(t(theta.vec))
dim(t(M.phi))

emat2a <- t(sapply(1:nrow(M.phi), function(xx){theta.vec * M.phi[xx,]})) # This however give the right result!
dim(emat2a)
emat1 - emat2a

emat2b <- t(diag(theta.vec) %*%  t(M.phi)) # This works too but it's ugly
dim(emat2b)
emat1 - emat2b

emat2c <- M.phi %*% diag(theta.vec) # ****This also works and it's less ugly............
emat1 - emat2c

cbind(
  theta.vec,
  M.phi[1,],
  emat1[1,], # Ref
  emat2[1,], # ???? WRONG!
  emat2a[1,],# Correct
  emat2b[1,],# Correct
  emat2c[1,] # Correct
)



