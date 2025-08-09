library(CRFutil2)

# Graph formula for Star field:
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

# Compute the full model matrix containing features doe all possible configs:
M <- compute.model.matrix.e(X.i, wp)

# There are 2 nodes with 2 states, 2 nodes with three states and 1 node with 4 states
# so there should be this many rows in M:
2^2 * 3^2 * 4^1
nrow(M) # Same?

# Compute the vector of expected number of features:
dist.pot.info <- distribution.from.potentials(wp)
prx <- dist.pot.info$config.probs$prob

th   <- wp$par
eps  <- array(NA,c(nrow(M), wp$n.par))
aeps <- array(NA,c(nrow(M), wp$n.par))
eph  <- array(NA,c(nrow(M), wp$n.par))
ens  <- array(NA,c(nrow(M)))
for(j in 1:nrow(M)) {
  for(k in 1:wp$n.par) {
    eps[j,k]  <- M[j,k]*th[k]         # energy contrib from theta_k to config X_j
    aeps[j,k] <- M[j,k]*th[k]*prx[j]  # addens for mean param energies?
    eph[j,k]  <- M[j,k]*prx[j]        # addens for mean features?
  }
  ens[j] <- energye(X.i[j,],wp) # configuration energy
}

# Configuration energies. These should be the same:
rowSums(eps)
ens

mep <- colSums(aeps) # mean energies of parameters?
plot(th, typ="h", main = "theta")
plot(mep, typ="h")
plot(exp(-mep),typ="h", ylim=c(0,4)) # Relative parameter importance??

# mean features
mph <- colSums(eph)
plot(mph, typ="h")

# Expected (mean) features. These should be the same
mph
t(prx)%*%M

th*mph # mean energies of parameters too?
mep


#sum(exp(-eps[,1]) * prx[1])

junk <- t(prx) %*% exp(-eps)
junk2 <- t(exp(-eps)) %*% prx
dim(junk)
dim(junk2)

junk[1]
junk2[1]

exp(-eps[,1]) %*% prx
exp(-eps[,2]) %*% prx

t(exp(-eps)) %*% as.matrix(prx)
t(as.matrix(prx)) %*% exp(-eps)

as.matrix(prx)
