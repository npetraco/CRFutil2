library(CRFutil2)

# Graph formula for simple two node field:
grph <- ~A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+D:E
adj   <- ug(grphf, result = "matrix")

# Define node state space:
#s1 <- 0; s2 <- 1
#s1 <- 1; s2 <- 2
s1 <- "up"; s2 <- "dn"

star <- make.empty.field(graph.eq             = grph,
                         num.states           = 2,
                         state.names          = rep(list(c(s1,s2)), 5),
                         parameterization.typ = "standard",
                         plotQ                = T)

# Make up some random parameters and insert them into the model:
set.seed(1)
star.rthetas <- rnorm(n = star$num.par, mean = 0, sd = 1)
insert.params.and.pots(star, params.samp = star.rthetas)
dump.crf(star)

# Generate and order all configurations in node state index form:
config.mat   <- order.configs(configs.mat=expand.grid(star$node.state.names), crf = star, st.idxQ=T, order.nodesQ=T)$config.mat
config.mat.i <- configs.n2i(config.mat, crf = star)

# E({\bf X}) = {\boldsymbol \theta}^{\dagger} {\boldsymbol \phi}({\bf X})
cpe <- sapply(1:nrow(config.mat.i), function(xx){energyphe(config.mat.i[xx,], star)})

# E({\bf X}) =\sum_{i=1}^{\#\text{nodes}}{\bf f}^{\dagger}(X_i=s_{X_i}) {\boldsymbol \tau}_{i} + \sum_{i\sim j}^{\# \text{edges}} {\bf f}^{\dagger}(X_i=s_{X_i}) {\boldsymbol \omega}_{ij} {\bf f}(X_j=s_{X_j}) 
ce  <- sapply(1:nrow(config.mat.i), function(xx){energye(config.mat.i[xx,], star)})

# Compare: Differences should be 0
data.frame(config.mat, cpe, ce, cpe-ce)

# Model matrix:
M   <- t(sapply(1:nrow(config.mat.i), function(xx){phi.features.e(config.mat.i[xx,], crf = star)}))

# Compute all energies as a matrix-vector multiply between the model matrix
# and the parameter vector:
cme <- M %*% star$par

# Compare: Differences should be 0
data.frame(config.mat, cme, ce, cme-ce)
