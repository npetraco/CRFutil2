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

phi.features.e(config.mat.i[10,], crf = star)
phi_features_e_C(config.mat.i[10,], edges_mat = star$edges, num_params = star$num.par, node_par = star$node.par, edge_par = star$edge.par)


# Config energies:
ce <- sapply(1:nrow(config.mat.i), function(xx){energye(config.mat.i[xx,], star)})

# Model matrix:
M   <- compute.model.matrix.e(config.mat.i, star)
cme <- M%*%star$par

# Compare: Differences should be 0
data.frame(config.mat, cme, ce, cme-ce)
