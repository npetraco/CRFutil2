library(CRFutil)

# Graph formula for Star field:
grphf <- ~A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+D:E

# Adjacenty matrix:
adj <- ug(grphf, result="matrix")

# Check the graph:
#gp <- ug(grphf, result = "matrix")
gp <- graph_from_adjacency_matrix(adj, mode = "undirected")
dev.off()
iplot(gp)


# Define states and feature function:
s1 <- 1
s2 <- 2
f  <- function(y){ as.numeric(c((y==s1),(y==s2))) }

# NEW! Make up random potentials and return a CRF-object
star <- sim.field.random(adjacentcy.matrix=adj, num.states=2, num.sims=100, seed=1)$model
star$par

# NEW: Get energies from potentials and decorate both with gRbase annotations:
V(gp)
star.params   <- make.gRbase.potentials(crf=star, node.names=V(gp), state.nmes=c(s1,s2))
#star.params   <- make.gRbase.potentials(crf=star, node.names=c("A","B","C","D","E"), state.nmes=c(s1,s2))
star.node.en  <- star.params$node.energies
star.edge.en  <- star.params$edge.energies
star.node.pot <- star.params$node.potentials
star.edge.pot <- star.params$edge.potentials

# Enumerate all the state configurations
config.mat <- expand.grid(c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2))
colnames(config.mat) <- c("A","B","C","D","E")

# Configuration energies:
t(t(sapply(1:nrow(config.mat), function(xx){config.energy(config = config.mat[xx,], edges.mat = star$edges, one.lgp = star.node.en, two.lgp = star.edge.en, ff = f)})))


# NEW: Compute Probs of all configurations and logZ from energies:
dist.en.info <- distribution.from.energies(
  state.space   = config.mat,
  edges.mat     = star$edges,
  node.energies = star.node.en,
  edge.energies = star.edge.en,
  energy.func   = config.energy,
  ff = f)
Pr.en <- dist.en.info$state.probs
Pr.en

# NEW: As a check compute Probs of all configurations and logZ from potentials as well:
dist.pot.info <- distribution.from.potentials(
  gRbase.node.potentials = star.node.pot,
  gRbase.edge.potentials = star.edge.pot)
Pr.pot <- dist.pot.info$state.probs
Pr.pot

# Check quick to make sure all config energies are the same between the two combination methods:
#library(prodlim)
names(dimnames(Pr.pot))
order(names(dimnames(Pr.pot)))

Pr.pot2 <- as.data.frame(as.table(Pr.pot))
Pr.pot2
gR.idxs <- Pr.pot2[,order(names(dimnames(Pr.pot)))]

# Rearrange gRbase state order to be in the same order as config.mat:
rearrange.idxs <- sapply(1:nrow(config.mat),
                         function(xx){row.match(config.mat[xx,], table = gR.idxs)})
rearrange.idxs

# Columns the same?
cbind(Pr.pot2[rearrange.idxs,6],Pr.en, Pr.pot2[rearrange.idxs,6]-Pr.en)

# Put in a nice table:
prodPots        <- Pr.en*exp(dist.en.info$logZ) # work backwards
config.energies <- log(prodPots)                # work backwards
cbind(config.mat, config.energies, prodPots, Pr.en, Pr.pot2[rearrange.idxs,6])
t(t(Pr.en))

ce2 <- c(0.5858956,3.4453112,-1.3376104,0.5774732,-0.6111688,2.2482468,-0.9622000,0.9528836,4.5390643,4.3187753,-0.0831671,-1.2477881,-2.0985933,-2.3188823,-5.1483499,-6.3129709,-4.6089232,-0.6926718,-5.1987262,-2.2268068,-0.1307452,3.7855062,0.8519266,3.8238460,3.7031633,4.5397101,0.4146349,0.3068497,2.7407481,3.5772949,1.0246945,0.9169093)
cbind(
  sort(config.energies),
  sort(ce2)
)

dist.en.info$logZ
dist.pot.info$logZ
# > dist.en.info$logZ
# [1] 6.225897
# > dist.pot.info$logZ
# [1] 6.225897

logsumexp(ce2)
