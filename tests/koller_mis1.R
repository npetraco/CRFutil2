library(CRFutil2)

# Koller Miscinception:

# Graph formula for simple two node field:
grphf <- ~A:B+B:C+C:D+D:A
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
s1 <- 0; s2 <- 1
#s1 <- 1; s2 <- 2
#s1 <- "agree"; s2 <- "disagree"

m2n  <- make.empty.field(graph.eq             = grphf,
                         num.states           = 2,
                         state.names          = list(c(s1,s2), c(s1,s2), c(s1,s2), c(s1,s2)),
                         parameterization.typ = "general",
                         plotQ                = T)
par.vec <- -log(c(1,1,1,1,
                  30,5,10,
                  100,1,100,
                  100,1,100,
                  0.01,1,0.01
                  )) # A, B, A-B potentials of Koller misconception cf. pp. 104, fig 4.1a
insert.params.and.pots(m2n, params.samp = par.vec)
dump.crf(m2n)

m2n$gR <- make.gRbase.potentials(m2n)

config.mat <- as.matrix(expand.grid(c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2)))
colnames(config.mat) <- c("A","B","C","D")
config.mat2 <- configs.n2i(config.mat, m2n) # Convert state names to indices

config.energies <- sapply(1:nrow(config.mat2), function(xx){energye(config.mat2[xx,], m2n)})
prodPots        <- exp(-config.energies) # Product Potentials
Z               <- sum(prodPots)         # The Partition function (by brute force...)
Prs             <- prodPots/Z            # Configuration probabilities
Prs

config.energies
data.frame(config.mat,Prs)

# Re-arrange to be in Koller's order (cf. pp. 105):
po2 <- rev(2**(1:4 - 1)) # For little endian
ki  <- sapply(1:nrow(config.mat), function(xx){sum(config.mat[xx,]*po2)+1})
data.frame(config.mat,Prs)[ki,]


dist.pot.info <- distribution.from.potentials(
  gRbase.node.potentials = m2n$gR$node.potentials,
  gRbase.edge.potentials = m2n$gR$edge.potentials)
Pr.pot <- dist.pot.info$state.probs
Pr.pot2 <- as.data.frame(as.table(Pr.pot))
Pr.pot2[,c(3,4,2,1,5)]

Pr.pot2[,c(3,4,2,1)]
config.mat
