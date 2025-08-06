library(CRFutil2)
#library(DescTools)

# Koller Miscinception:

# Graph formula for simple two node field:
grphf <- ~A:B+B:C+C:D+D:A
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
#s1 <- 0; s2 <- 1
#s1 <- 1; s2 <- 2
s1 <- "agree"; s2 <- "disagree"

m2n  <- make.empty.field(graph.eq             = grphf,
                         num.states           = 2,
                         state.names          = list(c(s1,s2), c(s1,s2), c(s1,s2), c(s1,s2)),
                         parameterization.typ = "general",
                         plotQ                = T)
par.vec <- -log(c(1,1,1,1,30,5,10,100,1,100,0.01,1,0.01,100,1,100)) # A, B, A-B potentials of Koller misconception cf. pp. 104, fig 4.1a
insert.params.and.pots(m2n, params.samp = par.vec)
dump.crf(m2n)

make.gRbase.potentials(m2n,includeQ = T)

config.mat <- as.matrix(expand.grid(c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2)))
colnames(config.mat) <- c("A","B","C","D")
config.mat.i <- configs.n2i(config.mat, m2n)
config.mat
config.mat.i

config.energy(
  config    = config.mat[3,],
  edges.mat = m2n$edges,
  one.nlp   = log_list(m2n$node.pot.list, neglogQ = T),
  two.nlp   = log_list(m2n$edge.pot, neglogQ = T), # use same order as edges!
  ff        = ff1,
  nss.vec   = c(s1,s2))

config.energy(
  config    = config.mat[3,],
  edges.mat = m2n$edges,
  one.nlp   = m2n$gR$node.energies,
  two.nlp   = m2n$gR$edge.energies, # use same order as edges!
  ff        = ff1,
  nss.vec   = c(s1,s2))

energyf(
  config    = config.mat[3,],
  m2n,
  ff        = ff1,
  nss.vec   = c(s1,s2))

energye(
  config    = config.mat.i[3,],
  crf       = m2n)

energyeC(
  config    = config.mat.i[3,],
  crf       = m2n)

# Use to compute all configuration energies:
config.energies    <- sapply(1:nrow(config.mat),   function(xx){energyf(config.mat[xx,], m2n, ff = ff1, nss.vec = c(s1,s2))})
config.energies.e  <- sapply(1:nrow(config.mat.i), function(xx){energye(config.mat.i[xx,], m2n)})
config.energies.eC <- sapply(1:nrow(config.mat.i), function(xx){energyeC(config.mat.i[xx,], m2n)})

config.energies.e.p <- sapply(1:nrow(config.mat.i), function(xx){
  sum(phi.features.e_sb(config=config.mat.i[xx,], crf=m2n) * m2n$par)
})

cbind(config.mat.i, config.energies, config.energies.e, config.energies.eC, config.energies.e.p)
config.energies - config.energies.e
config.energies - config.energies.eC
config.energies - config.energies.e.p
