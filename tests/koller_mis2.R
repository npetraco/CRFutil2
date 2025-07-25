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

dist.pot.info <- distribution.from.potentials(m2n)
dist.pot.info
