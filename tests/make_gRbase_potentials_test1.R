library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+D:E
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
# s1 <- 1
# s2 <- 2
# s1 <- "up"
# s2 <- "down"
sA <- c("a1","a2","a3")
sB <- c("b1","b2")
sC <- c("c1","c2","c3","c4")
sD <- c("d1","d2","d3")
sE <- c("e1","e2","e3","e4","e5")

star  <- make.empty.field(graph.eq             = grphf,
                          num.states           = c(3,2,4,3,5),
                          state.names          = list(sA,sB,sC,sD,sE),
                          #state.names          = NULL,
                          parameterization.typ = "standard",
                          plotQ                = T)

# Make up some random parameters and insert them into the model:
set.seed(1)
star.rthetas <- rnorm(n = star$num.par, mean = 0, sd = 1)
insert.params.and.pots(star, params.samp = star.rthetas)
dump.crf(star)

make.gRbase.potentials(star)
star$node.pot.list

# Enumerate all the state configurations
config.mat <- expand.grid(c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2),c(s1,s2))
colnames(config.mat) <- c("A","B","C","D","E")

star$node.name.tab
star
