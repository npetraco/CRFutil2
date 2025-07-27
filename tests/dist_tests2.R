library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+D:E
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
#s1 <- "up"; s2 <- "dn"
s1 <- 1; s2 <- 2

star  <- make.empty.field(graph.eq             = grphf,
                          num.states           = 2,
                          state.names          = rep(list(c(s1,s2)), nrow(adj)),
                          parameterization.typ = "standard",
                          plotQ                = T)

# Make up some random parameters and insert them into the model:
set.seed(1)
#star.rthetas <- rnorm(n = star$num.par, mean = 0, sd = 1)
star.rthetas <- c(-1.0175307, -0.5231160, 0.2935024, 2.2919458, -1.3758152, 2.1794589, 2.8376212, 0.6668515, 0.5284179, -2.7202966, -1.3493627, -1.5398523, 0.7862374, -0.4721660)
insert.params.and.pots(star, params.samp = star.rthetas)
dump.crf(star)

# Decorate  potentials and energies with gRbase annotations to use later:
star$gR <- make.gRbase.potentials(crf=star)

dist.pot.infox <- distribution.from.potentials(star)
dist.pot.info
