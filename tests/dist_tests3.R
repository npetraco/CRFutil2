library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B+A:C+A:D+A:E+B:C+B:D+B:E+C:D+D:E
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
s1 <- "up"; s2 <- "dn"
#s1 <- 1; s2 <- 2

star  <- make.empty.field(graph.eq             = grphf,
                          num.states           = 2,
                          state.names          = rep(list(c(s1,s2)), nrow(adj)),
                          parameterization.typ = "standard",
                          plotQ                = T)

# Make up some random parameters and insert them into the model:
#set.seed(1)
#star.rthetas <- rnorm(n = star$num.par, mean = 0, sd = 1)
star.rthetas <- c(-1.0175307, -0.5231160, 0.2935024, 2.2919458, -1.3758152, 2.1794589, 2.8376212, 0.6668515, 0.5284179, -2.7202966, -1.3493627, -1.5398523, 0.7862374, -0.4721660)
insert.params.and.pots(star, params.samp = star.rthetas)
dump.crf(star)

# Decorate  potentials and energies with gRbase annotations to use later:
star$gR <- make.gRbase.potentials(crf=star)

je <- distribution.from.energies(star)
jp <- distribution.from.potentials(star)
je$config.probs
jp$config.probs

je$logZ
jp$logZ

je$config.probs$prob
jp$config.probs$prob

je$config.probs$prob - jp$config.probs$prob

# [1] "edge.name.tab"
#      edge.idx n1.idx n2.idx  n1.name n2.name
# 1        1      1      2       A       B
# 2        2      1      3       A       C
# 3        3      1      4       A       D
# 4        4      1      5       A       E
# 5        5      2      3       B       C
# 6        6      2      4       B       D
# 7        7      2      5       B       E
# 8        8      3      4       C       D
# 9        9      4      5       D       E
#-1.0175307, -0.5231160, 0.2935024, 2.2919458, -1.3758152,
#2.1794589, 2.8376212, 0.6668515, 0.5284179, -2.7202966, -1.3493627, -1.5398523, 0.7862374, -0.4721660)

#2.76635536  1.68727702  0.74564743  0.10106961  3.95830222
#0.11310271  0.05856481  0.51332223  0.58953694 15.18482540  3.85496798  4.66390136  0.45555565  1.60346354

