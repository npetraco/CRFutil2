library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B+B:C+C:D+D:A+D:B+C:E
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space (weird on purpose):
sA1 <- "A1"; sA2 <- "A2"
sB1 <- "B1"; sB2 <- "B2"; sB3 <- "B3"
sC1 <- 1;    sC2 <- 2
sD1 <- "D1"; sD2 <- "D2"; sD3 <- "D3"; sD4 <- "D4"
sE1 <- 1;    sE2 <- 3;    sE3 <- 2

wp  <- make.empty.field(graph.eq    = grphf,
                        num.states  = c(2,3,2,4,3),
                        state.names = list(c(sA1,sA2),
                                           c(sB1,sB2,sB3),
                                           c(sC1,sC2),
                                           c(sD1,sD2,sD3,sD4),
                                           c(sE1,sE2,sE3)),
                        parameterization.typ = "standard",
                        plotQ                = T)
dump.crf(wp)

# Make up some random parameters and insert them into the model:
set.seed(1)
wp.rthetas <- rnorm(n = wp$num.par, mean = 0, sd = 1)
wp.rthetas
length(wp.rthetas)
insert.params.and.pots(wp, params.samp = wp.rthetas)

# Decorate  potentials and energies with gRbase annotations to use later:
make.gRbase.potentials(crf=wp, includeQ = T)
dump.crf(wp)

# Get a sample of configs:
wp.samp <- sample.crf(crf = wp, size = 1000, crf.sample.method = sample.junction, dress.sampleQ = T, data.frameQ=T)
plot.marginal.sample(wp.samp, wp)
plot.configuration.sample(wp.samp, crf = wp, num.top.configs = 3)

