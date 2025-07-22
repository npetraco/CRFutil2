library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
s1 <- "up"; s2 <- "dn"; s3 <- "eh"

m2n  <- make.empty.field(graph.eq             = grphf,
                         num.states           = c(2,3),
                         state.names          = list(c(s1,s2), c(s1,s3,s2)),
                         parameterization.typ = "standard",
                         plotQ                = T)
# Make up some random parameters and insert them into the model:
par.vec <- -log(c(0.25,0.5,0.75,1,1,1))
insert.params.and.pots(m2n, params.samp = par.vec)
dump.crf(m2n)
make.gRbase.potentials(m2n)
