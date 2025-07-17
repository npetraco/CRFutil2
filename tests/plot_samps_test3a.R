library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
s1 <- "up"; s2 <- "dn"; s3 <- "eh"

m2n  <- make.empty.field(graph.eq             = grphf,
                         #num.states           = 2,
                         #num.states           = 3,
                         num.states           = c(2,3),
                         #state.names          = rep(list(c(s1,s2)), nrow(adj)),
                         #state.names          = rep(list(c(s1,s2,s3)), nrow(adj)),
                         state.names          = list(c(s1,s2), c(s1,s3,s2)),
                         parameterization.typ = "standard",
                         plotQ                = T)
# Make up some random parameters and insert them into the model:
#par.vec <- -log(c(0.5,1,1))       # Total independence and 50/50 on each node is c(1,1,1)
#par.vec <- -log(c(1,1,1,1,1,1,1)) # Total independence and 50/50 on each node
par.vec <- -log(c(0.25,0.5,0.75,1,1,1))
insert.params.and.pots(m2n, params.samp = par.vec)
dump.crf(m2n)

# Raw sample (raw case list)
infer.junction(m2n)
set.seed(1)
m2n.samp <- sample.crf(m2n, 1000, sample.junction, dress.sampleQ = T)
plot.marginal.sample2(m2n.samp, m2n)

#dimnames = list(c("states","nodes"))
