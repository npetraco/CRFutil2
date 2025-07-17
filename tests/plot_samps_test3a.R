library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
s1 <- "up"; s2 <- "dn"; s3 <- "eh"
#s1 <- 1; s2 <- 2; s3 <- 3
#s1 <- "1"; s2 <- "2"; s3 <- "3"

m2n  <- make.empty.field(graph.eq             = grphf,
                         #num.states           = 2,
                         #num.states           = 3,
                         num.states           = c(2,3),
                         #state.names          = rep(list(c(s1,s2)), nrow(adj)),
                         #state.names          = rep(list(c(s1,s2,s3)), nrow(adj)),
                         #state.names          = list(c(s1,s2), c(s1,s3,s2)),  # ** DON'T DO THIS WITH NUMBER NAME STATES. KEEP THEM IN NUMERICAL ORDER OF CAN RUN INTO PROBLEMS WITH CRF SAMPLER REORDERING THE STATES!
                         state.names          = list(c(s1,s2), c(s1,s2,s3)),
                         parameterization.typ = "standard",
                         plotQ                = T)
# Make up some random parameters and insert them into the model:
#par.vec <- -log(c(0.5,1,1))       # Total independence and 50/50 on each node is c(1,1,1)
#par.vec <- -log(c(1,1,1,1,1,1,1)) # Total independence and 50/50 on each node
par.vec <- -log(c(1.25,0.5,0.75, 1.1,1.5,0.7))
insert.params.and.pots(m2n, params.samp = par.vec)
dump.crf(m2n)


infer.junction(m2n)
set.seed(1)
m2n.samp <- sample.crf(m2n, 5000, sample.junction, dress.sampleQ = T)
plot.marginal.sample(m2n.samp, m2n)
t(infer.junction(m2n)$node.bel)

plot.configuration.sample(m2n.samp)

