library(CRFutil2)

# Graph formula for Star field:
grphf <- ~A:B
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
s1 <- "up"; s2 <- "dn"; s3 <- "eh"

m2n  <- make.empty.field(graph.eq             = grphf,
                         num.states           = 2,
                         #num.states           = 3,
                         state.names          = rep(list(c(s1,s2)), nrow(adj)),
                         #state.names          = rep(list(c(s1,s2,s3)), nrow(adj)),
                         parameterization.typ = "standard",
                         plotQ                = T)

# Make up some random parameters and insert them into the model:
par.vec <- -log(c(0.5,1,1)) # Total independence and 50/50 on each node
#par.vec <- -log(c(1,1,1,1,1,1,1)) # Total independence and 50/50 on each node
insert.params.and.pots(m2n, params.samp = par.vec)
dump.crf(m2n)


# **** FIX UP BELOW TO A SIMULATION FUNCTION THAT CAN USE ANY OF CRFs SAMPLERS AND NAMES THE SAMPLE
# STATE NAMES CORRECTLY

# Raw sample (raw case list)
infer.junction(m2n)
m2n.samp <- sample.junction(m2n, 1000)
m2n.samp

plot.marginal.sample(m2n.samp)
class(m2n.samp[1])
nnj <- c("A","B")
m2n.samptt <- m2n.samp
colnames(m2n.samptt) <- nnj
head(m2n.samptt)
plot.marginal.sample(m2n.samptt)
m2n.sampd <- dress.sample(crf = m2n, samples = m2n.samp)
plot.marginal.sample(m2n.sampd)

unique(as.vector(m2n.sampd))

# Aggregated Frequency Table
as.data.frame(ftable(data.frame(m2n.samp)))

# **** Make plot also for aggregated frequency table
as.data.frame(ftable(data.frame(m2n.sampd)))

# Contingency table
m2n.ct <- xtabs(~., data=data.frame(m2n.samp))
m2n.ct

# Chi-square independence test on contingency table:
chisq.test(m2n.ct, correct = T)


compute.full.distribution(m2n.samp) # **** ADD THIS
infer.junction(m2n)
