library(CRFutil2)
library(DescTools)
library(entropy)
library(philentropy)

# Edge association measures

# Graph formula for simple two node field:
grphf <- ~A:B
adj   <- ug(grphf, result = "matrix")

# Define node state space and configuration space:
#s1 <- 0; s2 <- 1
#s1 <- 1; s2 <- 2
s1 <- "y"; s2 <- "n"

ea  <- make.empty.field(graph.eq             = grphf,
                        num.states           = 2,
                        state.names          = list(c(s1,s2),c(s1,s2)),
                        parameterization.typ = "general",
                        plotQ                = T)

par.vec <- -log(c(1,1,
                  #100,1,100    # Very strong agree?
                  10,1,10      # Strong agree?
                  #3,1,3        # Usually agree?
                  #1.5,1,1.5    # Usually agree?
                  #1.05,1,1.05    # Maybe agree?
                  #0.01,1,0.01  # Strong disagree?
                  #1,1,1        # independent?
))
insert.params.and.pots(ea, params.samp = par.vec)
make.gRbase.potentials(ea)
dump.crf(ea)

# Get a sample of configs:
ea.samp <- sample.crf(crf               = ea,
                      size              = 1000,
                      crf.sample.method = sample.junction,
                      dress.sampleQ     = T,
                      data.frameQ       =T)
plot.marginal.sample(ea.samp, ea)
plot.configuration.sample(ea.samp, crf = ea, num.top.configs = 3)

#Check exact
dist.pot.info <- distribution.from.potentials(ea)
dist.pot.info

nn <- c("A","B")
tb <- tabNew.crf(nn, vals=ea.samp[nn], ea)
tb

# Chisq test of independence
chisq.test(tb, correct = T)

# Fisher test of independence
fisher.test(tb)

CramerV(tb)
Lambda(tb)
#Desc(tb) #????

# Check Pr(A,B) = Pr(A|B)Pr(B) ?=? Pr(A)Pr(B)
pab <- tabNew.crf(nn, vals=ea.samp[nn], ea, normalize="all")
pab

# Pr(B)
pb <- tabMarg(pab,"B")
pb

# Pr(A)
pa <- tabMarg(pab,"A")
pa

# Pr(A|B) = Pr(A,B)/Pr(B)
pagb <- tabDiv(pab,pb)
pagb
#tabNew.crf(nn, vals=ea.samp[nn], ea, normalize="first") # Pr(A|B) too

tabMult(pagb, pb) # Pr(A|B)Pr(B)
tabMult(pa, pb)   # Pr(A)Pr(B)

# Check Kullback-Leibler for distance??
#y1 <- as.numeric(round(1000*tabMult(pagb, pb)))
#y2 <- as.numeric(round(1000*tabMult(pa, pb)))
y1 <- as.numeric(tabMult(pagb, pb))
y2 <- as.numeric(tabMult(pa, pb))
KL.empirical(y1,y2)
KL.plugin(y1,y2)
chi2indep.empirical(tb)

library(philentropy)
kullback_leibler_distance(y1,y2, testNA = T, unit = "log", epsilon = 0.00001)


# Repeat using exact marginals:
bel.ab <- infer.junction(ea)$edge.bel[[1]]
bel.ab
peab   <- tabNew.crf(nn, vals=as.numeric(bel.ab), ea, normalize="all")
peb    <- tabMarg(peab,"B")
pea    <- tabMarg(peab,"A")
peagb  <- tabDiv(peab,peb)

tabMult(peagb, peb) # Pr(A|B)Pr(B)
tabMult(pea, peb)   # Pr(A)Pr(B)

y1e <- as.numeric(tabMult(peagb, peb))
y2e <- as.numeric(tabMult(pea, peb))
KL.empirical(y1e,y2e)
KL.plugin(y1e,y2e)
kullback_leibler_distance(y1e,y2e, testNA = T, unit = "log", epsilon = 0.00001)
