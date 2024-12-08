# To cut down on the amount of work we have to do, use CRFutil2:
library(CRFutil2)

# Define the edge connectivity matrix
edges <- rbind(
  c(1,2), #AB
  c(1,3), #AC
  c(2,3)  #BC
)

# node weights:
tauA <- c( 1,    -1.3)
tauB <- c(-0.85, -2.4)
tauC <- c(3.82,   1.4)

# edge weights:
omegaAB <- rbind(
  c( 3.5, -1.4),
  c(-1.4,  2.5)
)
omegaBC <- rbind(
  c( 2.6, 0.4),
  c( 0.4, 2.5)
)
omegaAC <- rbind(
  c(-0.6,  1.2),
  c( 1.2, -0.6)
)

# Define states and feature function:
s1 <- 1
s2 <- 2

# Enumerate all the state configurations
config.mat <- as.matrix(expand.grid(c(s1,s2),c(s1,s2),c(s1,s2)))
colnames(config.mat) <- c("A","B","C")
config.mat

# Test: CRFutil2 function to compute a configuration energy.
# config.energy(config = c(2,1,2),
#               edges.mat = edges,
#               one.nlp = list(tauA,tauB,tauC),
#               two.nlp = list(omegaAB,omegaAC,omegaBC), # use same order as edges!
#               ff = ff1, nss.vec=c(s1,s2))
config.energy.e(config = c(2,1,2),
                edges.mat = edges,
                one.nlp = list(tauA,tauB,tauC),
                two.nlp = list(omegaAB,omegaAC,omegaBC) # use same order as edges!
               )

# Define a convenience function wrapper:
ener.func <- function(config) {
  engy <-config.energy.e(
    config = config,
    edges.mat = edges,
    one.nlp = list(tauA,tauB,tauC),
    two.nlp = list(omegaAB,omegaAC,omegaBC))
  return(engy)
}

# Test:
ener.func(c(2,1,2))

# Use to compute all configuration energies:
config.energies <- sapply(1:nrow(config.mat), function(xx){ener.func(config.mat[xx,])})
cbind(config.mat, config.energies)

# Cf. slide XX for input

# All configuration energies:
prodPots        <- exp(-config.energies) # Product Potentials
Z               <- sum(prodPots)         # The Partition function (by brute force...)
Prs             <-prodPots/Z             # Configuration probabilities

Z                                        # Partition function value
sum(Prs)                                 # Are the probabilities normalized?
cbind(config.mat, config.energies, prodPots, Prs)
plot(1:nrow(config.mat), Prs, typ="h", xlab="X config #", ylab="Pr(X)", main="Configuration Probabilities")
