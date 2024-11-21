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
s1 <- "up"
s2 <- "dn"

# Enumerate all the state configurations
config.mat <- expand.grid(c(s1,s2),c(s1,s2),c(s1,s2))
colnames(config.mat) <- c("A","B","C")
config.mat


# Test: CRFutil2 function to compute a configuration energy.
config.energy(config = c("dn","up","dn"),
              edges.mat = edges,
              one.nlp = list(tauA,tauB,tauC),
              two.nlp = list(omegaAB,omegaAC,omegaBC), # use same order as edges!
              ff = ff1, nss.vec=c(s1,s2))



# Define a convenience function wrapper:
ener.func <- function(config) {
  engy <-config.energy(
    config = config,
    edges.mat = edges,
    one.nlp = list(tauA,tauB,tauC),
    two.nlp = list(omegaAB,omegaAC,omegaBC),
    ff = ff1, nss.vec=c(s1,s2))
  return(engy)
}

# Test:
ener.func(c("dn","up","dn"))

config.energy(
  config = as.data.frame(config.mat[6,]),
  edges.mat = edges,
  one.nlp = list(tauA,tauB,tauC),
  two.nlp = list(omegaAB,omegaAC,omegaBC),
  ff = ff1, nss.vec=c(s1,s2))

config.energies <- sapply(1:nrow(config.mat), function(xx){ener.func(config.mat[xx,])})
cbind(config.mat, config.energies)

# Cf. slide XX for input

# All configuration energies:
config.energies <- sapply(1:nrow(config.mat), function(xx){ener.func(config.mat[xx,])})
prodPots        <- exp(-config.energies) # Product Potentials
Z               <- sum(prodPots)         # The Partition function (by brute force...)
Prs             <-prodPots/Z             # Configuration probabilities

Z
sum(Prs)
cbind(config.mat, config.energies, prodPots, Prs)
