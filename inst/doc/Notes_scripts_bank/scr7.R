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
config.mat <- expand.grid(c(s1,s2),c(s1,s2),c(s1,s2))
colnames(config.mat) <- c("A","B","C")
config.mat

# Energy(A=2,B=1,C=2)
e1.212 <- ff0(2)%*%tauA + ff0(1)%*%tauB + ff0(2)%*%tauC
e2.212 <- ff0(2)%*%omegaAB%*%ff0(1) + ff0(1)%*%omegaBC%*%ff0(2) + ff0(2)%*%omegaAC%*%ff0(2)
e.212  <- e1.212 + e2.212
e.212
