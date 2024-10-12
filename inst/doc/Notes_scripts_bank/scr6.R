#Consider an Potts-like model with node/edge weights:
tau   <- c(2, 3.4, 3)
omega <- rbind(
  c(1,  9,   4.1),
  c(3, -2,  -2.3),
  c(6, -5.7, 3  )
)

# Define states and feature function:
s1 <- 1
s2 <- 2
s3 <- 3
f  <- function(y){ as.numeric(c((y==s1),(y==s2),(y==s3))) }

# one-body energies:
e1 <- c(ff0(1, ss.dim = 3) %*% tau, ff0(2, ss.dim = 3) %*% tau, ff0(3, ss.dim = 3) %*% tau)
e1

# two body energies
e2 <- rbind(
  c(ff0(1,3) %*% omega %*% ff0(1,3), ff0(1,3) %*% omega %*% ff0(2,3), ff0(1,3) %*% omega %*% ff0(3,3)),
  c(ff0(2,3) %*% omega %*% ff0(1,3), ff0(2,3) %*% omega %*% ff0(2,3), ff0(2,3) %*% omega %*% ff0(3,3)),
  c(ff0(3,3) %*% omega %*% ff0(1,3), ff0(3,3) %*% omega %*% ff0(2,3), ff0(3,3) %*% omega %*% ff0(3,3))
)
e2
