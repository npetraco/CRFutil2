library(CRFutil2)

# Consider an Ising-like model with:
tau   <- c(2, 3.4)    # node weights
omega <- rbind(       # edge weights
  c(1,9),
  c(3,-2)
)

# one-body energies:
e1 <- c(ff0(1) %*% tau , ff0(2) %*% tau)
e1

# two-body energies
e2 <- rbind(
  c(ff0(1) %*% omega %*% ff0(1), ff0(1) %*% omega %*% ff0(2)),
  c(ff0(2) %*% omega %*% ff0(1), ff0(2) %*% omega %*% ff0(2))
)
e2
