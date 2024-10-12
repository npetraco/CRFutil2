# State labels used by CRF:
s1 <- 1
s2 <- 2

# Define feature function. State weights are implicitly 1:
f <- function(y){ as.numeric(c((y==s1),(y==s2))) }

# Try it out:
f(1)
f(2)
