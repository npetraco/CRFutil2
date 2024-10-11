# Make up some state labels:
s1 <- "a"
s2 <- "b"

# Define a weight vector:
w <- c(2,-1)

# Define feature function:
f <- function(y){ w * as.numeric(c((y==s1),(y==s2))) }

# Try it out:
f("a")
f("b")

# What happens for an undefined state?
f(12)
