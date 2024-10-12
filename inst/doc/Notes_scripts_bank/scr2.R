library(CRFutil2)

# Make up some state labels:
s1 <- "a"
s2 <- "b"

# Define a weight vector:
w <- c(2,-1)

# A feature function built into CRFutil2:
ff0(st = "b", ss.dim = 2, w.vec = w, st.vec = c(s1,s2))
