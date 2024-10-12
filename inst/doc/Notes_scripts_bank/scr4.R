library(CRFutil2)

# Define 4 state labels for a Potts-like model:
s1 <- 1
s2 <- 2
s3 <- 3
s4 <- 4

# Try it out:
ff0(st = 1, ss.dim = 4, st.vec = c(s1, s2, s3, s4))
ff0(st = 2, ss.dim = 4, st.vec = c(s1, s2, s3, s4))
ff0(st = 3, ss.dim = 4, st.vec = c(s1, s2, s3, s4))
ff0(st = 4, ss.dim = 4, st.vec = c(s1, s2, s3, s4))
