library(rbenchmark)

ff1_C(st = 2, ss_dim=2, w_vec=c(1,1), st_vec=c(1,2))
ff2_C(st = 2, ss_dim=2, st_vec=c(1,2))


benchmark(
  ff1_C(st = 2, ss_dim=2, w_vec=c(1,1), st_vec=c(1,2)),
  ff2_C(st = 2, ss_dim=2, st_vec=c(1,2)),
  ff0_C(st = 2, ss_dim = 2, st_vec = c(1,2)),
  ff0(st = 2, ss.dim = 2, st.vec=c(1,2))
)[,1:3]


len  <- 1000
tmesR <- array(-1, c(len))
tmes0 <- array(-1, c(len))
tmes1 <- array(-1, c(len))
tmes2 <- array(-1, c(len))

for(i in 1:len){
  ste <- Sys.time()
  ff0(st = 2, ss.dim = 2, w.vec = c(1,1), st.vec=c(1,2))
  ete <- Sys.time()
  tmesR[i] <- (ete-ste)[[1]]
}

for(i in 1:len){
  ste <- Sys.time()
  ff0_C(st = 2, ss_dim = 2, w_vec = c(1,1), st_vec=c(1,2))
  ete <- Sys.time()
  tmes0[i] <- (ete-ste)[[1]]
}

for(i in 1:len){
  ste <- Sys.time()
  ff1_C(st = 2, ss_dim = 2, w_vec = c(1,1), st_vec=c(1,2))
  ete <- Sys.time()
  tmes1[i] <- (ete-ste)[[1]]
}

for(i in 1:len){
  ste <- Sys.time()
  ff2_C(st = 2, ss_dim = 2, st_vec=c(1,2))
  ete <- Sys.time()
  tmes2[i] <- (ete-ste)[[1]]
}



hist(tmesR*1e6)
mean(tmesR*1e6)
sd(tmesR*1e6)

hist(tmes0*1e6)
mean(tmes0*1e6)
sd(tmes0*1e6)

hist(tmes1*1e6)
mean(tmes1*1e6)
sd(tmes1*1e6)

hist(tmes2*1e6)
mean(tmes2*1e6)
sd(tmes2*1e6)

s1 <- "a"
s2 <- "b"
ff0(st = "b", ss.dim = 2, st.vec = c(s1,s2))

# Make up some state labels:
s1 <- "a"
s2 <- "b"

# Define a weight vector:
w <- c(2,-1)

# A feature function built into CRFutil2:
ff0(st = "b", ss.dim = 2, w.vec = w, st.vec = c(s1,s2))


s1 <- 1
s2 <- 2
s3 <- 3
s4 <- 4
ff0(st = 1, ss.dim = 4, st.vec = c(s1,s2, s3, s4))
ff0(st = 2, ss.dim = 4, st.vec = c(s1,s2, s3, s4))
ff0(st = 3, ss.dim = 4, st.vec = c(s1,s2, s3, s4))
ff0(st = 4, ss.dim = 4, st.vec = c(s1,s2, s3, s4))

ff0(st = 1, ss.dim = 4)
ff0(st = 2, ss.dim = 4)
ff0(st = 3, ss.dim = 4)
ff0(st = 4, ss.dim = 4)
ff0(st = 5, ss.dim = 4)

ff0(st = 2)
