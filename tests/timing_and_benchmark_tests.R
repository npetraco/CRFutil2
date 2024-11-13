library(CRFutil2)
library(rbenchmark)
library(microbenchmark)
library(RcppClock)


bmi <- benchmark(
  ff0(  st = "b", ss.dim =  2, st.vec  = c("a","b")),
  ff0_C(st = "b", ss_dim =  2, st_vec  = c("a","b")),
  ff1(  ns = "b", nss.dim = 2, nss.vec = c("a","b")),
  ff13_C(ns = 2,   nss_dim = 2),
  ff02_C(st = 2,   st_vec = c(1,2)),
  ff1_C(ns = "b", dots = list(nss.dim=2, nss.vec=c("a", "b")) ),
  replications = 1000, order="elapsed"
)
bmis <- bmi[,c(1,4,3,5)]


mbi <- microbenchmark(
  ff0(   st = "b", ss.dim =  2, st.vec  = c("a","b")),
  ff0_C( st = "b", ss_dim =  2, st_vec  = c("a","b")),
  ff1(   ns = "b", nss.dim = 2, nss.vec = c("a","b")),
  ff13_C(ns = 2,   nss_dim = 2),
  ff02_C(st = 2,   st_vec = c(1,2)),
  ff1_C( ns = "b", dots = list(nss.dim=2, nss.vec=c("a", "b")) ),
  times=1000L, unit = "microsecond")
#mbi
#summary(mbi)[,c(1,2,7,3,6,4,5)]
mbis <- summary(mbi)[,c(1,3,6,5,4)]
mbis <- mbis[order(mbis[,5]), ] # Order rows by median time

mbis
bmis

# ff1_times_C(ns = "b", dots = list(nss.dim=2, nss.vec=c("a", "b")) )
# sect_times
# summary(sect_times)
# plot(sect_times)

ts.mat <- array(-1, c(1000,8))
for(i in 1:1000){
  ff1_times_C(ns = "b", dots = list(nss.dim=2, nss.vec=c("a", "b")) )
  ts.mat[i,] <- summary(sect_times)[,2]
}

colnames(ts.mat) <- c("sect1","sect2","sect2a","sect2b","sect2c","sect2d","sect3","sect4")
apply(ts.mat, MARGIN = 2, FUN = mean)
apply(ts.mat, MARGIN = 2, FUN = median)
apply(ts.mat, MARGIN = 2, FUN = sd)
apply(ts.mat, MARGIN = 2, FUN = min)
apply(ts.mat, MARGIN = 2, FUN = max)

library(lattice)
ts.mat2 <- stack(data.frame(ts.mat))
histogram(~values | factor(ind), data = ts.mat2)


