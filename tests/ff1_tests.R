ff1(ns=3, nss.dim=2)

ff1(ns = 3, nss.dim = 4, nss.vec = c(0,1,2,3))
ff1(ns = "a", nss.vec = c("a","b"))
ff1(ns = "ID", nss.dim = 3, nss.vec = c("ID","exclude","inconclusive"))
ff1(1)
ff1(1,3)
ff1(ns = "b")            # Throws error
ff1(ns = 5)              # Throws error
ff1(ns = 5, nss.dim = 4) # Throws error

compare_element("b", c("a", "b", "c"))
compare_element("b", c(1, "b", "c"))
compare_element(1, c("1", "2", "3"))
compare_element("1", c(1,2,3))
compare_element(1, c(1,2,3))
compare_element(5, c(1,2,3))

ff1_C(ns = 1, dots = list(a=1, nss.dim=2, nss.vec=c(1,"b", "c")) )
ff1_C(ns = "1", dots = list(a=1, nss.dim=2, nss.vec=c(1,"b", "c")) )
ff1_C(ns = "b", dots = list(a=1, nss.dim=2, nss.vec=c(1,"b", "c")) )
ff1_C(ns = "d", dots = list(a=1, nss.dim=2, nss.vec=c(1,"b", "c")) ) # Throws error

library(rbenchmark)
benchmark(
  ff0_C(st = "b", ss_dim = 2, st_vec = c("a","b")),
  ff0(st = "b", ss.dim = 2, st.vec=c("a","b")),
  ff1_C(ns = "b", dots = list(nss.dim=2, nss.vec=c("a", "b")) )
)[,1:3]


len  <- 1000
tmes <- array(-1, c(len))
for(i in 1:len){
  ste <- Sys.time()
  #ff0(st = "b", ss.dim = 2, st.vec=c("a","b"))
  ff0_C(st = "b", ss_dim = 2, st_vec=c("a","b"))
  #ff1_C(ns = "b", dots = list(nss.dim=2, nss.vec=c("a", "b")) )
  ete <- Sys.time()
  tmes[i] <- (ete-ste)[[1]]
}
hist(tmes*1e6)
mean(tmes*1e6)
sd(tmes*1e6)
