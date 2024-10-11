library(rbenchmark)

# ff0 R tests

ff0(st = 3, ss.dim=4, w.vec=NULL, st.vec=NULL)
ff0(st = 3, ss.dim=4, w.vec=NULL, st.vec=c("a","b", "c", "d"))

ff0(st = "a", ss.dim=4, w.vec=NULL, st.vec=c("a","b", "c", "d"))
ff0(st = -1, ss.dim=2, w.vec=c(1,-1), st.vec=c(1,-1))

ff_C(1)
ff0_C(2)

ff0_C(st = 2, ss_dim = 5, w_vec = NULL)
ff0_C(2, w_vec = c(1,-1))

ff0_C(st = 2, ss_dim = 5, st_vec = c("a","b"))
ff0_C(st = "a", ss_dim = 5, st_vec = c(-1,2))
ff0_C(st = 2, ss_dim = 2, st_vec = c(-1,2))
ff0_C(st = "a", ss_dim = 5)
ff0_C(st = 1, ss_dim = 5)
ff0_C(st = 2, ss_dim = 5)
ff0_C(st = 6, ss_dim = 5)

ff0_C(st = "a", ss_dim = 2, st_vec = c("a","b"))
junk <- ff0_C(st = "a", ss_dim = 2, st_vec = c("a","b"))
as.character(junk[[1]]) # cannot coerce type 'char' to vector of type 'character'??

junk <- ff0_C(st = "a", ss_dim = 2, st_vec = c("b","c"))


ff0_C(st = 1, ss_dim = 2)

ste <- Sys.time()
ff0_C(st = "b", ss_dim = 2, st_vec = c("a","b"))
ete <- Sys.time()
ete-ste

len  <- 1000
tmes <- array(-1, c(len))
for(i in 1:len){
  ste <- Sys.time()
  #ff0(st = "b", ss.dim = 2, st.vec=c("a","b"))
  ff0_C(st = "b", ss_dim = 2, st_vec=c("a","b"))
  ete <- Sys.time()
  tmes[i] <- (ete-ste)[[1]]
}
hist(tmes*1e6)
mean(tmes*1e6)
sd(tmes*1e6)


benchmark(
  ff0_C(st = "b", ss_dim = 2, st_vec = c("a","b")),
  ff0(st = "b", ss.dim = 2, st.vec=c("a","b"))
  )[,1:3]
