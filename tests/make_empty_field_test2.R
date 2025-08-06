library(CRFutil2)

# Graph formula:
grphf <- ~A:B:C
adj   <- ug(grphf, result="matrix")

s1 <- "y"; s2 <- "n"

# Parameterization:
np <- array(0, c(3,2,1))
np[1,1,] <- 1
np[2,1,] <- 2
np[3,1,] <- 2

epa <- array(0, c(2,2,1))
ep  <- list(epa,epa,epa)
ep[[1]][1,1,1] <- 3
ep[[1]][2,2,1] <- 3
ep[[2]][1,1,1] <- 2
ep[[2]][2,2,1] <- 4
ep[[3]][1,1,1] <- 5
ep[[3]][2,2,1] <- 6
ep


tc  <- make.empty.field2_sb(graph.eq             = grphf,
                            num.states           = 2,
                            state.names          = list(c(s1,s2),c(s1,s2),c(s1,s2)),
                            parameterization.typ = "custom",
                            node.par             = np,
                            edge.par             = ep,
                            plotQ                = T)
dump.crf(tc)
