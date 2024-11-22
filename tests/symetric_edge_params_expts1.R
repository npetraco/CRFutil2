# r < c
rs <- 4
cs <- 5
tm <- array(1:(rs*cs), c(rs,cs))
tm
tm[1:rs, 1:rs]
tm[1:rs, (rs+1):cs]

# r > c
rs1 <- 4
cs1 <- 3
tm1 <- array(1:(rs1*cs1), c(rs1,cs1))
tm1
tm1[1:cs1, 1:cs1]
tm1[(cs1+1):rs1, 1:cs1]

# Make reference element bottom part of square segment??
rs <- 4
cs <- 6
tm <- array(-1, c(rs,cs))
# **** if r < c
tm.sub.sq <- tm[1:rs, 1:rs]        # square
tm.sub.cp <-tm[1:rs, (cs-rs+1):cs] # complement

# if ii != jj
count <- 1
for(i in 1:rs){
  for(j in 1:rs){
    if(i>=j) {
      tm.sub.sq[i,j] <- count
      count <- count + 1
    }
  }
}
tm.sub.sq[rs,]  <- tm.sub.sq[rs,] - 1
tm.sub.sq[rs,1] <- 0
tm.sub.sq



# if ii == jj
count <- 1
for(i in 1:rs){
  for(j in 1:rs){
    if(i>j) {
      tm.sub.sq[i,j] <- count
      count <- count + 1
    }
  }
}
tm.sub.sq[rs,]  <- tm.sub.sq[rs,] - 1
tm.sub.sq[rs,1] <- 0
tm.sub.sq

tm.sub.sq <- tm.sub.sq + 1
tm.sub.sq[rs,1] <- 0
diag(tm.sub.sq) <- 1
tm.sub.sq

# Symmetrize
tm.sub.sq[upper.tri(tm.sub.sq)] <- t(tm.sub.sq)[upper.tri(tm.sub.sq)]
tm.sub.sq

# Then state at max param + 1 of the square and fill in tm.sub.cp sequentially
