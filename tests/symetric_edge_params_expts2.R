edge.param.indices.helper <- function(nr, nc, pmax.idx, iijj.eqQ=T) {

  im    <- array(-1, c(nr,nc))   # parameter index matrix

  if(nr < nc) {
    im.sq <- im[1:nr, 1:nr]      # square part
    im.cp <- im[1:nr, (nr+1):nc] # complement
  } else if(nr > nc) {
    im.sq <- im[1:nc, 1:nc]      # square part
    im.cp <- im[(nc+1):nr, 1:nc] # complement
  } else {
    im.sq <- im[1:nr, 1:nc]      # nr == nc so parameter index matrix is only square
  }

  # Handel the square part of the parameter index matrix
  nsq   <- nrow(im.sq) # square part dimension
  if(iijj.eqQ == T){
    # diagonal elements equal ii == jj
    count <- pmax.idx
    for(i in 1:nsq){
      for(j in 1:nsq){
        if(i>j) {
          im.sq[i,j] <- count
          count <- count + 1
        }
      }
    }
    # Adjust the last row so bottom left corner is 0 and re-index:
    im.sq[nsq,]  <- im.sq[nsq,] - 1
    #im.sq[nr,1] <- 0
    im.sq       <- im.sq + 1
    im.sq[nsq,1] <- 0
    diag(im.sq) <- pmax.idx

  } else {
    # if diagonal elements not equal ii != jj
    count <- pmax.idx
    for(i in 1:nsq){
      for(j in 1:nsq){
        if(i>=j) {
          im.sq[i,j] <- count
          count      <- count + 1
        }
      }
    }
    im.sq[nsq,]  <- im.sq[nsq,] - 1
    im.sq[nsq,1] <- 0

  }

  # Symmetrize here before handeling complenent:
  im.sq[upper.tri(im.sq)] <- t(im.sq)[upper.tri(im.sq)]
  #print(im.sq)

  # Handel the rectangular complement
  if(nr < nc) {
    count <- max(im.sq) + 1
    im.cp <- matrix( count:(count + prod(dim(im.cp)) - 1), nrow(im.cp), ncol(im.cp), byrow=T ) # Use row-major order
    im    <- cbind(im.sq,im.cp)
  } else if(nr > nc) {
    count <- max(im.sq) + 1
    im.cp <- matrix( count:(count + prod(dim(im.cp)) - 1), nrow(im.cp), ncol(im.cp), byrow=T ) # Use row-major order
    im    <- rbind(im.sq,im.cp)
  } else {
    im <- im.sq
  }

  #list(im)

}

edge.param.indices.helper(7,4, pmax.idx = 3, iijj.eqQ = T)
edge.param.indices.helper(4,7, pmax.idx = 3, iijj.eqQ = T)
edge.param.indices.helper(7,7, pmax.idx = 3, iijj.eqQ = T)

edge.param.indices.helper(2,2, pmax.idx = 3, iijj.eqQ = F)
edge.param.indices.helper(3,3, pmax.idx = 3, iijj.eqQ = F)
