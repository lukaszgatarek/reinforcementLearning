g <- function(x, v){
  return(x * v)
}

# weight
w <- c(20,18,14,12,10,16,22,24)
# value
v <- c(72,60,40,27,20,50,85,96)

# we have N types of items
N <- length(w)

# consider a case with two functions

Z <- seq(1, 100, 1)

# get the maximum Z
zMax <- 100

# we need the value function f

f <- matrix(0, zMax, N + 1) # + 1 corresponds to the 0 as the initial index for i
x <- array(0, dim = c(zMax, N + 1, N + 1)) #each iteration the previous values of optimal values also change, 
# so we need to introduce an additional dimension to keep the state of 1 : (N + 1) variables at the state i in (1, N + 1)

for (i in 1:N){
  z <- 1
  print(i)
  
  while (z <= zMax){
    
    alpha <- floor(z / w[i])
    
    auxiliary <- matrix(0, alpha + 1, 1)
    
    for (xi in 0:alpha){
      # xi is just potential x[i] value to test for the maximum
      if ( (z - w[i] * xi) == 0){
        # in this case we have no more capacity, so we will just not earn more, so we put 0 for the f_(N-1) 
        auxiliary[xi + 1, 1] <- g(xi, v[i])
        #x[z, 1:i] <- 0
      } else {
        auxiliary[xi + 1, 1] <- g(xi, v[i]) + f[z - (w[i] * xi), i]    
      }
    } 
    # get the xi for which the value function is the biggest 
    maximizingX <- which(auxiliary == max(auxiliary, na.rm = TRUE) )
    
    if (length(maximizingX) > 1 ){  
      
      wWith0 <- c(0, w) 
      
      x[z , i + 1, i + 1] <- maximizingX[(which(wWith0[maximizingX] == min(wWith0[maximizingX]) ) )] - 1 # -1 corresponds to the zero which is also availale there
      f[z , i + 1] <- max(auxiliary, na.rm = TRUE)
      
    } else {
      
      x[z , i + 1, i + 1] <- which(auxiliary == max(auxiliary, na.rm = TRUE) ) - 1
      
      # if we use some value function obtainable from the items of the previous (i-1) types,
      # then we need to copy the history of their choices as well, as to get there, we need that specific program
      
      if (z - (w[i] * x[z , i + 1, i + 1]) > 0) {
        # always copy the ith state of the variables 1:i while optimizing the (i+1)-th state !!!!!
        x[z, 1 : i, i + 1] <- x[z - (w[i] * x[z , i + 1, i + 1]), 1 : i, i]
      
      } else {
        # on the other hand, if we exert all the capacity based on the (i+1)-th, so the currently optimized, item type,
        # then we just set everything else to 0
  
        x[z, 1 : i, i + 1] <- 0
      }
      
      f[z , i + 1] <- max(auxiliary, na.rm = TRUE)
    }
    
    print(z)
    print(auxiliary)
    z <- z + 1     
    
  }
}

# plot the f function evolution over item type
plot(f[1,], type = 'l', ylim = c(0,max(f)), xlab = 'item type')

for (z in 1:zMax){
  lines(f[z,])
}

# # plot the f function evolution over item type
# plot(x[1,], type = 'l', ylim = c(0,max(x)), xlab = 'item type')
# 
# for (z in 1:zMax){
#   lines(x[z,])
# }



# the first column of x is artificially corresponding to f_0, which is just 0 for any cargo constraint z,
# so to print the program we take
print( x[zMax,2:(N+1), N+1])
