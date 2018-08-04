# this script implements the fly away kit problem on page 42 in Applied dynami programming book

g <- function(x_k, lambda_k, c_k) {
  sum <- 0
  # we need to cut the Poisson somewhere
  poissonCut <- 40
  
  for (z in ((x_k + 1):poissonCut)) {
    sum <- sum + ((z - x_k) * dpois(z, lambda_k))
  }
  
  # c_k is common for all the item types
  return(c_k * sum)
}

# weight
w <- c(3, 5, 2, 5, 6, 2, 4, 7, 5, 3)
# size
size <- c(5, 4, 7, 4, 6, 2, 5, 7, 3, 4)

# we have N types of items
N <- length(w)

# get the maximum Z corresponding to the weight
zMax <- 30
# get the maximum S corresponding to the size
sMax <- 30

# we need the value function f
f <-
  array(0, dim = c(zMax + 1, sMax + 1, N + 1)) # + 1 corresponds to the 0 as the initial index for i

# this holder corresponds to optimal variable x_N.
# Although we have two types of constraintes here, we have a problem of allocating one type of resource
# 3rd dimension corresponds to the optimized x_k's
# across the stages of optimization (one stage corresponding to one x_k)
# in each stage of optimization we need to run the procedure for each value in the domain of the function
# that means that the optimal sequence of x_k maximizing f(w,s) can be different across the stages i
# thus if we look up some value in the table, we need to in fact look it up for a specific stage i

x <-
  array(0, dim = c(zMax + 1, sMax + 1, N + 1, N + 1)) # + 1 corresponds to starting from 0

# The main iteration retains the same as in case of the one-dimensional allocation process
# again we go type by type of product, which are characterized by specific value, size and weight

for (i in 1:N) {
  print(i)
  
  #start to iterate over z
  z <- 0
  
  while (z <= zMax) {
    # compute the alphs for weight
    alphaWeight <- floor(z / w[i])
    
    # start to iterate over s
    s <- 0
    
    while (s <= sMax) {
      # compute the alphs for size
      alphaSize <- floor(s / size[i])
      # now the function value container spans over both potential weights as well as sizes
      
      # we need to take the tested number of itmes based on the more restrictive constraint
      alpha <- min(alphaWeight, alphaSize)
      
      # prepare a holder for the evaluations in the current round (specified by z and s)
      auxiliary <- matrix(0, alpha + 1)
      
      xPotential <- matrix(0, alpha + 1, i + 1)
      
      for (xi in 0:alpha) {
        # we compute for each potential xi and then we would select the optimal one belowe
        auxiliary[xi + 1, 1] <-
          g(xi, 3, 4) + f[z - (w[i] * xi) + 1, s - (size[i] * xi) + 1, i]
        
        # the last dimension stores the current state, the ith optimal value in the 3nd dimension can change from the i-th to (i+1)-th iteration in the 4rth dimension
        xPotential[xi + 1, i + 1] <- xi
        
        xPotential[xi + 1, 1:i] <-
          x[z - (w[i] * xi) + 1, s - (size[i] * xi) + 1, 1:i, i]
        
      }
      
      x[z + 1 , s + 1, 1:(i + 1), i + 1] <-
        xPotential[min(which(auxiliary == min(auxiliary, na.rm = TRUE))), 1:(i + 1)]
      
      f[z + 1 , s + 1, i + 1] <- min(auxiliary, na.rm = TRUE)
      print(z)
      print(s)
      s <- s + 1
    }
    z <- z + 1
  }
}

# plot the f function
plot(f[1,1,], type = 'l', ylim = c(min(f),max(f)))

for (z in 1:zMax){
  for (s in 1:sMax){
    lines(f[z,s,])
  }
}

# PRINT THE OPTIMAL PROGRAM
# the first column of x is artificially corresponding to f_0, which is just 0 for every available cargo size constraint z
print(x[zMax+1,sMax+1,2:(N+1), N+1])