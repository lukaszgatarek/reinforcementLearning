g <- function(x, v){
  return(x * v)
}

# weight
w <- c(20,18,14,12,10,16,22,24)
# size
size <- c(2,1,1,2,1,1,3,2)
# value
v <- c(72,60,40,27,20,50,85,96)

# we have N types of items
N <- length(w)

# get the maximum Z corresponding to the weight
zMax <- 100
# get the maximum S corresponding to the size
sMax <- 100

# we need the value function f

f <- array(0, dim = c(zMax, sMax, N + 1)) # + 1 corresponds to the 0 as the initial index for i
# this holder corresponds to optimal variable x_N. 
# Although we have two types of constraintes here, we have a problem of allocating one type of resource
x <- array(0, dim = c(zMax, sMax, N + 1, N + 1)) 

# The main iteration retains the same as in case of the one-dimensional allocation process
# again we go type by type of product, which are characterized by specific value, size and weight 

for (i in 1:N){ 
  print(i)
  
  #start to iterate over z
  z <- 1
  
  while (z <= zMax){
    
    # compute the alphs for weight
    alphaWeight <- floor(z / w[i])
    
    # start to iterate over s
    s <- 1
    
    while (s <= sMax){
    
      # compute the alphs for size
      alphaSize <- floor(s / size[i])
      # now the function value container spans over both potential weights as well as sizes
      
      # we need to take the tested number of itmes based on the more restrictive constraint
      alpha <- min(alphaWeight, alphaSize)
      # prepare a holder for the evaluations in the current round (specified by z and s)
      auxiliary <- matrix(0, alpha + 1 )
      
      for (xi in 0:alpha){
          
          # xi is just potential x[i] value to test for the maximum
          if ( ( (z - w[i] * xi) == 0 ) || (s - (size[i] * xi) == 0 ) ) {
            auxiliary[xi + 1, 1] <- g(xi, v[i]) }
          
          if ( ( (z - w[i] * xi) > 0 ) && (s - (size[i] * xi) > 0 ) )  {
            
            auxiliary[xi + 1, 1] <- g(xi, v[i]) + f[z - (w[i] * xi), s - (size[i] * xi), i]    
          
          }
        }

      maximizingX <- which(auxiliary == max(auxiliary, na.rm = TRUE) )
      
      if (length(maximizingX) > 1 ){  
        # just let us use the size to select among the alternatives
      print(1)
        
        x[z, s, i + 1, i + 1] <- max(maximizingX) - 1
        
        } else {
          
         x[z , s, i + 1, i + 1] <- maximizingX - 1
        
        }
        
        if ( ( (z - (w[i] * x[z , s, i + 1, i + 1]) ) > 0 ) & (s - (size[i] * x[z , s, i + 1, i + 1]) > 0 ) ) {
           x[z, s, 1:i, i + 1] <- x[z - (w[i] * x[z , s, i + 1, i + 1]), s - (size[i] * x[z , s, i + 1, i + 1]), 1:i , i]
         } else {
          # if all the capacity is exhausted by the current item type, then all the others previously types loaded are given 0 capcity
          x[z, s, 1:i, i + 1] <- 0  
          }
        
        f[z , s, i + 1] <- max(auxiliary, na.rm = TRUE)
  #    }
    
      s <- s+1
      print(z)
      print(s)
     
      }
   
      z <- z + 1     
    }
  }

plot(f[1,1,], type = 'l', ylim = c(0,max(f)))

for (z in 1:zMax){
  for (s in 1:sMax){
    lines(f[z,s,])
  }
}

# PRINT THE OPTIMAL PROGRAM
# the first column of x is artificially corresponding to f_0, which is just 0 for any cargo constraint z
print(x[zMax, sMax, 2:(N+1), N + 1])