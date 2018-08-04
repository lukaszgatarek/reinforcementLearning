g <- function(x_k, lambda_k, c_k){
  sum <- 0
  # we need to cut the Poisson somewhere
  poissonCut <- 40
  
  for (z in ((x_k+1):poissonCut) ){
    sum <- sum + ( (z-x_k) * dpois(z, lambda_k))
    # print(( (z-x_k) * dpois(z, lambda_k)))
    # print(sum)
  }
  
  # c_k is common for all the item types
  return(c_k * sum)
}

# weight
w <- c(3,5,2,5,6,2,4,7,5,3)
# size
size <- c(5,4,7,4,6,2,5,7,3,4)
# value
# value and rather cost is determined by the poisson based cost function
#v <- c(72,60,40,27,20,50,85,96)

# we have N types of items
N <- length(w)

# get the maximum Z corresponding to the weight
zMax <- 9
# get the maximum S corresponding to the size
sMax <- 9

# we need the value function f

f <- array(0, dim = c(zMax + 1, sMax + 1, N + 1)) # + 1 corresponds to the 0 as the initial index for i

# this holder corresponds to optimal variable x_N. 
# Although we have two types of constraintes here, we have a problem of allocating one type of resource
x <- array(0, dim = c(zMax + 1, sMax + 1, N + 1)) # + 1 corresponds to starting from 0 

# The main iteration retains the same as in case of the one-dimensional allocation process
# again we go type by type of product, which are characterized by specific value, size and weight 

for (i in 1:N){ 
  print(i)
  
  #start to iterate over z
  z <- 0
  
  while (z <= zMax){
    
    # compute the alphs for weight
    alphaWeight <- floor(z / w[i])
    
    # start to iterate over s
    s <- 0
    
    while (s <= sMax){
    
      # compute the alphs for size
      alphaSize <- floor(s / size[i])
      # now the function value container spans over both potential weights as well as sizes
      
      # we need to take the tested number of itmes based on the more restrictive constraint
      alpha <- min(alphaWeight, alphaSize)
      
      # prepare a holder for the evaluations in the current round (specified by z and s)
      auxiliary <- matrix(0, alpha + 1 )
      
      xPotential <- matrix(0, alpha + 1, i + 1)
      
      for (xi in 0:alpha){
          
        ### IMPORTANTLY
        
          # xi is just potential x[i] value to test for the maximum
        #   if ( ( (z - w[i] * xi) == 0 ) || (s - (size[i] * xi) == 0 ) ) {
        #     auxiliary[xi + 1, 1] <- g(xi, 3, 4) }
        #     
        # #  } else {
        #     
        #   if ( ( (z - w[i] * xi) > 0 ) && (s - (size[i] * xi) > 0 ) )  {
            auxiliary[xi + 1, 1] <- g(xi, 3, 4) + f[z - (w[i] * xi) + 1, s - (size[i] * xi) + 1, i]
           
            xPotential[xi + 1, i + 1] <- xi
            
          
            xPotential[xi + 1, 1 : i ] <- x[z - (w[i] * xi) + 1, s - (size[i] * xi ) + 1 , 1 : i]
          
            
        #  }
        }
      # get the si and wi for which the value function is the biggest achievable 
      # maximizingX <- which(auxiliary[wi + 1, ] == max(auxiliary[wi + 1, ], na.rm = TRUE) )
      
     #maximizingX <- which(auxiliary == max(auxiliary), arr.ind = TRUE)
      
  #     maximizingX <- which(auxiliary == min(auxiliary, na.rm = TRUE) )
  #     
  #     if (length(maximizingX) > 1 ){  
  #       # just let us use the size to select among the alternatives
  #     print(1)
  #       
  #       # wWith0 <- c(0, size)
  #       # 
  #       # x[z, s, i + 1] <- maximizingX[(which(wWith0[maximizingX] == min(wWith0[maximizingX]) ) )] - 1
  #    
  #       x[z + 1, s + 1, i + 1] <- max(maximizingX) - 1 # this min corresponds to the situation when a few potential values xi of x_k results in the same function value
  #       
  #       } else {
  #         
  # #      sizeWith0 <- c(0, size) 
  #       # x[z , s, i + 1] <- maximizingX[(which(sizeWith0[maximizingX] == min(sizeWith0[maximizingX]) ) )] - 1 # -1 corresponds to the zero which is also availale there
  #       # f[z , s, i + 1] <- max(auxiliary[wi + 1, ], na.rm = TRUE)
  # #    } else {
  #       #x[z , s, i + 1] <- which(auxiliary[wi + 1, ] == max(auxiliary[wi + 1, ], na.rm = TRUE) ) - 1
  #       x[z + 1 , s + 1, i + 1] <- maximizingX - 1
  #       
  #       
  #     #  y[z , s, i + 1] <- maximizingX[1,2] - 1
  #       # if we use some value function obtainable from the items of the previous (i-1) types,
  #       # then we need to copy the history of their choices as well
  #       }
  #       
  #     if  ( (z!=0) && (s!=0) ){
  #       
  #       if ( ( (z - (w[i] * x[z + 1 , s + 1, i + 1]) ) > 0 ) & (s - (size[i] * x[z + 1, s + 1, i + 1]) > 0 ) ) {
  #   #    if ( min (( (z - (w[i] * x[z , s, i + 1]) > 0) ) , (  (s - (size[i] * x[z , s, i + 1]) > 0 ) ) ) > 0){
  #         x[z + 1, s + 1, 1:i] <- x[z - (w[i] * x[z + 1, s + 1, i + 1]) + 1, s - (size[i] * x[z + 1, s + 1, i + 1]) + 1, 1:i ]
  #      #   y[z, s, 1:i] = y[z - (w[i] * x[z , s, i + 1]), s - (size[i] * y[z , s, i + 1]), 1:i]
  #       } else {
  #         # if all the capacity is exhausted by the current item type, then all the others previously loaded types are given 0 capacity
  #       x[z + 1, s + 1, 1:i] <- 0  
  #       }
  #       
  #     }
  #       
        # if (  (s - (size[i] * y[z , s, i + 1]) > 0 ) ) {
        #   y[z, s, 1:i] = y[z, s - (size[i] * y[z , s, i + 1]), 1:i] }
      
        #x[z + 1 , s + 1, (i+1)] <- xPotential[ max(which(auxiliary == min(auxiliary, na.rm = TRUE) )), (i+1)]
      
      
         x[z + 1 , s + 1, 1: (i+1)] <- xPotential[ min(which(auxiliary == min(auxiliary, na.rm = TRUE) )), 1: (i + 1)]
        
         
       #  print(auxiliary)
         if (i>0){
        #  print(xPotential[ min(which(auxiliary == min(auxiliary, na.rm = TRUE) )), 1: (i + 1)])
         }
         
        f[z + 1 , s + 1, i + 1] <- min(auxiliary, na.rm = TRUE)
      #  print(f[z + 1 , s + 1, 1:(i + 1)])
  #    }
    
      print(z)
      print(s)
      
      #print(auxiliary)
      s <- s+1
      }
      
      #print(z)
      
      z <- z + 1     

  }

  
        print(x[7,7,])
  #    print(f[7,7,])
  
  }

plot(f[1,1,], type = 'l', ylim = c(min(f),max(f)))

for (z in 1:zMax){
  for (s in 1:sMax){
    lines(f[z,s,])
  }
}



# the first column of x is artificially corresponding to f_0, which is just 0 for any cargo constraint z
print[x[,2:N+1]]


###### just some chek it up routine below (nonnecessary for the main part)
# get the ordering of items' values

xProgram <- matrix(0, zMax, N)

# ordering from the biggest value to the smallest
ordering <- rev(order(v))
#for (i in 1:length(ordering)){
z <- zMax
j <- 1
usedWeight <- 0 

while ((usedWeight<=z) & (j <= N)) {
  print(j)
  # start with the most valuable item and take as much 
  # of it as proposed by the dynamic programming solution
  
  # for (i in 1 : x[z, ordering[j] + 1]){
  
  i <- 1
  
  while ((usedWeight <= z ) & (i <= x[z, ordering[j] + 1]) & (usedWeight + w[ordering[j] ] <= z) ){
    usedWeight <- usedWeight + w[ordering[j] ] 
    
    xProgram[z, ordering[j]] <- xProgram[z, ordering[j]] + 1 
    
    i <- i + 1
  }
  
  
  #}
  j <- j + 1
  
}

print(xProgram[z,])


