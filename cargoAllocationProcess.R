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
x <- matrix(0, zMax, N + 1)

for (i in 1:N){
  z <- 1
  print(i)
  
  while (z <= zMax){
    
    alpha <- floor(z / w[i])
    
    auxiliary <- matrix(0, alpha + 1, 1)
    
    for (xi in 0:alpha){
      # xi is just potential x[i] value to test for the maximum
      if ( (z - w[i] * xi) == 0){
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
      
      x[z , i + 1] <- maximizingX[(which(wWith0[maximizingX] == min(wWith0[maximizingX]) ) )] - 1 # -1 corresponds to the zero which is also availale there
      f[z , i + 1] <- max(auxiliary, na.rm = TRUE)
      
    } else {
      
      x[z , i + 1] <- which(auxiliary == max(auxiliary, na.rm = TRUE) ) - 1
      
      # if we use some value function obtainable from the items of the previous (i-1) types,
      # then we need to copy the history of their choices as well
      
      if (z - (w[i] * x[z , i + 1]) > 0) {
        
        x[z, 1:i] = x[z - (w[i] * x[z , i + 1]), 1:i]
        
      }
      
      f[z , i + 1] <- max(auxiliary, na.rm = TRUE)
    }
    
    print(z)
    print(auxiliary)
    z <- z + 1     
    
  }
}


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


