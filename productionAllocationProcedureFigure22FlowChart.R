# this is a code that resembles the flow chart in Figure 22
# and corresponding problem of allocation of production resources across N production units
# production in each of unit is characterized by the market specifics,
# determined by the competition value a and potential market size, v

# utility function based on competition value, a, and potential market size, v
g <- function(x, v_i, a_i){
  return(v_i*(1   -   ( 1-exp(-a_i/x) )^x)  )
}

# number of production units (production sites) to allocate production budget z across
N <- 20
# competition values
a <- 1:N
# potential markets
v <- 1:N

# maximum z - which is in fact the value of the constraint, x, in the book formula 21.4 page 57 
# and thus a max of the domain for the function to be evaluated
zMax <- 100

# we need the place to store the value function f
f <- matrix(0, zMax + 1, N + 1) # + 1 corresponds to the 0 as the initial index for i
# we need the place to store the optimal x. there are N of them (+ 1 for the x_0th one which corresponds to the fact that 
# have f_0 = 0 being 0 for all z. we need it as when we starts for i = 1, we have to evalute f_(i-1), so 0 in this case)
x <- array(0, dim = c(zMax + 1, N + 1, N + 1)) # Then, importantly, one need to keep in mind, that with each iteration i,
# the previous values of optimal values also change, # so we need to introduce an additional dimension to keep the state of 1 : (N + 1) variables at the state i in (1, N + 1)

for (i in 1:N){
  z <- 0
  print(i)
  
  while (z <= zMax){
    
    auxiliary <- matrix(0, z + 1, 1)
    
    xPotential <- matrix(0, z + 1, i + 1)
    
    for (xi in 0:z){
      print(xi)
      # xi is just potential x[i] value to test for the maximum
      #if ( (z - xi) == 0){
        # in this case we have no more capacity, so we will just not earn more, so we put 0 for the f_(N-1) 
      #  auxiliary[xi + 1, 1] <- g(xi, v[i])
        #x[z, 1:i] <- 0
      #} else {
        auxiliary[xi + 1, 1] <- g(xi, v[i], a[i]) + f[z - xi + 1, i]    
      #}
        xPotential[xi + 1, i + 1] <- xi
        xPotential[xi + 1, 1 : i] <- x[z - xi + 1, 1 : i, i]
    } 
    x[z + 1, 1:(i+1), i + 1] <- xPotential[ min(which(auxiliary == max(auxiliary, na.rm = TRUE) )), 1: (i + 1)]
    f[z + 1, i + 1] <- max(auxiliary, na.rm = TRUE)

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

# plot optimal function value vs the number of production units to allocate
plot(f[,N+1])


# # plot the f function evolution over item type
# plot(x[1,], type = 'l', ylim = c(0,max(x)), xlab = 'item type')
# 
# for (z in 1:zMax){
#   lines(x[z,])
# }

# the first column of x is artificially corresponding to f_0, which is just 0 for any cargo constraint z,
# so to print the program we take
print(x[zMax,2:(N+1), N+1])
