# this is a code that resembles the flow chart in Figure 22
# and corresponding problem of allocation of production resources across N production units
# production in each of unit is characterized by the market specifics,
# determined by the competition value a and potential market size, v

# this is a version extended by the application of lagrange multiplier as in formula 23.1 page 58

g <- function(x, v_i, a_i, yMax, lambda) {
  auxiliaryY <- matrix(0, yMax + 1, 1)
  
  for (y in 0:yMax) {
    auxiliaryY[y + 1, 1] <-
      (v_i * (1   -   (1 - exp(-a_i /  (
        x + y
      ))) ^ x)) - lambda * y
  }
  
  output <- list()
  # we treat the returns as a function of x, so optimization w.r.t. y takes place for any x
  output[[1]] <- max(auxiliaryY)
  output[[2]] <- which(auxiliaryY == max(auxiliaryY))
  
  return(output)
}

# number of production units to allocate across
N <- 20
# competition values
a <- 1:N
# potential markets
v <- 1:N

# maximum z - which is in fact the value of the constraint, x,
# and thus a max of the domain for the function to be evaluated
zMax <- 100

# maximum y, the max budget for advertising campaign
yMax <- 100

# lambda for Lagrange
lambda <- 1

# we need to store the value function f
f <-
  matrix(0, zMax + 1, N + 1) # + 1 corresponds to the 0 as the initial index for i
x <-
  array(0, dim = c(zMax + 1, N + 1, N + 1)) #each iteration the previous values of optimal values also change,
# so we need to introduce an additional dimension to keep the state of 1 : (N + 1) variables at the state i in (1, N + 1)
y <- array(0, dim = c(zMax + 1, N + 1, N + 1))

for (i in 1:N) {
  z <- 0
  print(i)
  
  while (z <= zMax) {
    auxiliary <- matrix(0, z + 1, 1)
    
    xPotential <- matrix(0, z + 1, i + 1)
    yPotential <- matrix(0, z + 1, i + 1)
    
    for (xi in 0:z) {
      auxiliary[xi + 1, 1] <-
        g(xi, v[i], a[i], yMax, lambda)[[1]] + f[z - xi + 1, i]
      
      xPotential[xi + 1, i + 1] <- xi
      xPotential[xi + 1, 1:i] <- x[z - xi + 1, 1:i, i]
      
      # we need to store y as well
      yPotential[xi + 1, i + 1] <-
        g(xi, v[i], a[i], 20, 0.5)[[2]]
      yPotential[xi + 1, 1:i] <- y[z - xi + 1, 1:i, i]
    }
    x[z + 1, 1:(i + 1), i + 1] <-
      xPotential[min(which(auxiliary == max(auxiliary, na.rm = TRUE))), 1:(i + 1)]
    y[z + 1, 1:(i + 1), i + 1] <-
      yPotential[min(which(auxiliary == max(auxiliary, na.rm = TRUE))), 1:(i + 1)]
    
    f[z + 1, i + 1] <- max(auxiliary, na.rm = TRUE)
    
    z <- z + 1
  }
}

# plot the f function evolution over item type
plot(f[1, ],
     type = 'l',
     ylim = c(0, max(f)),
     xlab = 'item type')

for (z in 1:zMax) {
  lines(f[z, ])
}

# plot optimal function value vs the number of production units to allocate
plot(f[, N + 1])

# PRINT THE OPTIMAL PROGRAM
# the first column of x is artificially corresponding to f_0, which is just 0 for any cargo constraint z,
# so to print the program we take
print(x[zMax, 2:(N + 1), N + 1])
print(y[zMax, 2:(N + 1), N + 1])

# plot sum of y_i's for a given level of z. This corresponds to the Fgure 26 on page 61 of Applied ...
plot(0:zMax, apply(y[, , N + 1], 1, sum))