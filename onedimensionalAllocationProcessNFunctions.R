g <- function(x, a, b){
  return(a * x^2 + b*x)
}

N <- 3

a <- matrix(N, 1)
b <- matrix(N, 1)

# consider a case with two function
a[1] <- 1
a[2] <- 2.5
a[3] <- 3.3

b[1] <- -1
b[2] <- -4
b[3] <- -8

Delta <- 0.5
X <- seq(0, 6, Delta)

plot(g(X, a[1], b[1]), type = 'l')
lines(g(X, a[2], b[2]), col = 'red')
lines(g(X, a[3], b[3]), col = 'blue')

R <- length(X)

f <- matrix(NA, R, N)
xnMaximizingFunction <- matrix(NA, R, N)

# at the beginning we assume everything goes to the 1st function
xnMaximizingFunction[,1] <- X

 f[,1] <- g(X, a[1], b[1])

 for ( k in 2:N){
   
  for (l in 1:R){
  
    print(paste('function evaluated for for x = ', toString(X[l])) )
    
    x <- X[l]
    
    auxiliaryRecord <- matrix(NA, l , 1)
    
    for (d in 0 : ( l - 1 ) ){
      
      print(paste('we check x_n = ', toString(k * Delta) ) )
      # which(X == x - k * Delta) denotes the index giving the proper value of function f associated with x - k * Delta
      if ( (x - d * Delta) %in% X){
        auxiliaryRecord[d + 1, 1] <- g(d * Delta, a[k], b[k]) + f[which(X == x - d * Delta ), k-1]
      }
    }
    
      argMax <- which(auxiliaryRecord == max(auxiliaryRecord, na.rm = TRUE) )
      
      xnMaximizingFunction[l, k] <- max(X[argMax])
      
      if ( !( xnMaximizingFunction[l, k] == 0) ){
        xnMaximizingFunction[l, k-1] <- 0
      }
      
      # get the aux f[,2] value
      f[l, k] <- max(auxiliaryRecord, na.rm = TRUE)
  }
 
}
 
   # check the obtained solution with the 
 #  plot(f[,1] + f[,2], col = 'red')
   # against the original problem
   
   matplot(t(f), type = 'l')
   
   # the optimal vs the components
   plot(f[,N])
  
   lines( g(X, a[1], b[1]), col = 'blue')
   lines( g(X, a[2], b[2]), col = 'red')
   lines( g(X, a[3], b[3]), col = 'green')
   lines(f[,N])
   
   print(xnMaximizingFunction)
   
   
   
   
