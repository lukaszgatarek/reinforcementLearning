g <- function(x, a, b){
  return(a * x^2 + b*x)
}

N <- 2

a <- matrix(N, 1)
b <- matrix(N, 1)

# consider a case with two function
a[1] <- 1
a[2] <- 2.5

b[1] <- -1
b[2] <- -4

Delta <- 0.5
X <- seq(0, 5, Delta)

plot(g(X, a[1], b[1]), type = 'l')
lines(g(X, a[2], b[2]), col = 'red')

R <- length(X)

f <- matrix(NA, R, N)
xnMaximizingFunction <- matrix(NA, R, N)

 f[,1] <- g(X, a[1], b[1])

for (l in 1:R){

  print(paste('function evaluated for for x = ', toString(X[l])) )
  
  x <- X[l]
  
  auxiliaryRecord <- matrix(NA, l , 1)
  
  for (k in 0 : ( l - 1 ) ){
    
    print(paste('we check x_n = ', toString(k * Delta) ) )
    # which(X == x - k * Delta) denotes the index giving the proper value of function f associated with x - k * Delta
    if ( (x - k * Delta) %in% X){
      auxiliaryRecord[k + 1, 1] <- g(k * Delta, a[2], b[2]) + f[which(X == x - k * Delta ), 1]
    }
  }
    argMax <- which(auxiliaryRecord == max(auxiliaryRecord, na.rm = TRUE) )
    
    xnMaximizingFunction[l, 2] <- max(X[argMax])
    xnMaximizingFunction[l, 1] <- x - xnMaximizingFunction[l, 2]
    # get the aux f[,2] value
    f[l ,2] <- max(auxiliaryRecord, na.rm = TRUE)
}

 # check the obtained solution with the 
 plot(f[,1] + f[,2], col = 'red')
 # against the original problem
 
 matplot(t(f), type = 'l')
 
 # the optimal vs the components
 plot(f[,2])
 lines(f[,1], col = 'red')
 lines( g(X, a[2], b[2]), col = 'blue')
 
 print(xnMaximizingFunction)
 
 
 
 
