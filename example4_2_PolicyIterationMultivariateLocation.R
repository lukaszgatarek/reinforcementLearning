# This is the implementation of jack rental problem 4.2 in Sutton et al.

#rm(list = ls())
# matrix to keep all the iteration of the policy iteration algorithm. It is setup artificially at 300, but then it gets truncated with the number of iterations achieved
policyRecord <- matrix(0,300,3)

# maximal number of cars in one location
m <- 5

# get the required libraries
require(ramify)
require(plotly)
require(plot3D)

## the problem solution is based on the general formula; the net between the today's state s' and yesterday's s, so s'-s equals the net between returns and rental
# processed today i.e. s' - s = ret - ren.

# At the same time we know that the rental amount can iterate only up to yesterday's s, as only the cars 'returned' yesterday, or in general kept in the location at yesterday's close of business.
# So in fact the amount s, can be rented out today. 

# However the cars that are moved during the night can also be rented out today, so the amount of cars that can be rented is changed from s to s + a.
# If a is positive then the cars are moved to the concerned location. Otherwise they are removed from this location and moved to another one.

## policy evaluation function: this function is used for policy evaluation. It is a key component of the system, as it allows evaluation the value function for any state with any policy applied.

policyEvaluationFunction <- function(s, p, a){
  
  # policy evaluation function is applied to a given state and a given policy concerning this state
  # as a state we understand a close of business number of cars in all locations.
  
  # In this case we have two locations. Thus the state is given by a cuple (s,p). The action applied to this cuple is defined as a number of cars shifted from location 2 to location 1.
  # So if the amount a is positive, then the number of cars a are shifted from location 2 to location 1.
  
  # So if an action is applied. Then it is applied to all the locations, as shifting a car from one location would change the other one, as well,
  # getting one car from p to s means: 
  #       s <- s + 1
  #       p <- p - 1
  
  # Thus, applying a policy we mimic it by getting a new s; it is like we changed the s into a bigger or lower value depending if we move out or bring the cars
  # to the 1st location:
  s <- s + a
  # 2nd location: the logic is simple: if you are adding cars in s, then you need to substract them in p and vice versa
  p <- p - a
  
  ## iterate over the 1st location
  
  # auxiliary variable for intermediate step computation of value functions
  componentsVSPprim <- matrix(NA, m + 1, m + 1)
  
  # We iterate over (s', p') , which is a state realization in a period to follow. From s we can get to many s's. 
  # There is a probability that represents a chance of getting from s to any s'.
  
  for (sprim in 0:m){
    
    for (pprim in 0:m){
      
      # lower lever auxiliary variable: componentsV add up to componentsVSPprim, and those add up to value function   
      componentsV <- matrix(NA, s + 1, p + 1) # as the base state is s = 0 and p = 0, we need to prepare memory for (s + 1) x (p + 1) components
      
      # now, we are ready to apply the idea with max amount of rentals today being lower equal to the yestarday's evening state (plus the overnight moves)
      for (ren1 in 0:s){
        
        # again the car return is a simple function of rentals, s and s'
        ret1 <- ren1 + (sprim - s)
        
        # same logic applies for the 2nd location
        for (ren2 in 0:p){   
          
          ret2 <- ren2 + (pprim - p)  
          
          # thus, by iterating over different s', 
          # which are attainable from s, to evaluate the value function in s,
          # we can actually, instead, iterate over distinct amounts of rentals, which make it possible to get from s to s'.
          # Those distinct rental amounts are bounded by the car returns being nonnegative:
          
          if ( (ret1 >= 0) & (ret2 >= 0) ){
            
            # two important issues: rewards systems changes as we need to pay for each car to be moved
            # probability changes, as thanks to moving we actually have different s where we start
            componentsV[ren1 + 1, ren2 + 1] <- prob1[s + 1, sprim + 1] * prob2[p + 1, pprim + 1] * ( reward1[s + 1, sprim + 1] + reward2[p + 1, pprim + 1] - (abs(a) * 2) + discount * V[sprim + 1, pprim + 1])
          } else {
            componentsV[ren1 + 1, ren2 + 1] <- NA
          }
        }
        componentsVSPprim[sprim + 1, pprim + 1] <- mean(componentsV, na.rm = TRUE) 
      }
    }
  }
    return(sum(componentsVSPprim, na.rm = TRUE) )  
}

# variable to save optimal policies for each state (where state is two-dimensional)
optimalPolicy <- matrix(0, m + 1, m + 1)

# maximal number of iterations for the policy evaluation
nSim <- 100

# initial value of Value funtion given a state, across the iterations
V <- matrix(0, m + 1, m + 1)

# discount
discount <- 0.9

# pay for rental
payRental <- 15

# lambdas for Poisson for location 1
lambdaRental1 <- 4
lambdaReturn1 <- 2

# lambdas for Poisson for location 2
lambdaRental2 <- 3
lambdaReturn2 <- 3

# allocate the memory for the probabilities of state change at the 1st location
prob1 <- matrix(0, m + 1, m + 1)

# allocate the memory for the probabilities of state change at the 2nd location
prob2 <- matrix(0, m + 1, m + 1)

# allocate the memory for the rewards at the 1st location
reward1 <- matrix(0, m + 1, m + 1)

# allocate the memory for the rewards at the 2nd location
reward2 <- matrix(0, m + 1, m + 1)

# obtain the probabilities of state change in 1st location
for (s in 0:m){
  for (sprim in 0:m){

    # the storage variable for keeping the individual components of the probability ...
    componentsProb <- NULL
    # ... and reward
    componentsReward <- NULL
    
    for (ren in 0:s){
      
      ret <- ren + (sprim - s)
      
      # the number of returned cars cannot be negative
      if (ret >= 0){ 
        
        componentsProb[ren + 1] <- dpois(ren, lambdaRental1) * dpois(ret, lambdaReturn1)
        componentsReward[ren + 1] <- ren * payRental
        
      } else {
        
        componentsProb[ren + 1] <- 0
        componentsReward[ren + 1] <- NA
        
      }  
    }
    
    # probability of moving from state s to s' (it takes into account all possible schemes of car rentals and returns)
    prob1[s + 1, sprim + 1] <- sum(componentsProb) # change from so to s' can be achieved in many ways; as those actions are independent, we sum them up
    
    # similarly for each route from s to s' there is a different reward, depending on the amount of rentals, that have led to it
    reward1[s + 1, sprim + 1] <- mean(componentsReward, na.rm = TRUE) 
  }
}

# matrix normalization
prob1 <- prob1 / apply(prob1, 1, sum)




# obtain the probabilities of state change in 1st location
for (s in 0:m){
  for (sprim in 0:m){
    # the storage variable for keeping the individual components of the probability
    componentsProb <- NULL
    componentsReward <- NULL
    
    for (ren in 0:s){
      
      ret <- ren + (sprim - s)
      
      # the number of returned cars cannot be negative
      if (ret >= 0){ 
        
        componentsProb[ren + 1] <- dpois(ren, lambdaRental2) * dpois(ret, lambdaReturn2)
        componentsReward[ren + 1] <- ren * payRental
        
      } else {
        
        componentsProb[ren + 1] <- 0
        componentsReward[ren + 1] <- NA
        
      }  
    }
    
    # probability of moving from state s to s' (it takes into account all possible schemes of car rentals and returns)
    prob2[s + 1, sprim + 1] <- sum(componentsProb) # change from so to s' can be achieved in many ways; as those actions are independent, we sum them up
    
    # similarly for each route from s to s' there is a different reward, depending on the amount of rentals, that have led to it
    reward2[s + 1, sprim + 1] <- mean(componentsReward, na.rm = TRUE) 
  }
}

# matrix normalization
prob2 <- prob2 / apply(prob2, 1, sum)

# initially we do not improve policy upon any state
s_toBeImprovedUpon <- Inf
p_toBeImprovedUpon <- Inf

# initial policy is just a policy evaluation without any actions
## a <- 0     

# policy is not stable as an initialization; that is for the while function to start with the basic policy
policyStable <- FALSE

# the value function iterations within the optimization algorithm in the policy evaluation part will be recorded
allPoliciesV <- list()

# counter for the big policy improvement loop
j <- 0 

while (!policyStable){
  
  j <- j + 1
  
  # record of V's for a given policy in each iteration
  recV <- array(0, dim = c(m + 1, m + 1, nSim) )
  
  # deltas for all states
  delta <- matrix(0, m + 1, m + 1)
  # initialize the value function increment
  Delta <- 0.1
  
  # iteration
  i <- 2
  
  # run the value function evaluation (with endogenous optimization, iterating between the s's to get the stable evaluation of the value function)
  
  while (Delta > 0.0001){
    
    #for (i in 2:nSim){    
    for (s in 0:m){
      for (p in 0:m){  
        v <- V[s + 1, p + 1]
        
        # for the first round, the basic policy, we just take the standard evaluation with a = 0
        # it would apply for all states s in the system 
        
        if (s_toBeImprovedUpon == Inf){
          V[s + 1, p + 1] <- policyEvaluationFunction(s, p, 0)
        }
        
        # if we have some as upon which we improve with a given policy, then we tackle it here 
        if ((s == s_toBeImprovedUpon) & (p == p_toBeImprovedUpon)){
          
          V[s + 1, p + 1] <- policyEvaluationFunction(s, p, actions[a]) }
        
        # and for the other s we just apply the currently binding (for them) policy 
        # (as recored in the optimal policy vector)
        # but we need to recalculate the value functions for them as we just changed 
        # the value function of the s_toBeImprovedUpon above
        
        if ((s != s_toBeImprovedUpon) & (s_toBeImprovedUpon != Inf) & (p != p_toBeImprovedUpon) & (p_toBeImprovedUpon != Inf) ){
          
          V[s + 1, p + 1] <- policyEvaluationFunction(s, p, optimalPolicy[s + 1, p + 1]) }
        
        
        delta[s + 1, p + 1] <- abs(v - V[s + 1, p + 1])
        
      }
    }
    
    recV[, , i] <- V
    
    # take a maximum of deltas
    Delta <- max(delta)
    
    i <- i + 1
    print(i)
  }
  
  
  # delete everything further than iteration i from the matrix
  recV <- recV[ , , 1:(i-1)]  
  recV
  
  # matplot(t(recV[,2:dim(recV)[2]]), type = 'l')
  
  
  # plotss <- plot_ly(z = recV[ , , 2:dim(recV)[3]] ) %>% add_surface()
  
  
  M <- mesh(seq(0, m + 1, length.out = m + 1),
            seq(0, m + 1, length.out = m + 1))
  
  u <- M$x ; v <- M$y
  x <- u
  y <- v
  
  z <- recV[ , , i-1] 
  
  surf3D(x, y, z, colvar = z, border = "black", colkey = TRUE, xlab = "# cars 1 location: s", ylab = "# cars 2 location: p",
         zlab = "value(s, p)")
  
  
  allPoliciesV[[j]] <- recV
  
  # maximal numer of cars to be moved
  maxM <- 2
  
  # allocate memory for testing the stability of policies
  improvedValue <- array(NA, dim = c(m + 1, m + 1, 2 * maxM + 1) ) # 2 * maxM + 1 stands for all the options of moving from 1 to maxM cars as well as policy of not moving anything (represented by this +1)
  
  # now let us disturb each of the s again with some action a
  # actions <- c((-maxM:-1),(1:maxM))
  # the above way of defining actions is not enough as we also need to keep the base scenario of doing nothing
  actions <- c(-maxM:maxM)
  aIndex  <- 1: length(actions)
  
  # before getting in the loop, we assume for a while that the current policy is stable
  policyStable <- TRUE
  
  #    for (s in 0:m){
  
  # initialize the state and go one by one
  s <- 0
  p <- 0
  
  while ((policyStable) & (s <= m)) {
    while ((policyStable) & (s <= m) & (p <= m)) {            
      # we need to take all the options to check which one would be the best
      # this calculation is performed given the current value of V(sprim,...) vector,
      # what means that each policy for s is evaluated given the currently holding optimal policies
      # saved in optimalPolicy vector, which implies the current value of V
      for (a in aIndex ) {
        
        if ( ((s + actions[a]) >= 0) & ((s + actions[a]) <= m) & ((p - actions[a]) >= 0) & ((p - actions[a]) <= m)  ){
          improvedValue[s + 1, p + 1, a] <- policyEvaluationFunction(s, p, actions[a])}
      }
      
      #              argMaxes <- argmax(improvedValue, rows = TRUE)
      
      # we need to take a proper slice and take the maximum over it             
      a <- which(improvedValue[s + 1, p + 1,] == max((improvedValue[s + 1, p + 1,]), na.rm = TRUE) )
      
      # we save the current value of s that we are going to improve upon
      s_toBeImprovedUpon <- s
      p_toBeImprovedUpon <- p
      
      #              a <- argMaxes[[s + 1]]
      
      ## this would be a condition on value function, ... 
      ## if (improvedValue[s + 1, a] <= V[s + 1] ){
      ## ... but in case of policy improvement, we rather have a condition on policy
      
      if (actions[a] == optimalPolicy[s + 1, p + 1]){
        
        if (p < m){
          #                s <- s + 1
          p <- p + 1}
        
        if (p == m){
          s <- s + 1
          p <- 0
        }
        
      } else {
        policyStable <- FALSE # so that we can go and improve the current value
        # save the policy that was optimal for a given s
        optimalPolicy[s + 1, p + 1] <- actions[a]
        
        policyRecord[j, 1] <- s
        policyRecord[j, 2] <- p
        policyRecord[j, 3] <- actions[a]
        
        if (actions[a] < 0 ){
          print( paste('improving state s =', toString(s), ' by shipping ', toString(actions[a]), ' cars to p = ', toString(p)  ) ) }
        
        if (actions[a] > 0 ){
          print( paste('improving state s =', toString(s), ' by shipping ', toString(actions[a]), ' cars from p = ', toString(p)  ) ) }
        
      }
      
      
    }
    
    
  }
}          

changingV <- array(NA, dim = c (m + 1, m + 1, length(allPoliciesV) ) )         

# times a new policy has been selected and tested
numAllPolicies <- length(allPoliciesV)

for (j in 1:numAllPolicies){
  changingV[ , ,j] <- allPoliciesV[[j]][, , dim(allPoliciesV[[j]])[2] ]
}


#matplot(t(changingV), type = 'l')

policyRecord <- policyRecord[1:j,]

M <- mesh(seq(0, m + 1, length.out = m + 1),
          seq(0, m + 1, length.out = m + 1))

u <- M$x ; v <- M$y

x <- u
y <- v

# j corresponds to number of time the algo has taken some action a from any state s
for (j in 1:numAllPolicies){
  
  z <- changingV[ , ,j]
  
  surf3D(x, y, z, colvar = z, border = "black", colkey = FALSE)
  Sys.sleep(0.5)
}


for (s in 0:m){
  for (p in 0:m){
    if ((s == 0) & (p == 0) ){
      plot(changingV[s+1,p+1,2:numAllPolicies], type = 'l', ylim = c(450,600)) }
    else { lines(changingV[s+1,p+1,2:numAllPolicies]) }
  }
}

