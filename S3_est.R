
##### The development in valuefunctions over time per type conditional on the development of amenities #########
# is found by regression

R = 100
# Number of regressors: n.lags * number of variables in regression + dummies

n.lags = 2
n.neighborhoods = 4 #Kun for at gemme
n.reg = n.neighborhoods * 2 + n.lags * 5

beta.m <- as.data.frame(matrix(0, n.reg, n.types))
res.m  <- as.data.frame(matrix(0, (n.periods - n.lags) * n.neighborhoods, n.types))

beta.o <- as.data.frame(matrix(0, 4, n.types))
res.o  <- as.data.frame(matrix(0, 8, n.types))

beta.price <- as.data.frame(matrix(0, n.reg, 1)) 
res.price  <- as.data.frame(matrix(0, (n.periods-n.lags)*n.neighborhoods, 1)) 

# Regressing the value evolvement for each type over time
for (m in 1:n.types) {
  
  val.vec <- as.matrix(as.vector(value.functions.tilde[m,1:n.neighborhoods, ]))
  
  temp <- as.matrix(cbind(val.vec, crime.vec, traffic.vec, expense.vec, meanprice.vec))
  
  lag1 <- rbind(matrix(0, nrow = n.neighborhoods,ncol(temp)), temp[1:(nrow(temp) - n.neighborhoods), ])
  lag2 <- rbind(matrix(0, nrow = (n.neighborhoods * 2),ncol(temp)), temp[1:(nrow(temp) - n.neighborhoods * 2), ])
  lags <- cbind(lag1, lag2)

  Y <- temp[(2 * n.neighborhoods + 1):nrow(temp), 1]
  X <- cbind(dummy, dummy.time, lags[(2 * n.neighborhoods + 1):nrow(lags), ])
  
  beta.m[, m] <- solve(t(X) %*% X) %*% t(X) %*% Y
  res.m[, m]  <- Y - X %*% beta.m[ ,m]

# We find the parameters for the outside option, as we do not have other regressors than the valuefunctions
  
  val.vec.out = as.matrix(as.vector(value.functions.tilde[m, n.neighborhoods + 1, ]))
  
  lag1 <- rbind(matrix(0, 1, 1), as.matrix(val.vec.out[1:(nrow(val.vec.out) - 1), ]))
  lag2 <- rbind(matrix(0, 2, 1), as.matrix(val.vec.out[1:(nrow(val.vec.out) - 2), ]))
  lags <- cbind(lag1, lag2)
  
  Y <- val.vec.out[(2 + 1):nrow(val.vec.out), 1]
  X <- cbind(matrix(1,length(Y),1), matrix(3:(n.periods),n.periods - 2, 1), lags[(2 + 1):nrow(val.vec.out), ])
  
  beta.o[, m] <- solve(t(X) %*% X) %*% t(X) %*% Y
  res.o[, m]  <- Y - X %*% beta.o[, m]
}

##### Regressing the evolvement of prices as a function of Nhood amenities ----

price.vec <- as.matrix(as.vector(as.matrix(meanprices[1:n.neighborhoods, ])))
temp.t    <- as.matrix(cbind(price.vec, crime.vec, traffic.vec, expense.vec, meanprice.vec))
lag1      <- rbind(matrix(0,nrow = n.neighborhoods, 3), temp.t[1:(nrow(temp.t) - n.neighborhoods), ])
lag2      <- rbind(matrix(0,nrow = (n.neighborhoods * 2), 3), temp.t[1:(nrow(temp.t) - n.neighborhoods * 2), ])
lags      <- cbind(lag1, lag2)

Y <- temp.t[(2 * n.neighborhoods + 1):nrow(temp.t), 1]
X <- cbind(dummy, dummy.time, lags[(2 * n.neighborhoods + 1):nrow(lags), ])

beta.price <- ginv(t(X) %*% X) %*% t(X) %*% Y
res.price  <- Y - X %*% beta.price

##### Preparing simulation of the expected value functions based on regressions -----
# Number of draws for integration
R = 100

# Splitting coefficients from regressions

# Kappa is the valuefunction coefficients for the lags, nhood and time trend dummies
kappa0 <- beta.m[1:n.neighborhoods, ]
kappa1 <- beta.m[(n.neighborhoods + 1):(2 * n.neighborhoods), ]

# Alpha is the  valuefunction coefficients  for crime and pollution amenities with lags
alpha11 <- beta.m[(2 * n.neighborhoods + 1):(2 * n.neighborhoods + 5), ]
alpha12 <- beta.m[(2 * n.neighborhoods + 6):(2 * n.neighborhoods + 10), ]

# Omega is the price coefficients for the nhood and time trend dummies
omega0 <- beta.price[1:n.neighborhoods, ]
omega1 <- beta.price[(n.neighborhoods + 1):(2 * n.neighborhoods), ]

# gamma is the price coefficients for the amenities 
gamma11 <- beta.price[(2 * n.neighborhoods + 1):(2 * n.neighborhoods + 5), ]
gamma12 <- beta.price[(2 * n.neighborhoods + 6):(2 * n.neighborhoods + 10), ]

# Draw simulated residuals given by the distributions from the value regressions
resinsidedraws  <- matrix(nrow = n.types, ncol = R)
resoutsidedraws <- matrix(nrow = n.types, ncol = R)
pricedraws      <- matrix(1, R)

for (m in 1:n.types) {
 
  resinsidedraws[m, ]  <- DrawResiduals(R, res.m[, m])
  resoutsidedraws[m, ] <- DrawResiduals(R, res.o[, m])
  
}

pricedraws <- DrawResiduals(R, as.matrix(res.price[, 1]))

# Create combined array of expected values of all each type/year combination values and the corresponding amenities

type.val     <- array(NA, dim = c(n.types, n.neighborhoods, n.periods))
type.val.out <- array(NA, dim = c(n.types, n.periods))

for (m in 1:n.types) {
  for (t in 1:n.periods) {
    type.val[m, , t] <- t(rbind(t(value.functions.tilde[m,1:n.neighborhoods,t]), df.crime[1:n.neighborhoods, t], df.traf[n.neighborhoods, t],
                              df.udg[n.neighborhoods,t], meanprices[n.neighborhoods,t])) %*% alpha11[, m] +
                        t(rbind(t(value.functions.tilde[m,1:n.neighborhoods,max(t - 1,1)]), df.crime[1:n.neighborhoods, max(t - 1,1)], df.pollution[n.neighborhoods, max(t - 1,1)],
                                df.udg[n.neighborhoods,max(t - 1,1)], meanprices[n.neighborhoods,max(t - 1,1)]  )) %*% alpha12[, m] +
                        kappa0[ ,m] + kappa1[ ,m] * (t + 1)
    
    type.val.out[m, t] <- beta.o[3,m] * value.functions.tilde[m, n.neighborhoods + 1,t] + 
      beta.o[4,m] * value.functions.tilde[m,n.neighborhoods + 1, max(t-1,1)] +
      beta.o[2,m] * (t + 1) + beta.o[1,m]
    
  }
}

##### Full integration over expected movements #############

next.wealth.moving  <- matrix(0,n.neighborhoods,n.types)
next.wealth.staying <- matrix(0,n.neighborhoods, n.types)

type.stay <- matrix(0,n.neighborhoods,n.types)
type.move <- matrix(0,n.neighborhoods,n.types)

exp.sum   <- array(NA, dim = dim(value.functions.tilde)) 
utiltilde <- array(NA, dim = dim(value.functions.tilde)) 
total.sum <- array(NA, dim = c(n.types,n.neighborhoods * n.types, R))

ptm <- proc.time()


for(t in 2:n.periods) {
  
  # Estimate prices of neighborhoods given the amenities at time t
  price.time.t <- (cbind(meanprices[, t], df.crime[, t], df.traf[, t], df.udg[, t])) %*% gamma11 + 
                  (cbind(meanprices[, t - 1], df.crime[, t - 1], df.traf[, t - 1], df.udg[, t - 1])) %*% gamma12 + omega0 + omega1 * (t + 1)

  # Estimate psychological moving costs
  exp.pmc.time <- exp(pmc.inv + mc.timetrend * t)
  
  # Draw residuals from the priceregression
  taures = matrix(0,n.neighborhoods, R)
  for (j in 1:n.neighborhoods) {
    
    taures.ind  <- sample(R, R)
    taures[j, ] <- pricedraws[taures.ind]
    
  }
  
  # Draw residuals from valueregressions
  value.res <- array(0,dim = c(n.types, n.neighborhoods + 1, R))

  for (m in n.types) {
    
    # Inside decision draws
    for (j in n.neighborhoods){
      
      value.res.ind     <- sample(R, R)
      value.res[m, j, ] <- resinsidedraws[m, value.res.ind]
      
    }
    
    # Outside decision draws
    value.res.out.ind <- sample(R, R)
    value.res[m , n.neighborhoods +1, ] <- resoutsidedraws[m, value.res.out.ind]
  }
  
  #
  for (r in 1:R){
    
    # add residual r to the price regression draw
    price.and.res <- price.time.t + taures[, r]
    
    # Combine the outside and neighborhood values
    value.t <- cbind((type.val[ , ,t] + value.res[,1:n.neighborhoods,r]), (type.val.out[ ,t] + value.res[,n.neighborhoods + 1, r]))
    
    # take the exponential of the values
    expvalue <- exp(value.t[,1:n.neighborhoods]) 
    
    # Summed values of all neighborhoods
    sum.exp.val.pmc <- rowSums(expvalue) / exp.pmc.time
    
    # Find the added value from the neighborhood amenities for moving and staying and multiply with the parameters
    f.moving       <- price.and.res - meanprices[,2] - moving.costs * price.and.res
    egamma.fmoving <- exp(f.moving %*% gammafmctau)
    
    f.staying       <- price.and.res - meanprices[,2]
    egamma.fstaying <- exp(f.staying %*% gammafmctau)
    
    # Find new types in next period
    for(m in 1:n.types){
      
      # Find the new wealth bins for the given decisions
      
      
      next.wealth.moving[, m]  <- round(pmin(wealth.max, pmax(type.comb[m,1] + price.and.res - meanprices[,t] - 0.06 * price.and.res, wealth.min)))
      next.wealth.staying[, m] <- round(pmin(wealth.max, pmax(type.comb[m,1] + price.and.res - meanprices[,t], wealth.min)))
      
      temp.move <- factor(findInterval(next.wealth.moving[, m], c(-Inf, quantile(zdata$wealth, probs = wealth.decil), Inf)))
      temp.stay <- factor(findInterval(next.wealth.staying[, m], c(-Inf, quantile(zdata$wealth, probs = wealth.decil), Inf)))
      
      type.move[, m] <- as.numeric(as.character(temp.move))
      type.stay[, m] <- as.numeric(as.character(temp.stay))
      
      # Find the total exp sums of all values from all neighborhoods for a given type/year combination with residual r
      total.sum[m, ,r] <- sum.exp.val.pmc[type.move[, m]] * egamma.fmoving[, m] + expvalue[type.stay[, m]] * egamma.fstaying[, m]
      
    }
  }
  
  FV <- rowMeans(log(total.sum[,,])) + Euler
  
  utiltilde[ ,1:n.neighborhoods, t] = value.functions.tilde[,1:n.neighborhoods, t] - beta * FV
  
}

proc.time() - ptm


