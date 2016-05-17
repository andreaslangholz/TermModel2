
EstimateValuefunctionsTilde <- function(shares, n.types, n.neighborhoods, n.periods){
  
  #  Calculates the estimated valuefunctions based on the FOC of MLE. Each valuefunction equals the difference in moving probability
  #  minus the average propensity to move to a specific neighborhood for a given type of household. 
  #  Equation (18) in paper. 
  #
  #Args :
  #  shares: the share of households moving
  #  n.types
  
  cells = n.types * (n.neighborhoods + 1) * n.periods
  
  value.tilde <- array(rep(NaN, cells), c(n.types, (n.neighborhoods + 1), n.periods))
  
  for (m in 1:n.types){
    
    for (t in 1:n.periods){
      
      value.tilde[m, , t] <- log(shares[m, , t]) - mean(log(shares[m, ,t]))
      
    }
  }
  
  return(value.tilde)
  
}


LikelihoodOfStayDecision <- function(y, x, value.stay, value.move, initial.param, max.iteration, tolerance){
  
  b = initial.param
  criterion = 1
  iteration = 1
  
  nr.vars = length(b) 
  
  #  Looping over successive ML approximations by the Newton Rhapson algorithm
  while (criterion > tolerance & iteration < max.iteration) {
      
    
    #  Calculate the CCP of staying in residence - eq (15)
    ccp.stay = value.stay / (value.stay + value.move * exp(x %*% b)) 
    
    #  Take the gradient of the likelihood 
    gradient = t(y - ccp.stay) %*% x
    
    #  Take the pdf of the choice probabilities which enters into the Hessian
    ccp.pdf = ccp.stay * (1 - ccp.stay)
    
    w = matrix(data = rep(NaN, n.obs * nr.vars), nrow = n.obs, ncol = nr.vars)
    
    for (i in 1:nr.vars){
      
      w[, i] = ccp.pdf
      
    }
    
    #  Calculate the Hessian 
    hessian = - t(w * x) %*% x
    
    hessian.inv = ginv(hessian)
    
    # Residual in the Newton - Rhapson procedure
    newton.raps.res = - hessian.inv %*% t(gradient)
    
    # Newton-Rhapson optimization
    step.size = 2
    
    likelihood.a = 0
    likelihood.b = 1
    
    while (likelihood.b > likelihood.a) {
      
      step.size = step.size / 2
      
      b.tilde.a = b + step.size * newton.raps.res
      b.tilde.b = b + step.size * newton.raps.res / 2
      
      likelihood.a = LikelihoodLogitNR(b.tilde.a, y, x, value.stay, value.move)
      likelihood.b = LikelihoodLogitNR(b.tilde.b, y, x, value.stay, value.move)
      
    }
    
    b.tilde = b + step.size * newton.raps.res
    b = b.tilde
    
    criterion = max(newton.raps.res)
    iteration = iteration + 1
  
  }
  
  likelihood = LikelihoodLogitNR(b, y, x, value.stay, value.move)  
  
  output = list(b, likelihood, iteration, criterion)  
  
  return(output)
}


LikelihoodLogitNR <- function(b.tilde, y, x, value.stay, value.move ){
  #  Helper function for LikelihoodOfStayDecision to compute the 
  #  likelihood in each step of the Newton Rhapson algorithm
  # 
  #  Args: 
  #  b.tilde: beta parameters with added NR residual  
  
  ccp.stay = value.stay / (value.stay + value.move * exp(x %*% b.tilde))
  
  likelihood.contribution = y * log(ccp.stay) + (1 - y) * log(1 - ccp.stay)
  
  likelihood = sum(likelihood.contribution)
  
  return(likelihood)
  
}


DrawResiduals <- function(ndraws, residuals) {
  #  Draws residuals from the given distributions obtained by the valuefunction and price regressions. 
  #  Uses John Kennans (2006) Approximations of Continuous Distributions, to derive the approximation of the empirical distributions.
  #
  #  Args:
  #  ndraws: number of draws from the distribution
  #  residuals: The emperical residuals obtained by the regressions.
  
  n <- nrow(residuals)

  # Evaluate whether the number of draws exceeds the number of given residuals. 
  # If TRUE, se actual residuals in draws and fill out with approximations.
  if(ndraws >= n) {
    n1 <- floor(ndraws / n)
    
    # Multiply empirical residuals to fillout as much as possible
    A <-  kronecker(matrix(1, n1, 1), residuals)
    
    # Test if there is are draws left that needs to be approximated
    rest <- ndraws - (n1 * n)
    
    if(rest > 0) {
      
      # Apply John Kennan approximation
      B <- matrix(0, rest, 1)
      
      for (i in 1:rest) {
        F.approx1 <- (2 * i - 1) / (2 * rest)
        B[i] <- quantile(residuals, F.approx1)
      }
      
      # Return full set
      draws = rbind(A, B)
    } else {
      draws = A
    }
    
  # If number of draws is lower than the number of residuals given, approximate all draws from distribution
  } else {
    
    draws <- matrix(0, ndraws,1)
    for (i in 1:ndraws) {
      
      F.approx2 <- (2 * i - 1) / (2 * ndraws)
      draws[i] <- quantile(residuals, F.approx2)
      
    }
  }
  return(draws)
}
