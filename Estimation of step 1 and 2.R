

initial.parameters <- c(-9.5, 0.02, 0.1, -0.2, 0.006)

#  Get the estimated value functions by the FOC of MLE problem
value.functions.tilde = EstimateValuefunctionsTilde(shares, n.types, n.neighborhoods, n.periods)

#  Take exp of values
exp.value.functions.tilde = exp(value.functions.tilde)

#  Sum exp(v) over neighborhoods if moving - exclude the last column, which is the value of staying, J+1.
summed.exp.value.functions.tilde = apply(exp.value.functions.tilde[ , , ], MARGIN = c(1,3), sum)

#  Vectors containing the value of staying in same neighborhood or moving 
value.stay = rep(0, nr.obs)
value.move = rep(0, nr.obs)

#  Calculate the value of moving and staying for each observation, given its type.
for (i in 1:nr.obs) {
  
  #  Get the observation's coordinate in the matrix of value functions. Note that nr.nhood + 1 is the column of stay-values in the shares-matrix.  
  a = cbind(zdata$type.tau[i], zdata$nhood[i], zdata$year.ind[i])
  b = cbind(zdata$type.tau[i], zdata$year.ind[i])
  
  value.stay[i] = exp.value.functions.tilde[a] 
  value.move[i] = summed.exp.value.functions.tilde[b]
  
}
