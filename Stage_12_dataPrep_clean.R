

library(tidyr)
library(dplyr)
library(MASS)

# parameters & Initial values -----------------------------

# The areas under investigation in the model
used.areas = c(1:5) # Skal instilles til de kommuner vi gerne vil have med

# lists of neoghborhoods and years
neighboorhood <- unique(zdata$area)
years <- unique(zdata$current.year)

# Counting the different vairables
n.periods <- length(unique(zdata$current.year))
n.neighborhoods <- length(used.areas)
nr.nhood <- length(used.areas)
n.obs <- nrow(zdata)
nr.obs <- n.obs
n.years <- length(years)

# Cost of moving
moving.costs <- 0.06  

# create dummy for outside choice when estimating CCP's
zdata$outside <- ifelse(zdata$flyt == 1 & !(zdata$kom.t.1 %in% used.areas), 1, 0)

# indicators for years and neighborhoods
for (i in 1:n.obs) {
 zdata$year.ind[i] = which(years == zdata$current.year[i])
}

for(i in 1:n.obs) {
  zdata$nhood[i] = which(neighboorhood == zdata$area[i])
}

# ------------------------Splitting observations in types based on income and wealth ----------------------
# Nr of types in each bin
n.incometypes = 2
n.wealthtypes = 2

# creating equal length intervals of types
# Income
zdata$income = as.numeric(zdata$income)
income.max = max(zdata$income)
income.min = min(zdata$income)
income.bins = seq(income.min, income.max, income.max / n.incometypes)
income.bins[n.incometypes + 1] <- Inf                                # Det maksimale loft for indtægt i den sidste gruppe er uendeligt

# Wealth
zdata$wealth = as.numeric(zdata$wealth)
wealth.max = max(zdata$wealth)
wealth.min = min(zdata$wealth)
wealth.bins = seq(wealth.min, wealth.max, wealth.max / n.wealthtypes)
wealth.bins[n.wealthtypes + 1] <- Inf                                 # det maksimale loft for formue i det sidste led er uendeligt

n.types <- n.wealthtypes * n.incometypes

# Categorizing the observations in their respective types
type.matrix <- as.data.frame(matrix(1:n.types, n.wealthtypes, n.incometypes))

for (i in 1:n.obs) {
  for (j in 1:n.incometypes) {
    for (w in 1:n.wealthtypes) {
      
      if(zdata$income[i] >= income.bins[j] & zdata$income[i] < income.bins[j + 1]
       & zdata$wealth[i] >= wealth.bins[w] & zdata$wealth[i] < wealth.bins[w + 1]) 
      {
        zdata$type.tau[i] <- type.matrix[w,j]
      }
  }
 } 
}


# New number of types, as we cant loop over types without content ## OBS Tjek lige om det her step er korrekt
n.types <- length(unique(zdata$type.tau))

# -------------Creating the Conditional Choice Probabilities by splitting into time, neighborhoods and years -------------------

# constructing frequency tables for of each type/year combination and moving decisions
# nr. of obs. in each combination
group.obs <- zdata %>% count(type.tau,year.ind,flyt)

group.obs$type.tau <- as.factor(group.obs$type.tau)
group.obs$year.ind <- as.factor(group.obs$year.ind)
group.obs$flyt     <- as.factor(group.obs$flyt)

full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)), 
                        flyt = levels(as.factor(zdata$flyt)))

group.obs <- left_join(full.grid, group.obs)

group.obs$n[is.na(group.obs$n)] <- 0

# nr. of obs. moving
group.obs.move <- zdata %>% count(type.tau,year.ind,flyt, kom.t.1)

group.obs.move$type.tau <- as.factor(group.obs.move$type.tau)
group.obs.move$year.ind <- as.factor(group.obs.move$year.ind)
group.obs.move$flyt     <- as.factor(group.obs.move$flyt)
group.obs.move$kom.t.1  <- as.factor(group.obs.move$kom.t.1)

full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)),
                        kom.t.1 = levels(as.factor(zdata$kom.t.1)), flyt = levels(as.factor(zdata$flyt)))

group.obs.move <- left_join(full.grid,group.obs.move)

group.obs.move$n[is.na(group.obs.move$n)] <- 0

# nr. of obs moving outside the chosen areas

group.obs.move.out <- zdata %>% count(type.tau,year.ind,flyt, outside)

group.obs.move.out$type.tau <- as.factor(group.obs.move.out$type.tau)
group.obs.move.out$year.ind <- as.factor(group.obs.move.out$year.ind)
group.obs.move.out$flyt     <- as.factor(group.obs.move.out$flyt)
group.obs.move.out$outside  <- as.factor(group.obs.move.out$outside)

full.grid = expand.grid(type.tau = levels(as.factor(zdata$type.tau)), year.ind = levels(as.factor(zdata$year.ind)), 
                        outside = levels(as.factor(zdata$outside)))

group.obs.move.out <- left_join(full.grid, group.obs.move.out)

group.obs.move.out$n[is.na(group.obs.move.out$n)] <- 0

# Finding the CCP's
shares <- array(0,dim=c(n.types,n.neighborhoods + 1, n.periods))

# CCP of moving to neighboorhood j conditional on moving
for (t in 1:n.periods) {
  for (m in 1:n.types) {
     for (j in 1:n.neighborhoods) {
       
       sum.move <- group.obs$n[(group.obs$type.tau == m) & (group.obs$year.ind == t) & (group.obs$flyt == 1)] 
       sum.tau  <- group.obs.move$n[(group.obs.move$kom.t.1 == j) & (group.obs.move$flyt == 1) 
                                    & (group.obs.move$type.tau == m) & (group.obs.move$year.ind == t)] 
                                  
       if (sum.tau == 0){
         shares[m,j,t] = 0.000001
       }
       
       else if (sum.move == 0 & sum.tau > 0) {
         shares[m,j,t] = 1
       }

       else {
         shares[m,j,t] = sum.tau / sum.move
       }
    }
  }
}


# CCP of moving outside conditional on moving

for (t in 1:n.periods) {
  for (m in 1:n.types) {
    sum.move <- group.obs$n[(group.obs$type.tau == m) & (group.obs$year.ind == t) & (group.obs$flyt == 1)]
    sum.out  <- group.obs.move.out$n[(group.obs.move.out$type.tau == m) & group.obs.move.out$year.ind == t 
                                     & (group.obs.move.out$outside == 1)]
    
    if (sum.out == 0) {
      shares[m, n.neighborhoods + 1, t] = 0.00000001
    }
    
    else if (sum.move == 0 & sum.out > 0) {
      shares[m, n.neighborhoods + 1 ,t] = 1
    }

    else {
      shares[m, n.neighborhoods + 1, t] = sum.out / sum.move
    }
  }
}

# Creating the ingoing values for the likelihood estimator (the X vector) ---------------------------

# X1 = 1
zdata$x1 = 1

# X2 = income (Psychological Moving Costs)
zdata$x2 = zdata$income / 10000

# x3 = 1
zdata$x3 = 1

# x4 = Price_t * moving costs (Financial moving costs)

# First we need to find the potential price in all periods OBS! Her skal laves en variable for husprisindeks afhængig af område

## Midlertidig prisudvikling
stignings.proc = 0.017 #midlertidig fra SIM rapport

stig.vec = array()
for (t in 1:n.periods) {
  stig.vec[t] <- (1+0.017)^t 
}

for (i in 1:n.obs) {
  if (zdata$current.year[i] == min(years)) {
    
    zdata$price[i] = zdata$vurdering[i]
  
    for (t in 1:(n.periods-1)) {
      
      zdata$price[i + t] <- zdata$vurdering[i] * stig.vec[t] 
    }
  }
}

zdata$x4 = (zdata$price / 10000) * moving.costs

# x5 = price * movingcosts * income (Type specific financial moving costs)
zdata$x5 <- (zdata$price / 10000) * moving.costs * (zdata$income / 10000)

# vector of x values for MLE
x_sx <- as.matrix(zdata[ ,c("x1","x2","x3","x4","x5")])

y_sx <- zdata[ ,"flyt"]
