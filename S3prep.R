# OBS! Fra dette skridt bruger de i estimationen kun de 3 indkomstgrupper
# de gerne vil finde MWP fra

registerDoParallel(cores = 4)
## Estimation step 3 prep - Recoveringm values and prep for getting back valuef

gamma <- c(-9.5, 0.02, 0.1, -0.2, 0.006) # will be given by former estimations

#Splitting gamma into FMC, PMC and a moving cost time trend coefficient

gamma.pmc    <- gamma[1:2]
gamma.fmc    <- gamma[4:5]
mc.timetrend <- gamma[3]

# We retrieve the mtau values and time invarient moving costs
gammafmctau   <- rep(0,n.types)
m.tau   <- rep(0,n.types)
pmc.inv <- matrix(0, n.types ,1)

for (m in 1:n.types){
  
  gammafmctau[m] <- gamma.fmc[1] + gamma.fmc[2] * type.comb[m, "income"]
  
  m.tau[m]   <- gammafmctau[m] * type.comb[m, "wealth"]
  
  pmc.inv[m] <- (gamma.pmc[1] * type.comb[m,"wealth"] + gamma.pmc[2] * type.comb[m, "income"])
                                                                                 
}

## Obs Her skal besluttes vorvidt vi ønsker at bruge alle typeværdier
## eller trække enkelte ud af sættet til MWP regressionerne

# Simulate Nhood matrix

df.tyv  <- read.csv("C:\\Users\\Langholz\\Documents\\GitHub\\DynamicProgramming_TermPaper\\Term paper model\\amenities\\tyveri.csv",sep = ";", header = TRUE)
df.traf <- read.csv("C:\\Users\\Langholz\\Documents\\GitHub\\DynamicProgramming_TermPaper\\Term paper model\\amenities\\uheldstæthed.csv", sep = ";", header = TRUE)
df.udg  <- read.csv("C:\\Users\\Langholz\\Documents\\GitHub\\DynamicProgramming_TermPaper\\Term paper model\\amenities\\udg.kultur+sport.csv", sep = ";", header = TRUE)

df.traf$kom.nr <- as.numeric(df.traf$kom.nr)

setorder(df.traf, kom.nr)
setorder(df.tyv, kom.nr)
setorder(df.udg, Kom.nr)

# Sort out names + data before 1998
df.tyv  <- df.tyv[,-c(1:7,21:24)] 
df.traf <- df.traf[,-c(1:2, 16:18)]
df.udg  <- df.udg[,-c(1:7, 21:25)]

crime.vec <- as.matrix(as.vector(df.tyv))
traffic.vec  <- as.matrix(as.vector(df.traf))
expense.vec <- as.matrix(as.vector(df.udg))

# Create time and nhoods dummies
n.lags = 2

dummy <- rep(c(1:n.neighborhoods), n.periods)
dummy <- dummy(dummy[(n.lags * n.neighborhoods + 1):length(dummy)])

time = dummy
dummy.time <- c((n.lags + 1):n.periods)
dummy.time <- time * as.vector(kronecker(dummy.time, rep(1, n.neighborhoods)))
    
# Find the mean price level for all years

meanprices <- data.frame(matrix(0,nrow = n.neighborhoods, ncol = n.periods))

for (t in 1:n.periods) {
  for (j in 1:n.neighborhoods) {
    
    temp <- subset(zdata, year.ind == t & nhood == j)
    meanprices[j,t] = mean(temp$price)
    
  }
}

meanprice.vec = as.matrix(as.vector(meanprices))
length(meanprice.vec)



