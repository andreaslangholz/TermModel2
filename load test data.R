
#Load in testdata 

matlabdir = 'C:/Users/jzk870/Documents/MATLAB/DMDHN/Est_Files/'
y = read.table(paste0(matlabdir, "y_sx.txt"), header=F)
x = read.table(paste0(matlabdir, "x_sx.txt"), header=F)
y = y$V1
x_sx = matrix(data = unlist(x), nrow = nr.obs, ncol = 5)

# Get shares
for (i in 1:11) {
  
  n = paste("share_y",i, ".txt", sep = "")
  m0 = read.table(paste0(matlabdir, n), header=F)
  m =  matrix(data = unlist(m0), nrow = n.types, ncol = 219)
  
  assign(paste("share.year.", i, sep = ""), m)
  
}

# Put shares into 3D array
all.shares = array(data = rep(0, (nr.nhood + 1) * n.types * 11), c(n.types, nr.nhood + 1, 11))
for (i in 1:11) {
  n = paste0('share.year.', i)
  all.shares[, , i] = get(n)
}  

rm(share.year.1, share.year.2, share.year.3, share.year.4, share.year.5, share.year.6, share.year.7, share.year.8, share.year.9, share.year.10, share.year.11)

nr.nhood = 218
nr.obs = length(y)
n.types = 625

# create artificial types
for (i in 1:nr.obs) {
  
  type0 = sample(x = 1:625, 1, replace = T)
  year0 = sample(x = 1:11, 1, replace = T)
  
  a = cbind(type0, nr.nhood + 1, year0)
  b = cbind(type0, year0)
  
  value.stay[i] = exp.value.functions.tilde[a] 
  value.move[i] = summed.exp.value.functions.tilde[b]
  
}
