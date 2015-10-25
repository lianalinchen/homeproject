load("/Users/apple/Desktop/homes.RData")
dim(homes)
long <- homes[,5]
precip <- homes[,7]
# order returns a permutation (index of a order) which rearranges its first argument into ascending or descending 
# order, breaking ties by further arguments.
ooo <- order(long)
long <- long[ooo]
precip <- precip[ooo]
n <- length(long)
plot(long,precip)

############################################
# Split the data into training and test set#
############################################
library(fields)
set.seed(8020)
np <- 1000
test <- sort(sample(1:n, np,replace = TRUE))
Xp <- long[test]
Yp <- precip[test]
Xo <- long[-test]
Yo <- precip[-test]
#Distance matrix of observation and prediction
do <- rdist(Xo,Xo)
diag(do) <- 0
dp <- rdist(Xp, Xo) 

##############################
# Gaussian Process Regression#
##############################
#Take off the mean trend and standardize residuals
b <- lm(Yo ~ Xo)$coef
r <- Yo - b[1] - b[2]*Xo
s <- sd(r)
r <- r/s
# Define log likelihood
GPcov <- function(d,rho){
  0.5 * exp( -d/rho ) + 0.5 * (d == 0)
}

log.like <- function(y, d, rho){
  S <- solve(GPcov(d,rho))
  l <- 0.5*determinant(S)$modulus[1] - 
    0.5*t(y)%*%S%*%y
  return(l)
}
 

rho.grid <- seq(0.1,5,length=20)
ll       <- rep(NA,20)

for(j in 1:length(ll)){
  ll[j] <- log.like(r,do,rho.grid[j])
}

plot(rho.grid,ll,type="l")

rho <- rho.grid[which.max(ll)]

So  <- GPcov(do,rho)
Sp  <- GPcov(dp,rho)
# r: residue
rhat  <- Sp%*%solve(So)%*%r 
yhat6 <- b[1] + b[2]*Xp + s*rhat

plot(Xo,Yo)
lines(Xp,yhat6,col=2,lwd=2)

 









