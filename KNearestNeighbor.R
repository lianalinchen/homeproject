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

#####################
# K nearest neighbor#
#####################
yhat4 <- yhat5 <- Xp
k1 <- 5
k2 <- 20
for(j in 1:np){
  rrr <- rank(dp[j,])
  rrr <- ifelse(rrr == min(rrr),0,rrr)
  yhat4[j] <- mean(Yo[rrr<=k1])
  yhat5[j] <- mean(Yo[rrr<=k2])
} 
plot(Xo,Yo)
lines(Xp,yhat4,col=2,lwd=2)
lines(Xp,yhat5,col=3,lwd=2)
legend("topright",c("K=5","K=20"),lwd=2,col=2:3,inset=0.05)


