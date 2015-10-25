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
##########################
# Local Linear Regression#
##########################
# Define the local weights for each prediction point for two bandwidth 
bw1 <- 2
w1 <- exp(-dp/bw1)
bw2 <- 0.5
w2 <- exp(-dp/bw2)

# Plot to see how distance between different observation points change
plot(Xo, w1[20,], type = "l")
points(Xo, w1[20,])
lines(Xo,w2[20,], type = "l", col=2)
points(Xo,w2[20,],col=2)
abline(v=Xp[20], col=3)
legend("topright", c("bw=10","bw=2","Prediction point"), col=1:3, lty=1, pch=c(1,1,NA),inset=0.05)

# Perform local regression 
yhat1 <- yhat2 <- Yp

for(j in 1:np){ 
  fit      <- lm(Yo~Xo,w=w1[j,])
  yhat1[j] <- fit$coef[1] + fit$coef[2]*Xp[j]
  fit      <- lm(Yo~Xo,w=w2[j,])
  yhat2[j] <- fit$coef[1] + fit$coef[2]*Xp[j]
} 
fit <- loess(Yo~Xo)
yhat3<- predict(fit,data.frame(Xo = Xp))

plot(Xo, Yo)
# bw = 2
lines(Xp, yhat1, col=2, lwd=2)
# bw = 0.5
lines(Xp, yhat2, col=3, lwd=2)
lines(Xp, yhat3, col=4, lwd=2)
legend("topright",c("bw-2","bw=0.5","loess"),lwd=2,col=2:4,inset=0.05)
