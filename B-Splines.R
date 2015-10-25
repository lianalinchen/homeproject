#A method using spline smoothing
library(splines)
set.seed(8020)

n <- 100
x <- seq(0,1,length = n)
fx <- 10*x*sin(10*pi*x)+3*cos(s*pi*x)
# mean=fx, sd=1
y <- rnorm(n, fx, 1)

plot(x,y)
lines(x,fx)
legend("topleft" , c("Data","True Curve"), lty = c(NA,1), pch=c(1,NA), inset=0.05)

#plot the b spline basic function
df <- 10
B <- bs(x, df)
dim(B)
matplot(B, type="l", lty=1)

#######################
# Fit spline using OLS#
#######################
fit <- lm( y~B)
fhat <- fit$fitted
summary(fit)

plot(x,y)
lines(x, fx)
lines(x, fhat, col=2)
legend("topleft",c("Data","True curve","Estimated curve"),
       lty=c(NA,1,1),pch=c(1,NA,NA),col=c(1,1,2),inset=0.05)


########################################################################
# FIT splines using OLS and pick the number of basis function using BIC#
########################################################################
df <- seq(5,90,1)
R2 <- df
aR2 <- df
BIC <- df
for(j in 1:length(df)){
  B <- bs(x, df[j])
  fit <- lm( y~B )
  R2[j] <- summary(fit)$r.squared
  aR2[j] <- summary(fit)$adj.r.squared 
  BIC[j] <- BIC(fit)
}

plot(df, R2, type="l", ylim=0:1)
lines(df,aR2, type="l", ylim=0:1)
legend("bottomright",c("R2","Adjusted R2"), lty=1, col=1:2, inset=0.05)

plot(df, BIC, type="l")

############################################################
# FIT splines using 50 Basis function and ridge regression #
############################################################
B <- cbind(1, bs(x,50))
matplot(B, type="l", lty=1) 
p <- ncol(B)
D <- 2*diag(p)
D[1,1] <- 0 
BB <- t(B) %*% B
lamda <- .1
H <- solve(BB + lamda*D) %*% t(B)
bhat <- H %*% y
fhat   <- B%*%bhat
effpar <- sum(diag(B%*%H))
plot(x,y,main=paste("Effective number of parameters =",round(effpar,1)))
lines(x,fhat)


##############################################################################
# B splines lead to sparse matrix                                            #
# It doesn't matter for the small matrix operations above,                   #
# but in harder problems exploiting sparcity can really speed up computation.#
##############################################################################

