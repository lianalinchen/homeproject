# Fit a generalized addtive model
set.seed(0820)
# 500 observations
n <- 500

x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
x4 <- rnorm(n)

f1 <- 2*x1
f2 <- 3*cos(2*x2)
f3 <- x3^2
f4 <- 0*x4
Y <- f1 + f2 + f3 +  f4 + rnorm(n)

plot(x1,Y, title('Y - x1'))
lines(x1,f1, col=2, lwd=3 )

plot(x2,Y, title('Y - x2'))
points(x2,f2,col=2,pch=9) 
 
plot(x3,Y, title('Y - x3'))
points(x3,f3,col=2,pch=9) 

plot(x4,Y, title('Y - x4')) 
lines(x4,f4, col=2, lwd=3 )


#Added variable to look at non-linearity
res <- lm(Y~x1+x2+x3+x4)$res
plot(x1,res)
# loess: Local Linear Regression
lo <- loess(res~x1)
summary(lo)
lines(lo$x[order(x1)],lo$fit[order(x1)], col=2, lwd=2)
 
plot(x2,res)
# loess: Local Linear Regression
lo <- loess(res~x2)
lines(lo$x[order(x2)],lo$fit[order(x2)], col=2, lwd=2)

plot(x3,res)
lo    <- loess(res~x3)
lines(lo$x[order(x3)],lo$fit[order(x3)],col=2,lwd=2)

plot(x4,res)
lo    <- loess(res~x4)
lines(lo$x[order(x4)],lo$fit[order(x4)],col=2,lwd=2)

install.packages("gam")
library(gam)
fit1 <- gam(Y ~ x1+x2+x3+x4)

par(mfrow=c(2,2))
plot(fit1)

# Fit the gam with splines
fit2 <- gam(Y ~ s(x1)+s(x2)+s(x3)+s(x4))
plot(fit2)
summary(fit2)

# Fit the gam with loess
fit3 <- gam(Y ~ lo(x1) + lo(x2) + lo(x3) + lo(x4))
plot(fit3)
summary(fit3)

# Fit the gam with loess for X2 and X3 only
fit4 <- gam(Y ~ x1 + lo(x2) + lo(x3) + x4)
plot(fit4)
summary(fit4)


# Fit the gam with loess for x2 x3 and their interaction
par(mfrow=c(3,2))
require("akima")
fit5 <- gam(Y ~ x1 + lo(x2) + lo(x3) + lo(x2,x4) + x4)
plot(fit5)
summary(fit5)

