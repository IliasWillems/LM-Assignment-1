
# loading training and test data 

data.full = read.table("dataset.txt",header=T)
set.seed(0199510)
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]
n <- dim(data.training)[1]
p <- dim(data.training)[2]


####################################################
#   Model with interaction term (without effort)   #
####################################################

# When deleting Effort, Natural is not significant anymore --> delete natural as well

fit5 <- lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy,data=data.training)
summary(fit5)
# The interaction term is significant and the adjusted R^2 is 0.9308

# Model assumptions
fit5.res <- residuals(fit5)
fit5.stdres <- stdres(fit5)
fit5.fittedvalues <- fitted.values(fit5)
par(mfrow = c(1,1))
qqnorm(fit5.stdres, main="")
qqline(fit5.stdres)
plot(fit5.res, xlab = "Index", ylab = "Residual")
plot(fit5.fittedvalues, fit5.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit5.res ~ fit5.fittedvalues), col = "red")
plot(fit5.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# Outlier observation 108


par(mfrow = c(1,3))
plot(data.training$Canopy, fit5.res, ylab = "Residual",xlab="Canopy")
lines(lowess(fit5$residuals ~ data.training$Canopy), col = "red")
plot(data.training$Shrub, fit5.res, ylab = "Residual",xlab="Shrub")
lines(lowess(fit5$residuals ~ data.training$Shrub), col = "red")
plot(data.training$Sex, fit5.res, ylab = "Residual",xlab="Shrub")
#UL: normality issue from the previous model is solved
#LL: not much of a funnel shape 
#LR: one outlier (observation 218)

summary(lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy,data=data.training))

#Investigate outliers
par(mfrow=c(1,1))
# DFFITS
fit5.dffits <- dffits(fit5)
plot(fit5.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")
abline(h = -2*sqrt(p/n), col = "red")
#3 outliers

fit5.dffits[77]
fit5.dffits[87]
fit5.dffits[108]

# Outliers 77,87,108

# DFBETAS
par(mfrow=c(2,2))
fit5.dfbetas <- dfbetas(fit5)
fit5.dfbetas
2/sqrt(n)
plot(fit5.dfbetas[,2], main="DFBETAS Sex")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)

plot(fit5.dfbetas[,3], main="DFBETAS Canopy")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)

plot(fit5.dfbetas[,4], main="DFBETAS Shrub")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)

plot(fit5.dfbetas[,5], main="DFBETAS Canopy:Shrub")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)

plot(fit5.dfbetas[,5], main="DFBETAS Canopy:Shrub")

# Outlier 77,87,108

# Studentized residuals
par(mfrow=c(1,1))
fit5.studres <- studres(fit5)
plot(fit5.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagonal elements of hat matrix
fit5.influence <- influence(fit5)$hat
distance<-sqrt((fit5.influence-1/n)*(n-1))
plot(distance, fit5.studres, ylim = c(-5,5),xlim=c(0,4.5), xlab = "Mahalanobis distances", ylab = "Studentized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")


# Outlier 87,108


#------------------#
# Model validation #
#------------------#

# Model 1: Length ~ Sex+Canopy+Shrub+Effort+Natural
model1.val <- lm(Length ~ Sex+Canopy+Shrub+Canopy*Shrub, data = data.test)
summary(model1.val)
summary(fit5)

MSE <- summary(fit5)$sigma^2
MSEP <- mean((predict(fit5, newdata = data.test) - data.test$Length)^2)
MSE
MSEP


##############
#  outliers  #
##############

# using robust procedure
library(robustbase)

# RLTS: because there is an outlier and a few points close to outlying;
RLTS <- ltsReg(Length ~ Sex+Canopy+Shrub+Canopy*Shrub, data = data.training, alpha = 0.9)
summary(RLTS)
#adjusted R^2 is 0.977

# Detection of outliers
plot(RLTS, which = "rindex")
plot(RLTS, which = "rdiag")
#There are quite some outliers, good leverage points and one bad leverage point

# Standardized residuals 
RLTS.stdres <- RLTS$residuals/RLTS$scale
qqnorm(RLTS.stdres,main="")
qqline(RLTS.stdres)

# Diagnostic plot
plot(RLTS$RD, RLTS.stdres, ylim = c(-12,12), xlab = "Robust distances", ylab = "Standardized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")


#------------------#
# Model validation #
#------------------#

RLTS.val <- ltsReg(Length ~ Sex+Canopy+Shrub+Canopy*Shrub , data = data.test, alpha=0.9) 
summary(RLTS.val)
summary(RLTS)


MSE2 <- summary(RLTS)$sigma^2
predict_RLTS=data.test$Length
for (i in seq(1,160)) {
  predict_RLTS[i]=RLTS$coefficients[1]+
                  RLTS$coefficients[2]*ifelse(data.test[i,1] == "male", 1, 0)+
                  RLTS$coefficients[3]*data.test[i,2]+
                  RLTS$coefficients[4]*data.test[i,3]+
                  RLTS$coefficients[5]*data.test[i,2]*data.test[i,3]
  }
MSEP2 <- sum((predict_RLTS- data.test$Length)^2)/160
MSE2
MSEP2

