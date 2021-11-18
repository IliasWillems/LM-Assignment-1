
# loading training and test data 

data.full = read.table("dataset.txt",header=T)
set.seed(0199510)
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]
n <- dim(data.training)[1]
p <- dim(data.training)[2]


# making categorical variables numeric

data.training$Sex=ifelse(data.training$Sex=="female",1,0)
data.training$dummy_1=ifelse(data.training$Forest=='Chawia',1,0)
data.training$dummy_2=ifelse(data.training$Forest=="Ngangao N",1,0)
data.training$Natural=ifelse(data.training$Natural=='yes',1,0)
data.test$Sex=ifelse(data.test$Sex=="female",1,0)
data.test$dummy_1=ifelse(data.training$Forest=='Chawia',1,0)
data.test$dummy_2=ifelse(data.training$Forest=="Ngangao N",1,0)
data.test$Natural=ifelse(data.test$Natural=='yes',1,0)

####################################
#   Model with interaction term    #
####################################

# There may be an interaction effect between shrub and canopy (proportion shrubs depends on proportion canopy)

fit1 <- lm(Length ~ Sex+Canopy+Shrub+Effort+Natural+Shrub*Canopy ,data=data.training)
summary(fit1)
# The interaction term is significant and the adjusted R^2 is 0.9624

# Model assumptions
library(MASS)
fit1.res <- residuals(fit1)
fit1.stdres <- stdres(fit1)
fit1.fittedvalues <- fitted.values(fit1)
par(mfrow = c(2,2))
qqnorm(fit1.stdres, main="")
qqline(fit1.stdres)
plot(fit1.res, xlab = "Index", ylab = "Residual")
plot(fit1.fittedvalues, fit1.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit1.res ~ fit1.fittedvalues), col = "red")
plot(fit1.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

#UL: seems a big deviation of normality (in tails)
#LL: funnel shape (heteroscedasticity)
#LR: one outlier (observation 87)

# To solve the non-normality and heteroscedasticity at once, a box-cox transformation of the y-variable is considered
par(mfrow = c(1,1))
out_boxcox <- boxcox(Length ~ Sex+Canopy+Shrub+Effort+Natural+Shrub*Canopy,data=data.training, lambda = seq(0,1,0.001), plotit = TRUE)
lambda <- out_boxcox$x[which(out_boxcox$y == max(out_boxcox$y))]
fit2 <- lm(((Length)^lambda - 1)/lambda ~Sex+Canopy+Shrub+Effort+Natural+Shrub*Canopy,data=data.training)
fit3 <- lm(((Length)^0.5 - 1)/0.5 ~Sex+Canopy+Shrub+Effort+Natural+Shrub*Canopy,data=data.training)
summary(fit2)
summary(fit3)

# adjusted R^2 is 0.9629 and 0.9627 respectively
fit2.res <- residuals(fit2)
fit2.stdres <- stdres(fit2)
fit2.fittedvalues <- fitted.values(fit2)
par(mfrow = c(2,2))
qqnorm(fit2.stdres, main="")
qqline(fit2.stdres)
plot(fit2.res, xlab = "Index", ylab = "Residual")
plot(fit2.fittedvalues, fit2.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit2.res ~ fit2.fittedvalues), col = "red")
plot(fit2.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
# Not much of an improvement, neither for normality nor for heteroscedasticity


# Weighted least squares to solve the heteroscedasticity
w2 <- 1/lm(abs(fit1.stdres) ~ Sex+Canopy+Shrub+Effort+Natural
           +Shrub*Canopy,data=data.training)$fitted.values^2
fit4 <- lm(Length ~ Sex+Canopy+Shrub+Effort+Natural
           +Shrub*Canopy, data=data.training, weight = w2)
summary(fit4)
summary(fit1)

# coefficient estimates are not much different, iteration not needed
# p-values are lower orginal ones because some standard errors decreased
# adjusted R^2=0.9624

# Detection of heteroscedasticity
fit4.res <- fit4$residuals
fit4.fittedvalues <- fit4$fitted.values
par(mfrow = c(1,1))
plot(fit4.fittedvalues, fit4.res*sqrt(w2), xlab = "Fitted value", ylab = "Weighted residual")
par(mfrow = c(2,2))
plot(data.training$Length, fit4.res, xlab = "Length", ylab = "Residual")
plot(data.training$Effort, fit4.res, xlab = "Effort", ylab = "Residual")
plot(data.training$Length, fit4.res*sqrt(w2), xlab = "Length", ylab = "Weighted residual")
plot(data.training$Effort, fit4.res*sqrt(w2), xlab = "Effort", ylab = "Weigthed residual")
# Has a very good effect on heteroscedasticity



#------------------#
# Model validation #
#------------------#

MSE <- summary(fit4)$sigma^2
MSEP <- mean((predict(fit4, newdata = data.test) - data.test$Length)^2)
MSE
MSEP


####################################################
#   Model with interaction term (without effort)   #
####################################################

# When deleting Effort, Natural is not significant anymore --> delete natural as well

fit5 <- lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy ,data=data.training)
summary(fit5)
# The interaction term is significant and the adjusted R^2 is 0.9308

# Model assumptions
fit5.res <- residuals(fit5)
fit5.stdres <- stdres(fit5)
fit5.fittedvalues <- fitted.values(fit5)
par(mfrow = c(2,2))
qqnorm(fit5.stdres, main="")
qqline(fit5.stdres)
plot(fit5.res, xlab = "Index", ylab = "Residual")
plot(fit5.fittedvalues, fit5.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(fit5.res ~ fit5.fittedvalues), col = "red")
plot(fit5.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

#UL: normality issue from the previous model is solved
#LL: not much of a funnel shape 
#LR: one outlier




#------------------#
# Model validation #
#------------------#

MSE <- summary(fit5)$sigma^2
MSEP <- mean((predict(fit5, newdata = data.test) - data.test$Length)^2)
MSE
MSEP

