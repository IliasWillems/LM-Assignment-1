# loading training and test data 

data.full = read.table("dataset.txt",header=T)
set.seed(0199510)
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]
n <- dim(data.training)[1]
p <- dim(data.training)[2]


library(MASS)

#########################################################
#   Full model with interaction term (without effort)   #
#########################################################

fit.full <- lm(Length~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy,data=data.training)
fit.full

# Residual diagnostics 
par(mfrow=c(2,2))  #figure 6

fitfull.res <- residuals(fit.full)
fitfull.stdres <- stdres(fit.full)
fitfull.fittedvalues <- fitted.values(fit.full)

# Normality
qqnorm(fitfull.stdres, main="Normal Q-Q plot")
qqline(fitfull.stdres)

# plots residuals against index
plot(fitfull.res, xlab = "Index", ylab = "Residual",main="Residuals")
plot(fitfull.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3),main="Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# plot residual against fitted values
plot(fitfull.fittedvalues, fitfull.res, xlab = "Fitted value", ylab = "Residual", main="M1: Fitted values vs residuals")
lines(lowess(fitfull.res ~ fitfull.fittedvalues), col = "red")

# Shrub & Canopy 
par(mfrow = c(1,2))   #figure 5
plot(data.training$Canopy, fitfull.res, ylab = "Residual",xlab="Canopy")
lines(lowess(fit.full$residuals ~ data.training$Canopy), col = "red")
plot(data.training$Shrub, fitfull.res, ylab = "Residual",xlab="Shrub")
lines(lowess(fit.full$residuals ~ data.training$Shrub), col = "red")


#modelselection

# Backward elimination based on AIC (no stepwise)

stepAIC(fit.full, scope = list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "backward")
# length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural

# Forward selection based on AIC
fit.null <- lm(Length ~ 1, data = data.training)
fit.null
stepAIC(fit.null, scope = list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "forward")
# length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural 

# Stepwise selection based on AIC (started at full model)
stepAIC(fit.full, scope=list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "both")
# length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural 

# Stepwise selection based on AIC (started at null model)
stepAIC(fit.null, scope=list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "both")
# length ~Sex+Canopy+Shrub+Canopy:Shrub + Natural 


# Length ~ Sex+Canopy+Shrub+Canopy*Shrub+Natural



########################################
# Interaction model after selection    #
########################################

fit <- lm(Length ~ Sex+Canopy+Shrub+Canopy*Shrub+Natural,data=data.training)
summary(fit)
# Natural is not significant (p=0) --> delete
fit5 <- lm(Length ~ Sex+Canopy+Shrub+Canopy*Shrub,data=data.training)
summary(fit5)

# Residual diagnostics 
par(mfrow=c(2,2))  # figure C2 (appendix)

fit5.res <- residuals(fit5)
fit5.stdres <- stdres(fit5)
fit5.fittedvalues <- fitted.values(fit5)

# Normality
qqnorm(fit5.stdres, main="Normal Q-Q plot")
qqline(fit5.stdres)

# plots residuals against index
plot(fit5.res, xlab = "Index", ylab = "Residual",main="Residuals")
plot(fit5.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3),main="Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# plot residual against fitted values
plot(fit5.fittedvalues, fit5.res, xlab = "Fitted value", ylab = "Residual", main="M1: Fitted values vs residuals")
lines(lowess(fit5.res ~ fit5.fittedvalues), col = "red")

# Shrub & Canopy   
par(mfrow = c(1,2))    # figure C1 (appendix)
plot(data.training$Canopy, fit5.res, ylab = "Residual",xlab="Canopy")
lines(lowess(fit5$residuals ~ data.training$Canopy), col = "red")
plot(data.training$Shrub, fit5.res, ylab = "Residual",xlab="Shrub")
lines(lowess(fit5$residuals ~ data.training$Shrub), col = "red")


#Investigate outliers
par(mfrow=c(1,2))     #figure D2 (appendix)
# DFFITS
fit5.dffits <- dffits(fit5)
plot(fit5.dffits, ylab = "DFFITS", main="DFFITS")
abline(h = 2*sqrt(p/n), col = "red")
abline(h = -2*sqrt(p/n), col = "red")
#3 outliers: 77,87,108

fit5.dffits[77]
fit5.dffits[87]
fit5.dffits[108]

# Studentized residuals

fit5.studres <- studres(fit5)
plot(fit5.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagonal elements of hat matrix
fit5.influence <- influence(fit5)$hat
distance<-sqrt((fit5.influence-1/n)*(n-1))
plot(distance, fit5.studres, ylim = c(-5,5),xlim=c(0,4.5), xlab = "Mahalanobis distances", ylab = "Studentized residuals", main="Diagnostic plot")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")
# 2 outliers 87,108



# DFBETAS
par(mfrow=c(2,2))    #figure D1 (appendix)
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
# Outlier 77,87,108




#------------------#
# Model validation #
#------------------#

# Model 1: Length ~ Sex+Canopy+Shrub+Canopy*Shrub
fit5.val <- lm(Length ~ Sex+Canopy+Shrub+Canopy*Shrub, data = data.test)
summary(fit5.val)
summary(fit5)

MSE <- summary(fit5)$sigma^2
MSEP <- mean((predict(fit5, newdata = data.test) - data.test$Length)^2)
MSE
MSEP



