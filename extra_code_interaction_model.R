#fitting interaction model (full model)

fit.full <- lm(Length~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy,data=data.training)
fit.full

# Model assumptions
par(mfrow=c(2,2))
fitfull.res <- residuals(fit.full)
fitfull.stdres <- stdres(fit.full)
fitfull.fittedvalues <- fitted.values(fit.full)
qqnorm(fitfull.stdres, main="Normal Q-Q plot")
qqline(fitfull.stdres)
plot(fitfull.res, xlab = "Index", ylab = "Residual",main="Residuals")
plot(fitfull.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3),main="Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)
plot(fitfull.fittedvalues, fitfull.res, xlab = "Fitted value", ylab = "Residual", main="M1: Fitted values vs residuals")
lines(lowess(fitfull.res ~ fitfull.fittedvalues), col = "red")

#modelselection

# Backward elimination based on AIC (no stepwise)
stepAIC(fit.full, scope = list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "backward")
# length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural (but Natural non significant hence delete)

# Forward selection based on AIC
fit.null <- lm(Length ~ 1, data = data.training)
fit.null
stepAIC(fit.null, scope = list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "forward")
# length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural (but Natural non significant hence delete)

# Stepwise selection based on AIC (started at full model)
stepAIC(fit.full, scope=list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "both")
# length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural (but Natural non significant hence delete)

# Stepwise selection based on AIC (started at null model)
stepAIC(fit.null, scope=list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "both")
# length ~Sex+Canopy+Shrub+Canopy:Shrub + Natural (but Natural non significant hence delete)

# Backward elimination based on F-statistic/t-statistic
# We can already see which term to drop by looking at the summary and than t-statistic (the least significant is removed)
summary(fit.full)
dropterm(fit.full, test = "F")
# Remove PForest
fit1 <- update(fit.full, ~ . - Forest)
summary(fit1)
dropterm(fit1, test = "F")
# Remove Size
fit2 <- update(fit1, ~ . - Size)
dropterm(fit2, test = "F")
# Remove Natural
fit3 <- update(fit2, ~ . - Natural)
dropterm(fit3, test = "F")
# Length ~ Sex+Canopy+Shrub+Canopy*Shrub

# Forward selection based on F-statistic/t-statistic
addterm(fit.null, ~ . + Sex + Canopy + Shrub+ Canopy*Shrub+ Size + Natural + Forest, test = "F")
# Add Sex
fit1 <- update(fit.null, ~ . + Sex)
addterm(fit1, ~ . + Canopy + Shrub+ Canopy*Shrub + Size + Natural + Forest, test = "F")
# Add Shrub
fit2 <- update(fit1, ~ . + Shrub)
addterm(fit2, ~. +  Canopy + Canopy*Shrub + Size + Natural + Forest, test = "F")
# Add Canopy (and interaction)
fit3 <- update(fit2, ~ . + Canopy + Canopy*Shrub)
addterm(fit2, ~. +Size + Natural + Forest, test = "F")
# Length ~ Sex+Canopy+Shrub+Canopy*Shrub
