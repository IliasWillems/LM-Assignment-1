library(MASS)
library(robustbase)
library(glmnet)
# loading data, spliting into training and test sets
setwd(choose.dir())
data.full = read.table("dataset.txt",header=T)
set.seed(0199510)
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]
n <- dim(data.training)[1]
p <- dim(data.training)[2]

attach(data.training)
####################################
# DESCRIPTIVE STATISTICS AND PLOTS #
####################################
# Descriptive part
sum(is.na(data.full)) # no missing values
head(data.full)
dim(data.full) # there are 320 observations, 8 variables 
summary(data.full)

### Plots ###
pairs(data.training[-c(1,4,7,8)], panel = function(x,y) {points(x,y); lines(lowess(x,y), col = "red")}) # Figure 1
# Linear tendency is the strongest between Length and Canopy, Length and Shrub

### Boxplots ###
par(mfrow=c(1,3))
boxplot(data.training$Length~data.training$Sex, xlab="Sex", ylab = "Length")
boxplot(data.training$Length~data.training$Forest, xlab="Forest", ylab="Length")
boxplot(data.training$Length~data.training$Natural, xlab="Natural",ylab="Length") # Figure 2
# Boxplots of Sex reveals differences in body length between males and females. Females tend to be bigger
# Variety within each sex seem to be similar for both females and males
# There are no differences in body length between each forest 
 
# Further investigation of the influence of Natural, Forest and Sex on the variables Canopy, Shrub and Size 
# Natural
plot(data.training$Canopy[which(data.training$Natural == "yes")], data.training$Length[which(data.training$Natural == "yes")], xlab = 'Canopy', ylab = 'Length', col = "black")
points(data.training$Canopy[which(data.training$Natural == "no")], data.training$Length[which(data.training$Natural == "no")], col = 'blue')
legend("topright", c("Intact", "human activity"), pch = 1, col = c("black", "blue"))

plot(data.training$Shrub[which(data.training$Natural == "yes")], data.training$Length[which(data.training$Natural == "yes")], xlab = 'Shrub', ylab = 'Length', col = "black")
points(data.training$Shrub[which(data.training$Natural == "no")], data.training$Length[which(data.training$Natural == "no")], col = 'blue')
legend("topright", c("Intact", "human activity"), pch = 1, col = c("black", "blue"))

plot(data.training$Size[which(data.training$Natural == "yes")], data.training$Length[which(data.training$Natural == "yes")], xlab = 'Size', ylab = 'Length', col = "black")
points(data.training$Size[which(data.training$Natural == "no")], data.training$Length[which(data.training$Natural == "no")], col = 'blue')
legend("topright", c("Intact", "human activity"), pch = 1, col = c("black", "blue"))
# Dots of both colors are spreaded randomly and equally across the data

# Forest
plot(data.training$Canopy[which(data.training$Forest == "Chawia")], data.training$Length[which(data.training$Forest == "Chawia")], xlab = 'Canopy', ylab = 'Length', col = "black")
points(data.training$Canopy[which(data.training$Forest== "Ngangao S")], data.training$Length[which(data.training$Forest == "Ngangao S")], col = 'blue')
points(data.training$Canopy[which(data.training$Forest== "Ngangao N")], data.training$Length[which(data.training$Forest == "Ngangao N")], col = 'green')
legend("topright", c("Chawia", "Ngangao S", "Ngangao N"), pch = 1, col = c("black", "blue","green"))

plot(data.training$Shrub[which(data.training$Forest == "Chawia")], data.training$Length[which(data.training$Forest == "Chawia")], xlab = 'Shrub', ylab = 'Length', col = "black")
points(data.training$Shrub[which(data.training$Forest== "Ngangao S")], data.training$Length[which(data.training$Forest == "Ngangao S")], col = 'blue')
points(data.training$Shrub[which(data.training$Forest== "Ngangao N")], data.training$Length[which(data.training$Forest == "Ngangao N")], col = 'green')
legend("topright", c("Chawia", "Ngangao S", "Ngangao N"), pch = 1, col = c("black", "blue","green"))

plot(data.training$Size[which(data.training$Forest == "Chawia")], data.training$Length[which(data.training$Forest == "Chawia")], xlab = 'Size', ylab = 'Length', col = "black")
points(data.training$Size[which(data.training$Forest== "Ngangao S")], data.training$Length[which(data.training$Forest == "Ngangao S")], col = 'blue')
points(data.training$Size[which(data.training$Forest== "Ngangao N")], data.training$Length[which(data.training$Forest == "Ngangao N")], col = 'green')
legend("topright", c("Chawia", "Ngangao S", "Ngangao N"), pch = 1, col = c("black", "blue","green"))
# All dots seems to be spreaded randomly and equally

####################
# MULTICOLINEARITY #
####################
data.cor <- cor(data.training[-c(1,4,6, 7,8)])
data.cor # Table 1
# VIF
data.vif <- diag(solve(data.cor[-4,-4]))
data.vif # Table 2
mean(data.vif)
# all values are close to 1
# mean of VIF values ~1

# Eigenvalues
data.eig <- eigen(data.cor[-4,-4])$values
data.eig # Table 2
# none of the eigenvalues is very small
# no multicollinearity problems


########################################################
#   Basic model with all variables  (without effort)   #
########################################################
full_model <- lm(Length~Sex+Canopy+Shrub+Size+Natural+Forest, data=data.training)

### Model assumptions
# residual diagnostics 
par(mfrow = c(2,2)) # Figure 3

# normality
full_model.stdres <- stdres(full_model)
qqnorm(full_model.stdres)
qqline(full_model.stdres)

# independence
plot(full_model$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(full_model.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# heteroscedasticity:
# fitted values 
plot(full_model$fitted.values, full_model$residuals, main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals")
lines(lowess(full_model$residuals ~ full_model$fitted.values), col = "red", lwd = 2)

# Shrub & Canopy 
par(mfrow = c(1,2)) # figure 4
plot(Shrub, full_model$residuals, 
     ylab = "Residuals", xlab = "Shrub") # slight shape: heteroscedasticity
lines(lowess(full_model$residuals ~ Shrub), col = "red", lwd = 2)
plot(Canopy, full_model$residuals,
     ylab = "Residuals", xlab = "Canopy") # slight shape: heteroscedasticity
lines(lowess(full_model$residuals ~ Canopy), col = "red", lwd = 2)

# other predictors
par(mfrow = c(2,2)) # Figure 5
plot(Size, full_model$residuals, main = "Size",
     xlab = "Size", ylab = "Residuals")
lines(lowess(full_model$residuals ~ Size), col = "red", lwd = 2)

d_male <- density(full_model$residuals[which(Sex == "male")])
d_female <- density(full_model$residuals[which(Sex == "female")])
plot(d_male, main = "Sex", lwd = 2, xlab = "Residuals")
lines(d_female, col = "red", lwd = 2, lty = 2)
legend("topright", c("male", "female"), col = c("black","red"), lty=c(1,2), lwd = 2)

d_no <- density(full_model$residuals[which(Natural == "yes")])
d_yes <- density(full_model$residuals[which(Natural == "no")])
plot(d_yes, main = "Natural", xlab = "Residuals", lwd= 2)
lines(d_no, col = "red", lwd = 2, lty = 2)
legend("topright", c("Yes", "No"), col = c("black","red"), lty=c(1,2), lwd = 2)

d_f1 <- density(full_model$residuals[which(Forest == "Ngangao N")])
d_f2 <- density(full_model$residuals[which(Forest == "Ngangao S")])
d_f3 <- density(full_model$residuals[which(Forest == "Chawia")])
plot(d_f1, main = "Forest", xlab = "Residuals", lwd = 2)
lines(d_f2, col = "red", lwd = 2, lty = 2)
lines(d_f3, col = 3, lwd = 2, lty = 3)
legend("topleft", c("N. North","N. South","Chawia"), 
       col = c(1,2,3), lty=c(1,2,3), lwd = 2)

### Variable selection

# Backward elimination
model_back <- stepAIC(full_model, list(upper~Sex+Canopy+Shrub+Size+Natural+Forest, lower~1), direction="back")
summary(model_back)
# Length ~ Sex + Canopy + Shrub

# Forward selection
model_initial <- lm(Length~1, data=data.training)
model_forw <- stepAIC(model_initial, list(upper~Sex+Canopy+Shrub+Size+Natural+Forest, lower~1), direction="forward")

# Stepwise regression
model_stepwise <- stepAIC(full_model, list(upper~Sex+Canopy+Shrub+Size+Natural+Forest, lower~1), direction="both")
summary(model_stepwise)
# Final model: Length ~ Sex + Canopy + Shrub
model_select <- lm(Length ~ Sex + Canopy + Shrub,data=data.training)

# Assumptions check on selected variables
# residual diagnostics 
par(mfrow = c(2,2))

# normality
model_select.stdres <- stdres(model_select)
qqnorm(model_select.stdres)
qqline(model_select.stdres)

# independence
plot(model_select$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(model_select.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# fitted values 
plot(model_select$fitted.values, model_select$residuals, main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals")
lines(lowess(model_select$residuals ~ model_select$fitted.values), col = "red", lwd = 2)

### Outliers detection for model obtained by variable selection
# Standardized residuals
model_select.stdres <- stdres(model_select)
plot(model_select.stdres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")
which(model_select.stdres>2.5)
which(model_select.stdres<(-2.5))
# 2 outliers

# Studentized residuals
model_select.studres <- studres(model_select)
plot(model_select.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")
which(model_select.studres>2.5)
which(model_select.studres<(-2.5))
# 2 outliers

# Diagonal elements of hat matrix
model_select.influence <- influence(model_select)
plot(model_select.influence$hat, ylab = "Diagonal elements of hat matrix")
abline(h = 2*p/n, col = "red")
# no outliers

# DFFITS
model_select.dffits <- dffits(model_select)
plot(model_select.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")
outliers <- which(model_select.dffits>2*sqrt(p/n))
# 5 outliers

##########################################
# Penalized Regression Analyssis - Lasso #
##########################################
beta_reg <- full_model$coefficients
lambdas = 10^seq(5, -5, by = -.1)
X = model.matrix(Length~Sex + Canopy + Shrub + Size + Natural + Forest, data.training)[,-1]
y = data.training$Length
### Lasso using standardized residuals (all variables, including categorical ones)
X.std <- model.matrix(Length ~ Sex + Canopy + Shrub + Size + Natural + Forest, data=data.training)
X.std[,2:8] <- scale(X.std[,2:8], center = TRUE, scale = TRUE)
y.std <- scale(data.training$Length, center = TRUE, scale = TRUE)
colnames(y.std) <- c("Length")

X.test.std <- model.matrix(Length ~Sex + Canopy + Shrub + Size + Natural + Forest, data=data.test)
X.test.std[,2:8] <- scale(X.test.std[,2:8], center = TRUE, scale = TRUE)
y.test.std <- scale(data.test[,5], center = TRUE, scale = TRUE)
colnames(y.test.std) <- c("Length")

# Regular regression model. Don't estimate the intercept as it is zero (standardized variables).
regular_regression_std <- lm(Length ~ -1+Sex + Canopy + Shrub + Size + Natural + Forest, data=as.data.frame(cbind(y.std, X.std[,-1])))
summary(regular_regression_std)
beta_reg_std <- regular_regression_std$coefficients

lambdas = 10^seq(5, -5, by = -.1)
X = model.matrix(Length~Sex + Canopy + Shrub + Size + Natural + Forest, data.training)[,-1]
y = data.training$Length

cv_fit_std = cv.glmnet(X, y, alpha = 1, lambda = lambdas, intercept = TRUE, standardize = TRUE)
lambda_best_std = cv_fit_std$lambda.min

lasso_model_std <- glmnet(X, y, alpha = 1, lambda = lambda_best_std, 
                          intercept = TRUE, standardize = TRUE)
beta_las_std <- coef(lasso_model_std)

# Show both regression coefficients side-by-side
df.std <- data.frame(rbind(as.matrix(beta_reg_std)),as.matrix(beta_las_std))
colnames(df.std) <- c("OLS std", "Lasso std")
df.std

# test the models on validation set
Xtest = model.matrix(Length~Sex + Canopy + Shrub + Size + Natural + Forest, data.test)[,-1]
ytest = data.test$Length
pred.las <- predict(lasso_model_std, newx=Xtest)

df.pred <- data.frame(pred.las, ytest)
colnames(df.pred) <- c("pred.las", "ytest")
df.pred
mse.las <- (1/dim(data.test)[1])*sum((ytest-pred.las)^2)
mse.las

####################################################
#   Model with interaction term (without effort)   #
####################################################

model_int_full <- lm(Length~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy,data=data.training)
model_int_full

### Residual diagnostics 
par(mfrow=c(2,2))  # Figure 7

model_int_full.res <- residuals(model_int_full)
model_int_full.stdres <- stdres(model_int_full)
model_int_full.fittedvalues <- fitted.values(model_int_full)

# Normality
qqnorm(model_int_full.stdres, main="Normal Q-Q plot")
qqline(model_int_full.stdres)

# plots residuals against index
plot(model_int_full.res, xlab = "Index", ylab = "Residual",main="Residuals")
plot(model_int_full.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3),main="Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# plot residual against fitted values
plot(model_int_full.fittedvalues, model_int_full.res, xlab = "Fitted value", ylab = "Residual", main="M1: Fitted values vs residuals")
lines(lowess(model_int_full.res ~ model_int_full.fittedvalues), col = "red")

# Shrub & Canopy 
par(mfrow = c(1,2))   # Figure 6
plot(data.training$Canopy, model_int_full.res, ylab = "Residual",xlab="Canopy")
lines(lowess(model_int_full$residuals ~ data.training$Canopy), col = "red")
plot(data.training$Shrub, model_int_full.res, ylab = "Residual",xlab="Shrub")
lines(lowess(model_int_full$residuals ~ data.training$Shrub), col = "red")

### Variable Selection

# Backward elimination based on AIC (no stepwise)
stepAIC(model_int_full, scope = list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "backward")
# Length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural

# Forward selection based on AIC
model_int_null <- lm(Length ~ 1, data = data.training)
model_int_null
stepAIC(model_int_null, scope = list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "forward")
# length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural 

# Stepwise selection based on AIC (started at full model)
stepAIC(model_int_null, scope=list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "both")
# Length ~ Sex+Canopy+Shrub+Canopy:Shrub + Natural 

# Stepwise selection based on AIC (started at null model)
stepAIC(model_int_null, scope=list(upper = ~ Sex+Canopy+Shrub+Size+Natural+Forest+Shrub*Canopy, lower = ~ 1), direction = "both")
# Length ~Sex+Canopy+Shrub+Canopy:Shrub + Natural 

model_int_fin <- lm(Length ~ Sex+Canopy+Shrub+Canopy*Shrub+Natural,data=data.training)
summary(model_int_fin)
# Natural is not significant --> delete

model_int <- lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy,data=data.training)
summary(model_int)

### Model assumptions
# Residual diagnostics 
par(mfrow=c(2,2))
model_int.res <- residuals(model_int)
model_int.stdres <- stdres(model_int)
model_int.fittedvalues <- fitted.values(model_int)

# Normality
qqnorm(model_int.stdres, main="")
qqline(model_int.stdres)

# Plots residuals against index
plot(model_int.res, xlab = "Index", ylab = "Residual")
plot(model_int.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

# Plot residual against fitted values
plot(model_int.fittedvalues, model_int.res, xlab = "Fitted value", ylab = "Residual")
lines(lowess(model_int.res ~ model_int.fittedvalues), col = "red")


# Shrub & Canopy   
par(mfrow = c(1,2))
plot(data.training$Canopy, model_int.res, ylab = "Residual",xlab="Canopy")
lines(lowess(model_int$residuals ~ data.training$Canopy), col = "red")
plot(data.training$Shrub, model_int.res, ylab = "Residual",xlab="Shrub")
lines(lowess(model_int$residuals ~ data.training$Shrub), col = "red")


### Investigate outliers
par(mfrow=c(1,2)) # Figure 11

# DFFITS
model_int.dffits <- dffits(model_int)
plot(model_int.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")
abline(h = -2*sqrt(p/n), col = "red")
#3 outliers: 77,87,108

model_int.dffits[77]
model_int.dffits[87]
model_int.dffits[108]

# Studentized residuals
model_int.studres <- studres(model_int)
plot(model_int.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")

# Diagonal elements of hat matrix - Figure 11
model_int.influence <- influence(model_int)$hat
distance<-sqrt((model_int.influence-1/n)*(n-1))
plot(distance, model_int.studres, ylim = c(-5,5),xlim=c(0,4.5), xlab = "Mahalanobis distances", ylab = "Studentized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")
# 2 outliers 87,108

# DFBETAS
par(mfrow=c(2,2)) # Figure D1 (appendix)
model_int.dfbetas <- dfbetas(model_int)
model_int.dfbetas
2/sqrt(n)
plot(model_int.dfbetas[,2], main="DFBETAS Sex")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)

plot(model_int.dfbetas[,3], main="DFBETAS Canopy")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)

plot(model_int.dfbetas[,4], main="DFBETAS Shrub")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)

plot(model_int.dfbetas[,5], main="DFBETAS Canopy:Shrub")
abline(h = 2/sqrt(n), lty = 2)
abline(h = -2/sqrt(n), lty = 2)
# Outlier 77,87,108

###############################
# MODELS WITH TRANSFORMATIONS #
###############################

# remedy for heteroscedasticity - option 1: Box-Cox transformation

par(mfrow = c(1,1)) # Figure 8
out <- boxcox(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data=data.training, lambda = seq(-0.5, 0.5, 0.001), plotit=TRUE)
lambda <- out$x[which(out$y == max(out$y))] # lambda = 0.004 -> lambda = 0

model_box <- lm(log(Length) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data = data.training)
summary(full_model)
summary(model_box)

# remedy for heteroscedasticity - option 2: weighted least squares
w <- 1/lm(abs(full_model.stdres) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data=data.training)$fitted.values^2
model_wls <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), weight = w, data = data.training)
summary(full_model)
summary(model_wls)

### Re-check model assumptions

# Box-Cox model (model_box)
par(mfrow = c(2,2)) # Figure 9
model_box.stdres <- stdres(model_box)
qqnorm(model_box.stdres)
qqline(model_box.stdres)

plot(model_box$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(model_box.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

plot(model_box$fitted.values, model_box$residuals, main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved
lines(lowess(model_box$residuals ~ model_box$fitted.values), col = "red", lwd = 2)

### WLS model (model_wls)
par(mfrow = c(2,2)) # Figure 10
model_wls.stdres <- stdres(model_wls)
qqnorm(model_wls.stdres)
qqline(model_wls.stdres)

plot(model_wls$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(model_wls.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

plot(model_wls$fitted.values, model_wls$residuals*sqrt(w), main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved but not as much
lines(lowess(model_wls$residuals*sqrt(w) ~ model_wls$fitted.values), col = "red", lwd = 2)

### Variable selection

# Box-Cox model (model_box)
# backward selection 
model_box.full <- lm(log(Length) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data = data.training)
stepAIC(model_box.full, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "backward")

# forward selection 
model_box.null <- lm(log(Length) ~ 1, data = data.training)
stepAIC(model_box.null, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "forward")

# stepwise selection (started at full model)
stepAIC(model_box.full, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")

# stepwise selection (started at null model)
stepAIC(model_box.null, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")
# -> in all cases: Sex, Shrub, Canopy, Natural

model_box_fin <- lm(log(Length) ~ Sex + Canopy + Shrub + Natural, data = data.training)
summary(model_box_fin)

# WLS model (model_wls)
# backward selection
model_wls.full <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), weight = w, data = data.training)
stepAIC(model_wls.full, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "backward")

# forward selection 
model_wls.null <- lm(Length ~ 1, weight = w, data = data.training)
stepAIC(model_wls.null, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "forward")

# stepwise selection (started at full model)
stepAIC(model_wls.full, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")

# stepwise selection (started at null model)
stepAIC(model_wls.null, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")
# -> in all cases: Sex, Shrub, Canopy, Natural

model_wls_fin <- lm(Length ~ Sex + Canopy + Shrub  + Natural, weight = w, data = data.training)
summary(model_wls_fin)

### Final models - re-check of assumptions
# Box-Cox final (model_box_fin)
par(mfrow = c(2,2)) # Figure C3
model_box_fin.stdres <- stdres(model_box_fin)
qqnorm(model_box_fin.stdres, main="")
qqline(model_box_fin.stdres)

plot(model_box_fin$residuals, xlab = "Index", ylab = "Residual")
plot(model_box_fin.stdres, xlab = "Index", ylab = "Standardized residual", ylim = c(-3,3))
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

plot(model_box_fin$fitted.values, model_box_fin$residuals, main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved
lines(lowess(model_box_fin$residuals ~ model_box_fin$fitted.values), col = "red", lwd = 2)


# Weighted Least Squares final (model_wls_fin)
par(mfrow = c(2,2)) # figure C4
model_wls_fin.stdres <- stdres(model_wls_fin)
qqnorm(model_wls_fin.stdres)
qqline(model_wls_fin.stdres)

plot(model_wls_fin$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(model_wls_fin.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

plot(model_wls_fin$fitted.values, model_wls_fin$residuals*sqrt(w), main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved but not as much
lines(lowess(model_wls_fin$residuals*sqrt(w) ~ model_wls_fin$fitted.values), col = "red", lwd = 2)

### Outliers detection - Box-Cox (model_box_fin)
# DFFITS
par(mfrow=c(1,1))
model_box_fin.dffits <- dffits(model_box_fin)
plot(model_box_fin.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")
abline(h = -2*sqrt(p/n), col = "red")
# 5 outliers

# Studentized residuals
model_box_fin.studres <- studres(model_box_fin)
plot(model_box_fin.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")
# 2 outliers

# Diagonal elements of hat matrix
model_box_fin.influence <- influence(model_box_fin)$hat
distance<-sqrt((model_box_fin.influence-1/n)*(n-1))
plot(distance, model_box_fin.studres, ylim = c(-5,5),xlim=c(0,4.5), xlab = "Mahalanobis distances", ylab = "Studentized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")
# 2 outliers

### Outliers detection - WLS (model_wls_fin)
# DFFITS
model_wls_fin.dffits <- dffits(model_wls_fin)
plot(model_wls_fin.dffits, ylab = "DFFITS")
abline(h = 2*sqrt(p/n), col = "red")
abline(h = -2*sqrt(p/n), col = "red")
# 5 outliers

# Studentized residuals
model_wls_fin.studres <- studres(model_wls_fin)
plot(model_wls_fin.studres, ylim = c(-4,4), ylab = "Studentized residuals")
abline(h = c(-2.5,2.5), col = "red")
# 2 outliers

# Diagonal elements of hat matrix
model_wls_fin.influence <- influence(model_wls_fin)$hat
distance<-sqrt((model_wls_fin.influence-1/n)*(n-1))
plot(distance, model_wls_fin.studres, ylim = c(-5,5),xlim=c(0,4.5), xlab = "Mahalanobis distances", ylab = "Studentized residuals")
abline(v = sqrt(qchisq(0.975, p - 1)), col = "red")
abline(h = c(-2.5,2.5), col = "red")
# 2 outliers


################################################################################
#                    Asses validity/stability of models                        #
################################################################################

# Refit on validation set
full_model_val <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data = data.test)
model_int_val <- lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy,data=data.test)
model_box_val <- lm(log(Length) ~ Sex + Canopy + Shrub + Natural, data = data.test)
full_model_val.stdres <- stdres(full_model_val)
w_val <- 1/lm(abs(full_model_val.stdres) ~ Sex + Canopy + Shrub + Size + Natural +
                I(Forest), data = data.test)$fitted.values^2
model_wls_val <- lm(Length ~ Sex + Canopy + Shrub  + Natural, weight = w_val, data = data.test)

# Table 5
round(data.frame(full_model$coefficients, full_model_val$coefficients), 3)
round(data.frame(model_int$coefficients, model_int_val$coefficients), 3)
round(data.frame(model_box_fin$coefficients, model_box_val$coefficients), 3)
round(data.frame(model_wls_fin$coefficients, model_wls_val$coefficients), 3)
# No significant differences are detected among the pairs of coefficients

################################################################################
#                               Compare the models                             #
################################################################################

# Prediction accuracy: MSEP of test data based on training data
baseline.pred <- predict(full_model, newdata = data.test)
inter.pred <- predict(model_int, newdata = data.test)
log.pred <- predict(model_box_fin, newdata = data.test)
WLS.pred <- predict(model_wls_fin, newdata = data.test)

baseline_MSEP <- mean((baseline.pred - data.test$Length)^2)
inter_MSEP <- mean((inter.pred - data.test$Length)^2)
log_MSEP <- mean((exp(log.pred) - data.test$Length)^2)
WLS_MSEP <- mean((WLS.pred - data.test$Length)^2)

MSEP.df <- data.frame(baseline_MSEP, inter_MSEP, log_MSEP, WLS_MSEP)
colnames(MSEP.df) <- c("OLS", "Interaction", "Log trans", "WLS")
rownames(MSEP.df) <- c("MSEP")
MSEP.df
# We see that the MSEPs are all relatively comparable, though from the above 
# results, the model with interaction seems to be the best one

# Prediction accuracy: MSEP of training data based on test data. That is,
# predict the training data based on the models obtained with the validation
# data.
baseline.pred.val <- predict(full_model_val, newdata = data.training)
inter.pred.val <- predict(model_int_val, newdata = data.training)
log.pred.val <- predict(model_box_val, newdata = data.training)
WLS.pred.val <- predict(model_wls_val, newdata = data.training)

baseline_MSEP_val <- mean((baseline.pred.val - data.training$Length)^2)
inter_MSEP_val <- mean((inter.pred.val - data.training$Length)^2)
log_MSEP_val <- mean((exp(log.pred.val) - data.training$Length)^2)
WLS_MSEP_val <- mean((WLS.pred.val - data.training$Length)^2)

MSEP.val.df <- data.frame(baseline_MSEP_val, inter_MSEP_val, log_MSEP_val,
                          WLS_MSEP_val)
colnames(MSEP.val.df) <- c("OLS", "Interaction", "Log trans", "WLS")
rownames(MSEP.val.df) <- c("MSEP")
MSEP.val.df
# Prediction accuracy: PRESS criterion on full data set (jackknifing)
frogs <- data.full
frogs <- frogs[,-4]
SS.del.res = c(rep(0, 4))
for (i in 1:dim(frogs)[1]) {
  
  # Determine training and test set
  train.set = frogs[-i,]
  test.set = frogs[i,]
  
  # Estimate the models
  m_baseline_jk <- lm(Length~ Sex + Canopy + Shrub + Size + Natural + I(Forest),
                      data = train.set)
  
  m_inter_jk <- lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy,data=train.set)
  
  m_log_jk <- lm(log(Length) ~ Sex + Canopy + Shrub + Natural, data = train.set)
  
  m1.stdres.jk <- stdres(m_baseline_jk)
  w.jk <- 1/lm(abs(m1.stdres.jk) ~ Sex + Canopy + Shrub + Size + Natural +
                 I(Forest), data = train.set)$fitted.values^2
  m_WLS_jk <- lm(Length ~ Sex + Canopy + Shrub  + Natural, weight = w.jk,
                 data = train.set)
  
  # Compute deleted residuals
  baseline.dres <- test.set$Length - predict(m_baseline_jk, newdata = test.set)
  inter.dres <- test.set$Length - predict(m_inter_jk, newdata = test.set)
  log.dres <- test.set$Length - exp(predict(m_log_jk, newdata = test.set))
  WLS.dres <- test.set$Length - predict(m_WLS_jk, newdata = test.set)
  
  # Add to sum of squared deleted residuals
  SS.del.res = SS.del.res + c(baseline.dres^2, inter.dres^2,
                              log.dres^2, WLS.dres^2)
}
PRESS <- as.data.frame(t(SS.del.res/dim(frogs)[1]))
colnames(PRESS) <- c("OLS", "Interaction", "Log trans", "WLS")
rownames(PRESS) <- c("PRESS")

rbind(MSEP.df,MSEP.val.df,PRESS) # Table 6
