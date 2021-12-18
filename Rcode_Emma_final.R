# Linear Models: Assignment

rm(list = ls())
setwd("C:/Data/Emma/KU Leuven/Year 1/Semester 1/Linear models/Assignment")
data.full <- read.table("dataset.txt", header = TRUE)
library(MASS)


########
# split data for cross-validation
set.seed(0199510)
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ]
attach(data.training)



########
# full linear model (without effort)
m1 <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data = data.training)
summary(m1)



########
# residual diagnostics 
par(mfrow = c(2,2)) # figure 3

### normality
m1.stdres <- stdres(m1)
qqnorm(m1.stdres)
qqline(m1.stdres)
# -> normality looks fine

### independence
plot(m1$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(m1.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
  abline(h = -2.5, lty = 2)
  abline(h = 2.5, lty = 2)
# -> independence looks fine

### heteroscedasticity:
# fitted values 
plot(m1$fitted.values, m1$residuals, main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals")
  lines(lowess(m1$residuals ~ m1$fitted.values), col = "red", lwd = 2)
 
# Shrub & Canopy 
par(mfrow = c(1,2)) # figure 4
plot(Shrub, m1$residuals, 
     ylab = "Residuals", xlab = "Shrub") # slight shape: heteroscedasticity
  lines(lowess(m1$residuals ~ Shrub), col = "red", lwd = 2)
plot(Canopy, m1$residuals,
     ylab = "Residuals", xlab = "Canopy") # slight shape: heteroscedasticity
  lines(lowess(m1$residuals ~ Canopy), col = "red", lwd = 2)

# other predictors -> appendix
par(mfrow = c(2,2)) # figure A1
plot(Size, m1$residuals, main = "Size",
     xlab = "Size", ylab = "Residuals")
  lines(lowess(m1$residuals ~ Size), col = "red", lwd = 2)

d_male <- density(m1$residuals[which(Sex == "male")])
d_female <- density(m1$residuals[which(Sex == "female")])
plot(d_male, main = "Sex", lwd = 2, xlab = "Residuals")
  lines(d_female, col = "red", lwd = 2, lty = 2)
  legend("topright", c("male", "female"), col = c("black","red"), lty=c(1,2), lwd = 2)

d_no <- density(m1$residuals[which(Natural == "yes")])
d_yes <- density(m1$residuals[which(Natural == "no")])
plot(d_yes, main = "Natural", xlab = "Residuals", lwd= 2)
  lines(d_no, col = "red", lwd = 2, lty = 2)
  legend("topright", c("Yes", "No"), col = c("black","red"), lty=c(1,2), lwd = 2)

d_f1 <- density(m1$residuals[which(Forest == "Ngangao N")])
d_f2 <- density(m1$residuals[which(Forest == "Ngangao S")])
d_f3 <- density(m1$residuals[which(Forest == "Chawia")])
plot(d_f1, main = "Forest", xlab = "Residuals", lwd = 2)
  lines(d_f2, col = "red", lwd = 2, lty = 2)
  lines(d_f3, col = 3, lwd = 2, lty = 3)
  legend("topleft", c("N. North","N. South","Chawia"), 
         col = c(1,2,3), lty=c(1,2,3), lwd = 2)



########
# remedy for heteroscedasticity - option 1: Box-Cox transformation 
par(mfrow = c(1,1)) # figure 7
out <- boxcox(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data=data.training, lambda = seq(-0.5, 0.5, 0.001), plotit=TRUE) # m1
lambda <- out$x[which(out$y == max(out$y))] # lambda = 0.004 -> lambda = 0

m2 <- lm(log(Length) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data = data.training)
summary(m1)
summary(m2)



########
# remedy for heteroscedasticity - option 2: weighted least squares
w <- 1/lm(abs(m1.stdres) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest))$fitted.values^2
m3 <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), weight = w, data = data.training)
summary(m1)
summary(m3)
# -> coefficients similar, natural now significant



########
# re-check model assumptions

### Box-Cox model (m2)
par(mfrow = c(2,2)) # figure 8
m2.stdres <- stdres(m2)
qqnorm(m2.stdres)
qqline(m2.stdres)

plot(m2$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(m2.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
  abline(h = -2.5, lty = 2)
  abline(h = 2.5, lty = 2)
  
plot(m2$fitted.values, m2$residuals, main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved
  lines(lowess(m2$residuals ~ m2$fitted.values), col = "red", lwd = 2)

### WLS model (m3)
par(mfrow = c(2,2)) # figure 9
m3.stdres <- stdres(m3)
qqnorm(m3.stdres)
qqline(m3.stdres)

plot(m3$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(m3.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
  abline(h = -2.5, lty = 2)
  abline(h = 2.5, lty = 2)
  
plot(m3$fitted.values, m3$residuals*sqrt(w), main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved but not as much
  lines(lowess(m3$residuals*sqrt(w) ~ m3$fitted.values), col = "red", lwd = 2)



######## 
# variable selection 
### log-model (m2)
  ### backward selection 
  m.full <- lm(log(Length) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data = data.training)
  stepAIC(m.full, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "backward")
  
  ### forward selection 
  m.null <- lm(log(Length) ~ 1, data = data.training)
  stepAIC(m.null, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "forward")
  
  ### stepwise selection (started at full model)
  stepAIC(m.full, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")
  
  ### stepwise selection (started at null model)
  stepAIC(m.null, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")
# -> in all cases: Sex, Shrub, Canopy, Natural
  
m4 <- lm(log(Length) ~ Sex + Canopy + Shrub + Natural, data = data.training)
summary(m4)
  
### WLS model (m3)
  ### backward selection
  m.full <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), weight = w, data = data.training)
  stepAIC(m.full, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "backward")
  
  ### forward selection 
  m.null <- lm(Length ~ 1, weight = w, data = data.training)
  stepAIC(m.null, scope = list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "forward")
  
  ### stepwise selection (started at full model)
  stepAIC(m.full, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")
  
  ### stepwise selection (started at null model)
  stepAIC(m.null, scope=list(upper = ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), lower = ~ 1), direction = "both")
# -> in all cases: Sex, Shrub, Canopy, Natural
  
m5 <- lm(Length ~ Sex + Canopy + Shrub + Natural, weight = w, data = data.training)
summary(m5)


  
######## 
# re-check model assumptions (similar to before variable selection) -> appendix

### Box-Cox model (m4)
par(mfrow = c(2,2)) # figure C3
m4.stdres <- stdres(m4)
qqnorm(m4.stdres)
qqline(m4.stdres)

plot(m4$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(m4.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

plot(m4$fitted.values, m4$residuals, main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved
lines(lowess(m4$residuals ~ m4$fitted.values), col = "red", lwd = 2)

### WLS model (m5)
par(mfrow = c(2,2)) # figure C4
m5.stdres <- stdres(m5)
qqnorm(m5.stdres)
qqline(m5.stdres)

plot(m5$residuals, main = "Independence (residuals)", ylab = "Residuals")
plot(m5.stdres, main = "Independence (std. residuals)", ylab = "Standardized residuals")
abline(h = -2.5, lty = 2)
abline(h = 2.5, lty = 2)

plot(m5$fitted.values, m5$residuals*sqrt(w), main = "Homoscedasticity", 
     xlab = "Fitted values", ylab = "Residuals") # heteroscedasticity improved but not as much
lines(lowess(m5$residuals*sqrt(w) ~ m5$fitted.values), col = "red", lwd = 2)



