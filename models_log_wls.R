# Linear Models: Assignment
### code for log- and WLS models
### final models: m4 (log-model) & m5 (WLS model)

rm(list = ls())
# setwd("C:/Data/Emma/KU Leuven/Year 1/Semester 1/Linear models/Assignment")
# data.full <- read.table("dataset.txt", header = TRUE)
data.full <- read.csv("~/,School/Master/Linear Models/Exam Assignment/dataset.txt",
         sep="")
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
# remedy for heteroscedasticity - option 1: Box-Cox transformation 
par(mfrow = c(1,1))
out <- boxcox(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data=data.training, lambda = seq(-0.5, 0.5, 0.001), plotit=TRUE) # m1
lambda <- out$x[which(out$y == max(out$y))] # lambda = 0.004 -> lambda = 0

m2 <- lm(log(Length) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), data = data.training)
summary(m1)
summary(m2)


########
# remedy for heteroscedasticity - option 2: weighted least squares
m1.stdres <- stdres(m1)
w <- 1/lm(abs(m1.stdres) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest))$fitted.values^2
m3 <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest), weight = w, data = data.training)
summary(m1)
summary(m3)


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


######## 
# final models
m4 <- lm(log(Length) ~ Sex + Canopy + Shrub + Natural, data = data.training)
summary(m4)

# Plot of the weighted residuals versus fitted values
plot(m4$fitted.values, resid(m4)*sqrt(w))

m5 <- lm(Length ~ Sex + Canopy + Shrub  + Natural, weight = w, data = data.training)
summary(m5)

