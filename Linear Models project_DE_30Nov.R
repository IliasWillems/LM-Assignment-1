setwd("/Users/deryaengin/Desktop") #set the working directory
install.packages("sjstats")
install.packages("sjmisc")
install.packages("tidyverse")
install.packages("Metrics")
install.packages('leaps')
library(Metrics)
library(tidyverse)
library(caret)
library(dplyr)
library(corrplot)
library(HH)
library(sjstats)
library(caret)
library(sjmisc)
data.full = read.table("dataset.txt",header=T)
set.seed(0199510)
d.test <- sample(1:dim(data.full)[1], round(dim(data.full)[1]/2) )
data.test <- data.full[d.test, ]
data.training <- data.full[-d.test, ] #split the dataset
glimpse(data.training)
dat.cor=data.training[,c(2:6)]
M=cor(dat.cor)
corrplot(M, method=c("number"),add=FALSE)

# Linear Regression Assumptions -------------------------------------------
#Four assumptions needed to be satisfied
#1.Linear relationship between predictors and response
#2.Errors are independent
#3.Errors are normally distributed
#4.Errors have equal variances
#In order for our model to be adequate, we need to do model adequacy checking

# Model Building ----------------------------------------------------------
model1=lm(Length~., data=data.training)
summary(model1)
model_no_effort=lm(Length~Sex+Canopy+Shrub+Size+Natural+Forest, data=data.training) # no Effort variable
summary(model_no_effort)
par(mfrow = c(2, 2))
plot(model_no_effort)
#The Residuals vs Fitted plot is used to chack linearity (1st assumption)
#Ideally, the residual plot will show no fitted pattern. 
#That is, the red line should be approximately horizontal at zero

#Normal Q-Q used to checks normality of residuals 
#Ideally, the residual should follow straight line.
#In our case residuals are far from the the right tail, so it is questionable

#Scale-Location plot is used to Check homogeneity of variance (4th assumption)
#This plot shows if residuals are spread equally along the ranges of predictors.
#It’s good if you see a horizontal line with equally spread points.
#In our example, that holds

#The plot highlights the top 3 most extreme points (#156, #68 and #187).
#However, there is no outliers that exceed 3 standard deviations, which is good.
model2=lm(Length~Sex+Canopy+Shrub, data=data.training)
summary(model2)
par(mfrow = c(2, 2))
plot(model2)
vif(model2) 

model4=lm(Length~Sex+Canopy+Shrub+Size+Natural+Forest, data=data.training) # no Effort variable
par(mfrow = c(2, 2))
plot(model4)
vif(model4) 

# Models With Interaction -------------------------------------------------
#Try models with interaction
model5=lm(Length~(Sex+Canopy+Shrub+Effort)^2, data=data.training)
summary(model5)
model6=lm(Length~(Sex+Canopy+Shrub+Effort+Natural+Forest)^2, data=data.training)
summary(model6)
model7=lm(Length~Sex+Canopy+Shrub+Canopy*Shrub+Canopy*Natural, data=data.training)
summary(model7)
model8=lm(Length~Sex+Canopy+Shrub+Canopy*Shrub, data=data.training)
summary(model8)
par(mfrow = c(2, 2))
plot(model8)

#The Residuals vs Fitted plot is used to chack linearity (1st assumption)
#The red line is approximately horizontal at zero.

#Normal Q-Q used to checks normality of residuals 
#In our case except for a few datapoints, data follows the normal line

#Scale-Location plot is used to Check homogeneity of variance (4th assumption)
#Residuals are spread equally, so homogeneity of variance holds

#There is no outliers that exceed 3 standard deviations, which is good.



# Lasso Variable Selection ------------------------------------------------
library(glmnet)
#define response variable
y <- data.training$Length

#define matrix of predictor variables
x <- data.matrix(data.training[, c('Sex', 'Canopy', 'Shrub', 'Effort','Size','Natural','Forest')])
#perform k-fold cross-validation to find optimal lambda value
cv_model <- cv.glmnet(x, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda <- cv_model$lambda.min
best_lambda
plot(cv_model) 
best_model <- glmnet(x, y, alpha = 1, lambda = best_lambda)
coef(best_model) #we see that the coefficients for Efoort, Size and Natural are almost equal 0
#this indicates that these variables are rather not useful for our prediction model 

x1 <- data.matrix(data.training[, c('Sex', 'Canopy', 'Shrub','Size','Natural','Forest')])
#perform k-fold cross-validation to find optimal lambda value
cv_model1 <- cv.glmnet(x1, y, alpha = 1)

#find optimal lambda value that minimizes test MSE
best_lambda1 <- cv_model1$lambda.min
best_lambda1
plot(cv_model1) 
best_model1 <- glmnet(x1, y, alpha = 1, lambda = best_lambda1)
coef(best_model1) #Size and Forest are close to 0 
#this indicates that these variables are rather not useful for our prediction model 


# ANOVA (reduced model, full model) ---------------------------------------

anova(model_no_effort,model1)
anova(model1,model4)
anova(model2,model8)#the interaction term has non-zero coefficient


# Leaps() -----------------------------------------------------------------

library(leaps)
par(mfrow=c(1,1))
# Leaps takes a design matrix as argument: throw away the intercept
# column or leaps will complain

X <- model.matrix(model1)[,-1]

# Look at R^2

# R^2 -- all subsets

r2.leaps <- leaps(X, data.training$Length, nbest=3, method='r2')
plot(r2.leaps$size, r2.leaps$r2, pch=23, bg='blue', cex=2)
best.model.r2 <- r2.leaps$which[which((r2.leaps$r2 == max(r2.leaps$r2))),]
print(best.model.r2)


# Adjusted R^2 -- all subsets

adjr2.leaps <- leaps(X, data.training$Length, nbest=3, method='adjr2')
plot(adjr2.leaps$size, adjr2.leaps$adjr2, pch=23, bg='blue', cex=2)
best.model.adjr2 <- adjr2.leaps$which[which((adjr2.leaps$adjr2 == max(adjr2.leaps$adjr2))),]
print(best.model.adjr2)


# Cp -- all subsets

Cp.leaps <- leaps(X, data.training$Length, nbest=3, method='Cp')
plot(Cp.leaps$size, Cp.leaps$Cp, pch=23, bg='blue', cex=2)
best.model.Cp <- Cp.leaps$which[which((Cp.leaps$Cp == min(Cp.leaps$Cp))),]
print(best.model.Cp)


# Use stepwise search, both direction, starting at full model

full.step.both <- step(model1, direction='both')
print(summary(full.step.both))

# Backward

full.step.backward <- step(model1, direction='backward')
print(summary(full.step.backward))





# Predictions -------------------------------------------------------------

# variance inflation factors are acceptable
predictions <- model2 %>% predict(data.test)
data.frame( R2 = R2(predictions, data.test$Length),
            RMSE = RMSE(predictions, data.test$Length),
            MAE = MAE(predictions, data.test$Length))
RMSE=sqrt(mean((data.test$Length - predictions)^2))
#or
rmse(predictions, data.test$Length)


model10=lm(Length~(Sex+Canopy+Shrub)^2, data=data.training)
summary(model10)
predictions <- model8 %>% predict(data.test)
RMSE2=sqrt(mean((data.test$Length - predictions)^2))
RMSE2

hist(model8$residuals, main="Histogram of Residuals", xlab = "Residuals")


# Influential Points ------------------------------------------------------
#Model 8 influential points
cooksD <- cooks.distance(model8)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]
influential
car::outlierTest(model8)
data.training['256',]#most extreme observation in the model
summary(data.training)

#try modeling without outliers
names_of_influential <- names(influential)
outliers <- data.training[names_of_influential,]
data_without_outliers <- data.training %>% anti_join(outliers)
model_new1 <- lm(Length ~ ., data = data_without_outliers)
summary(model_new1)
model_new2 <- lm(Length ~ (Sex+Canopy+Shrub+Effort)^2, data = hitters_without_outliers)
summary(model_new2)
model_new3 <- lm(Length ~ Sex+Canopy+Shrub+Effort, data = hitters_without_outliers)
summary(model_new3)
model_new4 <- lm(Length ~ Sex+Canopy+Shrub*Canopy, data = hitters_without_outliers)
summary(model_new4)
summary(model8)
#model_new4 is not substantially different, keeping the outliers may be a possible option
#Given it is not an entry error , it may provide some useful information about variablitiy of the data
