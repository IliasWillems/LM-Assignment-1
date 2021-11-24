setwd("/Users/deryaengin/Desktop") #set the working directory
install.packages("sjstats")
install.packages("sjmisc")
install.packages("tidyverse")
install.packages("Metrics")
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
model1=lm(Length~., data=data.training)
summary(model1)
model2=lm(Length~Sex+Canopy+Shrub+Effort, data=data.training)
summary(model2)
model3=lm(Length~Sex+Canopy+Shrub+Effort+Forest, data=data.training)
summary(model3)
model4=lm(Length~Sex+Canopy+Shrub+Effort+Natural, data=data.training)
summary(model4)
par(mfrow = c(2, 2))
plot(model2)
vif(model2) # variance inflation factors are acceptable
predictions <- model2 %>% predict(data.test)
data.frame( R2 = R2(predictions, data.test$Length),
            RMSE = RMSE(predictions, data.test$Length),
            MAE = MAE(predictions, data.test$Length))
RMSE=sqrt(mean((data.test$Length - predictions)^2))
#or
rmse(predictions, data.test$Length)
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
par(mfrow=c(1,1))
##Try Lasso Regression
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
model10=lm(Length~(Sex+Canopy+Shrub)^2, data=data.training)
summary(model10)
predictions <- model8 %>% predict(data.test)
RMSE2=sqrt(mean((data.test$Length - predictions)^2))
RMSE2

hist(model8$residuals, main="Histogram of Residuals", xlab = "Residuals")
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