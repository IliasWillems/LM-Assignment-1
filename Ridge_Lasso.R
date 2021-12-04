# Conclusion of all of the code below:

# Leaving out the Effort variable from the start, the OLS regression model indi-
# cates that the variables Size, Natural and Forest are not significant (where
# Natural is a borderline case). Doing variable selection with the Lasso, we 
# obtain that we should either leave all variables in (when not standardizing
# the data) or should exclude Size and ForestNgangao N from the model (when 
# standardizing the data, which is better (see below for reason)).

# We also obtain slightly better prediction results with the ridge and Lasso
# model. This is ofcourse to be expected, as there always exists a shrinkage
# parameter lambda such that the prediction is better (theorem). Between the 
# prediction results of the standardized and unstandardized ridge and lasso
# models there is only a very small difference. The standardized versions are
# a little bit better, which is likely due to more shrinkage of the parameters
# (some parameters in the lasso model were even set to zero), leading to less 
# variance in the prediction and resulting in a better prediction.

# If we were to include the ridge and lasso as one of the models, I think Lasso
# is definitely the better choice here. In that case there exist also nice plots
# we could make that show the shrinkage of the coefficients with respect to the
# value of the shrinkage parameter, as well as a plot of the mse with respect
# to the shrinkage parameter. Nevertheless, I dont think we could fill more than
# ~1.5 pages with the results below.

# clear workspace
rm(list = ls())

# Load necessary packages
library(glmnet)

# import the data. We choose to leave out the 'Effort' variable.
frogs <- read.csv("~/,School/Master/Linear Models/Exam Assignment/dataset.txt",
                  sep="")
frogs <- frogs[,-4]

# Divide the data into a training and test set
set.seed(0199510)
d.test <- sample(1:dim(frogs)[1], round(dim(frogs)[1]/2) )
data.test <- frogs[d.test, ]
data.training <- frogs[-d.test, ]

################################################################################
#                      Penalized Regression analysis                           #
################################################################################

# Regular regression model
regular_regression <- lm(Length ~ ., data=data.training)
summary(regular_regression)

# Store the regression coefficients in a vector
beta_reg <- regular_regression$coefficients

# Create a set of values for the tuning parameter lambda used later in the
# gmlnet functions. Out of this set of lambdas, the best one will be chosen 
# using CV.
lambdas = 10^seq(5, -5, by = -.1)

# Store the selected training data into a data matrix X and target vector y. The
# model.matrix function automatically adds a column of ones in front of the 
# matrix but this column is not needed in the glmnet functions. We therefore 
# leave it out (hence the [,-1] at the end).
X = model.matrix(Length~., data.training)[,-1]
y = data.training$Length

# Apply ridge regression on the data. Note that the best lambda is auto-
# matically selected by the function cv.glmnet().

# !! I chose to not standardize the variables and to determine the shrinkage !!
# !! parameter using Cross-Validation. It could be and idea to use           !!
# !! standardized variables or determine the shrinkage parameter using VIF.  !!
cv_fit = cv.glmnet(X, y, alpha = 0, lambda = lambdas, 
                   intercept = TRUE, standardize = FALSE)
lambda_best = cv_fit$lambda.min

ridge_model <- glmnet(X, y, alpha = 0, lambda = lambda_best, 
                      intercept = TRUE, standardize = FALSE)

# Store the regression coefficients in a vector
beta_rig <- coef(ridge_model)


# Apply lasso regression on the data.
cv_fit = cv.glmnet(X, y, alpha = 1, lambda = lambdas,
                   intercept = TRUE, standardize = FALSE)
lambda_best = cv_fit$lambda.min

lasso_model <- glmnet(X, y, alpha = 1, lambda = lambda_best, 
                      intercept = TRUE, standardize = FALSE)

# Store the regression coefficients in a vector
beta_las <- coef(lasso_model)

# Show both regression coefficients side-by-side
df.coeff <- data.frame(as.matrix(beta_reg), as.matrix(beta_rig), as.matrix(beta_las))
colnames(df.coeff) <- c("OLS", "Ridge", "Lasso")
df.coeff

# test the models on validation set
Xtest = model.matrix(Length~., data.test)[,-1]
ytest = data.test$Length

pred.manual <- model.matrix(Length ~ ., data.test) %*% beta_reg
pred.ols <- predict(regular_regression, data=data.test)

pred.rid <- predict(ridge_model, newx=Xtest)
pred.las <- predict(lasso_model, newx=Xtest)

df.pred <- data.frame(pred.ols, pred.manual, pred.rid, pred.las, ytest)
colnames(df.pred) <- c("pred.ols", "pred.manual", "pred.rid", "pred.las", "ytest")
df.pred

mse.ols <- (1/dim(data.test)[1])*sum((ytest-pred.ols)^2)
mse.manual <- (1/dim(data.test)[1])*sum((ytest-pred.manual)^2)
mse.rid <- (1/dim(data.test)[1])*sum((ytest-pred.rid)^2)
mse.las <- (1/dim(data.test)[1])*sum((ytest-pred.las)^2)

df.mse <- data.frame(mse.ols, mse.manual, mse.rid, mse.las)
colnames(df.mse) <- c("OLS", "Manual", "Ridge", "Lasso")
df.mse

# Ridge and Lasso do slightly better. This was ofcourse to be somewhat
# expected as there always exists a lambda such that ridge/lasso is better than
# OLS.

# Ridge and Lasso leave in all parameters (note that Effort was left out from
# the start).


################################################################################
#  Penalized Regression analysis: the same but with standardized coefficients  #
################################################################################

# This section is not so important. I think it is better (and one is supposed
# to) standardize all variables and not just the continuous ones.

# Clear workspace
rm(list = ls())

# import the data. We choose to leave out the 'Effort' variable.
frogs <- read.csv("~/,School/Master/Linear Models/Exam Assignment/dataset.txt",
                  sep="")
frogs <- frogs[,-4]

# Divide data and standardize the coefficients
set.seed(0199510)
frogs_std <- frogs
frogs_std[,2:5] <- scale(frogs[,2:5], center = TRUE, scale = TRUE)
d.test <- sample(1:dim(frogs_std)[1], round(dim(frogs_std)[1]/2) )
data.test.std <- frogs_std[d.test, ]
data.training.std <- frogs_std[-d.test, ]

# Possible issue: Doing it like this makes it so that you do not standardize the
# dummy coded variables for Forest and Natural. I don't know if you are supposed
# to standardize them as well.

# Regular regression model
regular_regression_std <- lm(Length ~ ., data=data.training.std)
summary(regular_regression_std)

# Store the regression coefficients in a vector
beta_reg_std <- regular_regression_std$coefficients

# Create a set of values for the tuning parameter lambda used later in the
# gmlnet functions.
lambdas = 10^seq(5, -5, by = -.1)

# Store the selected training data into a data matrix X and target vector y
X = model.matrix(Length~., data.training.std)[,-1]
y = data.training.std$Length

# Apply ridge regression on the data. Note that the best lambda is auto-
# matically selected by the function cv.glmnet().
cv_fit_std = cv.glmnet(X, y, alpha = 0, lambda = lambdas, 
                       intercept = TRUE, standardize = FALSE)
lambda_best_std = cv_fit_std$lambda.min

ridge_model_std <- glmnet(X, y, alpha = 0, lambda = lambda_best_std, 
                          intercept = TRUE, standardize = FALSE)

# Store the regression coefficients in a vector
beta_rig_std <- coef(ridge_model_std)


# Apply lasso regression on the data.
cv_fit_std = cv.glmnet(X, y, alpha = 1, lambda = lambdas, intercept = TRUE,
                       standardize = FALSE)
lambda_best_std = cv_fit_std$lambda.min

lasso_model_std <- glmnet(X, y, alpha = 1, lambda = lambda_best_std, 
                          intercept = TRUE, standardize = FALSE)

# Store the regression coefficients in a vector
beta_las_std <- coef(lasso_model_std)

# Show both regression coefficients side-by-side
df.std <- data.frame(as.matrix(beta_reg_std), as.matrix(beta_rig_std), as.matrix(beta_las_std))
colnames(df.std) <- c("OLS std", "Ridge std", "Lasso std")
df.std

# test the models on validation set
Xtest = model.matrix(Length~., data.test.std)[,-1]
ytest = data.test.std$Length
pred.ols.std <- predict(regular_regression_std, data=Xtest)
pred.manual.std <- model.matrix(Length ~ ., data.test.std) %*% beta_reg_std
pred.rid.std <- predict(ridge_model_std, newx=Xtest)
pred.las.std <- predict(lasso_model_std, newx=Xtest)

mse.ols.std <- (1/dim(data.test.std)[1])*sum((ytest-pred.ols.std)^2)
mse.manual.std <- (1/dim(data.test.std)[1])*sum((ytest-pred.manual.std)^2)
mse.rid.std <- (1/dim(data.test.std)[1])*sum((ytest-pred.rid.std)^2)
mse.las.std <- (1/dim(data.test.std)[1])*sum((ytest-pred.las.std)^2)

df.mse.std <- data.frame(mse.ols.std, mse.manual.std, mse.rid.std, mse.las.std)
colnames(df.mse.std) <- c("OLS std", "Manual std", "Ridge std", "Lasso std")
df.mse.std

# The results are all much worse than the results for unstandardized variables,
# which is weird?

################################################################################
#             All coefficients (including categorical) standardized            #
################################################################################

# Clear workspace
rm(list = ls())

# import the data. We choose to leave out the 'Effort' variable.
frogs <- read.csv("~/,School/Master/Linear Models/Exam Assignment/dataset.txt",
                  sep="")
frogs <- frogs[,-4]

# Divide the data into a training and test set
set.seed(0199510)
d.test <- sample(1:dim(frogs)[1], round(dim(frogs)[1]/2) )
data.test <- frogs[d.test, ]
data.training <- frogs[-d.test, ]

X.std <- model.matrix(Length ~ ., data=data.training)
X.std[,2:8] <- scale(X.std[,2:8], center = TRUE, scale = TRUE)
y.std <- scale(data.training$Length, center = TRUE, scale = TRUE)
colnames(y.std) <- c("Length")

X.test.std <- model.matrix(Length ~ ., data=data.test)
X.test.std[,2:8] <- scale(X.test.std[,2:8], center = TRUE, scale = TRUE)
y.test.std <- scale(data.test[,5], center = TRUE, scale = TRUE)
colnames(y.test.std) <- c("Length")

# Regular regression model. Don't estimate the intercept as it is zero (stan-
# dardized variables).
regular_regression_std <- lm(Length ~ . -1, data=as.data.frame(cbind(y.std, X.std[,-1])))
summary(regular_regression_std)

# Store the regression coefficients in a vector
beta_reg_std <- regular_regression_std$coefficients

# Create a set of values for the tuning parameter lambda used later in the
# gmlnet functions.
lambdas = 10^seq(5, -5, by = -.1)

# Store the selected training data into a data matrix X and target vector y
X = model.matrix(Length~., data.training)[,-1]
y = data.training$Length

# Apply ridge regression on the data. Note that the best lambda is auto-
# matically selected by the function cv.glmnet().
cv_fit_std = cv.glmnet(X, y, alpha = 0, lambda = lambdas, 
                       intercept = TRUE, standardize = TRUE)
lambda_best_std = cv_fit_std$lambda.min

ridge_model_std <- glmnet(X, y, alpha = 0, lambda = lambda_best_std, 
                          intercept = TRUE, standardize = TRUE)

# Store the regression coefficients in a vector
beta_rig_std <- coef(ridge_model_std)


# Apply lasso regression on the data.
cv_fit_std = cv.glmnet(X, y, alpha = 1, lambda = lambdas, intercept = TRUE, standardize = TRUE)
lambda_best_std = cv_fit_std$lambda.min

lasso_model_std <- glmnet(X, y, alpha = 1, lambda = lambda_best_std, 
                          intercept = TRUE, standardize = TRUE)

# Store the regression coefficients in a vector
beta_las_std <- coef(lasso_model_std)

# Show both regression coefficients side-by-side
df.std <- data.frame(rbind(0, as.matrix(beta_reg_std)), as.matrix(beta_rig_std),
                     as.matrix(beta_las_std))
colnames(df.std) <- c("OLS std", "Ridge std", "Lasso std")
df.std
# This table is not so useful as the glmnet functions return the coefficients on
# the original scale automatically.

# We can see that Lasso excludes Size and ForestNgangao N from the model

# Should you standardize the variables? Yes! Ridge and Lasso apply shrinkage
# evenly over all regression coefficients, which means that large regression
# coefficients will get set to zero more difficult as they need more shrinkage.
# It is therefore a good idea to standardize all variables.

# test the models on validation set
Xtest = model.matrix(Length~., data.test)[,-1]
ytest = data.test$Length

pred.rid <- predict(ridge_model_std, newx=Xtest)
pred.las <- predict(lasso_model_std, newx=Xtest)

df.pred <- data.frame(pred.rid, pred.las, ytest)
colnames(df.pred) <- c("pred.rid", "pred.las", "ytest")
df.pred

mse.rid <- (1/dim(data.test)[1])*sum((ytest-pred.rid)^2)
mse.las <- (1/dim(data.test)[1])*sum((ytest-pred.las)^2)

df.mse <- data.frame(mse.rid, mse.las)
colnames(df.mse) <- c("Ridge", "Lasso")
df.mse
# Comparing this with the mse's obtained in the first section, it can be seen
# that these ones are a bit better, though only by a very small amount.
