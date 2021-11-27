# Clear workspace
# rm(list = ls())

# import the data. We choose to leave out the 'Effort' variable.
frogs <- read.csv("~/,School/Master/Linear Models/Exam Assignment/dataset.txt",
                  sep="")
frogs <- frogs[,-4]

# Divide the data into a training and test set
set.seed(0199510)
d.test <- sample(1:dim(frogs)[1], round(dim(frogs)[1]/2) )
data.test <- frogs[d.test, ]
data.training <- frogs[-d.test, ]

# Regular regression model
regular_regression <- lm(Length ~ ., data=data.training)
summary(regular_regression)

# Store the regression coefficients in a vector
beta_reg <- regular_regression$coefficients

# Predict the test set targets with both matrix algebra and the predict 
# function. We should expect the same results (?). The model.matrix function
# sets up the design matrix X. It automatically dummy codes the categorical
# variables (in the same way as was done when fitting the regression model) and
# adds a row of ones in front of the data.
pred.manual <- model.matrix(Length ~ ., data.test) %*% beta_reg
pred.ols <- predict(regular_regression, data=data.test[,-5])

# But they are not the same
data.frame(pred.manual, pred.ols)

# The manual prediction is a lot better than the predict function prediction
ytest = data.test[,5]
mse.ols <- (1/dim(data.test)[1])*sum((ytest-pred.ols)^2)
mse.manual <- (1/dim(data.test)[1])*sum((ytest-pred.manual)^2)
df.mse <- data.frame(mse.ols, mse.manual)
colnames(df.mse) <- c("OLS", "Manual")
df.mse

# As a side note: the manual predictions also are a lot more similar to the 
# predictions I obtained using Ridge and Lasso regression (both of which choose
# a very low shrinkage parameter so the results should be similar to OLS 
# regression). Therefore I suspect the result given by the predict function to
# be wrong.