# This script compares the different models based on prediction accuracy. It 
# will also asses the stability of each model.

# clear workspace
rm(list = ls())

# Load required packages
library(MASS)

# Load in the data (change path to location of data on your pc). Since all
# leave out the variable Effort, we leave it out from the start.
frogs <- read.csv("~/,School/Master/Linear Models/Exam Assignment/dataset.txt",
                  sep="")
frogs <- frogs[,-4]

# split data for cross-validation
set.seed(0199510)
d.test <- sample(1:dim(frogs)[1], round(dim(frogs)[1]/2) )
data.test <- frogs[d.test, ]
data.training <- frogs[-d.test, ]
n <- dim(data.training)[1]
p <- dim(data.training)[2]

# Compute the three models to compare (+normal OLS as baseline)
m_baseline <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest),
         data = data.training)

m_inter <- lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy,data=data.training)

m_log <- lm(log(Length) ~ Sex + Canopy + Shrub + Natural, data = data.training)

m1.stdres <- stdres(m_baseline)
w <- 1/lm(abs(m1.stdres) ~ Sex + Canopy + Shrub + Size + Natural + I(Forest),
          data = data.training)$fitted.values^2
m_WLS <- lm(Length ~ Sex + Canopy + Shrub  + Natural, weight = w,
         data = data.training)

# Model validation:
# jackknifed, fit model on validation set and compare regression
# coefficients

# Model comparison:
# train set MSE, MSEP

################################################################################
#                    Asses validity/stability of models                        #
################################################################################

# To asses the stability of the models, we refit them on the validation set data
# and compare the regression coefficients.
m_baseline_val <- lm(Length ~ Sex + Canopy + Shrub + Size + Natural + I(Forest),
                 data = data.test)

m_inter_val <- lm(Length ~ Sex+Canopy+Shrub+Shrub*Canopy,data=data.test)

m_log_val <- lm(log(Length) ~ Sex + Canopy + Shrub + Natural, data = data.test)

m1.stdres.val <- stdres(m_baseline_val)
w.val <- 1/lm(abs(m1.stdres.val) ~ Sex + Canopy + Shrub + Size + Natural +
            I(Forest), data = data.test)$fitted.values^2
m_WLS_val <- lm(Length ~ Sex + Canopy + Shrub  + Natural, weight = w.val,
            data = data.test)

round(data.frame(m_baseline$coefficients, m_baseline_val$coefficients), 3)
round(data.frame(m_inter$coefficients, m_inter_val$coefficients), 3)
round(data.frame(m_log$coefficients, m_log_val$coefficients), 3)
round(data.frame(m_WLS$coefficients, m_WLS_val$coefficients), 3)


# !!! Both for the WLS and log model, the coefficients for Natural changes sign.
# !!! Is this an indication that we should leave it out of the model?

# Apart from that, no significant differences are detected among the pairs of
# coefficients.

################################################################################
#                               Compare the models                             #
################################################################################

# Prediction accuracy: MSEP of test data based on training data
baseline.pred <- predict(m_baseline, newdata = data.test)
inter.pred <- predict(m_inter, newdata = data.test)
log.pred <- predict(m_log, newdata = data.test)
WLS.pred <- predict(m_WLS, newdata = data.test)

baseline_MSEP <- mean((baseline.pred - data.test$Length)^2)
inter_MSEP <- mean((inter.pred - data.test$Length)^2)
log_MSEP <- mean((exp(log.pred) - data.test$Length)^2)
WLS_MSEP <- mean((WLS.pred - data.test$Length)^2)

MSEP.df <- data.frame(baseline_MSEP, inter_MSEP, log_MSEP, WLS_MSEP)
colnames(MSEP.df) <- c("OLS", "Interaction", "Log trans", "WLS")
rownames(MSEP.df) <- c("MSEP")
MSEP.df

# We see that the MSEPs are all relatively comparable, though from the above 
# results, the model with interaction seems to be the best one. The ranking is
# 1) Interaction
# 2) Log model
# 3) WLS
# 4) OLS

# Prediction accuracy: MSEP of training data based on test data. That is,
# predict the training data based on the models obtained with the validation
# data.
baseline.pred.val <- predict(m_baseline_val, newdata = data.training)
inter.pred.val <- predict(m_inter_val, newdata = data.training)
log.pred.val <- predict(m_log_val, newdata = data.training)
WLS.pred.val <- predict(m_WLS_val, newdata = data.training)

baseline_MSEP_val <- mean((baseline.pred.val - data.training$Length)^2)
inter_MSEP_val <- mean((inter.pred.val - data.training$Length)^2)
log_MSEP_val <- mean((exp(log.pred.val) - data.training$Length)^2)
WLS_MSEP_val <- mean((WLS.pred.val - data.training$Length)^2)

MSEP.val.df <- data.frame(baseline_MSEP_val, inter_MSEP_val, log_MSEP_val,
                      WLS_MSEP_val)
colnames(MSEP.val.df) <- c("OLS", "Interaction", "Log trans", "WLS")
rownames(MSEP.val.df) <- c("MSEP")
MSEP.val.df

# The results are exactly the same as in the previous test. The ranking is
# 1) Interaction
# 2) Log model
# 3) WLS
# 4) OLS

# Prediction accuracy: PRESS criterion on full data set (jackknifing)
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
colnames(PRESS) <- c("OLS", "Int", "Log", "WLS")
rownames(PRESS) <- c("PRESS")
PRESS

# The results are again exactly the same as in the previous test. The ranking is
# 1) Interaction
# 2) Log model
# 3) WLS
# 4) OLS
