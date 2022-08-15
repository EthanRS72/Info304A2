##############################################################
# logistic.R
##############################################################
#
# Implements logistic regression using imputed datasets
# and coefficient estimates for variable importance.
##############################################################
source("mice.R") #imputations and training/test splits 
library(caret)
####################################################
# fast.threshold
####################################################
#
# Calculate the probability threshold for +'ve cases
# IN: actual - the column of values of the response from the training data
#        NOTE:  values 2 and 1 
#     probs  - the predicted probabilities from the logistic regression
# OP: Uses actual probs to calculate the best threshold for maximising 
#     accuracy (TP + TN)
# OUT: The threshold probability (used as >=)
####################################################
fast.threshold <- function(actual,probs)
{
  act <- as.numeric(actual)-1 # make 0 and 1 for factors
  p.order <- order(probs) # get smallest to largest ordering of probs
  
  probs <- probs[p.order]
  act.order <- act[p.order]
  
  act.cs <- cumsum(act.order)
  x <- 1:length(act.cs)
  
  threshold <- x - 2*act.cs
  # return first maximum probability ...
  as.numeric(probs[(which(threshold==max(threshold)))[1]])
}

##########################################
# test.logistic(imp,formula,perc.train)
##########################################
# IN: imp - a list of imputed datasets
#  formula - R formula to construct a logistic model
#  perc.train - percentage of data to use for training
# OP: Creates a training and test set from the imputed datasets,
#  and builds a logistic model.
#  Determines the best threshold value for probability, and 
#  uses this for testing.  
# OUT: The accuracy of the prediction on the test data, defined
#  as the sum of (+,+) and (-,-) divided by total number of test items.
#  This can be interpreted as the accuracy, between 0 and 1.
#######################################################################

test.logistic <- function(imp,formula,perc.train=0.9)
{
  # get the training/test data randomly from one imputed datasets
  
  ttdata <- get.imp.train.test(imp,perc.train=perc.train)
  train <- ttdata[[1]]
  test <- ttdata[[2]]
  
  # Build the model...
  log.mod <- glm(formula, data=train, family="binomial")
  #
  # and now find the threshold to maximise accuracy...
  # uses the same training data (since we can't use test)
  train.pred <- predict(log.mod,newdata=train,type="response")
  
  # But now we can get back the threshold
  threshold <- fast.threshold(train$Class,train.pred)
  
  # and now test the model...
  test.pred <- predict(log.mod,newdata=test,type="response")
  
  # and use the threshold to calculate the final predicted values
  #
  pred.vals <- as.factor(ifelse(test.pred >=threshold,2,1))
  
  conf.mat <- caret::confusionMatrix(data = pred.vals, 
                                     reference=test$Class)
  
  # Return accuracy between 0 and 1
  (conf.mat$table[1]+conf.mat$table[4])/nrow(test)
}

#############################################################
# glm.coefficients(imp,formula,perc.train)
#
# Return the coefficient estimates for the glm using training
# data sourced from the imputed list of datasets.
#
# IN: imp - a list of imputed datasets
#  formula - R formula to construct a logistic model
#  perc.train - percentage of data to use for training
# OP: Create a glm using the training data, and return the 
# values of the coefficients.
# OUT: The named coefficients for the model
#############################################################
glm.coefficients <- function(imp,formula,perc.train=0.9)
{
  ttdata <- get.imp.train.test(imp,perc.train=perc.train)
  train <- ttdata[[1]]
  
  # Build the model...
  log.mod <- glm(formula, data=train, family="binomial")
  coef(log.mod)
}
#####################################################################
# variable.importance(..)
####################################################################
# This is the function that you need to write.
####################################################################
# IN: imp    - imputed list of datasets
#   formula  - the formula for the model
#   var.col  - the column number that is being assessed for importance
# perc.train - percentage of training data 
#
# OP:  Obtain training and test data from imp, initial test model
#      accuracy.
#      Then permute var.col in the test data, and reset.
#      Measure the percentage change in accuracy
#      Given initial.acc and final.acc this would be:
#      ((initial.acc-final.acc)/initial.acc)*100
# OUT: Single value as percentage change in accuracy.
####################################################################

variable.importance <- function(imp,formula,var.col=2,perc.train=0.9)
{
  #train test data
  dat <- as.matrix(imp)
  ttdata <- get.imp.train.test(dat,perc.train=perc.train)
  train <- ttdata[[1]]
  test <- ttdata[[2]]
  
  #create standard model
  log.mod <- glm(formula, data=train, family="binomial")
  train.pred <- predict(log.mod,newdata=train,type="response")
  threshold <- fast.threshold(train$Class,train.pred)
  test.pred <- predict(log.mod,newdata=test,type="response")
  pred.vals <- as.factor(ifelse(test.pred >=threshold,2,1))
  conf.mat <- caret::confusionMatrix(data = pred.vals, 
                                     reference=test$Class)
  baseline <- (conf.mat$table[1]+conf.mat$table[4])/nrow(test)
  #permutation test
  test[,var.col] <- sample(test[,var.col])
  test.pred <- predict(log.mod,newdata=test,type="response")
  pred.vals <- as.factor(ifelse(test.pred >=threshold,2,1))
  conf.mat <- caret::confusionMatrix(data = pred.vals, 
                                     reference=test$Class)
  perm <- (conf.mat$table[1]+conf.mat$table[4])/nrow(test)
  #get difference
  result <- ((baseline - perm) / baseline) * 100
  return(result)
}
################################################################
# collect.var.imp(..)
#########################
# This function can be called to do variable importance for all
# explanatory variables.
# IN: imp - the imputed dataset
#     cols - defaults to all columns
# perc.train - percentage of training data
# num.trials - Number of times to repeat variable importance test
# OP: Calls variable.importance *num.trials* times and collects up
#  the resulting accuracy for the columns specified in *cols*
# OUT: A table of results - number of columns = number of columns specified
#      by cols, number of rows = num.trials.
###################################################################
collect.var.imp <- function(imp,formula,cols=2:ncol(imp[[1]]),
                            perc.train=0.9, num.trials=100)
{
  res <- NULL
  for (i in 1:length(cols))
  {
    
    # Do lazy method of binding up results....
    res <- cbind(res,
                 replicate(num.trials,variable.importance(imp,
                                                    formula,var.col=cols[i])))
  }
  res
}
