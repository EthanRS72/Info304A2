#####################################
# simple random forest
#####################################
library(rpart)
library(caret)
library(randomForest)
#############################################################
# Build a single decision tree using the train data
# IN: train - the training data
#     formula - formula for model
#     maxdepth - depth of tree which should be forced
# OP: Builds a deep tree using the training data
# OUT: The rpart decision tree.
#############################################################
build.tree <- function(train,formula,maxdepth=30)
{
  rpart(formula,
        data = train, 
        model=TRUE,
        control=rpart.control(maxdepth=maxdepth,
                              minsplit=2,
                              minbucket=2, # Make a big tree if possible...
                              cp=0.0))
}
#######################################################
# 
####################################################################
# predict.tree
# Predict values for the test data given the single 
# decision tree
# IN: tree - (single) decision tree
#     test - test data
#     type - default is "class" given we are doing binary classification
# OP: Predicts values for the test examples
# OUT: The predictions as a vector
#######################################################################
predict.tree <- function(tree,test,type="class")
{
  predict(tree,
          newdata=test,
          type=type)
}




test.dt <- function(dat, formula, perc.train = 0.9, maxdepth = 30) {
  data <- get.imp.train.test(dat, perc.train=0.9)
  train <- data[[1]]
  test <- data[[2]]
  dt.mod <- build.tree(train, formula, maxdepth=30)
  predictions <- predict.tree(dt.mod, test)
  results <- table(test$Class, predictions)
  return (sum(diag(results))/sum(results))
}
##############################################################
# rf.predictions
#########################
# Given a forest of trees, predicts the test data
#
# IN: rf.trees:  forest of trees
#     test: Test data
# OP: Predict test data for each tree, and then take
#     majority vote (class problem) as result
# OUT: Predicted vector of classes for test data
###############################################################
rf.predictions <- function(rf.trees,test)
{
  res <- lapply(rf.trees,predict.tree,test)
  tree.predictions <- (do.call(cbind,res))
  preds <- apply(tree.predictions,1,
                 function(x) tail(names(sort(table(x))), 1))
  as.numeric(preds)
}
#############################################################
# test.rf
#############################################################
#
# Apply one sample of training/test data from the imputed datasets
# using a simple random forest.
# IN: imp - imputated datasets
#  perc.train - percentage of training data (as a fraction)
#  formula - model 
#  maxdepth - maximum depth of trees in forest
#  ntrees - number of trees in random forest
# OP: Sample training data from imp, create a forest, and then use
#     this to predict the test data.
# OUT: Accuracy measure (TP + TN)/total number of examples
##############################################################
test.rf <- function(imp,
                    formula=Class ~ .,
                    perc.train=0.9,
                    maxdepth=30,
                    ntrees=10)
{
  ttdata <- get.imp.train.test(imp,perc.train=perc.train)
  train <- ttdata[[1]]
  test <- ttdata[[2]]
  
  models <- list()
  
  for (i in 1:ntrees)
  {
    # bootstrap training dataset for each tree
    
    train.rows <- sample(nrow(train),nrow(train),replace=TRUE) # bootstrap
    train.data <- train[train.rows,]
    models[[i]] <- build.tree(train.data,formula,maxdepth) # build tree
  }
  pred.vals <- factor(rf.predictions(rf.trees=models,test),
         levels=levels(test$Class))
  conf.mat <- caret::confusionMatrix(data = pred.vals, 
                                     reference=test$Class)
  
  # Return accuracy between 0 and 1
  (conf.mat$table[1]+conf.mat$table[4])/nrow(test)
}


##################### Here is an implementation using the real 
# randomforest model (for comparison if you are interested)#########
# using the imputed dataset
# ntrees by default is 500, but can be passed in (that's what the ...
# is used for- other parameters to randomForest call).
####################################################################

imp.test.real.rf <- function(imp,formula=Class ~ .,perc.train=0.9,...)
{
  tt <- get.imp.train.test(imp)
  train <- tt[[1]]
  test <- tt[[2]]
  rf <- randomForest(formula, data=train,...)
  pred <- predict(rf,test)
  cm <- confusionMatrix(pred,test$Class)
  (cm$table[1]+cm$table[4])/nrow(test)
}

