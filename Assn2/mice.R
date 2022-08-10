############################################################
# mice.R
############################################################
#
# Functions to handle imputation using the mice package
# Main tasks are to manage the imputed datasets and
# produce a single training/test split on request.
############################################################

library(mice) # load mice package for imputation

#############################################################
# make.imputations
#####################
# Call to mice to build m imputated datasets
# IN: data - the dataset with NA's
#        m - number of imputations
#   printFlag - FALSE - don't give details of computation
#               TRUE  - give information about model as it runs.
# OP: Model NAs and produce m different imputed datasets 
# OUT: List datastructure holding the m different datasets
############################################################
make.imputations <- function(data,m=5, printFlag=FALSE)
{
  imps <- mice(data,m=m,printFlag=printFlag) # do default method since it chooses suitable based
                         # on the target column 
  
  
  comp <- complete(imps,action="long") # make one big dataset
  comp.split <- split(comp,comp$.imp) # split up datasets into m imputations
  # and remove .imp and .id columns from each split
  for (i in 1:length(comp.split))
  {
    comp.split[[i]] <- comp.split[[i]][,-c(1:2)]
  }
  comp.split
}
###########################################################################
# get.train.test
########################
# IN: imp        - the list of datasets returned from make.imputations(...)
#     perc.train - proportion of data as training data
#                  1-perc.train is amount of test data
# OP: Randomly selects one of the imputated datasets, and then cuts this into 
#     a training and test dataset
# OUT: 2 element list: [[1]] == training data
#                      [[2]] == test data
############################################################################
get.imp.train.test <- function(imp,perc.train=0.9)
{
  # pick the dataset
  index <- sample(1:length(imp),1)
  data <- imp[[index]]
  train.rows <- sample(1:nrow(data),nrow(data)*perc.train,replace=FALSE)
  return(list(data[train.rows,],data[-train.rows,]))
}

