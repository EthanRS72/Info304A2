##########################################################
# utils.R
##########################################################
#
# Contains general utility functions that are useful.
###########################################################

######################################################
# data.factorise(data,factor.cols)
###################################
# IN: data - a dataset
#   factor.cols - the columns to turn into factors
# OP: Turns these columns into factors
# OUT: The resulting dataset with the columns now as factors.
#
######################################################
data.factorise <- function(data,factor.cols)
{
  cols <- colnames(data)
  # Just keep columns that are factors
  # See hepatitis description for order of variables and values
  # These are the default values for factor.cols
  cols <- cols[factor.cols]
  data[,cols] <- lapply(data[,cols],factor)
  data
}
