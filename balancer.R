# ************************************************
# This work is licensed under a Creative Commons
# Attribution-NonCommercial 4.0 International License.
# ************************************************
#  PRACTICAL BUSINESS ANALYTICS
#  Course Work
#
# R-elatable
#   1.Akshay Pujari
#   2.Nischay Singh 
#   3.Preethi Patrick 
#   4.Suyash Saxena 
#   5.Vaibhav Sardana 
#   6.Vipul Mishra 
# MSc Data SCience
# University of Surrey
#
# UPDATE
# 1.00		26/04/2021 	- 	Function to balance the dataset.
# ************************************************
# R script to handle data balancing using SMOTE from UBL package

# ************************************************
# balance() :
#
# A utility method to balance our feature engineered dataset using smote from UBL
#
# INPUT:
#        DataFrame - data - dataframe of recency, frequency, monetary and target fields
#        String - targetFieldName - Name of the target field in the data dataframe object
#        List - balancingFactor - the ratio of data to be generated in each class (0,1,2,3)
#
# OUTPUT : DataFrame - Balanced dataset
# ************************************************

balance <- function(data, targetFieldName, balancingFactor=list("0"=519, "1"=115, "2"=6, "3"=1)) {

  data$Target = as.factor(data$Target)
  
  balanced <- SmoteClassif(
    as.formula(paste(targetFieldName, "~.")), 
    data, C.perc = balancingFactor, k = 1, 
    repl = FALSE, dist = "Euclidean", p = 2    
  )

  balanced = data.frame(lapply(balanced, as.integer))  
  balanced$Target = balanced$Target - 1
  
  return (balanced)
}
