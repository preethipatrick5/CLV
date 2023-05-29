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
# 1.00		04/05/2021 	- 	created function to calculate confusion matrix, and calculate f1score
# 1.01		05/05/2021 	- 	created function to plot roc curve and confusion matrix
# ************************************************
# ************************************************
# plotROC() :
#
# A utility method to plot ROC Curve.
#
# INPUT:
#        Numeric - y - Actual y values
#        Numeric - yPred - Predicted y values
#
# ************************************************

# https://stackoverflow.com/questions/34169774/plot-roc-for-multiclass-roc-in-proc-package

plotROC <- function(y, yPred){
  roc.multi <- multiclass.roc(y, yPred)
  rs <- roc.multi[['rocs']]
  plot.roc(rs[[1]])
  sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
}

# ************************************************
# printConfusionMatrix() :
#
# A utility method to calculate the confusion matrix.
#
# INPUT:
#        Numeric - y - Actual y values
#        Numeric - yPred - Predicted y values
#
# OUTPUT : List - calculated confusion matrix
# ************************************************

printConfusionMatrix <- function(y, yPred) {
  return (confusionMatrix(as.factor(yPred), as.factor(y)));
}


# ************************************************
# f1Score() :
#
# A utility method to calculate the f1 Score.
#
# INPUT:
#        List - confMatrix - Calculated confusion matrix
#
# OUTPUT : DataFrame - A dataframe with precision, recall and f1
# ************************************************

# https://stackoverflow.com/questions/55929790/how-can-i-calculate-f1-measure-and-roc-in-multiclass-classification-problem-in-r
f1Score <- function(confMatrix){
  confMatrix = as.matrix(confMatrix)
  
  # number of instances per class
  rowsums = apply(confMatrix, 1, sum)
  # number of predictions per class
  colsums = apply(confMatrix, 2, sum)
  # number of correctly classified instances per class   
  diag = diag(confMatrix)
  
  precision = diag / colsums 
  recall = diag / rowsums 
  f1 = 2 * precision * recall / (precision + recall) 
  
  return (data.frame(precision, recall, f1))
}

# ************************************************
# plotConfusionMatrix() :
#
# A utility method to calculate the f1 Score.
#
# INPUT:
#        List   - confMatrix - Calculated confusion matrix
#        String - title      - A title for the plot
#
# ************************************************

plotConfusionMatrix <- function(confMatrix, title){
  confMatrixPlot = pheatmap(as.matrix(confMatrix), main=paste("Confusion matrix : ", title), display_numbers = T, cluster_rows=FALSE, cluster_cols = FALSE)
  print(confMatrixPlot)
}

# ************************************************
# Accuracy() :
#
# A utility method to get accuracy of the model.
#
# INPUT:
#        Numeric - y - Actual y values
#        Numeric - yPred - Predicted y values
#
# OUTPUT : Confusion Matrix Statstics
# ************************************************
accuracy <- function(y, yPred) {
  confusionMatrix<-confusionMatrix(as.factor(yPred), as.factor(y))
  accuracy<-confusionMatrix$overall[1]
  return(accuracy)
}

# ************************************************
# printstats() :
#
# A utility method to get statistics of the confusion matrix.
#
# INPUT:
#        Numeric - y - Actual y values
#        Numeric - yPred - Predicted y values
#
# OUTPUT : Confusion Matrix Statstics
# ************************************************
printstats <- function(y, yPred) {
  confusionMatrix<-confusionMatrix(as.factor(yPred), as.factor(y))
  stats<-confusionMatrix$byClass
  return(data.frame(stats))
}

# ************************************************
# model_comparison() :
#
# A utility method to calculate the f1 Score.
#
# INPUT:
#        List   - confMatrix - Calculated confusion matrix
#        String - title      - A title for the plot
#
# ************************************************
model_comparison<-function(modelname,accuracy,comparison){
  #comparison<-data.frame()
  comparison<-rbind(comparison,list('Model'=modelname,'Accuracy'=accuracy))
  return(comparison)
}