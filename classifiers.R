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
# 1.00		04/05/2021 	- 	created functions to create and fit logistic regression model, svm model, knn and random forest
# 1.01		05/05/2021 	- 	created generic function to predict with logistic regression, random forest and svm model.
# ************************************************


# ************************************************
# buildLogisticRegression() :
#
# A utility method to build a logistic regressor model
#
# INPUT:
#        DataFrame - data            - A dataframe containing RFM and Target fields.
#        String    - targetFieldName - Name of the target field in the dataset.
#        Integer   - foldCount       - Number of folds to be used in the training.
#        Integer   - epochs          - Number of times each fold should be repeated.
#
# OUTPUT : model - A linear regression model object
# ************************************************

buildLogisticRegression <- function(data, targetFieldName, foldCount, epochs) {
  data[,c(targetFieldName)] = as.factor(data[,c(targetFieldName)])
  train.control <- trainControl(method = "cv", 
                                number = foldCount, repeats = epochs)  
  # model <- multinom(as.formula(paste(targetFieldName, "~ .")), data = data)
  model <- train(as.formula(paste(targetFieldName, "~ .")), data = data, method = "multinom",
                 trControl = train.control)  
  print(summary(model))
  return (model)
}

# ************************************************
# buildRandomForest() :
#
# A utility method to build a random forest model.
#
# INPUT:
#        DataFrame - data            - A dataframe containing RFM and Target fields.
#        String    - targetFieldName - Name of the target field in the dataset.
#
# OUTPUT : model - A random forest model object
# ************************************************

buildRandomForest <- function(data, targetFieldName) {
  data[,c(targetFieldName)] = as.factor(data[,c(targetFieldName)])
  model <- randomForest(as.formula(paste(targetFieldName, "~ .")),data = data,ntree=1500,mtry=1,importance=TRUE,proximity=TRUE,type='classification')
  print(summary(model))
  return (model)
}

# ************************************************
# predictClassifier() :
#
# A utility method to create predictions for classifiers.
#
# INPUT:
#        Model     - model           - A classifier model object (Logistic regressor, SVM)
#        DataFrame - data            - A dataframe containing RFM and Target fields.
#
# OUTPUT : Integer - A list of class labels
# ************************************************

predictClassifier <- function(model, data){
  predictions = as.integer(predict(model, newdata = data)) - 1
  return (predictions)
}

# ************************************************
# predictKNN() :
#
# A utility method to fit and return predicted clusters of customers
#
# INPUT:
#        DataFrame - trainData            - A dataframe containing RFM and Target fields meant for fitting the knn model.
#        DataFrame - testData             - A dataframe containing RFM fields that we want to predict for.
#        String    - targetFieldName      - The name of the target field
#
# OUTPUT : Integer - A list of class labels
# ************************************************

predictKNN <- function(trainData, testData, targetFieldName) {
  trainData[,c(targetFieldName)] = as.factor(trainData[,c(targetFieldName)])
  testData[,c(targetFieldName)] = as.factor(testData[,c(targetFieldName)])
  xtrain <- cbind(trainData$Recency , trainData$Frequency, trainData$Monetary)
  xtest <- cbind(testData$Recency , testData$Frequency , testData$Monetary)
  ytrain <- trainData$Target
  ytest <- testData$Target
  
  
  yPred <- knn(trainData, testData, cl=ytrain, k=5)
  yPred <- (as.integer(yPred)) - 1
  return (yPred)
}

# ************************************************

# buildXGB() :

#

# A utility method to build an xgb model.

#

# INPUT:

#        DataFrame - data            - A dataframe containing RFM and Target fields.

#        String    - targetFieldName - Name of the target field in the dataset.

#        Integer   - foldCount       - Number of folds to be used in the training.

#        Integer   - epochs          - Number of times each fold should be repeated.

#

# OUTPUT : model - An xgb model object

# ************************************************

buildXGB <- function(data, targetFieldName, foldCount, epochs) {
  
  data[,c(targetFieldName)] = as.factor(data[,c(targetFieldName)])
  
  train.control <- trainControl(method = "cv",
                                
                                number = foldCount, repeats = epochs, )
  
  
  
  model <- train(as.formula(paste(targetFieldName, "~ .")), data = data, method = "xgbLinear",objective = 'multi:softprob',num_class =4,
                 
                 tuneGrid = expand.grid(nrounds=c(50,100,150), lambda=c(0,0.1), alpha=c(0,0.1),eta =c(0.01,0.1,0.3)),
                 
                 trControl = train.control)
  
  
  
  print(model)
  
  print(summary(model))
  
  return (model)
  
}