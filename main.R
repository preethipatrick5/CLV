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
# 1.00		15/04/2021 	- 	Initial
# 1.01		16/04/2021 	- 	Added updated montly sales analysis plot
# 1.02		17/04/2021 	- 	updated with createMonetary function call
# 1.03		19/04/2021 	- 	updated RFM function calls from processor
# 1.04		21/04/2021 	- 	updated with RFM cluster creation calls from processor and sorting them
# 1.05		22/04/2021 	- 	invoking plots to visualize RFM
# 1.06		26/04/2021 	- 	invoking the balancer
# 1.07		30/04/2021 	- 	invoking keras model building and training
# 1.08		03/05/2021 	- 	integrating shuffle data before model training
# 1.09		05/05/2021 	- 	updating RFM creation and clustering and training data creation with calls from utils module. also integrated evaluation metrics
# 1.10		07/05/2021 	- 	integrated logistic regressor, svm, random forest and knn along with their evaluation metrics
# 1.11		08/05/2021 	- 	Integrated additional data summarization plots
# ************************************************

#  clears all objects in "global environment"
rm(list=ls())

# Utility function to load/insatll all the libraries required for the project
installRequirements <- function(requirements) {
  library(pacman)
  pacman::p_load(char=requirements, install=TRUE, character.only=TRUE)
}

# Return a list of required libraries
getRequirementList <- function(){
  c(
    "kernlab", "nnet", "rpart", "class", "e1071", "factoextra", "formattable",
    "summarytools", "plyr", "mice", "dplyr", "VIM", "Amelia", "mlbench",
    "corrplot", "ggplot2", "RColorBrewer", "tidyverse", "hrbrthemes", "ggpubr", 
    "mlbench", "caret", "pROC", "pheatmap", "UBL", "randomForest",
    "tfdatasets", "carData", "keras", "xgboost", "fpc", "cluster"
  )
}

# Invoking the requirement installation/loading
installRequirements(getRequirementList())

##Load R script files
source('processor.R')
source('model.R')
source('balancer.R')
source('evaluate.R')
source('utils.R')
source('data_visualisation.R')
source('classifiers.R')

# Clear console
cat("\014")

# GLOBALS
## File name of our dataset
DATASET_FILENAME="ecommerce.csv"

## The name of the target field
## For creating models as.formula is used to create the formula to train models but keras is not playing well with as.formula and it keeps throwing an error.
TARGET_FIELD = "Target"

## The test train split ratio, default ratio is 80% train to 20% test
TEST_TRAIN_SPLIT_RATIO = 0.8

## Plotting takes a lot of time and during testing it might slow us down so this flag helps us not plot if we are testing other parts of the code.
SHOULD_PLOT = TRUE

## The base graph colour
GRAPH_COLOR = "#69b3a2"

## The number of epochs the model should be run
EPOCH=5
FOLD_COUNT=5
CLUSTER_SIZE = 4

# ************************************************
# Name      :   main() :
# Purpose   :   Main entry point for evaluation of machine learning models
#
# INPUT     :   None
#
# OUTPUT    :   None
#
# ************************************************

main <- function(){
  # Starting the pipeline
  ## Loading the dataset from the configured file name
  data = getData(DATASET_FILENAME)
  
  if (SHOULD_PLOT){
    ## Plot the data summary in the viewer
    dataFrameToViewer(cleanDFSummary(dfSummary(data)))  
    ## Plot various graphs to undertand the data better
    visualizeBefore(data, GRAPH_COLOR)
  }
  
  # Starting preprocessing and feature engineering
  ## Removing outliers and null values
  data = cleanData(data)
  if (SHOULD_PLOT){
    ## print the data summary after cleaning the data
    dataFrameToViewer(cleanDFSummary(dfSummary(data)))
    ## print the type of data each column has and mean, min, max, skew and standard deviation if numeric column
    NPREPROCESSING_prettyDataset(data)
  }
  
  ## Change the type of InvoiceDate to date and then create YearMonth column and seperate Year, Month and Day columns
  data = convertInvoiceDateToDateAndCreateYearMonth(data)
  
  if (SHOULD_PLOT) {
    visualizeMonthlyMetric(data)
  }
  
  ## Create monetary field as Quantity * UnitPrice
  data = createMonetaryValues(data)
  
  ## The idea for splitting the dataset as such was inspired by this python notebook
  ## https://github.com/saritmaitra/Segmentation-Clustering/blob/master/RFM_%26_XGB.ipynb
  ## Calculating the 3 months data for finding RFM(this will be the X)
  threeMonthsData = createRFM(createThreeMonthsData(data))
  
  ## Calculating 6 months data to segment the users(the user segment will be Y)
  sixMonthsData = createRFM(createSixMonthsData(data))
  
  ## Creating clusters for RFM and sorting the clusters based on their center to make the label more meaningful
  sixMonthsData = createClusterAndSort(sixMonthsData, CLUSTER_SIZE)
  
  ## Renaming the monetary cluster column to the chosen name for the target field
  sixMonthsData = createTargetColumn(sixMonthsData, TARGET_FIELD)
  
  #Merging three months and 6 months data to merge X(3 months RFM) and Y(future 6 month customer segment) as one dataframe
  trainData = createTrainData(threeMonthsData, sixMonthsData, TARGET_FIELD)
  
  if (SHOULD_PLOT) {
    ## Plotting the RFM values and the clusters to get an understanding of the data distribution
    visualizeRFM(trainData, "Before balancing", GRAPH_COLOR)
    visualizeClusters(trainData, "Before balancing", CLUSTER_SIZE)
    dataFrameToViewer(cleanDFSummary(dfSummary(trainData)))
  }
  
  ## From the plots its clear the data is imbalanced, the data is being balanced with SMOTE from UBL library.
  ## The balancing factor controls how much data is generated for each class
  trainData = balance(data=trainData, targetFieldName=TARGET_FIELD, balancingFactor=list("0"=519, "1"=115, "2"=6, "3"=1))
  
  
  if (SHOULD_PLOT) {
    ## Plotting the RFM values after balancing.
    dataFrameToViewer(cleanDFSummary(dfSummary(trainData)))
    visualizeRFM(trainData, "After balancing")
    visualizeClusters(trainData, "After balancing", CLUSTER_SIZE)
  }
  
  ## Shuffling the dataset to remove the pattern generated by SMOTE or any other inherent pattern in the dataset.
  trainData = shuffleDataset(trainData)
  
  ## Splitting the dataset into train and test. Train will be used in training the model and test will be used in evaluating the model
  ## The train will again be split into test train if the model needs that. Test will only be used only for evaluation.
  ind = testTrainSplit(trainData, TEST_TRAIN_SPLIT_RATIO)
  
  train = trainData[ind,]
  test = trainData[-ind,]
  
  comparison<-data.frame()
  
  print("**************************************************Neural Network**************************************************")
  modelName = "DNN"
  model <- buildNeuralNetwork(trainData, TARGET_FIELD, outputDim=CLUSTER_SIZE)
  history <- kfoldTrain(model, train, EPOCH, TEST_TRAIN_SPLIT_RATIO, TARGET_FIELD, outputDim=CLUSTER_SIZE)
  
  yPred = predictNeuralNetwork(model, test)
  
  confMatrix = printConfusionMatrix(test[,c(TARGET_FIELD)], yPred)
  print(paste("Evaluation for ", modelName))
  plotConfusionMatrix(confMatrix, modelName)
  print(confMatrix)
  
  statsLr = formattable::formattable(printstats(test[,c(TARGET_FIELD)], yPred))
  print(statsLr)
  
  print(f1Score(confMatrix))
  
  accuracyLR = 100*(accuracy(test[,c(TARGET_FIELD)], yPred))
  print(accuracyLR)
  comparison<-model_comparison(modelName,accuracyLR,comparison)
  
  plotROC(test$Target, yPred)
  
  print("**************************************************Logistic Regression**************************************************")
  modelName = "Logistic Regression"
  logisticRegressor = buildLogisticRegression(train, TARGET_FIELD, FOLD_COUNT, EPOCH)
  yPred = predictClassifier(logisticRegressor, test)
  confMatrix = printConfusionMatrix(test[,c(TARGET_FIELD)], yPred)
  print(paste("Evaluation for ", modelName))
  plotConfusionMatrix(confMatrix, modelName)
  print(confMatrix)

  statsLr=formattable::formattable(printstats(test[,c(TARGET_FIELD)], yPred))
  print(statsLr)
  
  print(f1Score(confMatrix))
  
  accuracyLR = 100*(accuracy(test[,c(TARGET_FIELD)], yPred))
  print(accuracyLR)
  comparison<-model_comparison(modelName,accuracyLR,comparison)
  
  plotROC(test$Target, yPred)
  # Print the Hypertuned parameters tuning result for Log R
  generateHyperTuneTable(modeldf=logisticRegressor,modelName="Log R",beforeValue="decay")
  
  print("**************************************************XGBOOST**************************************************")
  modelName = "XGBoost"
  xgbModel = buildXGB(train, TARGET_FIELD, FOLD_COUNT, EPOCH)
  yPred = predictClassifier(xgbModel, test)
  confMatrix = printConfusionMatrix(test[,c(TARGET_FIELD)], yPred)
  print(paste("Evaluation for ", modelName))
  plotConfusionMatrix(confMatrix, modelName)

  statsLr=formattable::formattable(printstats(test[,c(TARGET_FIELD)], yPred))
  print(statsLr)
  
  print(f1Score(confMatrix))
  
  accuracyLR = 100*(accuracy(test[,c(TARGET_FIELD)], yPred))
  print(accuracyLR)
  comparison<-model_comparison(modelName,accuracyLR,comparison)
  
  print(f1Score(confMatrix))
  plotROC(test$Target, yPred)
  # Print the Hypertuned parameters tuning result for XGB
  
  generateHyperTuneTable(modeldf=xgbModel,modelName="XGB",beforeValue="nrounds")
  
  print("**************************************************Random Forest**************************************************")
  modelName = "Random Forest"
  randomForestModel = buildRandomForest(train, TARGET_FIELD)
  yPred = predictClassifier(randomForestModel, test)
  confMatrix = printConfusionMatrix(test[,c(TARGET_FIELD)], yPred)
  print(paste("Evaluation for ", modelName))
  plotConfusionMatrix(confMatrix, modelName)
  print(confMatrix)
  
  statsLr=formattable::formattable(printstats(test[,c(TARGET_FIELD)], yPred))
  print(statsLr)
  
  print(f1Score(confMatrix))
  
  accuracyLR = 100*(accuracy(test[,c(TARGET_FIELD)], yPred))
  print(accuracyLR)
  comparison<-model_comparison(modelName,accuracyLR,comparison)
  
  plotROC(test$Target, yPred)
  
  print("**************************************************KNN**************************************************")
  modelName = "KNN"
  yPred = predictKNN(train, test, TARGET_FIELD)
  confMatrix = printConfusionMatrix(test[,c(TARGET_FIELD)], yPred)
  print(paste("Evaluation for ", modelName))

  statsLr=formattable::formattable(printstats(test[,c(TARGET_FIELD)], yPred))
  print(statsLr)
  
  print(f1Score(confMatrix))
  
  accuracyLR = 100*(accuracy(test[,c(TARGET_FIELD)], yPred))
  print(accuracyLR)
  comparison<-model_comparison(modelName,accuracyLR,comparison)
  
  plotConfusionMatrix(confMatrix, modelName)
  
  print(f1Score(confMatrix))
  
  plotROC(test$Target, yPred)
  
  print("*********************************************Comparsion************************************************")
  
  comparison<-formattable::formattable(comparison)
  print(comparison)
}

# Entry point of the project
main()