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
# 1.00		27/04/2021 	- 	created function to split dataset into test and train.
# 1.01		28/04/2021 	- 	created function buildNeuralNetwork to return a keras sequential model for classification
# 1.02		29/04/2021 	- 	Created function to train the keras sequential model using test train split methodology
# 1.03		30/04/2021 	- 	created function to predict with a keras sequential model
# 1.04		03/05/2021	-	  created shuffleDataset function to shuffle the dataset
# 1.05    05/05/2021  -   created function to train with kfold validation
# ************************************************

set.seed(42)

# ************************************************
# shuffleDataset() :
#
# A utility method to shuffle data to remove any kind of pattern inherent in the dataset.
#
# INPUT:
#        DataFrame - dataset - A dataframe containing RFM and Target fields
#
# OUTPUT : DataFrame - dataset - Shuffled dataset
# ************************************************

shuffleDataset <- function(dataset) {
  rows <- sample(nrow(dataset))
  dataset <- dataset[rows, ]
  return (dataset)
}

# ************************************************
# testTrainSplit() :
#
# A utility method to split the dataset into test and train.
#
# INPUT:
#        DataFrame - dataset - A dataframe containing RFM and Target fields
#        Numeric   - split   - A ratio to split the dataset to train and test.
#
# OUTPUT : Integer - ind - A list of integers consisting of split % of dataset
# ************************************************

testTrainSplit <- function(dataset, split=0.8) {
  ind <- sample(nrow(dataset),round(nrow(dataset)*split,0))
  return (ind)
}

# ************************************************
# buildNeuralNetwork() :
#
# A utility method to build a keras model.
#
# INPUT:
#        DataFrame - data            - A dataframe containing RFM and Target fields.
#        String    - targetFieldName - Name of the target field in the dataset.
#        Numeric   - outputDim       - The number of labels in the dataset(for output layer).
#
# OUTPUT : KerasModel - model - A keras model object
# ************************************************

# https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/

buildNeuralNetwork <- function(data, targetFieldName, outputDim=4) {
  targetFormula = as.formula(paste(targetFieldName, "~."))
  print(targetFormula)
  spec <- feature_spec(data, Target ~ . ) %>%
  # spec <- feature_spec(train_df,  target_formula) %>%
    step_numeric_column(all_numeric(), normalizer_fn = scaler_min_max()) %>% 
    fit()  
  input <- layer_input_from_dataset(data[,c("Recency", "Frequency", "Monetary")])
  
  output <- input %>% 
    layer_dense_features(dense_features(spec)) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 128, activation = "relu") %>%
    # layer_dense(units = 256, activation = "relu") %>%
    layer_dense(units = 128, activation = "relu") %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dense(units = outputDim, activation= 'softmax') 
  
  model <- keras_model(input, output)
  
  model %>% 
    compile(
      loss = "categorical_crossentropy",
      optimizer = 'adam',
      metrics = list("accuracy")
    )
  print(summary(model))
  return (model)
}

# ************************************************
# trainNeuralNetwork() :
#
# A utility method to train a keras model.
#
# INPUT:
#        KerasModel - model           - A keras compiled model.
#        DataFrame  - data            - A dataframe containing RFM and Target fields for training.
#        Integer    - epoch           - Number of iterations the model should run for.
#        Numeric    - testTrainSplit  - The percentage of data to be used in test and train.
#        String     - targetFieldName - Name of the target field in the dataset.
#        Numeric    - outputDim       - The number of labels in the dataset(for output layer).
#
# OUTPUT : List - Keras model training history (loss and accuracy over epochs)
# ************************************************

trainNeuralNetwork <- function(model, data, epoch, testTrainSplit, targetFieldName, outputDim=4) {
  return (
    model %>% fit(
      x = data[,c("Recency", "Frequency", "Monetary")],
      y = to_categorical(data[,c(TARGET_FIELD)], num_classes = outputDim),
      epochs = epoch,
      validation_split = 1-testTrainSplit,
      verbose = 1
    )
  )
}

# ************************************************
# trainNeuralNetwork() :
#
# A utility method to train a keras model.
#
# INPUT:
#        KerasModel - model           - A keras compiled model.
#        DataFrame  - data            - A dataframe containing RFM and Target fields for training.
#        Integer    - epoch           - Number of iterations the model should run for.
#        Numeric    - testTrainSplit  - The percentage of data to be used in test and train.
#        String     - targetFieldName - Name of the target field in the dataset.
#        Numeric    - outputDim       - The number of labels in the dataset(for output layer).
#
# ************************************************

# https://github.com/rstudio/keras/issues/284#issuecomment-371906744

kfoldTrain <- function(model, data, epoch, testTrainSplit, targetFieldName, outputDim=4) {
  folds <- createFolds(y = data[, targetFieldName], k = 5, list = F)
  data$folds <- folds
  for(f in unique(data$folds)){
    cat("\n Fold: ", f)
    ind <- which(data$folds == f)
    trainData <- data[-ind,c("Recency", "Frequency", "Monetary")]
    yTrain <- to_categorical(data[-ind, targetFieldName], num_classes = outputDim)
    validationData <- data[ind,c("Recency", "Frequency", "Monetary")]
    yValidation <- to_categorical(data[ind, targetFieldName])
    trainingHistory <- model %>% fit(
      x = trainData, y = yTrain,
      batch_size = 256,
      epochs = epoch,validation_data = list(validationData, yValidation))
    
  } 
}

# ************************************************
# predictNeuralNetwork() :
#
# A utility method to predict with a keras model.
#
# INPUT:
#        KerasModel - model           - A keras compiled model.
#        DataFrame  - data            - A dataframe containing RFM and Target fields for prediction.
#
# OUTPUT : Numeric - yPred - A list of predicted classes
# ************************************************

predictNeuralNetwork <- function(model, data) {
  yPred = model %>% predict(data[,c("Recency", "Frequency", "Monetary")])
  yPred = apply(yPred, 1, which.max) - 1
  return (yPred)
}
