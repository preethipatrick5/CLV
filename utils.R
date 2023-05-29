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
# 1.00		3/05/2021 	- 	Create function createThreeMonthsData and createSixMonths data to move the filtering logic away from the main file, added createRFM function to wrap the functions calls to processor into a single function
# 1.01		4/05/2021 	- 	create wrapper function to make cluster creation and merging of three months data easier.
# 1.02		7/05/2021 	- 	created function to print dataset summary inspired from Prof. Nick's lab3 code
# 1.03		8/05/2021 	- 	created function to print more detailed dataset summary 
# ************************************************

source('processor.R')

# ************************************************
# NPREPROCESSING_prettyDataset()
# Output simple dataset field analysis results as a table in "Viewer"
#
# REQUIRES: formattable
#
# INPUT: data frame    - dataset, full dataset used for train/test
#                      - Each row is one record, each column in named
#                      - Values are not scaled or encoded
#        String - OPTIONAL string which is used in table as a header
#
# OUTPUT : none
#
# Requires the library: PerformanceAnalytics
#                       formattable
# ************************************************

# This function is taken from lab3 source code provided by Prof. Nick F Ryman-Tubb

NPREPROCESSING_prettyDataset<-function(dataset,...){
  
  params <- list(...)
  
  tidyTable<-data.frame(Field=names(dataset),
                        Catagorical=FALSE,
                        Symbols=0,
                        Name=0,
                        Min=0.0,
                        Mean=0.0,
                        Max=0.0,
                        Skew=0.0,
                        StandardDeviation=0.0,
                        stringsAsFactors = FALSE)
  
  if (length(params)>0){
    names(tidyTable)[1]<-params[1]
  }
  
  for (i in 1:ncol(dataset)){
    isFieldAfactor<-!is.numeric(dataset[,i])
    tidyTable$Catagorical[i]<-isFieldAfactor
    if (isFieldAfactor){
      tidyTable$Symbols[i]<-length(unique(dataset[,i]))  #Number of symbols in catagorical
      #Gets the count of each unique symbol
      symbolTable<-sapply(unique(dataset[,i]),function(x) length(which(dataset[,i]==x)))
      majoritySymbolPC<-round((sort(symbolTable,decreasing = TRUE)[1]/nrow(dataset))*100,digits=0)
      tidyTable$Name[i]<-paste(names(majoritySymbolPC),"(",majoritySymbolPC,"%)",sep="")
    } else
    {
      tidyTable$Max[i]<-round(max(dataset[,i]),2)
      tidyTable$Mean[i]<-round(mean(dataset[,i]),2)
      tidyTable$Min[i]<-round(min(dataset[,i]),2)
      tidyTable$StandardDeviation[i]<-round(sd(dataset[,i]),2)
      tidyTable$Skew[i]<-round(PerformanceAnalytics::skewness(dataset[,i],method="moment"),2)
    }
  }
  
  #Sort table so that all numerics are first
  t<-formattable::formattable(tidyTable[order(tidyTable$Catagorical),],
                              list(Catagorical = formatter("span",style = x ~ style(color = ifelse(x,"green", "red")),
                                                           x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))),
                                   Symbols = formatter("span",style = x ~ style(color = "black"),x ~ ifelse(x==0,"-",sprintf("%d", x))),
                                   Min = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Min, nsmall=2, big.mark=","))),
                                   Mean = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",format(Mean, nsmall=2, big.mark=","))),
                                   Max = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(Max, nsmall=2, big.mark=","))),
                                   StandardDeviation = formatter("span",style = x ~ style(color = "black"), ~ ifelse(Catagorical,"-",format(StandardDeviation, nsmall=2, big.mark=","))),
                                   Skew = formatter("span",style = x ~ style(color = "black"),~ ifelse(Catagorical,"-",sprintf("%.2f", Skew)))
                              ))
  print(t)
}

# ************************************************
# printDataSetMeasures() :
#
# A utility method to print the statistical measures of each column in the dataframe
#
# INPUT:
#        DataFrame - data - A DataFrame object.
#
# ************************************************

printDataSetMeasures <- function(data){
  print(formattable::formattable(data.frame(descr(data))))
}

# ************************************************
# dataFrameToViewer() :
#
# A utility method to print the dataframe to the viewer panel
#
# INPUT:
#        DataFrame - data - A DataFrame object.
#
# ************************************************

dataFrameToViewer <- function(data) {
  print(formattable::formattable(data))
}


# ************************************************
# cleanDFSummary() :
#
# A utility method to replace \n in the fields of dataframe generated by dfSummary method from summarytools library.
#
# INPUT:
#        DataFrame - data - A DataFrame object.
# OUTPUT:
#        DataFrame - data - A DataFrame object after replacing \n with br tags.
# ************************************************

cleanDFSummary <- function(data){
  data[,"No"] = gsub('\\\\\\n', '<br>', data[,"No"])
  data[,"No"] = gsub('\\\\.', '.', data[,"No"])
  
  data[,"Variable"] = gsub('\\\\\\n', '<br>', data[,"Variable"])
  data[,"Variable"] = gsub('\\\\.', '.', data[,"Variable"])
  
  data[,"Stats / Values"] = gsub('\\\\\\n', '<br>', data[,"Stats / Values"])
  data[,"Stats / Values"] = gsub('\\\\.', '.', data[,"Stats / Values"])
  
  data[,"Freqs (% of Valid)"] = gsub('\\\\\\n', '<br>', data[,"Freqs (% of Valid)"])
  data[,"Freqs (% of Valid)"] = gsub('\\\\', '', data[,"Freqs (% of Valid)"])
  
  data[,"Valid"] = gsub('\\\\\\n', '<br>', data[,"Valid"])
  data[,"Valid"] = gsub('\\\\.', '.', data[,"Valid"])
  
  data[,"Missing"] = gsub('\\\\\\n', '<br>', data[,"Missing"])
  data[,"Missing"] = gsub('\\\\.', '.', data[,"Missing"])
  data = data[,c("No", "Variable", "Stats / Values", "Freqs (% of Valid)", "Graph", "Valid", "Missing")]
  return (data)
}

# ************************************************
# createThreeMonthsData() :
#
# A utility function to filter out the first 3 months data from the dataset.
# INPUT:
#        DataFrame - rfm   - A DataFrame object with ecommerce data.
#
# OUTPUT : DataFrame - rfm   - A DataFrame object with the data for first 3 months of data.
# ************************************************

createThreeMonthsData <- function(data) {
  return (filter(data, ((data$InvoiceDate >= as.Date("01/03/2011", "%d/%m/%Y"))) & (data$InvoiceDate < as.Date("01/06/2011", "%d/%m/%Y"))))
}


# ************************************************
# createSixMonthsData() :
#
# A utility function to filter out the second 6 months data from the dataset.
# INPUT:
#        DataFrame - rfm   - A DataFrame object with ecommerce data.
#
# OUTPUT : DataFrame - rfm   - A DataFrame object with the data for second 6 months of data.
# ************************************************

createSixMonthsData <- function(data) {
  return (filter(data, ((data$InvoiceDate >= as.Date("01/06/2011", "%d/%m/%Y"))) & (data$InvoiceDate < as.Date("01/12/2011", "%d/%m/%Y"))))
}


# ************************************************
# createRFM() :
#
# A wrapper function to create RFM values by calling function from the processor module.
# INPUT:
#        DataFrame - data   - A DataFrame object with ecommerce data.
#
# OUTPUT : DataFrame - rfm   - A DataFrame object with feature engineered recency, frequency and monetary fields.
# ************************************************

createRFM <- function(data) {
  recency = getRecency(data)
  frequency = getFrequency(data)
  monetary = getMonetary(data)
  data = getRFM(recency, frequency, monetary)
  return (data)
}


# ************************************************
# createRFM() :
#
# A wrapper function to cluster and sort RFM clusters
# INPUT:
#        DataFrame - data        - A DataFrame object with feature engineered recency, frequency and monetary fields.
#        Integer   - clusterSize - The number of clusters to be created
#
# OUTPUT : DataFrame - rfm   - A DataFrame object with feature engineered recency, frequency and monetary fields and their associated clusters.
# ************************************************

createClusterAndSort <- function(data, clusterSize=4){
  data = getRFMClusters(data, clusterSize)
  data = sortAndReassignClusters(data)
  return (data)
}


# ************************************************
# createTargetColumn() :
#
# A utility function to create the target field after getting the rfm clusters
# INPUT:
#        DataFrame - data           - A DataFrame object with feature engineered recency, frequency and monetary fields.
#        String   - targetFieldName - The name to given to the target field
#
# OUTPUT : DataFrame - rfm   - A DataFrame object with target field renamed to the given targetFieldName
# ************************************************

createTargetColumn <- function(data, targetFieldName){
  colnames(data)[8] = targetFieldName
  data[,c(targetFieldName)] = data[,c(targetFieldName)] - 1
  return (data)
}


# ************************************************
# getUserSegment() :
#
# A utility function to Converting overall score to a class using rules.
# INPUT:
#        DataFrame - rfm   - A DataFrame object with a feature engineered fields Recency, Frequency, Monetary, clusters, OverallScore of each customers.
#
# OUTPUT : DataFrame - rfm - A DataFrame object with a feature engineered fields Recency, Frequency, Monetary, clusters, OverallScore of each customers and the the assigned segment.
# ************************************************

createTrainData <- function(threeMonthsData, sixMonthsData, targetFieldName) {
  trainData = merge(x=threeMonthsData, y=sixMonthsData[,c("CustomerID",targetFieldName)], by="CustomerID")
  trainData = trainData[,c("Recency","Frequency","Monetary",targetFieldName)]
  trainData = data.frame(lapply(trainData, as.integer))
  return (trainData)
}
