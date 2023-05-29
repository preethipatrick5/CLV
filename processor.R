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
# 1.00		14/04/2021 	- 	function to create monetary field as UnitPrice * Quantity
# 1.01		17/04/2021 	- 	clean data with removing null values and negative values for quantity and price, functions to change type of InvoiceDate to date and extract Month, Year and Day as seperate columns, 
# 1.02		19/04/2021 	- 	Functions to generate Recency, Frequency and monetary Values
# 1.03		20/04/2021 	- 	Functions to create clusters of Recency Frequency and Monetary Values
# 1.04		21/04/2021 	- 	Function to rename cluster labels to make the label more meaningful (ie, cluster 0 should have center smaller than cluster 1)
# ************************************************


# ************************************************
# getData() :
#
# A wrapper around the read.csv method in R.
#
# INPUT:
#        String - fileName - The name of the csv file to be loaded.
#
# OUTPUT : DataFrame - data - A DataFrame populated with the csv file data.
# ************************************************

getData <- function(fileName) {
  data = read.csv(fileName)
  return (data)
}

# ************************************************
# cleanData() :
#
# A utility method to clean the dataset. Removes null values and filters out 
# UnitPrice and Quantity with 0 and -ve values.
#
# INPUT:
#        DataFrame - data - A DataFrame object with the ecommerce data.
#
# OUTPUT : DataFrame - data - A DataFrame object without any null values or negative values for quantity and unit price.
# ************************************************

cleanData <- function(data){
  data <- na.omit(data)
  data$InvoiceDate <- as.Date(data$InvoiceDate, "%d/%m/%Y")
  data = filter(data, UnitPrice > 0, Quantity > 0)
  return (data)
}

# ************************************************
# convertInvoiceDateToDateAndCreateYearMonth() :
#
# A utility method extract year, month, day from InvoiceDate and create a new column YearMonth(eg : 201102)
#
# INPUT:
#        DataFrame - data - A DataFrame object with the ecommerce data.
#
# OUTPUT : DataFrame - data - A DataFrame object with the converted InvoiceDate column and the new YearMonth column.
# ************************************************

convertInvoiceDateToDateAndCreateYearMonth <- function(data){
  data$Year <- format(data$InvoiceDate, "%Y")
  data$Month <- format(data$InvoiceDate, "%m")
  data$Day <- format(data$InvoiceDate, "%d")
  
  data$YearMonth <- format(data$InvoiceDate, "%Y%m")
  
  return (data)
}

# ************************************************
# createMonetaryValues() :
#
# A utility method to find the total amount for each item in the dataset, ie, quantity * price
#
# INPUT:
#        DataFrame - data - A DataFrame object with the ecommerce data.
#
# OUTPUT : DataFrame - data - A DataFrame object with monetary field
# ************************************************

createMonetaryValues <- function(data){
  data$Monetary = data$UnitPrice * data$Quantity
  data = filter(data, data$Monetary<quantile(data$Monetary, 0.99))
  return (data)
}

# ************************************************
# sortAndReassignRecencyCluster() :
#
# A utility method to reassign recency cluster labels according to their cluster means/cluster centers
#
# INPUT:
#        DataFrame - data - A DataFrame object with rfm and rfm cluster data to make the recency cluster label more meaningful.
#
# OUTPUT : DataFrame - data - A DataFrame object with sorted cluster labels.
# ************************************************

sortAndReassignRecencyCluster <- function(data) {
  dn = aggregate(list(Center=data$Recency), by=list(RecencyCluster=data$RecencyCluster), FUN=mean)
  rownames(dn) = NULL
  dn = dn[order(-dn$Center), ]
  rownames(dn) = NULL
  dn$NewRecencyCluster = as.integer(rownames(dn))
  dn = merge(data, dn, by="RecencyCluster")
  data = subset(dn,select=c(-RecencyCluster, -Center))
  return (data)
}

# ************************************************
# sortAndReassignFrequencyCluster() :
#
# A utility method to reassign frequency cluster labels according to their cluster means/cluster centers
#
# INPUT:
#        DataFrame - data - A DataFrame object with rfm and rfm cluster data to make the frequency cluster label more meaningful.
#
# OUTPUT : DataFrame - data - A DataFrame object with sorted cluster labels.
# ************************************************

sortAndReassignFrequencyCluster <- function(data) {
  dn = aggregate(list(Center=data$Frequency), by=list(FrequencyCluster=data$FrequencyCluster), FUN=mean)
  rownames(dn) = NULL
  dn = dn[order(-dn$Center), ]
  rownames(dn) = NULL
  dn$NewFrequencyCluster = as.integer(rownames(dn))
  dn = merge(data, dn, by="FrequencyCluster")
  data = subset(dn,select=c(-FrequencyCluster, -Center))
  return (data)
}

# ************************************************
# sortAndReassignMonetaryCluster() :
#
# A utility method to reassign monetary cluster labels according to their cluster means/cluster centers
#
# INPUT:
#        DataFrame - data - A DataFrame object with rfm and rfm cluster data to make the monetary cluster label more meaningful.
#
# OUTPUT : DataFrame - data - A DataFrame object with sorted cluster labels.
# ************************************************

sortAndReassignMonetaryCluster <- function(data) {
  dn = aggregate(list(Center=data$Monetary), by=list(MonetaryCluster=data$MonetaryCluster), FUN=mean)
  rownames(dn) = NULL
  dn = dn[order(-dn$Center), ]
  rownames(dn) = NULL
  dn$NewMonetaryCluster = as.integer(rownames(dn))
  dn = merge(data, dn, by="MonetaryCluster")
  data = subset(dn,select=c(-MonetaryCluster, -Center))
  return (data)
}

# ************************************************
# sortAndReassignOverallCluster() :
#
# A utility method to reassign overall score cluster labels according to their cluster means/cluster centers
#
# INPUT:
#        DataFrame - data - A DataFrame object with rfm and rfm cluster data to make the monetary cluster label more meaningful.
#
# OUTPUT : DataFrame - data - A DataFrame object with sorted cluster labels.
# ************************************************

sortAndReassignOverallCluster <- function(data) {
  dn = aggregate(list(Center=data$OverallScore), by=list(Segment=data$Segment), FUN=mean)
  rownames(dn) = NULL
  dn = dn[order(-dn$Center), ]
  rownames(dn) = NULL
  dn$NewSegment = as.integer(rownames(dn))
  dn = merge(data, dn, by="Segment")
  data = subset(dn,select=c(-Segment, -Center))
  colnames(data)[10] = "Segment"
  data$Segment = data$Segment - 1
  return (data)
}

# ************************************************
# sortAndReassignClusters() :
#
# A utility method to wrap around the sort functions for recency, frequncy and monetary
#
# INPUT:
#        DataFrame - data - A DataFrame object with rfm and rfm cluster data.
#
# OUTPUT : DataFrame - data - A DataFrame object with sorted cluster labels for recency, frequency and monetary.
# ************************************************

sortAndReassignClusters <- function(data) {
  data = sortAndReassignRecencyCluster(data)
  data = sortAndReassignFrequencyCluster(data)
  data = sortAndReassignMonetaryCluster(data)
  colnames(data)[6:8] = list("RecencyCluster", "FrequencyCluster", "MonetaryCluster")
  return (data)
}

# ************************************************
# getRecency() :
#
# A utility method to calculate the recency of each customer.
#
# INPUT:
#        DataFrame - data - A DataFrame object with ecommerce data.
#
# OUTPUT : DataFrame - recencyData - A DataFrame object with a feature engineered field Recency
# ************************************************

getRecency <- function(data) {
  recencyData = aggregate(list(MaxPurchaseDate=data$InvoiceDate), by=list(CustomerID=data$CustomerID), FUN=function(x){max(x)})
  recencyData$Recency = max(recencyData$MaxPurchaseDate) - recencyData$MaxPurchaseDate
  return (
    recencyData
  )  
}

# ************************************************
# getFrequency() :
#
# A utility method to calculate the frequency of each customer.
#
# INPUT:
#        DataFrame - data - A DataFrame object with ecommerce data.
#
# OUTPUT : DataFrame - frequencyData - A DataFrame object with a feature engineered field Frequency
# ************************************************

getFrequency <- function(data) {
  frequencyData = aggregate(list(Frequency=data$InvoiceNo), by=list(CustomerID=data$CustomerID), FUN=function(x){length(unique(x))})
  return (frequencyData)
}

# ************************************************
# getMonetary() :
#
# A utility method to calculate the monetary value of each customer.
#
# INPUT:
#        DataFrame - recency - A DataFrame object with ecommerce data.
#
# OUTPUT : DataFrame - monetaryData - A DataFrame object with a feature engineered field Monetary.
# ************************************************

getMonetary <- function(data) {
  monetaryData = aggregate(list(Monetary=data$Monetary), by=list(CustomerID=data$CustomerID), FUN=sum)
  return (monetaryData)
}

# ************************************************
# getRFM() :
#
# A utility method to merge the recency frequency and monetary values of each customer.
#
# INPUT:
#        DataFrame - recency   - A DataFrame object with recency for each customer.
#        DataFrame - frequency - A DataFrame object with frequency for each customer.
#        DataFrame - monetary  - A DataFrame object with monetary for each customer.
#
# OUTPUT : DataFrame - rfm - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary.
# ************************************************

getRFM <- function(recency, frequency, monetary) {
  rfm = merge(recency, frequency, by="CustomerID", all.x = TRUE)
  rfm = merge(rfm, monetary, by="CustomerID", all.x = TRUE)
  return (rfm)
}

# ************************************************
# getRecencyCluster() :
#
# A utility method to calculate the recency clusters using kmeans.
#
# INPUT:
#        DataFrame - rfm   - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary values for each customer.
#
# OUTPUT : DataFrame - rfm - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary and recency cluster.
# ************************************************

getRecencyCluster <- function(rfm, clusterSize=4) {
  clusters = kmeans(rfm$Recency, clusterSize)
  rfm$RecencyCluster = clusters$cluster
  return (rfm)
}

# ************************************************
# getFrequencyCluster() :
#
# A utility method to calculate the frequency clusters using kmeans.
#
# INPUT:
#        DataFrame - rfm   - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary values for each customer.
#
# OUTPUT : DataFrame - rfm - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary and frequency cluster.
# ************************************************

getFrequencyCluster <- function(rfm, clusterSize=4) {
  clusters = kmeans(rfm$Frequency, clusterSize)
  rfm$FrequencyCluster = clusters$cluster
  return (rfm)
}

# ************************************************
# getFrequencyCluster() :
#
# A utility method to calculate the monetary clusters using kmeans.
#
# INPUT:
#        DataFrame - rfm   - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary values for each customer.
#
# OUTPUT : DataFrame - rfm - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary and monetary cluster.
# ************************************************

getMonetaryCluster <- function(rfm, clusterSize=4) {
  clusters = kmeans(rfm$Monetary, clusterSize)
  rfm$MonetaryCluster = clusters$cluster
  return (rfm)
}


# ************************************************
# getRFMClusters() :
#
# A Wrapper function for calculating each cluster
# INPUT:
#        DataFrame - rfm   - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary values for each customer.
#
# OUTPUT : DataFrame - rfm - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary and clusters of each field.
# ************************************************

getRFMClusters <- function(rfm, clusterSize=4) {
  rfm = getRecencyCluster(rfm, clusterSize)
  rfm = getFrequencyCluster(rfm, clusterSize)
  rfm = getMonetaryCluster(rfm, clusterSize)
  return (rfm)
}

# ************************************************
# calculateOverallScore() :
#
# A utility function to calculate the overall score as sum of each cluster value
# INPUT:
#        DataFrame - rfm   - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary and clusters of each field.
#
# OUTPUT : DataFrame - rfm - A DataFrame object with a feature engineered fields Recency, Frequency and Monetary and clusters of each field along with the overallscore field.
# ************************************************

calculateOverallScore <- function(rfm) {
  rfm$OverallScore = rfm$RecencyCluster + rfm$FrequencyCluster + rfm$MonetaryCluster
  return (rfm)
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

getUserSegment <- function(rfm) {
  rfm$Segment = 0
  rfm$Segment[
    (rfm$OverallScore > 5) & (rfm$OverallScore <= 10)
  ] = 1
  rfm$Segment[
    (rfm$OverallScore > 10)
  ] = 2
  return (rfm)
}