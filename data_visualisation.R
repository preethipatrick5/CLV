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
# 1.00 	    14/04/2021 	- 	plot missing values, invoice by country distribution, customer purchase distribution, purchase frequency, monthly purchase distribution
# 1.01 	    15/04/2021 	- 	plots to check outliers in price * quantity
# 1.02 	    16/04/2021 	- 	plots to analyze montly sales
# 1.03 	    22/04/2021 	- 	Plots to visualize Recency, Frequency and Monetary
# ************************************************

# Libraries that are used for the visualisation
source('processor.R')

# ************************************************
# visualizeBefore() :
#
# A wrapper function to plot various graphs before feature engineering to understand the dataset.
# INPUT:
#        DataFrame - dataset   - A DataFrame object of our ecommerce dataset.
#        String    - color     - The base color of all the graphs
#
# ************************************************

#https://www.r-graph-gallery.com/barplot.html
#https://rpubs.com/sheng0628/273249

visualizeBefore <- function(dataset, color="#69b3a2") {
  # Converting invoice date to date (dd/mm/yy) format
  dataset$InvoiceDate <- as.Date(dataset$InvoiceDate, "%d/%m/%Y")
  
  # Taking year and month from invoice date
  dataset$InvoiceDate <- as.Date(dataset$InvoiceDate, "%Y%m")
  
  # Taking year and month as seperate columns from invoice date
  dataset$year <- format(dataset$InvoiceDate, "%Y")
  dataset$month <- format(dataset$InvoiceDate, "%m")
  
  
  # Plotting missing data in the dataset
  md.pattern(dataset, plot = TRUE, rotate.names = TRUE)
  
  
  # counting unique invoices in each country
  invoiceDistribution <- count(dataset[!duplicated(dataset$InvoiceNo), ], Country)
  
  # plotting out the unique invoices in each country
  plotUniqueInvoices <- ggplot(data = invoiceDistribution, aes(x = Country, y = n)) + geom_bar(stat = "identity", fill = color) + coord_flip() + ylab("Number of invoices") + xlab("Countries") + ggtitle("Number of invoices in a country")
  print(plotUniqueInvoices)
  
  # filtering out UK data
  ukData <- filter(dataset, Country=="United Kingdom")
  
  # finding total amount i.e, unit price * quantity
  ukData["total_amount"] <- ukData$UnitPrice * ukData$Quantity
  
  # invoice amount of each invoice of each customer
  invoiceAmountDistribution <- aggregate(ukData$total_amount, by=list(InvoiceID=ukData$InvoiceNo, CustomerNo=ukData$CustomerID), FUN=sum)
  
  # customer purchase amount for a year
  customerPurchaseDistribution <- aggregate(invoiceAmountDistribution$x, by=list(CustomerNo=invoiceAmountDistribution$CustomerNo), FUN=sum)
  
  # plotting customer purchase distribution
  customerPurchase <- customerPurchaseDistribution %>%
    ggplot( aes(x=x)) +
    geom_histogram( binwidth=10000, fill=color, color="blue", alpha=0.9) +
    ggtitle("Customer Purchase distribution") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=15)
    )
  print(customerPurchase)

  # plotting customer purchase distribution with amount less than 20000
  customerPurchaseFill <- customerPurchaseDistribution %>%
    filter(x<20000) %>%
    ggplot( aes(x=x)) +
    geom_histogram( binwidth=500, fill=color, color="blue", alpha=0.9) +
    ggtitle("Customer Purchase distribution with amount < 20000") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=15)
    )
  print(customerPurchaseFill)

  # Average purchase frequency of a customer
  purchaseCount <- count(invoiceAmountDistribution, CustomerNo)
  purchaseCount$n <- purchaseCount$n / 12
  # Plotting the purchase frequency of a customer
  avgPurchaseFrequency <- purchaseCount %>%
    ggplot( aes(x=n)) +
    geom_histogram( binwidth=2, fill=color, color="blue", alpha=0.9) +
    ggtitle("Average Purchase Frequency") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=15)
    )
  print(avgPurchaseFrequency)

  # Plotting the purchase frequency of a customer with frequency < 5
  avgPurchaseFrequencyFill <- purchaseCount %>% filter(n<5) %>%
    ggplot( aes(x=n)) +
    geom_histogram( binwidth=1, fill=color, color="blue", alpha=0.9) +
    ggtitle("Average Purchase Frequency with n < 5") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=15)
    )
  print(avgPurchaseFrequencyFill)

  # Plotting the purchase frequency of a customer with frequency > 5
  avgPurchaseFrequencyFill <- purchaseCount %>% filter(n>5) %>%
    ggplot( aes(x=n)) +
    geom_histogram( binwidth=1, fill=color, color="blue", alpha=0.9) +
    ggtitle("Average Purchase Frequency with n > 5") +
    theme_ipsum() +
    theme(
      plot.title = element_text(size=15)
    )
  print(avgPurchaseFrequencyFill)


  # monthly purchase ditribution
  purchaseDistributionDF <- ukData[!duplicated(ukData$InvoiceNo), ]
  purchaseDistribution <- count(purchaseDistributionDF, month)
  # Plotting the monthly purchase distribution of the firm
  barplot(height=purchaseDistribution$n, names=purchaseDistribution$month , xlab = "Months", ylab = "Purchases", main = "Monthly purchase distribution", col=color )


  ## boxplot of Amount
  boxplot(ukData$total_amount, ylab="Total amount", main = "Boxplot of revenue from the purchases")

  #cutoff outliers
  amount <- subset(ukData,ukData$total_amount>= 0 & ukData$total_amount<= 10000 )
  boxplot(amount$total_amount, ylab="Total amount", main = "Boxplot of revenue from the purchases")


  #top selling products
  topSell <- ddply(amount, .(StockCode, Description), summarize, sumAmount= sum(total_amount), sumQuantity= sum(Quantity), nCustomer= length(unique(CustomerID)), nPurchase= length(unique(InvoiceNo)) )
  print(head(topSell[order(-topSell$sumQuantity),] ))
  print(head(topSell[order(-topSell$nCustomer),] ))

  #Top 5 selling products
  topProduct <- subset(ukData, Description%in%c("MEDIUM CERAMIC TOP STORAGE JAR","JUMBO BAG RED RETROSPOT","REGENCY CAKESTAND 3 TIER","WHITE HANGING HEART T-LIGHT HOLDER","PARTY BUNTING","WORLD WAR 2 GLIDERS ASSTD DESIGNS"), select = c(Description,InvoiceDate,month,year,Quantity,CustomerID,total_amount,InvoiceNo))
  topProduct$Decription<-as.character(topProduct$Description)


  #Sales by month of each product with respect to the quantity
  salesByMonthQuantity <- ggplot(topProduct, aes(x=month, y= Quantity))+ facet_wrap(~Description, ncol=2) +
    geom_bar(stat="identity",fill=color) +
    labs(title = "Monthly sales w.r.t Quantity", x = "Months", y = "Quantity")
  print(salesByMonthQuantity)


  #Sales by month of each product with respect to the customer ID
  salesByMonthCustomerID <- ggplot(topProduct, aes(x=month, y= length(unique(CustomerID)) )) + facet_wrap(~Description, ncol=2) +
    geom_bar(stat="identity", fill=color) +
    labs(title = "Monthly sales w.r.t Customer ID", x = "Months", y = "Customer ID")
  print(salesByMonthCustomerID + coord_flip())


  #Sales by month of each product with respect to the revenue
  salesByMonthRevenue <- ggplot(topProduct, aes(x=month, y= total_amount )) + facet_wrap(~Description, ncol=2) +
    geom_bar(stat="identity", fill=color) +
    labs(title = "Monthly sales w.r.t Revenue", x = "Months", y = "Revenue")
  print(salesByMonthRevenue)


  #Sales by month of each product with respect to the purchases
  salesByMonthPurchase <- ggplot(topProduct, aes(x=month, y= length(unique(InvoiceNo)) )) + facet_wrap(~Description, ncol=2) +
    geom_bar(stat="identity", fill=color) +
    labs(title = "Monthly Sales w.r.t Purchases", x = "Months", y = "Number of Product Purchases")
  print(salesByMonthPurchase + coord_flip())
}

# ************************************************
# visualizeRFM() :
#
# A wrapper function to plot the rfm values to understand the distribution of customers.
# INPUT:
#        DataFrame - dataset   - A DataFrame object withe feature engineered recency, frequency and monetary columns.
#        String    - title     - Additional information to be included in the graph
#        String    - color     - Base color of the graph
#
# ************************************************

visualizeRFM <- function(data, title, color="#69b3a2") {
  
  d <- density(data$Recency)
  plot(d, main=paste(title, " : Recency"))
  polygon(d, col=color, border="black")
  print(d)
  e <- density(data$Frequency)
  plot(e, main=paste(title, " : Frequency"))
  polygon(e, col=color, border="black")
  print(e)
  f <- density(data$Monetary)
  plot(f, main=paste(title, " : Monetary"))
  polygon(f, col=color, border="black")
  print(f)
  p <- fviz_nbclust(as.matrix(data[,c("Monetary")]), kmeans, method = "wss")
  print(p)
}

# ************************************************
# visualizeClusters() :
#
# A utitlity function to visualize the clustering of RFM values in the dataset.
# INPUT:
#        DataFrame - dataset   - A DataFrame object with feature engineered recency, frequency and monetary columns.
#        String    - title     - Additional information to be included in the graph
#        String    - color     - Base color of the graph
#
# ************************************************

visualizeClusters <- function(data, title, clusterSize=4){
  data = data[,c("Recency", "Frequency", "Monetary")]
  clusters = kmeans(data$Monetary, clusterSize)
  # plot <- fviz_cluster(clusters, data = data, main = paste(title, " : Cluster plot"),
  #              palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#D7B800"), 
  #              geom = "point",
  #              ellipse.type = "convex", 
  #              ggtheme = theme_bw()
  # )
  plot <- plotcluster(data, clusters$cluster)
  print(plot)
}


# ************************************************
# visualizeMonthlyMetric() :
#
# A utitlity function to visualize the monthly metrics.
# INPUT:
#        DataFrame - dataset   - A DataFrame object with ecommerce dataset.
#        String    - title     - Additional information to be included in the graph
#        String    - color     - Base color of the graph
#
# ************************************************

visualizeMonthlyMetric <- function(data) {
  
  # Plot for number of cusomers per month
  custmonthplot <- data %>% group_by(YearMonth) %>% summarise(count =n_distinct(CustomerID)-1)
  custmonthplot  <- custmonthplot[1:(nrow(custmonthplot['YearMonth'])-1),]
  plot(custmonthplot$count,type = "o", col = "blue", xlab = "Month", ylab = "Customer Count", main = "Number of Customers per Month")
  
  
  # Plot for number of invoices per month
  invoicemonthplot <- data %>% group_by(YearMonth) %>% summarise(count =n_distinct(InvoiceNo)-1)
  invoicemonthplot <- invoicemonthplot [1:(nrow(invoicemonthplot['YearMonth'])-1),]
  plot(invoicemonthplot$count,type = "o", col = "blue", xlab = "Month", ylab = "Invoice Count", main = "Number of Invoice per Month")
  
  # Plot for monthy quantity
  ordermonthlyplot <- data  %>%group_by(YearMonth) %>% summarise(count =sum(Quantity))
  ordermonthlyplot <- ordermonthlyplot[1:(nrow(ordermonthlyplot['YearMonth'])-1),]
  plot(ordermonthlyplot$count,type = "o", col = "blue", xlab = "Month", ylab = "Quantity", main = "Monthly Quantity")
  
}


# ************************************************

# generateHyperTuneTable() :

#

# A utitlity function to view the result of hypertuning of model parameters.

# INPUT:

#        DataFrame - modeldf      - A model object which contains the model result.

#        String    - modelName    - Name of the model

#        String    - beforeValue  - Column before which the modelname is to be inserted.

#

# ************************************************

generateHyperTuneTable <- function(modeldf,modelName,beforeValue){
  
  modelresults <- data.frame()
  
  modelresults <-rbind(modelresults,modeldf$results,make.row.names=FALSE)
  
  modelresults <-add_column(modelresults, ModelName = modelName, .before = beforeValue)
  
  t <- formattable::formattable(modelresults)
  
  print(t)
  
}