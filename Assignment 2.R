pkgs <- c("factoextra",  "NbClust","vegan")
install.packages("fpc")
install.packages("dbscan")
install.packages(pkgs)
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(readxl)
library(ggplot2)

library(dbscan)
library(fpc)
library(factoextra)
library(NbClust)
library(vegan)

#import data set
data <- read.csv("OnlineRetail.csv")
summary(data)
#create a new collumn containing total amount of money spent
data$totalSpent <- data$UnitPrice * data$Quantity
#parse dats strings so they can be interperated as numerical questions
dates <- as.character(data$InvoiceDate)
datesX <- strsplit(dates, " ")
datesX <- matrix(unlist(datesX), ncol=2, byrow=TRUE)

datesY <- strsplit(datesX[,1], "/")
datesY <- matrix(unlist(datesY), ncol=3, byrow=TRUE)

datesZ <- strsplit(datesX[,2], ":")
datesZ <- matrix(unlist(datesZ), ncol=2, byrow=TRUE)
#conjoin those intervalues as new collumns
data$month <- datesY[,1]
data$day <- datesY[,2]
data$year <- datesY[,3]
data$hour <- datesZ[,1]
data$minute <- datesZ[,2]

data$month <- as.numeric(as.character(data$month))
data$day <- as.numeric(as.character(data$day))
data$year <- as.numeric(as.character(data$year))
data$hour <- as.numeric(as.character(data$hour))
data$minute <- as.numeric(as.character(data$minute))

#with these new features more graphs could be produced will work on more after done with unsupervised learning

#number of unique orders
str(data$InvoiceNo)
summary(data$InvoiceNo)
#number of unique products
str(data$StockCode)
summary(data$StockCode)
str(data$Description)
#small discrepency between descriptions and product id/ could be special cases or faulty data ie extra space

#creating new collumns for new transactions
data$Cancelled<-ifelse(substring(data$InvoiceNo,0,1)=="C",1,0)
data$Amazon<-ifelse(data$StockCode =="AMAZONFEE",1,0)
data$BANK<-ifelse(data$StockCode =="BANK CHARGES",1,0)
data$Carriage<-ifelse(data$StockCode =="C2",1,0)
data$Commision<-ifelse(data$StockCode =="CRUK",1,0)
data$Discount<-ifelse(data$StockCode =="D",1,0)
data$DOT<-ifelse(data$StockCode =="DOT",1,0)
data$Manual<-ifelse(data$StockCode =="M",1,0)
data$post<-ifelse(data$StockCode =="POST",1,0)
data$Samples<-ifelse(data$StockCode =="SAMPLES",1,0)

#general visualizations of each feature
#DATA EXPLORATION HERE 

summary(data$Description)
#you can see the the most popular items are listed first
str(data$Description)
summary(data$Quantity)
table(data$Quantity)
summary(data$UnitPrice)

summary(data$Country)
summary(data$totalSpent)
table(data$month,data$day)
table(data$year,data$month)

#example of a table you can print to see which products are popular depending on the month
table(data$Description,data$month)
#a few more examples
table(data$Country,data$hour)

#the lie below allows for the full set to be printed out
options(max.print=10000)
#here you can get an image of the most popular products

#visualization of unique transactions
#they take up only a small portion of transactions in the dataset

table(data$Cancelled)
table(data$Amazon)
table(data$BANK)
table(data$Carriage)
table(data$Commision)
table(data$Commision)
table(data$Discount)
table(data$DOT)
table(data$Manual)
table(data$post)
table(data$Samples)

summary(data$Quantity)
summary(data$UnitPrice)
#graph

#view number of cancelled transactions
table(data$Cancelled)

#missing data
is.na(data) = data==""
sum(is.na(data))
#sum of rows missing from data
mean(is.na(data))



#this is where i start preparing the data to be used by k means
#erase those rows
data = na.omit(data)
#customerID 135080 are missing
#productdescription 1000 are missing

#data cleanup for invalid transactions
data<-data[!(data$Amazon=="1" & data$BANK=="1" &data$Cancelled=="1" &data$Carriage=="1" &data$Commision=="1" &data$Discount=="1" &data$DOT=="1" &data$Manual=="1" &data$post=="1" &data$Samples=="1"),]

data$CustomerID <- as.factor(data$CustomerID)
str(data$CustomerID)


OrderTable <- levels(data$InvoiceNo)
OrderTable <- cbind(OrderTable)
OrderTable <- as.data.frame(as.table(OrderTable))
OrderTable$Var1 <- NULL
OrderTable$Var2 <- NULL
OrderTable[,"AverageNumofProd"] <- NA

for( i in rownames(OrderTable) ){
  for( j in rownames(data)){
    if(as.character(OrderTable[i,"Freq"])==as.character(data[j,"InvoiceNo"])){
      if(is.na(OrderTable[i,"AverageNumofProd"])){
        OrderTable[i,"AverageNumofProd"] <- 1
      }
      else{
        OrderTable[i,"AverageNumofProd"] <- OrderTable[i,"AverageNumofProd"]+1
      }
    }
  }
}

Customers <- levels(data$CustomerID)
Customers <- cbind(Customers)
Customers <- as.data.frame(as.table(Customers))
Customers$Var1 <- NULL
Customers$Var2 <- NULL
Customers[,"AmountSpent"] <- NA
for( i in rownames(Customers) ){
  for( j in rownames(data)){
    if(as.character(Customers[i,"Freq"])==as.character(data[j,"CustomerID"])){
      if(is.na(Customers[i,"AmountSPent"])){
        Customers[i,"AmountSpent"] <- data[j,"totalSpent"]
      }
      else{
        Customers[i,"AmountSpent"] <- (Customers[i,"totalSpent"]+data[j,"totalSpent"])/2
      }
    }
  }
}
summary(Customers$AmountSpent)



#adjust dataset so that it can be interperated by unsupervised learning algorithms
data$InvoiceNo <- NULL
data$StockCode<-NULL
data$Description<-NULL
data$InvoiceDate<-NULL
data$Country<-NULL
data$CustomerID<-NULL
data$year<-data$year-2010
data$totalSpent<-NULL
data$Cancelled <- NULL
data$Amazon <- NULL
data$BANK<-NULL
data$Carriage<-NULL
data$Commision<-NULL
data$Discount<-NULL
data$DOT<-NULL
data$Manual<-NULL
data$post<-NULL
data$Samples<-NULL

#deal with outliers
data$UnitPrice<-ifelse(data$UnitPrice < 0,-50,data$UnitPrice)
data$Quantity<-ifelse(data$Quantity < 0,-50,data$Quantity)

#determine number of clusters
model <- cascadeKM(data, 1, 10, iter = 10)#takes to long to run with more iters dont bother
model$results[2,]
#plot this following line last takes to long to generate plot
#problems seem to arise since the data set is so big R studio doesnt seem to handle all the memory
plot(model, sortg = TRUE)

#recommends which number of clusters to use, outputs 3
which.max(model$results[2,])
#we're going with 2 clusters
kresult <- kmeans(data,3)#3 if missing data is omiited #2if data wasnt omitted
kresult$size
#see the visualization of the spread of clusters
#output not that promising will try other algos
#add clusters as a collumn corresponding to each transaction
data$kresult <- kresult$cluster

kresult <- kmeans(data$month, 4)
kresult$size

#other algorithms doesnt work because the data set it to big quotes the vector size is too big
hclustresult <- hclust(dist(data))
dbscanresult<- dbscan(data, .15)