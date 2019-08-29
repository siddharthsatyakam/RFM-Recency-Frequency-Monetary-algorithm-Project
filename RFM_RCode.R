install.packages("data.table")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lattice")
install.packages("tidyr")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("rmarkdown")
install.packages("misc")
install.packages("ape")

setwd("D:\\SubjectDetails\\StatisticalDataMining\\Sem2Project\\")

library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)
library(rmarkdown)
library(lattice)
library(ape)
library(plotrix)

## Loading of the Data of the UK Retailer:

Initial_data <- read.csv(file = "preproccessedData.csv",header = T,sep = ",")
head(Initial_data)

Initial_data <- as.data.frame.data.frame(Initial_data)

Initial_data <- Initial_data %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))

Initial_data <- Initial_data %>% 
  mutate(total_dolar = Initial_data$Quantity*Initial_data$UnitPrice)
head(Initial_data)

## To have a much more better idea about the data we would separate the data based on countries first

cont_data=split(Initial_data,f=Initial_data$Country)

head(cont_data[[1]])  ## first country is Australia

cont_RFM <- Initial_data %>% 
  group_by(Country) %>% 
  summarise(recency=as.numeric(as.Date("2019-01-01")-max(InvoiceDate)),
            freq=n_distinct(InvoiceNo), montery= sum(total_dolar)/n_distinct(InvoiceNo)) 

head(cont_RFM)

locax=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37)
  # Now having the data of country wise we can draw a inference as what and how can we promote more goods in the countries

  # Recency:
    plot(cont_RFM$Country,cont_RFM$recency,xaxt='n')
    axis(1, at=locax, labels=cont_RFM$Country, tck=.01, cex.axis=0.9, srt=90, col.ticks = NULL, las=2)
    
  # Frequency:
    plot(cont_RFM$Country,cont_RFM$freq,xaxt='n')
    axis(1, at=locax, labels=cont_RFM$Country, tck=.01, cex.axis=0.9, srt=90, col.ticks = NULL, las=2)
    
  # Monetary:
    plot(cont_RFM$Country,cont_RFM$montery,xaxt='n')
    axis(1, at=locax, labels=cont_RFM$Country, tck=.01, cex.axis=0.9, srt=90, col.ticks = NULL, las=2)
    
##Joint analysis based on all the 3 qualities webring all the 3 factors together based on the clustering:
  
    ### Hierarchical Clustering
    cont_RFM_clust <- cont_RFM
    row.names(cont_RFM_clust) <- cont_RFM_clust$Country
    cont_RFM_clust$Country <- NULL
    
    cont_RFM_clust <- scale(cont_RFM_clust)
    summary(cont_RFM_clust)
    cont_RFM_clust <- as.data.frame.matrix(cont_RFM_clust)
    
    clust_data <- hclust(dist(cont_RFM_clust), method = 'ward.D2')
    
    plot(clust_data, type = "triangle")
    
    ## Tree Pruning depending on our requirement as how many divisions we want or how our company wants to invest wheather they have say 7 divisions of budget we make the tree pruning accordingly giving the exact requirement of the 7 groups:
    
    clust_prun_data <- cutree(clust_data,k = 7)
    
    ##Total Divisions:
    table(clust_prun_data)
    
    final_clusts <- aggregate(cont_RFM_clust,by=list(clust_prun_data),mean)
    
    
    
## Now say I want to find a much more detailed description into a countries details,so as to make plan for the country:
    
    cont_RFM_Aus <- cont_data[[1]] %>% 
      group_by(Month) %>% 
      summarise(recency=as.numeric(as.Date("2019-01-01")-max(InvoiceDate)),
                freq=n_distinct(InvoiceNo), montery= sum(total_dolar)/n_distinct(InvoiceNo)) 
    
    
    locax=c(1,2,3,4,5,6,7,8,9,10,11,12)
    # Now having the data of country wise we can draw a inference as what and how can we promote more goods in the countries
    
    # Recency:
    plot(cont_RFM_Aus$Month,cont_RFM_Aus$recency,xaxt='n')
    axis(1, at=locax, labels=cont_RFM_Aus$Month, tck=.01, cex.axis=0.9, srt=90, col.ticks = NULL, las=2)
    
    # Frequency:
    plot(cont_RFM_Aus$Month,cont_RFM_Aus$freq,xaxt='n')
    axis(1, at=locax, labels=cont_RFM_Aus$Month, tck=.01, cex.axis=0.9, srt=90, col.ticks = NULL, las=2)
    
    # Monetary:
    plot(cont_RFM_Aus$Month,cont_RFM_Aus$montery,xaxt='n')
    axis(1, at=locax, labels=cont_RFM_Aus$Month, tck=.01, cex.axis=0.9, srt=90, col.ticks = NULL, las=2)
    
    ##Joint analysis based on all the 3 qualities webring all the 3 factors together based on the clustering:
    
    ### Hierarchical Clustering
    cont_RFM_clust_Aus <- cont_RFM_Aus
    row.names(cont_RFM_clust_Aus) <- cont_RFM_clust_Aus$Month
    cont_RFM_clust_Aus$Month <- NULL
    
    cont_RFM_clust_Aus <- scale(cont_RFM_clust_Aus)
    summary(cont_RFM_clust_Aus)
    cont_RFM_clust_Aus <- as.data.frame.matrix(cont_RFM_clust_Aus)
    
    clust_data_Aus <- hclust(dist(cont_RFM_clust_Aus), method = 'ward.D2')
    
    plot(clust_data_Aus, type = "triangle")
    
    ## Tree Pruning depending on our requirement as how many divisions we want or how our company wants to invest wheather they have say 7 divisions of budget we make the tree pruning accordingly giving the exact requirement of the 7 groups:
    
    clust_prun_data_Aus <- cutree(clust_data_Aus,k = 6)
    
    ##Total Divisions:
    table(clust_prun_data_Aus)
    
    final_clusts_Aus <- aggregate(cont_RFM_clust_Aus,by=list(clust_prun_data_Aus),mean)
    
    
    ## Now we will study how to use the RFM data of the clusters in the Australia data that we found because its one of the states to have the maximum RFM, so we can increase our sales in it
    
    AusMons <- split(cont_data[[1]],f = cont_data[[1]]$Month)
    
    head(AusMons)
    
    AusMons[[9]]
    
    Aus_May_RFM <-  AusMons[[9]] %>% 
      group_by(WeekDay) %>% 
      summarise(recency=as.numeric(as.Date("2019-01-01")-max(InvoiceDate)),
                freq=n_distinct(InvoiceNo), montery= sum(total_dolar)/n_distinct(InvoiceNo)) 
    
    ### Hierarchical Clustering
    Aus_May_clust <- Aus_May_RFM
    row.names(Aus_May_clust) <- Aus_May_clust$WeekDay
    Aus_May_clust$WeekDay <- NULL
    
    Aus_May_clust <- scale(Aus_May_clust)
    summary(Aus_May_clust)
    Aus_May_clust <- as.data.frame.matrix(Aus_May_clust)
    
    clust_data_Aus_May <- hclust(dist(Aus_May_clust), method = 'ward.D2')
    
    plot(clust_data_Aus_May, type = "triangle")
    
    ## Tree Pruning depending on our requirement as how many divisions we want or how our company wants to invest wheather they have say 7 divisions of budget we make the tree pruning accordingly giving the exact requirement of the 7 groups:
    
    clust_prun_data_Aus_May <- cutree(clust_data_Aus_May,k = 3)
    
    ##Total Divisions:
    table(clust_prun_data_Aus)
    
    final_clusts_Aus_May <- aggregate(Aus_May_RFM,by=list(clust_prun_data_Aus_May),mean)
    
    
    ###Now customer segmentation on the data of the Aus_may
 
      Aus_May_cust_RFM <-  AusMons[[9]] %>% 
      group_by(CustomerID) %>% 
      summarise(recency=as.numeric(as.Date("2019-01-01")-max(InvoiceDate)),
                freq=n_distinct(InvoiceNo), montery= sum(total_dolar)/n_distinct(InvoiceNo)) 
    
    View(Aus_May_cust_RFM)
    
    
    #### SO Here diving into the RFM concept in Australlia we saw that we can have maximum sales in Australia in MAy and MArch and more specifically in MAy in Mon and FriDAys where we can push our most dangerous ,costly and luxury goods for people to pick up like sheeps.
    #### aND WE Also saw the customer segmentation over the strech and were able tpo get a idea how it works.