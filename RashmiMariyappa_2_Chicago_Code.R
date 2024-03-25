#Rashmi Mariyappa
#Statistical Models + Methods Class
#Final Project


#########################################
#Load Libraries
library(tree)
library(randomForest)
library(ggplot2)
library(tidyverse)
library(class)

#########################################
#Quick Access Command

#Remove All Objects
rm(list=ls())

#Plot Margins
par(mfrow=c(1,1), mar=c(4,6,2.5,6))

#########################################
#Functions

#Range Transform Zero - One Scale
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#########################################
#Read in File

#Chicago Community Original Data Set
chicagoCom <- read.csv('C:/Users/rmari/...ChicagoCommunity77.csv', header = TRUE, sep = ",")

#Building Permit Data Set
buildingPerm <- read.csv('C:/Users/rmari/...BuildingPermitsFINAL.csv', header = TRUE, sep = ",")

str(chicagoCom)
str(buildingPerm)

#Check for Missing Values
cc_check<-na.omit(chicagoCom) #check for missing values - none
rm(cc_check)


#########################################
#CLUSTER ANALYSIS - RESEARCH QUESTION 1
#########################################

#________________________________________
#Cluster by 311 Requests
#________________________________________

#Get Subset of 311 Data
c311 <- chicagoCom[,c(16,17,18,19)]

#Transform Data to 0 - 1 Range
S311Rat  <- range01(c311$R311Rat)
S311Graf <- range01(c311$R311Graf)
S311Cart <- range01(c311$R311Cart)
S311Vac  <- range01(c311$R311Vac)
rm(S311Rat,S311Graf,S311Cart,S311Vac)

#Make New Subset of Variables
sc311 <- as.data.frame(cbind(S311Rat, S311Graf, S311Cart, S311Vac))


#----------------------------------------------------
#311 K-Means
#----------------------------------------------------
#Scree Plot to Determine Number of Clusters
par(mfrow=c(1,1), mar=c(4,6,2.5,6))

mydata <- sc311
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", pch=19, col="tomato", xlab="Number of Clusters",
     ylab="Within Groups Sum of Squares")
#----------------------------------------------------

#Result of Plot Best Cluster at 4, Possibly 8


#Test a Few Cluster Assignment
set.seed(3)

#K=2
km2_311 <- kmeans(sc311, 2, nstart = 20)
km2_311$cluster
km2_311


#K=3
km3_311 <- kmeans(sc311, 3, nstart = 20)
km3_311$cluster
km3_311

#K=4
km4_311 <- kmeans(sc311, 4, nstart =100)
km4_311$cluster
km4_311$centers
km4_311$tot.withinss
km4_311
table(km4_311$cluster)

#K=5
km5_311 <- kmeans(sc311, 5, nstart = 20)
km5_311$cluster
km5_311

#K=6
km6_311 <- kmeans(sc311, 6, nstart = 20)
km6_311$cluster
km6_311

#K=7
km7_311 <- kmeans(sc311, 7, nstart = 20)
km7_311$cluster
km7_311

#K=8
km8_311 <- kmeans(sc311, 8, nstart = 20)
km8_311$cluster
km8_311

#----------------------------------------------------
#K-Means Cluster Analysis
#append cluster assignment
sc311 <- data.frame(sc311, km4_311$cluster)
chicagoCom3 <- data.frame(chicagoCom, km4_311$cluster)

#Aggregate Values by Cluster for Analysis
summaryk4311_N <- aggregate(chicagoCom3, by=list(km4_311$cluster), FUN=mean)

##Save Aggregate Values to CSV
write.csv(summaryk4311, file="C:/Users/rmari/...311Agg.csv")
write.csv(summaryk4311_N, file="C:/Users/rmari/...311AggNew.csv")
write.csv(k4311means, file="C:/Users/rmari/...311means4.csv")
write.csv(chicagoCom3, file="C:/Users/rmari/...NEWtotwithclusters.csv")

#----------------------------------------------------
#Make centroid Plot
k4311means <- as.data.frame(km4_311$center)
#(ggk4 <- ggplot(data = k4311means) + geom_line(mapping = aes(x = 1)) + scale_fill_manual(values=c("cyan3"))+ theme(legend.position="none") + labs(x = "Degree", y= "Frequency"))
#Not Working Made Chart in Excel


#----------------------------------------------------
#311 Hierarchical Agglomerative
#----------------------------------------------------
?hclust
ha_com311 =hclust(dist(sc311), method="complete")
ha_ave311 =hclust(dist(sc311), method="average")
ha_sin311 =hclust(dist(sc311), method="single")

plot(ha_com311 ,main=NA, xlab="", sub="", cex=.9)
plot(ha_ave311 ,main="Average Linkage ", xlab="", sub="", cex=.9)   #bad
plot(ha_sin311 ,main="Single Linkage ", xlab="", sub="", cex=.9)    #bad

test <- cutree(ha_com311 , 5)
table(test)

#________________________________________
#Cluster by Green Variables Requests
#________________________________________

#Get Subset of Green Variables Data
cGreen <- chicagoCom[,c(11,13,14,15)]
#c2Green <- chicagoCom[,c(3,11,15)]
#rm(c2Green)

#Transform Data to 0 - 1 Range
SElecUse  <- range01(cGreen$ElecUse)
SBlueSta <- range01(cGreen$BlueSta)
SRedSta <- range01(cGreen$RedSta)
SWalkScore  <- range01(cGreen$WalkScore)
SDist <- range01(c2Green$Dist)
SElecUse <- range01(c2Green$ElecUse)
SWalkScore <- range01(c2Green$WalkScore)
#rm(SDist, SElecUse, SWalkScore )
#rm(SElecUse, SBlueSta, SRedSta, SWalkScore)

#Make New Subset of Variables
scGreen <- as.data.frame(cbind(SElecUse, SBlueSta, SRedSta, SWalkScore))
#newGreen <- scale(cGreen)
#rm(newGreen)

#----------------------------------------------------
#Green K-Means
#----------------------------------------------------
#Scree Plot to Determine Number of Clusters
par(mfrow=c(1,1), mar=c(4,6,2.5,6))

mydata <- scGreen
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", pch=19, col="tomato", xlab="Number of Clusters",
     ylab="Within Groups Sum of Squares")
#----------------------------------------------------
#Result of Plot Best Cluster at 3- 6

#Test Cluster Assignments 3 through 6
set.seed(3)

#K=3
km3_green <- kmeans(scGreen, 3, nstart = 100)
km3_green$cluster
km3_green
km3_green$centers
km3_green$tot.withinss

#K=4
km4_green <- kmeans(scGreen, 4, nstart = 100)
km4_green$cluster
km4_green

#K=5
km5_green <- kmeans(scGreen, 5, nstart =100)
km5_green$cluster
km5_green

#K=6
km6_green <- kmeans(scGreen, 6, nstart = 100)
km6_green$cluster
km6_green
km6_green$centers
km6_green$tot.withinss
table(km6_green$cluster)


#----------------------------------------------------
#K-Means Cluster Analysis Green
#append cluster assignment
scGreen_clust <- data.frame(scGreen, km6_green$cluster)
chicagoComGreen <- data.frame(chicagoCom, km6_green$cluster)

#Aggregate Values by Cluster for Analysis
summary_K6Green <- aggregate(chicagoComGreen, by=list(km6_green$cluster), FUN=mean)

##Save Aggregate Values to CSV
write.csv(summary_K6Green, file="C:/Users/rmari/...GreenAgg.csv")
write.csv(k6GreenMeans, file="C:/Users/rmari/...GreenMeans.csv")
write.csv(chicagoComGreen, file="C:/Users/rmari/...GreenAllwithCluster.csv")

#----------------------------------------------------
#Make centroid Plot
k6GreenMeans <- as.data.frame(km6_green$center)
#(ggk4 <- ggplot(data = k4311means) + geom_line(mapping = aes(x = 1)) + scale_fill_manual(values=c("cyan3"))+ theme(legend.position="none") + labs(x = "Degree", y= "Frequency"))



#----------------------------------------------------
#Green Hierarchical Agglomerative
#----------------------------------------------------
?hclust
ha_comGreen =hclust(dist(scGreen), method="complete")
ha_aveGreen =hclust(dist(scGreen), method="average")
ha_sinGreen =hclust(dist(scGreen), method="single")

plot(ha_comGreen ,main=NA, xlab="Complete Linkage", sub="", cex=.9)
plot(ha_aveGreen ,main="Average Linkage ", xlab="", sub="", cex=.9)   
plot(ha_sinGreen ,main="Single Linkage ", xlab="", sub="", cex=.9)    

test <- cutree(ha_sinGreen , 4)
table(test)





#########################################
#BUILDING PERMITS - RESEARCH QUESTION 2
#########################################

table(buildingPerm$PERMIT_TYP)

#________________________________________
#Set Up the Data

#Merge Community and Building Data Set
CommBuildPerm <-  merge(buildingPerm, chicagoCom, by = "ComNo")

#Build Data Subset
randomBuild <- trainDNBP

#Check if Label is Number or Factor
str(CommBuildPerm)

#Convert to Factor & Check if Factor
CommBuildPerm$PERMIT_TYP <- as.factor(CommBuildPerm$PERMIT_TYP)
str(CommBuildPerm)

#Break Into Traing and Test Data 
set.seed(1234)		#Generate Random Number
trainIndex <- sample(1:nrow(CommBuildPerm), size = 0.7*nrow(CommBuildPerm))

#Train Dataset
trainDNBP <- CommBuildPerm[trainIndex,]
# Test Dataset
testDNBP <- CommBuildPerm[-trainIndex,] 

str(trainDNBP)
str(testDNBP)

attach(trainDNBP) 


#----------------------------------------------------
##Build Random Forest Model
#----------------------------------------------------

#________________________________________
#With 311 Requests
#Create Random Forest for m = 1 - 16
rf1_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=5, importance = TRUE)

rf2_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=10, ntree=1000,importance = TRUE)
 
rf3_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=1, ntree=1000,importance = TRUE)

rf4_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=2, ntree=1000,importance = TRUE)

rf5_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=3, ntree=1000,importance = TRUE)

rf6_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=4, ntree=1000,importance = TRUE)

rf7_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=6, ntree=1000,importance = TRUE)

rf8_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=8, ntree=1000,importance = TRUE)

rf9_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=12, ntree=1000,importance = TRUE)

rf10_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=14, ntree=1000,importance = TRUE)

rf11_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                  ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=16, ntree=1000,importance = TRUE)


#________________________________________
#Without 311 Requests
#Create Random Forest for m = 1 - 12

rfw1_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=1, ntree=1000, importance = TRUE)

rfw2_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=2, ntree=1000, importance = TRUE)

rfw3_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=4, ntree=1000, importance = TRUE)

rfw4_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=6, ntree=1000, importance = TRUE)

rfw5_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=8, ntree=1000, importance = TRUE)

rfw6_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=10, ntree=1000, importance = TRUE)

rfw7_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=12, ntree=1000, importance = TRUE)


#----------------------------------------------------
#Check Error

#________________________________________
#With 311 Requests
#M=5
rf1_build_ptrain <- predict(rf1_tree_build, newdata=trainDNBP)
rf1_build_ptest <- predict(rf1_tree_build, newdata=testDNBP)
tab_train <-table(rf1_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf1_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%
tab_test
table(rf1_build_ptest)

#M=10
rf2_build_ptrain <- predict(rf2_tree_build, newdata=trainDNBP)
rf2_build_ptest <- predict(rf2_tree_build, newdata=testDNBP)
tab_train <-table(rf2_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf2_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=1
rf3_build_ptrain <- predict(rf3_tree_build, newdata=trainDNBP)
rf3_build_ptest <- predict(rf3_tree_build, newdata=testDNBP)
tab_train <-table(rf3_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf3_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=2
rf4_build_ptrain <- predict(rf4_tree_build, newdata=trainDNBP)
rf4_build_ptest <- predict(rf4_tree_build, newdata=testDNBP)
tab_train <-table(rf4_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf4_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=3
rf5_build_ptrain <- predict(rf5_tree_build, newdata=trainDNBP)
rf5_build_ptest <- predict(rf5_tree_build, newdata=testDNBP)
tab_train <-table(rf5_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf5_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%


#M=4
rf6_build_ptrain <- predict(rf6_tree_build, newdata=trainDNBP)
rf6_build_ptest <- predict(rf6_tree_build, newdata=testDNBP)
tab_train <-table(rf6_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf6_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=6
rf7_build_ptrain <- predict(rf7_tree_build, newdata=trainDNBP)
rf7_build_ptest <- predict(rf7_tree_build, newdata=testDNBP)
tab_train <-table(rf7_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf7_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%


#M=8
rf8_build_ptrain <- predict(rf8_tree_build, newdata=trainDNBP)
rf8_build_ptest <- predict(rf8_tree_build, newdata=testDNBP)
tab_train <-table(rf8_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf8_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%


#M=12
rf9_build_ptrain <- predict(rf9_tree_build, newdata=trainDNBP)
rf9_build_ptest <- predict(rf9_tree_build, newdata=testDNBP)
tab_train <-table(rf9_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf9_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)  #18.349%

#M=14
rf10_build_ptrain <- predict(rf10_tree_build, newdata=trainDNBP)
rf10_build_ptest <- predict(rf10_tree_build, newdata=testDNBP)
tab_train <-table(rf10_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf10_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=16
rf11_build_ptrain <- predict(rf11_tree_build, newdata=trainDNBP)
rf11_build_ptest <- predict(rf11_tree_build, newdata=testDNBP)
tab_train <-table(rf2_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rf2_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#Remove No Longer Used Builds
#rm(rf6_build_ptrain, rf6_build_ptest, rf6_tree_build)
#rm(bag1_build_ptrain, bag1_build_ptest, bag1_tree_build)
#rm(ha_ave311, ha_com311, ha_sin311)

#________________________________________
#Without 311 Requests
#M=1
rfw1_build_ptrain <- predict(rfw1_tree_build, newdata=trainDNBP)
rfw1_build_ptest <- predict(rfw1_tree_build, newdata=testDNBP)
tab_train <-table(rfw1_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rfw1_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=2
rfw2_build_ptrain <- predict(rfw2_tree_build, newdata=trainDNBP)
rfw2_build_ptest <- predict(rfw2_tree_build, newdata=testDNBP)
tab_train <-table(rfw2_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rfw2_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=4
rfw3_build_ptrain <- predict(rfw3_tree_build, newdata=trainDNBP)
rfw3_build_ptest <- predict(rfw3_tree_build, newdata=testDNBP)
tab_train <-table(rfw3_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rfw3_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%
tab_test
table(rfw3_build_ptest)

#M=6
rfw4_build_ptrain <- predict(rfw4_tree_build, newdata=trainDNBP)
rfw4_build_ptest <- predict(rfw4_tree_build, newdata=testDNBP)
tab_train <-table(rfw4_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rfw4_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=8
rfw5_build_ptrain <- predict(rfw5_tree_build, newdata=trainDNBP)
rfw5_build_ptest <- predict(rfw5_tree_build, newdata=testDNBP)
tab_train <-table(rfw5_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rfw5_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%


#M=10
rfw6_build_ptrain <- predict(rfw6_tree_build, newdata=trainDNBP)
rfw6_build_ptest <- predict(rfw6_tree_build, newdata=testDNBP)
tab_train <-table(rfw6_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rfw6_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#M=12
rfw7_build_ptrain <- predict(rfw7_tree_build, newdata=trainDNBP)
rfw7_build_ptest <- predict(rfw7_tree_build, newdata=testDNBP)
tab_train <-table(rfw7_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(rfw7_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%

#Remove Not Selected Models
#rm(rfw1_tree_build,rfw1_build_ptrain,rfw1_build_ptest)


#----------------------------------------------------
##Build Bagged Model
#----------------------------------------------------

#________________________________________
#With 311 Requests
bag1_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore + R311Rat + R311Graf + R311Cart + R311Vac, data=trainDNBP, mtry=17, ntree=1000, importance = TRUE)


#________________________________________
#Without 311 Requests
bag2_tree_build  <-randomForest(PERMIT_TYP~Dist + PopTot + MedianAge + Income + HomePrice + ResSales + ResStock + MortgAct + 
                                 ElecUse + CityLots + BlueSta + RedSta + WalkScore, data=trainDNBP, mtry=13, ntree=1000, importance = TRUE)


#----------------------------------------------------
#Check Classification Error

#________________________________________
#With 311 Requests
bag1_build_ptrain <- predict(bag1_tree_build, newdata=trainDNBP)
bag1_build_ptest <- predict(bag1_tree_build, newdata=testDNBP)
tab_train <-table(bag1_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(bag1_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%


#________________________________________
#Without 311 Requests
bag2_build_ptrain <- predict(bag2_tree_build, newdata=trainDNBP)
bag2_build_ptest <- predict(bag2_tree_build, newdata=testDNBP)
tab_train <-table(bag2_build_ptrain, trainDNBP$PERMIT_TYP)
tab_test  <-table(bag2_build_ptest, testDNBP$PERMIT_TYP)
print("Train Error")
1-sum(diag(tab_train))/sum(tab_train) #19.700%
print("Test Error")
1-sum(diag(tab_test))/sum(tab_test)   #18.349%


#----------------------------------------------------
#Checking the Important Variables on Final Models
#----------------------------------------------------

#________________________________________
#With 311 Requests

#M=5
importance(rf1_tree_build)
varImpPlot(rf1_tree_build, main=NULL) #Which variable is important?
varImpPlot(rf1_tree_build, col="goldenrod1", pch=19, cex = 1.5, main=NULL) #Which variable is important?
varImpPlot(rf1_tree_build,  pch=19, cex = 1.5, main=NULL) #Which variable is important?

#________________________________________
#Without 311 Requests

#M=4
importance(rfw3_tree_build)
varImpPlot(rfw3_tree_build, col="deepskyblue", pch=19, cex = 1.5, main=NULL) #Which variable is important?
varImpPlot(rfw3_tree_build,  pch=19, cex = 1.5, main=NULL) #Which variable is important?

dev.off()

