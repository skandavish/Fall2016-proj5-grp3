rm(list=ls())
cat('\014')

library(data.table)
library(plyr)
library(zoo)
library(nnet)
library(foreign)
library(randomForest)
library(reshape2)

setwd('C:\\Users\\karth\\Desktop\\Santander')

# Reading the two datasets
# 1. The independent variables till February
DataUptoFeb <- fread("Data\\customersTillFeb.csv")
# 2. The response variable as of Marc
ActivityinMarch <- fread("Data\\CustomerDataForMarch.csv")
ActivityinMarch[,"V1" := NULL]

# Merging the 2 datasets
FinalData <- merge(DataUptoFeb, ActivityinMarch, by = "CustomerCode")

FinalData[,c("CustomerCode","TotalNoOfProducts","newChannelforJoining","fecha_dato","EmployeeIndex","CustomersCountryResidence",
                          "Sex","Age","AccountOpeningDate","NewCustomerIndex","CustomerSeniority",
                          "PrimaryCustomerIndex","LastDateAsPrimaryCustomer","CustomerType",
                          "CustomerRelationType","ResidenceIndex","ForeignerIndex","SpouseIndex",
                          "ChannelforJoining","Address","ProvinceName","ActivityIndex","GrossIncome",
                          "Segmentation","SavingsAccount","Guarantees","CurrentAccounts","DerivadaAccount",
                          "PayRollAccount","JuniorAccount","MasParticularAccount","ParticularAccount",
                          "ParticularPlusAccount","ShortTermDeposits","MediumTermDeposits","LongTermDeposits",
                          "EAccount","Funds","Mortgage","Pensions","Loans","Taxes","CreditCard","Securities",
                          "HomeAccount","PayRoll","PensionsII","DirectDebit","Customer_Duration","MonthOfPurchase",
                          "SavingsAccount_Max","Guarantees_Max","CurrentAccounts_Max", "DerivadaAccount_Max", "PayRollAccount_Max",
                          "JuniorAccount_Max","MasParticularAccount_Max","ParticularAccount_Max","ParticularPlusAccount_Max",
                          "ShortTermDeposits_Max","MediumTermDeposits_Max","LongTermDeposits_Max","EAccount_Max","Funds_Max",
                          "Mortgage_Max","Pensions_Max","Loans_Max","Taxes_Max","CreditCard_Max","Securities_Max","HomeAccount_Max",
                          "PayRoll_Max","PensionsII_Max","DirectDebit_Max"):= NULL]


# summary(FinalData)
FinalData$PurchasedProductCode <- as.factor(FinalData$PurchasedProductCode)

rm(ActivityinMarch)
rm(DataUptoFeb)

########################### PCA ###########################
# for(i in 1:ncol(FinalData)){
#   
#   if(sapply(FinalData,class)[i] == 'character'){
#     print(colnames(FinalData)[i])
#   }
# }

FinalData[, employeeIndexMode:=as.factor(employeeIndexMode)]
FinalData[, custCountryMode:=as.factor(custCountryMode)]
FinalData[, sexMode:=as.factor(sexMode)]
FinalData[, custRelTypeMode:=as.factor(custRelTypeMode)]
FinalData[, resIndexMode:=as.factor(resIndexMode)]
FinalData[, forgnIndexMode:=as.factor(forgnIndexMode)]
FinalData[, chnlJoiningMode:=as.factor(chnlJoiningMode)]
FinalData[, prvnNameMode:=as.factor(prvnNameMode)]
FinalData[, segmentationMode:=as.factor(segmentationMode)]

onlyIndepVars <- data.table(data.frame(FinalData)) 

onlyIndepVars[,"PurchasedProductCode" := NULL]
onlyIndepVars <- data.matrix(onlyIndepVars)
toBeRmvd <- integer()

for(i in 1:ncol(onlyIndepVars)){
  if(length(unique(onlyIndepVars[,i])) == 1){
    toBeRmvd <- c(toBeRmvd,i)
  }
}

onlyIndepVars <- onlyIndepVars[,-toBeRmvd]
# colnames(onlyIndepVars)
pr.out <- prcomp(onlyIndepVars,scale=T)

# pr.out$rotation #columns contain the principal components (matrix P in Z=XP)

#New covariates are ranked in terms of how "informative" they are,
#measured by "proportion of explained variance"
# summary(pr.out)

reduced.data <- data.frame(pr.out$x)
reduced.data <- reduced.data[c(1:30)]
reduced.data$response <- FinalData$PurchasedProductCode
# head(reduced.data)

############# Exploring the distribution of the response variable #############
aggData <- data.frame(table(reduced.data$response))
aggData <- aggData[order(aggData$Freq,decreasing = T),]
aggData$perc <- aggData$Freq/sum(aggData$Freq)
aggData$cumilPerc <- cumsum(aggData$perc)

head(aggData)

# Only current ~ 42%
# None  25%
# current & particular ~ 4.3%
# current & Direct debit ~ 3%
# Only particular ~ 2.6%
# Current & EA ~ 1.4%

aggData$finalLabel <- aggData$Var1
aggData$finalLabel <- as.character(aggData$finalLabel)
aggData$finalLabel[7:nrow(aggData)] <- 'Others'
aggData$finalLabel <- as.factor(aggData$finalLabel)
colnames(aggData)[1] <- 'response'

################### Creating an updated reduced dataset ###################
reduced.data <- merge(reduced.data,aggData[c('response','finalLabel')],by='response')

reduced.data <- reduced.data[-1]

####################### Model attempts #######################
# Experiment One - Multinomial Logit
# chk <- FinalData[c(1:10000),]
TrainModel <- multinom(finalLabel ~ ., data = reduced.data)
hist(TrainModel$residuals)

fittedValues <- data.table(TrainModel$fitted.values)
fittedValues$fitValue <- colnames(fittedValues)[max.col(fittedValues,ties.method="first")]
fittedValues$actual <- reduced.data$finalLabel

#Confusion matrix
inSampleCF <- data.frame(table(Actual = fittedValues$actual, Predicted = fittedValues$fitValue))
castedData <- dcast(inSampleCF,Actual ~ Predicted, value.var = 'Freq')
castedData$Actual <- as.character(castedData$Actual)

for(i in 1:ncol(castedData)){
  colnames(castedData)[i] <- paste('X_',colnames(castedData)[i],sep='')
  castedData$X_Actual[i] <- paste('X_',castedData$X_Actual[i],sep='')
}

# colnames(castedData) <- paste('X_',col)

write.csv(castedData,'inSampleConfusionMatrix.csv',row.names = F)

############################################################################################
################################ Not included, as of now ###################################

# # Prediction attempts
# #Reading files for predictions
# DataUptoMar <- fread("Data\\toPredForApril.csv")
# ActivityinApril <- fread("Data\\CustomerDataApril.csv")
# ActivityinApril[,"V1" := NULL]
# 
# # Merging the 2 datasets
# PredData <- merge(DataUptoMar, ActivityinApril, by = "CustomerCode")
# 
# PredData[, employeeIndexMode:=as.factor(employeeIndexMode)]
# PredData[, custCountryMode:=as.factor(custCountryMode)]
# PredData[, sexMode:=as.factor(sexMode)]
# PredData[, custRelTypeMode:=as.factor(custRelTypeMode)]
# PredData[, resIndexMode:=as.factor(resIndexMode)]
# PredData[, forgnIndexMode:=as.factor(forgnIndexMode)]
# PredData[, chnlJoiningMode:=as.factor(chnlJoiningMode)]
# PredData[, prvnNameMode:=as.factor(prvnNameMode)]
# PredData[, segmentationMode:=as.factor(segmentationMode)]
# 
# PredData <- PredData[,c(colnames(onlyIndepVars),"PurchasedProductCode"),with = F]
# PredDataMat <- as.matrix(PredData[,c(colnames(onlyIndepVars)),with = F])
# 
# #Using the output from PCA to create the new variables
# PredDataMat <- PredDataMat %*% pr.out$rotation
# test.data <- predict(pr.out, newdata = PredData)
# 
# # summary(FinalData)
# FinalData$PurchasedProductCode <- as.factor(FinalData$PurchasedProductCode)
# 
# rm(ActivityinMarch)
# rm(DataUptoFeb)
# 
# # Experiment Two - Random Forest
# 
# # Store columns names
# varNames <- names(reduced.data)
# # Exclude ID or Response variable
# varNames <- varNames[!varNames %in% c("response")]
# # add + sign between exploratory variables
# varNames1 <- paste(varNames, collapse = "+")
# # Add response variable and convert to a formula object
# rf.form <- as.formula(paste("response", varNames1, sep = " ~ "))
# 
# #Obtain best mtry
# xVar <- reduced.data[-ncol(reduced.data)]
# reduced.data$response <- as.factor(reduced.data$response)
# best_mtry <- tuneRF(x = xVar,y=reduced.data$response, stepFactor = 0.5)
# 
# ## mtry = 3  OOB error = 0.32% 
# ## Searching left ...
# ## mtry = 6     OOB error = 0.35% 
# ## -0.1052632 0.05 
# ## Searching right ...
# ## mtry = 1     OOB error = 0.3% 
# ## 0.05263158 0.05
# 
# ## Warning in randomForest.default(x, y, mtry = mtryCur, ntree = ntreeTry, :
# ## invalid mtry: reset to within valid range
# 
# ## mtry = 0     OOB error = 0.3% 
# ## 0 0.05
# 
# ## Warning in xy.coords(x, y, xlabel, ylabel, log): 1 x value <= 0 omitted
# ## from logarithmic plot
# chk <- reduced.data[1:10000,]
# rf_model <- randomForest(rf.form,chk,ntree=10,importance=T,mtry=2)
# chk$response
 
