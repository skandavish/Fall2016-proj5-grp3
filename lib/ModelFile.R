rm(list=ls())
cat('\014')

library(data.table)
library(plyr)
library(zoo)
library(nnet)
library(foreign)

DataUptoFeb <- fread("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 5/Data/customersTillFeb.csv")
ActivityinMarch <- fread("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 5/Data/CustomerDataMarch.csv")
ActivityinMarch[,"V1" := NULL]

FinalData <- merge(DataUptoFeb, ActivityinMarch, by = "CustomerCode")

FinalData[,c("CustomerCode","fecha_dato","EmployeeIndex","CustomersCountryResidence",
                          "Sex","Age","AccountOpeningDate","NewCustomerIndex","CustomerSeniority",
                          "PrimaryCustomerIndex","LastDateAsPrimaryCustomer","CustomerType",
                          "CustomerRelationType","ResidenceIndex","ForeignerIndex","SpouseIndex",
                          "ChannelforJoining","Address","ProvinceName","ActivityIndex","GrossIncome",
                          "Segmentation","SavingsAccount","Guarantees","CurrentAccounts","DerivadaAccount",
                          "PayRollAccount","JuniorAccount","MasParticularAccount","ParticularAccount",
                          "ParticularPlusAccount","ShortTermDeposits","MediumTermDeposits","LongTermDeposits",
                          "EAccount","Funds","Mortgage","Pensions","Loans","Taxes","CreditCard","Securities",
                          "HomeAccount","PayRoll","PensionsII","DirectDebit","Customer_Duration","MonthOfPurchase"):= NULL]

summary(FinalData)
FinalData$PurchasedProductCode <- as.factor(FinalData$PurchasedProductCode)

rm(ActivityinMarch)
rm(DataUptoFeb)

TrainModel <- multinom(PurchasedProductCode ~., data = FinalData)






