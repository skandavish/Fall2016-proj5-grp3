rm(list=ls())
cat('\014')

library(data.table)
library(plyr)
library(zoo)

#setwd('C:\\Users\\karth\\Desktop\\Santander')

##############################################################################################################
##############Initial Processsing/Data cleaning#############
#########
inputData <- fread("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 5/Data/train_ver2.csv")

setnames(inputData,c('fecha_dato', 'CustomerCode','EmployeeIndex', 'CustomersCountryResidence', 'Sex',
                     'Age','AccountOpeningDate', 'NewCustomerIndex', 'CustomerSeniority','PrimaryCustomerIndex','LastDateAsPrimaryCustomer',
                     'CustomerType','CustomerRelationType', 'ResidenceIndex', 'ForeignerIndex', 'SpouseIndex','ChannelforJoining',
                     'DeceasedIndex','Address', 'ProvinceCode','ProvinceName','ActivityIndex','GrossIncome','Segmentation',
                     'SavingsAccount','Guarantees','CurrentAccounts','DerivadaAccount','PayRollAccount','JuniorAccount',
                     'MasParticularAccount','ParticularAccount','ParticularPlusAccount','ShortTermDeposits','MediumTermDeposits',
                     'LongTermDeposits','EAccount','Funds', 'Mortgage','Pensions','Loans','Taxes','CreditCard',
                     'Securities','HomeAccount','PayRoll','PensionsII','DirectDebit'))

#changing customer type variable as 
custType <- function(x){
  toRet <- ifelse((x == '1' | x == '1.0'),1,0)
  return(toRet)
}

inputData[,CustomerType:= custType(CustomerType)]

###Deleting 24000 rows which are NAs across all columns 
inputData <- inputData[!which(is.na(inputData[,Age]))] #13619575 rows

#Deleting province code since it has the same info as province name
inputData[,"ProvinceCode" := NULL]
#summary(inputData)

##############################################################################################################
##So GrossIncome has nas. Hence we will find Median Income and add it there
#merge TemporaryDatasetForMedian and the original table by the provincename
#then find missing and copy values from the median income wala column
inputData[, GrossIncome := replace(GrossIncome, is.na(GrossIncome), median(GrossIncome, na.rm=TRUE)), by=ProvinceName]
#summary(inputData)
#hist(inputData$GrossIncome, breaks = 1000)
#temp <- inputData[GrossIncome == max(GrossIncome,na.rm=T)]

##############################################################################################################
#Cleaning the seniority (tenure) variable
##Absurd no like -999999..need to change that
##COnverting Dates to date time format
#head(inputData)
#Changing data types of account opening date & fecha_dato
inputData[, AccountOpeningDate := as.Date(AccountOpeningDate,format="%Y-%m-%d")]
inputData[, fecha_dato := as.Date(fecha_dato,format="%Y-%m-%d")]
# setkey(inputData,fecha_dato)
# unique(inputData[,fecha_dato])


#Recalculating tenure variable
tenureCal <- function(x,y){
  
  #x - fecha_dato
  #y - accOpenDate
  
  tenure <- (year(x) - year(y))*12 + (month(x) - month(y))
  return(tenure)
}

#Creating the new tenure variable - As of feb 28
inputData[, Customer_Duration := tenureCal(as.Date('2016-02-28',format="%Y-%m-%d"), AccountOpeningDate)]
# cat('\014')
# head(inputData, n = 500)
# hist(inputData$Customer_Duration)

## Adding a month of purchase variable
## Adding month nos Jan 2015 = 1, 
monOfPurchase <- function(x){
  toRet <- ifelse(year(x) == 2015, month(x), month(x)+12)
  return(toRet)
}

inputData[, MonthOfPurchase := monOfPurchase(fecha_dato)]
# head(inputData, n = 100)
# hist(inputData$MonthOfPurchase)

##############################################################################################################
#Added a column to show total no of purchases in a month
inputData[, SavingsAccount := replace(SavingsAccount, is.na(SavingsAccount), 0)]
inputData[, Guarantees := replace(Guarantees, is.na(Guarantees), 0)]
inputData[, CurrentAccounts := replace(CurrentAccounts, is.na(CurrentAccounts), 0)]
inputData[, DerivadaAccount := replace(DerivadaAccount, is.na(DerivadaAccount), 0)]
inputData[, PayRollAccount := replace(PayRollAccount, is.na(PayRollAccount), 0)]
inputData[, JuniorAccount := replace(JuniorAccount, is.na(JuniorAccount), 0)]
inputData[, MasParticularAccount := replace(MasParticularAccount, is.na(MasParticularAccount), 0)]
inputData[, ParticularAccount := replace(ParticularAccount, is.na(ParticularAccount), 0)]
inputData[, ParticularPlusAccount := replace(ParticularPlusAccount, is.na(ParticularPlusAccount), 0)]
inputData[, ShortTermDeposits := replace(ShortTermDeposits, is.na(ShortTermDeposits), 0)]
inputData[, MediumTermDeposits := replace(MediumTermDeposits, is.na(MediumTermDeposits), 0)]
inputData[, LongTermDeposits := replace(LongTermDeposits, is.na(LongTermDeposits), 0)]
inputData[, EAccount := replace(EAccount, is.na(EAccount), 0)]
inputData[, Funds := replace(Funds, is.na(Funds), 0)]
inputData[, Mortgage := replace(Mortgage, is.na(Mortgage), 0)]
inputData[, Pensions := replace(Pensions, is.na(Pensions), 0)]
inputData[, Loans := replace(Loans, is.na(Loans), 0)]
inputData[, Taxes := replace(Taxes, is.na(Taxes), 0)]
inputData[, CreditCard := replace(CreditCard, is.na(CreditCard), 0)]
inputData[, Securities := replace(Securities, is.na(Securities), 0)]
inputData[, HomeAccount := replace(HomeAccount, is.na(HomeAccount), 0)]
inputData[, PayRoll := replace(PayRoll, is.na(PayRoll), 0)]
inputData[, PensionsII := replace(PensionsII, is.na(PensionsII), 0)]
inputData[, DirectDebit := replace(DirectDebit, is.na(DirectDebit), 0)]

inputData[, TotalNoOfProducts := sum(SavingsAccount,Guarantees,CurrentAccounts,DerivadaAccount,PayRollAccount,JuniorAccount,MasParticularAccount,ParticularAccount,ParticularPlusAccount,ShortTermDeposits,MediumTermDeposits,LongTermDeposits,EAccount,Funds,Mortgage,Pensions,Loans,Taxes,CreditCard,Securities,HomeAccount,PayRoll,PensionsII,DirectDebit), by = row.names(inputData)]
# head(inputData, n = 100)
# hist(inputData$TotalNoOfProducts)

##############################################################################################################
############Exploring a few other variables, one variable at a time###############
##How many are alive/dead
inputData[,.N,by = DeceasedIndex]
#Almost everyone is alive. Hence we can get rid of this column
inputData[,"DeceasedIndex" := NULL]

##Checking channel wise distribution
chanDist <- inputData[,.N,by = ChannelforJoining]
chanDist$perContri <- chanDist$N/sum(chanDist$N)
chanDist <- chanDist[order(chanDist$N,decreasing = T),]
chanDist$cumilPerc <- cumsum(chanDist$perContri)

#Top three channels contribute to 76.5%, while the other 160 contribute to 23.5%

# barplot(chanDist$N, main="Channel Used to Join", xlab="Channels")
#IMPORTANT - xAxis values code to be taken

##Due to heavy skew, Mapping smaller channels to SmallerChannels
smallChnlMap <- function(x){
  toRet <- ifelse(x == 'KAT' | x == 'KFC' |x == 'KHE', x, 'otherChannels')
  return(toRet)
}

inputData[,newChannelforJoining:= smallChnlMap(ChannelforJoining)]

inputData[,.N,by = newChannelforJoining]

##Level of data check
##Check if there are unique ids.. i presume not. Then we can see the services that he/she has added/dropped
length(unique(inputData$CustomerCode))/nrow(inputData) #0.0697242 ==> ~ 14 rows per customer

##############################################################################################################
#Exploring the overall avergae purchase pattern over the first 12 months of a customer
avgPurchMonthly <- inputData[, .(avgPurch = mean(TotalNoOfProducts)), by = MonthOfPurchase]
#plot(x = avgPurchMonthly$MonthOfPurchase, y = avgPurchMonthly$avgPurch, type = 'o', col = 'red', lwd = 3.5)

##We can see that the avg # of products purchased reduces over time 

###let me look at the products owned over time. For different segments.
avgPurchMonthlyByProd <- inputData[, lapply(.SD, mean), by=MonthOfPurchase, .SDcols=c('SavingsAccount','Guarantees', 'CurrentAccounts','DerivadaAccount','PayRollAccount','JuniorAccount','MasParticularAccount','ParticularAccount','ParticularPlusAccount','ShortTermDeposits')]

##############################################################################################################
##How are active customers doing? (Column = customer realtionship type)
curRelTypeDist <- inputData[,.N,by = CustomerRelationType]
curRelTypeDist$perContri <- curRelTypeDist$N/sum(curRelTypeDist$N)
curRelTypeDist <- curRelTypeDist[order(curRelTypeDist$N,decreasing = T),]
curRelTypeDist$cumilPerc <- cumsum(curRelTypeDist$perContri)
##Active (45%) and inactive(53%) contribute to 98% of total data. So we will look at product usage in

#Average product usage for active & inactive customers seperately
avgPurchMonthlyByProdActive <- inputData[CustomerRelationType == "A", lapply(.SD, mean), by=MonthOfPurchase, .SDcols=c('SavingsAccount','Guarantees', 'CurrentAccounts','DerivadaAccount','PayRollAccount','JuniorAccount','MasParticularAccount','ParticularAccount','ParticularPlusAccount','ShortTermDeposits')]
avgPurchMonthlyByProdInActive <- inputData[CustomerRelationType == "I", lapply(.SD, mean), by=MonthOfPurchase, .SDcols=c('SavingsAccount','Guarantees', 'CurrentAccounts','DerivadaAccount','PayRollAccount','JuniorAccount','MasParticularAccount','ParticularAccount','ParticularPlusAccount','ShortTermDeposits')]

###Precipitous Decline for both types of customers

##############################################################################################################
##Analyzing purchasing patterns by type of customer

avgPurchMonthlyByProdTop <- inputData[Segmentation == "01 - TOP", lapply(.SD, mean), by=MonthOfPurchase, .SDcols=c('SavingsAccount','Guarantees', 'CurrentAccounts','DerivadaAccount','PayRollAccount','JuniorAccount','MasParticularAccount','ParticularAccount','ParticularPlusAccount','ShortTermDeposits')]
avgPurchMonthlyByProdParticul <- inputData[Segmentation == "02 - PARTICULARES", lapply(.SD, mean), by=MonthOfPurchase, .SDcols=c('SavingsAccount','Guarantees', 'CurrentAccounts','DerivadaAccount','PayRollAccount','JuniorAccount','MasParticularAccount','ParticularAccount','ParticularPlusAccount','ShortTermDeposits')]
avgPurchMonthlyByProdUniversit <- inputData[Segmentation == "03 - UNIVERSITARIO", lapply(.SD, mean), by=MonthOfPurchase, .SDcols=c('SavingsAccount','Guarantees', 'CurrentAccounts','DerivadaAccount','PayRollAccount','JuniorAccount','MasParticularAccount','ParticularAccount','ParticularPlusAccount','ShortTermDeposits')]

##People start unsubscribing April onwards and a few producst show a reversal in that trend??????????

##############################################################################################################
#Distribution of product purchased by month of purchase

monthWiseDist <- inputData[,.N,by = MonthOfPurchase]
# barplot(monthWiseDist$N, main="MonthWiseCounts", xlab="Months")

##Almost no new customers from Jan to June

##############################################################################################################

##############################################################################################################

####################################### CREATING THE ANALYTICAL DATASET ######################################
# head(inputData)
inputDataBup <- inputData[fecha_dato == as.Date('2016-04-28',format="%Y-%m-%d")]
rm(inputData)
# inputDataBup[which(inputDataBup$CustomerCode == '1375586'),]

# Rolling up data to a customer level in March
inputDataBup[,SavingsAccount_Max:= max(SavingsAccount),by=CustomerCode]
inputDataBup[,Guarantees_Max:= max(Guarantees),by=CustomerCode]
inputDataBup[,DerivadaAccount_Max:= max(DerivadaAccount),by=CustomerCode]
inputDataBup[,JuniorAccount_Max:= max(JuniorAccount),by=CustomerCode]
inputDataBup[,MasParticularAccount_Max:= max(MasParticularAccount),by=CustomerCode]
inputDataBup[,ParticularAccount_Max:= max(ParticularAccount),by=CustomerCode]
inputDataBup[,ParticularPlusAccount_Max:= max(ParticularPlusAccount),by=CustomerCode]
inputDataBup[,Funds_Max:= max(Funds),by=CustomerCode]
inputDataBup[,Mortgage_Max:= max(Mortgage),by=CustomerCode]
inputDataBup[,Pensions_Max:= max(Pensions),by=CustomerCode]
inputDataBup[,Loans_Max:= max(Loans),by=CustomerCode]
inputDataBup[,Taxes_Max:= max(Taxes),by=CustomerCode]
inputDataBup[,CreditCard_Max:= max(CreditCard),by=CustomerCode]
inputDataBup[,Securities_Max:= max(Securities),by=CustomerCode]
inputDataBup[,CurrentAccounts_Max:= max(CurrentAccounts),by=CustomerCode]
inputDataBup[,HomeAccount_Max:= max(HomeAccount),by=CustomerCode]
inputDataBup[,DirectDebit_Max:= max(DirectDebit),by=CustomerCode]
inputDataBup[,PayRoll_Max:= max(PayRoll),by=CustomerCode]
inputDataBup[,PensionsII_Max:= max(PensionsII),by=CustomerCode]
inputDataBup[,ShortTermDeposits_Max:= max(ShortTermDeposits),by=CustomerCode]
inputDataBup[,MediumTermDeposits_Max:= max(MediumTermDeposits),by=CustomerCode]
inputDataBup[,LongTermDeposits_Max:= max(LongTermDeposits),by=CustomerCode]
inputDataBup[,EAccount_Max:= max(EAccount),by=CustomerCode]
inputDataBup[,PayRollAccount_Max:= max(PayRollAccount),by=CustomerCode]

setkey(inputDataBup,CustomerCode,SavingsAccount,Guarantees,CurrentAccounts,DerivadaAccount,
       PayRollAccount,JuniorAccount,MasParticularAccount,ParticularAccount,
       ParticularPlusAccount,ShortTermDeposits,MediumTermDeposits,LongTermDeposits,EAccount,
       Funds,Mortgage,Pensions,Loans,Taxes,CreditCard,Securities,HomeAccount,PayRoll,
       PensionsII,DirectDebit)


FinalDT <- inputDataBup[,c("CustomerCode","SavingsAccount_Max","Guarantees_Max","CurrentAccounts_Max",
                           "DerivadaAccount_Max","PayRollAccount_Max","JuniorAccount_Max","MasParticularAccount_Max",
                           "ParticularAccount_Max","ParticularPlusAccount_Max","ShortTermDeposits_Max","MediumTermDeposits_Max",
                           "LongTermDeposits_Max","EAccount_Max","Funds_Max","Mortgage_Max","Pensions_Max",
                           "Loans_Max","Taxes_Max","CreditCard_Max","Securities_Max","HomeAccount_Max",
                           "PayRoll_Max","PensionsII_Max","DirectDebit_Max"),with = F]
FinalDT = unique(FinalDT)
FinalDT[,PurchasedProductCode:= paste(SavingsAccount_Max,Guarantees_Max,CurrentAccounts_Max,
                                      DerivadaAccount_Max,PayRollAccount_Max,JuniorAccount_Max,
                                      MasParticularAccount_Max,ParticularAccount_Max,ParticularPlusAccount_Max,
                                      ShortTermDeposits_Max,MediumTermDeposits_Max,LongTermDeposits_Max,
                                      EAccount_Max,Funds_Max,Mortgage_Max,Pensions_Max,Loans_Max,Taxes_Max,
                                      CreditCard_Max,Securities_Max,HomeAccount_Max,PayRoll_Max,PensionsII_Max,
                                      DirectDebit_Max,sep = "")]

FinalDT <- FinalDT[,c("CustomerCode","PurchasedProductCode"),with = FALSE]

write.csv(FinalDT,"C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 5/Data/CustomerDataApril.csv")



