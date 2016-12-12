rm(list=ls())
cat('\014')

library(data.table)
library(plyr)
library(zoo)


##############################################################################################################
#########################Initial Processsing/Data cleaning#############
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
inputDataBup <- inputData[fecha_dato <= as.Date('2016-04-28',format="%Y-%m-%d")]
rm(inputData)
# inputDataBup[which(inputDataBup$CustomerCode == '1375586'),]

#User define mode function
Mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# Mean gross income
inputDataBup[,meanIncome:= mean(GrossIncome),by=CustomerCode]

# Mode of employee index
inputDataBup[,employeeIndexMode:= Mode(EmployeeIndex),by=CustomerCode]

# Mode CustomersCountryResidence
inputDataBup[,custCountryMode:= Mode(CustomersCountryResidence),by=CustomerCode]

# Mode of Sex 
inputDataBup[,sexMode:= Mode(Sex),by=CustomerCode]

# Max of Age 
inputDataBup[,ageMax:= max(Age),by=CustomerCode]

# Mode of Primary customer index
inputDataBup[,priCustIndMode:= Mode(PrimaryCustomerIndex),by=CustomerCode]

# Mode of CustomerType
inputDataBup[,custTypeMode:= Mode(CustomerType),by=CustomerCode]

# Mode of CustomerRelationType
inputDataBup[,custRelTypeMode:= Mode(CustomerRelationType),by=CustomerCode]

# Mode of ResidenceIndex
inputDataBup[,resIndexMode:= Mode(ResidenceIndex),by=CustomerCode]

# Mode of ForeignerIndex
inputDataBup[,forgnIndexMode:= Mode(ForeignerIndex),by=CustomerCode]

# Mode of channel of joining
inputDataBup[,chnlJoiningMode:= Mode(newChannelforJoining),by=CustomerCode]

# Mode of province name
inputDataBup[,prvnNameMode:= Mode(ProvinceName),by=CustomerCode]

# Mode of activity index
inputDataBup[,actyIndxMode:= Mode(ActivityIndex),by=CustomerCode]

# Mode of Segmentation
inputDataBup[,segmentationMode:= Mode(Segmentation),by=CustomerCode]

# Current products subscriced
currentProdSub <- function(x){
  return(ifelse(x == 1,1,0))
}

inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,SavingsAccount_currSub:= currentProdSub(SavingsAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,Guarantees_currSub:= currentProdSub(Guarantees),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,DerivadaAccount_currSub:= currentProdSub(DerivadaAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,JuniorAccount_currSub:= currentProdSub(JuniorAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,MasParticularAccount_currSub:= currentProdSub(MasParticularAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,ParticularAccount_currSub:= currentProdSub(ParticularAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,ParticularPlusAccount_currSub:= currentProdSub(ParticularPlusAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,Funds_currSub:= currentProdSub(Funds),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,Mortgage_currSub:= currentProdSub(Mortgage),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,Pensions_currSub:= currentProdSub(Pensions),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,Loans_currSub:= currentProdSub(Loans),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,Taxes_currSub:= currentProdSub(Taxes),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,CreditCard_currSub:= currentProdSub(CreditCard),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,Securities_currSub:= currentProdSub(Securities),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,CurrentAccounts_currSub:= currentProdSub(CurrentAccounts),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,DirectDebit_currSub:= currentProdSub(DirectDebit),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,PayRoll_currSub:= currentProdSub(PayRoll),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,PensionsII_currSub:= currentProdSub(PensionsII),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,ShortTermDeposits_currSub:= currentProdSub(ShortTermDeposits),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,MediumTermDeposits_currSub:= currentProdSub(MediumTermDeposits),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,LongTermDeposits_currSub:= currentProdSub(LongTermDeposits),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,EAccount_currSub:= currentProdSub(EAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,PayRollAccount_currSub:= currentProdSub(PayRollAccount),by=CustomerCode]
inputDataBup[month(fecha_dato) == 03 & year(fecha_dato) == 2016,HomeAccount_currSub:= currentProdSub(HomeAccount),by=CustomerCode]

# Finding no of months of association for each product, one product at a time
inputDataBup[,SavingsAccount_Association:= sum(SavingsAccount),by=CustomerCode]
inputDataBup[,Guarantees_Association:= sum(Guarantees),by=CustomerCode]
inputDataBup[,DerivadaAccount_Association:= sum(DerivadaAccount),by=CustomerCode]
inputDataBup[,JuniorAccount_Association:= sum(JuniorAccount),by=CustomerCode]
inputDataBup[,MasParticularAccount_Association:= sum(MasParticularAccount),by=CustomerCode]
inputDataBup[,ParticularAccount_Association:= sum(ParticularAccount),by=CustomerCode]
inputDataBup[,ParticularPlusAccount_Association:= sum(ParticularPlusAccount),by=CustomerCode]
inputDataBup[,Funds_Association:= sum(Funds),by=CustomerCode]
inputDataBup[,Mortgage_Association:= sum(Mortgage),by=CustomerCode]
inputDataBup[,Pensions_Association:= sum(Pensions),by=CustomerCode]
inputDataBup[,Loans_Association:= sum(Loans),by=CustomerCode]
inputDataBup[,Taxes_Association:= sum(Taxes),by=CustomerCode]
inputDataBup[,CreditCard_Association:= sum(CreditCard),by=CustomerCode]
inputDataBup[,Securities_Association:= sum(Securities),by=CustomerCode]
inputDataBup[,CurrentAccounts_Association:= sum(CurrentAccounts),by=CustomerCode]
inputDataBup[,HomeAccount_Association:= sum(HomeAccount),by=CustomerCode]
inputDataBup[,DirectDebit_Association:= sum(DirectDebit),by=CustomerCode]
inputDataBup[,PayRoll_Association:= sum(PayRoll),by=CustomerCode]
inputDataBup[,PensionsII_Association:= sum(PensionsII),by=CustomerCode]
inputDataBup[,ShortTermDeposits_Association:= sum(ShortTermDeposits),by=CustomerCode]
inputDataBup[,MediumTermDeposits_Association:= sum(MediumTermDeposits),by=CustomerCode]
inputDataBup[,LongTermDeposits_Association:= sum(LongTermDeposits),by=CustomerCode]
inputDataBup[,EAccount_Association:= sum(EAccount),by=CustomerCode]
inputDataBup[,PayRollAccount_Association:= sum(PayRollAccount),by=CustomerCode]

# Finding no of breaks in months for each product, one product at a time
numBreaksCal <- function(x){
  if(sum(x) > 0){
    tempData <- data.frame(colOne = x,colTwo = 1:length(x))
    duration <- max(tempData$colTwo[which(tempData$colOne == 1)]) - min(tempData$colTwo[which(tempData$colOne == 1)]) + 1
    return(duration - sum(x))
  }else{
    return(0)
  }
}

inputDataBup[,SavingsAccount_numBreaks:= numBreaksCal(SavingsAccount),by=CustomerCode]
inputDataBup[,Guarantees_numBreaks:= numBreaksCal(Guarantees),by=CustomerCode]
inputDataBup[,DerivadaAccount_numBreaks:= numBreaksCal(DerivadaAccount),by=CustomerCode]
inputDataBup[,JuniorAccount_numBreaks:= numBreaksCal(JuniorAccount),by=CustomerCode]
inputDataBup[,MasParticularAccount_numBreaks:= numBreaksCal(MasParticularAccount),by=CustomerCode]
inputDataBup[,ParticularAccount_numBreaks:= numBreaksCal(ParticularAccount),by=CustomerCode]
inputDataBup[,ParticularPlusAccount_numBreaks:= numBreaksCal(ParticularPlusAccount),by=CustomerCode]
inputDataBup[,Funds_numBreaks:= numBreaksCal(Funds),by=CustomerCode]
inputDataBup[,Mortgage_numBreaks:= numBreaksCal(Mortgage),by=CustomerCode]
inputDataBup[,Pensions_numBreaks:= numBreaksCal(Pensions),by=CustomerCode]
inputDataBup[,Loans_numBreaks:= numBreaksCal(Loans),by=CustomerCode]
inputDataBup[,Taxes_numBreaks:= numBreaksCal(Taxes),by=CustomerCode]
inputDataBup[,CreditCard_numBreaks:= numBreaksCal(CreditCard),by=CustomerCode]
inputDataBup[,Securities_numBreaks:= numBreaksCal(Securities),by=CustomerCode]
inputDataBup[,CurrentAccounts_numBreaks:= numBreaksCal(CurrentAccounts),by=CustomerCode]
inputDataBup[,HomeAccount_numBreaks:= numBreaksCal(HomeAccount),by=CustomerCode]
inputDataBup[,DirectDebit_numBreaks:= numBreaksCal(DirectDebit),by=CustomerCode]
inputDataBup[,PayRoll_numBreaks:= numBreaksCal(PayRoll),by=CustomerCode]
inputDataBup[,PensionsII_numBreaks:= numBreaksCal(PensionsII),by=CustomerCode]
inputDataBup[,ShortTermDeposits_numBreaks:= numBreaksCal(ShortTermDeposits),by=CustomerCode]
inputDataBup[,MediumTermDeposits_numBreaks:= numBreaksCal(MediumTermDeposits),by=CustomerCode]
inputDataBup[,LongTermDeposits_numBreaks:= numBreaksCal(LongTermDeposits),by=CustomerCode]
inputDataBup[,EAccount_numBreaks:= numBreaksCal(EAccount),by=CustomerCode]
inputDataBup[,PayRollAccount_numBreaks:= numBreaksCal(PayRollAccount),by=CustomerCode]

### Getting rid of NA rows and keeping unique values only
inputDataBup <- inputDataBup[!which(is.na(inputDataBup[,SavingsAccount_currSub]))] #13619575 rows

write.csv(inputDataBup,'customersTillMarch.csv',row.names = F)



#inputData <- fread("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 5/Data/customersTillMarch.csv")
# MarchData <- fread("C:/Users/svish/Documents/Sem 2/Applied Data Science/Project 5/Data/train_ver2.csv")
# head(inputData)


