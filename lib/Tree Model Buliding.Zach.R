install.packages("tree")
install.packages("randomForest")
library(tree)
library(randomForest)

#setting up the data
setwd("C:\\Users\\Zachary\\Desktop\\Santander Data")
load("df(final).Rdata")

new_train_data = df
dim(new_train_data)
str(new_train_data)


indepen_var1 = new_train_data[c(1,3:6,8:10,12:18,21:24)]

#dependent variable: savings account
new_train_data$ind_ahor_fin_ult1 = as.factor(new_train_data$ind_ahor_fin_ult1)
saving_1 = new_train_data$ind_ahor_fin_ult1

model_data = data.frame(saving_1,indepen_var1)
unique(model_data$fecha_dato)

####################################################################
model_data_1 = model_data[model_data$fecha_dato=="2015-01-28",]
######################################################################
model.data.1 = subset(model_data_1, select= -c(pais_residencia,canal_entrada,nomprov,
                              fecha_dato))
##########################################
#modeling buliding 
#single classification tree
fit_tree = tree(saving_1~.-saving_1,data=model.data.1,split="gini")
summary(fit_tree)
plot(fit_tree)
text(fit_tree,pretty=0)

#pruning the tree
set.seed(1498)
cv_fit_tree = cv.tree(fit_tree,FUN=prune.misclass)
names(cv_fit_tree)
cv_fit_tree

#Random forest classfication tree
set.seed(659)
sample_size = floor(0.6*nrow(model.data.1))
sample_data = model.data.1[sample(nrow(model.data.1),size=sample_size,replace=FALSE),]
#random forest with 5 variables
rand_tree_1 = randomForest(saving_1~.-saving_1,data=sample_data,mtry=5,importance=T)
rand_tree_1

###################################################################
#generating all predicting data sets one month ahead 
model_data_2 = model_data[model_data$fecha_dato=="2015-02-28",]
model_data_3 = model_data[model_data$fecha_dato=="2015-03-28",]
model_data_4 = model_data[model_data$fecha_dato=="2015-04-28",]
model_data_5 = model_data[model_data$fecha_dato=="2015-05-28",]
model_data_6 = model_data[model_data$fecha_dato=="2015-06-28",]
model_data_7 = model_data[model_data$fecha_dato=="2015-07-28",]
model_data_8 = model_data[model_data$fecha_dato=="2015-08-28",]
model_data_9 = model_data[model_data$fecha_dato=="2015-09-28",]
model_data_10 = model_data[model_data$fecha_dato=="2015-10-28",]
model_data_11 = model_data[model_data$fecha_dato=="2015-11-28",]
model_data_12 = model_data[model_data$fecha_dato=="2015-12-28",]
model_data_13 = model_data[model_data$fecha_dato=="2016-01-28",]
model_data_14 = model_data[model_data$fecha_dato=="2016-02-28",]
model_data_15 = model_data[model_data$fecha_dato=="2016-03-28",]
model_data_16 = model_data[model_data$fecha_dato=="2016-04-28",]
model_data_17 = model_data[model_data$fecha_dato=="2016-05-28",]

model.data.2 = subset(model_data_2, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.3 = subset(model_data_3, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.4 = subset(model_data_4, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.5 = subset(model_data_5, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.6 = subset(model_data_6, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.7 = subset(model_data_7, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.8 = subset(model_data_8, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.9 = subset(model_data_9, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.10 = subset(model_data_10, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.11 = subset(model_data_11, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.12 = subset(model_data_12, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.13 = subset(model_data_13, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.14 = subset(model_data_14, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.15 = subset(model_data_15, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.16 = subset(model_data_16, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
model.data.17 = subset(model_data_17, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
#####################################################################


