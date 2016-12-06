setwd("C:\\Users\\Zachary\\Desktop\\Santander Data")

#prediction for random forrest
#1st month
pred_1 = predict(rand_tree_1,model.data.2,type="class")
table(pred_1,model.data.2$saving_1)
rate_1 = 65507/length(model.data.2$saving_1)

#2nd month
set.seed(659)
sample_size_2 = floor(0.6*nrow(model.data.2))
sample_data_2 = model.data.2[sample(nrow(model.data.2),size=sample_size_2,replace=FALSE),]
rand_tree_2 = randomForest(saving_1~.-saving_1,data=sample_data_2,mtry=5,importance=T)
pred_2 = predict(rand_tree_2,model.data.3,type="class")
table(pred_2,model.data.3$saving_1)
rate_2 = 65684/length(model.data.3$saving_1)

#3rd month
sample_size_3 = floor(0.6*nrow(model.data.3))
sample_data_3 = model.data.3[sample(nrow(model.data.3),size=sample_size_3,replace=FALSE),]
rand_tree_3 = randomForest(saving_1~.-saving_1,data=sample_data_3,mtry=5,importance=T)
pred_3 = predict(rand_tree_3,model.data.4,type="class")
table(pred_3,model.data.4$saving_1)
rate_3 = 65785/length(model.data.4$saving_1)

#4th month
sample_size_4 = floor(0.6*nrow(model.data.4))
sample_data_4 = model.data.4[sample(nrow(model.data.4),size=sample_size_4,replace=FALSE),]
rand_tree_4 = randomForest(saving_1~.-saving_1,data=sample_data_4,mtry=5,importance=T)
pred_4 = predict(rand_tree_4,model.data.5,type="class")
table(pred_4,model.data.5$saving_1)
rate_4 = (65964+1)/length(model.data.5$saving_1)

#5th month
sample_size_5 = floor(0.6*nrow(model.data.5))
sample_data_5 = model.data.5[sample(nrow(model.data.5),size=sample_size_5,replace=FALSE),]
rand_tree_5 = randomForest(saving_1~.-saving_1,data=sample_data_5,mtry=5,importance=T)
pred_5 = predict(rand_tree_5,model.data.6,type="class")
table(pred_5,model.data.6$saving_1)
rate_5 = (65995+1)/length(model.data.6$saving_1)

#6th month
sample_size_6 = floor(0.6*nrow(model.data.6))
sample_data_6 = model.data.6[sample(nrow(model.data.6),size=sample_size_6,replace=FALSE),]
rand_tree_6 = randomForest(saving_1~.-saving_1,data=sample_data_6,mtry=5,importance=T)
pred_6 = predict(rand_tree_6,model.data.7,type="class")
table(pred_6,model.data.7$saving_1)
rate_6 = (86804)/length(model.data.7$saving_1)

#7th month
sample_size_7 = floor(0.6*nrow(model.data.7))
sample_data_7 = model.data.7[sample(nrow(model.data.7),size=sample_size_7,replace=FALSE),]
rand_tree_7 = randomForest(saving_1~.-saving_1,data=sample_data_7,mtry=5,importance=T)
pred_7 = predict(rand_tree_7,model.data.8,type="class")
table(pred_7,model.data.8$saving_1)
rate_7 = (88190+1)/length(model.data.8$saving_1)

#8th month
sample_size_8 = floor(0.6*nrow(model.data.8))
sample_data_8 = model.data.8[sample(nrow(model.data.8),size=sample_size_8,replace=FALSE),]
rand_tree_8 = randomForest(saving_1~.-saving_1,data=sample_data_8,mtry=5,importance=T)
pred_8 = predict(rand_tree_8,model.data.9,type="class")
table(pred_8,model.data.9$saving_1)
rate_8 = (90494+1)/length(model.data.9$saving_1)

#9th month
sample_size_9 = floor(0.6*nrow(model.data.9))
sample_data_9 = model.data.9[sample(nrow(model.data.9),size=sample_size_9,replace=FALSE),]
rand_tree_9 = randomForest(saving_1~.-saving_1,data=sample_data_9,mtry=5,importance=T)
pred_9 = predict(rand_tree_9,model.data.10,type="class")
table(pred_9,model.data.10$saving_1)
rate_9 = (93262)/length(model.data.10$saving_1)

#10 month
sample_size_10 = floor(0.6*nrow(model.data.10))
sample_data_10 = model.data.10[sample(nrow(model.data.10),size=sample_size_10,replace=FALSE),]
rand_tree_10 = randomForest(saving_1~.-saving_1,data=sample_data_10,mtry=5,importance=T)
pred_10 = predict(rand_tree_10,model.data.11,type="class")
table(pred_10,model.data.11$saving_1)
rate_10 = (94753+1)/length(model.data.11$saving_1)

#11 month
sample_size_11 = floor(0.6*nrow(model.data.11))
sample_data_11 = model.data.11[sample(nrow(model.data.11),size=sample_size_11,replace=FALSE),]
rand_tree_11 = randomForest(saving_1~.-saving_1,data=sample_data_11,mtry=5,importance=T)
pred_11 = predict(rand_tree_11,model.data.12,type="class")
table(pred_11,model.data.12$saving_1)
rate_11 = (95378)/length(model.data.12$saving_1)

#12 month
sample_size_12 = floor(0.6*nrow(model.data.12))
sample_data_12 = model.data.12[sample(nrow(model.data.12),size=sample_size_12,replace=FALSE),]
rand_tree_12 = randomForest(saving_1~.-saving_1,data=sample_data_12,mtry=5,importance=T)
pred_12 = predict(rand_tree_12,model.data.13,type="class")
table(pred_12,model.data.13$saving_1)
rate_12 = (95802+1)/length(model.data.13$saving_1)

#13 month
sample_size_13 = floor(0.6*nrow(model.data.13))
sample_data_13 = model.data.13[sample(nrow(model.data.13),size=sample_size_13,replace=FALSE),]
rand_tree_13 = randomForest(saving_1~.-saving_1,data=sample_data_13,mtry=5,importance=T)
pred_13 = predict(rand_tree_13,model.data.14,type="class")
table(pred_13,model.data.14$saving_1)
rate_13 = (96257)/length(model.data.14$saving_1)

#14 month
sample_size_14 = floor(0.6*nrow(model.data.14))
sample_data_14 = model.data.14[sample(nrow(model.data.14),size=sample_size_14,replace=FALSE),]
rand_tree_14 = randomForest(saving_1~.-saving_1,data=sample_data_14,mtry=5,importance=T)
pred_14 = predict(rand_tree_14,model.data.15,type="class")
table(pred_14,model.data.15$saving_1)
rate_14 = (96735)/length(model.data.15$saving_1)

#15 month
sample_size_15 = floor(0.4*nrow(model.data.15))
sample_data_15 = model.data.15[sample(nrow(model.data.15),size=sample_size_15,replace=FALSE),]
rand_tree_15 = randomForest(saving_1~.-saving_1,data=sample_data_15,mtry=5,importance=T)
pred_15 = predict(rand_tree_15,model.data.16,type="class")
table(pred_15,model.data.16$saving_1)
rate_15 = (97096)/length(model.data.16$saving_1)

#16 month 
sample_size_16 = floor(0.4*nrow(model.data.16))
sample_data_16 = model.data.16[sample(nrow(model.data.16),size=sample_size_16,replace=FALSE),]
rand_tree_16 = randomForest(saving_1~.-saving_1,data=sample_data_16,mtry=5,importance=T)
pred_16 = predict(rand_tree_16,model.data.17,type="class")
table(pred_16,model.data.17$saving_1)
rate_16 = (97395)/length(model.data.17$saving_1)

total_rate = (rate_1 + rate_2 + rate_3 + rate_4 + rate_5 + rate_6 + rate_7 + rate_8
              + rate_9 + rate_10 + rate_11 + rate_12 + rate_13 + rate_14 + rate_15 
              + rate_16)
mean_rate = mean((total_rate*100)/16)

#17 month model buliding
sample_size_17 = floor(0.5*nrow(model.data.17))
sample_data_17 = model.data.17[sample(nrow(model.data.17),size=sample_size_17,replace=FALSE),]
rand_tree_17 = randomForest(saving_1~.-saving_1,data=sample_data_17,mtry=5,importance=T)

#cross validation prediction data set
cross_vali_pred_results = cbind(pred_1,pred_2,pred_3,pred_4,pred_5,pred_4,pred_5,
                                                 pred_6,pred_7,pred_8,pred_9,pred_10,pred_11,pred_12,
                                                 pred_13,pred_14,pred_15,pred_16)

write.csv(cross_vali_pred_results,file="Cross_vali_pred_results_savings_accounts.Zach.csv",row.names=FALSE)

#####
#model prediction on the test data 
load("san.test.ori.Rdata")
test_data = san.test
str(test_data)
test_data_new = subset(test_data, select=c(fecha_dato,ind_empleado,pais_residencia,sexo,age, ind_nuevo,antiguedad,
                                           indrel,indrel_1mes,tiprel_1mes,indresi,indext,conyuemp,canal_entrada,
                                           indfall,nomprov,ind_actividad_cliente,renta, segmento))
test_data_new_1 = subset(test_data_new, select= -c(pais_residencia,canal_entrada,nomprov,fecha_dato))
test_data_new_1$ind_empleado = as.factor(test_data_new_1$ind_empleado)
test_data_new_1$sexo = as.factor(test_data_new_1$sexo)
test_data_new_1$indrel_1mes = as.factor(test_data_new_1$indrel_1mes)
test_data_new_1$tiprel_1mes = as.factor(test_data_new_1$tiprel_1mes)
test_data_new_1$indresi = as.factor(test_data_new_1$indresi)
test_data_new_1$indext = as.factor(test_data_new_1$indext)
test_data_new_1$conyuemp = as.factor(test_data_new_1$conyuemp)
test_data_new_1$indfall = as.factor(test_data_new_1$indfall)
test_data_new_1$segmento = as.factor(test_data_new_1$segmento)
#random forest model building on whole data set
#sample_size_new = floor(0.5*nrow(model_data))
#sample_data_new = model_data[sample(nrow(model_data),size=sample_size_new,replace=FALSE),]
#new_data = subset(model_data)

pred_test = predict(rand_tree_16,model.data.17)

write.csv(pred_test,file="prediction on test data.Zach.csv",row.names=FALSE)
