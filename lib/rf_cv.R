library(dplyr)
library(randomForest)

#setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADS\\project_5')
load('df(final).RData')

train_df <- sample_n(df, 100000)


for (i in c(3, 4, 5, 12, 13, 14, 15, 17, 18, 21, 24)){
  train_df[, i] <- as.factor(train_df[ , i])
}

###Train the model
train_df_X <- train_df[ , c(3, 5, 6, 8:10, 12:15, 18, 19, 20, 22:24)]

cv_number <- nrow(train_df_X)
k_folds <- cut(1:cv_number, breaks = 5, labels = FALSE)
cv_errors <- matrix(0, nrow = 24, ncol = 5)


for (k in 1:5){
  models <- list()
  cv_indices <- which(k_folds == k, arr.ind = TRUE)
  cv_train <- train_df_X[cv_indices, ]
  cv_test <- train_df_X[-cv_indices, ]
  
  for (i in 25:48){
    cv_y <- train_df[cv_indices , i]
    temp_model <- randomForest(x = cv_train, 
                          y = cv_y 
                          )
    models[[i - 24]] <- temp_model
    print(i)
  }
  
  pred_matrix <- matrix(0, ncol = 24, nrow = nrow(cv_test))
  error_rate <- rep(0, 24)
  
  for (j in 1:24){
    
    pred <- round(predict(models[[j]], newdata = cv_test, type = 'response'))
    pred_matrix[ , j] <- pred
    error_rate[j] <- sum(pred != train_df[-cv_indices , j + 24]) / length(pred)
    
    print(j)
  }
  cv_errors[, k] <- error_rate
}