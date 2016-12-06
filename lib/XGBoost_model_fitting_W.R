library(dplyr)
library(xgboost)

setwd('C:\\Users\\LENOVO\\Desktop\\Academic\\ADS\\project_5')
load('df(final).RData')

train_3 <- df %>%
  filter(year_mon == '2016_3')

train_4 <- df %>%
  filter(year_mon == '2016_4') 

train_3 <- train_3[ , -49]
train_4 <- train_4[ , -c(1, 3:24, 49)]
colnames(train_4)[2:25] <- paste(colnames(train_4)[2:25], '_Y', sep = '')
train_df <- inner_join(train_3, train_4, by = 'ncodpers')
  
for (i in c(3, 4, 5, 12, 13, 14, 15, 17, 18, 21, 24)){
  train_df[, i] <- as.factor(train_df[ , i])
}

###Train the model
train_df_X <- data.matrix(train_df[ , c(3:6, 8:48)])

models <- list()
#72
for (i in 49:72){

  train_df_Y <- train_df[ , i]
  temp_model <- xgboost(data = train_df_X, 
                        label = train_df_Y, 
                        nrounds = 100,
                        missing = NaN
                        )

  models[[i - 48]] <- temp_model
}

###### Test the model

test_4 <- df %>%
  filter(year_mon == '2016_4')

test_5 <- df %>%
  filter(year_mon == '2016_5') 

test_4 <- test_4[ , -49]
test_5 <- test_5[ , -c(1, 3:24, 49)]
colnames(test_5)[2:25] <- paste(colnames(test_5)[2:25], '_Y', sep = '')
test_df <- inner_join(test_4, test_5, by = 'ncodpers')

for (i in c(3, 4, 5, 12, 13, 14, 15, 17, 18, 21, 24)){
  test_df[, i] <- as.factor(test_df[ , i])
}

test_df_X <- data.matrix(test_df[ , c(3:6, 8:48)])

pred_matrix <- matrix(0, ncol = 24, nrow = nrow(test_df_X))
error_rate <- rep(0, 24)

for (i in 1:24){
  
  pred <- round(predict(models[[i]], test_df_X, missing = NaN))
  pred_matrix[ , i] <- pred
  error_rate[i] <- sum(pred != test_df[ , i + 48]) / length(pred)

}

sum(error_rate)

error_rate_2 <- error_rate




