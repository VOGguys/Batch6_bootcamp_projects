#Build a regression model

## Regressions Model ## 

# Call package
library(tidyverse)
library(caret)
library(mlbench)

# load dataset

kc_house <- read.csv("kc_house.csv",stringsAsFactors = FALSE)

# Prep_data & clean_data

glimpse(kc_house)
view(kc_house)

mean(complete.cases(kc_house))  #if 1 is awesome 
# if less than 5% >> can be clear data that not impact

kc_house <- drop_na(kc_house)
kc_house %>%
  head(5)

view(kc_house) # check agains

## Train_Model

train_test_split <- function(data){
  set.seed(28)
  n <- nrow(data)
  id <-sample(1:n,size = n*0.7)
  train_data <- data[id,]
  test_data <- data[-id,]
  return(list(train_data,test_data))
}

split_data <- train_test_split(kc_house)

set.seed(25)
train_data <- split_data[[1]]
test_data <- split_data[[2]]
nrow(train_data);nrow(test_data)

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = TRUE)

lm_model <-train(price ~ .,
      data = train_data,
      method = "lm",
      trControl = ctrl)

# Predict 
p1 <- predict(lm_model,newdata = test_data)

error <- test_data$price - p1

rmse <- sqrt(mean(error**2))

# check importance factor 
varImp(lm_model)
