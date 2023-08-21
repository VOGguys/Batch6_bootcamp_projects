library(titanic)
library(tidyverse)
library(dplyr)

tibble(titanic_train)

titanic_train <- na.omit(titanic_train) ## clear NA value
nrow(titanic_train)

##spit data
train_test_spilt <- function(data){
  set.seed(42)
  n <-nrow(data)
  id <-sample(1:n,size = n*0.7)
  train_data <- data[id,]
  test_data <- data[-id,]
  return(list(train_data,test_data))
}

spilt_data <- train_test_spilt(titanic_train)

## Train Model

model <- glm(Survived ~ Pclass+Sex+Age+Parch,data= spilt_data[[1]],family = "binomial")

summary(model)

## Train Data

spilt_data[[1]]$prob_survived <- predict(model,type = "response")

spilt_data[[1]]$pred_survived <-ifelse(spilt_data[[1]]$prob_survived>=0.5,1,0)


## Test Data

spilt_data[[2]]$prob_survived <- predict(model,newdata =spilt_data[[2]],type = "response")

spilt_data[[2]]$pred_survived <-ifelse(spilt_data[[2]]$prob_survived>=0.5,1,0)

## Model Evaluation
# Confusion Matrix
# Train Data
train_con <-table(spilt_data[[1]]$pred_survived,spilt_data[[1]]$Survived,
              dnn=c("Predicted","Actual"))

train_acc <- train_con[1,1]+train_con[2,2] / sum(train_con)
train_pre <- train_con[2,2] / sum(train_con[2,1]+train_con[2,2])
train_rec <- train_con[2,2] / sum(train_con[1,2]+train_con[2,2])
train_f1 <- 2*((train_pre*train_rec)/(train_pre+train_rec))

cat("Model Evaluation for train data",
    "\n Accuracy:",train_acc,
    "\n Precision:",train_pre,
    "\n Recall:",train_rec,
    "\n F1 Score:",train_f1)

# Test Data

test_con <-table(spilt_data[[2]]$pred_survived,spilt_data[[2]]$Survived,
                  dnn=c("Predicted","Actual"))

test_acc <- test_con[1,1]+test_con[2,2] / sum(test_con)
test_pre <- test_con[2,2] / sum(test_con[2,1]+test_con[2,2])
test_rec <- test_con[2,2] / sum(test_con[1,2]+test_con[2,2])
test_f1 <- 2*((test_pre*test_rec)/(test_pre+test_rec))

cat("Model Evaluation for train data",
    "\n Accuracy:",test_acc,
    "\n Precision:",test_pre,
    "\n Recall:",test_rec,
    "\n F1 Score:",test_f1)

# Verify the accuracy of train and test model prediction
# 1 Train model accuracy
traincheck <- mean(spilt_data[[1]]$Survived == spilt_data[[1]]$pred_survived)
cat("The percentage of the train model prediction accuracy is", traincheck)
# 2 Test model accuracy
testcheck <- mean(spilt_data[[2]]$Survived == spilt_data[[2]]$pred_survived)
cat("The percentage of the train model prediction accuracy is", testcheck)
