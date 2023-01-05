install.packages("mlbench")
install.packages("caret")

library(mlbench)
library(caret)

# Classifications Problem
data("PimaIndiansDiabetes")

df <- PimaIndiansDiabetes
head(df)

library(tidyverse)

glimpse(df)

# Prep Data and explore data

df %>%
  count(diabetes) %>%
  mutate(pct = n/sum(n))

mean(complete.cases(df))

# Train Model

train_test_split <- function(data,train_size=0.8){
  set.seed(42)
  n <- nrow(data)
  id <-sample(1:n,size = n*train_size)
  train_data <- data[id,]
  test_data <- data[-id,]
  return(list(train_data,test_data))
}

split_data <- train_test_split(df)
train_data <- split_data[[1]]
test_data <- split_data[[2]]

## Build knn model

crtl <- trainControl(method = "CV",
             number = 5,
             verboseIter = TRUE)


knn_model <- train(diabetes ~ .,
      data = train_data,
      method = "knn",
      metric = "Accuracy",
      trControl = crtl)

# Predict

p1 <- predict(knn_model,newdata = test_data)

mean(p1 == test_data$diabetes)


## Build Random forest model
set.seed(3)
rf_model <- train(diabetes ~ .,
               data = train_data,
               method = "rf",
               metric = "Accuracy",
               trControl = crtl)

# Predict 

p2 <- predict(rf_model,newdata = test_data)

mean(p2 == test_data$diabetes)

## Build tree model
set.seed(15)
tree_model <- train(diabetes ~ .,
                  data = train_data,
                  method = "rpart",
                  metric = "Accuracy",
                  trControl = crtl)

# Predict

p3 <- predict(tree_model,newdata = test_data)

mean(p3 == test_data$diabetes)

## Build logistic model
set.seed(19)
logic_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "glm",
                    metric = "Accuracy",
                    trControl = crtl)

# Predict logistic model

p4 <- predict(logic_model,newdata = test_data)

mean(p4 == test_data$diabetes)


#evaluates model

list_models <- list(
  knn = knn_model,
  logistic = logic_model,
  tree = tree_model,
  randomforst = rf_model
)

result <-resamples(list_models)

summary(result)


confusionMatrix(p4, # logistic model
                test_data$diabetes,
                positive = "pos",# we find positive case of diabetes
                mode = "prec_recall")
