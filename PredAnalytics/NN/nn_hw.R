library(nnet)

train <- read.csv(file='tit-train.csv')
test <- read.csv(file='tit-test.csv')

## Clean Data ##

#remove passengerID as this is a unique identifier
train$PassengerId <- NULL
test$PassengerId <- NULL

#Name will be a unique identifer as well, let's get rid of it. 
train$Name <- NULL
test$Name <- NULL

#Ticket num should be unique as well, let's get rid of that one too. 
train$Ticket <- NULL
test$Ticket <- NULL

#Convert target var to factor
train$Survived <- as.factor(train$Survived)

train$male <- ifelse(train$Sex == 'male', 1, 0)
train$female <- ifelse(train$Sex == 'female', 1, 0)
test$male <- ifelse(test$Sex == 'male', 1, 0)
test$female <- ifelse(test$Sex == 'female', 1, 0)

#We can now drop the Sex col.. 
train$Sex <- NULL
test$Sex <- NULL


