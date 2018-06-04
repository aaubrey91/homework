library(nnet)
library(caret)

train <- read.csv(file='tit-train.csv', stringsAsFactors = FALSE)
test <- read.csv(file='tit-test.csv', stringsAsFactors = FALSE)

#Keep track of train and test after combined..
train$type <- "train"
test$type <- "test"
test$Survived <- "NULL"
data <- rbind(train,test)

#Fill missing values
data[data$Embarked == "",]$Embarked <- names(which(table(data$Embarked) == max(table(data$Embarked))))
data$Age <- as.numeric(data$Age)
data[is.na(data$Age),]$Age <- round(mean(data$Age, na.rm = T))
data[data$Cabin == "",]$Cabin <- "NULL"

## Clean Data ##

#remove passengerID as this is a unique identifier
#train$PassengerId <- NULL
#test$PassengerId <- NULL
data$PassengerId <- NULL

### get last name ###
library(stringr)
#grab everything up until the first comma.. should be last name.
#train$LastName  <- str_extract(train$Name, "^[^,]+")
#test$LastName  <- str_extract(test$Name, "^[^,]+")
data$LastName  <- str_extract(data$Name, "^[^,]+")

tmp <- str_extract(data$Name, ", [^ ]+")
data$Title <- str_extract(tmp, "[^ ]+$")
rm(tmp)

data <- cbind(data, model.matrix(~ Title - 1, data=data))
data$Title <- NULL

#Onehot encode 
#train <- cbind(train, model.matrix(~ LastName - 1, data=train))
#test <- cbind(test, model.matrix(~ LastName - 1, data=test))
data <- cbind(data, model.matrix(~ LastName - 1, data=data))

#We can remove Full Name now.
#train$Name <- NULL
#test$Name <- NULL
data$Name <- NULL
data$LastName <- NULL


#Convert Sex to nums
#train$male <- ifelse(train$Sex == 'male', 1, 0)
#train$female <- ifelse(train$Sex == 'female', 1, 0)
#test$male <- ifelse(test$Sex == 'male', 1, 0)
#test$female <- ifelse(test$Sex == 'female', 1, 0)
data$male <- ifelse(data$Sex == 'male', 1, 0)
data$female <- ifelse(data$Sex == 'female', 1, 0)


#We can now drop the Sex col.. 
#train$Sex <- NULL
#test$Sex <- NULL
data$Sex <- NULL


#Convert Embarked to nums
#train$Embarked_C <- ifelse(train$Embarked == 'C', 1, 0)
#train$Embarked_Q <- ifelse(train$Embarked == 'Q', 1, 0)
#train$Embarked_S <- ifelse(train$Embarked == 'S', 1, 0)
#test$Embarked_C <- ifelse(test$Embarked == 'C', 1, 0)
#test$Embarked_Q <- ifelse(test$Embarked == 'Q', 1, 0)
#test$Embarked_S <- ifelse(test$Embarked == 'S', 1, 0)
data$Embarked_C <- ifelse(data$Embarked == 'C', 1, 0)
data$Embarked_Q <- ifelse(data$Embarked == 'Q', 1, 0)
data$Embarked_S <- ifelse(data$Embarked == 'S', 1, 0)


#We can now drop the Embarked col..
#train$Embarked <- NULL
#test$Embarked <- NULL
data$Embarked <- NULL

#Convert to factors
#data$Pclass <- as.factor(data$Pclass)

#Get meaning from ticket..
data$ticket_title <- str_extract(data$Ticket, "^[^ ]+")
data$ticket_title <- ifelse(grepl("^[0-9]+$", data$ticket_title), "all_nums", data$ticket_title)
data <- cbind(data, model.matrix(~ ticket_title - 1, data=data))
data$Ticket <- NULL
data$ticket_title <- NULL

#data[data$Cabin == "",]$Cabin <- "NULL"
data <- cbind(data, model.matrix(~ Cabin - 1, data=data))
data$Cabin <- NULL

#data$Parch <- as.numeric(data$Parch)
#data$Pclass <- as.numeric(data$Pclass)
#data$SibSp <- as.numeric(data$SibSp)
#data$Age <- as.numeric(data$Age)
#data$Fare <- as.numeric(data$Fare)

#data <- apply(data, 2, as.numeric)

#Get train/test data back..
train <- data[data$type == 'train',]
train$type <- NULL
test <- data[data$type == 'test',]
test$Survived <- NULL #Remove col we added so we could smoothly combine the df earlier..
test$type <- NULL

#train[,c(2:1142)] <- as.data.frame(sapply(train[,c(2:1142)], as.numeric))
train <- apply(as.data.frame(train), 2, as.numeric)
train <- as.data.frame(train)

#Convert target var to factor
train$Survived <- as.factor(train$Survived)

n <- dim(train)[1]

t1 <- sample(1:n, n*.8)
t2 <- setdiff(1:n, t1)

train_train <- train[t1, ]
train_train <- as.data.frame(train_train)
train_train$Survived <- as.factor(train_train$Survived)
train_test <- subset(train[t2,], select=- Survived)

grid <- expand.grid(size=c(7,8,9), decay=c(0.01,0.1))
nfit <- train(Survived ~ ., data=train_train, 
              method="nnet", 
              tuneGrid=grid, 
              skip=FALSE, 
              linout=FALSE, 
              na.action=na.exclude,
              MaxNWts = 20000)

pred <- predict(nfit, train_test, type="raw")
confusionMatrix(pred)

