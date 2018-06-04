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
data$PassengerId <- NULL

### get last name ###
library(stringr)
#grab everything up until the first comma.. should be last name.
data$LastName  <- str_extract(data$Name, "^[^,]+")

#Grab everything after the first comma up until the next space..
tmp <- str_extract(data$Name, ", [^ ]+")
data$Title <- str_extract(tmp, "[^ ]+$")
rm(tmp)

#Global list to keep track of all unique instances of value in the title column
g_title <- list()

convert_title_to_num <- function(dp) {
  i <- length(g_title) + 1
  if (dp %in% g_title) {
    return(which(g_title == dp))
  }
  g_title[i] <<- dp
  return(i)
}
data$Title <- sapply(data$Title, convert_title_to_num)


g_ln <- list()
convert_ln_to_num <- function(dp) {
  i <- length(g_ln) + 1
  if (dp %in% g_ln) {
    return(which(g_ln == dp))
  }
  g_ln[i] <<- dp
  return(i)
}
data$LastName <- sapply(data$LastName, convert_ln_to_num)

#We can remove the full name feature now.
data$Name <- NULL

#Expand the Sex col and remove.
data$male <- ifelse(data$Sex == 'male', 1, 0)
data$female <- ifelse(data$Sex == 'female', 1, 0)
data$Sex <- NULL

#Expand the Embarked Column and remove.
data$Embarked_C <- ifelse(data$Embarked == 'C', 1, 0)
data$Embarked_Q <- ifelse(data$Embarked == 'Q', 1, 0)
data$Embarked_S <- ifelse(data$Embarked == 'S', 1, 0)
data$Embarked <- NULL

#extract first part out of ticket
data$ticket_title <- str_extract(data$Ticket, "^[^ ]+")
data$ticket_title <- ifelse(grepl("^[0-9]+$", data$ticket_title), "all_nums", data$ticket_title)

g_tt <- list()
convert_tt_to_num <- function(dp) {
  i <- length(g_tt) + 1
  if (dp %in% g_tt) {
    return(which(g_tt == dp))
  }
  g_tt[i] <<- dp
  return(i)
}
#Get all unique instances from ticket_title col and remove ticket.
data$ticket_title <- sapply(data$ticket_title, convert_tt_to_num)
data$Ticket <- NULL

#Get all unique instances from Cabin.
g_c <- list()
convert_c_to_num <- function(dp) {
  i <- length(g_c) + 1
  if (dp %in% g_c) {
    return(which(g_c == dp))
  }
  g_c[i] <<- dp
  return(i)
}
data$Cabin <- sapply(data$Cabin, convert_c_to_num)

#Scale numeric columns
data[,c("Age","Fare")] <- scale(data[,c("Age","Fare")], center=TRUE, scale=TRUE)

#Get train/test data back after preprocessing..
train <- data[data$type == 'train',]
train$type <- NULL #remove type col that we created to keep track of train/test original

test <- data[data$type == 'test',]
test$type <- NULL #remove type col that we created to keep track of train/test original

#Convert target var to factor
train$Survived <- as.factor(train$Survived)
test$Survived <- as.factor(test$Survived)

n <- dim(train)[1]

t1 <- sample(1:n, n*.8)
t2 <- setdiff(1:n, t1)

train_train <- train[t1, ]
train_train <- as.data.frame(train_train)
#train_train$Survived <- as.factor(train_train$Survived)
train_test <- subset(train[t2,], select=- Survived)

grid <- expand.grid(size=c(6,7,8,9,10,11,12,13,14,15), decay=c(0,0.01,0.1,1))
nfit <- train(Survived ~ ., data=train_train, 
              method="nnet", 
              tuneGrid=grid, 
              skip=FALSE, 
              linout=FALSE, 
              na.action=na.exclude,
              MaxNWts = 20000)

pred <- predict(nfit, train_test, type="raw")
confusionMatrix(pred, train[t2,]$Survived)


pred <- predict(nfit, test, type="raw")

print("Number of survival..")
table(pred)
