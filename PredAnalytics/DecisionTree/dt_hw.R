library(rpart)
library(caret)

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
                 col.names=c("ID", "RI", "Na", "Mg", "Al","Si","K",
                             "Ca","Ba","Fe", "glass"))

#Convert class num to label
data$glass_name <- "NULL"
data[data$glass == 1,]$glass_name <- "building_windows_float_processed"
data[data$glass == 2,]$glass_name <- "building_windows_non_float_processed"
data[data$glass == 3,]$glass_name <- "vehicle_windows_float_processed"

#none of class 4 in this dataset
#data[data$glass == 4,]$glass_name <- "vehicle_windows_non_float_processed"
data[data$glass == 5,]$glass_name <- "containers"
data[data$glass == 6,]$glass_name <- "tableware"
data[data$glass == 7,]$glass_name <- "headlamps"
data$glass_name <- as.factor(data$glass_name)

#Drop ID and the class number before we changed it
data$ID <- NULL
data$glass <- NULL

#Train/Test Split
n <- dim(data)[1]

t1 <- sample(1:n, n*.8)
t2 <- setdiff(1:n, t1)

train <- subset(data[t1,])
test <- subset(data[t2,], select =- glass_name)

### 1. BASIC ###
fitControl <- trainControl(method='cv', number=10)

Grid <- expand.grid(cp=seq(0,0.1, 0.005))
#Run the training, needed to remove NAs
trained_tree <- train(glass_name ~ ., data=train, method='rpart', 
                      trControl=fitControl, metric='Accuracy', maximize=TRUE, tuneGrid=Grid)
trained_tree

pred <- predict(trained_tree, test, type="raw")
confusionMatrix(table(pred, data[t2,]$glass_name))
plot(pred)

temp <- rpart.control(xval=10, minbucket = 2, minsplit = 4, cp = 0)
#Let's plot the tree with the rpart function, couldn't get fancy plot to work on mac
dfit <- rpart(glass_name ~ ., data=train, control=temp, cp=trained_tree)
rpart.plot::rpart.plot(dfit)

### 2. Bagging functino ###
library(ipred)

### Find best nbagg checking from 1-70
acc <- NULL
for (i in 1:70) {
  baggedTree <- bagging(glass_name ~ ., data=train, nbagg=i)
  pred <- predict(baggedTree, test)
  
  #In case our test data has classes that aren't in the predicted
  u = union(pred, data[t2,]$glass_name) 
  t = table(factor(pred,u), factor(data[t2,]$glass_name, u))
  c <- confusionMatrix(t)
  
  #c <- confusionMatrix(table(pred, data[t2,]$glass_name))  
  acc[i] <- c$overall[1]
}

#What's the nbagg that gives us the max accuracy
nbagg <- which(max(acc) == acc, arr.ind = TRUE)[1]
baggedTree <- bagging(glass_name ~ ., data=train, nbagg=nbagg)
pred <- predict(baggedTree, test)
confusionMatrix(table(pred, data[t2,]$glass_name))  

### 3. RandomForest ###
library(randomForest)
control <- trainControl(method="repeatedcv", number=4, repeats=3)
metric <- 'Accuracy'
n <- round(sqrt(ncol(train)))
tuneGrid <- expand.grid(.mtry=seq(1,n,1))
rf_default <- train(glass_name ~ ., data=train, method="rf", 
                    metric=metric, tuneGrid=tuneGrid, trControl=control)
print(rf_default)

pred <- predict(rf_default, test)
confusionMatrix(table(pred, data[t2,]$glass_name))
