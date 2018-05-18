library(class)
library(caret)

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt",
                 head=FALSE, 
                 col.names=c("varOWTi","skwOWTi","curtOWTI","entropy","class"))

# This is a class value not a continuous attribute
data$class <- as.factor(data$class)

preProcess(data, method=c("center", "scale"))

n <- dim(data)[1]

t1 <- sample(1:n, n*.8)
t2 <- setdiff(1:n, t1)

c1 <- data[t1,]$class

train <- subset(data[t1,], select =- class)
test <- subset(data[t2,], select =- class)

ktune <- train(train, c1, method="knn", 
               tuneGrid=data.frame(.k = 1:50), 
               trControl = trainControl(method = "cv"))

ktune #k = 14 suggestion

pred <- predict(ktune, test)
confusionMatrix(pred, data[t2,]$class)

