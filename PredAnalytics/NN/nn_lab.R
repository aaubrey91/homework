library(nnet)
library(caret)

data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/glass/glass.data",
                 head=FALSE, sep=",")

data$V11 <- as.factor(data$V11)
#Remove ID
data$V1 <- NULL

train = sample(1:214, 171)
test = setdiff(1:214, train)
data_train = data[train,]
data_test = subset(data[test,], select=-V11)

mod <- nnet(V11 ~ ., data[train,], size=10,skip=FALSE,linout=FALSE)
summary(mod)

pred <- predict(mod, data_test)
pred

pred <- predict(mod, data_test, type="class")
pred

## In case we don't predict at least one of every potential class. 
u = union(pred, data[test,]$V11) 
t = table(factor(data[test,]$V11,u), factor(pred, u))
confusionMatrix(t)

grid <- expand.grid(size=c(6,7,8,9,10,11,12,13,14,15), decay=c(0,0.01,0.1,1))
nfit <- train(V11 ~ ., data=data[train,], method="nnet", tuneGrid=grid, skip=FALSE, linout=FALSE)

pred3 <- predict(nfit, data_test, type="raw")
confusionMatrix(table(factor(data[test,]$V11,u), factor(pred3, u)))


