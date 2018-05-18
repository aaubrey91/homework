library(class)
library(caret)

data <- read.csv(file='balance.csv', head=FALSE, sep=",")

preProcess(data, method=c("center", "scale"))

t1 = sample(1:625, 500)
t2 = setdiff(1:625, t1)

c1 = data[t1,]$V1

train = subset(data[t1,], select =- V1)
test = subset(data[t2,], select =- V1)

pred <- knn(train, test, c1, k=3, prob=FALSE, use.all = TRUE)

confusionMatrix(pred, data[t2,]$V1)

ktune <- train(train, c1, method="knn", 
               tuneGrid=data.frame(.k = 1:20), 
               trControl = trainControl(method = "cv"))

ktune

pred2 <- predict(ktune, test)
saveRDS(ktune, file="ktune.rds")

reload = readRDS("ktune.rds")
predict(reload, test)
