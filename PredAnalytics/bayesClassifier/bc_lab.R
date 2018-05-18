library(e1071)
library(caret)

data <- read.csv(file='balance.csv', head=FALSE, sep=",")

t1 = sample(1:625, 500)
t2 = setdiff(1:625, t1)

cl = data[t2,]$V1

train = subset(data[t1,])
test = subset(data[t2,],select=- V1)

model <- naiveBayes(V1 ~., data=train)

pred <- predict(model, test)

confusionMatrix(pred, cl)

pred <- predict(model, test, type="raw")
pred

